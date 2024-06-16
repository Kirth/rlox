
use std::borrow::BorrowMut;
use std::thread::scope;

use interpreter::interpreter::{ExprVisitor, StmtVisitor, Object}; /// AAA, ExprVisitor and StmtVisitor expect and Interpreter Object!!!
use interpreter::scanner::{self, *};
use interpreter::parser::*;

use crate::vm::*;
use crate::chunk::*;

type Value = crate::value::Object;

#[derive(Debug)]
pub struct Compiler {
    state: Vec<CompilerState>,
}

// here follows a long list of wrapper functions that exposes methods for internal compiler state on the current compiler
// so that I don't have to think too much about changing the underlying implementation
impl Compiler {
    pub fn new() -> Self { 
        let state = CompilerState { function: LoxFunctionBuilder::new("root".to_string()), current_scope: 0 };
        
        Compiler {
            state: vec![state]
        }
    }    
    
    pub fn begin_function(&mut self, name: String, current_scope: usize) -> &mut CompilerState { 
        self.state.push(CompilerState { function: LoxFunctionBuilder::new(name), current_scope: current_scope });
        self.state.last_mut().expect("compiler state creation not to fail")
    }

    pub fn function(&mut self) -> &mut LoxFunctionBuilder {
        self.state.last_mut().expect("states not to be empty").function.borrow_mut()
    }

    pub fn current_scope(&self) -> usize {
        self.state.last().expect("states not to be empty").current_scope
    }

    pub fn end_function(&mut self) -> LoxFunction {
        self.function().chunk.emit(Opcode::Return.into());
        return self.state.pop().expect("state not to be empty").function.build()
    }

    pub fn resolve_local(&self, scope_depth: usize, name: &str) -> Option<u8> {
        self.state.last().expect("states to not be empty").resolve_local(scope_depth, name)
    }

    fn begin_scope(&mut self) {
        self.state.last_mut().expect("states not to be empty").begin_scope()
    }

    fn end_scope(&mut self) {
        self.state.last_mut().expect("states not to be empty").end_scope()
    }

    pub fn declare_variable(&mut self, name: String) -> Result<(), String> {
        self.state.last_mut().expect("states to not be empty").declare_variable(name)
    }

    pub fn add_local(&mut self, name: String, scope_depth: usize) {
        self.state.last_mut().expect("states to not be empty").add_local(name, scope_depth)
    }
}

#[derive(Debug)]
struct CompilerState {
    function: LoxFunctionBuilder, // TODO: cwbriones has Locals in the CompilerSTate, not the builder?
    current_scope: usize,
}

impl CompilerState {
    fn begin_scope(&mut self) {
        self.current_scope += 1;
    }

    fn end_scope(&mut self) {
        self.current_scope -= 1;

        while self.function.locals.len() > 0 && self.function.locals[self.function.locals.len() - 1].1 > self.current_scope {
            self.function.chunk.emit(Opcode::Pop.into());
            self.function.locals.pop();
        }
    }

    // resolve a (Name, ScopeDepth) tuple to a slot on the stack
    fn resolve_local(&self, scope_depth: usize, name: &str) -> Option<u8> {
        for (i, l) in self.function.locals.iter().enumerate().rev() {
            if &l.0 == name /*&& l.1 == scope_depth */ { // resolveLocal does not explicitly check scope depth, it goes highest to lowest to find the most local and thus support shadowing
                return Some(i as u8);
            }
        }

        return None;
    }

    // vm pops value from the stack and pushes it either into its global dict, 
    // in clox parseVariable+declareVariable are separate/different from namedVariable
    pub fn declare_variable(&mut self, name: String) -> Result<(), String> {
        //println!(">> declare_variable for {} at depth {}", name, self.current_scope);
        // globals go in to the vm's globals dictionary
        if self.current_scope == 0 {
            /*
            Global variables are looked up by name at runtime. That means the VM—the
            bytecode interpreter loop—needs access to the name. A whole string is too big
            to stuff into the bytecode stream as an operand. Instead, we store the string in
            the constant table and the instruction then refers to the name by its index in the table.
            */

            let idx = self.function.chunk.emit_const_value(Value::String(name));
            //println!("Emitted name {} to idx {}", name.token.as_string().unwrap(), idx);

            self.function.chunk.emit(Opcode::DefineGlobal.into());
            self.function.chunk.emit(idx);

            return Ok(());
        } 

        //println!("declare_variable, populating locals with {}, {}", name, self.current_scope);
        for (i, l) in self.function.locals.iter().enumerate().rev() {

            if &l.0 == &name && l.1 == self.current_scope {
                panic!("variable {} already present in scope {}", name, self.current_scope);
            }
        }

        self.add_local(name, self.current_scope);

        self.function.chunk.emit(Opcode::SetLocal.into());
        self.function.chunk.emit((self.function.locals.len() - 1) as u8); // the local's fixed stack slot
        // this gets cleaned up because the stack always grows with the callframes' invocations


        return Ok(());
    }

    fn add_local(&mut self, name: String, scope_depth: usize) {
        self.function.locals.push((name.clone(), scope_depth));
    }
}

impl ExprVisitor<Result<(), String>> for Compiler {
    fn visit_assign(&mut self, expr: Expr, name: &TokenLoc, value: &Expr) -> Result<(), String> {
        for (i, c) in self.function().chunk.constants.iter().enumerate() {
            if c.is_string() && c.as_string() == name.token.as_string() {
                println!("RE-ASSIGNING {:?}", name);
                value.visit(self).unwrap();
                self.function().chunk.emit(Opcode::SetGlobal.into());
                self.function().chunk.emit(i as u8);
                return Ok(())
            }
        }
        
        panic!("Unknown variable name {}", name.token.as_string().unwrap());
        return Err(format!("Unknown variable name {}", name.token.as_string().unwrap()));
    }

    fn visit_binary(&mut self, expr: Expr, left: &Expr, op: &TokenLoc, right: &Expr) -> Result<(), String> {
        left.visit(self);
        right.visit(self);

        match &op.token {
            Token::PLUS => { self.function().chunk.emit(Opcode::Add.into()) },
            Token::MINUS => { self.function().chunk.emit(Opcode::Subtract.into()) },
            Token::STAR => { self.function().chunk.emit(Opcode::Multiply.into()) },
            Token::SLASH => { self.function().chunk.emit(Opcode::Divide.into()) },

            Token::EQ_EQ => { self.function().chunk.emit(Opcode::Equal.into()) },
            Token::GT => { self.function().chunk.emit(Opcode::Greater.into()) },
            Token::LS => { self.function().chunk.emit(Opcode::Less.into()) },

            _ => { eprintln!("Unknown binary operation {:?}", op); return Err(format!("Unknown binary operation {:?}", op)) }
        }

        Ok(())
    }

    fn visit_call(
            &mut self,
            expr: Expr,
            callee: &Box<Expr>,
            paren: &TokenLoc,
            args: &Vec<Expr>,
        ) -> Result<(), String> {

        callee.visit(self);

        // TODO: can I abstract visit_variable into this?
        //self.visit_variable(expr, name)

        let mut argc : u8 = 0;

        for arg in args {
            arg.visit(self);
            argc += 1;
        }

        self.function().chunk.emit(Opcode::Call.into());
        self.function().chunk.emit(argc);

        Ok(())
    }

    fn visit_get(&mut self, expr: Expr, object: &Box<Expr>, name: &TokenLoc) -> Result<(), String> {
        unimplemented!()
    }

    fn visit_grouping(&mut self, expr: &Expr) -> Result<(), String> {
        expr.visit(self)
    }

    fn visit_literal(&mut self, expr: Expr, value: &Object) -> Result<(), String> {

        /*
        With number literals, we had to deal with the fact that there are billions of
        possible numeric values. We attended to that by storing the literal’s value in the
        chunk’s constant table and emitting a bytecode instruction that simply loaded
        that constant. We could do the same thing for the new types. We’d store, say,
        true, in the constant table, and use an OP_CONSTANT to read it out.
        But given that there are literally (heh) only three possible values we need to
        worry about with these new types, it’s gratuitous—and slow!—to waste a two
        byte instruction and a constant table entry on them. Instead, we’ll define three
        dedicated instructions to push each of these literals on the stack.
        */

        match &value {
            Object::Boolean(b) => if *b == true { self.function().chunk.emit(Opcode::True.into()) } else { self.function().chunk.emit(Opcode::False.into()) },
            Object::Nil => self.function().chunk.emit(Opcode::Nil.into()),
            _ => {
                // dirty hack to transform interpreter::Object to vm::Value
                let offset = self.function().chunk.emit_const_value(match &value {
                    Object::Number(n) => Value::Number(n.clone()),
                    Object::String(s) => Value::String(s.clone()),
                    e => panic!("interpreter-value value unknown to vm: {:?}", e)
                });
                self.function().chunk.emit_const(offset);
            }
        }

        return Ok(())
        //return Err(format!("Uknown Value type {:?}", value));
    }

    fn visit_logical(&mut self, expr: Expr, left: &Expr, op: &TokenLoc, right: &Expr) -> Result<(), String> {
        match &op.token {
            Token::AND => {
                // only evaluate the RHS if the LHS is true
                left.visit(self);
                let end = self.function().chunk.emit_jump(Opcode::JumpIfFalse.into());
                self.function().chunk.emit(Opcode::Pop.into());
                right.visit(self);
                
                self.function().chunk.patch_jump(end);
            },
            Token::OR => {
                left.visit(self);
                // if the LHS is truthy, we don't evaluate the right operand
                let else_jmp = self.function().chunk.emit_jump(Opcode::JumpIfFalse.into());
                let end_jmp = self.function().chunk.emit_jump(Opcode::Jump.into());
                self.function().chunk.patch_jump(else_jmp); // if false -> jump over the jump that avoids RHS
                self.function().chunk.emit(Opcode::Pop.into());
                right.visit(self);
                self.function().chunk.patch_jump(end_jmp);

            },
            _ => {
                return  Err(format!("Unknown logical operator '{:?}', ", op));
            }
        }

        return Ok(())
    }

    fn visit_set(
            &mut self,
            expr: Expr,
            object: &Box<Expr>,
            name: &TokenLoc,
            value: &Box<Expr>,
        ) -> Result<(), String> {
            unimplemented!()
    }

    fn visit_super(&mut self, expr: Expr, token: &TokenLoc, method: &TokenLoc) -> Result<(), String> {
        unimplemented!()
    }

    fn visit_this(&mut self, expr: Expr, token: &TokenLoc) -> Result<(), String> {
        unimplemented!()
    }

    fn visit_unary(&mut self, expr: Expr, op: &TokenLoc, right: &Expr) -> Result<(), String> {
        right.visit(self);

        match &op.token {
            Token::MINUS => self.function().chunk.emit(Opcode::Negate.into()),
            Token::BANG => self.function().chunk.emit(Opcode::Not.into()),
            e => return Err(format!("Uknown unary operator {:?}", op))
        }

        return Ok(())
    }

    fn visit_variable(&mut self, expr: Expr, name: &TokenLoc) -> Result<(), String> {
        //println!("visit_variable at scope {} with compiler:{:?}\n\n", self.current_scope(), self);
        if let Some(slot) = self.resolve_local(self.current_scope(), &name.token.as_string().unwrap()) {
            self.function().chunk.emit(Opcode::GetLocal.into());
            self.function().chunk.emit(slot);
            return Ok(())
        } else {
            for (i, c) in self.function().chunk.constants.iter().enumerate() {
                //println!("checking if {:?} is in globals: {},{}", name, i, c);
                if c.is_string() && c.as_string() == name.token.as_string() {
                    self.function().chunk.emit(Opcode::GetGlobal.into());
                    self.function().chunk.emit(i as u8);
                    return Ok(())
                }

                // BUG 12.06.2024: this breaks when the global is defined in a previous Chunk,
                // the current Chunk's const table then don't contain the variable name's string constant,
                // by current logic that means we assume it don't exist.
                // But only the VM knows which globals are there: order of invocation and execution is not compile time fixed.
                // 

            }

            // I still feel like I"m missing something and that checking whether the global exists should be possible.

            //println!("compiler: variable {} name not found in const table, adding", name.token.as_string().unwrap());
            let idx = self.function().chunk.emit_const_value(Value::String(name.token.as_string().unwrap()));
            self.function().chunk.emit(Opcode::GetGlobal.into());
            self.function().chunk.emit(idx);

            return Ok(())
        }

        panic!("Unknown variable name {}", name.token.as_string().unwrap());
        return Err(format!("Unknown variable name {}", name.token.as_string().unwrap()));
    }

}

impl StmtVisitor for Compiler {
    fn visit_expression(&mut self, expr: &Box<Expr>) -> Option<Object> {
        expr.visit(self);
        self.function().chunk.emit(Opcode::Pop.into()); // discard the unused result from an expression such as `brunch = "quiche"; _eat(brunch)_` off the stack
        None
    }

    fn visit_block(&mut self, stmts: &Vec<Stmt>) -> Option<Object> {
        self.begin_scope();
        
        for stmt in stmts {
            stmt.visit(self);
        }

        self.end_scope();

        None
    }

    fn visit_class(
            &mut self,
            name: &TokenLoc,
            superclass: &Option<Expr>,
            methods: &Vec<Stmt>,
        ) -> Option<Object> {
        unimplemented!()
    }

    fn visit_function(
            &mut self,
            name: &TokenLoc,
            params: &Vec<TokenLoc>,
            body: &Box<Stmt>,
        ) -> Option<Object> {
        
        self.begin_scope(); 
        // TODO: figure out the scope depth
        self.begin_function(name.token.as_string().unwrap(), self.current_scope());

        

        for param in params {
            // uint8_t constant = parseVariable("Expect parameter name.");  // emit a constant
            // defineVariable(constant); -> populate locals + emitBytes(DEFINE_GLOBAL, global)
            self.add_local(param.token.as_string().unwrap(), self.current_scope());
        }

        body.visit(self);
        let fun = self.end_function().clone(); // we drop out of the fn's CompilerState

        let name = fun.name.clone();

        let idx1 = self.function().chunk.emit_const_value(Value::LoxFunction(fun));
        self.function().chunk.emit_const(idx1);

        // push the FnObject to the stack
        // TODO: this should not be a global but a variable like any other.
        // in clox: `ObjFunction* function = endCompiler(); emitBytes(OP_CONSTANT, makeConstant(OBJ_VAL(function)));``
        let idx2 = self.function().chunk.emit_const_value(Value::String(name));
        self.function().chunk.emit(Opcode::DefineGlobal.into());
        self.function().chunk.emit(idx2);

        None
    }

    fn visit_if(
            &mut self,
            condition: &Box<Expr>,
            if_stmt: &Box<Stmt>,
            else_stmt: &Option<Box<Stmt>>,
        ) -> Option<Object> {
        
        condition.visit(self);


        let then_jmp = self.function().chunk.emit_jump(Opcode::JumpIfFalse.into());
        self.function().chunk.emit(Opcode::Pop.into());
        if_stmt.visit(self);


        let else_jmp = self.function().chunk.emit_jump(Opcode::Jump.into());
        
        // we jump over the previously emitted else JMP instruction
        self.function().chunk.patch_jump(then_jmp);
        self.function().chunk.emit(Opcode::Pop.into());

        if let Some(else_stmt) = else_stmt {
            else_stmt.visit(self);
        }

        // we always need to patch; outside of the conditional because
        // that only controls how many prior instructions there are to jump over
        self.function().chunk.patch_jump(else_jmp);
        

        None
    }

    fn visit_print(&mut self, expr: &Box<Expr>) -> Option<Object> {
        expr.visit(self);
        self.function().chunk.emit(Opcode::Print.into());

        None
    }

    fn visit_return(&mut self, keyword: &TokenLoc, value: &Box<Expr>) -> Option<Object> {
        value.visit(self);
        self.function().chunk.emit(Opcode::Return.into());

        None
    }

    fn visit_var(&mut self, name: &TokenLoc, initializer: &Option<Box<Expr>>) -> Option<Object> {
        match initializer {
            Some(expr) => expr.visit(self).unwrap(),
            None => self.function().chunk.emit(Opcode::Nil.into()),
        }
        
        self.declare_variable(name.token.as_string().unwrap());

        None
    }

    fn visit_while(&mut self, condition: &Box<Expr>, body: &Box<Stmt>) -> Option<Object> {
        let loop_start = self.function().chunk.instr.len(); // TODO: abstract?
        condition.visit(self);

        let exit_jmp = self.function().chunk.emit_jump(Opcode::JumpIfFalse.into());
        self.function().chunk.emit(Opcode::Pop.into());
        body.visit(self);

        self.function().chunk.emit_loop(loop_start);
        
        self.function().chunk.patch_jump(exit_jmp);
        

        None
    }
}
