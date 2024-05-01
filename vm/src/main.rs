use num_enum::{IntoPrimitive, TryFromPrimitive};
use std::convert::TryFrom;

use interpreter::interpreter::{ExprVisitor, Object, StmtVisitor};
use interpreter::scanner::{self, *};
use interpreter::parser::*;

type Value = Object;

#[repr(u8)]
#[derive(Debug, TryFromPrimitive, IntoPrimitive, PartialEq, PartialOrd)]
enum Opcode {
    Return,
    Constant,

    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,

    Not,
}

#[derive(Debug)]
enum InterpretError {
    OK,
    COMPILE_ERR,
    RUNTIME_ERR,
}

#[derive(Debug)]
struct Chunk {
    instr: Vec<u8>,
    constants: Vec<Value>,
}

impl Chunk {
    fn new() -> Self { Chunk { instr: vec![], constants: vec![] } }

    fn emit(&mut self, op: u8) {
        self.instr.push(op)
    }

    fn emit_const(&mut self, c: u8) { // todo: chunk_write_const(VALUE) ?
        self.instr.push(Opcode::Constant.into());
        self.instr.push(c)
    }

    fn emit_const_value(&mut self, value: Value) -> u8 {
        self.constants.push(value);
        return (self.constants.len() - 1) as u8
    }

    fn dissasemble_instruction(&self, offset: usize) {
        let op = Opcode::try_from(self.instr[offset]).unwrap();

        print!("| {:04}: {:?}", offset, op);

        match op {
            Opcode::Constant => {
                let idx = self.instr[offset + 1];
                let value = &self.constants[idx as usize];

                print!("({:02}): {:?}", idx, value);
            }
            /* Opcode::Add | Opcode::Divide | Opcode::Multiply | Opcode::Subtract => {
                let idx_a = self.instr[offset + 1];
                let value_a = &self.constants[idx_a as usize];
                let idx_b = self.instr[offset + 2];
                let value_b = &self.constants[idx_b as usize];

                print!("({:02}): {:?}, ({:02}): {:?}", idx_a, value_a, idx_b, value_b);
            } */
            _ => {},
        }

        println!("");
    }
}

#[derive(Debug)]
struct VM {
    chunk: Chunk, // unsure if there's one chunk or multiple
    ip: usize,
    stack: Vec<Value>,
    // the clox implementation has a `Value* stackTop;` pointer to the top of the stack
    // they use that as a counter for the current position.  I don't think that would work well in Rust
}

impl VM {
    fn new() -> Self {
        VM {
            chunk: Chunk { instr: vec![0, 0], constants: vec![ Value::Number(1337.0) ] },
            ip: 0,
            stack: vec![]
        }
    }

    fn read_u8(&mut self) -> u8 {
        let byte = self.chunk.instr[self.ip];
        self.ip = self.ip + 1;

        return byte;
    }

    fn read_opcode(&mut self) -> Result<Opcode, InterpretError> {
        let instr = Opcode::try_from(self.read_u8());

        return match instr {
            Ok(i) => Ok(i),
            Err(e) => Err(InterpretError::COMPILE_ERR)
        };
    }

    fn binary_operation(&mut self, op: fn(Value, Value) -> Option<Value>) -> Option<Value> {
        let b = self.pop().unwrap(); // TODO: insufficent bla err?
        let a = self.pop().unwrap();    
        op(a, b)
    }

    fn run(&mut self) -> Result<(), InterpretError> {
        while true {
            self.chunk.dissasemble_instruction(self.ip);
            let instr = self.read_opcode()?;
            

            match instr {
                Opcode::Return => {
                    println!("pop: {:?}", self.pop());
                    return Ok(());
                },
                Opcode::Constant => {
                    let constant = self.read_constant()?;
                    self.push(constant);                 
                }
                Opcode::Add => { 
                    if let Some(v) = self.binary_operation(|a, b| {
                        if a.is_str() || b.is_str() {
                            return Some(Value::String(format!("{}{}", a.to_string(), b.to_string())))
                        }
                        
                        Some(Value::Number(a.to_num().unwrap() + b.to_num().unwrap())) 
                    }) {
                        self.push(v)
                    }
                },
                Opcode::Subtract => { 
                    if let Some(v) = self.binary_operation(|a, b| {
                        Some(Value::Number(a.to_num().unwrap() - b.to_num().unwrap())) 
                    }) {
                        self.push(v)
                    }
                },
                Opcode::Divide => {
                    if let Some(v) = self.binary_operation(|a, b| {
                        Some(Value::Number(a.to_num().unwrap() / b.to_num().unwrap())) 
                    }) {
                        self.push(v)
                    }
                },
                Opcode::Multiply => {
                    if let Some(v) = self.binary_operation(|a, b| {
                        Some(Value::Number(a.to_num().unwrap() * b.to_num().unwrap())) 
                    }) {
                        self.push(v)
                    }
                },
                Opcode::Negate => {
                    let value = Value::Number(- self.pop().unwrap().to_num().unwrap());
                    self.push(value);
                },
                Opcode::Not => {
                    let value = self.pop().unwrap();
                    // todo: Interpreter::is_truthy should be an Object impl
                    self.push(Value::Boolean(match value {
                        Object::Nil => false,
                        Object::Boolean(b) => b,
                        _ => true,
                    }));
                }
            }
        }

        return Ok(());
    }

    fn read_constant(&mut self) -> Result<Value, InterpretError> {
        let idx = self.read_u8();
        let constant = self.chunk.constants[idx as usize].clone();
        
        Ok(constant)
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }

    fn trace_stack(&self) {
        for (i, v) in self.stack.iter().enumerate() {
            println!("[ {:?} ]", v);
        }
    }
}

struct Compiler {
    chunk: Chunk
}

impl Compiler {
    fn new() -> Self { Compiler { chunk: Chunk::new() }}
}

impl ExprVisitor<Result<(), String>> for Compiler {
    fn visit_assign(&mut self, expr: Expr, name: &TokenLoc, value: &Expr) -> Result<(), String> {
        unimplemented!()
    }

    fn visit_binary(&mut self, expr: Expr, left: &Expr, op: &TokenLoc, right: &Expr) -> Result<(), String> {
        println!("visiting right expr: {:?}", left);
        left.visit(self);
        println!("visiting left expr: {:?}", right);
        right.visit(self);

        match &op.token {
            Token::PLUS => { self.chunk.emit(Opcode::Add.into()) },
            Token::MINUS => { self.chunk.emit(Opcode::Subtract.into()) },
            Token::STAR => { self.chunk.emit(Opcode::Multiply.into()) },
            Token::SLASH => { self.chunk.emit(Opcode::Divide.into()) },

            _ => return Err(format!("Unknown operation {:?}", op))
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
        unimplemented!();
    }

    fn visit_get(&mut self, expr: Expr, object: &Box<Expr>, name: &TokenLoc) -> Result<(), String> {
        unimplemented!()
    }

    fn visit_grouping(&mut self, expr: &Expr) -> Result<(), String> {
        expr.visit(self)
    }

    fn visit_literal(&mut self, expr: Expr, value: &Object) -> Result<(), String> {
        let offset = self.chunk.emit_const_value(value.clone());
        self.chunk.emit_const(offset);

        return Ok(())
        //return Err(format!("Uknown Value type {:?}", value));
    }

    fn visit_logical(&mut self, expr: Expr, left: &Expr, op: &TokenLoc, right: &Expr) -> Result<(), String> {
        unimplemented!()
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
            Token::MINUS => self.chunk.emit(Opcode::Negate.into()),
            Token::BANG => self.chunk.emit(Opcode::Not.into()),
            e => return Err(format!("Uknown unary operator {:?}", op))
        }

        return Ok(())
    }

    fn visit_variable(&mut self, expr: Expr, name: &TokenLoc) -> Result<(), String> {
        unimplemented!()
    }

}

impl StmtVisitor for Compiler {
    fn visit_expression(&mut self, expr: &Box<Expr>) -> Option<Object> {
        println!("visit_expression: {:?}", expr);
        expr.visit(self);
        None
    }

    fn visit_block(&mut self, stmts: &Vec<Stmt>) -> Option<Object> {
        unimplemented!()
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
        unimplemented!()
    }

    fn visit_if(
            &mut self,
            condition: &Box<Expr>,
            if_stmt: &Box<Stmt>,
            else_stmt: &Option<Box<Stmt>>,
        ) -> Option<Object> {
        unimplemented!()
    }

    fn visit_print(&mut self, expr: &Box<Expr>) -> Option<Object> {
        unimplemented!()
    }

    fn visit_return(&mut self, keyword: &TokenLoc, value: &Box<Expr>) -> Option<Object> {
        unimplemented!()
    }

    fn visit_var(&mut self, name: &TokenLoc, initializer: &Option<Box<Expr>>) -> Option<Object> {
        unimplemented!()
    }

    fn visit_while(&mut self, condition: &Box<Expr>, body: &Box<Stmt>) -> Option<Object> {
        unimplemented!()
    }
}

fn compile(source: String) -> Result<Chunk, String> {
    let scanner = Scanner::new(&source);
    let tokens = scanner.scan_tokens();
    let mut parser = Parser::new(tokens);
    let stmts = parser.parse()?;
    let mut compiler = Compiler::new();

    for stmt in stmts {
        stmt.visit(&mut compiler);
    }

    let mut chunk = compiler.chunk;

    // endCompiler:
    chunk.emit(Opcode::Return.into());

    Ok(chunk)
}

fn main() {
    let mut vm = VM::new();
    vm.chunk = compile("(5 + 5) * 2 + \"foobar\";".to_string()).unwrap();

    println!("{:?}", vm);


    let res = vm.run();

    println!("{:?}", res);
}  