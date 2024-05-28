use crate::chunk::*;
use crate::compiler::Compiler;
use crate::value::*;
use interpreter::parser::{Expr, Stmt};
use num_enum::{IntoPrimitive, TryFromPrimitive};
use std::collections::HashMap;

type Value = Object;

#[repr(u8)]
#[derive(Debug, TryFromPrimitive, IntoPrimitive, PartialEq, PartialOrd)]
pub enum Opcode {
    Return,
    Constant,
    Nil,
    True,
    False,

    Print,

    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,

    Not,
    Equal,
    Greater,
    Less,

    Pop,
    DefineGlobal,
    SetGlobal,
    GetGlobal,
    SetLocal,
    GetLocal,

    JumpIfFalse,
    Jump,
    Loop, // unconditionally jumps backwards by a given offset
    
    Call,
}

#[derive(Debug)]
pub struct CallFrame {
    pub function: LoxFunction, // TODO: this should be a pointer (for looking up constants)
    pub ip: usize,
    pub slots: Vec<Value>,
}

impl CallFrame {
    pub fn new(fun: LoxFunction) -> Self {
        CallFrame {
            function: fun,
            ip: 0,
            slots: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct VM {
    frames: Vec<CallFrame>, // "the call stack"
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
    locals: Option<HashMap<Expr, usize>>,
    // the clox implementation has a `Value* stackTop;` pointer to the top of the stack
    // they use that as a counter for the current position.  I don't think that would work well in Rust
}

#[derive(Debug)]
pub enum InterpretError {
    COMPILE_ERR,
}

impl VM {
    pub fn new() -> Self {
        VM {
            frames: Vec::new(),
            stack: vec![],
            globals: HashMap::new(),
            locals: None,
        }
    }

    fn frame(&self) -> &CallFrame {
        self.frames.last().expect("frames not to be empty")
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().expect("frames to not be empty")
    }

    fn read_u8(&mut self) -> u8 {
        let frame = self.frame_mut();

        let byte = frame.function.chunk.instr[frame.ip];
        frame.ip += 1;

        return byte;
    }

    fn read_opcode(&mut self) -> Result<Opcode, InterpretError> {
        let instr = Opcode::try_from(self.read_u8());

        return match instr {
            Ok(i) => Ok(i),
            Err(e) => Err(InterpretError::COMPILE_ERR),
        };
    }

    fn read_short(&mut self) -> u16 {
        let frame = self.frame_mut();
        frame.ip += 2;

        let high = (frame.function.chunk.instr[frame.ip - 2] as u16) << 8;
        let low = frame.function.chunk.instr[frame.ip - 1] as u16;

        return high | low;
    }

    fn binary_operation(&mut self, op: fn(Value, Value) -> Option<Value>) -> Option<Value> {
        let b = self.pop().unwrap(); // TODO: insufficent bla err?
        let a = self.pop().unwrap();
        op(a, b)
    }

    // push the result immediately, returning false if failed
    fn binary_operation_store(&mut self, op: fn(Value, Value) -> Option<Value>) -> bool {
        let b = self.pop().unwrap(); // TODO: insufficent bla err?
        let a = self.pop().unwrap();
        let res = op(a, b);

        if res.is_some() {
            self.push(res.unwrap());
        } else {
            return false;
        }

        return true;
    }

    pub fn interpret(&mut self, source: &str) -> Result<(), InterpretError> {
        use interpreter::parser::*;
        use interpreter::scanner::*;

        let scanner = Scanner::new(&source);
        let tokens = scanner.scan_tokens();
        let mut parser = Parser::new(tokens);
        let stmts = parser.parse().expect("parser to return statements");
        let mut compiler = Compiler::new();

        println!("parsed code::\n{:?}\n\n", stmts);

        for stmt in stmts {
            stmt.visit(&mut compiler);
        }

        //let cf = CallFrame::new(compiler.end_compiler()); // TODO: what if there's more call frames?  currently those get dropped
        //self.frames.push(cf);

        /*
            24/05/2024 15:50
            in clox we push the LoxFunction that rolls out of end_compiler() to the stack 
            and call said function using call(function, 0);

            how does that translate WRT callframes? 
            => call(ObjFunction* function, int argCount) creates a new callframe and pushes this to the top.
        */

        let fun = compiler.end_compiler();
        self.push(Object::LoxFunction(fun));
        let callee = self.peek(0).unwrap().clone();
        println!("calling {:?}", callee);
        println!("stack: {:?}", self.stack);
        self.callValue(callee, 0);

        return self.run();
    }

    pub fn run(&mut self) -> Result<(), InterpretError> {
        // interpret():
        // push the frame-with-fn to the stack
        // self.push(Object::LoxFunction(())) // <- TODO, you were here,
        /* see TODO in main.rs: the VM should wrap the compiler */

        // end-interpret;
        while true {
            self.frame()
                .function
                .chunk
                .dissasemble_instruction(self.frame().ip); // TODO: frame.ip is not correctly calculated?

            let instr = self.read_opcode()?;

            match instr {
                Opcode::Return => {
                    //println!("pop: {:?}", self.pop());
                    return Ok(());
                }
                Opcode::Constant => {
                    let constant = self.read_constant()?;
                    self.push(constant);
                }
                Opcode::Nil => self.push(Value::Nil),
                Opcode::True => self.push(Value::Boolean(true)), // i guess this explains why other rlox implementations do Value::True, Value::False ;3
                Opcode::False => self.push(Value::Boolean(false)),
                Opcode::Add => {
                    self.binary_operation_store(|a, b| {
                        if a.is_string() || b.is_string() {
                            return Some(Value::String(format!(
                                "{}{}",
                                a.to_string(),
                                b.to_string()
                            )));
                        }

                        Some(Value::Number(a.to_num().unwrap() + b.to_num().unwrap()))
                    });
                }
                Opcode::Subtract => {
                    self.binary_operation_store(|a, b| {
                        Some(Value::Number(a.to_num().unwrap() - b.to_num().unwrap()))
                    });
                }
                Opcode::Divide => {
                    self.binary_operation_store(|a, b| {
                        Some(Value::Number(a.to_num().unwrap() / b.to_num().unwrap()))
                    });
                }
                Opcode::Multiply => {
                    self.binary_operation_store(|a, b| {
                        Some(Value::Number(a.to_num().unwrap() * b.to_num().unwrap()))
                    });
                }
                Opcode::Negate => {
                    let value = Value::Number(-self.pop().unwrap().to_num().unwrap());
                    self.push(value);
                }
                Opcode::Not => {
                    let value = self.pop().unwrap();
                    // todo: Interpreter::is_truthy should be an Object impl
                    self.push(Value::Boolean(value.is_falsey()));
                }
                Opcode::Print => {
                    println!("{}", self.pop().unwrap());
                }
                Opcode::Equal => {
                    self.binary_operation_store(|a, b| Some(Value::Boolean(a == b)));
                }
                Opcode::Less => {
                    // TODO: coerces Num for now
                    self.binary_operation_store(|a, b| {
                        Some(Value::Boolean(a.to_num().unwrap() < b.to_num().unwrap()))
                    });
                }
                Opcode::Greater => {
                    // TODO: coerces Num for now
                    self.binary_operation_store(|a, b| {
                        Some(Value::Boolean(a.to_num().unwrap() > b.to_num().unwrap()))
                    });
                }
                Opcode::Pop => {
                    println!("popped {:?} off the stack", self.pop());
                }
                Opcode::DefineGlobal => {
                    let name = self.read_constant()?.as_string().unwrap();
                    //println!("DefineGlobal read_constant as_string: {}", name);
                    self.globals
                        .insert(name, self.stack.last().unwrap().clone());
                    self.pop();
                }
                Opcode::GetGlobal => {
                    let name = self.read_constant()?.as_string().unwrap();
                    self.push(self.globals.get(&name).unwrap().clone());
                }
                Opcode::SetGlobal => {
                    let name = self.read_constant()?.as_string().unwrap();
                    if let std::collections::hash_map::Entry::Occupied(mut entry) =
                        self.globals.entry(name.clone())
                    {
                        *entry.get_mut() = self.stack.last().unwrap().clone();
                    } else {
                        eprintln!(
                            "trying to SetGlobal on a non-existant global variable {}",
                            name
                        );
                    }
                }
                Opcode::SetLocal => {
                    let slot = self.read_u8();
                    self.stack[slot as usize] = self.stack.last().unwrap().clone();
                    // todo: abstract this into `peek` already..
                }
                Opcode::GetLocal => {
                    let slot = self.read_u8();
                    self.push(self.stack[slot as usize].clone());
                }
                Opcode::JumpIfFalse => {
                    let offset = self.read_short();
                    println!("JUMP_IF_FALSE, top of stack {:?}", self.stack.last());

                    if self.stack.last().unwrap().is_falsey() {
                        println!("jumping");
                        self.frame_mut().ip += offset as usize;
                    }
                }
                Opcode::Jump => {
                    let offset = self.read_short();
                    self.frame_mut().ip += offset as usize;
                }
                Opcode::Loop => {
                    let offset = self.read_short();
                    self.frame_mut().ip -= offset as usize;
                }
                Opcode::Call => {
                    let argc = self.read_u8();

                    println!("argc: {};; stack: {:?}", argc, self.stack);
                    // missing?? function on stack
                    self.callValue(self.peek((argc) as usize).unwrap().clone(), argc);
                }
            }
        }

        return Ok(());
    }

    fn callValue(&mut self, callee: Value, argc: u8) -> bool {
        match callee {
            Value::LoxFunction(lf) => {
                // call(ObjFunction* function, int argCount) 
                // create callframe, load it with the function, set the ip to chunk.code
                // frame->slots = vm.stackTop - argCount - 1;
                let ip = lf.chunk.instr.len();
                let mut frame = CallFrame::new(lf);
                frame.ip = 0;
                // TODO: slots??

                self.frames.push(frame);
            },
            e => { panic!("Can't call object {:?}", e) }
        }

        return false;
    }

    fn peek(&self, offset: usize) -> Option<&Value> {
        println!("{} - 1 - {}", self.stack.len(), offset);
        println!("peeking offset {}={}", offset, self.stack.len() - 1 - offset);
        if offset > self.stack.len() {
            return None;
        }

        self.stack.get(self.stack.len() - 1 - offset)
    }

    fn read_constant(&mut self) -> Result<Value, InterpretError> {
        let idx = self.read_u8();
        let constant = self.frame().function.chunk.constants[idx as usize].clone();

        Ok(constant)
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    fn pop(&mut self) -> Option<Value> {
        let p = self.stack.pop();
        println!("? popd {:?}", p);
        //self.stack.pop()
        p
    }

    fn trace_stack(&self) {
        for (i, v) in self.stack.iter().enumerate() {
            println!("[ {:?} ]", v);
        }
    }
}
