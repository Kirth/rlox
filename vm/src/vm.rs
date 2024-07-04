use crate::chunk::*;
use crate::compiler::Compiler;
use crate::value::*;
use interpreter::parser::{Expr, Stmt};
use num_enum::{IntoPrimitive, TryFromPrimitive};
use std::collections::HashMap;

type Value = Object;

#[repr(u8)]
#[derive(Debug, TryFromPrimitive, IntoPrimitive, PartialEq, PartialOrd)]

// I have since learned that it's better for the reader and programmer to have hardcoded these
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
    SetUpvalue,
    GetUpvalue,

    JumpIfFalse,
    Jump,
    Loop, // unconditionally jumps backwards by a given offset
    
    Call,
    Closure,
}

#[derive(Debug)]
pub struct CallFrame {
    pub closure: Closure, // TODO: this should be a pointer (for looking up constants)
    pub ip: usize,
    pub stack_start: usize, // rather than slots
}

impl CallFrame {
    pub fn new(closure: Closure) -> Self {
        CallFrame {
            closure: closure,
            ip: 0,
            stack_start: 0,
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

        let byte = frame.closure.function.chunk.instr[frame.ip];
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

        let high = (frame.closure.function.chunk.instr[frame.ip - 2] as u16) << 8;
        let low = frame.closure.function.chunk.instr[frame.ip - 1] as u16;

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

        let fun = compiler.end_function();
        self.push(Object::Closure(Closure::new(fun)));
        let callee = self.peek(0).unwrap().clone();
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
            if cfg!(feature = "debug_output") {
                self.frame()
                    .closure
                    .function
                    .chunk
                    .dissasemble_instruction(self.frame().ip);
            }

            let instr = self.read_opcode()?;

            match instr {
                Opcode::Return => {

                    let result = self.pop().unwrap();
                    let frame = self.frames.pop().unwrap();

                    // BUG?  we pop the frame but the stack still contains the frame-related values.
                    let stack_growth = self.stack.len() - frame.stack_start;

                    if cfg!(feature = "debug_output") {
                        println!("stack grew by {} (= {} - {}) values during call execution", stack_growth, self.stack.len(), frame.stack_start);
                    }

                    for _ in 0 .. stack_growth {
                        self.pop();
                    }
                    
                    if cfg!(feature = "debug_output") {
                        println!("===== returning result \x1b[1m{} from frame {:?}\x1b[0m\n", result, frame.closure.function.name);
                    }

                    if self.frames.len() == 0 {
                        self.pop();
                        return Ok(());
                    }

                    self.push(result);
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
                    self.pop();
                    //println!("directPOP {:?} off the stack", self.pop());
                }
                Opcode::DefineGlobal => {
                    let name = self.read_constant()?.as_string().unwrap();
                    let value = self.pop().unwrap();
                    //println!("DefineGlobal read_constant as_string: {}", name);
                    self.globals.insert(name.clone(), value);
                }
                Opcode::GetGlobal => {
                    let name = self.read_constant()?.as_string().unwrap();
                    println!("{:?}", self.globals);
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
                    let value = self.peek(0).unwrap().clone();
                    let offset: usize = self.frame().stack_start + slot as usize + 1; // Slot 0 is the current fn?
                    self.stack[offset] = value;
                    // todo: abstract this into `peek` already..
                }
                Opcode::GetLocal => {
                    let slot = self.read_u8();
                    let offset = self.frame().stack_start + slot as usize + 1;
                    self.push(self.stack[offset].clone());
                }
                Opcode::JumpIfFalse => {
                    let offset = self.read_short();

                    if self.stack.last().unwrap().is_falsey() {
                        if cfg!(feature = "debug_output") {
                            println!("> jumping, stack top was falsey");
                        }
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

                    //println!("argc: {};; stack: {:?}", argc, self.stack);
                    // missing?? function on stack
                    self.callValue(self.peek((argc) as usize).unwrap().clone(), argc);
                    //self.trace_stack();

                    // println!("popped frame after Opcode::Call: {:.10}", format!("{:?}", self.frames.pop())); // this ain't right, we're popping straight after the call before we give the fn a chance to run
                }
                Opcode::Closure => {
                    let fun = self.read_constant().unwrap().as_function().unwrap();
                    let closure = Closure::new(fun);
                    self.push(Object::Closure(closure));
                }
                Opcode::GetUpvalue => unimplemented!(),
                Opcode::SetUpvalue => unimplemented!(),
            }
        }

        return Ok(());
    }

    fn callValue(&mut self, callee: Value, argc: u8) -> bool {
        match callee {
            /*
            Value::LoxFunction(lf) => {
                // call(ObjFunction* function, int argCount) 
                // create callframe, load it with the function, set the ip to chunk.code
                // frame->slots = vm.stackTop - argCount - 1;
                if cfg!(feature = "debug_output") {
                    println!("\n===== calling \x1b[1m{}\x1b[0m with {} args", lf.name, argc);
                }
                let ip = lf.chunk.instr.len();
                let mut frame = CallFrame::new(lf);
                frame.ip = 0;
                frame.stack_start = self.stack.len() - (argc + 1) as usize;

                self.frames.push(frame);

                
            }, */
            Value::Closure(c) => {
                if cfg!(feature = "debug_output") {
                    println!("\n===== calling \x1b[1m{}\x1b[0m with {} args", c.function.name, argc);
                }
                let ip = c.function.chunk.instr.len();
                let mut frame = CallFrame::new(c);
                frame.ip = 0;
                frame.stack_start = self.stack.len() - (argc + 1) as usize;

                self.frames.push(frame);
            }
            e => { panic!("Can't call object {:?}", e) }
        }

        return false;
    }

    fn peek(&self, offset: usize) -> Option<&Value> {
        //println!("{} - 1 - {}", self.stack.len(), offset);
        //println!("peeking offset {}={}", offset, self.stack.len() - 1 - offset);
        if offset > self.stack.len() {
            return None;
        }

        self.stack.get(self.stack.len() - 1 - offset)
    }

    fn read_constant(&mut self) -> Result<Value, InterpretError> {
        let idx = self.read_u8();
        let constant = self.frame().closure.function.chunk.constants[idx as usize].clone();

        Ok(constant)
    }

    fn push(&mut self, value: Value) {
        if cfg!(feature = "debug_output") {
            println!("! push: {:?}", value);
        }

        self.stack.push(value)
    }

    fn pop(&mut self) -> Option<Value> {
        let p = self.stack.pop();
        if cfg!(feature = "debug_output") {
            println!("? popd {:?}", p);
        }

        p
    }

    fn trace_stack(&self) {
        println!("current frame stack starts at idx {}", self.frame().stack_start);
        for (i, v) in self.stack.iter().enumerate() {
            println!("{}:\t [ {:?} ]", i, v);
        }
    }
}
