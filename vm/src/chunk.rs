use crate::value::Object;
use crate::vm::Opcode;

type Value = Object;

#[derive(Clone, Debug)]
pub struct Upvalue {
    pub is_local: bool,
    pub index: u8,
}

// in clox, this gets stored as an Object in the constants(?)
// TODO: how can I align this with LoxFunction?
#[derive(Clone)]
pub struct LoxFunction {  // todo: good new-wrapper to replace all these pub declarations
    pub arity: usize,  
    pub chunk: Chunk,
    pub name: String, // Option<String>?
}

impl std::fmt::Debug for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

#[derive(Debug)]
pub struct LoxFunctionBuilder {
    pub chunk: Chunk,
    pub locals: Vec<(String, usize)>, // todo: ??
    pub arity: usize,
    pub name: String, 
    pub upvalues: Vec<Upvalue>,
}

impl LoxFunctionBuilder {
    pub fn new(name: String) -> Self { LoxFunctionBuilder { chunk: Chunk::new(), locals: Vec::new(), arity: 0, name: name, upvalues: Vec::new() } }

    pub fn build(self) -> LoxFunction {
        LoxFunction {
            arity: self.arity,
            chunk: self.chunk,
            name: self.name,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub function: LoxFunction,
}

impl Closure {
    pub fn new(function: LoxFunction) -> Self {
        Closure {
            function: function
        }
    }
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub instr: Vec<u8>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self { Chunk { instr: vec![], constants: vec![] } }

    pub fn emit(&mut self, op: u8) {
        self.instr.push(op)
    }

    pub fn emit_const(&mut self, c: u8) { // todo: chunk_write_const(VALUE) ?
        self.instr.push(Opcode::Constant.into());
        self.instr.push(c)
    }

    pub fn emit_const_value(&mut self, value: Value) -> u8 {
        self.constants.push(value);
        return (self.constants.len() - 1) as u8
    }
    
    /*
        backtracking: we don't know how long a conditional statement's body is,
        so we don't know how far to jump at the point of emitting the JMP instr.
        so we pour in a placeholder and return a pointer to said placeholder for the compiler to later fill in 
    */ 
    pub fn emit_jump(&mut self, op: u8) -> u16 {
        self.emit(op);
        self.emit(0xff);  // 16-bit offset: jump over up to 64K bytes of code
        self.emit(0xff);

        return self.instr.len() as u16 - 2;
    }

    pub fn patch_jump(&mut self, offset: u16) {
        let jmp = self.instr.len() as u16 - offset - 2; // jump dest

        self.instr[offset as usize] = (jmp >> 8) as u8;
        self.instr[offset as usize + 1] = (jmp & 0xff) as u8;
    }

    pub fn emit_loop(&mut self, loop_start: usize) {
        self.emit(Opcode::Loop.into());

        // jump back loop_start bytes, to before the condition, causing it to re-evaluate
        // +2 to include the Loop Opcode emitted here.
        let offset = self.instr.len() - loop_start + 2; 

        self.emit((offset >> 8) as u8);
        self.emit(offset as u8);
    }

    pub fn dissasemble_instruction(&self, offset: usize) { // the chunk doesn't know enough about the environment for this but in debug.c we only look at the constant offsets and not the values
        let op = Opcode::try_from(self.instr[offset]).unwrap();

        print!("| {:04}: {:?}", offset, op);

        match op {
            Opcode::Constant => {
                let idx = self.instr[offset + 1];
                let value = &self.constants[idx as usize];
                print!("({:02}): {:?}", idx, value);
            },
            Opcode::GetGlobal | Opcode::SetGlobal | Opcode::DefineGlobal => {
                let idx = self.instr[offset + 1];
                let name = &self.constants[idx as usize].as_string().unwrap();
                print!("({:02}): {}", idx, name);
            },
            Opcode::SetLocal | Opcode::GetLocal => {
                let idx = self.instr[offset + 1];
                
                print!("({:02})", idx);
            }
            /*Opcode::Add | Opcode::Divide | Opcode::Multiply | Opcode::Subtract => {
                let idx_a = self.instr[offset + 1];
                let value_a = &self.constants[idx_a as usize];
                let idx_b = self.instr[offset + 2];
                let value_b = &self.constants[idx_b as usize];

                print!("({:02}): {:?}, ({:02}): {:?}", idx_a, value_a, idx_b, value_b);
            }  */
            _ => {},
        }

        println!("");
    }
}
