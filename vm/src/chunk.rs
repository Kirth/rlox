use crate::value::Object;
use crate::vm::Opcode;

type Value = Object;


// in clox, this gets stored as an Object in the constants(?)
// TODO: how can I align this with LoxFunction?
#[derive(Debug, Clone)]
pub struct LoxFunction {  // todo: good new-wrapper to replace all these pub declarations
    pub arity: usize,  
    pub chunk: Chunk,
    pub name: String,
}

pub struct LoxFunctionBuilder {
    pub chunk: Chunk,
    pub locals: Vec<(String, usize)>, // todo: does this also need to go in VmFunction??
}

impl LoxFunctionBuilder {
    pub fn new() -> Self { LoxFunctionBuilder { chunk: Chunk::new(), locals: Vec::new() } }

    pub fn build(self) -> LoxFunction {
        LoxFunction {
            arity: 0,
            chunk: self.chunk,
            name: "root".to_string(),
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

    pub fn dissasemble_instruction(&self, offset: usize) {
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
