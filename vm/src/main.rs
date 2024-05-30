

pub mod vm;
pub mod compiler;
pub mod value;
pub mod chunk;

use crate::vm::VM;

use crate::chunk::*;


fn main() {
    let mut vm = VM::new();
    let res = vm.interpret("fun a() { print \"huwwo\"; return 3; }\n a(); var v = a(); print v; print v + v;");

    println!("{:?}", vm);
    println!("{:?}", res);
}  