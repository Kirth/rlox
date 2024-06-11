pub mod vm;
pub mod compiler;
pub mod value;
pub mod chunk;

use crate::vm::VM;
use crate::chunk::*;


fn main() {
    let mut vm = VM::new();
    let res = vm.interpret("fun t(n) { fun v(n) { return n * 30; } print 3 + 1; print n * 2; print n; return v(n) * 10; } print t(5);");

    println!("{:?}", vm);
    println!("{:?}", res);
}  