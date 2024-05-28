

pub mod vm;
pub mod compiler;
pub mod value;
pub mod chunk;

use crate::vm::VM;

use crate::chunk::*;


fn main() {
    let mut vm = VM::new();
    let res = vm.interpret("fun b() { print \"B??\"; } fun a() { print \"A!!\"; } a();");

    /*
        TODO: move the compile method to the VM
        push the resulting LoxFunction to the stack,
        wrap that LoxFunction in a callframe and push to the framestack
    */

    println!("{:?}", vm);
    println!("{:?}", res);
}  