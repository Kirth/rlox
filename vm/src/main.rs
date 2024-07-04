pub mod vm;
pub mod compiler;
pub mod value;
pub mod chunk;

use crate::vm::VM;
use crate::chunk::*;


fn main() {
    let mut vm = VM::new();
    //let res = vm.interpret("fun t(a, b, c) { var a = 1; var b = 2; var c = 3; return a + b + c; } print t();");
    let res = vm.interpret(r#"
    { 
        var a = 3; 
        fun f() { 
            var b = 2;

            if (b > 1) {
                print "foo";
            } else {
                print "bar";
            }
        }
        f();
    }"#);
    /*let res = vm.interpret(r#"
    fun fib(n) { 
        if (n < 2) {
            return n; 
        }

        return fib(n - 2) + fib(n - 1);
    } 
    print fib(10);"#);*/

    //println!("{:?}", vm);
    //println!("{:?}", res);
}  