use interpreter::scanner::*;
use interpreter::parser::*;

pub mod vm;
pub mod compiler;
pub mod value;
pub mod chunk;

use crate::vm::VM;
use crate::compiler::Compiler;
use crate::chunk::*;

fn compile(source: String) -> Result<LoxFunction, String> {
    let scanner = Scanner::new(&source);
    let tokens = scanner.scan_tokens();
    let mut parser = Parser::new(tokens);
    let stmts = parser.parse()?;
    let mut compiler = Compiler::new();

    println!("parsed code::\n{:?}\n\n", stmts);

    for stmt in stmts {
        stmt.visit(&mut compiler);
    }

    Ok(compiler.end_compiler())
}

fn main() {
    let mut vm = VM::new();
    let fun = match compile("var n = 10; var i = 0; while (i < n) { i = i + 1; print i; }".to_string()) {
        Ok(c) => c,
        Err(s) => panic!("{}", s),
    };

    /*
        TODO: move the compile method to the VM
        push the resulting LoxFunction to the stack,
        wrap that LoxFunction in a callframe and push to the framestack
    */

    println!("{:?}", vm);

    let res = vm.run();
    println!("{:?}", res);
}  