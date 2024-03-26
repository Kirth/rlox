#![allow(non_camel_case_types)]

// In pass 1: I will not add any extra bells and whistles to Lox
use core::panic;
use std::borrow::Borrow;

pub mod interpreter;
pub mod parser;
pub mod scanner;
pub mod resolver;

use crate::interpreter::*;
use crate::parser::*;
use crate::scanner::*;
use crate::resolver::*;

struct Lox {
    had_error: bool,
    interpreter: Interpreter,
}

impl Lox {
    pub fn new() -> Self {
        Lox {
            had_error: false,
            interpreter: Interpreter::new(),
        }
    }

    fn error(line: i32, message: &str) {
        Lox::report(line, &"", message)
    }

    fn report(line: i32, loc: &str, msg: &str) {
        println!("[line {}] Error {}: {}", line, loc, msg)
    }

    fn run_stmts(&mut self, stmts: Vec<Stmt>) {
        self.interpreter.interpret(stmts)
    }

    fn run(&mut self, source: String) {
        let scanner = Scanner::new(&source);
        let tokens = scanner.scan_tokens();

        let mut parser = Parser::new(tokens);
        let stmts = parser.parse();

        match stmts {
            Ok(stmts) => {
                let mut printer = AstPrinter {};
                printer.print(&stmts);

                let mut resolver = Resolver::new(&mut self.interpreter);
                resolver.resolve_stmts(&stmts);

                self.interpreter.interpret(stmts);
            }
            Err(s) => {
                print!("Error while parsing: {}", s)
            }
        }
    }
}

fn main() {
    let path = "examples/counter.lox";
    let mut lox = Lox::new();

    /*  lox.run_stmts(vec![
        Stmt::Print(Box::new(Expr::Literal(Object::String("Hello world!".to_string()))))
    ]); */

    match std::fs::read_to_string(path) {
        Ok(script) => {
            println!(
                "running script:\n{}\n=======================================",
                &script
            );
            lox.run(script);
            // if parse/input error exit 65
            // if runtime error exit 70
        }
        Err(e) => {
            eprintln!("Failed to read file: {}", e);
            std::process::exit(2);
        }
    }

    // todo: exit 64 if it's not fed a file
}
