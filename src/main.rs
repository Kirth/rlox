#![allow(non_camel_case_types)]


// In pass 1: I will not add any extra bells and whistles to Lox
use core::panic;
use std::borrow::Borrow;

pub mod interpreter;
pub mod parser;
pub mod scanner;

use crate::interpreter::*;
use crate::scanner::*;
use crate::parser::*;

struct Lox {
    had_error: bool
}

impl Lox {
    fn error(line: i32, message: &str) {
        Lox::report(line, &"", message)
    }

    fn report(line: i32, loc: &str, msg: &str) {
        println!("[line {}] Error {}: {}", line, loc, msg)
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

                let mut interpreter = Interpreter::new();
                interpreter.interpret(stmts)
                //println!("{:?}", stmts.visit(&mut interpreter));
                
            },
            Err(s) => { print!("Error while parsing: {}", s)}
        }
    }
}

fn main() {

    let path = "examples/for_loop.lox";
    let mut lox = Lox { had_error: false };

    match std::fs::read_to_string(path) {
        Ok(script) => {
            println!("running script:\n{}\n=======================================", &script);
            lox.run(script);
            // if parse/input error exit 65
            // if runtime error exit 70
        }, 
        Err(e) => {
            eprintln!("Failed to read file: {}", e);
            std::process::exit(2);
        }
    }

    // todo: exit 64 if it's not fed a file


    
    
}