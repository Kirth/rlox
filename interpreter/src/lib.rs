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

use clap::{App, Arg};
use std::io::{self, Write};

pub struct Lox {
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

    pub fn run(&mut self, source: String) {
        let scanner = Scanner::new(&source);
        let tokens = scanner.scan_tokens();

        let mut parser = Parser::new(tokens);
        let stmts = parser.parse();

        match stmts {
            Ok(stmts) => {
                //let mut printer = AstPrinter {};
                //printer.print(&stmts);

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