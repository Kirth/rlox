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

fn main() {
    let matches = App::new("rlox")
                        .about("Rust port of the Lox programming language (see craftinginterpreters.com)")
                        .arg(Arg::new("script_path")
                                .index(1)
                                .required(false)
                                .help(".lox script file to execute"))
                        .arg(Arg::new("persist")
                                .short('p')
                                .long("persist")
                                .required(false)
                                .takes_value(false)
                                .help("get a REPL after executing a script file")) // TODO :)
                        .get_matches();

    let mut lox = Lox::new();
    if let Some(path) = matches.value_of("script_path") {
        match std::fs::read_to_string(path) {
            Ok(script) => {
                /*println!(
                    "running script:\n{}\n=======================================",
                    &script
                );*/
                lox.run(script.trim().to_string());
                // if parse/input error exit 65
                // if runtime error exit 70
            }
            Err(e) => {
                eprintln!("Failed to read file: {}", e);
                std::process::exit(2);
            }
        }
    }
    
    if matches.is_present("persist") || !matches.is_present("script_path") {
        let mut input = String::new();
        loop {
            input.clear();
            print!("lox> ");
            io::stdout().flush().unwrap();

            match io::stdin().read_line(&mut input) {
                Ok(0) | Err(_) => { // Ctrl+D or read error
                    println!("Exiting REPL...");
                    break;
                }

                Ok(_) => {
                    let trimmed = input.trim();

                    if trimmed == "exit" {
                        println!("Exiting REPL...");
                        break;
                    }

                    lox.run(trimmed.to_string());
                }
            }
        }
    }
    
    // todo: exit 64 if it's not fed a file
}
