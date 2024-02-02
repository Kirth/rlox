#![allow(non_camel_case_types)]


// In pass 1: I will not add any extra bells and whistles to Lox
use core::panic;
use std::borrow::Borrow;

use crate::interpreter::*;
use crate::parser::*;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
    COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,

    BANG, BANG_EQ, 
    EQ, EQ_EQ,
    GT, GT_EQ,
    LS, LS_EQ,

    Identifier(String),
    String(String),
    Number(f64),

    AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
    PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,

    EOF, Error(String), Comment(String)
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::LEFT_PAREN => write!(f, "LEFT_PAREN"),
            Token::RIGHT_PAREN => write!(f, "RIGHT_PAREN"),
            Token::LEFT_BRACE => write!(f, "LEFT_BRACE"),
            Token::RIGHT_BRACE => write!(f, "RIGHT_BRACE"),
            Token::COMMA => write!(f, "COMMA"),
            Token::DOT => write!(f, "DOT"),
            Token::MINUS => write!(f, "MINUS"),
            Token::PLUS => write!(f, "PLUS"),
            Token::SEMICOLON => write!(f, "SEMICOLON"),
            Token::SLASH => write!(f, "SLASH"),
            Token::STAR => write!(f, "STAR"),
            Token::BANG => write!(f, "BANG"),
            Token::BANG_EQ => write!(f, "BANG_EQ"),
            Token::EQ => write!(f, "EQ"),
            Token::EQ_EQ => write!(f, "EQ_EQ"),
            Token::GT => write!(f, "GT"),
            Token::GT_EQ => write!(f, "GT_EQ"),
            Token::LS => write!(f, "LS"),
            Token::LS_EQ => write!(f, "LS_EQ"),
            Token::Identifier(ref s) => write!(f, "Identifier({})", s),
            Token::String(ref s) => write!(f, "String({})", s),
            Token::Number(n) => write!(f, "Number({})", n),
            Token::AND => write!(f, "AND"),
            Token::CLASS => write!(f, "CLASS"),
            Token::ELSE => write!(f, "ELSE"),
            Token::FALSE => write!(f, "FALSE"),
            Token::FUN => write!(f, "FUN"),
            Token::FOR => write!(f, "FOR"),
            Token::IF => write!(f, "IF"),
            Token::NIL => write!(f, "NIL"),
            Token::OR => write!(f, "OR"),
            Token::PRINT => write!(f, "PRINT"),
            Token::RETURN => write!(f, "RETURN"),
            Token::SUPER => write!(f, "SUPER"),
            Token::THIS => write!(f, "THIS"),
            Token::TRUE => write!(f, "TRUE"),
            Token::VAR => write!(f, "VAR"),
            Token::WHILE => write!(f, "WHILE"),
            Token::EOF => write!(f, "EOF"),
            Token::Error(ref s) => write!(f, "Error({})", s),
            Token::Comment(ref s) => write!(f, "Comment({})", s),
        }
    }
}

#[derive(Debug)]
pub struct TokenLoc {
    pub token: Token,
    pub line: usize,
    pub column: usize
}

pub struct Scanner<'a> {
    source: String,
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    tokens: Vec<TokenLoc>,
    position: usize,
    line: usize,
    column: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner {
            source: source.to_string(), // copy this thing because ... ?
            chars: source.chars().peekable(),
            tokens: vec![],
            position: 0,
            line: 1,
            column: 0,
        }
    }

    pub fn scan_tokens(mut self) -> Vec<TokenLoc> {
        while !self.is_at_end() {
            self.scan_token();
        }

        self.tokens.push(TokenLoc { token: Token::EOF, line: 0, column: 0 });
        return self.tokens
    }

    fn is_at_end(&self) -> bool {
        //println!("pos: {}, len: {}", self.position, self.source.len());
        return self.position >= self.source.len()
    }

    fn scan_token(&mut self) {
        // eat all whitespace first
        while let Some(&c) = self.chars.peek() {
            if !c.is_whitespace() {
                break;
            }

            if c == '\n' {
                self.line += 1;
                self.column = 1;
            }

            self.advance();
        }

        let token = match self.advance() {
            '(' => Token::LEFT_PAREN,
            ')' => Token::RIGHT_PAREN,
            '{' => Token::LEFT_BRACE,
            '}' => Token::RIGHT_BRACE,
            ',' => Token::COMMA,
            '.' => Token::DOT,
            '-' => Token::MINUS,
            '+' => Token::PLUS,
            ';' => Token::SEMICOLON,
            '*' => Token::STAR,
            '!' => if self.next_is('=') { self.advance(); Token::BANG_EQ } else { Token::BANG},
            '=' => if self.next_is('=') { self.advance(); Token::EQ_EQ } else { Token::EQ },
            '<' => if self.next_is('=') { self.advance(); Token::LS_EQ } else { Token::LS},
            '>' => if self.next_is('=') { self.advance(); Token::GT_EQ } else { Token::GT},
            '/' => {
                if self.next_is('/') { // comment!
                    while self.chars.peek() != Some(&'\n') && !self.is_at_end() {
                        self.advance();
                    }

                    Token::Comment("i did not capture the comment :3".to_string())
                } else {
                    Token::SLASH
                }
            },
            '"' => self.string(),
            c if c.is_numeric() => self.number(c),
            c if c.is_alphanumeric() => self.identifier_or_keyword(c),
            c => Token::Error(format!("Uknown token {}", c)),
        };

        self.add_token(token)
    }

    fn string(&mut self) -> Token {
        let mut acc = String::new();
        while let Some(&c) = self.chars.peek() {
            if c == '"' {
                self.advance();
                break;
            }

            if self.is_at_end() {
                return Token::Error("Unterminated string".to_string())
            }

            acc.push(self.advance());
        }

        return Token::String(acc)
    }

    fn number(&mut self, first: char) -> Token {
        let mut acc = String::new();
        acc.push(first);

        while let Some(&c) = self.chars.peek() {
            if !c.is_numeric() && c != '.' {
                break;
            }

            acc.push(self.advance());
        }

        Token::Number(acc.parse::<f64>().unwrap())
    }

    fn identifier_or_keyword(&mut self, first: char) -> Token {
        let token = self.identifier(first);
        if let Token::Identifier(s) = token {
            return match s.to_lowercase().as_str() {
                "and" => Token::AND,
                "class" => Token::CLASS,
                "else" => Token::ELSE,
                "false" => Token::FALSE,
                "fun" => Token::FUN,
                "for" => Token::FOR,
                "if" => Token::IF,
                "nil" => Token::NIL,
                "or" => Token::OR,
                "print" => Token::PRINT,
                "return" => Token::RETURN,
                "super" => Token::SUPER,
                "this" => Token::THIS,
                "true" => Token::TRUE,
                "var" => Token::VAR,
                "while" => Token::WHILE,
                _ => Token::Identifier(s)
            }
        } else {
            return Token::Error(format!("expected to have parsed an identifer, but got a different token variant: {:?}", token));
        }

    }

    fn identifier(&mut self, first: char) -> Token {
        let mut acc = String::new();
        acc.push(first);

        while let Some(&c) = self.chars.peek() {
            if !c.is_alphanumeric() {
                break;
            }

            acc.push(self.advance());
        }

        Token::Identifier(acc)
    }

    fn advance(&mut self) -> char {
        self.position += 1;
        self.column += 1;
        return self.chars.next().unwrap()
    }

    fn add_token(&mut self, token: Token) {
        //println!("{:?}", token);
        self.tokens.push(TokenLoc {
            token: token,
            line : self.line,
            column: self.column,
        })
    }

    fn next_is(&mut self, expected: char) -> bool {
        if self.chars.peek() != Some(&expected) {
            return false;
        }

        self.position += 1;
        return true;
    }
}