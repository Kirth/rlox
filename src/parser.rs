#![allow(non_camel_case_types)]


// In pass 1: I will not add any extra bells and whistles to Lox
use core::panic;
use std::borrow::Borrow;
use std::fmt::format;

use crate::interpreter::*;
use crate::scanner::*;

pub struct Parser {
    tokens: Vec<TokenLoc>,
    current: usize,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Call(Box<Expr>, Token, Vec<Expr>),
    Unary(Token, Box<Expr>),
    Literal(Object),
    Logical(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Variable(String),
    Assign(String, Box<Expr>)
}

impl Expr {
    pub fn visit<T>(&self, visitor: &mut dyn ExprVisitor<T>) -> T {
        match self {
            Expr::Binary(left, op, right) => visitor.visit_binary(left, op, right),
            Expr::Unary(op, right) => visitor.visit_unary(op, right),
            Expr::Literal(value) => visitor.visit_literal(value),
            Expr::Logical(left, op, right) => visitor.visit_logical(left, op, right),
            Expr::Grouping(expr) => visitor.visit_grouping(expr),
            Expr::Variable(name) => visitor.visit_variable(name),
            Expr::Assign(name, value) => visitor.visit_assign(name, value),
            Expr::Call(callee, paren, args) => visitor.visit_call(callee, paren, args),
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary(left, op, right) => write!(f, "Binary({}, {}, {})", left, op, right),
            Expr::Unary(op, expr) => write!(f, "Unary({}, {})", op, expr),
            Expr::Literal(obj) => write!(f, "Literal({})", obj),
            Expr::Logical(left, op, right) => write!(f, "Logical({}, {}, {})", left, op, right),
            Expr::Grouping(expr) => write!(f, "Grouping({})", expr),
            Expr::Variable(name) => write!(f, "Variable({})", name),
            Expr::Assign(name, expr) => write!(f, "Assign({}, {})", name, expr),
            Expr::Call(callee, paren, args) => write!(f, "Call({}({:?}))", callee, args),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression(Box<Expr>),
    Print(Box<Expr>),
    Var(String, Box<Expr>),
    Block(Vec<Stmt>),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    While(Box<Expr>, Box<Stmt>),
    Function(String, Vec<String>, Box<Stmt>),
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expression(expr) => write!(f, "Expression({})", expr),
            Stmt::Print(expr) => write!(f, "Print({})", expr),
            Stmt::Var(name, expr) => write!(f, "Var({}, {})", name, expr),
            Stmt::Block(statements) => {
                write!(f, "Block(")?;
                for stmt in statements {
                    write!(f, "{}, ", stmt)?;
                }
                write!(f, ")")
            },
            Stmt::If(condition, then_branch, else_branch) => {
                write!(f, "If({}, {}, ", condition, then_branch)?;
                match else_branch {
                    Some(branch) => write!(f, "Else({})", branch),
                    None => write!(f, "Else(None)"),
                }
            },
            Stmt::While(condition, body) => {
                write!(f, "While({}, {}, ", condition, body)
            },
            Stmt::Function(name, args, body) => {
                write!(f, "Fn:{}({:?})", name, args)
            }
        }
    }
}

impl Stmt {
    pub fn visit(&self, visitor: &mut dyn StmtVisitor) { // statements produce no values
        match self {
            Stmt::Expression(e) => visitor.visit_expression(e), // todo: does it make sense to not peel this off here?
            Stmt::If(condition, if_stmt, else_stmt) => visitor.visit_if(condition, if_stmt, else_stmt),
            Stmt::While(condition, body) => visitor.visit_while(condition, body),
            Stmt::Print(e) => visitor.visit_print(e),
            Stmt::Var(name, initializer) => visitor.visit_var(name, initializer),
            Stmt::Block(stmts) => visitor.visit_block(stmts),
            Stmt::Function(name, params, body) => visitor.visit_function(name, params, body),
        }
    }
}

impl Parser {
    pub fn new(tokens: Vec<TokenLoc>) -> Self {
        Parser {
            tokens: tokens,
            current: 0
        }
    }

    fn expression(&mut self) -> Result<Box<Expr>, String> {
        self.assignment()
    }

    fn equality(&mut self) -> Result<Box<Expr>, String> {
        let mut e = self.comparison()?;

        while self.expect_token_type_one_of(vec![Token::EQ_EQ, Token::BANG_EQ]) {
            let op = self.previous().token.clone();
            let right = self.comparison()?;
            e = Box::new(Expr::Binary(e, op, right));
        }

        return Ok(e);
    }

    fn comparison(&mut self) -> Result<Box<Expr>, String> {
        let mut e = self.term()?;

        while self.expect_token_type_one_of(vec![Token::GT, Token::GT_EQ, Token::LS, Token::LS_EQ]) {
            let op = self.previous().token.clone();
            let right = self.term()?;
            e = Box::new(Expr::Binary(e, op, right));
        }

        return Ok(e);
    }

    fn term(&mut self) -> Result<Box<Expr>, String> {
        let mut e = self.factor()?;

        while self.expect_token_type_one_of(vec![Token::MINUS, Token::PLUS]) {
            let op = self.previous().token.clone();
            let right = self.factor()?;
            e = Box::new(Expr::Binary(e, op, right));
        }

        return Ok(e)
    }

    fn factor(&mut self) -> Result<Box<Expr>, String> {
        let mut e = self.unary()?;

        while self.expect_token_type_one_of(vec![Token::SLASH, Token::STAR]) {
            let op = self.previous().token.clone();
            let right = self.unary()?;
            e = Box::new(Expr::Binary(e, op, right));
        }

        return Ok(e)
    }

    fn unary(&mut self) -> Result<Box<Expr>, String> {
        if self.expect_token_type_one_of(vec![Token::BANG, Token::MINUS]) {
            let op = self.previous().token.clone();
            let right = self.unary()?;

            return Ok(Box::new(Expr::Unary(op, right)))
        }

        return self.call()
    }

    fn call(&mut self) -> Result<Box<Expr>, String> {
        let mut expr = self.primary();

        while true { // Why are we doing this?
            if self.expect_token_type_one_of(vec![Token::LEFT_PAREN]) {
                expr = self.finish_call(expr?);
            } else {
                break;
            }
        }

        return expr;
    }

    fn finish_call(&mut self, callee: Box<Expr>) -> Result<Box<Expr>, String> {
        let mut args = vec![];

        if !self.check(&Token::RIGHT_PAREN) {
            args.push(*self.expression()?);

            while self.expect_token_type_one_of(vec![Token::COMMA]) {
                if args.len() >= 255 {
                    return Err("Can't have more than 255 arguments.".to_string());
                }
                args.push(*self.expression()?);
            }
        }

        let paren = self.consume(&Token::RIGHT_PAREN, format!(" Expect ')' after argument list."));
        return Ok(Box::new(Expr::Call(callee, paren.unwrap().token.clone(), args)))
    }

    fn assignment(&mut self) -> Result<Box<Expr>, String> {
        let expr = self.or()?;

        if self.expect_token_type_one_of(vec![Token::EQ]) {
            let eq = self.previous();
            let value = *self.assignment()?;

            if let Expr::Variable(name) = *expr {
                return Ok(Box::new(Expr::Assign(name, Box::new(value))));
            }

            return Err(format!("Invalid left-hand assignment target: {:?}", &value)); // is this correct?  the book said not to throw it because there's no need to synchronize()
        }

        return Ok(expr);
    }

    fn or(&mut self) -> Result<Box<Expr>, String> {
        let mut expr = self.and()?;

        while self.expect_token_type_one_of(vec![Token::OR]) {
            let op = self.previous().token.clone();
            let right = self.and()?;
            expr = Box::new(Expr::Logical(expr, op, right));
        }

        return Ok(expr);
    }

    fn and(&mut self) -> Result<Box<Expr>, String> {
        let mut expr = self.equality()?;

        while self.expect_token_type_one_of(vec![Token::AND]) {
            let op = self.previous().token.clone();
            let right = self.equality()?;
            expr = Box::new(Expr::Logical(expr, op, right));
        }

        return Ok(expr);
    }

    fn primary(&mut self) -> Result<Box<Expr>, String> {

        if self.expect_token_type_one_of(vec![Token::FALSE]) {
            return Ok(Box::new(Expr::Literal(Object::Boolean(false))))
        }

        if self.expect_token_type_one_of(vec![Token::TRUE]) {
            return Ok(Box::new(Expr::Literal(Object::Boolean(true))))
        }

        if self.expect_token_type_one_of(vec![Token::NIL]) {
            return Ok(Box::new(Expr::Literal(Object::Nil)))
        }

        if self.expect_token_type_one_of(vec![Token::LEFT_PAREN]) {
            let e = self.expression()?;
            self.consume(&Token::RIGHT_PAREN, "Expect ')' after expression.".to_string())?;
            return Ok(Box::new(Expr::Grouping(e)))
        }

        if self.expect_token_type_one_of(vec![Token::String("".to_string()), Token::Number(1337.0)]) {
            match &self.previous().token {
                Token::String(s) => return Ok(Box::new(Expr::Literal(Object::String(s.clone())))),
                Token::Number(n) => return Ok(Box::new(Expr::Literal(Object::Number(*n)))),
                _ => { panic!("invariant failure") }
            }
        }

        if self.expect_token_type_one_of(vec![Token::Identifier(format!(""))]) {
            match &self.previous().token {
                Token::Identifier(n) => return Ok(Box::new(Expr::Variable(n.to_string()))),
                _ => { panic!("invariant failure!") }
            }
        }

        println!("primary did not generate; peek: {:?}", self.peek());
        println!("returning an empty nill");

        return Ok(Box::new(Expr::Literal(Object::Nil))) // TODO: is this an error?
    }

    fn consume(&mut self, token_type: &Token, msg: String) -> Result<&TokenLoc, String> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(msg)
        }
       
    }

    fn expect_token_type_one_of(&mut self, types: Vec<Token>) -> bool {
        for t in types {
            if self.check(&t) {
                self.advance();
                return true;
            }
        }

        return false;
    }

    fn check(&self, token: &Token) -> bool { // compare tagged enum's discriminants to compare type but not contained value
        // println!("check: {} against {}: {:?} - {:?}: {}", 
        //     &self.peek().token, &token, std::mem::discriminant(token), std::mem::discriminant(&self.peek().token), 
        //     std::mem::discriminant(token) == std::mem::discriminant(&self.peek().token));

        std::mem::discriminant(token) == std::mem::discriminant(&self.peek().token) 
    }

    fn advance(&mut self) -> &TokenLoc {
        if !self.is_at_end() {
            self.current += 1;
        }

        return self.previous()
    }

    fn is_at_end(&self) -> bool {
        return self.peek().token == Token::EOF;
    }

    fn peek(&self) -> &TokenLoc {
        return self.tokens.get(self.current).unwrap();
    }

    fn previous(&self) -> &TokenLoc {
        return self.tokens.get(self.current - 1).unwrap();
    }

    fn statement(&mut self) -> Result<Stmt, String> {
        //println!("parsing statement, peek() is: {:?}", self.peek());
        if self.expect_token_type_one_of(vec![Token::IF]) {
            self.if_statement()       
        } else if self.expect_token_type_one_of(vec![Token::PRINT]) {
            self.print_statement()
        } else if self.expect_token_type_one_of(vec![Token::WHILE]) {
            self.while_statement()
        } else if self.expect_token_type_one_of(vec![Token::FOR]) {
            self.for_statement()
        } else if self.expect_token_type_one_of(vec![Token::LEFT_BRACE]) {
            self.block()
        } else {
            self.expression_statement()
        }
    }

    fn block(&mut self) -> Result<Stmt, String> {
        let mut stmts = vec![];

        while !self.check(&Token::RIGHT_BRACE) && !self.is_at_end() {
            stmts.push(self.declaration()?);
        }

        //println!("debug block: after check, peek is {:?}", self.peek());

        self.consume(&Token::RIGHT_BRACE, format!("Expect '}}' after block."));
        //println!("debug block: after consume, peek is {:?}", self.peek());
        return Ok(Stmt::Block(stmts));
    }

    fn print_statement(&mut self) -> Result<Stmt, String> {
        let value = self.expression()?;
        self.consume(&Token::SEMICOLON, format!("Expect ';'after value."))?;

        return Ok(Stmt::Print(value))
    }
    
    fn expression_statement(&mut self) -> Result<Stmt, String> {
        let expr = self.expression()?;
        self.consume(&Token::SEMICOLON, format!("Expect ';' after print-expression"))?;

        return Ok(Stmt::Expression(expr))
    }

    fn if_statement(&mut self) -> Result<Stmt, String> {
        self.consume(&Token::LEFT_PAREN, format!("Expect '(' after 'if'."));
        let condition = self.expression()?;
        self.consume(&Token::RIGHT_PAREN,  format!("Expect ')' after 'if' condition."));

        let then_branch = Box::new(self.statement()?);
        let else_branch = if self.expect_token_type_one_of(vec![Token::ELSE]) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        return Ok(Stmt::If(condition, then_branch, else_branch))
    }

    fn for_statement(&mut self) -> Result<Stmt, String> {
        self.consume(&Token::LEFT_PAREN, format!("Expect '(' after 'for'."));
        let initializer = if self.check(&Token::SEMICOLON) {
            None
        } else if (self.expect_token_type_one_of(vec![Token::VAR])) {
            Some(self.var_declaration()?) // chased this being "self.assign" for way too long, how come Expr and Stmt fit in the same?
        } else {
            Some(self.expression_statement()?) // .. becuase this was self.expression() and not the stmt.
        };
        self.consume(&Token::SEMICOLON, format!("Expect ';' after for-loop initiator."));

        let condition = if self.check(&Token::SEMICOLON) {
            Box::new(Expr::Literal(Object::Boolean(true)))  
        } else {
            self.expression()?
        };
        self.consume(&Token::SEMICOLON, format!("Expect ';' after for-loop condition."));

        let increment = if !self.check(&Token::RIGHT_PAREN) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(&Token::RIGHT_PAREN, format!("Expect ')' after 'for'."));

        let mut body = self.statement()?;

        if let Some(increment) = increment {
            body = Stmt::Block(vec![body, Stmt::Expression(increment)]);
        }

        body = Stmt::While(condition, Box::new(body));

        if let Some(initializer) = initializer {
            body = Stmt::Block(vec![initializer, body]);
        }        

        return Ok(body)
    }

    fn while_statement(&mut self) -> Result<Stmt, String> {
        self.consume(&Token::LEFT_PAREN, format!("Expect '(' after 'while'."));
        let condition = self.expression()?;
        self.consume(&Token::RIGHT_PAREN, format!("Expect ')' after 'while'."));
        let body = Box::new(self.statement()?);

        return Ok(Stmt::While(condition, body))
    }

    fn var_declaration(&mut self) -> Result<Stmt, String> {
        let name = if let Token::Identifier(name) = 
            self.consume(&Token::Identifier(format!("")), format!("Expect variable name"))?.token.clone()  { name 
        } else {
            format!("boo boo pants")
        };

        let mut initializer = None;

        if self.expect_token_type_one_of(vec![Token::EQ]) {
            initializer = Some(self.expression()?);
        }

        self.consume(&Token::SEMICOLON, format!("Expect ';' after variable declaration."));

        return Ok(Stmt::Var(name, initializer.unwrap_or(Box::new(Expr::Literal(Object::Nil)))));
    }

    fn function(&mut self, kind: String) -> Result<Stmt, String> {
        let fn_name = {
            let fn_name = self.consume(&Token::Identifier("".to_string()), format!("Expect {} name.", kind))?;
            if let Token::Identifier(name) = fn_name.token.clone() {
                name
            } else {
                "None".to_string() // todo: Option?
            }
        };

        

        let mut params = vec![];
        self.consume(&Token::LEFT_PAREN, format!("Expect '(' before {} name", kind));

        if !self.check(&Token::RIGHT_BRACE) {
            println!("{:?}", self.peek());
            let tloc: &TokenLoc = self.consume(&Token::Identifier("".to_string()), "1st Expect parameter name".to_string())?;
            if let Token::Identifier(name) = tloc.token.clone() {
                params.push(name);
            }

            while self.expect_token_type_one_of(vec![Token::COMMA]) {
                let tloc: &TokenLoc = self.consume(&Token::Identifier("".to_string()), "2nd Expect parameter name".to_string())?;
                if let Token::Identifier(name) = tloc.token.clone() {
                    params.push(name);
                }    
            }
        }

        self.consume(&Token::RIGHT_PAREN, format!("Expect ')' after {} parameters", kind));
        self.consume(&Token::LEFT_BRACE, format!("Expect '{{' before {} body", kind));
        let body = self.block()?;

        return Ok(Stmt::Function(fn_name, params, Box::new(body)))
    }

    fn declaration(&mut self) -> Result<Stmt, String> {
        if self.expect_token_type_one_of(vec![Token::VAR]) {
            self.var_declaration()
        } else if self.expect_token_type_one_of(vec![Token::FUN]) {
            println!("Function!!");
            self.function(format!("function"))
        } else {
            self.statement()
        }

        // TODO: error handling, catch a parse error and synchronize();
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, String> {
        // self.expression() // - hack to get it running
        let mut statements = vec![];

        while !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        return Ok(statements)
    }
}