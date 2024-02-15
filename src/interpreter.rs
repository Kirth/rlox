#![allow(non_camel_case_types)]


// In pass 1: I will not add any extra bells and whistles to Lox


use std::cell::RefCell;
use std::rc::Rc;

use crate::scanner::*;
use crate::parser::*;


#[derive(Debug, Clone)]
pub struct LoxFunction { // this should contain the declaration
    name: Option<String>,
    declaration: Vec<Stmt>,
    // does this thing need its own environment/closure?
    is_method: bool,
}

impl LoxFunction {
    pub fn invoke(&self, interpreter: &mut Interpreter, args: Vec<Object>) {
        interpreter.execute_block(&self.declaration, interpreter.env.clone()) // TODO: correct environment
    }
}

impl LoxFunction {
    pub fn new(name: Option<String>, body: &Vec<Stmt>) -> Self {
        LoxFunction {
            name: name,
            declaration: body.clone(),
            is_method: false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Object {
    String(String),
    Number(f64),
    Boolean(bool),
    Callable(LoxFunction),
    Nil,
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::String(s) => write!(f, "'{}'", s),
            Object::Number(n) => write!(f, "{}", n),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Callable(func) => write!(f, "{}", func),
            Object::Nil => write!(f, "nil"),
        }
    }
}

impl std::fmt::Display for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.name {
            Some(name) => write!(f, "Function: {}, is_method: {}, declaration: {:?}", name, self.is_method, self.declaration),
            None => write!(f, "Anonymous Function, is_method: {}, declaration: {:?}", self.is_method, self.declaration),
        }
    }
}

pub trait ExprVisitor<T> {
    fn visit_binary(&mut self, left: &Expr, op: &Token, right: &Expr) -> T;
    fn visit_unary(&mut self, op: &Token, right: &Expr) -> T;
    fn visit_literal(&mut self, value: &Object) -> T;
    fn visit_logical(&mut self, left: &Expr, op: &Token, right: &Expr) -> T;
    fn visit_grouping(&mut self, expr: &Expr) -> T;
    fn visit_variable(&mut self, name: &String) -> T; // TODO: I don't want to copy variables on every use.
                                                      // this may mean wrapping them in RCs?  Do we even need ExprVisitor to be generic?,
    fn visit_assign(&mut self, name: &String, value: &Expr) -> T;
    fn visit_call(&mut self, callee: &Box<Expr>, paren: &Token, args: &Vec<Expr>) -> T;
}

pub trait StmtVisitor { // statements produce no values
    fn visit_expression(&mut self, expr: &Box<Expr>); // TODO: do I pass in boxes here?
    fn visit_print(&mut self, expr: &Box<Expr>);      //       does that make sense? what does it mean?
    fn visit_if(&mut self, condition: &Box<Expr>, if_stmt: &Box<Stmt>, else_stmt: &Option<Box<Stmt>>);
    fn visit_while(&mut self, condition: &Box<Expr>, body: &Box<Stmt>);
    fn visit_var(&mut self, name: &String, initializer: &Box<Expr>);
    fn visit_block(&mut self, stmts: &Vec<Stmt>);
    fn visit_function(&mut self, name: &String, params: &Vec<String>, body: &Box<Stmt>);
}

#[derive(Debug, Clone)]
pub struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    values: std::collections::HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: std::collections::HashMap::new(),
            enclosing: None
        }
    }

    pub fn with_enclosing(enclosing: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment {
            values: std::collections::HashMap::new(),
            enclosing: Some(enclosing) // whoops it's easy to accidentally double-clone an Rc
        }))
    }

    pub fn define(&mut self, name: String, value: Object) -> Option<Object> {
        self.values.insert(name, value)
    }

    pub fn assign(&mut self, name: String, value: Object) -> Option<Object> {
        if self.values.contains_key(&name) {
            return self.values.insert(name, value);
        }

        if let Some(enclosing) = &self.enclosing {
            let mut enclosing = enclosing.borrow_mut(); 
            return enclosing.assign(name, value);
        }

        return None;
    }
    

    pub fn get(&mut self, name: &String) -> Result<Object, String> {
        if let Some(obj) = self.values.get(name) {
            return Ok(obj.clone()) // TODO: don't clone
        } else {
            if self.enclosing.is_some() {
                return self.enclosing.as_mut().unwrap().borrow_mut().get(name);
            } else {
                return Err(format!("Undefined variable '{}'.", name))
            }
        }
    }

    pub fn all_keys(&self) -> Vec<String> {
        let mut keys = vec![];
        if let Some(enclosing) = &self.enclosing {
            let enclosing = enclosing.borrow();

            keys.append(&mut enclosing.all_keys());
        }

        for k in self.values.keys() {
            keys.push(k.to_string());
        }

        return keys;
    }
}

pub struct AstPrinter;

impl ExprVisitor<String> for AstPrinter {
    fn visit_binary(&mut self, left: &Expr, op: &Token, right: &Expr) -> String {
        self.parenthesize(format!("{}", op), &[left, right])
    }

    fn visit_unary(&mut self, op: &Token, right: &Expr) -> String {
        self.parenthesize(format!("{}", op), &[right])
    }

    fn visit_grouping(&mut self, expr: &Expr) -> String {
        self.parenthesize("group".to_string(), &[expr])
    }

    fn visit_literal(&mut self, value: &Object) -> String {
        value.to_string()
    }

    fn visit_logical(&mut self, left: &Expr, op: &Token, right: &Expr) -> String {
        self.parenthesize(format!("{}", op), &[left, right])
    }

    fn visit_variable(&mut self, name: &String) -> String {
        name.to_string()
    }

    fn visit_assign(&mut self, name: &String, value: &Expr) -> String {
        self.parenthesize(format!("assign {}", name), &[value])
    }

    fn visit_call(&mut self, callee: &Box<Expr>, paren: &Token, args: &Vec<Expr>) -> String {
        let args : Vec<_> = args.iter().collect();
        if let Expr::Call(callee, paren, args) = (*callee.clone()) {

        };

        let c = callee.visit(self);
        self.parenthesize(format!("call {}", c), &args)
    }
}

impl StmtVisitor for AstPrinter {
    fn visit_expression(&mut self, expr: &Box<Expr>) {
        expr.visit(self);
    }

    fn visit_print(&mut self, expr: &Box<Expr>) {
        expr.visit(self);
    }

    fn visit_var(&mut self, name: &String, initializer: &Box<Expr>) {
        initializer.visit(self);
    }

    fn visit_block(&mut self, stmts: &Vec<Stmt>) {
        for s in stmts {
            s.visit(self);
        }
    }

    fn visit_if(&mut self, condition: &Box<Expr>, if_stmt: &Box<Stmt>, else_stmt: &Option<Box<Stmt>>) {
        condition.visit(self);
    }

    fn visit_while(&mut self, condition: &Box<Expr>, body: &Box<Stmt>) {
        condition.visit(self);
    }

    fn visit_function(&mut self, name: &String, params: &Vec<String>, body: &Box<Stmt>) {
        body.visit(self);
    }
}

impl AstPrinter {
    fn parenthesize(&mut self, name: String, exprs: &[&Expr]) -> String {
        let mut s = String::new();
        s.push('(');
        s.push_str(&name);

        for expr in exprs {
            s.push(' ' );
            s.push_str(&expr.visit(self));
        }

        s.push(')');

        return s;
    }

    pub fn print(&mut self, stmts: &Vec<Stmt>) {
        for s in stmts {
            //println!("{:?}", &s);
            s.visit(self)
        }
    }
}

pub struct Interpreter { 
    env: Rc<RefCell<Environment>>
}

impl Interpreter {
    pub fn new() -> Self {
        let env = Environment::new();
        //env.define("clock", Object::Callable());
        Interpreter {
            env: Rc::new(RefCell::new(Environment::new()))
        }
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Object, String> {
        return expr.visit(self);
    }

    fn is_truthy(obj: &Object) -> bool {
        match obj {
            Object::Nil => false,
            Object::Boolean(b) => *b,
            _ => true,
        }
    }

    pub fn interpret(&mut self, stmts: Vec<Stmt>) {
        for stmt in stmts {
            self.execute(&stmt); // todo: catching runtime errors?
        }
    }

    pub fn execute(&mut self, stmt: &Stmt) {
        stmt.visit(self)
    }

    fn execute_block(&mut self, stmts: &Vec<Stmt>, environment: Rc<RefCell<Environment>>) {
        let prev_env = self.env.clone();
        self.env = environment;

        for stmt in stmts {
            self.execute(stmt);
        }

        self.env = prev_env;
    }
}


impl ExprVisitor<Result<Object, String>> for Interpreter {
    fn visit_binary(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Object, String> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;

        match (op, &left, &right) {
            // Number
            (Token::MINUS, Object::Number(l), Object::Number(r)) => Ok(Object::Number(l - r)),
            (Token::PLUS, Object::Number(l), Object::Number(r)) => Ok(Object::Number(l + r)),
            (Token::SLASH, Object::Number(l), Object::Number(r)) => Ok(Object::Number(l / r)),
            (Token::STAR, Object::Number(l), Object::Number(r)) => Ok(Object::Number(l * r)),
            (Token::GT, Object::Number(l), Object::Number(r)) => Ok(Object::Boolean(l > r)),
            (Token::LS, Object::Number(l), Object::Number(r)) => Ok(Object::Boolean(l < r)),
            (Token::EQ, Object::Number(l), Object::Number(r)) => Ok(Object::Boolean(l == r)),
            (Token::GT_EQ, Object::Number(l), Object::Number(r)) => Ok(Object::Boolean(l >= r)),
            (Token::LS_EQ, Object::Number(l), Object::Number(r)) => Ok(Object::Boolean(l <= r)),
            (Token::BANG_EQ, Object::Number(l), Object::Number(r)) => Ok(Object::Boolean(l != r)),

            // String
            (Token::PLUS, Object::String(l), Object::String(r)) => Ok(Object::String(format!("{}{}", l, r))),

            // BOOLE
            (Token::BANG_EQ, Object::Boolean(l), Object::Boolean(r)) => Ok(Object::Boolean(l != r)),
            (Token::EQ_EQ, Object::Boolean(l), Object::Boolean(r)) => Ok(Object::Boolean(l == r)),

            // hilfe
            (Token::EQ_EQ, _, Object::Number(_)) => Ok(Object::Boolean(false)),
            (Token::EQ_EQ, Object::Number(_), _) => Ok(Object::Boolean(false)),
            (Token::EQ_EQ, _, Object::String(_)) => Ok(Object::Boolean(false)),
            (Token::EQ_EQ, Object::String(_), _) => Ok(Object::Boolean(false)),
            (c, f, k) => { Err(format!("bad binary operation: {} {} {}", f, c, k)) },
        }
    }

    fn visit_unary(&mut self, op: &Token, right: &Expr) -> Result<Object, String> {
        let right = self.evaluate(right)?;

        match op {
            Token::MINUS => match right {
                Object::Number(d) => Ok(Object::Number(-d)),
                _ => Err(format!("unary operator {} only supports Object::Number", op))
            },
            Token::BANG => Ok(Object::Boolean(!Interpreter::is_truthy(&right))),
            _ => Err(format!("unsupported unary operator {}", op))
        }
    }

    fn visit_grouping(&mut self, expr: &Expr) -> Result<Object, String> {
        return self.evaluate(expr)
    }

    fn visit_literal(&mut self, value: &Object) -> Result<Object, String> {
        Ok(value.clone()) // todo: does this eat memory?
    }

    fn visit_logical(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Object, String> {
        let lobj = self.evaluate(left)?;

        println!("{};evaluated l-expr: {}, op: {}", left, lobj, &op);

        if op == &Token::OR {
            if Self::is_truthy(&lobj) {
                return Ok(lobj)
            }
        } else {
            if !Self::is_truthy(&lobj) {
                return Ok(lobj)
            }
        }

        return Ok(self.evaluate(right)?);
    }

    fn visit_variable(&mut self, name: &String) -> Result<Object, String> {
        self.env.borrow_mut().get(name)
    }

    fn visit_assign(&mut self, name: &String, value: &Expr) -> Result<Object, String> {
        let value = self.evaluate(value)?;
        match self.env.borrow_mut().assign(name.to_string(), value) {
            Some(obj) => Ok(obj),
            _ => Err(format!("did not assign variable {}", name))
        }
    }

    fn visit_call(&mut self, callee: &Box<Expr>, paren: &Token, args: &Vec<Expr>) -> Result<Object, String> {
        let callee = self.evaluate(callee)?;
        let args : Result<Vec<Object>, String> = args.iter().map(|a| self.evaluate(a)).collect();

        if let Object::Callable(funct) = callee {
            funct.invoke(self, args.unwrap());
            return Ok(Object::Nil);
        } else {
            return Err(format!("invalid function call: callee is not callable"));
        }
        

        //return Ok(Object::String(format!("fn '{:?}' with {:?} was called, but function calls are not implemented yet :3", callee, args)));
    }
}

impl StmtVisitor for Interpreter {
    fn visit_expression(&mut self, expr: &Box<Expr>) {
        self.evaluate(&expr);
    }

    fn visit_print(&mut self, expr: &Box<Expr>) {
        let value = self.evaluate(&expr);
        println!("{:?}", value.unwrap())
    }

    fn visit_var(&mut self, name: &String, initializer: &Box<Expr>) { // variable definition
        let init_eval = self.evaluate(&initializer);
        let value = match init_eval {
            Ok(obj) => obj,
            _ => Object::Nil,
        };

        self.env.borrow_mut().define(name.to_string(), value);
    }

    fn visit_block(&mut self, stmts: &Vec<Stmt>) {
        self.execute_block(stmts, Environment::with_enclosing(Rc::clone(&self.env)));
    }

    fn visit_if(&mut self, condition: &Box<Expr>, if_stmt: &Box<Stmt>, else_stmt: &Option<Box<Stmt>>) {
        if Interpreter::is_truthy(&self.evaluate(&condition).unwrap()) { // TODO: error handling
            self.execute(&if_stmt)
        } else if let Some(else_stmt_ref) = else_stmt.as_ref() {
            self.execute(else_stmt_ref)
        }
    }

    fn visit_while(&mut self, condition: &Box<Expr>, body: &Box<Stmt>) {
        while Self::is_truthy(&self.evaluate(&condition).unwrap()) { // TODO: error handling
            self.execute(body);
        }
    }

    fn visit_function(&mut self, name: &String, params: &Vec<String>, body: &Box<Stmt>) {
        let func = LoxFunction::new(Some(name.clone()), &vec![*body.clone()]);
        self.env.borrow_mut().define(name.clone(), Object::Callable(func));
    }
}
