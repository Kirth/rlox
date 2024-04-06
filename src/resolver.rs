use crate::interpreter::*;
use crate::parser::*;
use crate::scanner::*;


use std::borrow::BorrowMut;
use std::collections::HashMap;

pub enum FunctionType {
    METHOD,
    FUNCTION
}

#[derive(Debug)]
pub struct Resolver<'a> {
    interpreter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, bool>>, // Vec is used as a Stack
}

impl<'a> Resolver<'a> {
    pub fn new(i: &'a mut Interpreter) -> Self {
        Resolver {
            interpreter: i,
            scopes: vec![]
        }
    }

    pub fn dump(&self) {
        println!("{:?}", self.scopes);
    }

    pub fn resolve_stmts(&mut self, stmts: &Vec<Stmt>) {
        for stmt in stmts {
            self.resolve_stmt(stmt);
        }
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) {
        stmt.visit(self);
    }
    
    fn resolve_expr(&mut self, expr: &Expr) {
        //println!("expr: {:?}", expr);
        expr.visit(self);
    }

    fn begin_scope(&mut self) {
        //println!("============== BEGINNING NEW SCOPE ===========");
        //println!("starting new scope {} -> {}", self.scopes.len(), self.scopes.len() + 1);
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        //println!("popping scope scope {} -> {}", self.scopes.len(), self.scopes.len() - 1);
        //println!("!!!!!!!!!!!!!!!!! ENDING SCOPE !!!!!!!!!!!!!!!");
        self.scopes.pop();
    }

    fn declare(&mut self, id: Token) -> Result<(), String> {
        let id = if let Token::Identifier(id) = id { id }
        else { panic!("resolver::declare on non-Identifier token: {:?}", id) };

        if let Some(scope) = self.scopes.last_mut() {
            //println!("declaring {} in scope {:?}", id, scope);
            if scope.contains_key(&id) {
                eprintln!("Variable with name {} already present in current scope.", id);
                return Err(format!("Variable with name {} already present in current scope.", id));
            }
            scope.insert(id, false);
            return Ok(());
        }

        return Err("Declared but no scope".to_string());
    }

    fn define(&mut self, id: Token) {
        let id = if let Token::Identifier(id) = id { id }
        else { panic!("resolver::define on non-Identifier token: {:?}", id) };

        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(id, true);
        }
    }

    fn resolve_local(&mut self, id: &Token, expr: Expr) {
        let id = if let Token::Identifier(id) = id.clone() { id }
        else if let Token::THIS = id.clone() { "this".to_string() }
        else { panic!("resolver::resolve_local on non-Identifier token: {:?}", id) };
        
        for i in  (0 .. self.scopes.len()).rev() {
            if self.scopes.get(i).unwrap().get(&id).is_some() {
                self.interpreter.resolve(expr, self.scopes.len() - 1 -  i);
                return;
            } else {
                //println!("Scope({}) missing key '{}'", i, id);
            }
        }
    }

    fn resolve_fn(&mut self, name: &TokenLoc, params: &Vec<TokenLoc>, body: &Stmt, fn_type: FunctionType) {
        self.begin_scope();
        for param in params { 
            self.declare(param.token.clone());
            self.define(param.token.clone());
        }

        self.resolve_stmt(&body);
        self.end_scope();
    }
}

impl<'a> ExprVisitor<Result<Object, String>> for Resolver<'a> {
    fn visit_variable(&mut self, expr: Expr, name: &TokenLoc) -> Result<Object, String> {
        let id = if let Token::Identifier(id) = name.token.clone() { id }
        else { panic!("resolver::visit_variable on non-Identifier token: {:?}", name) }; // this is the 3rd time I repeat this in the code now, that's silly

        if let Some(scope) = self.scopes.last() {
            if scope.get(&id) == Some(&false) {
                return Err(format!("Can't read local variable {:?} in its own initializer.", name));
            }
        }

        self.resolve_local(&name.token, expr);
        return Ok(Object::Nil)
    }

    fn visit_assign(&mut self, expr: Expr, name: &TokenLoc, value: &Expr) -> Result<Object, String> {
        self.resolve_expr(value);
        self.resolve_local(&name.token, expr);
        return Ok(Object::Nil);        
    }

    fn visit_binary(&mut self, expr: Expr, left: &Expr, op: &TokenLoc, right: &Expr) -> Result<Object, String> {
        self.resolve_expr(left);
        self.resolve_expr(right);
        return Ok(Object::Nil);
    }

    fn visit_call(&mut self, expr: Expr, callee: &Box<Expr>, paren: &TokenLoc, args: &Vec<Expr>) -> Result<Object, String> {
        self.resolve_expr(&**callee);

        for arg in args {
            self.resolve_expr(arg);
        }

        return Ok(Object::Nil);
    }

    fn visit_grouping(&mut self, expr: &Expr) -> Result<Object, String> {
        self.resolve_expr(expr);
        return Ok(Object::Nil);
    }

    fn visit_literal(&mut self, expr: Expr, value: &Object) -> Result<Object, String> {
        return Ok(Object::Nil);
    }    

    fn visit_logical(&mut self, expr: Expr, left: &Expr, op: &TokenLoc, right: &Expr) -> Result<Object, String> {
        self.resolve_expr(left);
        self.resolve_expr(right);
        return Ok(Object::Nil);
    }

    fn visit_unary(&mut self, expr: Expr, op: &TokenLoc, right: &Expr) -> Result<Object, String> {
        self.resolve_expr(right);
        return Ok(Object::Nil);
    }

    fn visit_get(&mut self, expr: Expr, object: &Box<Expr>, name: &TokenLoc) -> Result<Object, String> {
        self.resolve_expr(&*object);
        return Ok(Object::Nil);
    }

    fn visit_set(&mut self, expr: Expr, object: &Box<Expr>, name: &TokenLoc, value: &Box<Expr>) -> Result<Object, String> {
        self.resolve_expr(&*value);
        self.resolve_expr(&*object);
        return Ok(Object::Nil);
    }

    fn visit_this(&mut self, expr: Expr, token: &TokenLoc) -> Result<Object, String> {
        self.resolve_local(&token.token, expr);
        return Ok(Object::Nil);
    }

}

impl<'a> StmtVisitor for Resolver<'a> {
    fn visit_block(&mut self, stmts: &Vec<Stmt>) -> Option<Object> {
        self.begin_scope();
        self.resolve_stmts(stmts);
        self.end_scope();

        return None;
    }

    fn visit_var(&mut self, name: &TokenLoc, initializer: &Option<Box<Expr>>) -> Option<Object> {
        self.declare(name.token.clone());
        if let Some(initializer) = initializer {
            self.resolve_expr(&*initializer.clone());
        }
        
        self.define(name.token.clone());

        return None;
    }

    fn visit_function(
            &mut self,
            name: &TokenLoc,
            params: &Vec<TokenLoc>,
            body: &Box<Stmt>,
        ) -> Option<Object> {
        self.declare(name.token.clone());
        self.define(name.token.clone());
        self.resolve_fn(name, params, body, FunctionType::FUNCTION);

        return None;
    }

    fn visit_expression(&mut self, expr: &Box<Expr>) -> Option<Object> {
        self.resolve_expr(&*expr);
        return None;
    }

    fn visit_print(&mut self, expr: &Box<Expr>) -> Option<Object> {
        self.resolve_expr(&*expr);
        return None;
    }

    fn visit_return(&mut self, keyword: &TokenLoc, value: &Box<Expr>) -> Option<Object> {
        self.resolve_expr(&*value);
        return None;
    }

    fn visit_while(&mut self, condition: &Box<Expr>, body: &Box<Stmt>) -> Option<Object> {
        self.resolve_expr(&**condition);
        self.resolve_stmt(&**body);

        return None;
    }

    fn visit_if(
            &mut self,
            condition: &Box<Expr>,
            if_stmt: &Box<Stmt>,
            else_stmt: &Option<Box<Stmt>>,
        ) -> Option<Object> {
        self.resolve_expr(&**condition);
        self.resolve_stmt(&**if_stmt);

        if let Some(else_stmt) = else_stmt {
            self.resolve_stmt(&**else_stmt);
        }

        return None;
    }

    fn visit_class(&mut self, name: &TokenLoc, methods: &Vec<Stmt>) -> Option<Object> {
        self.declare(name.token.clone());
        self.define(name.token.clone());

        self.begin_scope();
        self.scopes.last_mut().unwrap().insert("this".to_string(), true); 

        for stmt in methods {
            if let Stmt::Function(name, params, body) = stmt {
                self.resolve_fn(name, params, body, FunctionType::METHOD)
            } else {
                eprint!("resolver::visit_class: method parameter to fn {} not of Stmt::Function-variant; {:?}", name.token, stmt);
            }            
        }

        self.end_scope();

        return None;
    }
}