use crate::interpreter::*;
use crate::parser::*;
use crate::scanner::*;


use std::collections::HashMap;

#[derive(Debug)]
pub struct Resolver<'a> {
    interpreter: &'a mut Interpreter,
    scopes: Vec<HashMap<Expr, bool>>, // Vec is used as a Stack
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

    fn declare(&mut self, expr: Expr) -> Result<(), String> {
        println!("DECLARE CALLED");
        if let Some(scope) = self.scopes.last_mut() {
            println!("declaring {} in scope {:?}", expr, scope);
            if scope.contains_key(&expr) {
                eprintln!("Variable with name {} already present in current scope.", expr);
                return Err(format!("Variable with name {} already present in current scope.", expr));
            }
            scope.insert(expr, false);
            return Ok(());
        }

        return Err("Declared but no scope".to_string());
    }

    fn define(&mut self, expr: Expr) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(expr, true);
        }
    }

    fn resolve_local(&mut self, expr: Expr) {
        //println!("resolve_local for {}: scopes: {:#?}", name, self.scopes);
        //println!("self.scopes.len: {:?}", self.scopes.len());
        for i in  (0 .. self.scopes.len()).rev() {
            //println!("CHECKSCOPE DIST {}", i);
            //println!("I== {}", i);
            if self.scopes.get(i).unwrap().get(&expr).is_some() {
              //  print!("  => it does!  {:?}", self.scopes.get(i).unwrap().get(name));
                self.interpreter.resolve(expr, self.scopes.len() - 1 -  i);
                return;
            } else {
                //println!("SCOPES {} DOES NOT CONTAIN KEY {}", i, name);
            }

            //println!();
        }
    }

    fn resolve_fn(&mut self, name: &String, params: &Vec<String>, body: &Stmt) {
        self.begin_scope();
        for param in params { 
            self.declare(param.to_string());
            self.define(param.to_string());
        }

        self.resolve_stmt(&body);
        self.end_scope();
    }
}

impl<'a> ExprVisitor<Result<Object, String>> for Resolver<'a> {
    fn visit_variable(&mut self, expr: Expr, name: &String) -> Result<Object, String> {
        if let Some(scope) = self.scopes.last() {
            if scope.get(&expr) == Some(&false) {
                return Err(format!("Can't read local variable {} in its own initializer.", name));
            }
        }

        println!("visiting variable {}", name);

        self.resolve_local(expr);
        return Ok(Object::Nil)
    }

    fn visit_assign(&mut self, expr: Expr, name: &String, value: &Expr) -> Result<Object, String> {
        self.resolve_expr(value);
        self.resolve_local(expr);
        return Ok(Object::Nil);        
    }

    fn visit_binary(&mut self, expr: Expr, left: &Expr, op: &Token, right: &Expr) -> Result<Object, String> {
        self.resolve_expr(left);
        self.resolve_expr(right);
        return Ok(Object::Nil);
    }

    fn visit_call(&mut self, expr: Expr, callee: &Box<Expr>, paren: &Token, args: &Vec<Expr>) -> Result<Object, String> {
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

    fn visit_logical(&mut self, expr: Expr, left: &Expr, op: &Token, right: &Expr) -> Result<Object, String> {
        self.resolve_expr(left);
        self.resolve_expr(right);
        return Ok(Object::Nil);
    }

    fn visit_unary(&mut self, expr: Expr, op: &Token, right: &Expr) -> Result<Object, String> {
        self.resolve_expr(right);
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

    fn visit_var(&mut self, name: &String, initializer: &Option<Box<Expr>>) -> Option<Object> {
        self.declare(name.to_string());

        if let Some(initializer) = initializer {
            self.resolve_expr(&*initializer.clone());
        }
        
        self.define(name.to_string());

        return None;
    }

    fn visit_function(
            &mut self,
            name: &String,
            params: &Vec<String>,
            body: &Box<Stmt>,
        ) -> Option<Object> {
        //self.declare();
        //self.define(name.to_string());
        self.resolve_fn(name, params, body);

        return None;
    }

    fn visit_expression(&mut self, expr: &Box<Expr>) -> Option<Object> {
        self.resolve_expr(&*expr);
        return None;
    }

    fn visit_print(&mut self, expr: &Box<Expr>) -> Option<Object> {
        //println!("RESOLVER: print start");
        self.resolve_expr(&*expr);
        //println!("Resolver: print end");
        return None;
    }

    fn visit_return(&mut self, keyword: &String, value: &Box<Expr>) -> Option<Object> {
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
}