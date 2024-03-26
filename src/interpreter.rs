#![allow(non_camel_case_types)]

// In pass 1: I will not add any extra bells and whistles to Lox

use std::cell::RefCell;
use std::collections::HashMap;

use std::rc::Rc;

use rand::Rng;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::cmp::Ordering;

use crate::parser::*;
use crate::scanner::*;

// this is not a Box<dyn Callable> because those can't be easily cloned
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Callable {
    Lox(LoxFunction),
    Native(NativeFunction),
}

impl Callable {
    fn invoke(&self, interpreter: &mut Interpreter, args: Vec<Object>) -> Option<Object> {
        match self {
            Callable::Lox(lf) => lf.invoke(interpreter, args),
            Callable::Native(nf) => nf.invoke(interpreter, args),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoxFunction {
    // this should contain the declaration
    name: Option<String>,
    declaration: Box<Stmt>,
    params: Vec<TokenLoc>,
    closure: Environment,
    // does this thing need its own environment/closure?
    is_method: bool,
}

impl PartialEq for LoxFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.params == other.params && &self.declaration == &other.declaration
    }
}

impl Eq for LoxFunction {}

impl Hash for LoxFunction {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.params.hash(state);
        // Simplified hashing for declaration
        //stmt_hash(&self.declaration, state);
    }
}


impl LoxFunction {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn invoke(&self, interpreter: &mut Interpreter, args: Vec<Object>) -> Option<Object> {
        let mut env = self.closure.clone(); // we're not enclosing here but using the clone directly, is that ok? seems to work!

        /*println!(
            "invoking function {:?} IN ENV {} with params {:?}; env:",
            self.name, env.uid, args
        );*/
    

        for (k, v) in self.params.iter().zip(args.iter()) {
            if let Token::Identifier(name) = &k.token {
                let _ = env.define(name.clone(), v.clone());
            } else {
                panic!("LoxFunction::invoke assigning non-identifier {:?} as param name", k);
            }
            //println!("binding {} == {}", k, v);
            
        }
        //println!("executing function {} with declaration {:?}", self.name.clone().unwrap(), self.declaration);
        interpreter.execute_block(&vec![*self.declaration.clone()], Rc::new(RefCell::new(env)))
        // TODO: correct environment
    }

    pub fn new(
        name: Option<String>,
        params: &Vec<TokenLoc>,
        body: &Box<Stmt>,
        closure: Environment,
    ) -> Self {
        LoxFunction {
            name: name,
            params: params.clone(),
            declaration: body.clone(),
            is_method: false,
            closure: closure,
        }
    }
}

#[derive(Debug, Clone)]
pub struct NativeFunction {
    name: Option<String>,
    funct: fn(&Interpreter, Vec<Object>) -> Option<Object>, // todo: better error handling?
    params: Vec<TokenLoc>,
}

impl NativeFunction {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn invoke(&self, interpreter: &Interpreter, args: Vec<Object>) -> Option<Object> {
        (self.funct)(interpreter, args)
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.params == other.params
    }
}

impl Eq for NativeFunction {}

impl Hash for NativeFunction {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.params.hash(state);
        // We cannot directly hash `funct`, so we rely on name and params for identity.
    }
}

#[derive(Debug, Clone)]
pub enum Object {
    String(String),
    Number(f64),
    Boolean(bool),
    Callable(Callable),
    Nil,
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::String(a), Object::String(b)) => a == b,
            (Object::Number(a), Object::Number(b)) => a == b,
            (Object::Boolean(a), Object::Boolean(b)) => a == b,
            (Object::Callable(a), Object::Callable(b)) => a == b,
            (Object::Nil, Object::Nil) => true,
            _ => false,
        }
    }
}

impl Eq for Object {}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Object::String(s) => {
                s.hash(state);
            },
            Object::Number(n) => {
                // Use the wrapper strategy indirectly for f64
                n.to_bits().hash(state);
            },
            Object::Boolean(b) => {
                b.hash(state);
            },
            Object::Callable(c) => {
                // Assuming Callable implements Hash
                c.hash(state);
            },
            Object::Nil => {
                // Just use a constant value to represent Nil
                0.hash(state);
            }
        }
    }
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::String(s) => write!(f, "'{}'", s),
            Object::Number(n) => write!(f, "{}", n),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Callable(c) => {
                write!(
                    f,
                    "{}",
                    format!(
                        "callable({})",
                        match c {
                            Callable::Lox(c) => c.name.clone().unwrap_or("<no name>".to_string()),
                            Callable::Native(c) =>
                                c.name.clone().unwrap_or("<unnamed native fn>".to_string()),
                        }
                    )
                )
            }
            Object::Nil => write!(f, "nil"),
        }
    }
}

impl std::fmt::Display for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.name {
            Some(name) => write!(
                f,
                "Function: {}, is_method: {}, declaration: {:?}",
                name, self.is_method, self.declaration
            ),
            None => write!(
                f,
                "Anonymous Function, is_method: {}, declaration: {:?}",
                self.is_method, self.declaration
            ),
        }
    }
}

pub trait ExprVisitor<T> {
    fn visit_binary(&mut self, expr: Expr, left: &Expr, op: &TokenLoc, right: &Expr) -> T;
    fn visit_unary(&mut self, expr: Expr, op: &TokenLoc, right: &Expr) -> T;
    fn visit_literal(&mut self, expr: Expr, value: &Object) -> T;
    fn visit_logical(&mut self, expr: Expr, left: &Expr, op: &TokenLoc, right: &Expr) -> T;
    fn visit_grouping(&mut self, expr: &Expr) -> T;
    fn visit_variable(&mut self, expr: Expr, name: &TokenLoc) -> T; // TODO: I don't want to copy variables on every use.
                                                      // this may mean wrapping them in RCs?  Do we even need ExprVisitor to be generic?,
    fn visit_assign(&mut self, expr: Expr, name: &TokenLoc, value: &Expr) -> T;
    fn visit_call(&mut self, expr: Expr, callee: &Box<Expr>, paren: &TokenLoc, args: &Vec<Expr>) -> T;
}

// QUEST: in hindsight, it probably makes more sense to clone these and pass them down
// What would the impact of using an Rc be, memory wise and runtime wise?
pub trait StmtVisitor {
    // statements produce no values
    fn visit_expression(&mut self, expr: &Box<Expr>) -> Option<Object>; // TODO: do I pass in boxes here?
    fn visit_print(&mut self, expr: &Box<Expr>) -> Option<Object>; //       does that make sense? what does it mean?
    fn visit_if(
        &mut self,
        condition: &Box<Expr>,
        if_stmt: &Box<Stmt>,
        else_stmt: &Option<Box<Stmt>>,
    ) -> Option<Object>;
    fn visit_while(&mut self, condition: &Box<Expr>, body: &Box<Stmt>) -> Option<Object>;
    fn visit_var(&mut self, name: &TokenLoc, initializer: &Option<Box<Expr>>) -> Option<Object>;
    fn visit_block(&mut self, stmts: &Vec<Stmt>) -> Option<Object>; // hack: passing objects to support return values.
    fn visit_function(
        &mut self,
        name: &TokenLoc,
        params: &Vec<TokenLoc>,
        body: &Box<Stmt>,
    ) -> Option<Object>;
    fn visit_return(&mut self, keyword: &TokenLoc, value: &Box<Expr>) -> Option<Object>;
}

#[derive(Debug, Clone)]
pub struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    values: std::collections::HashMap<String, Object>,
    uid: u64, // for debugging purposes
}

struct UidGenerator {
    current_id: u64,
}

impl UidGenerator {
    fn new() -> Self {
        UidGenerator { current_id: 0 }
    }
    fn get_next_id(&mut self) -> u64 {
        self.current_id += 1;
        self.current_id
    }
}

static mut UID_GENERATOR: Option<UidGenerator> = None;

fn get_next_uid() -> u64 {
    unsafe {
        if UID_GENERATOR.is_none() {
            UID_GENERATOR = Some(UidGenerator::new());
        }

        UID_GENERATOR.as_mut().unwrap().get_next_id()
    }
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: std::collections::HashMap::new(),
            enclosing: None,
            uid: get_next_uid(),
        }
    }

    fn print(&self, indent_level: usize) {
        let indent = " ".repeat(indent_level * 2); // 2 spaces per indentation level
        println!("{}Environment:", indent);
        for (k, v) in self.values.iter() {
            //println!("{}  {}: {}", indent, k, &format!("{:?}", v)[0..20]); // Indent key-value pairs
            println!("{} {}: {}", indent, k, v);
        }
        if let Some(enclosing) = &self.enclosing {
            println!("{}Enclosing environment:", indent);
            enclosing.borrow().print(indent_level + 1); // Increase indent for the enclosing environment
        } else {
            println!("{}No enclosing environment.", indent);
        }
    }

    pub fn with_enclosing(enclosing: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        let uid = get_next_uid();
        //println!("ENCLOSING OVER NEW ENVIRONMENT, NOW AT UID {}", uid);
        Rc::new(RefCell::new(Environment {
            values: std::collections::HashMap::new(),
            uid: uid,
            enclosing: Some(enclosing), // whoops it's easy to accidentally double-clone an Rc
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

    pub fn get(&self, name: &String) -> Result<Object, String> {
        if let Some(obj) = self.values.get(name) {
            //println!("got object {} from env {}", name, self.uid);
            return Ok(obj.clone()); // TODO: don't clone
        } else {
            if self.enclosing.is_some() {
                //return self.enclosing.unwrap().borrow().get(name);
                /*println!(
                    "got object {} from enclosing env {} through {}",
                    name,
                    self.enclosing.as_ref().unwrap().borrow().uid,
                    self.uid
                );*/
                return self.enclosing.as_ref().unwrap().borrow().get(name);
            } else {
                //self.print(5);
                return Err(format!("Undefined variable '{}'.", name));
            }
        }
    }

    pub fn get_at(&self, distance: usize, name: &String) -> Option<Object> {
        return self.ancestor(distance).borrow().get(name).ok();
    }

    fn ancestor(&self, distance: usize) -> Rc<RefCell<Environment>> {
        let mut env = Rc::new(RefCell::new(self.clone()));

        for i in 0 .. distance {
            let next_env = {
                let benv = env.borrow();
                benv.enclosing.clone().unwrap() // we're cloning the Rc, not the env
            };
            env = next_env;
            //println!("next-now-current env: {:?}", env);
        }

        return env;
    }

    fn assign_at(&mut self, distance: usize, name: String, value: Object) {

        let ans = self.ancestor(distance);
        let mut bans = ans.borrow_mut();
        //println!("assigning {} at depth {} into {:?}", name, distance, bans);
        bans.define(name, value);
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
    fn visit_binary(&mut self, expr: Expr, left: &Expr, op: &TokenLoc, right: &Expr) -> String {
        self.parenthesize(format!("{:?}", op), &[left, right])
    }

    fn visit_unary(&mut self, expr: Expr, op: &TokenLoc, right: &Expr) -> String {
        self.parenthesize(format!("{:?}", op), &[right])
    }

    fn visit_grouping(&mut self, expr: &Expr) -> String {
        self.parenthesize("group".to_string(), &[expr])
    }

    fn visit_literal(&mut self, expr: Expr, value: &Object) -> String {
        value.to_string()
    }

    fn visit_logical(&mut self, expr: Expr, left: &Expr, op: &TokenLoc, right: &Expr) -> String {
        self.parenthesize(format!("{:?}", op), &[left, right])
    }

    fn visit_variable(&mut self, expr: Expr, name: &TokenLoc) -> String {
        format!("{:?}", name)
    }

    fn visit_assign(&mut self, expr: Expr, name: &TokenLoc, value: &Expr) -> String {
        self.parenthesize(format!("assign {:?}", name), &[value])
    }

    fn visit_call(&mut self, expr: Expr, callee: &Box<Expr>, paren: &TokenLoc, args: &Vec<Expr>) -> String {
        let args: Vec<_> = args.iter().collect();
        if let Expr::Call(callee, paren, args) = (*callee.clone()) {};

        let c = callee.visit(self);
        self.parenthesize(format!("call {}", c), &args)
    }
}

impl StmtVisitor for AstPrinter {
    fn visit_expression(&mut self, expr: &Box<Expr>) -> Option<Object> {
        expr.visit(self);
        None
    }

    fn visit_print(&mut self, expr: &Box<Expr>) -> Option<Object> {
        expr.visit(self);
        None
    }

    fn visit_var(&mut self, name: &TokenLoc, initializer: &Option<Box<Expr>>) -> Option<Object> {
        initializer.clone().unwrap_or(Box::new(Expr::Literal(Object::String("!this var has no initializer!".to_string())))).visit(self);
        None
    }

    fn visit_block(&mut self, stmts: &Vec<Stmt>) -> Option<Object> {
        for s in stmts {
            s.visit(self);
        }
        None
    }

    fn visit_if(
        &mut self,
        condition: &Box<Expr>,
        if_stmt: &Box<Stmt>,
        else_stmt: &Option<Box<Stmt>>,
    ) -> Option<Object> {
        condition.visit(self);
        None
    }

    fn visit_while(&mut self, condition: &Box<Expr>, body: &Box<Stmt>) -> Option<Object> {
        condition.visit(self);
        None
    }

    fn visit_function(
        &mut self,
        name: &TokenLoc,
        params: &Vec<TokenLoc>,
        body: &Box<Stmt>,
    ) -> Option<Object> {
        body.visit(self);
        None
    }

    fn visit_return(&mut self, keyword: &TokenLoc, value: &Box<Expr>) -> Option<Object> {
        value.visit(self);
        None
    }
}

impl AstPrinter {
    fn parenthesize(&mut self, name: String, exprs: &[&Expr]) -> String {
        let mut s = String::new();
        s.push('(');
        s.push_str(&name);

        for expr in exprs {
            s.push(' ');
            s.push_str(&expr.visit(self));
        }

        s.push(')');

        return s;
    }

    pub fn print(&mut self, stmts: &Vec<Stmt>) {
        println!("===============--AST--=============");
        for s in stmts {
            println!("{:#?}", &s);
            //s.visit(self);
        }
        println!("===============!!AST!!=============");
    }
}

#[derive(Debug)]
pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
    globals: Rc<RefCell<Environment>>,
    locals: HashMap<Expr, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut env = Environment::new();
        /* sometimes I comment these out because it makes debugging stuff with Environments less cluttered
        env.define(
            "clock".to_string(),
            Object::Callable(Callable::Native(NativeFunction {
                name: Some("<native fn: clock>".to_string()),
                params: vec![],
                funct: |_, _| {
                    Some(Object::Number(
                        std::time::SystemTime::now()
                            .duration_since(std::time::UNIX_EPOCH)
                            .unwrap()
                            .as_micros() as f64,
                    ))
                },
            })),
        );

        env.define(
            "rand".to_string(),
            Object::Callable(Callable::Native(NativeFunction {
                name: Some("<native fn: rand>".to_string()),
                params: vec![],
                funct: |_, _| {
                    let mut rng = rand::thread_rng();
                    Some(Object::Number(rng.gen()))
                },
            })),
        );

        env.define(
            "str_from_file".to_string(),
            Object::Callable(Callable::Native(NativeFunction {
                name: Some("<native fn: str_from_file>".to_string()),
                funct: |_, params| {
                    let path = if let Object::String(s) = params.get(0).unwrap() {
                        s
                    } else {
                        "no valid string fed into str_from_file!"
                    };
                    Some(Object::String(std::fs::read_to_string(path).unwrap()))
                },
                params: vec!["path".to_string()],
            })),
        );

        env.define(
            "debug__dump_globals".to_string(),
            Object::Callable(Callable::Native(NativeFunction {
                name: Some("<native fn: debug__dump_globals>".to_string()),
                params: vec![],
                funct: |i, _| {
                    println!("===== GLOBALS ===== \n {:#?} ======= END =======", i.globals);
                    return Some(Object::Nil);
                }
            }))
        );*/

        let env_ptr = Rc::new(RefCell::new(env));
        Interpreter {
            env: env_ptr.clone(),
            globals: env_ptr,
            locals: HashMap::new(),
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
        println!("pre-interpret() overview of locals: {:#?}", self.locals);

        for stmt in stmts {
            self.execute(&stmt); // todo: catching runtime errors?
        }
    }

    pub fn execute(&mut self, stmt: &Stmt) -> Option<Object> {
        stmt.visit(self)
    }

    pub fn resolve(&mut self, expr: Expr, depth: usize) {
        println!("resolving {} \t\t=> {}", expr, depth);
        self.locals.insert(expr, depth);
        println!("RESOLVE/LOCALS: {:#?}", self.locals);
    }

    fn execute_block(
        &mut self,
        stmts: &Vec<Stmt>,
        environment: Rc<RefCell<Environment>>,
    ) -> Option<Object> {
        let prev_env = self.env.clone();
        self.env = environment;

        //self.env.borrow().print(0);

        let mut res = None;

        for stmt in stmts {
            res = self.execute(stmt);

            if res.is_some() {
                //return res; // If we return here, we don't reset the to the previous_env;
                break;
            }
        }

        self.env = prev_env;
        return res;
    }

    fn lookup_var(&self, expr: &Expr, name: &TokenLoc) -> Option<Object> {
        let dist = self.locals.get(expr);
        let name = match &name.token {
            Token::Identifier(name) => name.clone(),
            e => format!("{:?}", e)
        };
        print!("looking up {} through expr {:?} at dist {:?}: ", name, expr, dist);

        //println!("locals: {:#?}", self.locals);
        //println!("self_env: {:#?}", self.env);

        if let Some(dist) = dist {
            //println!("==> value: {:?}", self.env.borrow().get_at(*dist, name));
            //println!("{:#?}", self.env);
            //println!("getting {} at dist {}: {:?}", name, dist, self.env.borrow().get_at(*dist, name));
            println!("{:?}", self.env.borrow().get_at(*dist, &name).is_some());
            return self.env.borrow().get_at(*dist, &name);
        } else {
            println!("{:?}", self.globals.borrow().get(&name).ok().is_some());
            return self.globals.borrow().get(&name).ok();
        }
    }
}

impl ExprVisitor<Result<Object, String>> for Interpreter {
    fn visit_binary(&mut self, expr: Expr, left: &Expr, op: &TokenLoc, right: &Expr) -> Result<Object, String> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;

        match (&op.token, &left, &right) {
            // Number
            (Token::MINUS, Object::Number(l), Object::Number(r)) => Ok(Object::Number(l - r)),
            (Token::PLUS, Object::Number(l), Object::Number(r)) => Ok(Object::Number(l + r)),
            (Token::SLASH, Object::Number(l), Object::Number(r)) => Ok(Object::Number(l / r)),
            (Token::STAR, Object::Number(l), Object::Number(r)) => Ok(Object::Number(l * r)),
            (Token::GT, Object::Number(l), Object::Number(r)) => Ok(Object::Boolean(l > r)),
            (Token::LS, Object::Number(l), Object::Number(r)) => Ok(Object::Boolean(l < r)),
            (Token::EQ_EQ, Object::Number(l), Object::Number(r)) => Ok(Object::Boolean(l == r)),
            (Token::GT_EQ, Object::Number(l), Object::Number(r)) => Ok(Object::Boolean(l >= r)),
            (Token::LS_EQ, Object::Number(l), Object::Number(r)) => Ok(Object::Boolean(l <= r)),
            (Token::BANG_EQ, Object::Number(l), Object::Number(r)) => Ok(Object::Boolean(l != r)),

            // String
            (Token::PLUS, Object::String(l), Object::String(r)) => {
                Ok(Object::String(format!("{}{}", l, r)))
            }

            // BOOLE
            (Token::BANG_EQ, Object::Boolean(l), Object::Boolean(r)) => Ok(Object::Boolean(l != r)),
            (Token::EQ_EQ, Object::Boolean(l), Object::Boolean(r)) => Ok(Object::Boolean(l == r)),

            // hilfe
            (Token::EQ_EQ, _, Object::Number(_)) => Ok(Object::Boolean(false)),
            (Token::EQ_EQ, Object::Number(_), _) => Ok(Object::Boolean(false)),
            (Token::EQ_EQ, _, Object::String(_)) => Ok(Object::Boolean(false)),
            (Token::EQ_EQ, Object::String(_), _) => Ok(Object::Boolean(false)),

            (Token::PLUS, Object::String(l), Object::Number(r)) => {
                Ok(Object::String(format!("{}{}", l, r)))
            }
            (Token::PLUS, Object::Number(l), Object::String(r)) => {
                Ok(Object::String(format!("{}{}", l, r)))
            }
            (c, f, k) => Err(format!("bad binary operation: {} {} {}", f, c, k)),
        }
    }

    fn visit_unary(&mut self, expr: Expr, op: &TokenLoc, right: &Expr) -> Result<Object, String> {
        let right = self.evaluate(right)?;

        match &op.token {
            Token::MINUS => match right {
                Object::Number(d) => Ok(Object::Number(-d)),
                _ => Err(format!(
                    "unary operator {} only supports Object::Number",
                    op.token
                )),
            },
            Token::BANG => Ok(Object::Boolean(!Interpreter::is_truthy(&right))),
            e => Err(format!("unsupported unary operator {}", e)),
        }
    }

    fn visit_grouping(&mut self, expr: &Expr) -> Result<Object, String> {
        return self.evaluate(expr);
    }

    fn visit_literal(&mut self, expr: Expr, value: &Object) -> Result<Object, String> {
        Ok(value.clone()) // todo: does this eat memory?
    }

    fn visit_logical(&mut self, expr: Expr, left: &Expr, op: &TokenLoc, right: &Expr) -> Result<Object, String> {
        let lobj = self.evaluate(left)?;

        println!("{};evaluated l-expr: {}, op: {}", left, lobj, &op.token);

        if op.token == Token::OR {
            if Self::is_truthy(&lobj) {
                return Ok(lobj);
            }
        } else {
            if !Self::is_truthy(&lobj) {
                return Ok(lobj);
            }
        }

        return Ok(self.evaluate(right)?);
    }

    fn visit_variable(&mut self, expr: Expr, name: &TokenLoc) -> Result<Object, String> {
        //self.env.borrow().get(name)
        self.lookup_var(&expr, name).ok_or(format!("err while looking up var {:?} in visit_variable in env: {:#?}", name, "cuck")) //self.env))
    }

    fn visit_assign(&mut self, expr: Expr, name: &TokenLoc, value: &Expr) -> Result<Object, String> {
        let value = self.evaluate(value)?;
        let name = match &name.token {
            Token::Identifier(name) => name.clone(),
            e => format!("{:?}", e)
        };
        

        if let Some(dist) = self.locals.get(&expr) {
            self.env.borrow_mut().assign_at(*dist, name.clone(), value.clone());
            return Ok(value); // QUEST: do we need this?  would this mean var a = (b = c); a == c?
        } else {
            self.globals.borrow_mut().assign(name.clone(), value.clone());
            return Ok(value);
        }

        /*match self.env.borrow_mut().assign(name.to_string(), value) {
            Some(obj) => Ok(obj),
            _ => Err(format!("did not assign variable {}", name)),
        }*/
    }

    fn visit_call(
        &mut self,
        expr: Expr,
        callee: &Box<Expr>,
        paren: &TokenLoc,
        args: &Vec<Expr>,
    ) -> Result<Object, String> {
        let callee = self.evaluate(callee)?;
        let args: Result<Vec<Object>, String> = args.iter().map(|a| self.evaluate(a)).collect();

        if let Object::Callable(funct) = callee {
            return funct
                .invoke(self, args.unwrap())
                .or(Some(Object::Nil))
                .ok_or(format!("function call broke"));
        } else {
            return Err(format!("invalid function call: callee is not callable"));
        }
        //return Ok(Object::String(format!("fn '{:?}' with {:?} was called, but function calls are not implemented yet :3", callee, args)));
    }
}

impl StmtVisitor for Interpreter {
    fn visit_expression(&mut self, expr: &Box<Expr>) -> Option<Object> {
        self.evaluate(&expr); // we only want explicit returns
        None
    }

    fn visit_print(&mut self, expr: &Box<Expr>) -> Option<Object> {
        let value = self.evaluate(&expr);
        println!(
            "{}",
            match value {
                Ok(v) => v,
                Err(e) => Object::String(e),
            }
        );

        return None;
    }

    fn visit_var(&mut self, name: &TokenLoc, initializer: &Option<Box<Expr>>) -> Option<Object> {
        // variable definition
        let value = if let Some(initializer) = initializer {
                match self.evaluate(&initializer) {
                Ok(obj) => obj,
                _ => Object::Nil,
            }
        } else { Object::Nil };

        let name = match &name.token {
            Token::Identifier(name) => name.clone(),
            e => format!("{:?}", e)
        };

        self.env.borrow_mut().define(name, value);
        return None;
    }

    fn visit_block(&mut self, stmts: &Vec<Stmt>) -> Option<Object> {
        self.execute_block(stmts, Environment::with_enclosing(Rc::clone(&self.env)))
    }

    fn visit_if(
        &mut self,
        condition: &Box<Expr>,
        if_stmt: &Box<Stmt>,
        else_stmt: &Option<Box<Stmt>>,
    ) -> Option<Object> {
        if Interpreter::is_truthy(&self.evaluate(&condition).unwrap()) {
            // TODO: error handling
            return self.execute(&if_stmt);
        } else if let Some(else_stmt_ref) = else_stmt.as_ref() {
            return self.execute(else_stmt_ref);
        }

        return None;
    }

    fn visit_while(&mut self, condition: &Box<Expr>, body: &Box<Stmt>) -> Option<Object> {
        //println!("in-while> evaluating condition '{:?}'", condition);
        while Self::is_truthy(&condition.visit(self).ok()?) {
            //  println!("in-while> condition '{:?}' still considered truthy", condition);
            let res = self.execute(body);

            if res.is_some() {
                return res;
            }
        }

        return None;
    }

    fn visit_function(
        &mut self,
        name: &TokenLoc,
        params: &Vec<TokenLoc>,
        body: &Box<Stmt>,
    ) -> Option<Object> {
        let name = if let Token::Identifier(name) = name.token.clone() { name }
        else { panic!("resolver::resolve_local on non-Identifier token: {:?}", name) };
        let env = Environment::with_enclosing(self.env.clone())
            .borrow()
            .clone(); // TODO FIX: this clones the parent but doesn't link back to it, so any new functions defined will not be found
        //println!("defining fn {}", name);
        //env.print(9);
        let func = LoxFunction::new(Some(name.clone()), params, body, env);
        self.env
            .borrow_mut()
            .define(name.clone(), Object::Callable(Callable::Lox(func)));

        return None;
    }

    fn visit_return(&mut self, keyword: &TokenLoc, value: &Box<Expr>) -> Option<Object> {
        self.evaluate(&value).ok()
    }
}
