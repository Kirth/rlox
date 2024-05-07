#![allow(non_camel_case_types)]

use wasm_bindgen::prelude::*; // this is here for the web component.  
        // the main way for the  interpreter to externalize any state or results 
        // is by means of the print statement, which internally uses println!()
        // for the web, rather than refactoring this thing to support externalizing responses
        // I hacked in stuff

// In pass 1: I will not add any extra bells and whistles to Lox

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use rand::Rng;
use std::hash::{Hash, Hasher};

use crate::scanner::*;
use crate::parser::*;

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

fn object_type(obj: &Object) -> &'static str {
    match obj {
        Object::String(_) => "String",
        Object::Number(_) => "Number",
        Object::Boolean(_) => "Boolean",
        Object::Callable(_) => "Function",
        Object::Class(_) => "Class",
        Object::Instance(_) => "Instance",
        Object::Nil => "Nil",
    }
}

fn print_environment(env: &Rc<RefCell<Environment>>, prefix: String, is_last: bool) {
    let env_borrow = env.borrow();

    let (current_prefix, child_prefix) = if is_last {
        ("└── ", "    ")
    } else {
        ("├── ", "|   ")
    };

    println!(
        "{}{}Environment UID = {}",
        prefix, current_prefix, env_borrow.uid
    );

    if !env_borrow.values.is_empty() {
        for (key, value) in env_borrow.values.iter() {
            let type_str = object_type(value);
            println!("{}{}{}: {}", prefix, child_prefix, key, type_str);
        }
    } else {
        println!("{}{}(no keys)", prefix, child_prefix);
    }

    if let Some(ref enclosing) = env_borrow.enclosing {
        let new_prefix = format!("{}{}", prefix, child_prefix);
        print_environment(enclosing, new_prefix, true);
    }
}

#[derive(Debug, Clone)]
pub struct LoxFunction {
    // this should contain the declaration
    name: Option<String>,
    declaration: Box<Stmt>,
    params: Vec<TokenLoc>,
    closure: Rc<RefCell<Environment>>,
    // does this thing need its own environment/closure?
    is_method: bool,
}

impl PartialEq for LoxFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.params == other.params
            && &self.declaration == &other.declaration
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
                let _ = env.borrow_mut().define(name.clone(), v.clone());
            } else {
                panic!(
                    "LoxFunction::invoke assigning non-identifier {:?} as param name",
                    k
                );
            }
            //println!("binding {} == {}", k, v);
        }
        //println!("executing function {} with declaration {:?}", self.name.clone().unwrap(), self.declaration);
        interpreter.execute_block(&vec![*self.declaration.clone()], env)
        // TODO: correct environment
    }

    pub fn new(
        name: Option<String>,
        params: &Vec<TokenLoc>,
        body: &Box<Stmt>,
        closure: Rc<RefCell<Environment>>,
    ) -> Self {
        LoxFunction {
            name: name,
            params: params.clone(),
            declaration: body.clone(),
            is_method: false,
            closure: closure,
        }
    }

    /* we don't bind stuff because loxinstance.bind() would need a refcell
     pub fn bind(&self, instance: Rc<RefCell<LoxInstance>>) -> LoxFunction {
        let mut env = self.closure.clone(); // todo, this is probably bad :x because I'm cloning the closure and modifying that rahter than keeping it in the Rc chain.
        env.define("this".to_string(), Object::Instance(instance));
        return LoxFunction::new(self.name.clone(), &self.params, &self.declaration, env);
    } */
}

#[derive(Debug, Clone)]
pub struct NativeFunction {
    name: Option<String>,
    funct: fn(&Interpreter, Vec<Object>) -> Option<Object>, // todo: better error handling?
    params: Vec<String>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoxClass {
    name: String,
    superclass: Option<Box<LoxClass>>,
    methods: HashMap<String, LoxFunction>,
}

impl LoxClass {
    pub fn new(
        name: String,
        superclass: Option<Box<LoxClass>>,
        methods: HashMap<String, LoxFunction>,
    ) -> Self {
        LoxClass {
            name: name,
            superclass: superclass,
            methods: methods,
        }
    }
}

impl Hash for LoxClass {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);

        for (k, v) in &self.methods {
            k.hash(state);
            v.hash(state);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoxInstance {
    loc: TokenLoc,
    class: LoxClass,
    fields: Rc<RefCell<HashMap<String, Object>>>,
}

impl Hash for LoxInstance {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.class.hash(state);

        for (k, v) in self.fields.borrow().iter() {
            k.hash(state);
            v.hash(state);
        }
    }
}

impl LoxInstance {
    pub fn new(class: LoxClass, loc: TokenLoc) -> Self {
        LoxInstance {
            loc: loc,
            class: class,
            fields: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn get(&self, name: &String) -> Option<Object> {
        if let Some(funct) = self.class.methods.get(name) {
            let mut funct = funct.clone();
            let env = Environment::with_enclosing(funct.closure.clone());
            env.borrow_mut()
                .define("this".to_string(), Object::Instance(self.clone()));
            //print_environment(&env, "".to_string(), true);

            funct.closure = env;
            return Some(Object::Callable(Callable::Lox(funct)));
        } else if let Some(sc) = self.class.superclass.clone() {
            // this is hideous :D
            if let Some(funct) = sc.methods.get(name) {
                let mut funct = funct.clone();
                let env = Environment::with_enclosing(funct.closure.clone());
                env.borrow_mut()
                    .define("this".to_string(), Object::Instance(self.clone()));
                //print_environment(&env, "".to_string(), true);
                //panic!("env->this: {:?}", env.borrow().get(&"this".to_string())); // there seems to be an overflow happening here because of a cyclical reference?

                funct.closure = env;
                return Some(Object::Callable(Callable::Lox(funct)));
            }
        }

        return self.fields.borrow().get(name).cloned();
    }
}

#[derive(Debug, Clone)]
pub enum Object {
    String(String),
    Number(f64),
    Boolean(bool),
    Callable(Callable), // is "Function" a better name?
    Class(LoxClass),
    Instance(LoxInstance),
    Nil,
}

impl Object {
    pub fn to_num(&self) -> Option<f64> { 
        if let Object::Number(n) = &self {
            return Some(*n);
        }

        if let Object::String(s) = &self {
            return s.trim().parse::<f64>().ok();
        }

        if let Object::Boolean(b) = &self {
            return if b == &true { Some(1.0) } else { Some(0.0) };
        }

        return None;
    }

    pub fn as_string(&self) -> Option<String> {
        if let Object::String(s) = &self { return Some(s.clone()) } else { return None }
    }

    pub fn is_string(&self) -> bool {
        if let Object::String(_) = self {
            return true;
        }

        return false;
    }
    pub fn is_num(&self) -> bool {
        if let Object::Number(_) = self {
            return true;
        }

        return false;
    }

    pub fn is_nil(&self) -> bool {
        if let Object::Nil = self {
            return true;
        }

        return false;
    }

    pub fn is_falsey(&self) -> bool {
        match self {
            Object::Nil => true,
            Object::Boolean(b) => !*b,
            _ => true,
        }
    }

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
            }
            Object::Number(n) => {
                // Use the wrapper strategy indirectly for f64
                n.to_bits().hash(state);
            }
            Object::Boolean(b) => {
                b.hash(state);
            }
            Object::Callable(c) => {
                // Assuming Callable implements Hash
                c.hash(state);
            }
            Object::Class(k) => {
                k.hash(state);
            }
            Object::Instance(i) => {
                i.hash(state);
            }
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
            Object::String(s) => write!(f, "{}", s),
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
            Object::Class(k) => write!(f, "{}", k.name),
            Object::Instance(i) => write!(f, "InstanceOf({})", i.class.name),
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
    fn visit_call(
        &mut self,
        expr: Expr,
        callee: &Box<Expr>,
        paren: &TokenLoc,
        args: &Vec<Expr>,
    ) -> T;
    fn visit_get(&mut self, expr: Expr, object: &Box<Expr>, name: &TokenLoc) -> T;
    fn visit_set(
        &mut self,
        expr: Expr,
        object: &Box<Expr>,
        name: &TokenLoc,
        value: &Box<Expr>,
    ) -> T;
    fn visit_this(&mut self, expr: Expr, token: &TokenLoc) -> T;
    fn visit_super(&mut self, expr: Expr, token: &TokenLoc, method: &TokenLoc) -> T;
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
    fn visit_class(
        &mut self,
        name: &TokenLoc,
        superclass: &Option<Expr>,
        methods: &Vec<Stmt>,
    ) -> Option<Object>;
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
        //print_environment(&Rc::new(RefCell::new(self.clone())), "env_get>> ".to_string(), true);
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
        //println!("::get_at, retrieving {} at dist {} from environment:", name, distance);
        let distance = if name == &"this".to_string() || name == &"super".to_string() {
            distance - 1
        } else {
            distance
        }; // HACKFIX: very dirty!
           //print_environment(&self.ancestor(distance), "ga>> ".to_string(), true);
        return self.ancestor(distance).borrow().get(name).ok();
    }

    fn ancestor(&self, distance: usize) -> Rc<RefCell<Environment>> {
        let mut env = Rc::new(RefCell::new(self.clone()));

        for i in 0..distance {
            let next_env = {
                let benv = env.borrow();

                if benv.enclosing.is_none() {
                    eprintln!("!!! environment::ancestor: enclosing environment is None, current depth i={}, aiming dist={}", i, distance);
                }

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

impl AstPrinter {
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
        /* sometimes I comment these out because it makes debugging stuff with Environments less cluttered */
        env.define(
            "now".to_string(),
            Object::Callable(Callable::Native(NativeFunction {
                name: Some("<native fn: now>".to_string()),
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
            "str_replace".to_string(),
            Object::Callable(Callable::Native(NativeFunction {
                name: Some("<native fn: str_replace>".to_string()),
                funct: |_, params| {
                    let src = params.get(0).unwrap().to_string();
                    let from = params.get(1).unwrap().to_string();
                    let to = params.get(2).unwrap().to_string();

                    Some(Object::String(src.replace(&from, &to)))
                },
                params: vec!["source".to_string(), "from".to_string(), "to".to_string()],
            })),
        );

        env.define(
            "str_len".to_string(),
            Object::Callable(Callable::Native(NativeFunction {
                name: Some("<native fn: str_len>".to_string()),
                funct: |_, params| {
                    let src = params.get(0).unwrap().to_string();

                    Some(Object::Number(src.len() as f64))
                },
                params: vec!["source".to_string()],
            })),
        );

        env.define(
            "str_char_at".to_string(),
            Object::Callable(Callable::Native(NativeFunction {
                name: Some("<native fn: str_char_at>".to_string()),
                funct: |_, params| {
                    let src = params.get(0).unwrap().to_string();
                    let idx = params.get(1).unwrap().to_num().unwrap();

                    Some(Object::String(match src.chars().nth(idx as usize) {
                        Some(c) => c.to_string(),
                        None => "".to_string()
                    }))
                },
                params: vec!["source".to_string(), "idx".to_string()],
            })),
        );

        env.define(
            "str_contains".to_string(),
            Object::Callable(Callable::Native(NativeFunction {
                name: Some("<native fn: str_contains>".to_string()),
                funct: |_, params| {
                    let src = params.get(0).unwrap().to_string();
                    let test = params.get(1).unwrap().to_string();

                    Some(Object::Boolean(src.contains(&test)))
                },
                params: vec!["source".to_string(), "test".to_string()],
            })),
        );

        
        

        env.define(
            "dump_environment".to_string(),
            Object::Callable(Callable::Native(NativeFunction {
                name: Some("<native fn: dump_environment>".to_string()),
                params: vec![],
                funct: |i, _| {
                    print_environment(&i.env, " ".to_string(), true);
                    return Some(Object::Nil);
                }
            }))
        );

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

    pub fn interpret(&mut self, stmts: Vec<Stmt>, locals: HashMap<Expr, usize>) {
        //println!("pre-interpret() overview of locals: {:#?}", self.locals);
        self.locals = locals; // TODO: nasty hack fix to pull in the locals (rather than have them pushed in straight from the Resolver instance)

        for stmt in stmts {
            self.execute(&stmt); // todo: catching runtime errors?
        }
    }

    pub fn execute(&mut self, stmt: &Stmt) -> Option<Object> {
        stmt.visit(self)
    }

    // TODO: fn to be cleaned up; this fn was called from resolver->interpreter
    // to separate concerns, the resolver now generates the locals HashMap directly
    /*ub fn resolve(&mut self, expr: Expr, depth: usize) {
        //println!("resolving {} \t\t=> {}", expr, depth);
        self.locals.insert(expr, depth);
        //println!("RESOLVE/LOCALS: {:#?}", self.locals);
    }*/

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
            Token::THIS => "this".to_string(),
            e => format!("{:?}", e),
        };
        //println!("looking up {} through expr {:?} at dist {:?}: ", name, expr, dist);


        if let Some(dist) = dist {
            return self.env.borrow().get_at(*dist, &name);
        } else {
            ///print_environment(&self.globals, "lvr.globals>>".to_string(), true);
            return self.globals.borrow().get(&name).ok();
        }
    }

    pub fn dump_environment(&self) {
        print_environment(&self.env, ">> ".to_string(), true);
    }
}

impl ExprVisitor<Result<Object, String>> for Interpreter {
    fn visit_binary(
        &mut self,
        expr: Expr,
        left: &Expr,
        op: &TokenLoc,
        right: &Expr,
    ) -> Result<Object, String> {
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

    fn visit_logical(
        &mut self,
        expr: Expr,
        left: &Expr,
        op: &TokenLoc,
        right: &Expr,
    ) -> Result<Object, String> {
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
        self.lookup_var(&expr, name).ok_or(format!(
            "err while looking up var {:?} in visit_variable", name
        ))
    }

    fn visit_assign(
        &mut self,
        expr: Expr,
        name: &TokenLoc,
        value: &Expr,
    ) -> Result<Object, String> {
        let value = self.evaluate(value)?;
        let name = match &name.token {
            Token::Identifier(name) => name.clone(),
            e => format!("{:?}", e),
        };

        if let Some(dist) = self.locals.get(&expr) {
            self.env
                .borrow_mut()
                .assign_at(*dist, name.clone(), value.clone());
            return Ok(value); // QUEST: do we need this?  would this mean var a = (b = c); a == c?
        } else {
            self.globals
                .borrow_mut()
                .assign(name.clone(), value.clone());
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
        } else if let Object::Class(k) = callee {
            let instance = LoxInstance::new(k, paren.clone());
            let init = instance.class.methods.get("init").map(|m| m.clone());
            if let Some(init) = init {
                init.closure
                    .borrow_mut()
                    .define("this".to_string(), Object::Instance(instance.clone()));
                //println!("invoking class {} initializer", instance.class.name);
                init.invoke(self, args?);
            }
            return Ok(Object::Instance(instance));
        } else {
            eprintln!(
                "invalid function call: callee '{:?}' is not callable",
                callee
            );
            return Err(format!(
                "invalid function call: callee '{:?}' is not callable",
                callee
            ));
        }
        //return Ok(Object::String(format!("fn '{:?}' with {:?} was called, but function calls are not implemented yet :3", callee, args)));
    }

    fn visit_get(
        &mut self,
        expr: Expr,
        object: &Box<Expr>,
        name: &TokenLoc,
    ) -> Result<Object, String> {
        let object = self.evaluate(&*object.clone())?;
        let name = if let Token::Identifier(name) = name.token.clone() {
            name
        } else {
            panic!(
                "interpreter::visit_get, property accessor is non-Identifier token: {:?}",
                name
            )
        };

        if let Object::Instance(k) = object {
            return k // bug: this.foo = this.foo + 1 returns a borrowed error
                .get(&name)
                .map(|v| v.clone())
                .ok_or(format!("Object does not have field {}", name));
        }

        eprintln!("Error accessing property '{}' on object {:?}", name, object);
        return Err(format!(
            "Error accessing property '{}' on object {:?}",
            name, object
        ));
    }

    fn visit_set(
        &mut self,
        expr: Expr,
        object: &Box<Expr>,
        name: &TokenLoc,
        value: &Box<Expr>,
    ) -> Result<Object, String> {
        let object = self.evaluate(object.as_ref())?;
        let name = if let Token::Identifier(name) = name.token.clone() {
            name
        } else {
            panic!(
                "interpreter::visit_set, property accessor is non-Identifier token: {:?}",
                name
            )
        };

        if let Object::Instance(inst) = object {
            // TODO: why does this not mutate the instance directly?
            // this don't look pretty but they won't let me if let != foobar :(
            let value = self.evaluate(&*value)?;
            inst.fields.borrow_mut().insert(name, value.clone());

            return Ok(value);
        } else {
            eprintln!("Error accessing property '{}' on object {:?}", name, object);
            return Err(format!(
                "Error accessing property '{}' on object {:?}",
                name, object
            ));
        }
    }

    fn visit_this(&mut self, expr: Expr, token: &TokenLoc) -> Result<Object, String> {
        self.lookup_var(&expr, token).ok_or(format!(
            "failed looking up 'this' in environment from expr: {:?}",
            expr
        ))
    }

    fn visit_super(
        &mut self,
        expr: Expr,
        token: &TokenLoc,
        method: &TokenLoc,
    ) -> Result<Object, String> {
        let distance = self.locals.get(&expr).unwrap();
        //println!("VISIT_SUPER looking for {:?} at {}", expr, distance);
        //print_environment(&self.env, format!("visit_super.{}>", distance), true);

        let superclass = if let Object::Class(superclass) = self
            .env
            .borrow()
            .get_at(*distance, &"super".to_string())
            .ok_or("interpreter::visit_super: failed looking up super".to_string())?
        {
            superclass
        } else {
            unreachable!();
            panic!("superclass is not a class :o");
        };

        let object = if let Object::Instance(object) = self
            .env
            .borrow()
            .get_at(*distance - 1, &"this".to_string())
            .ok_or("interpreter::visit_super: failed looking up this")?
        {
            object
        } else {
            unreachable!();
            panic!("object is not an instance :o");
        };
        

        let method = if let Token::Identifier(method) = method.token.clone() { method } else { panic!("interpreter::visit_super: method name is non-Identifier token variant: {:?}", method); };
        

        //println!("visit_super:looking up method {}", &format!("{}", method));
        let method = superclass.methods.get(&method).unwrap().clone(); // TODO, is the format! a good idea? nope, that don't werk BUG
        method.closure.borrow_mut().define("this".to_string(), Object::Instance(object));
        
        return Ok(Object::Callable(Callable::Lox(method)));
        


    }
}

impl StmtVisitor for Interpreter {
    fn visit_expression(&mut self, expr: &Box<Expr>) -> Option<Object> {
        self.evaluate(&expr); // we only want explicit returns
        None
    }

    #[cfg(target_arch = "wasm32")]
    fn visit_print(&mut self, expr: &Box<Expr>) -> Option<Object> {

        #[wasm_bindgen]
        extern "C" {
            #[wasm_bindgen(js_namespace = console)]
            fn log(s: &str);
        }

        let value = self.evaluate(&expr);
        log(&format!(
            "{}",
            match value {
                Ok(v) => v,
                Err(e) => Object::String(e),
            }
        ));

        return None;
    }

    #[cfg(not(target_arch = "wasm32"))]
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
        } else {
            Object::Nil
        };

        let name = match &name.token {
            Token::Identifier(name) => name.clone(),
            e => format!("{:?}", e),
        };

        self.env.borrow_mut().define(name.clone(), value);
        //print_environment(&self.env, format!("visit_var:{}", name), true);
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
        let name = if let Token::Identifier(name) = name.token.clone() {
            name
        } else {
            panic!(
                "interpreter::visit_function, function name is non-Identifier token: {:?}",
                name
            )
        };
        let env = Environment::with_enclosing(self.env.clone())
            .borrow()
            .clone(); // TODO FIX: this clones the parent but doesn't link back to it, so any new functions defined will not be found
                      //println!("defining fn {}", name);
                      //env.print(9);
        let func = LoxFunction::new(Some(name.clone()), params, body, Rc::new(RefCell::new(env)));
        self.env
            .borrow_mut()
            .define(name.clone(), Object::Callable(Callable::Lox(func)));

        return None;
    }

    fn visit_return(&mut self, keyword: &TokenLoc, value: &Box<Expr>) -> Option<Object> {
        self.evaluate(&value).ok()
    }

    fn visit_class(
        &mut self,
        name: &TokenLoc,
        superclass: &Option<Expr>,
        methods: &Vec<Stmt>,
    ) -> Option<Object> {
        let name = if let Token::Identifier(name) = name.token.clone() {
            name
        } else {
            panic!(
                "interpreter::visit_class, class name is non-Identifier token: {:?}",
                name
            )
        };

        let superclass = if let Some(superclass) = superclass {
            let superclass = self.evaluate(superclass);
            if superclass.is_err() {
                // TODO: I can do this cleaner :3
                panic!("Error evaluating superclass for class {}", name);
            }
            let superclass = superclass.unwrap();

            if let Object::Class(superclass) = superclass {
                Some(Box::new(superclass))
            } else {
                panic!(
                    "Object {:?} is not a valid superclass for {}",
                    superclass, name
                );
            }
        } else {
            None
        };

        if superclass.is_some() {
            let old_env = self.env.clone();
            self.env = Environment::with_enclosing(old_env);
            self.env.borrow_mut().define("super".to_string(), Object::Class(*superclass.clone().unwrap()));
        }

        let methods = methods
            .iter()
            .filter_map(|m| {
                if let Stmt::Function(name, params, body) = m {
                    let fn_name = if let Token::Identifier(name) = name.token.clone() {
                        name
                    } else {
                        panic!(
                            "interpreter::visit_class, method name is non-Identifier token: {:?}",
                            name
                        )
                    }; // this is getting ridiculous...
                    Some((
                        fn_name.clone(),
                        LoxFunction::new(Some(fn_name), &params, &body, self.env.clone()),
                    )) // TODO: is cloning the current env into there a good idea?
                } else {
                    panic!(
                        "interpreter::visit_class: method expr is non-function: {:?}",
                        m
                    );
                }
            })
            .collect();

        if superclass.is_some() {
            let enclosing = self.env.borrow().enclosing.clone();
            self.env = enclosing.unwrap();
        }

        let klass = Object::Class(LoxClass::new(name.clone(), superclass.clone(), methods));
        self.env.borrow_mut().define(name.clone(), klass.clone());

        return None;
    }
}
