use wasm_bindgen::prelude::*;
use ::interpreter::*;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn interpret(s: String) {
    let mut lox = Lox::new();
    lox.run(s);
}