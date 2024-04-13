# rlox

A Rust implementation of Lox, the language from CraftingInterpreters.com by Robert Nystrom.

* [Lox Language Overview](https://craftinginterpreters.com/the-lox-language.html)


```
docker build -t rlox .
```

```
docker run --rm -ti -v $(pwd)/examples:/scripts/ rlox /root/rlox /scripts/hello_world.lox
```