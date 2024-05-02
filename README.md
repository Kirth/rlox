# rlox

A Rust implementation of Lox, the language from CraftingInterpreters.com by Robert Nystrom.

* [Lox Language Overview](https://craftinginterpreters.com/the-lox-language.html)


```
docker build -t rlox .
```

```
docker run --rm -ti -v $(pwd)/examples:/scripts/ rlox /root/rlox /scripts/hello_world.lox
```

```
printf 'print str_len("Hello world!");' | docker run --rm -i rlox
```


## Further Exercises

Rust has a delightful standard library which allowed me to skip many of the exercises in the second part of the book, on implementing data structures in C:
- Resizable Array and Stack
- HashMap

It may be prudent to revisit those separately.