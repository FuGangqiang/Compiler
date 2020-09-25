# My Compiler

WIP: Work In Process

- [x] lexer
- [x] parser
- [ ] macro expander
- [ ] type checker
- [ ] IR

## Goals

- LL parsing: time complexity of O(n) 
- C89 implementation: almost every c compiler can build it
- unicode symbol: path, file, pkg, mod, type, fn, var, macro, label, ..., unicode symbol everywhere
- less dependence: bootstrapping depends only on the c compiler
- modern lang features:
    - minimal runtime
    - module system
    - attribute system
    - type inference
    - hygenic macro
    - pattern match
    - algebraic data types
    - trait-based generics
    - modern package manager
    - friendly test system

This is a **long-term project**!

## Test

```sh
make test

# memery check
make test-mem

# change compiler
CC=clang make

# clear built tmp files
make clean
```
