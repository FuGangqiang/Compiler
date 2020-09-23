# My Compiler(WIP)


## Goals

- LL parsing: time complexity of O(n) 
- C89 implementation: almost every c compiler can build it
- less dependence: bootstrapping depends only on the c compiler

## Todos

- [x] lexer
- [x] parser
- [ ] macro expander
- [ ] type checker
- [ ] ir

## Test

```sh
make test

# memery check
make test-mem

# change compiler
CC=clang make
```
