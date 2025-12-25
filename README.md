# Khasmc

The compiler for the khasm programming language.

## NOTE:

Khasm and khasmc are still in pre-beta development.
The below partially writes like it is a currently working language - it is not.
Consider the below, for the moment, a wishlist for what this language will hopefully eventually look like.

The code quality is also not always amazing. I am more interested in getting it working.

## What is khasm?

Khasm is a functional programming language that aims to essentially be a better, and more functional, Go. You may ask - is this not simply Erlang? Indeed, Erlang and Khasm share similar goals. However, Erlang is untyped and runs in the BEAM VM; Khasm is typed and compiles to machine code. This is not to say that Khasm is strictly better than Erlang - this untyped and VM-based nature allows Erlang to do many things Khasm cannot.


## Goals:
- Simple, but expressive, with a core featureset encompassing no more than:
  - Algebraic Data Types (rust's `enum`)
  - Easy to use records
  - Pattern matching
  - Simplified traits/typeclasses (backburner)
  - Easy-to-use controlled local and global mutation
- Native Async capabilities, a-la Erlang and Go
- Optimizations encompassing all the common functional usecases
- A comprehensive (mostly) non-opinionated standard library
