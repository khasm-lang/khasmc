# Khasmc

The compiler for the khasm programming lanugage.

[![](https://tokei.rs/b1/github/khasm-lang/khasmc)](https://github.com/khasm-lang/khasmc).


## What is khasm?

Khasm is an experimental programming language based on the System-F type system, with the goal of being rather polymorphic, and rather fast, utilising memoization.

### The System-F type system

The System-F type system is a higher level type system that allows the expression of functions like this:

```ocaml
let apply_to_tuple f a b = (f a, f b)
```
Here, the type of `apply_to_tuple` would be `∀a b, (∀c, c -> c) -> a -> b -> (a, b)`, which cannot be expressed in the default type systems of OCaml or Haskell, due to the nested forall. However, the tradeoff is that you cannot have proper type inference.

## An example program:

Here's a hello world program in khasm:

```ocaml
(* Uses the stdlib. *)

let main () : () -> () =
    Stdlib.print "Hello, World!"

```
Khasm, unlike OCaml, does not have toplevel execution, so the `main` function is the entry point. It must always have type `() -> ()`.

## The TODO List:

- [X] Modules (Half done)
- [ ] ADTs and pattern matching
- [ ] Stdlib work
- [X] Backend rewrite - In progress on #middleend 
- [ ] Exceptions
- [ ] Records
- [ ] Compiler options
- [ ] Typeclasses
- [ ] Typeclass proving
- [X] Parser rewrite into recdec
# Notes:

- Khasm is currently moving away from an independent backend, previously called kavern.
- Khasm is currently undergoing a parser rewrite, in the #parser branch.
- Khasm is in extreme beta. Please do not use Khasm for any major project or programs.
