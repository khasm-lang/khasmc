# Khasmc

The compiler for the khasm programming lanugage.

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

- [ ] Modules
- [ ] ADTs and pattern matching
- [ ] Stdlib work
- [ ] Backend rewrite - memoization 
- [ ] Exceptions
- [ ] Records
- [ ] Compiler options
- [ ] Typeclasses
- [ ] Typeclass proving
- [ ] Parser rewrite into recdec - maybe do earlier?
