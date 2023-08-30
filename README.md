# Khasmc

The compiler for the khasm programming language.

## NOTE:

Khasm and khasmc are still in pre-β development.
The below partially writes like it is a currently working language - it is not.
Consider the below, for the moment, a wishlist for what this language will hopefully eventually look like.

## What is khasm?

Khasm is an experimental programming language based on the System-F type system, with the goal of being rather polymorphic, and rather fast.

### The System-F type system

The System-F type system is a higher level type system that allows the expression of functions like this:

```ocaml
let apply_to_tuple f a b = (f a, f b)
where
typeof(a) <> typeof(b)
```
Here, the type of `apply_to_tuple` would be `∀a b, (∀c, c -> c) -> a -> b -> (a, b)`, which cannot be expressed in the default type systems of OCaml or Haskell, due to the nested forall. However, the tradeoff is that you cannot have proper type inference.

## An example program:

Here's a hello world program in khasm:

```ocaml
open Stdlib
let main () : () -> () =
    print_str "Hello, World!"

```
Khasm, unlike OCaml, does not have toplevel execution, so the `main` function is the entry point. It must always have type `() -> ()`.

## Goals:

- Builtin proofing of typeclasses to allow for a seamlessly correct experience
- A large standard library, to allow for easy usage
- Linear types for resources
- Uniqueness types for concurrent/multithreaded programming
- The full power of the ML module and System-F type systems

## TODOs

For a comprehensive list, see TODO.md.

# Notes:

- Khasm isn't even in β yet - it's probably closer to being in γ or something :P. Please do not use Khasm for any major project or programs.
