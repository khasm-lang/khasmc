# Khasmc

The compiler for the khasm programming language.

## On pause

Due to some ongoing conditions, the development of khasm is currently on pause. I'll be back at some point soon, hopefully!

## NOTE:

Khasm and khasmc are still in pre-β development.
The below partially writes like it is a currently working language - it is not.
Consider the below, for the moment, a wishlist for what this language will hopefully eventually look like.

## What is khasm?

Khasm is a functional programming language that aims to be simple, but expressive. Minimalism is *not* the name of the game - making code that's easy to understand is.

### Simple and effective type system

Khasm's type system is based off the likes of Haskell and OCaml, removing global inference. While this may seem odd, the end goal of this is to improve user experience by offering better errors and allowing programmers a more finely grained control over the code they write.

## A few example programs:

Here's a hello world program in khasm:

```ocaml
import Stdlib
let main (): unit =
    Stdlib.print "Hello, World!"
```

The classic recursive fibonacci:
```ocaml
let fib (n: int): int =
    if n <= 1 then
        1
    else fib n + fib (n - 1)
{- No let rec needed! -}
```

List operations:
```ocaml
import List

let add_three (l: List int): List int =
    l
    |> List.map (\x -> x + 3)

{- 
    Piping is the most natural way of expressing many problems - and it's always optimized away.
-}

```
Want laziness for list operations? We can do that too!
```
import List
import Stream

let streaming_add_two (l: List int): Stream int =
    l
    |> Stream.from
    |> Stream.map (\x -> x + 2)
```

## Goals:
- Simple, but expressive, with a core featureset encompassing no more than:
  - Algebraic Data Types (rust's `enum`)
  - Easy to use records
  - Pattern matching
  - Polymorphic errors (akin to OCaml's polymorphic varients)
  - Simplified traits/typeclasses ()
  - Easy-to-use controlled local and global mutation
  - No inductive lists by default!
- Optimizations encompassing all the common functional usecases
- A comprehensive (mostly) non-opinionated standard library
