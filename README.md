# Khasmc

The compiler for the khasm programming language.

## NOTE:

Khasm and khasmc are still in pre-beta development.
The below partially writes like it is a currently working language - it is not.
Consider the below, for the moment, a wishlist for what this language will hopefully eventually look like.

## What is khasm?

Khasm is a functional programming language that aims to essentially be a better, and more functional, Go. You may ask - is this not simply Erlang? Indeed, Erlang and Khasm share similar goals. However, Erlang is untyped and runs in the BEAM VM; Khasm is typed and compiles to machine code. This is not to say that Khasm is strictly better than Erlang - this untyped and VM-based nature allows Erlang to do many things Khasm cannot.

## A few example programs:

Here's a hello world program in khasm:

```ocaml
import Stdlib
fun main (): unit =
    println "Hello, World!"
```

The classic recursive fibonacci:
```ocaml
fun fib (n: int): int =
    if n <= 1 then
        1
    else fib n + fib (n - 1)
(* No let rec needed! *)
```

List operations:
```ocaml
import List

fun process (l: List int): List int =
    List.map (fn x -> x + 3) l
    |> List.filter (fn x -> x % 2 == 0)
    |> List.map (fn x -> gcd x 10)
    |> List.fold_left (fn acc x -> acc + x)
```
Lazy list/Stream operations
```ocaml
import List
import Stream

fun streaming_add_two (l: List Int): Stream Int =
    l
    |> Stream.from
    |> Stream.map (fn x -> x + 2)
```
Traits:
```ocaml
trait Show a =
	fun show : a -> String
end

impl Show Int = 
	fun show (x: Int): String =
		int_to_string x
end

fun two_ints_to_strings (x: Int) (y: Int): (String, String) = 
	(show x, show y)

```



## Goals:
- Simple, but expressive, with a core featureset encompassing no more than:
  - Algebraic Data Types (rust's `enum`)
  - Easy to use records
  - Pattern matching
  - Simplified traits/typeclasses
  - Easy-to-use controlled local and global mutation
  - No inductive lists by default
- Native Async capabilities, a-la Erlang and Go
- Optimizations encompassing all the common functional usecases
- A comprehensive (mostly) non-opinionated standard library
