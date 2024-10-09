# Khasmc

The compiler for the khasm programming language.

## NOTE:

Khasm and khasmc are still in pre-beta development.
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

(* 
    Piping is the most natural way of expressing many problems - and it's always optimized away.
    In fact, in cases like the above, it's often possible for the entire expression to be compiled down to a single loop!
*)

```
Want laziness for list operations? We can do that too!
```ocaml
import List
import Stream

fun streaming_add_two (l: List Int): Stream Int =
    l
    |> Stream.from
    |> Stream.map (fn x -> x + 2)
```
Traits? You bet!
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

(* Trait object, too: *)

fun trait_object (t: dyn Show): String = show t
fun returns_trait_object (): dyn Show = dyn 10 (* we use dyn in both type position, and to create a trait object *)
```



## Goals:
- Simple, but expressive, with a core featureset encompassing no more than:
  - Algebraic Data Types (rust's `enum`)
  - Easy to use records
  - Pattern matching
  - Simplified traits/typeclasses
  - Easy-to-use controlled local and global mutation
  - No inductive lists by default!
- Optimizations encompassing all the common functional usecases
- A comprehensive (mostly) non-opinionated standard library
