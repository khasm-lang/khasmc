# Typechecking in khasmc

so basically, everything on the toplevel has to have a type assigned to it, like so:

```

sig id : ∀ 'a, 'a -> 'a

let id x = x

```
because otherwise, with rank N types, we can't infer everything ( gotta love godel :D )
you'll notice that there's a ton of logic within `subtype.ml` - this is because just checking a subtype is a pain.

take for example, this:

```

∀a b, (a -> b) -> a -> b

subtype_of

(int -> bool) -> int -> bool

```
you have to bind `a := int` and `b := bool`, and because of course you're doing this all recursively, you need to export those bindings *back upwards*, which is so much fun to work out :D 

* note to self: baturef would probably work if you ever refactor this
* also i bound `<@@>` to `subtype_of` (in a sub module ofc) because `if a <@@> b then ` looks really clean and it's not that hard to read, really
