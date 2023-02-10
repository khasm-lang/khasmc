# Typecheck.ml - The frontend typechecker

This is a high level documentation of the khasm typechecker.

## Important functions:

### `typecheck_program_list program_list`

type: `Ast.program list -> ()`

desc:

Typechecks a list of khasm programs - These must be formatted in a spesfic way, produced by the earlier parts of the compiler. 
If this function returns, the program list is validly typed.

throws: UnifyErr, TypeErr, NotFound, Impossible

### `check term type`

type: `ctx -> Ast.expr -> Ast.typesig -> ()`

desc:

Checks a term has a type. If this returns, the term has that type.

throws: TypeErr, UnifyErr

### `infer term`

type: `ctx -> Ast.expr -> Ast.typesig`

desc:

Attempts to infer the type of an expression.

throws: TypeErr, UnifyErr
