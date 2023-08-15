exception TypeErr of string
exception NotFound of string
exception NotImpl of string
exception Impossible of string
exception UnifyErr of string
exception Todo of string
exception NotSupported of string
exception CompileError of string
exception Ambigious of string

let ambigious x = raise (Ambigious x)
let impossible x = raise (Impossible x)
let notfound x = raise (NotFound x)
let todo x = raise (Todo x)
