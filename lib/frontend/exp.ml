exception TypeErr of string
exception NotFound of string
exception NotImpl of string
exception Impossible of string
exception UnifyErr of string
exception Todo of string
exception NotSupported of string
exception CompileError of string

let todo x = raise (Todo x)
