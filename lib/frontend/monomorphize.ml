open Parsing.Ast
open Typecheck
open Share.Maybe
open Share.Uuid


let monomorphize (top : (resolved, unit) toplevel list)
    : (resolved, resolved typ) toplevel list =
  failwith "todo: monomorphize"
