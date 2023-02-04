open Ast

let wrap_in name program =
  match program with Program p -> Program [ SimplModule (name, p) ]
