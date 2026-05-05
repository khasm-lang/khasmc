open IR

let rec subst_for_local_name nm replace (e : expr) =
  match e with
  | Expr (_, Named (`Local, nm), []) -> replace
  | Expr (d, tag, children) ->
      Expr
        (d, tag, List.map (subst_for_local_name nm replace) children)
