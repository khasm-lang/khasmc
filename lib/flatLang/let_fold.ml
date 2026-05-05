open IR
open IR_helpers

let rec let_fold_e did_something (expr : expr) =
  let go = let_fold_e did_something in
  match expr with
  | Expr (d1, Let replace, [
      (Expr (d2, Named (tag, nm), []) as e);
      rest
    ]) ->
    did_something := true;
    subst_for_local_name replace e rest
    |> go
  | Expr (dat, tag, children) ->
    Expr (dat, tag, List.map go children)

let let_fold_one did top =
  let fold_def def =
    { def with
      body = let_fold_e did def.body
    }
  in
  { top with
    defs = List.map fold_def top.defs 
  }

let let_fold_n top =
  let did = ref false in
  let rec go n top =
    if n <= 0 then top else
      let_fold_one did top
      |> go (n - 1)
  in
  (* TODO: proper heuristic *)
  go 5 top

let let_fold_fixpoint top =
  let iters = ref 0 in
  let did = ref false in
  let rec go top =
    did := false;
    incr iters;
    print_endline ("let fold iter: " ^ string_of_int !iters);
    let next = let_fold_one did top in
    if !did then
      go next
    else
      top
  in
  go top
let let_fold top =
  let_fold_fixpoint top
