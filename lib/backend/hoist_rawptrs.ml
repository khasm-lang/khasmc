open Exp
open Khagm

let hoist ctx args =
  let n =
    List.map
      (fun x ->
        if List.mem x ctx then
          Kir.get_random_num ()
        else
          x)
      args
  in
  let adds =
    List.map2
      (fun x y ->
        if x = y then
          []
        else
          [ LetInVal (x, Val y) ])
      n args
    |> List.concat
  in
  (adds, n)

let rec hoist_expr ctx e =
  match e with
  | [] -> []
  | curr :: rest -> (
      match curr with
      | LetInCall (id, func, args) ->
          print_endline "hi!";
          let add, args' = hoist ctx args in
          let add1, func' :: _ = hoist ctx [ func ] in
          add1 @ add @ (LetInCall (id, func', args') :: hoist_expr ctx rest)
      | _ -> curr :: hoist_expr ctx rest)

let hoist_top ctx code =
  List.map
    (fun elm ->
      match elm with
      | Let (id, args, body) -> Let (id, args, hoist_expr ctx body)
      | _ -> elm)
    code

let hoist_rawptrs (khagm : Khagm.khagm) =
  let code, tbl = khagm in
  let fst_binds = List.map fst tbl.binds in
  (hoist_top fst_binds code, tbl)
