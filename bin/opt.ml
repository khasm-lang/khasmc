open Ast

let addStr x y =
  string_of_int ((int_of_string x) + (int_of_string y))

let subStr x y =
  string_of_int ((int_of_string x) - (int_of_string y))

let mulStr x y =
  string_of_int ((int_of_string x) * (int_of_string y))

let addStrf x y =
  string_of_float ((float_of_string x) +. (float_of_string y))

let subStrf x y =
  string_of_float ((float_of_string x) -. (float_of_string y))

let mulStrf x y =
  string_of_float ((float_of_string x) *. (float_of_string y))


let rec optBinOpSub e =
  match e with
  | Paren(BinOp (e1, b, e2)) -> optBinOp e1 b e2
  | _ -> e

and optBinOp e1 b e2 =
  let n1 = optBinOpSub e1 in
  let n2 = optBinOpSub e2 in
  match n1, b, n2 with
  | (Base(Int(x))), (BinOpPlus), (Base(Int(y))) -> Base(Int(addStr x y))
  | (Base(Int(x))), (BinOpMinus), (Base(Int(y))) -> Base(Int(subStr x y))
  | (Base(Int(x))), (BinOpMul), (Base(Int(y))) -> Base(Int(mulStr x y))
  | (Base(Float(x))), (BinOpPlus), (Base(Float(y))) -> Base(Float(addStrf x y))
  | (Base(Float(x))), (BinOpMinus), (Base(Float(y))) -> Base(Float(subStrf x y))
  | (Base(Float(x))), (BinOpMul), (Base(Float(y))) -> Base(Float(mulStrf x y))
  | _ -> BinOp(n1, b, n2)

let optExpr e =
  match e with
  | BinOp (e1, b, e2) -> optBinOp e1 b e2
  | _ -> e

let rec optBlock b =
  match b with
  | Many (x) -> Many(List.map optBlock x)
  | AssignBlock (i, il, b) -> AssignBlock(i, il, (optBlock b))
  | Assign (i, il, e) -> Assign(i, il, (optExpr e))
  | If (i, b) -> If(i, (optBlock b))
  | While (i, b) -> While(i, (optBlock b))
  | Return (e) -> Return(optExpr e)
  | _ -> b

let optToplevel_h t =
  match t with
  | AssignBlock (i, il, b) -> AssignBlock(i, il, (optBlock b))
  | Assign (i, il, e) -> Assign(i, il, (optExpr e))
  | _ -> t

let rec optToplevel t =
  match t with
  | [] -> []
  | x :: xs ->
     ([optToplevel_h x]) @ (optToplevel xs)

let rec optToplevelList l =
  match l with
  | [] -> []
  | x :: xs ->
     optToplevel x :: optToplevelList xs
