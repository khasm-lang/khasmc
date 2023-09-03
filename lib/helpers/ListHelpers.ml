open Exp

let rec map_rev f l =
  match l with
  | [] -> []
  | [ x ] -> [ f x ]
  | x :: xs ->
      let rest = map_rev f xs in
      f x :: rest

let rec filter_extract_h fn list acn acy =
  match list with
  | [] -> (acn, acy)
  | x :: xs ->
      if fn x then
        filter_extract_h fn xs acn (x :: acy)
      else
        filter_extract_h fn xs (x :: acn) acy

let filter_extract fn list = filter_extract_h fn list [] []

let rec last xs =
  match xs with
  | [] -> raise @@ Impossible "ListHelpers.last : empty list"
  | [ x ] -> x
  | _ :: xs -> last xs

let rec fold_left' f xs =
  match xs with
  | [] -> raise @@ Impossible "fold_left' empty"
  | [ x ] -> x
  | x :: xs -> f x (fold_left' f xs)

let rec transpose list =
  match list with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss -> List.((x :: map hd xss) :: transpose (xs :: map tl xss))

let rec make_headh xs i acc =
  match xs with
  | [] -> raise @@ NotFound "make_headh"
  | x :: xs ->
      if i = 0 then
        (x :: List.rev acc) @ xs
      else
        make_headh xs (i - 1) (x :: acc)

let make_head xs i = make_headh xs i []
let ( let* ) o f = match o with None -> None | Some x -> f x

let rec indexof xs i =
  match xs with
  | [] -> None
  | x :: xs ->
      if x = i then
        Some 0
      else
        let* x = indexof xs i in
        Some (x + 1)
