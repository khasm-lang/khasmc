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
  | [] -> raise @@ Impossible "Listlast : empty list"
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

let rec double_partition f a b =
  match (a, b) with
  | [], [] -> (([], []), ([], []))
  | x :: xs, y :: ys ->
      let (fa, fb), (ga, gb) = double_partition f xs ys in
      if f x then
        ((x :: fa, fb), (y :: ga, gb))
      else
        ((fa, x :: fb), (ga, y :: gb))
  | _ -> raise @@ NotFound "double_partition uneq lists"

type threestate =
  | True
  | False
  | Both

let partition_three p l =
  let rec part yes no = function
    | [] -> (List.rev yes, List.rev no)
    | x :: l -> (
        match p x with
        | True -> part (x :: yes) no l
        | False -> part yes (x :: no) l
        | Both -> part (x :: yes) (x :: no) l)
  in
  part [] [] l

let[@tail_mod_cons] rec map f x =
  match x with
  | [] -> []
  | x :: xs ->
      let y = f x in
      y :: map f xs

let rec take l n =
  match (n, l) with
  | 0, _ -> []
  | n, x :: xs -> x :: take xs (n - 1)
  | _, _ -> raise Not_found

let maybe_take l n = try Some (take l n) with Not_found -> None

let rec without_n l n =
  match (n, l) with
  | 0, l -> l
  | n, x :: xs -> without_n xs (n - 1)
  | _, _ -> raise Not_found

let rec maybe_without_n l n = try Some (without_n l n) with Not_found -> None

let rec increasing start num =
  match num with 0 -> [] | n -> start :: increasing (start + 1) (n - 1)

let rec in_x_not_y x y = List.filter (fun z -> not @@ List.mem z y) x
