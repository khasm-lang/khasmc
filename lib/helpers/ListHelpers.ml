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
