let rec map_rev f l =
  match l with
  | [] -> []
  | [ x ] -> [ f x ]
  | x :: xs ->
      let rest = map_rev f xs in
      f x :: rest
