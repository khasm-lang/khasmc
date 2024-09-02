let ( let* ) (x : ('a, 'b) result) (f : 'a -> ('c, 'b) result) =
  match x with Ok x -> f x | Error x -> Error x

let err x = Error x
let ok x = Ok x

let ( let** ) (x : 'a option * string) (f : 'a -> ('c, string) result)
    =
  match fst x with Some n -> f n | None -> Error (snd x)
