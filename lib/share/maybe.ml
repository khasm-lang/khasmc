type no = No [@@deriving show { with_path = false }]
type yes = Yes [@@deriving show { with_path = false }]

type (_, _) maybe =
  | Nothing : ('a, no) maybe
  | Just : 'a -> ('a, yes) maybe

let pp_maybe : type q w.
    ('a -> q -> unit) -> 's -> 'a -> (q, w) maybe -> unit =
 fun p1 _ fmt x ->
  match x with
  | Nothing -> Format.fprintf fmt "Nothing"
  | Just x -> Format.fprintf fmt "Just (%a)" p1 x

let get (Just x) = x

let to_option : type a. ('b, a) maybe -> 'b option =
 fun x -> match x with Nothing -> None | Just x -> Some x

let try_from_option : 'b option -> ('b, yes) maybe option =
 fun x -> match x with None -> None | Some x -> Some (Just x)

let forget : ('a, yes) maybe -> ('a, no) maybe = fun x -> Nothing
