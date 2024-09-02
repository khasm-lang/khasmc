type uuid = UUID of int
[@@deriving show {with_path = false}]

type 'a by_uuid = (uuid, 'a) Hashtbl.t

let new_by_uuid n : (uuid, 'a) Hashtbl.t = Hashtbl.create n 

let uuid =
  let x = ref (-1) in
  fun () ->
  incr x;
  !x
