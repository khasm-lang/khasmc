type 'a uuid = UUID of (int * 'a)
[@@deriving show { with_path = false }]

let new_by_uuid n : ('a uuid, 'b) Hashtbl.t = Hashtbl.create n
let uuid_orig (UUID (a, b)) = UUID (a, 0)

let uuid_by_orig tbl (UUID (a, b)) =
  Hashtbl.find_opt tbl (UUID (a, 0))

let print_by_uuid show showsnd p =
  let s = Hashtbl.to_seq p in
  s
  |> Seq.map (fun (a, b) -> show_uuid showsnd a ^ ": " ^ show b)
  |> List.of_seq
  |> String.concat "\n"
  |> print_endline

module Priv : sig
  val uuid : unit -> unit uuid
  val uuid_using : 'a -> 'a uuid
end = struct
  let uuid_counter = ref 1000

  let uuid : unit -> unit uuid =
   fun () ->
    incr uuid_counter;
    UUID (!uuid_counter, ())

  let uuid_using : 'a -> 'a uuid =
   fun p ->
    incr uuid_counter;
    UUID (!uuid_counter, p)
end

include Priv

let uuid_forget (v : 'a uuid) : unit uuid =
  let (UUID (a, b)) = v in
  UUID (a, ())

let uuid_set_snd v uuid =
  let (UUID (a, b)) = uuid in
  UUID (a, v)

let uuid_get_snd (UUID (a, b)) = b
