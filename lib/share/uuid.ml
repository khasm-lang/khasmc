type uuid = UUID of (int * int)
[@@deriving show { with_path = false }]

let new_by_uuid n : (uuid, 'a) Hashtbl.t = Hashtbl.create n

let by_orig_uuid tbl (UUID (a, b)) =
  Hashtbl.find_opt tbl (UUID (a, 0))

let print_by_uuid show p =
  let s = Hashtbl.to_seq p in
  s
  |> Seq.map (fun (a, b) -> show_uuid a ^ ": " ^ show b)
  |> List.of_seq
  |> String.concat "\n"
  |> print_endline

let uuid =
  let x = ref 1000 in
  fun () ->
    incr x;
    UUID (!x, 0)

let uuid_incr_version uuid =
  let (UUID (a, b)) = uuid in
  UUID (a, b + 1)
