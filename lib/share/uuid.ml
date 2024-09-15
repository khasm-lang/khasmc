type uuid = UUID of int [@@deriving show { with_path = false }]
type 'a by_uuid = (uuid, 'a) Hashtbl.t

let new_by_uuid n : (uuid, 'a) Hashtbl.t = Hashtbl.create n

let print_by_uuid show p =
  let s = Hashtbl.to_seq p in
  s
  |> Seq.map (fun (a, b) -> show_uuid a ^ ": " ^ show b)
  |> List.of_seq
  |> String.concat "\n"
  |> print_endline

let uuid =
  let x = ref (-1) in
  fun () ->
    incr x;
    UUID !x
