type info = ..
type data = ..
type info += Dummy
type data += Dummy'

(* unique ids *)
type id = Id of int [@@deriving show { with_path = false }]

let noid = Id (-1)
let id' () = Id (Fresh.fresh ())

let p_INFO_TABLE : (id, (info * data) list) Hashtbl.t =
  Hashtbl.create 100

let get_property (id : id) (prop : info) : data option =
  let open Monad.O in
  let* tbl = Hashtbl.find_opt p_INFO_TABLE id in
  List.assoc_opt prop tbl

let set_property (id : id) (prop : info) (data : data) : unit =
  match Hashtbl.find_opt p_INFO_TABLE id with
  | None ->
      (* create new *)
      Hashtbl.add p_INFO_TABLE id [ (prop, data) ]
  | Some l ->
      (* update *)
      Hashtbl.replace p_INFO_TABLE id ((prop, data) :: l)

let print_related_entries i printer =
  Hashtbl.iter
    (fun id v ->
      List.iter
        (fun (info, data) ->
          if info = i then
            printer id data)
        v)
    p_INFO_TABLE
