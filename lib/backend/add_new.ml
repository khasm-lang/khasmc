open Khagm
(** Gives the lambda lifted things names *)

let rec add_new top nms =
  match top with
  | [] -> nms
  | Let (id, _, _) :: xs -> (
      match Kir.get_bind_id nms id with
      | Some _ -> add_new xs nms
      | None ->
          let name = "compobj" ^ string_of_int id in
          add_new xs (Kir.add_bind nms name id))
  | _ :: xs -> add_new xs nms
