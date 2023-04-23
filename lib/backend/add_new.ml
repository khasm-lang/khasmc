open Khagm

(* Gives the lambda lifted things names *)

let rec add_new top (nms : Kir.transtable) =
  match top with
  | [] -> nms
  | Let (id, _, _) :: xs -> (
      match List.assoc_opt id nms with
      | Some _ -> add_new xs nms
      | None ->
          let name = "compobj" ^ string_of_int id in
          add_new xs ((id, name) :: nms))
  | _ :: xs -> add_new xs nms
