type void = | [@@deriving show { with_path = false }]

let no_void (x : void) : 'a = match x with _ -> .

(*
  https://stackoverflow.com/questions/59740132/pretty-print-a-hashtbl-in-ocaml-to-work-with-ppx-deriving
*)
module Hashtbl_p = struct
  type ('k, 's) t = ('k, 's) Hashtbl.t

  let pp pp_key pp_value ppf values =
    Format.fprintf ppf "{ ";
    Hashtbl.iter
      (fun key data ->
        Format.fprintf ppf "@[<1>%a: %a@]@." pp_key key pp_value data)
      values;
    Format.fprintf ppf " }"
end

module Hashtbl = struct
  include Hashtbl

  let add_list : ('a, 'b) Hashtbl.t -> 'a list -> 'b list -> unit =
   fun tbl xs ys -> List.iter2 (Hashtbl.add tbl) xs ys
end
