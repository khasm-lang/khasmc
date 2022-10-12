open Filename
exception Impossible

let rec output_code files codes =
  match (files, codes) with
  | ([], []) -> ()
  | (x :: xs, y :: ys) ->
     begin
       let fp = open_out ((remove_extension (basename x)) ^ ".out") in
       Printf.fprintf fp "%s" y
     end;
     output_code xs ys
  | (_, _) -> raise Impossible
