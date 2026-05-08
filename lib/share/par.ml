let use_par = ref false
let num_domains = ref 16

(* TODO: configurable *)
let par_chunk = 1000
let par_map f x = List.map f x
