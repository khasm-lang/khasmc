type span =
  | Empty
  | Span of string * int * int
[@@deriving show { with_path = false }]

let time = ref false
let debug_parse_verbose = ref false
let debug_parse = ref false
let debug_flat_verbose = ref false
let debug_flat = ref false
let debug_gc = ref false
let emit = ref false
let input_files : string list ref = ref []
let debug s = Printf.fprintf stderr "debug: %s" s

module type WithDebug = sig
  val name : string
  val debug : bool ref
end

module Debug (T : WithDebug) = struct
  let debug s =
    if !T.debug then begin
      print_endline @@ "== DEBUG " ^ T.name ^ " ==";
      print_endline (Lazy.force s);
      print_endline "== END DEBUG ==\n"
    end

  let error span s = failwith "implement spans"
  let warn span s = failwith "implement warnings"
end

module DebugParse = Debug (struct
  let name = "DebugParse"
  let debug = debug_parse
end)

module DebugFlat = Debug (struct
  let name = "DebugFlat"
  let debug = debug_flat
end)
