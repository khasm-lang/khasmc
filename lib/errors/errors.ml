(** Error handling *)

type position = {
  line : int;
  col : int;
}
[@@deriving show { with_path = false }]

let lexpos_to_pos (lexpos : Lexing.position) =
  { line = lexpos.pos_lnum; col = lexpos.pos_bol }

let dummypos = { line = -1; col = -1 }

type span = {
  filename : string;
  file : string ref; [@printer fun fmt t -> fprintf fmt "<src file>"]
  startloc : position;
  endloc : position;
}
[@@deriving show { with_path = false }]

let emptyspan =
  {
    filename = "#dummyfilename#";
    file = ref "empty";
    startloc = dummypos;
    endloc = dummypos;
  }

let lexbuf_to_span file (lexbuf_old : Lexing.position)
    (lexbuf_new : Lexing.position) =
  {
    filename = lexbuf_new.pos_fname;
    file = ref file;
    startloc = lexpos_to_pos lexbuf_old;
    endloc = lexpos_to_pos lexbuf_new;
  }

let spandiff old new' =
  {
    filename = old.filename;
    file = old.file;
    startloc = old.startloc;
    endloc = new'.endloc;
  }

let merge spanlist =
  if spanlist = [] then
    emptyspan
  else
    let a = List.hd spanlist in
    let b = ListHelpers.last spanlist in
    spandiff a b

type display =
  | Default
  | SurroundN of int
  | Span of span
[@@deriving show { with_path = false }]

type error = {
  mutable display : display;
  mutable infos : (span * string) list;
  file : string ref; [@printer fun f _t -> fprintf f "<src file>"]
}
[@@deriving show { with_path = false }]

let from_file file = { display = Default; infos = []; file }
let add ctx span msg = ctx.infos <- (span, msg) :: ctx.infos
let set_display ctx display = ctx.display <- display
let in_range i a b = i >= a && i <= b
let x = ref 0

let counter () =
  let tmp = !x in
  x := !x + 1;
  tmp

let ( -- ) i j =
  let rec aux n acc =
    if n < i then
      acc
    else
      aux (n - 1) (n :: acc)
  in
  aux j []

(** Generate a single span message *)
let gen_line line (span, (msg : string)) =
  (* TODO: doesn't support counter > 9 *)
  let index = counter () in
  if span = emptyspan then
    (index, "", "")
  else
    let start =
      if line = span.startloc.line - 1 then
        span.startloc.col
      else
        0
    in
    let stop =
      if line = span.endloc.line - 1 then
        span.endloc.col
      else
        Int.max_int
    in
    let middle = ((stop - start) / 2) + start in
    let go i =
      let e =
        if i >= start && i <= stop then
          if i = middle - 1 then
            "["
          else if i = middle + 1 then
            "]"
          else if i = middle then
            string_of_int index
          else
            "^"
        else
          " "
      in
      e
    in
    (index, msg, String.concat "" (List.map go (1 -- (stop + 1))) ^ "\n")

(** Generate multiple span messages *)
let generate_added index infos =
  let relevant =
    List.filter
      (fun (x, _) -> x.startloc.line - 1 = index || x.endloc.line - 1 = index)
      infos
  in
  let lines = List.map (gen_line index) (List.rev relevant) in
  let added =
    String.concat "" @@ List.map (fun (index, msg, added) -> added) lines
  in
  let post =
    String.concat ""
    @@ List.map
         (fun (index, msg, added) ->
           "[" ^ string_of_int index ^ "]: " ^ msg ^ "\n")
         lines
  in
  (added, post)

(** Converts an error ctx to a string *)
let to_string ctx =
  print_endline (show_error ctx);
  let lines =
    List.concat_map
      (fun (x, _) -> [ x.startloc.line - 1; x.endloc.line - 1 ])
      ctx.infos
  in
  let split =
    List.map (fun x -> x ^ "\n") (Str.split (Str.regexp "\n") !(ctx.file))
  in
  let relevant =
    List.filteri (fun i _elm -> List.mem i lines)
    @@ List.mapi (fun i x -> (i, x)) split
  in
  List.iter (fun (a, b) -> print_endline b) relevant;
  let bits =
    List.map
      (fun (index, line) ->
        let added, post = generate_added index ctx.infos in
        (line, added, post))
      relevant
  in
  let body =
    String.concat "" @@ List.map (fun (line, added, post) -> line ^ added) bits
  in
  let appendix =
    String.concat "" @@ List.map (fun (line, added, post) -> post) bits
  in
  body ^ "\nNotes:\n" ^ appendix
