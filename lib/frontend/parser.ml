let digit = [%sedlex.regexp? '0' .. '9']
let num = [%sedlex.regexp? Plus digit]
let id = [%sedlex.regexp? Plus ll]
let tid = [%sedlex.regexp? lu, Plus (ll | lu)]
let polyid = [%sedlex.regexp? '\'', id]
let space = [%sedlex.regexp? Plus (zs | cc)]
let char = [%sedlex.regexp? Compl '"']
let string = [%sedlex.regexp? '"', Star char, '"']
let float = [%sedlex.regexp? num, '.', num]

open Token

let rec lexer_ buf : (t_TOKEN, exn) Result.t =
  match
    begin
      match%sedlex buf with space -> () | _ -> ()
    end;
    begin
      match%sedlex buf with
      | '(' -> LEFTP
      | ')' -> RIGHTP
      | '{' -> LEFTC
      | '}' -> RIGHTC
      | '>' -> GT
      | '<' -> LT
      | '$' -> DOLLAR
      | '#' -> HASH
      | '@' -> AT
      | '!' -> BANG
      | '*' -> STAR
      | '%' -> PERCENT
      | '+' -> PLUS
      | '-' -> MINUS
      | '&' -> AND
      | '|' -> PIPE
      | ',' -> COMMA
      | ';' -> SEMICOLON
      | ':' -> COLON
      | '=' -> EQUALS
      | '/' -> FSLASH
      | '\\' -> BSLASH
      | "type" -> TYPE
      | "trait" -> TRAIT
      | "ref" -> REF
      | "where" -> WHERE
      | "let" -> LET
      | "in" -> IN
      | "as" -> AS
      | "->" -> ARROW
      | "Int" -> TYINT
      | "String" -> TYSTRING
      | "Char" -> TYCHAR
      | "Float64" -> TYFLOAT
      | "Bool" -> TYBOOL
      | "impl" -> IMPL
      | "module" -> MODULE
      | "end" -> END
      | "match" -> MATCH
      | "fun" -> FUN
      | "true" -> BOOL true
      | "false" -> BOOL false
      | string -> STRING (Sedlexing.Utf8.lexeme buf)
      | id -> ID (Sedlexing.Utf8.lexeme buf)
      | tid -> TYPEID (Sedlexing.Utf8.lexeme buf)
      | polyid -> POLYID (Sedlexing.Utf8.lexeme buf)
      | num -> INT (Sedlexing.Utf8.lexeme buf)
      | float -> FLOAT (Sedlexing.Utf8.lexeme buf)
      | eof -> DONE
      | any -> failwith (Sedlexing.Utf8.lexeme buf)
      | _ -> failwith "IMPOSSIBLE"
    end
  with
  | s -> Ok s
  | exception e ->
      print_endline "ERROR!";
      print_endline (Printexc.to_string e);
      Error e

let lexer buf =
  let rec go acc =
    match lexer_ buf with
    | Ok DONE -> List.rev acc
    | Ok s -> go (s :: acc)
    | Error e -> List.rev acc
  in
  go []

let next buf =
  match !buf with
  | x :: xs ->
      buf := xs;
      x
  | [] -> failwith "next on empty buffer"

let next' buf = ignore (next buf)

let peek buf =
  match !buf with
  | x :: xs -> x
  | [] -> failwith "peek on empty buffer"

let peek2 buf =
  match !buf with
  | x :: y :: xs -> y
  | _ -> failwith "peek2 on emptyish buffer"

let type' buf = failwith "parse types"
let expr buf = failwith "parse exprs"

let rec toplevel' buf =
  match next buf with
  | FUN -> begin
      let (ID name) = next buf in
      let targs =
        match peek buf with
        | LEFTB ->
            let xs =
              begin
                match peek2 buf with
                | TYPE ->
                    next' buf;
                    next' buf;
                    let rec go () =
                      match next buf with
                      | TYPEID t -> t :: go ()
                      | RIGHTB -> []
                      | _ -> failwith "parsing type list what"
                    in
                    Some (go ())
                | _ -> None
              end
            in
            xs
        | _ -> None
      in
      let targs = match targs with Some xs -> xs | None -> [] in
      let bounds = failwith "bounds" in
      let arg_list =
        let rec go () =
          match peek buf with
          | LEFTP ->
              next' buf;
              let (ID argnm) = next buf in
              let COLON = next buf in
              let typ = type' buf in
              let RIGHTP = next buf in
              (argnm, typ) :: go ()
          | _ -> []
        in
        go ()
      in
      let COLON = next buf in
      let typ = type' buf in
      let EQUALS = next buf in
      let body = expr buf in
      failwith "todo"
    end
  | _ -> failwith "todo"

let toplevel buf =
  let toks = ref (lexer buf) in
  List.iter (fun x -> print_string (show_t_TOKEN x ^ " ")) !toks;
  print_newline ();
  toplevel' toks
