open Ast

type t_TOKEN =
  | LEFTB
  | RIGHTB
  | LEFTP
  | RIGHTP
  | LEFTC
  | RIGHTC
  | GT
  | LT
  | DOLLAR
  | HASH
  | AT
  | STAR
  | BANG
  | PERCENT
  | PLUS
  | MINUS
  | AND
  | PIPE
  | COMMA
  | SEMICOLON
  | COLON
  | EQUALS
  | FSLASH
  | BSLASH
  | TYPE
  | TRAIT
  | REF
  | WHERE
  | LET
  | IN
  | AS
  | ARROW
  | TYINT
  | TYSTRING
  | TYCHAR
  | TYFLOAT
  | TYBOOL
  | IMPL
  | MODULE
  | END
  | MATCH
  | FUN
  | IF
  | THEN
  | ELSE
  | BOOL of bool
  | STRING of string
  | ID of resolved
  | TYPEID of resolved
  | POLYID of resolved
  | INT of string
  | FLOAT of string
  | DONE
  | OTHER of string
[@@deriving show { with_path = false }]

let digit = [%sedlex.regexp? '0' .. '9']
let num = [%sedlex.regexp? Plus digit]
let id = [%sedlex.regexp? Plus ll]
let tid = [%sedlex.regexp? lu, Plus (ll | lu)]
let polyid = [%sedlex.regexp? '\'', id]
let space = [%sedlex.regexp? Plus (zs | cc)]
let char = [%sedlex.regexp? Compl '"']
let string = [%sedlex.regexp? '"', Star char, '"']
let float = [%sedlex.regexp? num, '.', num]

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
      | '/' -> FSLASH
      | '\\' -> BSLASH
      | '=' -> EQUALS
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
      | "if" -> IF
      | "then" -> THEN
      | "else" -> ELSE
      | "true" -> BOOL true
      | "false" -> BOOL false
      | string ->
          let str = Sedlexing.Utf8.lexeme buf in
          let str' = String.sub str 1 (String.length str - 2) in
          STRING str'
      | id -> ID (R (Sedlexing.Utf8.lexeme buf))
      | tid -> TYPEID (R (Sedlexing.Utf8.lexeme buf))
      | polyid -> POLYID (R (Sedlexing.Utf8.lexeme buf))
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
