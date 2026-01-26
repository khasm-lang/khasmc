open Ast

type t_TOKEN =
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
  | FATARROW
  | TYINT
  | TYSTRING
  | TYCHAR
  | TYFLOAT
  | TYBOOL
  | IMPL
  | MODULE
  | END
  | MATCH
  | WITH
  | FUN
  | IF
  | THEN
  | ELSE
  | OF
  | BOOL of bool
  | STRING of string
  | ID of string
  | TYPEID of string
  | POLYID of string
  | INT of string
  | FLOAT of string
  | DONE
  | OTHER of string
[@@deriving show { with_path = false }]

let digit = [%sedlex.regexp? '0' .. '9']
let idchar = [%sedlex.regexp? Star (ll | lu | digit)]
let path = [%sedlex.regexp? Star ((lu, idchar), '.')]
let id = [%sedlex.regexp? path, ll, idchar]
let type_id = [%sedlex.regexp? path, lu, idchar]
let poly_id = [%sedlex.regexp? '\'', ll, idchar]
let num = [%sedlex.regexp? Plus digit]
let space = [%sedlex.regexp? Plus (zs | cc)]
let char = [%sedlex.regexp? Compl '"']
let string = [%sedlex.regexp? '"', Star char, '"']
let float = [%sedlex.regexp? num, '.', num]

let rec lexer_ buf : (t_TOKEN, exn) Result.t =
  match
    begin match%sedlex buf with space -> () | _ -> ()
    end;
    begin match%sedlex buf with
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
    | "=>" -> FATARROW
    | "Int" -> TYINT
    | "String" -> TYSTRING
    | "Char" -> TYCHAR
    | "Float64" -> TYFLOAT
    | "Bool" -> TYBOOL
    | "impl" -> IMPL
    | "module" -> MODULE
    | "end" -> END
    | "match" -> MATCH
    | "with" -> WITH
    | "fun" -> FUN
    | "if" -> IF
    | "then" -> THEN
    | "else" -> ELSE
    | "of" -> OF
    | "true" -> BOOL true
    | "false" -> BOOL false
    | string ->
        let str = Sedlexing.Utf8.lexeme buf in
        let str' = String.sub str 1 (String.length str - 2) in
        STRING str'
    | id -> ID (Sedlexing.Utf8.lexeme buf)
    | type_id -> TYPEID (Sedlexing.Utf8.lexeme buf)
    | poly_id -> POLYID (Sedlexing.Utf8.lexeme buf)
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
