
(* The type of tokens. *)

type token = 
  | T_STRING of (string)
  | T_INT of (string)
  | T_IDENT of (string)
  | T_FLOAT of (string)
  | TS_TO
  | TRUE
  | SUB
  | STRAIGHT
  | SLASH
  | SEMICOLON
  | RPAREN
  | RBRACK
  | RBRACE
  | QMARK
  | PERCENT
  | NOMANGLE
  | MUL
  | LT
  | LPAREN
  | LBRACK
  | LBRACE
  | KW_WHILE
  | KW_RETURN
  | KW_IF
  | KW_FOR
  | INLINE
  | IGNORE
  | HASH
  | GT
  | FALSE
  | EQ
  | EOF
  | DOT
  | COMMA
  | COLON
  | BSLASH
  | BANG
  | AT
  | AND
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error of int

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program)
