
(* The type of tokens. *)

type token = 
  | WHILE
  | T_STRING of (string)
  | T_INT of (string)
  | T_IDENT of (string)
  | T_FLOAT of (string)
  | TS_TO
  | TRUE
  | TILDE_OP of (string)
  | TILDE
  | THEN
  | SUB_OP of (string)
  | SUB
  | STRAIGHT
  | SLASH
  | SIG
  | SEMICOLON
  | RPAREN
  | RETURN
  | RBRACK
  | RBRACE
  | QMARK
  | POW_OP of (string)
  | PIP_OP of (string)
  | PERCENT
  | NOMANGLE
  | MUL_OP of (string)
  | MUL
  | MOD_OP of (string)
  | LT_OP of (string)
  | LT
  | LPAREN
  | LOR
  | LET
  | LBRACK
  | LBRACE
  | LAND
  | INLINE
  | IN
  | IGNORE
  | IF
  | HASH
  | GT_OP of (string)
  | GT
  | FORALL
  | FOR
  | FALSE
  | EQ_OP of (string)
  | EQ
  | EOF
  | ELSE
  | DOT
  | DOL_OP of (string)
  | DOLLAR
  | DIV_OP of (string)
  | COMMA
  | COL_OP of (string)
  | COLON
  | CAR_OP of (string)
  | BSLASH
  | BANG_OP of (string)
  | BANG
  | AT_OP of (string)
  | AT
  | AND_OP of (string)
  | AND
  | ADD_OP of (string)
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error of int

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program)
