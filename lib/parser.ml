type token =
  | T_IDENT of string
  | T_FIDENT of string
  | T_INT of string
  | T_FLOAT of string
  | T_STRING of string
  | INTIDENT of string
  | INTEXTERN
  | TRUE
  | FALSE
  | ADD
  | SUB
  | MUL
  | SLASH
  | BSLASH
  | STRAIGHT
  | AND
  | DOT
  | PERCENT
  | AT
  | HASH
  | GT
  | LT
  | COMMA
  | BANG
  | EQ
  | DOLLAR
  | QMARK
  | IF
  | OF
  | THEN
  | ELSE
  | WHILE
  | FOR
  | RETURN
  | IN
  | LET
  | REC
  | COLON
  | SEMICOLON
  | EOF
  | TS_TO
  | LAM_TO
  | IGNORE
  | FORALL
  | SIG
  | TILDE
  | FUN
  | TFUN
  | END
  | BANG_OP of string
  | TILDE_OP of string
  | POW_OP of string
  | MUL_OP of string
  | DIV_OP of string
  | MOD_OP of string
  | ADD_OP of string
  | SUB_OP of string
  | COL_OP of string
  | CAR_OP of string
  | AT_OP of string
  | EQ_OP of string
  | LT_OP of string
  | GT_OP of string
  | PIP_OP of string
  | AND_OP of string
  | DOL_OP of string
  | MODULE
  | STRUCT
  | FUNCTOR
  | BIND
  | LAND
  | LOR
  | NOMANGLE
  | INLINE
  | EXTERN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | LPAREN
  | RPAREN
  | EMPTY
  | ANY
[@@deriving show { with_path = false }]

exception ParseError

open Ast

module Lexing = struct
  include Lexing

  let pp_lexbuf _l _r = ()
end

let rec repl s i = match i with 0 -> "" | x -> s ^ repl s (x - 1)

let rec delim s sl =
  match sl with [] -> "" | [ x ] -> x | x :: xs -> x ^ s ^ delim s xs

let parse_error lines (offsets : Lexing.position) actual follow_set =
  let line = List.nth lines (offsets.pos_lnum - 1) in
  let coff = repl " " offsets.pos_bol ^ "^" in
  print_endline @@ "Error in file " ^ offsets.pos_fname ^ " line "
  ^ string_of_int offsets.pos_lnum;
  print_endline line;
  print_endline coff;
  print_endline @@ "Got " ^ show_token actual;
  print_endline @@ "Expected ["
  ^ delim ", " (List.map show_token follow_set)
  ^ "]"

module ParserState = struct
  type state = {
    lex_func : Lexing.lexbuf -> token;
    lex_buf : Lexing.lexbuf;
    buffer : token list ref;
    file : string;
  }
  [@@deriving show { with_path = false }]

  let print_state s = print_endline (show_state !s)

  let new_state lex_func lex_buf file =
    ref { lex_func; lex_buf; buffer = ref []; file }

  let error state actual follow_set =
    let offset = !state.lex_buf.lex_curr_p in
    let split = String.split_on_char '\n' !state.file in
    parse_error split offset actual follow_set;
    exit 1

  let pop state =
    match !(!state.buffer) with
    | [] -> !state.lex_func !state.lex_buf
    | x :: xs ->
        !state.buffer := xs;
        x

  let toss state =
    match !(!state.buffer) with
    | [] -> ignore (!state.lex_func !state.lex_buf)
    | _ :: xs ->
        !state.buffer := xs;
        ()

  let peek state int =
    let int = int - 1 in
    for i = 0 to int do
      let t = !state.lex_func !state.lex_buf in
      !state.buffer := !(!state.buffer) @ [ t ]
    done;
    try List.nth !(!state.buffer) int
    with Failure _ -> error state EOF [ ANY ]

  let expect state tok =
    let t = pop state in
    if t == tok then () else error state t [ tok ]
end

let print_token t = print_endline (show_token t)

open ParserState
open Exp

let rec id_list state =
  match peek state 1 with
  | T_IDENT s ->
      toss state;
      s :: id_list state
  | _ -> []

let nonempty l exp state =
  match l with [] -> error state EMPTY [ exp ] | _ -> l

let rec nop () = ()

and parse_type_tuple state =
  let lhs = parse_type state in
  match pop state with
  | COMMA -> lhs :: parse_type_tuple state
  | RPAREN -> lhs :: []
  | x -> error state x [ COMMA; RPAREN ]

and parse_type_helper state =
  match pop state with
  | T_IDENT s -> if s = "()" then TSBottom else TSBase s
  | LPAREN -> (
      let lhs = parse_type state in
      match pop state with
      | RPAREN -> lhs
      | COMMA -> TSTuple (lhs :: parse_type_tuple state)
      | x -> error state x [ RPAREN; COMMA ])
  | x -> error state x [ T_IDENT "example"; LPAREN ]

and parse_type_pratt state =
  let lhs = parse_type_helper state in
  match peek state 1 with
  | TS_TO ->
      toss state;
      let rhs = parse_type_pratt state in
      TSMap (lhs, rhs)
  | _ -> lhs

and parse_type state =
  match peek state 1 with
  | FORALL ->
      toss state;
      let idents = nonempty (id_list state) (T_IDENT "example") state in
      expect state COMMA;
      let rec help idents =
        match idents with
        | [] -> raise @@ Impossible "parse_type"
        | [ x ] -> TSForall (x, parse_type state)
        | x :: xs -> TSForall (x, help xs)
      in
      help idents
  | _ -> parse_type_pratt state

and parse_toplevel_list state =
  let tok = pop state in
  []

and program token lexbuf file =
  let state = new_state token lexbuf file in
  Program (parse_toplevel_list state)

let test token lexbuf file =
  let state = new_state token lexbuf file in
  print_endline (pshow_typesig (parse_type state))
