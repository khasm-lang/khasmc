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
  | BINARY_OP
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
  let line' =
    match line.[String.length line - 1] with '\n' -> line | _ -> line ^ "\n"
  in
  let coff = repl " " (offsets.pos_bol - 1) ^ "^\n" in
  print_endline @@ "Error in file " ^ offsets.pos_fname ^ " line "
  ^ string_of_int offsets.pos_lnum;
  print_string line';
  print_string coff;
  print_endline @@ "Got " ^ show_token actual;
  print_endline @@ "Expected ["
  ^ delim ", " (List.map show_token follow_set)
  ^ "]"

module ParserState = struct
  type state = {
    lex_func : Lexing.lexbuf -> token;
    lex_buf : Lexing.lexbuf ref;
    buffer : token list ref;
    file : string;
  }
  [@@deriving show { with_path = false }]

  let print_state s = print_endline (show_state !s)

  let new_state lex_func lex_buf file =
    ref { lex_func; lex_buf = ref lex_buf; buffer = ref []; file }

  let error state actual follow_set =
    let offset = !(!state.lex_buf).lex_curr_p in
    let split = String.split_on_char '\n' !state.file in
    parse_error split offset actual follow_set;
    raise @@ ParseError

  let pop state =
    match !(!state.buffer) with
    | [] -> !state.lex_func !(!state.lex_buf)
    | x :: xs ->
        !state.buffer := xs;
        x

  let toss state =
    match !(!state.buffer) with
    | [] -> ignore (!state.lex_func !(!state.lex_buf))
    | _ :: xs ->
        !state.buffer := xs;
        ()

  let peek state int =
    let int = int - 1 in
    for i = 0 to int do
      let t = !state.lex_func !(!state.lex_buf) in
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

exception ParseExprHelper of kexpr

let rec nop () = ()

and get_ident state =
  match pop state with
  | T_IDENT s -> s
  | x -> error state x [ T_IDENT "example" ]

and get_ident_peek state =
  match peek state 1 with T_IDENT s -> Some s | _ -> None

and get_binop state =
  match pop state with
  | POW_OP s
  | MUL_OP s
  | DIV_OP s
  | MOD_OP s
  | ADD_OP s
  | SUB_OP s
  | COL_OP s
  | CAR_OP s
  | AT_OP s
  | EQ_OP s
  | LT_OP s
  | GT_OP s
  | PIP_OP s
  | AND_OP s
  | DOL_OP s ->
      s
  | x -> error state x [ BINARY_OP ]

and get_binop_peek state =
  match peek state 1 with
  | POW_OP s
  | MUL_OP s
  | DIV_OP s
  | MOD_OP s
  | ADD_OP s
  | SUB_OP s
  | COL_OP s
  | CAR_OP s
  | AT_OP s
  | EQ_OP s
  | LT_OP s
  | GT_OP s
  | PIP_OP s
  | AND_OP s
  | DOL_OP s ->
      Some s
  | _ -> None

and get_ident_or_binop state =
  match get_ident_peek state with
  | Some s ->
      toss state;
      s
  | None -> (
      match peek state 1 with
      | LPAREN ->
          toss state;
          let t = get_binop state in
          expect state RPAREN;
          t
      | x -> error state x [ LPAREN; T_IDENT "example module" ])

and parse_type_tuple state =
  let lhs = parse_type state in
  match pop state with
  | COMMA -> lhs :: parse_type_tuple state
  | RPAREN -> lhs :: []
  | x -> error state x [ COMMA; RPAREN ]

and parse_type_mulop state =
  let lhs = parse_type state in
  match pop state with
  | MUL_OP "*" -> lhs :: parse_type_tuple state
  | RPAREN -> lhs :: []
  | x -> error state x [ MUL_OP "*"; RPAREN ]

and parse_type_tuple_2 state =
  let lhs = parse_type state in
  match pop state with
  | COMMA -> lhs :: parse_type_tuple state
  | MUL_OP "*" -> lhs :: parse_type_mulop state
  | RPAREN -> lhs :: []
  | x -> error state x [ COMMA; RPAREN; MUL_OP "*" ]

and parse_type_helper state =
  let first =
    match pop state with
    | T_IDENT s -> if s = "()" then TSBottom else TSBase s
    | LPAREN -> (
        let lhs = parse_type state in
        match pop state with
        | RPAREN -> lhs
        | COMMA | MUL_OP "*" -> TSTuple (lhs :: parse_type_tuple_2 state)
        | x -> error state x [ RPAREN; COMMA; MUL_OP "*" ])
    | x -> error state x [ T_IDENT "example1"; LPAREN ]
  in
  match peek state 1 with
  | T_IDENT s ->
      toss state;
      TSApp (first, s)
  | _ -> first

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
      let idents = nonempty (id_list state) (T_IDENT "example2") state in
      expect state COMMA;
      let rec help idents =
        match idents with
        | [] -> raise @@ Impossible "parse_type"
        | [ x ] -> TSForall (x, parse_type state)
        | x :: xs -> TSForall (x, help xs)
      in
      help idents
  | _ -> parse_type_pratt state

and infix_bind_pow tok =
  let first = String.get tok 0 in
  let mab =
    if first = '*' then
      try if String.get tok 1 = '*' then (16, 17) else (0, 0) with _ -> (0, 0)
    else (0, 0)
  in
  if mab <> (0, 0) then mab
  else
    match first with
    | ';' -> (0, 1)
    | '$' -> (3, 2)
    | '=' | '>' | '<' -> (5, 4)
    | '|' | '&' -> (7, 6)
    | '@' | '^' -> (8, 9)
    | ':' -> (10, 11)
    | '+' | '-' -> (13, 12)
    | '*' | '/' | '%' -> (15, 14)
    (* | "**" -> (16, 17) *)
    | _ -> raise @@ Impossible "invalid char index bind pow"

and parse_tuple state =
  let b = parse_expr state in
  match pop state with
  | COMMA -> b :: parse_tuple state
  | RPAREN -> b :: []
  | x -> error state x [ COMMA; RPAREN ]

and parse_funccall b state =
  let b2 = parse_base state in
  let f = FCall (mkinfo (), b, b2) in
  parse_funccall_try f state

and parse_funccall_try b state =
  match peek state 1 with
  | LPAREN | T_IDENT _ | T_INT _ | T_FLOAT _ | T_STRING _ ->
      parse_funccall b state
  | _ -> b

and parse_base state =
  match pop state with
  | BANG_OP s | TILDE_OP s ->
      FCall (mkinfo (), Base (mkinfo (), Ident (mkinfo (), s)), parse_base state)
  | LPAREN -> (
      match peek state 1 with
      | RPAREN -> Base (mkinfo (), Ident (mkinfo (), "()"))
      | _ -> (
          let e' = parse_expr state in
          match pop state with
          | COMMA -> Base (mkinfo (), Tuple (e' :: parse_tuple state))
          | RPAREN -> e'
          | x -> error state x [ COMMA; RPAREN ]))
  | T_IDENT s -> (
      match peek state 1 with
      | DOT -> (
          let rec helper state =
            let id = get_ident_or_binop state in
            match peek state 1 with
            | DOT ->
                toss state;
                id :: helper state
            | _ -> [ id ]
          in
          toss state;
          let l = s :: helper state in
          let rev = List.rev l in
          match rev with
          | x :: xs -> ModAccess (mkinfo (), List.rev xs, x)
          | [] -> raise @@ Impossible "parse_base")
      | _ -> Base (mkinfo (), Ident (mkinfo (), s)))
  | T_INT s -> Base (mkinfo (), Int s)
  | T_FLOAT s -> Base (mkinfo (), Float s)
  | T_STRING s -> Base (mkinfo (), Str s)
  | TRUE -> Base (mkinfo (), True)
  | FALSE -> Base (mkinfo (), False)
  | x ->
      error state x
        [
          BANG_OP "!";
          TILDE_OP "~";
          LPAREN;
          T_IDENT "example";
          T_INT "10";
          T_FLOAT "1.0";
          TRUE;
          FALSE;
          IF;
          LET;
        ]

and parse_compound state =
  match peek state 1 with
  | IF ->
      toss state;
      let cond = parse_expr state in
      expect state THEN;
      let e1 = parse_expr state in
      expect state ELSE;
      let e2 = parse_expr state in
      IfElse (mkinfo (), cond, e1, e2)
  | LET ->
      toss state;
      let var = get_ident state in
      (match pop state with
      | EQ_OP "=" -> ()
      | x -> error state x [ EQ_OP "=" ]);
      let first = parse_expr state in
      expect state IN;
      let second = parse_expr state in
      LetIn (mkinfo (), var, first, second)
  | _ -> (
      let b = parse_base state in
      match peek state 1 with
      | DOT -> (
          toss state;
          match pop state with
          | T_INT s -> TupAccess (mkinfo (), b, int_of_string s)
          | x -> error state x [ T_INT "1" ])
      | _ -> parse_funccall_try b state)

and parse_expr_h state res prec =
  let op = get_binop_peek state in
  match op with
  | None -> res
  | Some s ->
      let pl, pr = infix_bind_pow s in
      if pl >= prec then (
        toss state;
        let next_prec = pr in
        parse_expr_h state
          (FCall
             ( mkinfo (),
               FCall (mkinfo (), Base (mkinfo (), Ident (mkinfo (), s)), res),
               parse_expr state ))
          next_prec)
      else res

and parse_expr state =
  let lhs = parse_compound state in
  parse_expr_h state lhs 0

and parse_let state =
  let names = id_list state in
  let id, args =
    match names with
    | x :: xs -> (x, xs)
    | [] -> error state (pop state) [ T_IDENT "example" ]
  in
  let ts =
    match pop state with
    | COL_OP ":" -> parse_type state
    | x -> error state x [ COL_OP ":" ]
  in
  let expr =
    match pop state with
    | EQ_OP "=" -> parse_expr state
    | x -> error state x [ EQ_OP "=" ]
  in
  ((id, ts), (id, args, expr))

and parse_let_norm state =
  match peek state 1 with
  | REC ->
      toss state;
      let (id, ts), (_id, args, expr) = parse_let state in
      TopAssignRec ((id, ts), (id, args, expr))
  | _ ->
      let (id, ts), (_id, args, expr) = parse_let state in
      TopAssign ((id, ts), (id, args, expr))

and parse_module state =
  let name =
    match pop state with
    | T_IDENT s -> s
    | x -> error state x [ T_IDENT "example" ]
  in
  (match pop state with EQ_OP "=" -> () | x -> error state x [ EQ_OP "=" ]);
  (match pop state with STRUCT -> () | x -> error state x [ STRUCT ]);
  let top = parse_toplevel_list state in
  expect state END;
  SimplModule (name, top)

and parse_bind state =
  expect state LPAREN;
  let op = get_binop state in
  expect state RPAREN;
  (match pop state with EQ_OP "=" -> () | x -> error state x [ EQ_OP "=" ]);
  let name = get_ident state in
  Bind (op, [], name)

and parse_extern state =
  let nm = get_ident state in
  (match pop state with EQ_OP "=" -> () | x -> error state x [ EQ_OP "=" ]);
  let ts = parse_type state in
  Extern (nm, ts)

and parse_intextern state =
  let nm =
    match pop state with INTIDENT s -> s | x -> error state x [ EQ_OP "=" ]
  in
  (match pop state with EQ_OP "=" -> () | x -> error state x [ EQ_OP "=" ]);
  let id = get_ident state in
  (match pop state with COL_OP ":" -> () | x -> error state x [ COL_OP ":" ]);
  let ts = parse_type state in
  IntExtern (nm, id, ts)

and parse_toplevel_list state =
  let first =
    match peek state 1 with
    | LET ->
        toss state;
        Some (parse_let_norm state)
    | MODULE ->
        toss state;
        Some (parse_module state)
    | BIND ->
        toss state;
        Some (parse_bind state)
    | EXTERN ->
        toss state;
        Some (parse_extern state)
    | INTEXTERN ->
        toss state;
        Some (parse_intextern state)
    | EOF -> None
    | _ -> None
  in
  match first with Some x -> x :: parse_toplevel_list state | None -> []

and program token lexbuf file =
  let token' buf =
    let t' = token buf in
    print_endline (show_token t');
    t'
  in
  let state = new_state token lexbuf file in
  let tmp = parse_toplevel_list state in
  if tmp = [] then raise ParseError else Program tmp

let test token lexbuf file =
  let token' buf =
    let t' = token buf in
    print_endline (show_token t');
    t'
  in
  let state = new_state token' lexbuf file in
  print_endline (show_kexpr @@ parse_expr state)
