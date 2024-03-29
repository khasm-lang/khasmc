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
  | TICK
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
  | OPEN
  | BIND
  | TYPE
  | LAND
  | LOR
  | NOMANGLE
  | MATCH
  | WITH
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
  | UNDERSCORE
[@@deriving show { with_path = false }]

exception ParseError

open Ast
open ListHelpers

module Lexing = struct
  include Lexing

  let pp_lexbuf _l _r = ()
  let pp_position _l _r = ()
end

let rec repl s i = match i with 0 -> "" | x -> s ^ repl s (x - 1)

let rec delim s sl =
  match sl with [] -> "" | [ x ] -> x | x :: xs -> x ^ s ^ delim s xs

let parse_error lines (offsets : Lexing.position) actual follow_set =
  try
    let line = List.nth lines (offsets.pos_lnum - 1) in
    let line' =
      match line.[String.length line - 1] with '\n' -> line | _ -> line ^ "\n"
    in
    let coff = repl " " (offsets.pos_bol - 1) ^ "^\n" in
    print_endline
    @@ "Error in file "
    ^ offsets.pos_fname
    ^ " line "
    ^ string_of_int offsets.pos_lnum;
    print_string line';
    print_string coff;
    print_endline @@ "Got " ^ show_token actual;
    print_endline
    @@ "Expected ["
    ^ delim ", " (List.map show_token follow_set)
    ^ "]"
  with Invalid_argument _ | Failure _ ->
    print_endline
    @@ "Error in file "
    ^ offsets.pos_fname
    ^ " line "
    ^ string_of_int offsets.pos_lnum;
    print_endline @@ "Got " ^ show_token actual;
    print_endline
    @@ "Expected ["
    ^ delim ", " (List.map show_token follow_set)
    ^ "]"

let print_token t = print_endline (show_token t)

module ParserState = struct
  let pp t =
    let t' = t in
    t'

  type state = {
    lex_func : Lexing.lexbuf -> token;
    lex_buf : Lexing.lexbuf ref;
    buffer : (token * Lexing.position) list ref;
    file : string;
  }
  [@@deriving show { with_path = false }]

  let print_state s = print_endline (show_state !s)

  let new_state lex_func lex_buf file =
    ref { lex_func; lex_buf = ref lex_buf; buffer = ref []; file }

  let rec last k =
    match k with [] -> failwith "empty" | [ x ] -> x | _ :: xs -> last xs

  let error state actual follow_set =
    let offset =
      match !(!state.buffer) with
      | [] -> !(!state.lex_buf).lex_curr_p
      | x :: _ -> snd x
    in
    let split = String.split_on_char '\n' !state.file in
    parse_error split offset actual follow_set;
    raise @@ ParseError

  let pop state =
    match !(!state.buffer) with
    | [] -> !state.lex_func !(!state.lex_buf)
    | x :: xs ->
        !state.buffer := xs;
        pp @@ fst x

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
      !state.buffer := !(!state.buffer) @ [ (t, !(!state.lex_buf).lex_curr_p) ]
    done;
    try
      let ret = fst (List.nth !(!state.buffer) int) in
      pp ret
    with Failure _ -> error state EOF [ ANY ]

  let expect state tok =
    let t = pop state in
    if t = tok then
      ()
    else
      error state t [ tok ]
end

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

let rec repeat_until f x =
  match f x with None -> [] | Some t -> t :: repeat_until f x

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
  (* | COL_OP s *)
  | CAR_OP s
  | AT_OP s
  | EQ_OP s
  | LT_OP s
  | GT_OP s
  | AND_OP s
  | DOL_OP s ->
      s
  | PIP_OP s when String.length s > 1 -> s
  | x -> error state x [ BINARY_OP ]

and get_binop_peek state =
  match peek state 1 with
  | POW_OP s
  | MUL_OP s
  | DIV_OP s
  | MOD_OP s
  | ADD_OP s
  | SUB_OP s
  (* | COL_OP s *)
  | CAR_OP s
  | AT_OP s
  | EQ_OP s
  | LT_OP s
  | GT_OP s
  | AND_OP s
  | DOL_OP s ->
      Some s
  | PIP_OP s when String.length s > 1 -> Some s
  | SEMICOLON -> Some ";"
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

and parse_type_compound state =
  match parse_type_tuple state with
  | [] -> impossible "empty type"
  | [ x ] -> x
  | x -> TSTuple x

and parse_single state =
  match peek state 1 with
  | T_IDENT t ->
      toss state;
      Some (TSBase t)
  | LPAREN -> (
      toss state;
      match peek state 1 with
      | RPAREN ->
          toss state;
          Some (TSTuple [])
      | _ -> Some (parse_type_compound state))
  | _ -> None

and parse_type_helper state =
  match peek state 1 with
  | T_IDENT t -> (
      toss state;
      let list = repeat_until parse_single state in
      match list with [] -> TSBase t | x -> TSApp (x, t))
  | LPAREN -> (
      toss state;
      match peek state 1 with
      | RPAREN ->
          toss state;
          TSTuple []
      | _ -> (
          match parse_type_tuple state with
          | [] -> impossible "empty type"
          | [ x ] -> x
          | x -> TSTuple x))
  | x -> error state x [ T_IDENT "example"; LPAREN ]

and parse_type_helper_maybe_noapp state =
  match peek state 1 with
  | T_IDENT t ->
      toss state;
      Some (TSBase t)
  | LPAREN -> (
      toss state;
      match peek state 1 with
      | RPAREN ->
          toss state;
          Some (TSTuple [])
      | _ ->
          Some
            (match parse_type_tuple state with
            | [] -> impossible "empty type"
            | [ x ] -> x
            | x -> TSTuple x))
  | _ -> None

and parse_type_helper_maybe state =
  match peek state 1 with
  | T_IDENT t -> (
      toss state;
      let list = repeat_until parse_single state in
      match list with [] -> Some (TSBase t) | x -> Some (TSApp (x, t)))
  | LPAREN -> (
      toss state;
      match peek state 1 with
      | RPAREN ->
          toss state;
          Some (TSTuple [])
      | _ ->
          Some
            (match parse_type_tuple state with
            | [] -> impossible "empty type"
            | [ x ] -> x
            | x -> TSTuple x))
  | _ -> None

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
  | LT_OP "<" ->
      toss state;
      let idents = nonempty (id_list state) (T_IDENT "example2") state in
      expect state (GT_OP ">");
      let rec help idents =
        match idents with
        | [] -> raise @@ Impossible "parse_type"
        | [ x ] -> TSForall (x, parse_type state)
        | x :: xs -> TSForall (x, help xs)
      in
      help idents
  | _ ->
      let ret = parse_type_pratt state in
      ret

and last_tail l acc =
  match l with
  | [] -> raise @@ Impossible "last_tale empty"
  | [ x ] -> (x, List.rev acc)
  | x :: xs -> last_tail xs (x :: acc)

and parse_match_pattern_tup state =
  let rec go acc =
    let tmp = parse_match_pattern state in
    match pop state with
    | RPAREN -> List.rev (tmp :: acc)
    | COMMA -> go (tmp :: acc)
    | x -> error state x [ RPAREN; COMMA ]
  in
  go []

and parse_match_list state =
  match peek state 1 with
  | TICK ->
      toss state;
      let id = get_ident state in
      MPApp (id, []) :: parse_match_list state
  | T_IDENT t ->
      toss state;
      MPId t :: parse_match_list state
  | UNDERSCORE ->
      toss state;
      MPWild :: parse_match_list state
  | LPAREN -> (
      toss state;
      match peek state 1 with
      | RPAREN ->
          toss state;
          []
      | _ ->
          let tmp = parse_match_pattern_tup state in
          let tmp = match tmp with [ x ] -> x | _ -> MPTup tmp in
          tmp :: parse_match_list state)
  | _ -> []

and parse_match_pattern state =
  match pop state with
  | TICK ->
      let t = get_ident state in
      MPApp (t, [])
  | T_IDENT t -> (
      match parse_match_list state with [] -> MPId t | xs -> MPApp (t, xs))
  | UNDERSCORE -> MPWild
  | LPAREN -> (
      match peek state 1 with
      | RPAREN ->
          toss state;
          MPTup []
      | _ -> (
          match parse_match_pattern_tup state with
          | [] -> raise @@ Impossible "parse_match_pattern"
          | [ x ] -> x
          | x -> MPTup x))
  | T_INT t -> MPInt t
  | x -> error state x [ T_IDENT "MatchExample"; LPAREN; T_INT "0" ]

and parse_adt_pattern state =
  let id = get_ident state in
  match peek state 1 with
  | COL_OP ":" ->
      (* GADT *)
      toss state;
      let rec helper state =
        match parse_type_helper_maybe state with
        | Some x -> (
            match peek state 1 with
            | TS_TO ->
                toss state;
                x :: helper state
            | _ -> [ x ])
        | None -> []
      in
      let all = helper state in
      let final, args = last_tail all [] in
      { head = id; args; typ = Ok final }
  | _ ->
      (* ADT *)
      let all = repeat_until parse_type_helper_maybe_noapp state in
      { head = id; args = all; typ = Error () }

and infix_bind_pow tok =
  let first = String.get tok 0 in
  let mab =
    if first = '*' then
      try
        if String.get tok 1 = '*' then
          (16, 17)
        else
          (0, 0)
      with _ -> (0, 0)
    else
      (0, 0)
  in
  if mab <> (0, 0) then
    mab
  else
    match first with
    | ';' -> (1, 0)
    | '$' -> (2, 3)
    | '=' | '>' | '<' -> (4, 5)
    | '|' | '&' -> (6, 7)
    | '@' | '^' -> (9, 8)
    | ':' -> (11, 10)
    | '+' | '-' -> (12, 13)
    | '*' | '/' | '%' -> (14, 15)
    (* | "**" -> (16, 17) *)
    | _ -> raise @@ Impossible "invalid char index bind pow"

and parse_tuple state =
  let b = parse_expr state 0 in
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
  | LPAREN | T_IDENT _ | T_INT _ | T_FLOAT _ | T_STRING _ | TRUE | FALSE ->
      parse_funccall b state
  | _ -> b

and parse_base state =
  match pop state with
  | BANG_OP s | TILDE_OP s ->
      FCall (mkinfo (), Base (mkinfo (), Ident (mkinfo (), s)), parse_base state)
  | LPAREN -> (
      match peek state 1 with
      | RPAREN ->
          toss state;
          Base (mkinfo (), Tuple [])
      | _ -> (
          let e' = parse_expr state 0 in
          match pop state with
          | COMMA -> Base (mkinfo (), Tuple (e' :: parse_tuple state))
          | RPAREN -> e'
          | x -> error state x [ COMMA; RPAREN ]))
  | T_IDENT s -> (
      match (peek state 1, peek state 2) with
      | DOT, T_INT _ -> Base (mkinfo (), Ident (mkinfo (), s))
      | DOT, _ -> (
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
      let cond = parse_expr state 0 in
      expect state THEN;
      let e1 = parse_expr state 0 in
      expect state ELSE;
      let e2 = parse_expr state 0 in
      IfElse (mkinfo (), cond, e1, e2)
  | LET -> (
      toss state;
      match pop state with
      | T_IDENT var -> (
          match pop state with
          | EQ_OP "=" ->
              let first = parse_expr state 0 in
              expect state IN;
              let second = parse_expr state 0 in
              LetIn (mkinfo (), var, first, second)
          | COL_OP ":" ->
              let ts = parse_type state in
              expect state (EQ_OP "=");
              let first = parse_expr state 0 in
              expect state IN;
              let second = parse_expr state 0 in
              AnnotLet (mkinfo (), var, ts, first, second)
          | x -> error state x [ EQ_OP "="; COL_OP ":" ])
      | REC ->
          let var = get_ident state in
          expect state (COL_OP ":");
          let ts = parse_type state in
          expect state (EQ_OP "=");
          let first = parse_expr state 0 in
          expect state IN;
          let second = parse_expr state 0 in
          LetRecIn (mkinfo (), ts, var, first, second)
      | x -> error state x [ REC; EQ_OP "=" ])
  | FUN ->
      toss state;
      let v = get_ident state in
      (match pop state with
      | COL_OP ":" -> ()
      | x -> error state x [ COL_OP ":" ]);
      let typ = parse_type state in
      (match pop state with LAM_TO -> () | x -> error state x [ LAM_TO ]);
      let expr = parse_expr state 0 in
      Ast.AnnotLam (mkinfo (), v, typ, expr)
  | TFUN ->
      toss state;
      let v = get_ident state in
      (match pop state with LAM_TO -> () | x -> error state x [ COL_OP ":" ]);
      let expr = parse_expr state 0 in
      Ast.TypeLam (mkinfo (), v, expr)
  | MATCH ->
      toss state;
      let pat = parse_expr state 0 in
      expect state WITH;
      let rec go acc =
        match pop state with
        | PIP_OP "|" ->
            let p = parse_match_pattern state in
            expect state LAM_TO;
            let e = parse_expr state 0 in
            go ((p, e) :: acc)
        | END -> List.rev acc
        | x -> error state x [ PIP_OP "|"; END ]
      in
      Match (mkinfo (), pat, go [])
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
  | Some ";" ->
      let pl, pr = infix_bind_pow ";" in
      if pl >= prec then (
        toss state;
        let next_prec = pr in
        parse_expr_h state (Join (mkinfo (), res, parse_expr state next_prec)) 0)
      else
        res
  | Some s ->
      let pl, pr = infix_bind_pow s in
      if pl >= prec then (
        toss state;
        let next_prec = pr in
        parse_expr_h state
          (FCall
             ( mkinfo (),
               FCall (mkinfo (), Base (mkinfo (), Ident (mkinfo (), s)), res),
               parse_expr state next_prec ))
          0)
      else
        res

and parse_expr state prec =
  let lhs = parse_compound state in
  parse_expr_h state lhs prec

and parse_let state =
  let names = id_list state in
  let id, args =
    match names with
    | x :: xs -> (x, xs)
    | [] -> error state (pop state) [ T_IDENT "example" ]
  in
  let expr =
    match pop state with
    | EQ_OP "=" -> parse_expr state 0
    | x -> error state x [ EQ_OP "=" ]
  in
  (id, args, expr)

and parse_let_norm state =
  let ts = parse_type state in
  expect state LET;
  match peek state 1 with
  | REC ->
      toss state;
      let id, args, expr = parse_let state in
      TopAssignRec ((id, ts), (id, args, expr))
  | _ ->
      let id, args, expr = parse_let state in
      TopAssign ((id, ts), (id, args, expr))

and parse_module state =
  let name =
    match pop state with
    | T_IDENT s -> s
    | x -> error state x [ T_IDENT "example" ]
  in
  (match pop state with EQ_OP "=" -> () | x -> error state x [ EQ_OP "=" ]);
  (match peek state 1 with
  | MODULE ->
      toss state;
      ()
  | _ -> ());
  let top = parse_toplevel_list state in
  expect state END;
  SimplModule (name, top)

and parse_bind state =
  let op = get_binop state in
  (match pop state with EQ_OP "=" -> () | x -> error state x [ EQ_OP "=" ]);
  let name = get_ident state in
  Bind (op, [], name)

and parse_extern state =
  let arity =
    match pop state with
    | T_INT s -> int_of_string s
    | x -> error state x [ T_INT "1" ]
  in
  let nm =
    match pop state with
    | INTIDENT s -> s
    | x -> error state x [ INTIDENT "`foo" ]
  in
  expect state (COL_OP ":");
  let ts = parse_type state in
  expect state (EQ_OP "=");
  let id = get_ident state in
  IntExtern (nm, id, arity, ts)

and parse_open state =
  let s = get_ident state in
  Open s

and parse_typedecl_pats nm args state =
  let p = parse_adt_pattern state in
  let fixed =
    match p.typ with
    | Ok _ -> p
    | Error () ->
        let rec go xs =
          match xs with [] -> [] | x :: xs -> TSBase x :: go xs
        in
        { p with typ = Ok (TSApp (go args, nm)) }
  in
  match peek state 1 with
  | PIP_OP "|" ->
      toss state;
      fixed :: parse_typedecl_pats nm args state
  | _ -> [ fixed ]

and parse_typedecl state =
  match id_list state with
  | [] -> error state (peek state 1) [ T_IDENT "Type_example" ]
  | x :: xs -> (
      match pop state with
      | EQ_OP "=" -> (
          match peek state 1 with
          | PIP_OP "|" ->
              toss state;
              let pats = parse_typedecl_pats x xs state in
              Typedecl (x, xs, pats)
          | _ ->
              let typ = parse_type state in
              Typealias (x, xs, typ))
      | x -> error state x [ EQ_OP "=" ])

and parse_toplevel_list state =
  let first =
    match peek state 1 with
    | SIG ->
        toss state;
        Some (parse_let_norm state)
    | MODULE ->
        toss state;
        Some (parse_module state)
    | BIND ->
        toss state;
        Some (parse_bind state)
    | INTEXTERN ->
        toss state;
        Some (parse_extern state)
    | OPEN ->
        toss state;
        Some (parse_open state)
    | TYPE ->
        toss state;
        Some (parse_typedecl state)
    | EOF -> None
    | x -> None
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
  expect state EOF;
  Program tmp
