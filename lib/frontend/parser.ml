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
  | BAD
[@@deriving show { with_path = false }]

open Ast
open Errors

module Lexing = struct
  include Lexing

  let pp_lexbuf _l _r = ()
  let pp_position _l _r = ()
end

module BatVect = struct
  include BatVect

  let pp _l _r _f = ()
end

type tok = {
  token : token;
  span : span;
}
[@@deriving show { with_path = false }]

let maketok t span = { token = t; span }

module Tok = struct
  type core = {
    mutable tokens : tok BatVect.t;
    mutable peek_num : int;
    mutable index : int;
    lexer : Lexing.lexbuf -> Lexing.position * token;
    buf : Lexing.lexbuf;
    file : string; [@printer fun fmt _t -> fprintf fmt "<src file>"]
  }
  [@@deriving show { with_path = false }]

  let new_core lexfunc lexbuf file =
    ref
      {
        tokens = BatVect.empty;
        peek_num = 0;
        index = 0;
        lexer = lexfunc;
        buf = lexbuf;
        file;
      }

  let get_token core =
    let before, token = !core.lexer !core.buf in
    let after = !core.buf.lex_curr_p in
    maketok token (Errors.lexbuf_to_span !core.file before after)

  let peek core =
    let len = ref @@ BatVect.length !core.tokens in
    while !core.peek_num + !core.index >= !len do
      len := BatVect.length !core.tokens;
      !core.tokens <- BatVect.append (get_token core) !core.tokens
    done;
    !core.peek_num <- !core.peek_num + 1;
    BatVect.get !core.tokens (!core.peek_num + !core.index - 1)

  let pop core =
    let len = ref @@ BatVect.length !core.tokens in
    while !core.index >= !len do
      len := BatVect.length !core.tokens;
      !core.tokens <- BatVect.append (get_token core) !core.tokens
    done;
    !core.index <- !core.index + 1;
    !core.peek_num <- 0;
    BatVect.get !core.tokens (!core.index - 1)

  let reset_peek core = !core.peek_num <- 0

  let toss core =
    !core.index <- !core.index + !core.peek_num;
    reset_peek core

  let current core = BatVect.get !core.tokens (!core.index + !core.peek_num - 1)

  let mkspan core oldbuf =
    let { token = _; span } = current core in
    Errors.spandiff oldbuf span
end

open Tok
open Exp

let error core span tok =
  let ctx = Errors.from_file (ref !core.file) in
  Errors.add ctx span (Printf.sprintf "Expected token %s" (show_token tok));
  raise @@ SyntaxErr (Errors.to_string ctx)

let errorexpect core span got expected =
  let ctx = Errors.from_file (ref !core.file) in
  Errors.add ctx span
    (Printf.sprintf "Got: %s, expected one of: %s" (show_token got)
       (String.concat ", " (List.map show_token expected)));
  raise @@ SyntaxErr (Errors.to_string ctx)

type tokwrapper = T of token * span

let untok x =
  let { span; token } = x in
  T (token, span)

let retok x =
  let (T (a, b)) = x in
  { span = b; token = a }

let pop' core = untok @@ pop core
let toss' core = toss core
let peek' core = untok @@ peek core

let currentspan core =
  let { span; token } = current core in
  span

let expect core t =
  match pop' core with
  | T (t', _) when t' = t -> ()
  | T (t, s) -> error core s t

let mergespans l =
  let a, b = List.split l in
  let b' = Errors.merge b in
  (a, b')

let rec dummy () = dummy ()

and parse_ident core =
  match pop' core with
  | T (T_IDENT i, span) -> (i, span)
  | T (_t, span) -> error core span (T_IDENT "Example")

and parse_peek_ident core =
  match peek' core with
  | T (T_IDENT i, span) ->
      toss core;
      Some (i, span)
  | T (_t, _span) -> None

and parse_peek_ident_list core =
  match parse_peek_ident core with
  | Some (i, s) -> (i, s) :: parse_peek_ident_list core
  | None -> []

and parse_tuple_helper core =
  let t = parse_typesig core in
  match peek' core with
  | T (RPAREN, _rspan) ->
      toss core;
      [ t ]
  | T (COMMA, _cspan) ->
      toss core;
      let rest = parse_tuple_helper core in
      t :: rest
  | T (x, s) -> errorexpect core s x [ RPAREN; COMMA ]

and parse_typesig_elem_list core =
  match peek' core with
  | T (LPAREN, _) | T (T_IDENT _, _) ->
      reset_peek core;
      let elm = parse_typesig_elem ~rec':true core in
      elm :: parse_typesig_elem_list core
  | _ ->
      reset_peek core;
      []

and parse_typesig_elem ?(rec' = false) core =
  match peek' core with
  | T (LPAREN, _pspan) -> (
      toss core;
      match peek' core with
      | T (RPAREN, _rspan) -> TSTuple []
      | _ -> (
          reset_peek core;
          let t = parse_typesig core in
          match peek' core with
          | T (RPAREN, _rspan) ->
              toss core;
              t
          | T (COMMA, _cspan) ->
              toss core;
              let rest = parse_tuple_helper core in
              TSTuple (t :: rest)
          | T (x, span) -> errorexpect core span x [ RPAREN; COMMA ]))
  | T (T_IDENT nm, _span) ->
      toss core;
      if rec' then
        TSBase nm
      else
        let l = parse_typesig_elem_list core in
        if l = [] then (
          reset_peek core;
          TSBase nm)
        else
          TSApp (l, nm)
  | T (x, s) -> errorexpect core s x [ LPAREN; T_IDENT "example" ]

and parse_typesig core : typesig =
  let tmp = parse_typesig_elem core in
  match peek' core with
  | T (TS_TO, _s) ->
      toss core;
      let rest = parse_typesig core in
      TSMap (tmp, rest)
  | _ ->
      reset_peek core;
      tmp

and parse_peek_typecase_list rettyp core =
  match peek' core with
  | T (PIP_OP "|", pipspan) -> (
      toss' core;
      let nm = parse_ident core in
      match peek' core with
      | T (PIP_OP "|", span) ->
          reset_peek core;
          ({ head = fst nm; args = []; typ = Ok rettyp }, snd nm)
          :: parse_peek_typecase_list rettyp core
      | T (T_IDENT _, span) ->
          let idlist = parse_typesig_elem_list core in
          let totalspan = Errors.spandiff pipspan span in
          ({ head = fst nm; args = idlist; typ = Ok rettyp }, totalspan)
          :: parse_peek_typecase_list rettyp core
      | T (COL_OP ":", span) ->
          toss' core;
          let rec go () =
            let tmp = parse_typesig_elem core in
            match peek' core with
            | T (TS_TO, s) ->
                toss core;
                tmp :: go ()
            | _ ->
                reset_peek core;
                [ tmp ]
          in
          let rec all_but_last = function
            | [] ->
                errorexpect core
                  (let { span; token } = current core in
                   span)
                  EMPTY [ TYPE ]
            | [ x ] -> []
            | x :: xs -> x :: all_but_last xs
          in
          let buf = go () in
          let last = ListHelpers.last buf in
          let allbut = all_but_last buf in
          let span = spandiff pipspan (currentspan core) in
          ({ head = fst nm; args = allbut; typ = Ok last }, span)
          :: parse_peek_typecase_list rettyp core
      | T (x, span) -> errorexpect core span x [ T_IDENT "a"; COL_OP ":" ])
  | _ ->
      reset_peek core;
      []

and parse_type core =
  expect core TYPE;
  let id = parse_ident core in
  let idlist, idlistspan = mergespans @@ parse_peek_ident_list core in
  expect core (EQ_OP "=");
  match peek' core with
  | T (PIP_OP "|", s) ->
      reset_peek core;
      let pats =
        parse_peek_typecase_list
          (TSApp (List.map (fun x -> TSBase x) idlist, fst id))
          core
      in
      let pats', patlistspan' = mergespans pats in
      let spans = info3 (snd id, idlistspan, patlistspan') in
      Typedecl (spans, fst id, idlist, pats')
  | _ ->
      reset_peek core;
      let typ = parse_typesig core in
      let spans =
        info3 (snd id, idlistspan, Errors.endtoend idlistspan (currentspan core))
      in
      Typealias (spans, fst id, idlist, typ)

(*
let rec parse_bin_exp min_prec = parse_bin_exp' (parse_unary_expr()) min_prec
and parse_bin_exp' res min_prec =
    let op = !curr_tok in
    if is_bin op && prec op >= min_prec then
        next_tok();
        let min_prec' = if is_left_assoc op then 1+(prec op) else prec op in
        parse_bin_exp' (Bin(res, op, parse_bin_exp min_prec')) min_prec
    else res
*)

and parse_expr' core prec = match peek' core with _ -> todo "parseexpr"
and parse_expr core = parse_expr' core 0

and parse_let core =
  expect core SIG;
  let ts = parse_typesig core in
  print_endline (show_core !core);
  print_endline (show_tok @@ current core);
  match peek' core with
  | T (LET, lspan) ->
      toss core;
      let nm, nmspan = parse_ident core in
      let args, span = mergespans @@ parse_peek_ident_list core in
      expect core (EQ_OP "=");
      let body = parse_expr core in
      let inf = info4 (nmspan, emptyspan, span, get_span body) in
      TopAssign (inf, nm, ts, args, body)

and parse_toplevel core =
  match peek' core with
  | T (TYPE, s) -> Some (parse_type core)
  | T (SIG, s) -> Some (parse_let core)
  (* | T (MODULE, s) -> Some (parse_module core)
     | T (BIND, s) -> Some (parse_bind core)
     | T (EXTERN, s) -> Some (parse_extern core)
     | T (OPEN, s) -> Some (parse_open core)
  *)
  | _ -> None

let program_h lexfunc lexbuf file =
  let core = Tok.new_core lexfunc lexbuf file in
  let rec go () =
    match parse_toplevel core with Some t -> t :: go () | None -> []
  in
  let p = Program (go ()) in
  print_endline (show_program p);
  p

let program lexfunc lexbuf file = program_h lexfunc lexbuf file
