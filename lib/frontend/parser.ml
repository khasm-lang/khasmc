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
  | INVALID_EXPR
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
  | T (_t, _span) ->
      reset_peek core;
      None

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
      | T (RPAREN, _rspan) ->
          toss core;
          TSTuple []
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

and parse_typesig_h core =
  let tmp = parse_typesig_elem core in
  match peek' core with
  | T (TS_TO, _s) ->
      toss core;
      let rest = parse_typesig core in
      TSMap (tmp, rest)
  | _ ->
      reset_peek core;
      tmp

and parse_typesig core : typesig =
  match peek' core with
  | T (LT_OP "<", _s) ->
      toss' core;
      let ids = List.map fst @@ parse_peek_ident_list core in
      expect core (GT_OP ">");
      let ts = parse_typesig_h core in
      let rec go is =
        match is with [] -> ts | x :: xs -> TSForall (x, go xs)
      in
      go ids
  | _ ->
      reset_peek core;
      parse_typesig_h core

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

and binop_precedence s =
  match KhasmUTF.split_unicode s with
  | s, _ -> (
      match String.get s 0 with
      | ';' -> 0
      | '$' -> 1
      | '=' | '>' | '<' -> 2
      | '|' | '&' -> 3
      | '@' | '^' -> 4
      | ':' -> 5
      | '+' | '-' -> 6
      | '*' | '/' | '%' -> 7
      | _ -> raise @@ Impossible "invalid char index bind pow")

and is_leftassoc s =
  match KhasmUTF.split_unicode s with
  | s, _ -> (
      match String.get s 0 with
      | ';' -> true
      | '@' | '^' -> true
      | ':' -> true
      | _ -> false)

and get_binop_peek core =
  match peek' core with
  | T (POW_OP s, _)
  | T (MUL_OP s, _)
  | T (DIV_OP s, _)
  | T (MOD_OP s, _)
  | T (ADD_OP s, _)
  | T (SUB_OP s, _)
  | T (CAR_OP s, _)
  | T (AT_OP s, _)
  | T (EQ_OP s, _)
  | T (LT_OP s, _)
  | T (GT_OP s, _)
  | T (AND_OP s, _)
  | T (DOL_OP s, _) ->
      Some s
  | T (PIP_OP s, _) when String.length s > 1 -> Some s
  | T (SEMICOLON, _) -> Some ";"
  | _ -> None

and parse_match_branch core =
  expect core (PIP_OP "|");
  let some x = Some x in
  let rec parse_pat around core =
    match peek' core with
    | T (T_IDENT s, _) ->
        toss' core;
        let rec h () =
          match parse_pat true core with Some n -> n :: h () | None -> []
        in
        if not around then
          let h = h () in
          some @@ MPApp (s, h)
        else
          some @@ MPId s
    | T (TICK, _) ->
        toss' core;
        let i, s = parse_ident core in
        some @@ MPId i
    | T (LPAREN, _) ->
        toss' core;
        let rec h () =
          match parse_pat false core with
          | Some n ->
              expect core COMMA;
              n :: h ()
          | None ->
              expect core RPAREN;
              []
        in
        let h = h () in
        some @@ MPTup h
    | _ ->
        reset_peek core;
        None
  in
  match parse_pat false core with
  | None -> error core (currentspan core) (T_IDENT "id")
  | Some pat ->
      expect core LAM_TO;
      let e = parse_expr core 0 in
      (pat, e)

and parse_expr_base_list =
  [ T_IDENT "ident"; T_INT "5"; T_FLOAT "5.3"; T_STRING "l"; LPAREN ]

and parse_expr_base_h core =
  match peek' core with
  | T (T_IDENT id, s) ->
      toss core;
      Some (Base (info s, Ident (info s, id)))
  | T (T_INT id, s) ->
      toss core;
      Some (Base (info s, Int id))
  | T (T_FLOAT id, s) ->
      toss core;
      Some (Base (info s, Float id))
  | T (T_STRING id, s) ->
      toss core;
      Some (Base (info s, Str id))
  | T (TRUE, s) ->
      toss core;
      Some (Base (info s, True))
  | T (FALSE, s) ->
      toss core;
      Some (Base (info s, False))
  | T (LPAREN, s) -> (
      toss core;
      match peek' core with
      | T (RPAREN, s) ->
          toss core;
          Some (Base (info s, Tuple []))
      | _ -> (
          reset_peek core;
          let e = parse_expr core 0 in
          let rec go acc =
            match peek' core with
            | T (COMMA, s) ->
                toss' core;
                let e = parse_expr core 0 in
                go (e :: acc)
            | T (RPAREN, s) ->
                toss core;
                List.rev acc
            | T (x, s) -> errorexpect core s x [ COMMA; RPAREN ]
          in
          match go [] with
          | [] -> Some e
          | x :: xs -> Some (Base (info s, Tuple (e :: x :: xs)))))
  | _ ->
      reset_peek core;
      None

and parse_expr_base core =
  match parse_expr_base_h core with
  | None -> None
  | Some ret -> (
      let rec go acc =
        match parse_expr_base_h core with
        | Some n -> go (n :: acc)
        | None -> acc
      in
      match go [] with
      | [] -> Some ret
      | x ->
          let rec helper xs =
            match xs with
            | [] -> raise @@ Impossible "empty function call"
            | [ x ] -> FCall (get_info x, ret, x)
            | x :: xs -> FCall (get_info x, helper xs, x)
          in
          Some (helper x))

and parse_expr_compound core =
  let some x = Some x in
  let ret =
    match peek' core with
    | T (IF, s) ->
        toss' core;
        let c = parse_expr core 0 in
        expect core THEN;
        let e1 = parse_expr core 0 in
        expect core ELSE;
        let e2 = parse_expr core 0 in
        some @@ IfElse (info (Errors.spandiff s (currentspan core)), c, e1, e2)
    | T (LET, s) -> (
        toss' core;
        let id, _s = parse_ident core in
        match pop' core with
        | T (COL_OP ":", s) ->
            let ts = parse_typesig core in
            expect core (EQ_OP "=");
            let e1 = parse_expr core 0 in
            expect core IN;
            let e2 = parse_expr core 0 in
            some
            @@ AnnotLet
                 (info (Errors.spandiff s (currentspan core)), id, ts, e1, e2)
        | T (EQ_OP "=", s) ->
            let e1 = parse_expr core 0 in
            expect core IN;
            let e2 = parse_expr core 0 in
            some
            @@ LetIn (info (Errors.spandiff s (currentspan core)), id, e1, e2)
        | T (x, s) -> errorexpect core s x [ COL_OP ":"; EQ_OP "=" ])
    | T (FUN, s) -> (
        toss' core;
        let id, _s = parse_ident core in
        match pop' core with
        | T (COL_OP ":", _) ->
            let ts = parse_typesig core in
            expect core LAM_TO;
            let e = parse_expr core 0 in
            some @@ AnnotLam (info s, id, ts, e)
        | T (LAM_TO, _) ->
            let e = parse_expr core 0 in
            some @@ Lam (info s, id, e)
        | T (x, s) -> errorexpect core s x [ COL_OP ":"; EQ_OP "=" ])
    | T (TFUN, s) ->
        toss' core;
        let id, _s = parse_ident core in
        expect core LAM_TO;
        let e = parse_expr core 0 in
        some @@ TypeLam (info s, id, e)
    | T (MATCH, s) ->
        toss' core;
        let e = parse_expr core 0 in
        expect core WITH;
        let rec helper () =
          match peek' core with
          | T (PIP_OP "|", _) ->
              let p = parse_match_branch core in
              p :: helper ()
          | T (END, _) ->
              toss' core;
              []
          | T (a, e) -> errorexpect core e a [ PIP_OP "|"; END ]
        in
        let h = helper () in
        some
        @@ Match (info (Errors.spandiff (get_span e) (currentspan core)), e, h)
    | _ ->
        reset_peek core;
        parse_expr_base core
  in
  ret
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

and parse_expr' core lhs prec =
  match get_binop_peek core with
  | Some ";" ->
      let p = binop_precedence ";" in
      if p >= prec then (
        toss core;
        let min = p + 1 in
        let inner = parse_expr core min in
        parse_expr' core (Join (dummyinfo, lhs, inner)) prec)
      else (
        reset_peek core;
        lhs)
  | Some op ->
      let p = binop_precedence op in
      if p >= prec then (
        toss core;
        let min =
          if is_leftassoc op then
            1 + p
          else
            p
        in
        let inner =
          FCall (dummyinfo, Base (dummyinfo, Ident (dummyinfo, op)), lhs)
        in
        let inner2 = parse_expr core min in
        parse_expr' core (FCall (dummyinfo, inner, inner2)) prec)
      else (
        reset_peek core;
        lhs)
  | None ->
      reset_peek core;
      lhs

and parse_expr core prec =
  let e1 = parse_expr_compound core in
  match e1 with
  | Some x -> (
      let r = parse_expr' core x prec in
      match peek' core with
      | T (DOT, s) ->
          (* TODO: chained tuple access *)
          toss' core;
          let i =
            match pop' core with
            | T (T_INT s, _s) -> int_of_string s
            | T (x, s) -> error core s (T_INT "0")
          in
          TupAccess (info s, r, i)
      | _ ->
          reset_peek core;
          r)
  | None ->
      errorexpect core (currentspan core) INVALID_EXPR parse_expr_base_list

and parse_let core =
  expect core SIG;
  let ts = parse_typesig core in
  match peek' core with
  | T (LET, lspan) -> (
      toss core;
      match peek' core with
      | T (REC, lspan) ->
          toss' core;
          let nm, nmspan = parse_ident core in
          let args, span = mergespans @@ parse_peek_ident_list core in
          expect core (EQ_OP "=");
          let body = parse_expr core 0 in
          let inf = info4 (nmspan, emptyspan, span, get_span body) in
          TopAssignRec (inf, nm, ts, args, body)
      | _ ->
          reset_peek core;
          let nm, nmspan = parse_ident core in
          let args, span = mergespans @@ parse_peek_ident_list core in
          expect core (EQ_OP "=");
          let body = parse_expr core 0 in
          let inf = info4 (nmspan, emptyspan, span, get_span body) in
          TopAssign (inf, nm, ts, args, body))
  | T (a, b) -> errorexpect core b a [ LET ]

and parse_module core =
  expect core MODULE;
  let id, is = parse_ident core in
  expect core (EQ_OP "=");
  let body = parse_toplevel_list core in
  expect core END;
  SimplModule (info2 (is, Errors.spandiff is (currentspan core)), id, body)

and parse_open core =
  expect core OPEN;
  let id, is = parse_ident core in
  Open (info is, id)

and parse_bind core =
  let i =
    match pop' core with T (BIND, i) -> i | T (a, b) -> error core b a BIND
  in
  match get_binop_peek core with
  | None -> error core (currentspan core) BAD
  | Some x ->
      toss core;
      expect core (EQ_OP "=");
      let id, is = parse_ident core in
      (* TODO: Handle modules (or maybe not?) *)
      Bind (info3 (i, is, Errors.spandiff i is), x, [], id)

and parse_extern core =
  expect core EXTERN;
  let i, is =
    match pop' core with T (T_INT i, s) -> (i, s) | T (a, b) -> error core b a
  in
  let i' = int_of_string i in
  let id, ids = parse_ident core in
  expect core (COL_OP ":");
  let ts = parse_typesig core in
  expect core (EQ_OP "=");
  let bindto, bindtos = parse_ident core in
  IntExtern
    (info4 (is, ids, bindtos, Errors.spandiff is bindtos), id, bindto, i', ts)

and parse_toplevel core =
  match peek' core with
  | T (TYPE, s) -> Some (parse_type core)
  | T (SIG, s) -> Some (parse_let core)
  | T (MODULE, s) -> Some (parse_module core)
  | T (OPEN, s) -> Some (parse_open core)
  | T (BIND, s) -> Some (parse_bind core)
  | T (EXTERN, s) -> Some (parse_extern core)
  | _ ->
      reset_peek core;
      None

and parse_toplevel_list core =
  match parse_toplevel core with
  | Some x -> x :: parse_toplevel_list core
  | None -> []

let program_h lexfunc lexbuf file =
  let core = Tok.new_core lexfunc lexbuf file in
  let p = Program (parse_toplevel_list core) in
  if p = Program [] then (
    let ctx = Errors.from_file (ref !core.file) in
    Errors.add ctx Errors.emptyspan "Expected Nonempty file";
    raise @@ SyntaxErr (Errors.to_string ctx))
  else
    p

let program lexfunc lexbuf file = program_h lexfunc lexbuf file
