open Ast

let ws = Parseff.skip_whitespace

let lit s =
  ws ();
  ignore @@ Parseff.consume s

let d () =
  { uuid = Share.Uuid.uuid (); span = Span (Parseff.location ()) }

(* makes the assumption nobody is putting \b in their file *)
let backtrack () = ignore @@ Parseff.consume "\b"
let num_re = Re.compile (Re.Posix.re "[0-9]+")
let float_re = Re.compile (Re.Posix.re "[0-9]+\\.?[0-9]+")
let any_re = "[_a-zA-Z0-9]*"
let path = "[A-Z]([A-Z][A-Za-z0-9_\\.]*)*"
let any_re_var_path = path ^ "\\.[a-z]" ^ any_re
let any_re_ctor_path = path ^ "\\.[A-Z]" ^ any_re
let pathless_var = Re.compile (Re.Posix.re ("[a-z]" ^ any_re))
let lower_var = Re.compile (Re.Posix.re any_re_var_path)
let upper_var = Re.compile (Re.Posix.re any_re_ctor_path)
let poly_var = Re.compile (Re.Posix.re "'[a-z]+")
let not_backslash = Re.compile (Re.Posix.re "[^\\]")

let rec type' () =
  Parseff.rec_ (fun () ->
    Parseff.one_of [
      (fun () ->
        let p = Parseff.match_regex upper_var in
        let rest = Parseff.sep_by type'_paren Parseff.whitespace () in
        TyCustom (p, rest)
      );
      (fun () ->
        let rest = Parseff.sep_by type'_paren
          (fun () -> ignore @@ Parseff.char ',') ()
        in
        TyTuple rest
      )
    ] ()
  )

(* doesn't handle function types properly lol *)
and type'_paren () =
  let lhs = Parseff.one_of [
    (fun () ->
      let p = Parseff.match_regex pathless_var in
      TyPoly p
    );
    (fun () ->
      lit "Int";
      TyInt
    );
    (fun () ->
      lit "String";
      TyString
    );
    (fun () ->
      lit "Float";
      TyFloat
    );
    (fun () ->
      lit "Char";
      TyChar
    );
    (fun () ->
      lit "Bool";
      TyBool
    );
    (fun () ->
      lit "Ref";
      type'_paren ()
    );
    (fun () ->
      lit "(";
      let res = type' () in
      lit ")";
      res
    );
  ] ()
  in
  match Parseff.optional (fun () -> ignore @@ Parseff.consume "->") () with
  | None -> lhs
  | Some _ ->
    let rhs = type'_paren () in
    TyArrow (lhs, rhs)

let rec case () =
  Parseff.rec_ (fun () ->
    Parseff.one_of [
      (fun () ->
        let nm = Parseff.match_regex upper_var in
        let rest = Parseff.sep_by case_paren Parseff.whitespace () in
        CaseCtor (nm, rest)
      );
      (fun () ->
        let tup = Parseff.sep_by case_paren Parseff.whitespace () in
        CaseTuple tup
      )
    ] ()
  )
  
and case_paren () =
  ignore @@ Parseff.whitespace ();
  Parseff.one_of [
    (fun () ->
      CaseVar (Parseff.match_regex pathless_var)
    );
    (fun () ->
      let s = Parseff.match_regex num_re in
      CaseLit (LInt s)
    );
    (fun () ->
      let s = Parseff.or_
        (fun () -> Parseff.consume "true")
        (fun () -> Parseff.consume "false") ()
      in
      CaseLit (LBool (bool_of_string s))
    );
    (fun () ->
      ignore @@ Parseff.char '_';
      CaseWild
    );
    (fun () ->
      ignore @@ Parseff.char '(';
      let res = case () in
      ignore @@ Parseff.char ')';
      res
    )
  ] ()

let rec basic () =
  Parseff.one_of
    [
      (fun () ->
        lit "true";
        Bool (d (), true));
      (fun () ->
        lit "false";
        Bool (d (), false));
      (fun () ->
        let s = Parseff.match_regex num_re in
        Int (d (), s));
      (fun () ->
        let s = Parseff.match_regex float_re in
        Float (d (), s));
      (fun () ->
        let s = Parseff.match_regex lower_var in
        Var (d (), s));
      (fun () ->
        lit "\"";
        let s =
          Parseff.many
            (fun () ->
              let k =
                Parseff.or_
                  (fun () ->
                    ignore @@ Parseff.char '\\';
                    Parseff.any_char ())
                  Parseff.any_char ()
              in
              k)
            ()
        in
        lit "\"";
        String (d (), String.of_seq (List.to_seq s)));
      (fun () ->
        let s = Parseff.match_regex upper_var in
        ignore @@ Parseff.satisfy (fun c -> c <> '{') ~label:"record";
        Constructor (d (), s));
    ]
    ()

let prec x =
  match x with
  (* technically should be || and && but we only peek at one *)
  | '+' | '-' | '|' -> 20
  | '*' | '/' | '&' -> 30
  | '=' -> 0
  | _ -> -1

let to_binop x =
  match x with
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | "&&" -> LAnd
  | "||" -> LOr
  | "=" -> Eq
  | _ -> failwith "to_binop failed"

let flip' f = fun a b () -> f b a ()

(* parses operator application and application
   recursive pratt parser 
   *)
let rec expr_app sofar curr_prec () =
  Parseff.or_
    (fun () ->
      (* operator application *)
      let prec =
        Parseff.look_ahead (fun () -> prec (Parseff.any_char ()))
      in
      if prec = -1 then
        backtrack ();
      if prec < curr_prec then
        sofar
      else begin
        let s =
          Parseff.one_of
            (List.map
               (fun x () -> Parseff.consume x)
               [ "+"; "-"; "*"; "/"; "="; "&&"; "||" ])
            ()
        in
        let rhs = expr_base () in
        let sofar' = BinOp (d (), to_binop s, sofar, rhs) in
        expr_app sofar' curr_prec ()
      end)
    (fun () ->
      (* maybe application? *)
      flip' Parseff.or_
        (fun () -> expr_base ())
        (fun () ->
          let p = expr_base () in
          let rest = Parseff.sep_by expr_base Parseff.whitespace () in
          let orig = Funccall (d (), sofar, p) in
          List.fold_left
            (fun acc x ->
              Funccall (d (), x, acc))
            orig rest)
        ())
    ()

(* parses the inner of big expressions
*)
and expr_paren_inner () =
  Parseff.one_of [
    (fun () ->
      lit "let";
      let c = case () in
      lit "=";
      let hd = expr_paren_inner () in
      lit "in";
      let bd = expr_paren_inner () in
      LetIn (d (), c, None, hd, bd)
    );
    (fun () ->
      lit "fun";
      let v = Parseff.match_regex pathless_var in
      lit "=>";
      let e = expr_paren_inner () in
      Lambda (d(), v, None, e)
    );
    (fun () ->
      let r = Parseff.match_regex upper_var in
      ignore @@ Parseff.char '{';
      let fields = Parseff.sep_by (fun () ->
        let nm = Parseff.match_regex lower_var in
        ignore @@ Parseff.char '=';
        let e = expr_base () in
        (nm, e)) (fun () -> Parseff.char ';') ()
      in
      Record (d(), r, fields));
    (fun () ->
      lit "if";
      let c = expr_paren_inner () in
      lit "then";
      let a = expr_paren_inner () in
      lit "else";
      let b = expr_paren_inner () in
      let cases =
        [
          (CaseLit (LBool true), a);
          (CaseLit (LBool false), b);
        ]
      in
      Match (d (), c, cases)
    );
    (fun () ->
      lit "match";
      let hd = expr_paren_inner () in
      lit "with";
      let cases = Parseff.sep_by (fun () ->
        let c = case () in
        lit "=>";
        let e = expr_base () in
        (c, e)
      ) (fun () -> ignore @@ Parseff.char '|') () in
      Match (d(), hd, cases)
    )
  ] ()

(* parses "big" expressions, that must be paren wrapped *)
and expr_paren prec () =
  Parseff.one_of [
    (fun () ->
      lit "(";
      let res = expr_paren_inner () in
      lit ")";
      res);
    (fun () ->
      lit "(";
      let res = Parseff.sep_by expr_base
        (fun () -> ignore @@ Parseff.char ',') ()
      in
      lit ")";
      Tuple (d(), res)
    );
    (fun () ->
      expr 0 ()
    )
  ] ()

(* parses "base" / "simple expressions" *)
and expr_base () =
  Parseff.one_of
    [ (fun () -> basic ()); (fun () -> expr_paren 0 ()) ]
    ()

(* "starting point" *)
and expr curr_prec () =
  let curr = expr_base () in
  expr_app curr curr_prec ()

let do_expr () = expr 0 ()

let def_up_to_body () =
  lit "fun";
  let nm = Parseff.match_regex pathless_var in
  let targs =
    Parseff.optional (fun () ->
      lit "{";
      lit "type";
      let polys = Parseff.sep_by
        (fun () -> Parseff.match_regex poly_var)
        Parseff.whitespace ()
      in
      lit "}";
      polys
    ) ()
    |> function
      | None -> []
      | Some xs -> xs
  in
  let arg_list =
    Parseff.sep_by ~at_least:1 (fun () ->
      lit "(";
      let nm = Parseff.match_regex pathless_var in
      lit ":";
      let ty = type' () in
      lit "(";
      (nm, ty)
    ) Parseff.whitespace ()
  in 
  lit ":";
  let ret = type' () in
  {
    data = d();
    name = nm;
    typeargs = targs;
    args = arg_list;
    return = ret;
    body = Nothing;
  }
