open Ast

let pp_string fmt s = Format.fprintf fmt "%s" s
let ws = Parseff.skip_whitespace

let p_expr x = print_endline (show_expr pp_string pp_unit x)

let lit s =
  ws ();
  ignore @@ Parseff.consume s

let iflit b s =
  if b then begin
    ws ();
    ignore @@ Parseff.consume s
  end

let d () =
  { uuid = Share.Uuid.uuid (); span = Span (Parseff.location ()) }

let keywords =
  [ "let"; "in"; "fun"; "match"; "if";
    "then"; "else"; "type"; "with"; "end"]

(* makes the assumption nobody is putting \b in their file *)
let backtrack () : 'a =
  ignore @@ Parseff.consume "\b";
  failwith "unreachable?"

let num_re = Re.compile (Re.Posix.re "[0-9]+")
let float_re = Re.compile (Re.Posix.re "[0-9]+\\.?[0-9]+")
(* TODO: handle module paths *)

let any_re = "[_a-zA-Z0-9]*"
let any_re_var_path = "[a-z]" ^ any_re
let any_re_ctor_path = "[A-Z]" ^ any_re
let pathless_var = Re.compile (Re.Posix.re ("[a-z]" ^ any_re))
let lower_var = Re.compile (Re.Posix.re any_re_var_path)
let upper_var = Re.compile (Re.Posix.re any_re_ctor_path)
let poly_var = Re.compile (Re.Posix.re "'[a-z]+")
let not_backslash = Re.compile (Re.Posix.re "[^\\]")

(* doesn't handle function types properly lol *)
let rec type' dop () =
  Parseff.rec_ (fun () ->
      let lhs =
        Parseff.one_of
          [
            (fun () ->
              let p = Parseff.match_regex pathless_var in
              TyPoly p);
            (fun () ->
              lit "Int";
              TyInt);
            (fun () ->
              lit "String";
              TyString);
            (fun () ->
              lit "Float";
              TyFloat);
            (fun () ->
              lit "Char";
              TyChar);
            (fun () ->
              lit "Bool";
              TyBool);
            (fun () ->
              lit "Ref";
              type' true ());
            (fun () ->
              iflit dop "(";
              let p = Parseff.match_regex upper_var in
              let rest =
                Parseff.sep_by (type' true) Parseff.whitespace ()
              in
              iflit dop ")";
              TyCustom (p, rest));
            (fun () ->
              lit "(";
              let rest =
                Parseff.sep_by (type' false)
                  (fun () -> ignore @@ Parseff.char ',')
                  ()
              in
              lit ")";
              TyTuple rest);
          ]
          ()
      in
      match
        Parseff.optional (fun () -> ignore @@ Parseff.consume "->") ()
      with
      | None -> lhs
      | Some _ ->
          let rhs = type' false () in
          TyArrow (lhs, rhs))

let do_type = type' false

let rec case dop () =
  Parseff.rec_ (fun () ->
      ignore @@ Parseff.whitespace ();
      Parseff.one_of
        [
          (fun () -> CaseVar (Parseff.match_regex pathless_var));
          (fun () ->
            let s = Parseff.match_regex num_re in
            CaseLit (LInt s));
          (fun () ->
            let s =
              Parseff.or_
                (fun () -> Parseff.consume "true")
                (fun () -> Parseff.consume "false")
                ()
            in
            CaseLit (LBool (bool_of_string s)));
          (fun () ->
            ignore @@ Parseff.char '_';
            CaseWild);
          (fun () ->
            iflit dop "(";
            let nm = Parseff.match_regex upper_var in
            let rest =
              Parseff.sep_by (case true) Parseff.whitespace ()
            in
            iflit dop ")";
            CaseCtor (nm, rest));
          (fun () ->
            let tup =
              Parseff.sep_by (case false) Parseff.whitespace ()
            in
            CaseTuple tup);
        ]
        ())

let do_case = case false

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
        if List.mem s keywords then begin
          (* for some reason it doesn't like backtracking here
             also it would be miserable perf wise
             *)
          backtrack ()
        end
        else
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
  (* technically should be || and && but we only peek at one
        ocaml-esque 
        *)
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
  ws ();
  Parseff.one_of
    [
      (fun () ->
        (* operator application *)
        let prec =
          Parseff.look_ahead (fun () -> prec (Parseff.any_char ()))
        in
        (* char does not correspond to an operator *)
        if prec = -1 then begin
          backtrack ()
        end;
        (* do the normal pratt parsing dance *)
        if prec < curr_prec then begin
          sofar
        end
        else begin
          let s =
            Parseff.one_of
              (List.map
                 (fun x () -> Parseff.consume x)
                 [ "+"; "-"; "*"; "/"; "="; "&&"; "||" ])
              ()
          in
          let rhs = expr true prec () in
          let sofar' = BinOp (d (), to_binop s, sofar, rhs) in
          expr_app sofar' curr_prec ()
        end);
      (fun () ->
        (* maybe application? *)
        Parseff.or_
          (fun () ->
            (* must be one at least one app-e *)
            let p = expr_paren true () in
            (* collect further app-es *)
            let rest =
              Parseff.sep_by (expr_paren true) Parseff.whitespace
                ()
            in
            let orig = Funccall (d (), sofar, p) in
            let app =
              List.fold_right
                (fun x acc -> Funccall (d (), acc, x))
                rest orig
            in
            (* continue onwards; cannot be another application immediately,
             as we've captured all of them here, but this allows for e.g.
             1 + foo x y * 2 to work
             you decide whether that's good or not
             *)
            expr_app app curr_prec ())
          (fun () -> sofar)
          ());
    ]
    ()

(* parses "big" expressions, that must be paren wrapped (sometimes) *)
and expr_paren dop () =
  ws ();
  Parseff.one_of
    [
      basic;
      (fun () ->
        lit "(";
        let res =
          Parseff.sep_by ~at_least:2 (expr false 0)
            (fun () -> ignore @@ Parseff.char ',')
            ()
        in
        lit ")";
        Tuple (d (), res));
      (fun () ->
        iflit dop "(";
        lit "let";
        let c = do_case () in
        lit "=";
        let hd = expr false 0 () in
        lit "in";
        let bd = expr false 0 () in
        iflit dop ")";
        LetIn (d (), c, None, hd, bd));
      (fun () ->
        iflit dop "(";
        lit "fun";
        let v = Parseff.match_regex pathless_var in
        lit "=>";
        let e = expr false 0 () in
        iflit dop ")";
        Lambda (d (), v, None, e));
      (fun () ->
        iflit dop "(";
        let r = Parseff.match_regex upper_var in
        lit "{";
        let fields =
          Parseff.sep_by
            (fun () ->
              let nm = Parseff.match_regex lower_var in
              ignore @@ Parseff.char '=';
              let e = expr true 0 () in
              (nm, e))
            (fun () -> Parseff.char ';')
            ()
        in
        lit "}";
        iflit dop ")";
        Record (d (), r, fields));
      (fun () ->
        iflit dop "(";
        lit "if";
        let c = expr false 0 () in
        lit "then";
        let a = expr false 0 () in
        lit "else";
        let b = expr false 0 () in
        let cases =
          [ (CaseLit (LBool true), a); (CaseLit (LBool false), b) ]
        in
        iflit dop ")";
        Match (d (), c, cases));
      (fun () ->
        iflit dop "(";
        lit "match";
        let hd = expr false 0 () in
        lit "with";
        let cases =
          Parseff.many 
            (fun () ->
              lit "|";
              let c = do_case () in
              lit "=>";
              let e = expr false 0 () in
              (c, e))
            ()
        in
        lit "end";
        iflit dop ")";
        Match (d (), hd, cases));
      (fun () ->
        lit "(";
        let res = expr false 0 () in
        lit ")";
        res
      );
    ]
    ()

(* "starting point" *)
and expr (dop : bool) (curr_prec : int) () =
  Parseff.rec_ (fun () -> 
    let curr = expr_paren dop () in
    expr_app curr curr_prec ())
    
let do_expr () = expr false 0 ()

let print_error error =
  match error with `Expected s -> print_endline s | _ -> ()

let parser () =
  let res = do_expr () in
  Parseff.end_of_input ();
  res

let test () =
  let s = {|
  (1,(3,4,5+3*5=8))
|} in
  let src = Parseff.Source.of_string s in
  let ret = Parseff.parse_source src parser in
  match ret with
  | Ok ret -> print_endline (show_expr pp_string pp_unit ret)
  | Error { pos; error } ->
      print_error error;
      print_string "error at: ";
      print_int (pos - 1);
      print_string ", ";
      print_endline
        ( String.sub s pos (pos+5) )

let def_up_to_body () =
  lit "fun";
  let nm = Parseff.match_regex pathless_var in
  let targs =
    Parseff.optional
      (fun () ->
        lit "{";
        lit "type";
        let polys =
          Parseff.sep_by
            (fun () -> Parseff.match_regex poly_var)
            Parseff.whitespace ()
        in
        lit "}";
        polys)
      ()
    |> function
    | None -> []
    | Some xs -> xs
  in
  let arg_list =
    Parseff.sep_by ~at_least:1
      (fun () ->
        lit "(";
        let nm = Parseff.match_regex pathless_var in
        lit ":";
        let ty = do_type () in
        lit "(";
        (nm, ty))
      Parseff.whitespace ()
  in
  lit ":";
  let ret = do_type () in
  {
    data = d ();
    name = nm;
    typeargs = targs;
    args = arg_list;
    return = ret;
    body = Nothing;
  }
