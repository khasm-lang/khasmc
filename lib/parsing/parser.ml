[@@@ocaml.warning "-8-28-26"]

open Token
open Share.Uuid
open Share.Maybe
open Ast

let data' () : unit data =
  { uuid = uuid (); counter = 0; span = None }

let lexer buf =
  let rec go acc =
    match lexer_ buf with
    | Ok DONE -> List.rev (DONE :: acc)
    | Ok s -> go (s :: acc)
    | Error e -> List.rev acc
  in
  go []

let next buf =
  match !buf with
  | x :: xs ->
      buf := xs;
      x
  | [] -> failwith "next on empty buffer"

let next' buf = ignore (next buf)

let peek buf =
  match !buf with
  | x :: xs -> x
  | [] -> failwith "peek on empty buffer"

let peek2 buf =
  match !buf with
  | x :: y :: xs -> y
  | _ -> failwith "peek2 on emptyish buffer"

exception ParseError

let expect tok buf =
  match next buf with
  | s when s = tok -> ()
  | s ->
      print_endline "expect failed";
      print_endline ("wanted: " ^ show_t_TOKEN tok);
      print_endline ("got: " ^ show_t_TOKEN s);
      raise ParseError

let prec x =
  match x with
  | PLUS | MINUS -> 20
  | STAR | FSLASH -> 30
  | AND | PIPE -> 10
  | EQUALS -> 0
  | _ -> -1

let to_binop x =
  match x with
  | PLUS -> Add
  | MINUS -> Sub
  | STAR -> Mul
  | FSLASH -> Div
  | AND -> LAnd
  | PIPE -> LOr
  | EQUALS -> Eq
  | _ -> failwith "to_binop failed"

let rec type' buf =
  let start =
    match next buf with
    | LEFTP -> begin
        match peek buf with
        | RIGHTP ->
            next' buf;
            TyTuple []
        | _ ->
            let s = type' buf in
            begin match next buf with
            | RIGHTP -> s
            | COMMA ->
                (* tuple *)
                let rec go () =
                  let s = type' buf in
                  match next buf with
                  | RIGHTP -> [ s ]
                  | COMMA -> s :: go ()
                in
                TyTuple (s :: go ())
            end
      end
    | TYINT -> TyInt
    | TYSTRING -> TyString
    | TYCHAR -> TyChar
    | TYFLOAT -> TyFloat
    | TYBOOL -> TyBool
    | POLYID s -> TyPoly s
    | REF -> TyRef (type' buf)
    | TYPEID s -> begin
        match peek buf with
        | TYPEID _ | POLYID _ | LEFTP _ ->
            (* custom with args *)
            failwith "args"
        | _ -> failwith "no args"
      end
    | _ -> failwith "other type weirdness"
  in
  match peek buf with
  | ARROW ->
      next' buf;
      let rest = type' buf in
      TyArrow (start, rest)
  | _ -> start

let rec case' buf =
  match next buf with
  | ID s ->
      (* maybe custom, maybe not *)
      let rec handle () =
        begin match peek buf with
        | LEFTP ->
            (* subexpr *)
            next' buf;
            let sub = case' buf in
            expect RIGHTP buf;
            sub :: handle ()
        | ID s ->
            (* subvar *)
            next' buf;
            CaseVar s :: handle ()
        | _ ->
            (* not any of that *)
            []
        end
      in
      begin match handle () with
      | [] -> CaseVar s
      | xs -> CaseCtor (s, xs)
      end
  | LEFTP ->
      (* tuple or abbrev *)
      begin match peek buf with
      | RIGHTP ->
          next' buf;
          CaseTuple []
      | _ ->
          let e = case' buf in
          let rec go () =
            match next buf with
            | RIGHTP -> []
            | COMMA ->
                let k = case' buf in
                let rest = go () in
                k :: rest
          in
          CaseTuple (e :: go ())
      end
  | _ -> failwith "huh?"

module Expr = struct
  let ( let* ) = Option.bind

  let ( let+ ) x f =
    match x with
    | None ->
        print_endline "let+ None";
        raise ParseError
    | Some s -> f s

  let some x = Some x

  let rec expr curr curr_prec buf =
    let t = peek buf in
    match prec t with
    | -1 ->
        (* no valid operator char - maybe application? *)
        (* must be a current for this to be true *)
        begin match expr_small buf with
        | None ->
            (* not application *)
            some curr
        | Some s ->
            (* yes application *)
            let rec do_app () =
              match expr_small buf with
              | None -> []
              | Some s -> s :: do_app ()
            in
            let rest = do_app () |> List.rev in
            let orig = Funccall (data' (), curr, s) in
            some
              (List.fold_right
                 (fun acc x -> Funccall (data' (), x, acc))
                 rest orig)
        end
    | prec ->
        if prec < curr_prec then
          some curr
        else begin
          next' buf;
          let* rhs = expr' prec buf in
          let c = Binop (data' (), to_binop t, curr, rhs) in
          expr c curr_prec buf
        end

  and expr' curr_prec buf =
    let* curr = expr_small buf in
    expr curr curr_prec buf

  and expr_small buf =
    let exception NoValid in
    try
      begin match peek buf with
      | LEFTP ->
          next' buf;
          let* e = expr' 0 buf in
          (* either parens or tuple *)
          begin match next buf with
          | RIGHTP -> some e
          | COMMA ->
              let rec tupler () =
                let* s = expr' 0 buf in
                begin match next buf with
                | RIGHTP -> some [ s ]
                | COMMA ->
                    let* rest = tupler () in
                    some (s :: rest)
                end
              in
              let* rest = tupler () in
              some @@ Tuple (data' (), e :: rest)
          end
      | ID i ->
          next' buf;
          some @@ Var (data' (), i)
      | INT i ->
          next' buf;
          some @@ Int (data' (), i)
      | FLOAT i ->
          next' buf;
          some @@ Float (data' (), i)
      | STRING i ->
          next' buf;
          some @@ String (data' (), i)
      | BOOL b ->
          next' buf;
          some @@ Bool (data' (), b)
      | LET -> begin
          next' buf;
          let case = case' buf in
          let ty =
            match next buf with
            | COLON ->
                let ty = type' buf in
                expect EQUALS buf;
                Some ty
            | EQUALS -> None
          in
          let+ expr'' = expr' 0 buf in
          expect IN buf;
          let+ body = expr' 0 buf in
          some @@ LetIn (data' (), case, ty, expr'', body)
        end
      | IF -> begin
          next' buf;
          let+ c = expr' 0 buf in
          expect THEN buf;
          let+ true' = expr' 0 buf in
          expect ELSE buf;
          let+ false' = expr' 0 buf in
          let cases =
            [
              (CaseLit (LBool true), true');
              (CaseLit (LBool false), false');
            ]
          in
          some @@ Match (data' (), c, cases)
        end
      | _ -> raise NoValid
      end
    with NoValid -> None
end

open Expr

let definition_up_to_body buf =
  begin match peek buf with FUN -> next' buf | _ -> ()
  end;
  let (ID name) = next buf in
  let targs =
    match peek buf with
    | LEFTC -> begin
        match peek2 buf with
        | TYPE ->
            next' buf;
            next' buf;
            let rec go () =
              match next buf with
              | POLYID t -> t :: go ()
              | RIGHTC -> []
              | t ->
                  print_endline (show_t_TOKEN t);
                  failwith "parsing type list weird"
            in
            Some (go ())
        | _ -> None
      end
    | _ -> None
  in
  let targs = match targs with Some xs -> xs | None -> [] in
  let arg_list =
    let rec go () =
      match peek buf with
      | LEFTP ->
          next' buf;
          let (ID argnm) = next buf in
          let COLON = next buf in
          let typ = type' buf in
          let RIGHTP = next buf in
          (argnm, typ) :: go ()
      | _ -> []
    in
    go ()
  in
  expect COLON buf;
  let ret = type' buf in
  {
    data = data' ();
    name;
    typeargs = targs;
    args = arg_list;
    return = ret;
    body = Nothing;
  }

let rec toplevel' buf =
  match next buf with
  | FUN -> begin
      let definition_main = definition_up_to_body buf in
      let EQUALS = next buf in
      let+ body = expr' 0 buf in
      let data = data' () in
      let res =
        Definition { definition_main with body = Just body }
      in
      res :: toplevel' buf
    end
  | DONE -> []
  | t ->
      print_endline (show_t_TOKEN t);
      failwith "anything other than definition"

let toplevel buf : ((string, unit) toplevel list, 'a) Result.t =
  let toks = ref (lexer buf) in

  List.iter (fun x -> print_string (show_t_TOKEN x ^ " ")) !toks;
  print_newline ();

  try
    let t = toplevel' toks in
    Ok t
  with exc ->
    print_endline (Printexc.to_string exc);
    print_endline "next ten tokens:";
    for i = 0 to 10 do
      print_string (show_t_TOKEN (next toks) ^ " ")
    done;
    failwith "parser error"
