open Angstrom
open Share.Maybe
open Ast

let is_space = function ' ' | '\t' | '\n' -> true | _ -> false
let spaces = skip_while is_space
let string t = spaces *> string t

let chainl1 e op =
  let rec go acc =
    lift2 (fun f x -> f acc x) op e >>= go <|> return acc
  in
  e >>= fun init -> go init

let char c = spaces *> char c <* spaces
let parens p = spaces *> (char '(' *> p <* char ')')

let integer =
  spaces
  *> choice
       [
         take_while1 (function '0' .. '9' -> true | _ -> false)
         >>| int_of_string;
         ( char '-'
         *> take_while1 (function '0' .. '9' -> true | _ -> false)
         >>| fun x -> int_of_string x * -1 );
       ]

let uuid =
  let* i = integer in
  return @@ Share.Uuid.UUID i

let id = spaces *> integer >>= fun i -> return @@ R i
let cons x xs = x :: xs

let sep_by2 s p =
  s *> p >>= fun r ->
  let* a = fix (fun m -> lift2 cons p (s *> m <|> return [])) in
  return @@ (r :: a)

let typ =
  fix (fun typ ->
      let rec trait_bound : 'a trait_bound Angstrom.t =
        parens
        @@
        let* id1 = uuid in
        id >>= fun i ->
        parens
          (many
          @@ ( id >>= fun x ->
               typ >>= fun y -> return (x, y) ))
        >>= fun one ->
        parens
          (many
          @@ ( id >>= fun x ->
               typ >>= fun y -> return (x, y) ))
        >>= fun two -> return @@ (id1, i, one, two)
      in
      choice
        [
          string "TyInt" *> return TyInt;
          string "TyString" *> return TyString;
          string "TyChar" *> return TyChar;
          string "TyFloat" *> return TyFloat;
          string "TyBool" *> return TyBool;
          parens
            (sep_by2 (string ",") typ >>= fun t -> return @@ TyTuple t);
          parens
            ( string "->" *> count 2 typ >>= fun [ a; b ] ->
              return @@ TyArrow (a, b) );
          parens (string "poly" *> id >>= fun t -> return @@ TyPoly t);
          parens
            ( string "custom" *> id >>= fun t ->
              many typ >>= fun l -> return @@ TyCustom (t, l) );
          (* parens
             ( string "assoc" *> count 3 id >>= fun [ a; b; c ] ->
               return @@ TyAssoc (a, b, c) ); *)
          parens (string "ref" *> typ >>= fun i -> return @@ TyRef i);
          parens (string "meta" *> (return @@ TyMeta (ref Unresolved)));
          parens typ;
        ])

let rec trait_bound : 'a trait_bound Angstrom.t =
  parens
  @@
  let* id1 = uuid in
  id >>= fun i ->
  parens
    (many
    @@ ( id >>= fun x ->
         typ >>= fun y -> return (x, y) ))
  >>= fun one ->
  parens
    (many
    @@ ( id >>= fun x ->
         typ >>= fun y -> return (x, y) ))
  >>= fun two -> return @@ (id1, i, one, two)

let case =
  fix (fun case ->
      choice
        [
          (id >>= fun i -> return @@ CaseVar i);
          parens
            ( sep_by2 (string ",") case >>= fun t ->
              return @@ CaseTuple t );
          parens
            ( string "ctor" *> id >>= fun i ->
              many case >>= fun c -> return @@ CaseCtor (i, c) );
        ])

let d () = data ()

let expr =
  fix (fun expr ->
      choice
        [
          (id >>= fun i -> return @@ Var (d (), i));
          string "int" *> return (Int (d (), "5"));
          string "string" *> return (String (d (), "hi"));
          string "float" *> return (Float (d (), "8.7"));
          string "bool" *> return (Bool (d (), true));
          string "char" *> return (Char (d (), "g"));
          parens
            (string "letin"
            *>
            let* c = case in
            let* a = expr in
            let* b = expr in
            return @@ LetIn (d (), c, None, a, b));
          parens
            (string "seq"
            *>
            let* a = expr in
            let* b = expr in
            return @@ Seq (d (), a, b));
          parens
            (string "$"
            *>
            let* a = parens expr in
            let* b = expr in
            return @@ Funccall (d (), a, b));
          parens
            (string "fun"
            *>
            let* i = id in
            let* e = expr in
            return @@ Lambda (d (), i, None, e));
          parens
            ( sep_by2 (string ",") expr >>= fun t ->
              return @@ Tuple (d (), t) );
          parens
            (string "annot"
            *>
            let* t = typ in
            let* e = expr in
            return @@ Annot (d (), e, t));
          parens
            (string "match"
            *>
            let* e = expr in
            let* b =
              many
                (parens
                   (let* c = case in
                    let* e = expr in
                    return (c, e)))
            in
            return @@ Match (d (), e, b));
          parens
            (string "proj"
            *>
            let* i = integer in
            let* e = expr in
            return @@ Project (d (), e, i));
          parens
            (string "ref"
            *>
            let+ e = expr in
            Ref (d (), e));
          parens
            (string "set"
            *>
            let* i = id in
            let+ e = expr in
            Modify (d (), i, e));
          parens
            (string "record"
            *>
            let* i = id in
            let* c =
              many
                (parens
                   (let* i = id in
                    let* e = expr in
                    return (i, e)))
            in
            return @@ (Record (d (), i, c) : 'a expr));
          parens expr;
        ])

let typdef =
  parens
  @@ begin
       let* _ = string "typdef" in
       let* i = id in
       let* args = parens @@ many id in
       let* content =
         choice
           [
             parens
               ( string "record"
               *> many
                    (parens
                       (let* i = id in
                        let* t = typ in
                        return (i, t)))
               >>= fun t -> return @@ Record t );
             parens
               ( string "sum"
               *> many
                    (parens
                       (let* i = id in
                        let* args = many typ in
                        return (i, args)))
               >>= fun t -> return @@ Sum t );
           ]
       in
       return { data = d (); name = i; args; content }
     end

let definition_body =
  parens
  @@
  let* _ = string "let" in
  let* n = id in
  let* targs = parens (many id) in
  let* ts = parens @@ many trait_bound in
  let* args =
    parens
      (many
         (parens
            (let* i = id in
             let* t = typ in
             return (i, t))))
  in
  let* ret = typ in
  let* body = expr in
  return
    {
      data = d ();
      name = n;
      typeargs = targs;
      args;
      bounds = ts;
      return = ret;
      body = Just body;
    }

let definition_nobody =
  parens
  @@
  let* _ = string "let" in

  let* n = id in
  let* targs = parens (many id) in
  let* ts = parens @@ many trait_bound in
  let* args =
    parens
      (many
         (parens
            (let* i = id in
             let* t = typ in
             return (i, t))))
  in
  let* ret = typ in
  return
    {
      data = d ();
      name = n;
      typeargs = targs;
      args;
      bounds = ts;
      return = ret;
      body = Nothing;
    }

let trait =
  parens
  @@
  let* _ = string "trait" in
  let* name = id in
  let* args = parens @@ many id in
  let* assoc = parens @@ many id in
  let* requires = parens @@ many trait_bound in
  let* functions = parens @@ many definition_nobody in
  return
    {
      data = d ();
      name;
      args;
      assoc;
      requirements = requires;
      functions;
    }

let impl =
  parens
  @@
  let* _ = string "impl" in

  let* name = id in
  let* parent = id in
  let* args =
    parens
      (many
         (parens
            (let* i = id in
             let* t = typ in
             return (i, t))))
  in
  let* assocs =
    parens
      (many
         (parens
            (let* i = id in
             let* t = typ in
             return (i, t))))
  in
  let* impls =
    parens
    @@ many
    @@
    let* id = id in
    let* d = definition_body in
    return (id, d)
  in
  return @@ ({ data = d (); parent; args; assocs; impls } : 'a impl)

let toplevel =
  spaces
  *> many
       (choice
          [
            (typdef >>= fun i -> return @@ Typdef i);
            (trait >>= fun t -> return @@ Trait t);
            (impl >>= fun i -> return @@ Impl i);
            (definition_body >>= fun d -> return @@ Definition d);
          ])
  <* spaces
