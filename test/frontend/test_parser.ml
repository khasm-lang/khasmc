let%test_unit "Parser" =
  let open Khasmc.Parser in
  let open Khasmc.Lexer in
  let source =
    {|
    internal_extern 1 `whatever : forall a, a -> a = sub
    bind / = foo
    bind @++++++++ = gobbo
    let foo a b c d : int = a b c d
    let rec bar : int = bar
    let a : int = 1 + 2
    let b : int = 1 +++++++ 5
    let c : int = 1 + 2 + 3 + 4 + 5 + 6
    let d : function = fun x : int => x
    let e : int = tfun T => fun x : T => x
    let f : tup = tuple.0
    let g : woop =
    match k with
    | 'Three => 1
    | Two a => g
    | (a, b) => f
    | (((((a),b),c),d),e) => f
    | _ => k
    end
|}
  in
  let lex = Lexing.from_string source in
  Lexing.set_filename lex "test";
  let _res = program token lex "test" in
  ()
