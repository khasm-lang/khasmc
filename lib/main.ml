open Front.Ast
open Common.Info
open Common.Error

let example_files =
  [
    {
      name = "CoolModule";
      phys_path = "./nice.kha";
      imports = [];
      opens = [];
      toplevel =
        [
          Type
            ( noid,
              {
                name = "MyRecord";
                args = [];
                expr = TRecord [ ("foo", TyInt); ("bar", TyBool) ];
              } );
          Definition
            ( noid,
              {
                name = "RecordTest";
                free_vars = [];
                constraints = [];
                args = [];
                ret = Custom (Base "MyRecord");
                body =
                  Record
                    ( noid,
                      Base "MyRecord",
                      [
                        ("foo", Int (noid, "5"));
                        ("bar", Bool (noid, true));
                      ] );
              } );
          Definition
            ( noid,
              {
                name = "ProjTest";
                free_vars = [];
                constraints = [];
                args = [ ("x", Custom (Base "MyRecord")) ];
                ret = TyInt;
                body = Project (noid, Var (noid, "x"), "foo");
              } );
        ];
    };
  ]

let driver () = example_files |> Front.Driver.do_frontend

let main () =
  Printexc.record_backtrace true;
  driver () |> function
  | Ok e ->
      print_endline "ok!";
      ok e
  | Error e ->
      print_endline "err :(";
      err' e
