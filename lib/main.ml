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
        ];
    };
  ]

let driver () =
  example_files
  |> Front.Driver.do_frontend
  |$> failwith "todo: middleend-y things"

let main () =
  Printexc.record_backtrace true;
  driver ()
