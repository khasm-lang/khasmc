open Front.Ast
open Common.Info

let example_files =
  [
    {
      name = "CoolModule";
      phys_path = "./nice.kha";
      imports = [];
      opens = [];
      toplevel =
        [
          Trait
            ( noid,
              {
                name = "Trt";
                args = [ "a" ];
                constraints = [];
                assoc_types = [];
                functions =
                  [
                    {
                      name = "Foo";
                      free_vars = [];
                      constraints = [];
                      args = [ ("x", TyInt) ];
                      ret = Free "a";
                    };
                  ];
              } );
          Definition
            ( noid,
              {
                name = "foo";
                free_vars = [];
                constraints = [];
                args = [];
                ret = TyInt;
                body =
                  App
                    ( noid,
                      Bound (noid, InMod ("Trt", Base "Foo")),
                      [ Int (noid, "5") ] );
              } );
        ];
    };
  ]

let main () =
  Printexc.record_backtrace true;
  example_files |> Front.Driver.do_frontend |> failwith "todo!"
