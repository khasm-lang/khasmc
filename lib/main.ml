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
          Type
            ( noid,
              {
                name = Base "Ty";
                args = [];
                expr = TVariant [ ("Constr", [ TyInt ]) ];
              } );
          Definition
            ( noid,
              {
                name = Base "id";
                free_vars = [ "a" ];
                constraints = [];
                args = [ ("x", Free "a") ];
                ret = Free "a";
                body = Var (noid, "x");
              } );
          Definition
            ( noid,
              {
                name = Base "call";
                free_vars = [];
                constraints = [];
                args = [ ("w", TyInt) ];
                ret = TyInt;
                body =
                  App (noid, Var (noid, "id"), [ Var (noid, "w") ]);
              } );
          Definition
            ( noid,
              {
                name = Base "test!";
                free_vars = [];
                constraints = [];
                args = [];
                ret = Custom (Base "Ty");
                body = Bound (noid, InMod ("Ty", Base "Constr"));
              } );
        ];
    };
  ]

let main () =
  Printexc.record_backtrace true;
  example_files |> Front.Driver.do_frontend |> failwith "todo!"
