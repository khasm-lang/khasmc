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
          Trait
            {
              name = "Show";
              args = [ "a" ];
              assoc_types = [];
              constraints = [];
              functions =
                [
                  {
                    name = "show";
                    free_vars = [];
                    constraints = [];
                    args = [ ("dummy", Free "a") ];
                    ret = TyString;
                    id = id' ();
                  };
                ];
              id = id' ();
            };
          Impl
            {
              name = "Show";
              args = [ ("a", TyInt) ];
              assoc_types = [];
              impls =
                [
                  {
                    name = "show";
                    free_vars = [];
                    constraints = [];
                    args = [ ("dummy", TyInt) ];
                    ret = TyInt;
                    body = Var (id' (), "MAGIC");
                    id = id' ();
                  };
                ];
              id = id' ();
            };
        ];
    };
  ]

let driver () = example_files |> Front.Driver.do_frontend

let main () =
  Common.Log.init_log ();
  Printexc.record_backtrace true;
  ignore
  @@ begin
       match driver () with
       | Ok e ->
           print_endline "ok!";
           ok e
       | Error e ->
           Common.Log.error "error";
           print_endline "err :(";
           err' e
       | exception e ->
           Common.Log.error "exception!";
           print_endline (Printexc.to_string e);
           Printexc.print_backtrace stdout;
           err' "exn"
     end;
  Common.Log.print_log ();
  Front.Tycheck.print_types ()
