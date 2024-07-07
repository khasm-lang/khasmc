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
          Definition
            {
              id = id' ();
              name = "id";
              constraints = [];
              free_vars = [ "a" ];
              args = [ ("x", Free "a") ];
              ret = Free "a";
              body = Var (id' (), "x");
            };
          Definition
            {
              id = id' ();
              name = "integer";
              constraints = [];
              free_vars = [ "a"; "b" ];
              args = [];
              ret = Arrow (Free "b", Free "b");
              body =
                App
                  (id' (), Var (id' (), "id"), [ Var (id' (), "id") ]);
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
           print_endline "exception!";
           err' "exn"
     end;
  Common.Log.print_log ();
  Front.Tycheck.print_types ()
