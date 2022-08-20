open Ast
open Kenv
open Print_ast
let rec do_all f a = List.iter

let varNotFound ctx i =
  print_endline ("Variable " ^ i ^ " not found")

let rec getArgs args (ts:typeSig) =
  match args with
  | [] -> []
  | (x :: xs) ->
     match typeSigLeft ts with
     | None -> print_endline "Malformed typesig";
               printTypesig ts;
               exit 1
     | Some(tl) ->
        begin
          match typeSigRight ts with
          | None -> print_endline "Malformed typesig";
                    printTypesig ts;
                    exit 1
          | Some(tr) ->
             {id=x; ts=tl} :: getArgs xs tr
        end


let typecheckAssign ast ctx =
  match ast with
  | Assign (i, args, e) ->
     begin
       match findVar ctx i with
       | None -> varNotFound i; exit 1
       | Some (x) ->
          let newctx = {parent=Some(ctx);inherit_ctx=true;
                        ts=Some(x.ts);vars=[];args=getArgs args x.ts} in
          typecheckExpr e newctx
     end
  | AssignBlock (i, args, b) ->
     addArgs args ctx;
     typecheckBlock b

  

let typecheckToplevel ast ctx =
  let newctx =
    match ctx with
    | None -> {parent=None; inherit_ctx=false;
               ts=None; vars=[]; args=[]}
    | Some (x) -> x
  in
  begin
    match ast with
    | [] -> ()
    | (x :: xs) ->
       begin
         match x with
         | Typesig (i, t) -> addVar newctx {id=i; ts=t;}
         | _ -> typecheckAssign x newctx
       end;
       typecheckToplevel xs newctx
  end
let typecheckAst ast =
  typecheckToplevel ast None
