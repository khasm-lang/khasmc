open Ast
open Kenv
open Print_ast
let rec do_all = List.iter

let varNotFound i =
  print_endline ("Variable " ^ i ^ " not found")

let wrongType t1 t2 ctx =
  print_endline "Type sig";
  printTypesig t1 0;
  print_endline "does not match expected";
  printTypesig t2 0

let rec getLastTs t args =
  match args with
  | [] ->
     print_endline "UNREACHABLE getLastTs empty list";
     exit 1
  | [x] ->
     begin
       match typeSigRight t with
       | None -> print_endline "Malformed typesig right getlast []";
                 print_endline "Maybe typesig too short for args?";
                 do_all (print_endline) args;
                 printTypesig t 0;
                 exit 1
       | Some(t) -> t
     end
  | (x :: xs) ->
     begin
       match typeSigRight t with
       | None -> print_endline "Malformed typesig right getlast";
                 print_endline "Maybe typesig too short for args?";
                 do_all (print_endline) args;
                 printTypesig t 0;
                 exit 1
       | Some(t) -> getLastTs t xs 
     end


let rec getArgs args (ts:typeSig) =
  match args with
  | [] -> []
  | (x :: xs) ->
     match typeSigLeft ts with
     | None -> print_endline "Malformed typesig left";
               print_endline "Maybe typesig is short for args?";
               do_all (print_endline) args;
               printTypesig ts 0;
               exit 1
     | Some(tl) ->
        begin
          match typeSigRight ts with
          | None -> print_endline "Malformed typesig right";
                    print_endline "Maybe typesig is long for args?";
                    do_all (print_endline) args;
                    printTypesig ts 0;
                    exit 1
          | Some(tr) ->
             {id=x; ts=tl} :: getArgs xs tr
        end

let rec unifyUnary x ts =
  match x with
  | UnOpRef ->
     begin
       match ts with
       | Arrow (_,_) ->
          begin
            print_endline "Cannot reference a lambda";
            printTypesig ts 0;
            printUnOp 0 x;
            exit 1
          end
       | TSBase (x) -> Ptr(1, x)
       | Ptr (i, x) -> Ptr(i + 1, x)
     end
  | UnOpDeref ->
     begin
       match ts with
       | Arrow (_,_) ->
          begin
            print_endline "Cannot dereference a lambda";
            printTypesig ts 0;
            printUnOp 0 x;
            exit 1
          end
       | TSBase (z) ->
          begin
            print_endline "Cannot dereference base type";
            print_endline z;
            printUnOp 0 x;
            exit 1
          end
       | Ptr(i, x) ->
          begin
            match i with
            | 1 -> TSBase(x)
            | y -> Ptr(y - 1, x)
          end
     end
  | UnOpPos ->
     begin
       match ts with
       | Arrow (_,_) ->
          begin
            print_endline "Cannot force positive a lambda";
            printTypesig ts 0;
            printUnOp 0 x;
            exit 1
          end
       | Ptr (_,_) ->
          begin
            print_endline "Cannot force positive a pointer";
            printTypesig ts 0;
            printUnOp 0 x;
            exit 1
          end
       | TSBase (x) -> TSBase (x)
     end
  | UnOpNeg -> begin
       match ts with
       | Arrow (_,_) ->
          begin
            print_endline "Cannot negate a lambda";
            printTypesig ts 0;
            printUnOp 0 x;
            exit 1
          end
       | Ptr (_,_) ->
          begin
            print_endline "Cannot negate a pointer";
            printTypesig ts 0;
            printUnOp 0 x;
            exit 1
          end
       | TSBase (x) -> TSBase (x)
    end

and typecheckUnary ul x ctx =
  begin
    match (ul, x) with
    | ([y], x) ->
       begin
         let xt = typecheckExpr x ctx in
         unifyUnary y xt
       end
    | (y :: ys, x) ->
       begin
         let rest = typecheckUnary ys x ctx in
         let res = unifyUnary y rest in
         res
       end
    | ([], _) ->
       begin
         print_endline "UNREACHABLE: typecheckUnary";
         exit 1
       end
  end

and typecheckBinOp binop e1 e2 ctx =
  begin
    let t1 = typecheckExpr e1 ctx in
    let t2 = typecheckExpr e2 ctx in
    match binop with
    | BinOpPlus | BinOpMinus | BinOpMul | BinOpDiv ->
       begin
       if (typeSigEq t1 t2) != true then
         begin
           print_endline "Invalid types for expr:";
           printTypesig t1 0;
           print_endline "and";
           printTypesig t2 0;
           exit 1
         end;
       t1
       end
    end
    



and getTypeConst x ctx =
  match x with
  | Int z -> TSBase("i32")
  | Float z -> TSBase("f32")
  | String z -> TSBase("str")
  | True -> TSBase("bool")
  | False -> TSBase("bool")
  | Id z -> let v = findVar ctx z in
            match v with
            | None -> varNotFound z;
                      exit 1
            | Some z -> z.ts

and getLastArgs ts args =
  match args with
  | [] -> begin print_endline "UNREACHABLE"; exit 1 end
  | [x] ->
     begin
       match typeSigRight ts with
       | None ->
          begin
            print_endline "Invalid type sig for args";
            printTypesig ts 0;
            do_all (printExpr 0) args;
            exit 1
          end
       | Some (tsr) -> tsr
     end
  | x :: xs ->
     begin
       match typeSigRight ts with
       | None ->
          begin
            print_endline "Invalid type sig for args";
            printTypesig ts 0;
            do_all (printExpr 0) args;
            exit 1
          end
       | Some (tsr) ->
          begin
            getLastArgs tsr xs
          end
     end

and unifyTypesig t1 t2 =
  if (typeSigEq t1 t2) != true then
    begin
      print_endline "Typesigs";
      printTypesig t1 0;
      printTypesig t2 0;
      print_endline "do not match, and need to"
    end
    

and typecheckArgs ts args ctx =
  match args with
  | [] -> ()
  | [x] ->
     begin
       match (typeSigLeft ts) with
       | None ->
          begin
            print_endline "Invalid typesig for args";
            printTypesig ts 0;
            do_all (printExpr 0) args;
            exit 1
          end
       | Some (tl) ->
          begin
            unifyTypesig tl (typecheckExpr x ctx)
          end
     end;
     ()
  | x :: xs ->
     begin
       let lefto = typeSigLeft ts in
       let righto = typeSigRight ts in
       match (lefto, righto) with
       | (None, _) | (_, None) ->
          begin
            print_endline "Invalid typesig for args";
            printTypesig ts 0;
            do_all (printExpr 0) args;
            exit 1
          end
       | (Some (tl), Some (tr)) ->
          begin
            unifyTypesig tl (typecheckExpr x ctx);
            typecheckArgs tr xs ctx
          end
     end;
     ()

and typecheckFuncCall func args ctx =
  match func with
  | Base x ->
     begin
       match x with
       | Id id ->
          begin
            let tso = findVar ctx id  in
            match tso with
            | None ->
               begin
                 print_endline ("can't find variable" ^ (id));
                 exit 1
               end
            | Some (ts) ->
               begin
                 typecheckArgs ts.ts args ctx
               end;
               getLastArgs ts.ts args
          end
       | _ ->
          begin
            print_endline "Cannot call";
            print_endline (string_of_const x);
            print_endline "as function";
            exit 1
          end
     end
  | _ ->
     begin
       print_endline "not implemented typecheckFuncCall";
       exit 1
     end

and typecheckExpr ast ctx =
  match ast with
  | Paren x -> typecheckExpr x ctx
  | Base x -> getTypeConst x ctx
  | UnOp (ul, x) -> typecheckUnary ul x ctx
  | BinOp (e1, b, e2) -> typecheckBinOp b e1 e2 ctx
  | FuncCall (e, el) -> typecheckFuncCall e el ctx

let typecheckAssign ast ctx =
  match ast with
  | Assign (i, args, e) ->
     begin
       match findVar ctx i with
       | None -> varNotFound i; exit 1
       | Some (x) ->
          let newctx = {
              parent=Some(ctx);
              inherit_ctx=true;
              ts=Some(x.ts);vars=[];
              args=getArgs args x.ts} in
          let typeOfExpr = typecheckExpr e newctx in
          begin
            if typeSigEq (getLastTs x.ts args) typeOfExpr != true then
              begin
                wrongType typeOfExpr (getLastTs x.ts args) ctx;
                exit 1
              end
          end
     end; ()
  | AssignBlock (i, args, b) ->
     begin
       print_endline "NOTIMPL";
       exit 1
     end
  | _ ->
     begin
       print_endline "UNREACHABLE typecheckAssign";
       exit 1
     end



let rec typecheckToplevel ast ctx =
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
         | Typesig (i, t) ->
            begin
              let newerctx = addVar newctx {id=i; ts=t;} in
              typecheckToplevel xs (Some(newerctx))
            end
         | Assign (i, a, e) ->
            typecheckAssign (Assign(i, a, e)) newctx;
            typecheckToplevel xs (Some(newctx))
         | AssignBlock (i, a, b) ->
            typecheckAssign (AssignBlock(i,a,b)) newctx;
            typecheckToplevel xs (Some(newctx))
       end;
  end;
  ()
let typecheckAst ast =
  typecheckToplevel ast None;
  Printf.printf "\ntypechecking: ✓\n"
