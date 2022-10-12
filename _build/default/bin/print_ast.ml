open Ast

let swap f g x = g (f x)

let rec do_all f lst =
  match lst with
  | [] -> ()
  | x :: xs -> f x; do_all f xs

let rec pIt str ind =
  match ind with
  | 0 -> print_string str;
  | _ -> print_string " "; pIt str (ind - 1)

let rec loop f i =
  match i with
  | 0 -> f
  | _ -> f; loop f (i - 1)

let pI str ind =
  begin
    print_string "\n";
    pIt str ind
  end


let rec pIn str ind =
  match ind with
  | 0 -> print_string str;
  | _ -> print_string " "; pI str (ind - 1)


let string_of_const b =
  match b with
  | Int    (i) -> i
  | Float  (f) -> f
  | String (s) -> "\"" ^ s ^ "\""
  | Id     (i) -> i
  | True -> "True"
  | False -> "False"

let rec printUnOp (i:int) (u:unop) =
  begin
    match u with
    | UnOpRef -> print_string " (ref)"
    | UnOpDeref -> print_string " (deref)"
    | UnOpPos -> print_string " (pos)"
    | UnOpNeg -> print_string " (neg)"
  end


let printBinOp (i:int) (b:binop) =
  begin
    match b with
    | BinOpPlus -> print_string "(add)"
    | BinOpMinus -> print_string "(sub)"
    | BinOpMul -> print_string "(mul)"
    | BinOpDiv -> print_string "(div)"
  end


let rec printTypesig (t:typeSig) (i:int) =
  begin
    match t with
    | Ptr (l, a) -> begin
        print_string " (";
        loop (print_string "@") l;
        print_string (a ^ ") ")
      end
    | TSBase (a) -> print_string ("(" ^ a ^ ")")
    | Arrow (l, r) -> begin
        pI "(typesig " i;
        printTypesig l (i + 1);
        print_string " -> ";
        printTypesig r (i + 1);
        print_string ")"
      end
  end

let rec printArgs ind args =
  match args with
  | [] -> ()
  | (x :: xs) ->  print_string (" (" ^ x ^ ")");
                  printArgs ind xs
     

let rec printExpr ind ast =
  begin
    pI "(expr" ind;
    match ast with
    | Paren (e) -> begin
        pI "(paren" (ind + 1);
        printExpr (ind + 2) e;
        print_string ")"
      end
    | Base (b) -> begin
        pI ("(base " ^ string_of_const b ^ ")") (ind + 1)
      end
    | UnOp (u, e) -> begin
        pI ("(unop ") (ind + 1);
        do_all (printUnOp (ind + 2)) u;
        printExpr (ind + 2) e;
        print_string ")"
      end
    | BinOp (e1, b, e2) -> begin
        pI ("(binop ") (ind + 1);
        printBinOp (ind + 2) b;
        printExpr (ind + 2) e1;
        printExpr (ind + 2) e2;
      end
    | FuncCall (e, el) -> begin
        pI ("(funccall") (ind + 1);
        printExpr (ind + 2) e;
        do_all (printExpr (ind + 3)) el;
        print_string ")"
      end
  end;
  print_string ")"

let rec print_attrs attr =
  match attr with
  | [] -> ()
  | x :: xs ->
     begin
       let tmp =
         match x with
         | Inline(y) -> "(inline " ^ (string_of_bool y) ^ ")"
         | NoMangle(y) -> "(nomangle " ^ (string_of_bool y) ^ ")"
       in
       print_string tmp
     end;
     print_attrs xs

let rec printBlock ind ast =
  begin
    pI "(block" (ind - 1);
    match ast with
    | Many b -> do_all (printBlock (ind + 2)) b
    | AssignBlock (i, args, b) -> begin
        pI ("(assign" ^ i) ind;
        printArgs (ind + 1) args;
        printBlock (ind + 1) b;
        print_string ")"
      end
    | Assign (i, args, e)-> begin
        pI ("(assign " ^ i) ind;
        printArgs (ind + 1) args;
        printExpr (ind + 1) e;
        print_string ")"
      end
    | Typesig (i, t, a) -> begin
        pIn ("(typedec " ^ i ^ " (attrs ") ind;
        print_attrs a;
        print_string ")";
        printTypesig t (ind + 1);
        print_string ")"
      end
    | If (e) -> begin
        pI ("(if ") ind;
        printExpr (ind + 1) (fst e);
        printBlock (ind + 1) (snd e);
        print_string ")"
      end
    | While (e) -> begin
        pI ("(while ") ind;
        printExpr  (ind + 1) (fst e);
        printBlock (ind + 1) (snd e);
        print_string ")"
      end
    | Return (e) -> begin
        pI ("(return") ind;
        printExpr (ind + 1) e;
        print_string ")"
      end
  end;
  print_string ")"

let printToplevel ind ast =
  begin
  pI "(toplevel " ind;
  match ast with
  | AssignBlock (i, a, b) -> begin
      pI ("(assign " ^ i) ind;
      printArgs (ind + 1) a;
      printBlock (ind + 1) b;
      print_string ")"
    end
  | Assign (i, a, e)-> begin
      pI ("(assign " ^ i) ind;
      printArgs (ind + 1) a;
      printExpr (ind + 1) e;
      print_string ")"
    end
  | Typesig (i, t, a) -> begin
      pIn ("(typedec " ^ i ^ " (attrs ") ind;
      print_attrs a;
      print_string ")";
      printTypesig t (ind + 1);
      print_string ")"
    end
  end;
  print_string ")"

let printProgram ast ind = begin
    print_string "(program";
    match ast with
    | Prog (a) -> do_all (printToplevel (ind + 1)) a
  end;
  print_string ")\n"
let printAst ast =
  begin
    printProgram ast 0;
    print_endline ""
  end

    
(* Local Variables: *)
(* caml-annot-dir: "../_build/default/bin/.main.eobjs/byte" *)
(* End: *)
