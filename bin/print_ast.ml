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
  | String (s) -> s
  | Id     (i) -> i


let printUnOp (i:int) (b:unop) = print_string "TEMP1"

let printBinOp (i:int) (b:binop) = print_string "TEM2"

let rec printTypesig (t:typeSig) (i:int) =
  begin
    match t with
    | Base (a) -> print_string (" (" ^ a ^ ")")
    | Arrow (l, r) -> begin
        pI "(typesig" i;
        printTypesig l (i + 1);
        print_string " -> ";
        printTypesig r (i + 1);
        print_string ")"
      end
  end

     

let rec printExpr ind ast =
  begin
    pI "(expr" ind;
    match ast with
    | Paren (e) -> begin
        pI "(paren " ind;
        printExpr (ind + 1) ast;
        print_string ")"
      end
    | Base (b) -> begin
        pI ("(base " ^ string_of_const b ^ ")") ind
      end
    | UnOp (u, e) -> begin
        pI ("(unop ") ind;
        do_all (printUnOp (ind + 1)) u;
        printExpr (ind + 1) e;
        print_string ")"
      end
    | BinOp (e1, b, e2) -> begin
        pI ("(binop ") ind;
        printBinOp (ind + 1) b;
        printExpr (ind + 1) e1;
        printExpr (ind + 1) e2;
      end
    | FuncCall (e, el) -> begin
        pI ("(funccall") ind;
        printExpr (ind + 1) e;
        do_all (printExpr (ind + 2)) el;
        print_string ")"
      end
  end;
  print_string ")"


let rec printBlock ind ast =
  begin
    pI "(block" ind;
    match ast with
    | Many b -> do_all (printBlock (ind + 3)) b
    | AssignBlock (i, b) -> begin
        pI ("(assign" ^ i) ind;
        printBlock (ind + 1) b;
        print_string ")"
      end
    | Assign (i, e)-> begin
        pI ("(assign " ^ i) ind;
        printExpr (ind + 1) e;
        print_string ")"
      end
    | Typesig (i, t) -> begin
        pIn ("(typedec " ^ i) ind;
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
  | AssignBlock (i, b) -> begin
      pI ("(assign " ^ i) ind;
      printBlock (ind + 1) b;
      print_string ")"
    end
  | Assign (i, e)-> begin
      pI ("(assign " ^ i) ind;
      printExpr (ind + 1) e;
      print_string ")"
    end
  | Typesig (i, t) -> begin
      pI ("typedec " ^ i) ind;
      printTypesig t (ind + 1);
      print_string ")"
    end
  end;
  print_string ")"

let printProgram ast ind = begin
    print_string "(program";
    match ast with
    | Prog (a) -> do_all (printToplevel (ind + 1)) a
    | _ -> print_string "impossible"

  end;
  print_string ")\n"
let printAst ast = printProgram ast 0
(* Local Variables: *)
(* caml-annot-dir: "../_build/default/bin/.main.eobjs/byte" *)
(* End: *)
