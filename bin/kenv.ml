
open Ast

type context_var = {
    id : string;
    typesig : typeSig;
    scope : int;
  }

type context = {
    parent: context option;
    vars: context_var list;
  }


let addVar ctx ctxvar = {
    ctx with
    vars = ctxvar :: ctx.vars 
  }

let findVar ctx id =
  begin
    try 
      let id = List.find (fun v -> v.id = id) ctx.vars in
      Some(id)
    with Not_found -> None
  end

