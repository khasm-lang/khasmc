%{
    open Ast

    

let unOp x y = FCall(mkinfo(),
		     Base(mkinfo(),
			  Ident(mkinfo(),
				x)), y)
    let binOp x o z = FCall(mkinfo(),
			    FCall(mkinfo(),
				  Base(mkinfo(),
				       Ident(mkinfo(),
					     o)), x) , z )


    
%}

%token <string> T_IDENT
%token <string> T_FIDENT
%token <string> T_INT
%token <string> T_FLOAT
%token <string> T_STRING
%token <string> INTIDENT
%token INTEXTERN
%token TRUE
%token FALSE
%token ADD
%token SUB
%token MUL
%token SLASH
%token BSLASH
%token STRAIGHT
%token AND
%token DOT
%token PERCENT

%token AT
%token HASH
%token GT
%token LT
%token COMMA
%token BANG
%token EQ
%token DOLLAR
%token QMARK
%token IF
%token OF
%token THEN
%token ELSE
%token WHILE

%token FOR
%token RETURN
%token IN
%token LET
%token REC
%token COLON
%token SEMICOLON
%token EOF
%token TS_TO
%token LAM_TO
%token IGNORE

%token FORALL
%token SIG
%token TILDE
%token FUN
%token TFUN
%token END

%token<string> BANG_OP
%token<string> TILDE_OP

%token<string> POW_OP

%token<string> MUL_OP
%token<string> DIV_OP
%token<string> MOD_OP

%token<string> ADD_OP
%token<string> SUB_OP

%token<string> COL_OP

%token<string> CAR_OP
%token<string> AT_OP

%token<string> EQ_OP
%token<string> LT_OP
%token<string> GT_OP
%token<string> PIP_OP
%token<string> AND_OP
%token<string> DOL_OP

%token MODULE
%token STRUCT
%token FUNCTOR

%token BIND

%token LAND
%token LOR

%token NOMANGLE
%token INLINE
%token EXTERN

%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token LPAREN
%token RPAREN

%type<Ast.typesig> typesig
%type<Ast.typesig> typesig_i
%type<Ast.kident list> nonempty_list(T_IDENT)
%type<Ast.program> program
%type<Ast.kbase> base
%type<Ast.kexpr> expr
%type<Ast.kexpr> fexpr
%type<Ast.kident list> list(T_IDENT)
%type<Ast.kexpr list> nonempty_list(parenexpr)
%type<Ast.toplevel list> nonempty_list(toplevel)
%type<Ast.kexpr> parenexpr
%type<Ast.toplevel> toplevel

%right TS_TO


%right SEMICOLON
%left DOL_OP
%left EQ_OP GT_OP LT_OP PIP_OP AND_OP
%right AT_OP CAR_OP
%right COL_OP
%left ADD_OP SUB_OP
%left MUL_OP DIV_OP MOD_OP
%right POW_OP
%nonassoc TILDE_OP BANG_OP
%left DOT


%right KTYPE

%start program

%%

ktype:
  | t = T_IDENT
    {
      if t = "()" then
	TSBottom
      else
	TSBase(t)
    }
  | a = ktype; b = T_IDENT {TSApp(a, b)}
  | LPAREN; a = typesig; RPAREN; b = T_IDENT
    {TSApp(a, b)}
  | LPAREN; t = separated_nonempty_list(COMMA, typesig); RPAREN; b = T_IDENT
    {TSApp(TSTuple(t), b)}

typesig_i:
  | k = ktype {k}
  | a = typesig_i; TS_TO; b = typesig_i {TSMap(a, b)}
  | LPAREN; t = typesig; RPAREN; {t}
  | LPAREN; t = separated_nonempty_list(COMMA, typesig); RPAREN
    {TSTuple(t)}
  | t1=typesig_i; MUL_OP; t2=typesig_i
    {
      match (t1, t2) with
      | (TSTuple(x), TSTuple(y)) -> TSTuple(x @ y)
      | (TSTuple(x), _) -> TSTuple(x @ [t2])
      | (_, TSTuple(y)) -> TSTuple([t1] @ y)
      | (_, _) -> TSTuple([t1] @ [t2])
    }

typesig:
  | t = typesig_i {t}
  | FORALL; f = nonempty_list(T_IDENT); COMMA; a = typesig
    {
      let rec make sl a =
	match sl with
	| [] -> failwith "Impossible"
        | [x] -> TSForall(x, a)
        | x :: xs -> TSForall(x, make xs a)
      in
      make f a
    }

base:
  | t = T_IDENT
    {
      Ident(mkinfo(), t)
	   (*FIDENT SUPPORT NEEDED HERE*)
    }
  | t = T_INT   {Int(t)}
  | t = T_FLOAT {Float(t)}
  | t = T_STRING {Str(t)}
  | TRUE  {True}
  | FALSE {False}
  | LPAREN; s=separated_nonempty_list(COMMA, expr); RPAREN {Tuple(s)}
  | LPAREN; t = letid_h; RPAREN {Ident(mkinfo(), t)}

letid:
  | t = T_IDENT {t}
  | LPAREN; t = letid_h; RPAREN; {t}

letid_h:
  | BANG_OP {$1}
  | POW_OP {$1}
  | TILDE_OP {$1}
  | MUL_OP {$1}
  | DIV_OP {$1}
  | MOD_OP {$1}
  | ADD_OP {$1}
  | SUB_OP {$1}
  | COL_OP {$1}
  | CAR_OP {$1}
  | AT_OP {$1}
  | EQ_OP {$1}
  | LT_OP {$1}
  | GT_OP {$1}
  | PIP_OP {$1}
  | AND_OP {$1}
  | DOL_OP {$1}


parenexpr:
  | b = base {Base(mkinfo(), b)}
  | LPAREN; e = expr RPAREN {e}

fexpr:
  | LPAREN; e = expr; RPAREN {e}
  | m = module_acc {m}
  | t = letid {Base(mkinfo(), Ident(mkinfo(), t))}

mod_acc_h:
  | DOT; m=letid {m}

module_acc:
  | i = T_IDENT; m = nonempty_list(mod_acc_h);
    {
      let rev = List.rev m in
      match rev with
      | [] -> failwith "impossible"
      | [x] -> ModAccess(mkinfo(), [i], x)
      | x :: xs ->
	 let rest = i :: List.rev xs in
	 ModAccess(mkinfo(), rest, x)
    }


expr:
  | e=expr1 {e}

expr1:
  | b=BANG_OP; e=expr {unOp b e} %prec BANG_OP
  | t=TILDE_OP; e=expr {unOp t e} %prec TILDE_OP
  | m=module_acc {m}
  | e=expr2 {e}

expr2:
  | f=fexpr; e=nonempty_list(parenexpr)
    {
      let rec tmp x y =
	match y with
	| [] -> failwith "parsing failure expr2"
        | [k] -> FCall(mkinfo(), x, k)
        | k :: ks -> FCall(mkinfo(), tmp x ks, k)
      in
      tmp f (List.rev e)
    }
  | e=expr3 {e}

expr3:
  | e1=expr; p=POW_OP; e2=expr {binOp e1  p e2}
  | e=expr4 {e}

expr4:
  | e1=expr; p=MUL_OP; e2=expr {binOp e1 p e2}
  | e1=expr; d=DIV_OP; e2=expr {binOp e1 d e2}
  | e1=expr; m=MOD_OP; e2=expr {binOp e1 m e2}
  | e=expr5 {e}

expr5:
  | e1=expr; a=ADD_OP; e2=expr {binOp e1 a e2}
  | e1=expr; a=SUB_OP; e2=expr {binOp e1 a e2}
  | e=expr6 {e}

expr6:
  | e1=expr; a=COL_OP; e2=expr {binOp e1 a e2}
  | e=expr7 {e}

expr7:
  | e1=expr; a=AT_OP; e2=expr {binOp e1 a e2}
  | e1=expr; a=CAR_OP; e2=expr {binOp e1 a e2}
  | e=expr8 {e}

expr8:
  | e1=expr; a=EQ_OP; e2=expr {binOp e1 a e2}
  | e1=expr; a=LT_OP; e2=expr {binOp e1 a e2}
  | e1=expr; a=GT_OP; e2=expr {binOp e1 a e2}
  | e1=expr; a=PIP_OP; e2=expr {binOp e1 a e2}
  | e1=expr; a=AND_OP; e2=expr {binOp e1 a e2}
  | e1=expr; a=DOL_OP; e2=expr {binOp e1 a e2}
  | e=expr9 {e}

expr9:

  | e=expr10 {e}

expr10:
  | IF; e1=expr; THEN; e2=expr; ELSE; e3=expr {IfElse
						 (mkinfo(), e1, e2, e3)}
  | LET; t=letid; args=list(T_IDENT); EQ_OP; e1=expr; IN; e2=expr
    {
      let rec tmp x y =
	match x with
	| [] -> y
	| x :: xs -> Lam(mkinfo(), x, tmp xs y)
      in
      LetIn(mkinfo(), t, tmp args e1, e2)
    }
  | LET; i=T_IDENT; args=list(T_IDENT); COL_OP; t=typesig;
    EQ_OP; e1=expr; IN; e2=expr;
    {
      let rec tmp x y =
	match x with
	| [] -> y
	| x :: xs -> Lam(mkinfo(), x, tmp xs y)
      in
      AnnotLet(mkinfo(), i, t, tmp args e1, e2)
    }
  | FUN; a=T_IDENT; COL_OP; t=typesig; LAM_TO; e=expr
    {
      let rec helper ts id ex =
	match ts with
	| TSForall(fv, bd) -> TypeLam(mkinfo(), fv, helper bd id ex)
        | _ -> AnnotLam(mkinfo(), id, ts, ex)
      in
      helper t a e 
    }
  | TFUN; a=T_IDENT; COL_OP; t=typesig; LAM_TO; e=expr
    {
      AnnotLam(mkinfo(), a, t, e)
    }
  | TFUN; a=T_IDENT; LAM_TO; e=expr
    {
      TypeLam(mkinfo(), a, e)
    }
  | LPAREN; e=expr; RPAREN {e}
  | e=expr; LBRACK; t=T_INT; RBRACK {TupAccess(mkinfo(), e, int_of_string t)}
  | e=expr11 {e}

expr11:
  | e1=expr; SEMICOLON; e2=expr {Join(mkinfo(), e1, e2)}
  | e=expr12 {e}

expr12:
  | b=base {Base(mkinfo(), b)}

bind:
  | BIND; l=letid; EQ_OP; e = T_IDENT; {Bind(l, [], e)}

module_decl:
  | MODULE; s=T_IDENT; EQ_OP;
    STRUCT; b=nonempty_list(toplevel); END {SimplModule(s, b)}

siglet:
  | LET; b = T_IDENT; args=list(T_IDENT); col=COL_OP; t=typesig; eq=EQ_OP; e=expr
    { TopAssign((b, t), (b, args, e)) }
  | LET; REC; b=T_IDENT; args=list(T_IDENT); COL_OP; t=typesig; EQ_OP; e=expr {TopAssignRec((b, t), (b, args, e))}

toplevel:
  | a = module_decl {a}
  | a = siglet {a}
  | a = bind {a}
  | EXTERN; a=T_IDENT; COL_OP; t=typesig; {Extern(a, t)}
  | INTEXTERN; a=INTIDENT; EQ_OP; b=T_IDENT; COL_OP; t=typesig {IntExtern(a, b, t)} 

program:
  | a = nonempty_list(toplevel); EOF {Program(a)}
