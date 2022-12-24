%{
    open Ast
    let unOp x y = FCall(Base(Ident(x)), y)
    let binOp x o z = FCall(FCall(Base(Ident(o)), x) , z )
%}

%token <string> T_IDENT
%token <string> T_FIDENT
%token <string> T_INT
%token <string> T_FLOAT
%token <string> T_STRING
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


%token LAND
%token LOR

%token NOMANGLE
%token INLINE

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
%left EQ_OP GT_OP LT_OP PIP_OP AND_OP DOL_OP
%right AT_OP CAR_OP
%right COL_OP
%left ADD_OP SUB_OP
%left MUL_OP DIV_OP MOD_OP
%right POW_OP
%nonassoc TILDE_OP BANG_OP

%right KTYPE

%start program

%%

ktype:
  | LPAREN; RPAREN {TSBottom}
  | t = T_IDENT {TSBase(t)}
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
  | FORALL; f = nonempty_list(T_IDENT); COMMA; a = typesig_i
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
      Ident(t)
	   (*FIDENT SUPPORT NEEDED HERE*)
    }
  | t = T_INT   {Int(t)}
  | t = T_FLOAT {Float(t)}
  | t = T_STRING {Str(t)}
  | TRUE  {True}
  | FALSE {False}

parenexpr:
  | b = base {Base(b)}
  | LPAREN; e = expr RPAREN {e}

fexpr:
  | LPAREN; e = expr; RPAREN {e}
  | t = T_IDENT {Base(Ident(t))}

expr:
  | e=expr1 {e}

expr1:
  | b=BANG_OP; e=expr {unOp b e} %prec BANG_OP
  | t=TILDE_OP; e=expr {unOp t e} %prec TILDE_OP
  | e=expr2 {e}

expr2:
  | f=fexpr; e=nonempty_list(parenexpr)
    {
      let rec tmp x y =
	match y with
	| [] -> failwith "parsing failure expr2"
        | [k] -> FCall(x, k)
        | k :: ks -> FCall(tmp x ks, k)
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
  | LPAREN; s=separated_nonempty_list(COMMA, expr); RPAREN {Base(Tuple(s))}
  | e=expr10 {e}

expr10:
  | IF; e1=expr; THEN; e2=expr; ELSE; e3=expr {IfElse(e1, e2, e3)}
  | LET; t=T_IDENT; args=list(T_IDENT); EQ_OP; e1=expr; IN; e2=expr
    {
      let rec tmp x y =
	match x with
	| [] -> y
	| x :: xs -> Lam(x, tmp xs y)
      in
      LetIn(t, tmp args e1, e2)
    }
  | LET; i=T_IDENT; args=list(T_IDENT); COL_OP; t=typesig;
    EQ_OP; e1=expr; IN; e2=expr;
    {
      let rec tmp x y =
	match x with
	| [] -> y
	| x :: xs -> Lam(x, tmp xs y)
      in
      AnnotLet(i, t, tmp args e1, e2)
    }
  | FUN; a=T_IDENT; COL_OP; t=typesig; LAM_TO; e=expr
    {
      let rec helper ts id ex =
	match ts with
	| TSForall(fv, bd) -> TypeLam(fv, helper bd id ex)
        | _ -> AnnotLam(id, ts, ex)
      in
      helper t a e
    }
  | LPAREN; e=expr; RPAREN {e}
  | e=expr; DOT; t=T_INT {TupAccess(e, int_of_string t)}
  | e=expr11 {e}

expr11:
  | e1=expr; SEMICOLON; e2=expr {Join(e1, e2)}
  | e=expr12 {e}

expr12:
  | b=base {Base(b)}

siglet:
  | SIG; t = typesig; IN; LET; b = T_IDENT; args=list(T_IDENT); eq=EQ_OP; e = expr
    {
      TopAssign((b, t),(b, args, e))
    }

toplevel:
  | a = siglet {a}

program:
  | a = nonempty_list(toplevel); EOF {Program(a)}
