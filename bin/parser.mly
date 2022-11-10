%{
    open Ast
    exception Parse_error of string
%}

%token <string> T_IDENT
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
%token IGNORE
%token FORALL
%token SIG
%token TILDE

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
%type<Ast.ktype> ktype
%type<Ast.kident list> nonempty_list(T_IDENT)
%type<Ast.program> program
%type<Ast.kass> assign
%type<Ast.kbase> base
%type<Ast.kexpr> expr
%type<Ast.kexpr> fexpr
%type<Ast.kident list> list(T_IDENT)
%type<Ast.kexpr list> nonempty_list(parenexpr)
%type<Ast.toplevel list> nonempty_list(toplevel)
%type<Ast.kexpr> parenexpr
%type<Ast.tdecl> tdecl
%type<Ast.toplevel> toplevel

%right TS_TO


%right SEMICOLON
%left EQ_OP GT_OP LT_OP PIP_OP AND_OP DOL_OP
%right AT_OP CAR_OP
%right COL_OP
%left ADD_OP SUB_OP
%left MUL_OP DIV_OP MOD_OP
%right POW_OP

%start program

%%

ktype:
  | t = T_IDENT {KTypeBasic(t)}

typesig_i:
  | k = ktype {TSBase(k)}
  | a = typesig_i; TS_TO; b = typesig_i {TSMap(a, b)}
  | LPAREN; t = typesig; RPAREN; {t}

typesig:
  | t = typesig_i {t}
  | FORALL; f = nonempty_list(T_IDENT); COMMA; a = typesig_i
    {TSForall(f, a)}

base:
  | t = T_IDENT {Ident(t)}
  | t = T_INT   {Int(t)}
  | t = T_FLOAT {Float(t)}
  | t = T_STRING {Str(t)}

parenexpr:
  | b = base {Base(b)}
  | LPAREN; e = expr RPAREN {e}

fexpr:
  | LPAREN; e = expr; RPAREN {e}
  | t = T_IDENT {Base(Ident(t))}

expr:
  | e=expr1 {e}

expr1:
  | b=BANG_OP; e=expr {UnOp(b, e)}
  | t=TILDE_OP; e=expr {UnOp(t, e)}
  | e=expr2 {e}

expr2:
  | f=fexpr; e=nonempty_list(parenexpr) {FCall(f, e)}
  | e=expr3 {e}

expr3:
  | e1=expr; p=POW_OP; e2=expr {BinOp(e1, p, e2)}
  | e=expr4 {e}

expr4:
  | e1=expr; p=MUL_OP; e2=expr {BinOp(e1, p, e2)}
  | e1=expr; d=DIV_OP; e2=expr {BinOp(e1, d, e2)}
  | e1=expr; m=MOD_OP; e2=expr {BinOp(e1, m, e2)}
  | e=expr5 {e}

expr5:
  | e1=expr; a=ADD_OP; e2=expr {BinOp(e1, a, e2)}
  | e1=expr; a=SUB_OP; e2=expr {BinOp(e1, a, e2)}
  | e=expr6 {e}

expr6:
  | e1=expr; a=COL_OP; e2=expr {BinOp(e1, a, e2)}
  | e=expr7 {e}

expr7:
  | e1=expr; a=AT_OP; e2=expr {BinOp(e1, a, e2)}
  | e1=expr; a=CAR_OP; e2=expr {BinOp(e1, a, e2)}
  | e=expr8 {e}

expr8:
  | e1=expr; a=EQ_OP; e2=expr {BinOp(e1, a, e2)}
  | e1=expr; a=LT_OP; e2=expr {BinOp(e1, a, e2)}
  | e1=expr; a=GT_OP; e2=expr {BinOp(e1, a, e2)}
  | e1=expr; a=PIP_OP; e2=expr {BinOp(e1, a, e2)}
  | e1=expr; a=AND_OP; e2=expr {BinOp(e1, a, e2)}
  | e1=expr; a=DOL_OP; e2=expr {BinOp(e1, a, e2)}
  | e=expr9 {e}

expr9:
  | LPAREN; s=separated_nonempty_list(COMMA, expr); RPAREN {Base(Tuple(s))}
  | e=expr10 {e}

expr10:
  | IF; e1=expr; THEN; e2=expr; ELSE; e3=expr {IfElse(e1, e2, e3)}
  | LET; t=T_IDENT; args=list(T_IDENT); EQ_OP; e1=expr; IN; e2=expr
    {LetIn(t, args, e1, e2)}
  | e=expr11 {e}

expr11:
  | e1=expr; SEMICOLON; e2=expr {Join(e1, e2)}
  | e=expr12 {e}

expr12:
  | b=base {Base(b)}

tdecl:
  | SIG; a = T_IDENT; e=EQ_OP; t = typesig
    {TDecl(a, t)}

assign:
  | LET; a = T_IDENT; args = list(T_IDENT); eq=EQ_OP; e = expr
    {KAss(a, args, e)}

toplevel:
  | a = assign {TopAssign(a)}
  | t = tdecl  {TopTDecl(t)}

program:
  | a = nonempty_list(toplevel); EOF {Program(a)}
