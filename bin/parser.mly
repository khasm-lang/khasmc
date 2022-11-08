%{
    open Ast
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
%token QMARK
%token KW_IF
%token KW_ELSE
%token KW_WHILE
%token KW_FOR
%token KW_RETURN
%token KW_IN
%token KW_LET
%token COLON
%token SEMICOLON
%token EOF
%token TS_TO
%token IGNORE
%token FORALL

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
%type<Ast.binop> binop
%type<Ast.kexpr> expr
%type<Ast.kexpr> fexpr
%type<Ast.kident list> list(T_IDENT)
%type<Ast.kexpr list> nonempty_list(parenexpr)
%type<Ast.toplevel list> nonempty_list(toplevel)
%type<Ast.kexpr> parenexpr
%type<Ast.tdecl> tdecl
%type<Ast.toplevel> toplevel

%right TS_TO

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

binop:
  | ADD {ADD}

parenexpr:
  | b = base {Base(b)}
  | LPAREN; e = expr RPAREN {e}

fexpr:
  | LPAREN; e = expr; RPAREN {e}
  | t = T_IDENT {Base(Ident(t))}

expr:
  | a = parenexpr {a}
  | a = parenexpr; o = binop; b = parenexpr {BinOp(a, o, b)}
  | f = fexpr; args = nonempty_list(parenexpr) {FCall(f, args)}
  | KW_LET; a = T_IDENT; EQ; e1 = expr; KW_IN; e2 = expr
    {LetIn(a, e1, e2)}
  | KW_IF e1 = expr LBRACE e2 = expr RBRACE KW_ELSE LBRACE e3 = expr RBRACE
    {IfElse(e1, e2, e3)}

tdecl:
  | a = T_IDENT; COLON; t = typesig DOT {TDecl(a, t)}

assign:
  | a = T_IDENT; args = list(T_IDENT); EQ; e = expr; DOT {KAss(a, args, e)}

toplevel:
  | a = assign {TopAssign(a)}
  | t = tdecl  {TopTDecl(t)}

program:
  | a = nonempty_list(toplevel); EOF {Program(a)}
