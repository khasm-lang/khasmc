%token LEFTP
%token RIGHTP
%token LEFTC
%token RIGHTC
%token GT
%token LT
%token DOLLAR
%token HASH
%token AT
%token STAR
%token BANG
%token PERCENT
%token PLUS
%token MINUS
%token AND
%token OR
%token PIPE
%token COMMA
%token SEMICOLON
%token COLON
%token EQUALS
%token COLONEQ
%token FSLASH
%token BSLASH
%token TYPE
%token TRAIT
%token REF
%token WHERE
%token LET
%token IN
%token AS
%token ARROW
%token FATARROW
%token TYINT
%token TYSTRING
%token TYCHAR
%token TYFLOAT
%token TYBOOL
%token IMPL
%token MODULE
%token END
%token MATCH
%token WITH
%token FUN
%token IF
%token THEN
%token ELSE
%token OF
%token DOT
%token EXTERN

%token<bool> BOOL
%token<string> STRING
%token<string> ID
%token<string> TYPEID
%token<string> POLYID
%token<string> INT
%token<string> FLOAT
%token<string> CHAR
%token<string> OTHER

%token DONE

%type <('a, 'b) Ast.expr> expr
%type <('a * ('a, 'b) Ast.expr) list> record_expr
%start <('a, 'b, unit) Ast.toplevel list> main


%{
open Ast
let d () = data ()

let lexer buf =
let Ok next = Token.lexer_ buf in
next
%}

%nonassoc EQUALS LT GT AND OR
%left PLUS MINUS
%left FSLASH STAR
%right ARROW

%%

atom_case:
  | LEFTP ts = separated_nonempty_list(COMMA, case)
    RIGHTP { CaseTuple ts }
  | LEFTP c = case RIGHTP { c }
  | i = ID { CaseVar i }
  | b = BOOL { CaseLit (LBool b) }
  | i = INT { CaseLit (LInt i) }

case:
  | t = TYPEID a = list(atom_case) { CaseCtor (t,a) }
  | c = atom_case { c }

atom_type:
  | TYINT { TyInt }
  | TYSTRING { TyString }
  | TYCHAR { TyChar }
  | TYFLOAT { TyFloat }
  | TYBOOL { TyBool }
  | p = POLYID { TyPoly p }
  | REF a = atom_type { TyRef a }
  | LEFTP ts = separated_nonempty_list(COMMA, typ)
    RIGHTP { TyTuple ts }
  | LEFTP t = typ RIGHTP { t }
  | LEFTP RIGHTP { TyTuple [] }

typ:
  | t = TYPEID a = list(atom_type) {TyCustom(t,a)}
  | a = atom_type { a }
  | a=typ ARROW b=typ { TyArrow(a,b) }

record_expr:
  | n = ID EQUALS e = expr SEMICOLON { [n,e] }
  | n = ID EQUALS e = expr SEMICOLON r = record_expr
    {(n,e) :: r}

match_expr:
  | END { [] }
  | PIPE c=case FATARROW e=expr m=match_expr
    {(c,e) :: m}

atom_expr:
  | LEFTP e = expr RIGHTP { e }
  | i = INT { Int (d(), i) }
  | b = BOOL { Bool (d(), b) }
  | id = ID { Var (d(), id) }
  | f = FLOAT { Float (d(), f) }
  | c = CHAR { Char (d(), c) }
  | s = STRING { String (d(), s) }
  | t = TYPEID LEFTC r = record_expr RIGHTC
    { Record (d(),t, r) }
  | t = TYPEID { Constructor (d(), t) }

%inline unaryop:
  | BANG { BNegate }
  | MINUS { Negate }
  | REF { Ref }

%inline binop:
  | STAR { Mul }
  | FSLASH { Div }
  | EQUALS { Eq }
  | LT { Lt }
  | GT { Gt }
  | AND { LAnd }
  | OR { LOr }
  | PLUS { Add }
  | MINUS { Sub }

func_expr:
  | f = func_expr x = atom_expr {Funccall(d(),f,x)} 
  | a = atom_expr { a }

expr:
  | f = func_expr { f }
  | u = unaryop a = atom_expr {UnaryOp(d(),u,a)}
  | a = atom_expr COLON t = typ { Annot(d(),a,t) }
  | LET c = case o=option(preceded(COLON,typ)) EQUALS hd=expr IN tl=expr
    {LetIn(d(),c,o,hd,tl)}
  | LEFTP s=separated_nonempty_list(COMMA,expr) RIGHTP
    {Tuple(d(), s)}
  | MATCH e=expr WITH m=match_expr {Match(d(),e,m)}
  | FUN id=ID o=option(preceded(COLON,typ))
    FATARROW e=expr { Lambda(d(),id,o,e) }
  | a = atom_expr op = binop b = atom_expr
    { BinOp(d(), op,a,b) }
  | a = expr SEMICOLON b = expr { Seq(d(),a,b) }
  | i=ID COLONEQ e = expr { Modify(d(),i,e) }
  | a=atom_expr DOT i=ID {UnaryOp(d(),GetRecField i,a)}
  | a=atom_expr DOT ia=INT {UnaryOp(d(),Project (int_of_string ia),a)}

argument:
  | LEFTP i=ID COLON t=typ RIGHTP { i, t }

def:
  | FUN i=ID
    o=option(delimited(LEFTC, preceded(TYPE,list(POLYID)), RIGHTC))
    l=list(argument) COLON ret=typ EQUALS
    e = expr
    {
Definition {
data = d();
name = i;
typeargs = (match o with | None -> [] | Some s -> s);
args = l;
return = ret;
body = Just e;
}
    }

sum_case:
  | PIPE nm=TYPEID option(OF) args=list(atom_type) { nm, args }

record_case:
  | nm=ID COLON t=typ SEMICOLON { nm, t }

typdef:
  | TYPE nm=TYPEID p=list(POLYID) EQUALS t=list(sum_case) END
    {
Typdef {
data = d();
name = nm;
args = p;
content = Sum t;
}
    }
  | TYPE nm=TYPEID p=list(POLYID) EQUALS LEFTC t=list(record_case) RIGHTC
    {
Typdef {
data = d();
name = nm;
args = p;
content = Record t;
}
    }

toplevel:
  | d=def {d}
  | t=typdef {t}
  | EXTERN n=ID COLON t=typ { Extern (n, t) }

main:
  | e = list(toplevel) DONE { e } 
