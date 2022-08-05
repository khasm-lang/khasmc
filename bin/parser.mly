
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
%token KW_WHILE
%token KW_FOR
%token KW_RETURN
%token COLON
%token SEMICOLON
%token EOF

%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token LPAREN
%token RPAREN

%type<Ast.program> program
%type<Ast.toplevel> toplevel
%type<Ast.toplevel list> list(toplevel)
%type<Ast.typeSig> typesig
%type<Ast.expr>    expr
%type<Ast.block>   blocksub
%type<Ast.block list> list(blocksub)
%type<Ast.block>   block
%type<Ast.const>   const
%type<Ast.unop>    unop
%type<Ast.unop list> list(unop)
%type<Ast.binop>   binop
%type<Ast.expr> func
%type<Ast.expr> parenexpr




%start program

%%

typesig:
  | c=list(AT); base=T_IDENT {Ptr(List.length c, base)}
  | base=T_IDENT {Base(base)}
  | ty1=typesig; SUB; GT; ty2=typesig {Arrow(ty1, ty2)}
(* str -> str *)


const:
  | c=T_INT {Int(c)}
  | c=T_FLOAT {Float(c)}
  | c=T_STRING {String(c)}
  | c=T_IDENT {Id(c)}
  | TRUE {True}
  | FALSE {False}

unop:
  | AT  {UnOpDeref}
  | AND {UnOpRef}
  | ADD {UnOpPos}
  | SUB {UnOpNeg}

binop:
  | ADD {BinOpPlus}
  | SUB {BinOpMinus}
  | MUL {BinOpMul}
  | SLASH {BinOpDiv}

func:
  | c=T_IDENT {Base(Id(c))}
  | LPAREN e=expr RPAREN {Paren(e)}

parenexpr:
  | c=const {Base(c)}
  | LPAREN e=expr RPAREN {Paren(e)}


expr:
  | LPAREN; e=expr; RPAREN {Paren(e)}
  | c=const {Base(c)}
  | LPAREN; u=list(unop); ex=expr; RPAREN {UnOp(u, ex)}
  | ex1=parenexpr; b=binop; ex2=parenexpr {BinOp(ex1, b, ex2)}
  | ex1=func ex2=nonempty_list(parenexpr) {FuncCall(ex1, ex2)} 


blocksub:
  | id=T_IDENT; EQ; ex=expr; SEMICOLON {Assign(id, ex)}
     (* x = 1 + 1; *)
  | id=T_IDENT; EQ; LBRACE; bl=block; RBRACE {AssignBlock(id, bl)}
     (* x = { return 1; }*)
  | id=T_IDENT; COLON; ty=typesig; SEMICOLON {Typesig(id, ty)}
     (* x : int -> int *)
  | KW_IF; ex=expr; LBRACE; bl=block; RBRACE {If(ex, bl)}
     (* if x = 1 {do thing}*)
  | KW_WHILE; ex=expr; LBRACE; bl=block; RBRACE {While(ex, bl)}
     (* while x == 1 {do thing} *)
  | KW_RETURN; ex=expr; SEMICOLON {Return(ex)}

block:
  | many = list(blocksub) {Many(many)}

toplevel:
  | id=T_IDENT; EQ; ex=expr; SEMICOLON {Assign(id, ex)}
  | id=T_IDENT; EQ; LBRACE; bl=block; RBRACE {AssignBlock(id, bl)}
  | id=T_IDENT; COLON; ty=typesig; SEMICOLON {Typesig(id, ty)}


program:
  | top=list(toplevel) EOF {Prog(top)}
