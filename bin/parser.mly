
%{
    [@@@coverage exclude_file]
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
%token MULT
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

%type<Ast.program> program
%type<Ast.typeSig> typedec
%type<Ast.expr> expr

%right typesig

%start program

%%

typesig:
  | base=IDENT {Base(base)}
  | ty1=typesig; SUB; GT; ty2=typesig {Arrow(ty1, ty2)}
(* str -> str *)

block:
  | many=list(block) {Many(many)}
     (* list of things *)
  | id=IDENT; EQ; ex=expr; SEMICOLON {Assign(id, ex)}
     (* x = 1 + 1; *)
  | id=IDENT; EQ; LBRACE; bl=block; RBRACE {AssignBlock(id, bl)}
     (* x = { return 1; }*)
  | id=IDENT; COLON; ty=typesig; SEMICOLON {Typesig(id, ty)}
     (* x : int -> int *)
  | IF; ex=expr; LBRACE; bl=block; RBRACE {If(ex, bl)}
     (* if x = 1 {do thing}*)
  | WHILE; ex=expr; LBRACE; bl=block; RBRACE {While(ex, bl)}
     (* while x == 1 {do thing} *)


toplevel:
  | id=IDENT; EQ; ex=expr; SEMICOLON {Assign(id, ex)}
  | id=IDENT; EQ; LBRACE; bl=block; RBRACE {AssignBlock(id, bl)}
  | id=IDENT; COLON; ty=typesig; SEMICOLON {Typesig(id, ty)}


program:
  | toplevel=list(toplevel) EOF {Prog(toplevel)}
