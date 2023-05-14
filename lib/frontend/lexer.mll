{

  open Lexing
  open Parser
  exception SyntaxError of string
  exception NotImpl of string
  exception EOF of string


  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
    {
        pos with pos_bol = 0;
        pos_lnum = pos.pos_lnum + 1
    }
  let incr_by lexbuf =
    let len = String.length (Lexing.lexeme lexbuf) in
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
    {
	pos with pos_bol = pos.pos_bol + len
    }
  let incr_by_i i lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
    {
	pos with pos_bol = pos.pos_bol + i
    }
}

let U1 = [ '\000' - '\127' ]
let U2 = [ '\012' - '\013' ]
let U3 = '\014'
let U4 = '\015'
let follow = [ '\008' - '\011' ][ '\000' - '\255' ]

let unicode = (
    U1
    | U2 follow
    | U3 follow follow
    | U4 follow follow follow
)

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let INT = (digit)+

let start = ['a'-'z' 'A'-'Z' '_' '\'']

let all = ['a'-'z' 'A'-'Z' '_' '\'' '0'-'9' ]

let IDENT = start all*

let INTIDENT = '`' start all+


let operator_chars = ['$'  '&'  '@'  '+'  '*'  '-'  '='  '>'  '<'  '?'  ':'  '!'  '.'  '%'  '~'  '|'  '/'  '['  ']'  '~' '^']


let bang_op = '!' operator_chars*
let tilde_op = '~' operator_chars*
let pow_op = '*' '*' operator_chars*
let mul_op = '*' operator_chars*
let div_op = '/' operator_chars*
let mod_op = '%' operator_chars*
let add_op = '+' operator_chars*
let sub_op = '-' operator_chars*
let col_op = ':' operator_chars*
let at_op  = '@' operator_chars*
let car_op = '^' operator_chars*
let eq_op  = '=' operator_chars*
let lt_op  = '<' operator_chars*
let gt_op  = '>' operator_chars*
let pip_op = '|' operator_chars*
let and_op = '&' operator_chars*
let dol_op = '$' operator_chars*

let any = (_)*


let FLOAT = digit+'.'digit+ 
let WHITESPACE = [' ' '\t']+
let NEWLINE = '\n' | '\r' | "\r\n"
let COMMENT = "(*" any "*)"


rule token = parse
     | "(" {incr_by lexbuf; LPAREN}
     | ")" {incr_by lexbuf; RPAREN}
     | "{" {incr_by lexbuf; LBRACE}
     | "}" {incr_by lexbuf; RBRACE}
     | "[" {incr_by lexbuf; LBRACK}
     | "]" {incr_by lexbuf; RBRACK}
     | "true" {incr_by lexbuf; TRUE}
     | "false" {incr_by lexbuf; FALSE}
     | "\\" {incr_by lexbuf; BSLASH}
     | "&" {incr_by lexbuf; AND}
     | "?" {incr_by lexbuf; QMARK}
     | "#" {incr_by lexbuf; HASH}
     | "," {incr_by lexbuf; COMMA}
     | ";" {incr_by lexbuf; SEMICOLON}
     | "->" {incr_by lexbuf; TS_TO}
     | "→" {incr_by lexbuf; TS_TO}
     | "=>" {incr_by lexbuf; LAM_TO}
	| bang_op {incr_by lexbuf; BANG_OP (Lexing.lexeme lexbuf)}
	| tilde_op {incr_by lexbuf; TILDE_OP (Lexing.lexeme lexbuf)}
	| pow_op {incr_by lexbuf; POW_OP (Lexing.lexeme lexbuf)}
	| mul_op {incr_by lexbuf; MUL_OP (Lexing.lexeme lexbuf)}
	| div_op {incr_by lexbuf; DIV_OP (Lexing.lexeme lexbuf)}
	| mod_op {incr_by lexbuf; MOD_OP (Lexing.lexeme lexbuf)}
	| add_op {incr_by lexbuf; ADD_OP (Lexing.lexeme lexbuf)}
	| sub_op {incr_by lexbuf; SUB_OP (Lexing.lexeme lexbuf)}
	| col_op {incr_by lexbuf; COL_OP (Lexing.lexeme lexbuf)}
	| car_op {incr_by lexbuf; CAR_OP (Lexing.lexeme lexbuf)}
	| at_op  {incr_by lexbuf; AT_OP  (Lexing.lexeme lexbuf)}
	| eq_op  {incr_by lexbuf; EQ_OP  (Lexing.lexeme lexbuf)}
	| lt_op  {incr_by lexbuf; LT_OP  (Lexing.lexeme lexbuf)}
	| gt_op  {incr_by lexbuf; GT_OP  (Lexing.lexeme lexbuf)}
	| pip_op {incr_by lexbuf; PIP_OP (Lexing.lexeme lexbuf)}
	| and_op {incr_by lexbuf; AND_OP (Lexing.lexeme lexbuf)}
	| dol_op {incr_by lexbuf; DOL_OP (Lexing.lexeme lexbuf)}
     | "if"	{incr_by lexbuf; IF}
     | "of"	{incr_by lexbuf; OF}
     | "then"   {incr_by lexbuf; THEN}
     | "else"   {incr_by lexbuf; ELSE}
     | "while" 	{incr_by lexbuf; WHILE}
     | "for" 	{incr_by lexbuf; FOR}
     | "let"    {incr_by lexbuf; LET}
     | "rec" 	{incr_by lexbuf; REC}
     | "in"     {incr_by lexbuf; IN}
     | "end"    {incr_by lexbuf; END}
     | "true"   {incr_by lexbuf; TRUE}
     | "false"  {incr_by lexbuf; FALSE}
     | "fun"    {incr_by lexbuf; FUN}
     | "tfun"   {incr_by lexbuf; TFUN}
     | "nomangle" {incr_by lexbuf; NOMANGLE}
     | "inline" {incr_by lexbuf; INLINE}
     | "ignore" {incr_by lexbuf; IGNORE}
     | "forall" {incr_by lexbuf; FORALL}
     | "extern" {incr_by lexbuf; EXTERN}
     | "internal_extern" {incr_by lexbuf; INTEXTERN}
     | "bind" {incr_by lexbuf; BIND}
     | "and" {incr_by lexbuf; LAND}
     | "or"  {incr_by lexbuf; LOR}
     | "module" {incr_by lexbuf; MODULE}
     | "struct" {incr_by lexbuf; STRUCT}
     | "functor" {incr_by lexbuf; FUNCTOR}
     | "open" {incr_by lexbuf; OPEN}
     | "∀" {incr_by lexbuf; FORALL}
     | "λ" {incr_by lexbuf; FUN}
     | "Λ" {incr_by lexbuf; TFUN}
     | "sig" {incr_by lexbuf; SIG}
     | COMMENT {incr_by lexbuf;  token lexbuf }
     | WHITESPACE {incr_by lexbuf;  token lexbuf}
     | NEWLINE { next_line lexbuf; token lexbuf}
     | INT {incr_by lexbuf;  T_INT (Lexing.lexeme lexbuf) }
     | FLOAT {incr_by lexbuf;  T_FLOAT (Lexing.lexeme lexbuf)}
     | '"' {
     let buffer = Buffer.create 20 in
     let s = stringl buffer lexbuf in
     incr_by_i (String.length s + 2) lexbuf; 
     T_STRING(s)
     }
     | INTIDENT {incr_by lexbuf; INTIDENT (Lexing.lexeme lexbuf)}
     | IDENT {incr_by lexbuf; T_IDENT (Lexing.lexeme lexbuf)}
     | "." {incr_by lexbuf; DOT}
     | eof {EOF}
     | _ {raise (SyntaxError ("Lexer - Illegal Character: " ^ Lexing.lexeme lexbuf))}

and stringl buffer = parse
 | '"' { Buffer.contents buffer }	
 | "\\t" { Buffer.add_char buffer '\t'; stringl buffer lexbuf }
 | "\\n" { Buffer.add_char buffer '\n'; stringl buffer lexbuf }	
 | "\\n" { Buffer.add_char buffer '\n'; stringl buffer lexbuf }	
 | '\\' '"' { Buffer.add_char buffer '"'; stringl buffer lexbuf }	
 | '\\' '\\' { Buffer.add_char buffer '\\'; stringl buffer lexbuf }
 | eof { raise @@ EOF "unexpected EOF" }
 | _ as char { Buffer.add_char buffer char; stringl buffer lexbuf }	