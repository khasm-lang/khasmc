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
        pos with pos_bol = lexbuf.lex_curr_pos;
        pos_lnum = pos.pos_lnum + 1
    }
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let INT = (digit)+

let start = ['a'-'z' 'A'-'Z' '_' '\'']
let all = ['a'-'z' 'A'-'Z' '_' '\'' '0'-'9']

let IDENT = start all*

let FIDENT = start all* ((start all* | ':' | '.' )* start all *)? 


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
     | "(" {LPAREN}
     | ")" {RPAREN}
     | "{" {LBRACE}
     | "}" {RBRACE}
     | "true" {TRUE}
     | "false" {FALSE}
     | "\\" {BSLASH}
     | "&" {AND}
     | "?" {QMARK}
     | "#" {HASH}
     | "," {COMMA}
     | ";" {SEMICOLON}
     | "->" {TS_TO}
	| bang_op {BANG_OP (Lexing.lexeme lexbuf)}
	| tilde_op {TILDE_OP (Lexing.lexeme lexbuf)}
	| pow_op {POW_OP (Lexing.lexeme lexbuf)}
	| mul_op {MUL_OP (Lexing.lexeme lexbuf)}
	| div_op {DIV_OP (Lexing.lexeme lexbuf)}
	| mod_op {MOD_OP (Lexing.lexeme lexbuf)}
	| add_op {ADD_OP (Lexing.lexeme lexbuf)}
	| sub_op {SUB_OP (Lexing.lexeme lexbuf)}
	| col_op {COL_OP (Lexing.lexeme lexbuf)}
	| car_op {CAR_OP (Lexing.lexeme lexbuf)}
	| at_op  {AT_OP  (Lexing.lexeme lexbuf)}
	| eq_op  {EQ_OP  (Lexing.lexeme lexbuf)}
	| lt_op  {LT_OP  (Lexing.lexeme lexbuf)}
	| gt_op  {GT_OP  (Lexing.lexeme lexbuf)}
	| pip_op {PIP_OP (Lexing.lexeme lexbuf)}
	| and_op {AND_OP (Lexing.lexeme lexbuf)}
	| dol_op {DOL_OP (Lexing.lexeme lexbuf)}
     | "if"	{IF}
     | "of"	{OF}
     | "then"   {THEN}
     | "else"   {ELSE}
     | "while" 	{WHILE}
     | "for" 	{FOR}
     | "let"    {LET}
     | "in"     {IN}
     | "true"   {TRUE}
     | "false"  {FALSE}
     | "nomangle" {NOMANGLE}
     | "inline" {INLINE}
     | "ignore" {IGNORE}
     | "forall" {FORALL}
     | "and" {LAND}
     | "or"  {LOR}
     | "∀" {FORALL}
     | "sig" {SIG}
     | COMMENT { token lexbuf }
     | WHITESPACE { token lexbuf}
     | NEWLINE { next_line lexbuf; token lexbuf}
     | INT { T_INT (Lexing.lexeme lexbuf) }
     | FLOAT { T_FLOAT (Lexing.lexeme lexbuf)}
     | '"' {let buffer = Buffer.create 20 in T_STRING(stringl buffer lexbuf)}
     | IDENT { T_IDENT (Lexing.lexeme lexbuf)}
     | eof {EOF}
     | _ {raise (SyntaxError ("Lexer - Illegal Character: " ^ Lexing.lexeme lexbuf))}

and stringl buffer = parse
 | '"' { Buffer.contents buffer }	
 | "\\t" { Buffer.add_char buffer '\t'; stringl buffer lexbuf }
 | "\\n" { Buffer.add_char buffer '\n'; stringl buffer lexbuf }	
 | "\\n" { Buffer.add_char buffer '\n'; stringl buffer lexbuf }	
 | '\\' '"' { Buffer.add_char buffer '"'; stringl buffer lexbuf }	
 | '\\' '\\' { Buffer.add_char buffer '\\'; stringl buffer lexbuf }
 | eof { raise End_of_file }
 | _ as char { Buffer.add_char buffer char; stringl buffer lexbuf }	