{

  open Lexing
  open Parser
  exception SyntaxError of string
  exception NotImpl of string


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
let all = (alpha |digit|'_'|'\'')
let IDENT = (alpha) (all)*
let any = (_)*

let FLOAT = digit+'.'digit+ 
let WHITESPACE = [' ' '\t']+
let NEWLINE = '\n' | '\r' | "\r\n"
let COMMENT = "(*" any "*)"

let STRING = '\"' any '\"' 

rule token = parse
     | "(" {LPAREN}
     | ")" {RPAREN}
     | "{" {LBRACE}
     | "}" {RBRACE}
     | "[" {LBRACK}
     | "]" {RBRACK}
     | "True" {TRUE}
     | "False" {FALSE}
     | "\\" {BSLASH}
     | "/" {SLASH}
     | "&" {AND}
     | "%" {PERCENT}
     | "#" {HASH}
     | "!" {BANG}
     | "," {COMMA}
     | ">" {GT}
     | "<" {LT}
     | "=" {EQ}
     | "@" {AT}
     | "+" {ADD}
     | "-" {SUB}
     | "." {DOT}
     | "|" {STRAIGHT (* unlike me *)}
     | "*" {MUL}
     | "/" {SLASH}
     | ":" {COLON}
     | ";" {SEMICOLON}
     | "if"	{KW_IF}
     | "while" 	{KW_WHILE}
     | "for" 	{KW_FOR}
     | "return" {KW_RETURN}
     | "true"   {TRUE}
     | "false"  {FALSE}
     | COMMENT { token lexbuf }
     | WHITESPACE { token lexbuf}
     | NEWLINE { next_line lexbuf; token lexbuf}
     | INT { T_INT (Lexing.lexeme lexbuf) }
     | FLOAT { T_FLOAT (Lexing.lexeme lexbuf)}
     | STRING {raise (NotImpl ("Strings are not implemented"))}
     | IDENT { T_IDENT (Lexing.lexeme lexbuf)}
     | eof {EOF}
     | _ {raise (SyntaxError ("Lexer - Illegal Character: " ^ Lexing.lexeme lexbuf))}

