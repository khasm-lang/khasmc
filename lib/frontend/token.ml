type t_TOKEN =
  | LEFTB
  | RIGHTB
  | LEFTP
  | RIGHTP
  | LEFTC
  | RIGHTC
  | GT
  | LT
  | DOLLAR
  | HASH
  | AT
  | STAR
  | BANG
  | PERCENT
  | PLUS
  | MINUS
  | AND
  | PIPE
  | COMMA
  | SEMICOLON
  | COLON
  | EQUALS
  | FSLASH
  | BSLASH
  | TYPE
  | TRAIT
  | REF
  | WHERE
  | LET
  | IN
  | AS
  | ARROW
  | TYINT
  | TYSTRING
  | TYCHAR
  | TYFLOAT
  | TYBOOL
  | IMPL
  | MODULE
  | END
  | MATCH
  | FUN
  | BOOL of bool
  | STRING of string
  | ID of string
  | TYPEID of string
  | POLYID of string
  | INT of string
  | FLOAT of string
  | DONE
[@@deriving show { with_path = false }]
