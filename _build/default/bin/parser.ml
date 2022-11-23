
module MenhirBasics = struct
  
  exception Error of int
  
  let _eRR =
    fun _s ->
      raise (Error _s)
  
  type token = 
    | WHILE
    | T_STRING of (
# 8 "bin/parser.mly"
       (string)
# 16 "bin/parser.ml"
  )
    | T_INT of (
# 6 "bin/parser.mly"
       (string)
# 21 "bin/parser.ml"
  )
    | T_IDENT of (
# 5 "bin/parser.mly"
       (string)
# 26 "bin/parser.ml"
  )
    | T_FLOAT of (
# 7 "bin/parser.mly"
       (string)
# 31 "bin/parser.ml"
  )
    | TS_TO
    | TRUE
    | TILDE_OP of (
# 47 "bin/parser.mly"
      (string)
# 38 "bin/parser.ml"
  )
    | TILDE
    | THEN
    | SUB_OP of (
# 56 "bin/parser.mly"
      (string)
# 45 "bin/parser.ml"
  )
    | SUB
    | STRAIGHT
    | SLASH
    | SIG
    | SEMICOLON
    | RPAREN
    | RETURN
    | RBRACK
    | RBRACE
    | QMARK
    | POW_OP of (
# 49 "bin/parser.mly"
      (string)
# 60 "bin/parser.ml"
  )
    | PIP_OP of (
# 66 "bin/parser.mly"
      (string)
# 65 "bin/parser.ml"
  )
    | PERCENT
    | NOMANGLE
    | MUL_OP of (
# 51 "bin/parser.mly"
      (string)
# 72 "bin/parser.ml"
  )
    | MUL
    | MOD_OP of (
# 53 "bin/parser.mly"
      (string)
# 78 "bin/parser.ml"
  )
    | LT_OP of (
# 64 "bin/parser.mly"
      (string)
# 83 "bin/parser.ml"
  )
    | LT
    | LPAREN
    | LOR
    | LET
    | LBRACK
    | LBRACE
    | LAND
    | INLINE
    | IN
    | IGNORE
    | IF
    | HASH
    | GT_OP of (
# 65 "bin/parser.mly"
      (string)
# 100 "bin/parser.ml"
  )
    | GT
    | FORALL
    | FOR
    | FALSE
    | EQ_OP of (
# 63 "bin/parser.mly"
      (string)
# 109 "bin/parser.ml"
  )
    | EQ
    | EOF
    | ELSE
    | DOT
    | DOL_OP of (
# 68 "bin/parser.mly"
      (string)
# 118 "bin/parser.ml"
  )
    | DOLLAR
    | DIV_OP of (
# 52 "bin/parser.mly"
      (string)
# 124 "bin/parser.ml"
  )
    | COMMA
    | COL_OP of (
# 58 "bin/parser.mly"
      (string)
# 130 "bin/parser.ml"
  )
    | COLON
    | CAR_OP of (
# 60 "bin/parser.mly"
      (string)
# 136 "bin/parser.ml"
  )
    | BSLASH
    | BANG_OP of (
# 46 "bin/parser.mly"
      (string)
# 142 "bin/parser.ml"
  )
    | BANG
    | AT_OP of (
# 61 "bin/parser.mly"
      (string)
# 148 "bin/parser.ml"
  )
    | AT
    | AND_OP of (
# 67 "bin/parser.mly"
      (string)
# 154 "bin/parser.ml"
  )
    | AND
    | ADD_OP of (
# 55 "bin/parser.mly"
      (string)
# 160 "bin/parser.ml"
  )
    | ADD
  
end

include MenhirBasics

# 1 "bin/parser.mly"
  
    open Ast

# 172 "bin/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState000 : ('s, _menhir_box_program) _menhir_state
    (** State 000.
        Stack shape : .
        Start symbol: program. *)

  | MenhirState003 : (('s, _menhir_box_program) _menhir_cell1_SIG _menhir_cell0_T_IDENT _menhir_cell0_EQ_OP, _menhir_box_program) _menhir_state
    (** State 003.
        Stack shape : SIG T_IDENT EQ_OP.
        Start symbol: program. *)

  | MenhirState005 : (('s, _menhir_box_program) _menhir_cell1_LPAREN, _menhir_box_program) _menhir_state
    (** State 005.
        Stack shape : LPAREN.
        Start symbol: program. *)

  | MenhirState006 : (('s, _menhir_box_program) _menhir_cell1_FORALL, _menhir_box_program) _menhir_state
    (** State 006.
        Stack shape : FORALL.
        Start symbol: program. *)

  | MenhirState007 : (('s, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_state
    (** State 007.
        Stack shape : T_IDENT.
        Start symbol: program. *)

  | MenhirState010 : ((('s, _menhir_box_program) _menhir_cell1_FORALL, _menhir_box_program) _menhir_cell1_nonempty_list_T_IDENT_, _menhir_box_program) _menhir_state
    (** State 010.
        Stack shape : FORALL nonempty_list(T_IDENT).
        Start symbol: program. *)

  | MenhirState012 : (('s, _menhir_box_program) _menhir_cell1_typesig_i, _menhir_box_program) _menhir_state
    (** State 012.
        Stack shape : typesig_i.
        Start symbol: program. *)

  | MenhirState020 : (('s, _menhir_box_program) _menhir_cell1_typesig, _menhir_box_program) _menhir_state
    (** State 020.
        Stack shape : typesig.
        Start symbol: program. *)

  | MenhirState028 : (('s, _menhir_box_program) _menhir_cell1_LET _menhir_cell0_T_IDENT, _menhir_box_program) _menhir_state
    (** State 028.
        Stack shape : LET T_IDENT.
        Start symbol: program. *)

  | MenhirState029 : (('s, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_state
    (** State 029.
        Stack shape : T_IDENT.
        Start symbol: program. *)

  | MenhirState032 : ((('s, _menhir_box_program) _menhir_cell1_LET _menhir_cell0_T_IDENT, _menhir_box_program) _menhir_cell1_list_T_IDENT_ _menhir_cell0_EQ_OP, _menhir_box_program) _menhir_state
    (** State 032.
        Stack shape : LET T_IDENT list(T_IDENT) EQ_OP.
        Start symbol: program. *)

  | MenhirState037 : (('s, _menhir_box_program) _menhir_cell1_TILDE_OP, _menhir_box_program) _menhir_state
    (** State 037.
        Stack shape : TILDE_OP.
        Start symbol: program. *)

  | MenhirState038 : (('s, _menhir_box_program) _menhir_cell1_LPAREN, _menhir_box_program) _menhir_state
    (** State 038.
        Stack shape : LPAREN.
        Start symbol: program. *)

  | MenhirState040 : (('s, _menhir_box_program) _menhir_cell1_LET _menhir_cell0_T_IDENT, _menhir_box_program) _menhir_state
    (** State 040.
        Stack shape : LET T_IDENT.
        Start symbol: program. *)

  | MenhirState042 : ((('s, _menhir_box_program) _menhir_cell1_LET _menhir_cell0_T_IDENT, _menhir_box_program) _menhir_cell1_list_T_IDENT_ _menhir_cell0_EQ_OP, _menhir_box_program) _menhir_state
    (** State 042.
        Stack shape : LET T_IDENT list(T_IDENT) EQ_OP.
        Start symbol: program. *)

  | MenhirState043 : (('s, _menhir_box_program) _menhir_cell1_IF, _menhir_box_program) _menhir_state
    (** State 043.
        Stack shape : IF.
        Start symbol: program. *)

  | MenhirState044 : (('s, _menhir_box_program) _menhir_cell1_BANG_OP, _menhir_box_program) _menhir_state
    (** State 044.
        Stack shape : BANG_OP.
        Start symbol: program. *)

  | MenhirState045 : (('s, _menhir_box_program) _menhir_cell1_fexpr, _menhir_box_program) _menhir_state
    (** State 045.
        Stack shape : fexpr.
        Start symbol: program. *)

  | MenhirState047 : (('s, _menhir_box_program) _menhir_cell1_LPAREN, _menhir_box_program) _menhir_state
    (** State 047.
        Stack shape : LPAREN.
        Start symbol: program. *)

  | MenhirState061 : (('s, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_SUB_OP, _menhir_box_program) _menhir_state
    (** State 061.
        Stack shape : expr SUB_OP.
        Start symbol: program. *)

  | MenhirState063 : (('s, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_POW_OP, _menhir_box_program) _menhir_state
    (** State 063.
        Stack shape : expr POW_OP.
        Start symbol: program. *)

  | MenhirState066 : (('s, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_MUL_OP, _menhir_box_program) _menhir_state
    (** State 066.
        Stack shape : expr MUL_OP.
        Start symbol: program. *)

  | MenhirState068 : (('s, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_MOD_OP, _menhir_box_program) _menhir_state
    (** State 068.
        Stack shape : expr MOD_OP.
        Start symbol: program. *)

  | MenhirState070 : (('s, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_DIV_OP, _menhir_box_program) _menhir_state
    (** State 070.
        Stack shape : expr DIV_OP.
        Start symbol: program. *)

  | MenhirState072 : (('s, _menhir_box_program) _menhir_cell1_expr, _menhir_box_program) _menhir_state
    (** State 072.
        Stack shape : expr.
        Start symbol: program. *)

  | MenhirState074 : (('s, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_PIP_OP, _menhir_box_program) _menhir_state
    (** State 074.
        Stack shape : expr PIP_OP.
        Start symbol: program. *)

  | MenhirState076 : (('s, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_COL_OP, _menhir_box_program) _menhir_state
    (** State 076.
        Stack shape : expr COL_OP.
        Start symbol: program. *)

  | MenhirState078 : (('s, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_ADD_OP, _menhir_box_program) _menhir_state
    (** State 078.
        Stack shape : expr ADD_OP.
        Start symbol: program. *)

  | MenhirState080 : (('s, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_CAR_OP, _menhir_box_program) _menhir_state
    (** State 080.
        Stack shape : expr CAR_OP.
        Start symbol: program. *)

  | MenhirState082 : (('s, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_AT_OP, _menhir_box_program) _menhir_state
    (** State 082.
        Stack shape : expr AT_OP.
        Start symbol: program. *)

  | MenhirState084 : (('s, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_LT_OP, _menhir_box_program) _menhir_state
    (** State 084.
        Stack shape : expr LT_OP.
        Start symbol: program. *)

  | MenhirState086 : (('s, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_GT_OP, _menhir_box_program) _menhir_state
    (** State 086.
        Stack shape : expr GT_OP.
        Start symbol: program. *)

  | MenhirState088 : (('s, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_EQ_OP, _menhir_box_program) _menhir_state
    (** State 088.
        Stack shape : expr EQ_OP.
        Start symbol: program. *)

  | MenhirState090 : (('s, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_DOL_OP, _menhir_box_program) _menhir_state
    (** State 090.
        Stack shape : expr DOL_OP.
        Start symbol: program. *)

  | MenhirState092 : (('s, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_AND_OP, _menhir_box_program) _menhir_state
    (** State 092.
        Stack shape : expr AND_OP.
        Start symbol: program. *)

  | MenhirState095 : (('s, _menhir_box_program) _menhir_cell1_parenexpr, _menhir_box_program) _menhir_state
    (** State 095.
        Stack shape : parenexpr.
        Start symbol: program. *)

  | MenhirState101 : ((('s, _menhir_box_program) _menhir_cell1_IF, _menhir_box_program) _menhir_cell1_expr, _menhir_box_program) _menhir_state
    (** State 101.
        Stack shape : IF expr.
        Start symbol: program. *)

  | MenhirState103 : (((('s, _menhir_box_program) _menhir_cell1_IF, _menhir_box_program) _menhir_cell1_expr, _menhir_box_program) _menhir_cell1_expr, _menhir_box_program) _menhir_state
    (** State 103.
        Stack shape : IF expr expr.
        Start symbol: program. *)

  | MenhirState106 : (((('s, _menhir_box_program) _menhir_cell1_LET _menhir_cell0_T_IDENT, _menhir_box_program) _menhir_cell1_list_T_IDENT_ _menhir_cell0_EQ_OP, _menhir_box_program) _menhir_cell1_expr, _menhir_box_program) _menhir_state
    (** State 106.
        Stack shape : LET T_IDENT list(T_IDENT) EQ_OP expr.
        Start symbol: program. *)

  | MenhirState112 : (('s, _menhir_box_program) _menhir_cell1_expr, _menhir_box_program) _menhir_state
    (** State 112.
        Stack shape : expr.
        Start symbol: program. *)

  | MenhirState117 : (('s, _menhir_box_program) _menhir_cell1_toplevel, _menhir_box_program) _menhir_state
    (** State 117.
        Stack shape : toplevel.
        Start symbol: program. *)


and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (Ast.kexpr)

and ('s, 'r) _menhir_cell1_fexpr = 
  | MenhirCell1_fexpr of 's * ('s, 'r) _menhir_state * (Ast.kexpr)

and ('s, 'r) _menhir_cell1_list_T_IDENT_ = 
  | MenhirCell1_list_T_IDENT_ of 's * ('s, 'r) _menhir_state * (string list)

and ('s, 'r) _menhir_cell1_nonempty_list_T_IDENT_ = 
  | MenhirCell1_nonempty_list_T_IDENT_ of 's * ('s, 'r) _menhir_state * (string list)

and ('s, 'r) _menhir_cell1_parenexpr = 
  | MenhirCell1_parenexpr of 's * ('s, 'r) _menhir_state * (Ast.kexpr)

and ('s, 'r) _menhir_cell1_toplevel = 
  | MenhirCell1_toplevel of 's * ('s, 'r) _menhir_state * (Ast.toplevel)

and ('s, 'r) _menhir_cell1_typesig = 
  | MenhirCell1_typesig of 's * ('s, 'r) _menhir_state * (Ast.typesig)

and ('s, 'r) _menhir_cell1_typesig_i = 
  | MenhirCell1_typesig_i of 's * ('s, 'r) _menhir_state * (Ast.typesig)

and 's _menhir_cell0_ADD_OP = 
  | MenhirCell0_ADD_OP of 's * (
# 55 "bin/parser.mly"
      (string)
# 409 "bin/parser.ml"
)

and 's _menhir_cell0_AND_OP = 
  | MenhirCell0_AND_OP of 's * (
# 67 "bin/parser.mly"
      (string)
# 416 "bin/parser.ml"
)

and 's _menhir_cell0_AT_OP = 
  | MenhirCell0_AT_OP of 's * (
# 61 "bin/parser.mly"
      (string)
# 423 "bin/parser.ml"
)

and ('s, 'r) _menhir_cell1_BANG_OP = 
  | MenhirCell1_BANG_OP of 's * ('s, 'r) _menhir_state * (
# 46 "bin/parser.mly"
      (string)
# 430 "bin/parser.ml"
)

and 's _menhir_cell0_CAR_OP = 
  | MenhirCell0_CAR_OP of 's * (
# 60 "bin/parser.mly"
      (string)
# 437 "bin/parser.ml"
)

and 's _menhir_cell0_COL_OP = 
  | MenhirCell0_COL_OP of 's * (
# 58 "bin/parser.mly"
      (string)
# 444 "bin/parser.ml"
)

and 's _menhir_cell0_DIV_OP = 
  | MenhirCell0_DIV_OP of 's * (
# 52 "bin/parser.mly"
      (string)
# 451 "bin/parser.ml"
)

and 's _menhir_cell0_DOL_OP = 
  | MenhirCell0_DOL_OP of 's * (
# 68 "bin/parser.mly"
      (string)
# 458 "bin/parser.ml"
)

and 's _menhir_cell0_EQ_OP = 
  | MenhirCell0_EQ_OP of 's * (
# 63 "bin/parser.mly"
      (string)
# 465 "bin/parser.ml"
)

and ('s, 'r) _menhir_cell1_FORALL = 
  | MenhirCell1_FORALL of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_GT_OP = 
  | MenhirCell0_GT_OP of 's * (
# 65 "bin/parser.mly"
      (string)
# 475 "bin/parser.ml"
)

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LET = 
  | MenhirCell1_LET of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_LT_OP = 
  | MenhirCell0_LT_OP of 's * (
# 64 "bin/parser.mly"
      (string)
# 491 "bin/parser.ml"
)

and 's _menhir_cell0_MOD_OP = 
  | MenhirCell0_MOD_OP of 's * (
# 53 "bin/parser.mly"
      (string)
# 498 "bin/parser.ml"
)

and 's _menhir_cell0_MUL_OP = 
  | MenhirCell0_MUL_OP of 's * (
# 51 "bin/parser.mly"
      (string)
# 505 "bin/parser.ml"
)

and 's _menhir_cell0_PIP_OP = 
  | MenhirCell0_PIP_OP of 's * (
# 66 "bin/parser.mly"
      (string)
# 512 "bin/parser.ml"
)

and 's _menhir_cell0_POW_OP = 
  | MenhirCell0_POW_OP of 's * (
# 49 "bin/parser.mly"
      (string)
# 519 "bin/parser.ml"
)

and ('s, 'r) _menhir_cell1_SIG = 
  | MenhirCell1_SIG of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_SUB_OP = 
  | MenhirCell0_SUB_OP of 's * (
# 56 "bin/parser.mly"
      (string)
# 529 "bin/parser.ml"
)

and ('s, 'r) _menhir_cell1_TILDE_OP = 
  | MenhirCell1_TILDE_OP of 's * ('s, 'r) _menhir_state * (
# 47 "bin/parser.mly"
      (string)
# 536 "bin/parser.ml"
)

and ('s, 'r) _menhir_cell1_T_IDENT = 
  | MenhirCell1_T_IDENT of 's * ('s, 'r) _menhir_state * (
# 5 "bin/parser.mly"
       (string)
# 543 "bin/parser.ml"
)

and 's _menhir_cell0_T_IDENT = 
  | MenhirCell0_T_IDENT of 's * (
# 5 "bin/parser.mly"
       (string)
# 550 "bin/parser.ml"
)

and _menhir_box_program = 
  | MenhirBox_program of (Ast.program) [@@unboxed]

let _menhir_action_01 =
  fun a args e eq ->
    (
# 221 "bin/parser.mly"
    (KAss(a, args, e))
# 561 "bin/parser.ml"
     : (Ast.kass))

let _menhir_action_02 =
  fun t ->
    (
# 139 "bin/parser.mly"
                (Ident(t))
# 569 "bin/parser.ml"
     : (Ast.kbase))

let _menhir_action_03 =
  fun t ->
    (
# 140 "bin/parser.mly"
                (Int(t))
# 577 "bin/parser.ml"
     : (Ast.kbase))

let _menhir_action_04 =
  fun t ->
    (
# 141 "bin/parser.mly"
                (Float(t))
# 585 "bin/parser.ml"
     : (Ast.kbase))

let _menhir_action_05 =
  fun t ->
    (
# 142 "bin/parser.mly"
                 (Str(t))
# 593 "bin/parser.ml"
     : (Ast.kbase))

let _menhir_action_06 =
  fun e ->
    (
# 153 "bin/parser.mly"
            (e)
# 601 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_07 =
  fun b e ->
    (
# 156 "bin/parser.mly"
                      (UnOp(b, e))
# 609 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_08 =
  fun e t ->
    (
# 157 "bin/parser.mly"
                       (UnOp(t, e))
# 617 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_09 =
  fun e ->
    (
# 158 "bin/parser.mly"
            (e)
# 625 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_10 =
  fun e1 e2 e3 ->
    (
# 202 "bin/parser.mly"
                                              (IfElse(e1, e2, e3))
# 633 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_11 =
  fun args e1 e2 t ->
    (
# 204 "bin/parser.mly"
    (LetIn(t, args, e1, e2))
# 641 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_12 =
  fun e ->
    (
# 205 "bin/parser.mly"
                           (Paren(e))
# 649 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_13 =
  fun e ->
    (
# 206 "bin/parser.mly"
             (e)
# 657 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_14 =
  fun e1 e2 ->
    (
# 209 "bin/parser.mly"
                                (Join(e1, e2))
# 665 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_15 =
  fun e ->
    (
# 210 "bin/parser.mly"
             (e)
# 673 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_16 =
  fun b ->
    (
# 213 "bin/parser.mly"
           (Base(b))
# 681 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_17 =
  fun e f ->
    (
# 161 "bin/parser.mly"
                                        (FCall(f, e))
# 689 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_18 =
  fun e ->
    (
# 162 "bin/parser.mly"
            (e)
# 697 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_19 =
  fun e1 e2 p ->
    (
# 165 "bin/parser.mly"
                               (BinOp(e1, p, e2))
# 705 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_20 =
  fun e ->
    (
# 166 "bin/parser.mly"
            (e)
# 713 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_21 =
  fun e1 e2 p ->
    (
# 169 "bin/parser.mly"
                               (BinOp(e1, p, e2))
# 721 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_22 =
  fun d e1 e2 ->
    (
# 170 "bin/parser.mly"
                               (BinOp(e1, d, e2))
# 729 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_23 =
  fun e1 e2 m ->
    (
# 171 "bin/parser.mly"
                               (BinOp(e1, m, e2))
# 737 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_24 =
  fun e ->
    (
# 172 "bin/parser.mly"
            (e)
# 745 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_25 =
  fun a e1 e2 ->
    (
# 175 "bin/parser.mly"
                               (BinOp(e1, a, e2))
# 753 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_26 =
  fun a e1 e2 ->
    (
# 176 "bin/parser.mly"
                               (BinOp(e1, a, e2))
# 761 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_27 =
  fun e ->
    (
# 177 "bin/parser.mly"
            (e)
# 769 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_28 =
  fun a e1 e2 ->
    (
# 180 "bin/parser.mly"
                               (BinOp(e1, a, e2))
# 777 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_29 =
  fun e ->
    (
# 181 "bin/parser.mly"
            (e)
# 785 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_30 =
  fun a e1 e2 ->
    (
# 184 "bin/parser.mly"
                              (BinOp(e1, a, e2))
# 793 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_31 =
  fun a e1 e2 ->
    (
# 185 "bin/parser.mly"
                               (BinOp(e1, a, e2))
# 801 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_32 =
  fun e ->
    (
# 186 "bin/parser.mly"
            (e)
# 809 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_33 =
  fun a e1 e2 ->
    (
# 189 "bin/parser.mly"
                              (BinOp(e1, a, e2))
# 817 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_34 =
  fun a e1 e2 ->
    (
# 190 "bin/parser.mly"
                              (BinOp(e1, a, e2))
# 825 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_35 =
  fun a e1 e2 ->
    (
# 191 "bin/parser.mly"
                              (BinOp(e1, a, e2))
# 833 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_36 =
  fun a e1 e2 ->
    (
# 192 "bin/parser.mly"
                               (BinOp(e1, a, e2))
# 841 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_37 =
  fun a e1 e2 ->
    (
# 193 "bin/parser.mly"
                               (BinOp(e1, a, e2))
# 849 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_38 =
  fun a e1 e2 ->
    (
# 194 "bin/parser.mly"
                               (BinOp(e1, a, e2))
# 857 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_39 =
  fun e ->
    (
# 195 "bin/parser.mly"
            (e)
# 865 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_40 =
  fun s ->
    (
# 198 "bin/parser.mly"
                                                           (Base(Tuple(s)))
# 873 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_41 =
  fun e ->
    (
# 199 "bin/parser.mly"
             (e)
# 881 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_42 =
  fun e ->
    (
# 149 "bin/parser.mly"
                             (e)
# 889 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_43 =
  fun t ->
    (
# 150 "bin/parser.mly"
                (Base(Ident(t)))
# 897 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_44 =
  fun t ->
    (
# 119 "bin/parser.mly"
                (KTypeBasic(t))
# 905 "bin/parser.ml"
     : (Ast.ktype))

let _menhir_action_45 =
  fun a b ->
    (
# 120 "bin/parser.mly"
                           (KTypeApp(TSBase(a), b))
# 913 "bin/parser.ml"
     : (Ast.ktype))

let _menhir_action_46 =
  fun a b ->
    (
# 122 "bin/parser.mly"
    (KTypeApp(a, b))
# 921 "bin/parser.ml"
     : (Ast.ktype))

let _menhir_action_47 =
  fun b t ->
    (
# 124 "bin/parser.mly"
    (KTypeApp(TSTuple(t), b))
# 929 "bin/parser.ml"
     : (Ast.ktype))

let _menhir_action_48 =
  fun () ->
    (
# 208 "<standard.mly>"
    ( [] )
# 937 "bin/parser.ml"
     : (string list))

let _menhir_action_49 =
  fun x xs ->
    (
# 210 "<standard.mly>"
    ( x :: xs )
# 945 "bin/parser.ml"
     : (string list))

let _menhir_action_50 =
  fun x ->
    (
# 218 "<standard.mly>"
    ( [ x ] )
# 953 "bin/parser.ml"
     : (string list))

let _menhir_action_51 =
  fun x xs ->
    (
# 220 "<standard.mly>"
    ( x :: xs )
# 961 "bin/parser.ml"
     : (string list))

let _menhir_action_52 =
  fun x ->
    (
# 218 "<standard.mly>"
    ( [ x ] )
# 969 "bin/parser.ml"
     : (Ast.kexpr list))

let _menhir_action_53 =
  fun x xs ->
    (
# 220 "<standard.mly>"
    ( x :: xs )
# 977 "bin/parser.ml"
     : (Ast.kexpr list))

let _menhir_action_54 =
  fun x ->
    (
# 218 "<standard.mly>"
    ( [ x ] )
# 985 "bin/parser.ml"
     : (Ast.toplevel list))

let _menhir_action_55 =
  fun x xs ->
    (
# 220 "<standard.mly>"
    ( x :: xs )
# 993 "bin/parser.ml"
     : (Ast.toplevel list))

let _menhir_action_56 =
  fun b ->
    (
# 145 "bin/parser.mly"
             (Base(b))
# 1001 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_57 =
  fun e ->
    (
# 146 "bin/parser.mly"
                            (e)
# 1009 "bin/parser.ml"
     : (Ast.kexpr))

let _menhir_action_58 =
  fun a ->
    (
# 228 "bin/parser.mly"
                                     (Program(a))
# 1017 "bin/parser.ml"
     : (Ast.program))

let _menhir_action_59 =
  fun x ->
    (
# 238 "<standard.mly>"
    ( [ x ] )
# 1025 "bin/parser.ml"
     : (Ast.kexpr list))

let _menhir_action_60 =
  fun x xs ->
    (
# 240 "<standard.mly>"
    ( x :: xs )
# 1033 "bin/parser.ml"
     : (Ast.kexpr list))

let _menhir_action_61 =
  fun x ->
    (
# 238 "<standard.mly>"
    ( [ x ] )
# 1041 "bin/parser.ml"
     : (Ast.typesig list))

let _menhir_action_62 =
  fun x xs ->
    (
# 240 "<standard.mly>"
    ( x :: xs )
# 1049 "bin/parser.ml"
     : (Ast.typesig list))

let _menhir_action_63 =
  fun a e t ->
    (
# 217 "bin/parser.mly"
    ((a, t))
# 1057 "bin/parser.ml"
     : (Ast.tdecl))

let _menhir_action_64 =
  fun a ->
    (
# 224 "bin/parser.mly"
               (TopAssign(a))
# 1065 "bin/parser.ml"
     : (Ast.toplevel))

let _menhir_action_65 =
  fun t ->
    (
# 225 "bin/parser.mly"
               (TopTDecl(t))
# 1073 "bin/parser.ml"
     : (Ast.toplevel))

let _menhir_action_66 =
  fun t ->
    (
# 134 "bin/parser.mly"
                  (t)
# 1081 "bin/parser.ml"
     : (Ast.typesig))

let _menhir_action_67 =
  fun a f ->
    (
# 136 "bin/parser.mly"
    (TSForall(f, a))
# 1089 "bin/parser.ml"
     : (Ast.typesig))

let _menhir_action_68 =
  fun k ->
    (
# 127 "bin/parser.mly"
              (TSBase(k))
# 1097 "bin/parser.ml"
     : (Ast.typesig))

let _menhir_action_69 =
  fun a b ->
    (
# 128 "bin/parser.mly"
                                        (TSMap(a, b))
# 1105 "bin/parser.ml"
     : (Ast.typesig))

let _menhir_action_70 =
  fun t ->
    (
# 129 "bin/parser.mly"
                                 (t)
# 1113 "bin/parser.ml"
     : (Ast.typesig))

let _menhir_action_71 =
  fun t ->
    (
# 131 "bin/parser.mly"
    (TSTuple(t))
# 1121 "bin/parser.ml"
     : (Ast.typesig))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ADD ->
        "ADD"
    | ADD_OP _ ->
        "ADD_OP"
    | AND ->
        "AND"
    | AND_OP _ ->
        "AND_OP"
    | AT ->
        "AT"
    | AT_OP _ ->
        "AT_OP"
    | BANG ->
        "BANG"
    | BANG_OP _ ->
        "BANG_OP"
    | BSLASH ->
        "BSLASH"
    | CAR_OP _ ->
        "CAR_OP"
    | COLON ->
        "COLON"
    | COL_OP _ ->
        "COL_OP"
    | COMMA ->
        "COMMA"
    | DIV_OP _ ->
        "DIV_OP"
    | DOLLAR ->
        "DOLLAR"
    | DOL_OP _ ->
        "DOL_OP"
    | DOT ->
        "DOT"
    | ELSE ->
        "ELSE"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
    | EQ_OP _ ->
        "EQ_OP"
    | FALSE ->
        "FALSE"
    | FOR ->
        "FOR"
    | FORALL ->
        "FORALL"
    | GT ->
        "GT"
    | GT_OP _ ->
        "GT_OP"
    | HASH ->
        "HASH"
    | IF ->
        "IF"
    | IGNORE ->
        "IGNORE"
    | IN ->
        "IN"
    | INLINE ->
        "INLINE"
    | LAND ->
        "LAND"
    | LBRACE ->
        "LBRACE"
    | LBRACK ->
        "LBRACK"
    | LET ->
        "LET"
    | LOR ->
        "LOR"
    | LPAREN ->
        "LPAREN"
    | LT ->
        "LT"
    | LT_OP _ ->
        "LT_OP"
    | MOD_OP _ ->
        "MOD_OP"
    | MUL ->
        "MUL"
    | MUL_OP _ ->
        "MUL_OP"
    | NOMANGLE ->
        "NOMANGLE"
    | PERCENT ->
        "PERCENT"
    | PIP_OP _ ->
        "PIP_OP"
    | POW_OP _ ->
        "POW_OP"
    | QMARK ->
        "QMARK"
    | RBRACE ->
        "RBRACE"
    | RBRACK ->
        "RBRACK"
    | RETURN ->
        "RETURN"
    | RPAREN ->
        "RPAREN"
    | SEMICOLON ->
        "SEMICOLON"
    | SIG ->
        "SIG"
    | SLASH ->
        "SLASH"
    | STRAIGHT ->
        "STRAIGHT"
    | SUB ->
        "SUB"
    | SUB_OP _ ->
        "SUB_OP"
    | THEN ->
        "THEN"
    | TILDE ->
        "TILDE"
    | TILDE_OP _ ->
        "TILDE_OP"
    | TRUE ->
        "TRUE"
    | TS_TO ->
        "TS_TO"
    | T_FLOAT _ ->
        "T_FLOAT"
    | T_IDENT _ ->
        "T_IDENT"
    | T_INT _ ->
        "T_INT"
    | T_STRING _ ->
        "T_STRING"
    | WHILE ->
        "WHILE"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37-39"]
  
  let rec _menhir_run_122 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_program =
    fun _menhir_stack _v ->
      let a = _v in
      let _v = _menhir_action_58 a in
      MenhirBox_program _v
  
  let rec _menhir_goto_nonempty_list_toplevel_ : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState000 ->
          _menhir_run_122 _menhir_stack _v
      | MenhirState117 ->
          _menhir_run_119 _menhir_stack _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_119 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_toplevel -> _ -> _menhir_box_program =
    fun _menhir_stack _v ->
      let MenhirCell1_toplevel (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_55 x xs in
      _menhir_goto_nonempty_list_toplevel_ _menhir_stack _v _menhir_s
  
  let rec _menhir_run_001 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_SIG (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_IDENT _v ->
          let _menhir_stack = MenhirCell0_T_IDENT (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | EQ_OP _v_0 ->
              let _menhir_stack = MenhirCell0_EQ_OP (_menhir_stack, _v_0) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | T_IDENT _v_1 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let t = _v_1 in
                  let _v = _menhir_action_44 t in
                  _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState003 _tok
              | LPAREN ->
                  _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState003
              | FORALL ->
                  _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState003
              | _ ->
                  _eRR 3)
          | _ ->
              _eRR 2)
      | _ ->
          _eRR 1
  
  and _menhir_run_014 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | T_IDENT _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (b, a) = (_v_0, _v) in
          let _v = _menhir_action_45 a b in
          _menhir_goto_ktype _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA | EOF | LET | RPAREN | SIG | TS_TO ->
          let k = _v in
          let _v = _menhir_action_68 k in
          _menhir_goto_typesig_i _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 14
  
  and _menhir_goto_ktype : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_typesig_i : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState003 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState020 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState005 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState012 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_016 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TS_TO ->
          let _menhir_stack = MenhirCell1_typesig_i (_menhir_stack, _menhir_s, _v) in
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | EOF | LET | RPAREN | SIG ->
          let t = _v in
          let _v = _menhir_action_66 t in
          _menhir_goto_typesig _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_012 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_typesig_i -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v in
          let _v = _menhir_action_44 t in
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState012 _tok
      | LPAREN ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState012
      | _ ->
          _eRR 12
  
  and _menhir_run_005 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v in
          let _v = _menhir_action_44 t in
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState005 _tok
      | LPAREN ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState005
      | FORALL ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState005
      | _ ->
          _eRR 5
  
  and _menhir_run_006 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_FORALL (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_IDENT _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState006
      | _ ->
          _eRR 6
  
  and _menhir_run_007 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_T_IDENT (_menhir_stack, _menhir_s, _v) in
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState007
      | COMMA ->
          let x = _v in
          let _v = _menhir_action_50 x in
          _menhir_goto_nonempty_list_T_IDENT_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR 7
  
  and _menhir_goto_nonempty_list_T_IDENT_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState006 ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState007 ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_009 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_FORALL as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_nonempty_list_T_IDENT_ (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_IDENT _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_0 in
          let _v = _menhir_action_44 t in
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState010 _tok
      | LPAREN ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState010
      | _ ->
          _eRR 10
  
  and _menhir_run_008 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_T_IDENT -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_T_IDENT (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_51 x xs in
      _menhir_goto_nonempty_list_T_IDENT_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_typesig : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState003 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState020 ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState005 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_026 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_SIG _menhir_cell0_T_IDENT _menhir_cell0_EQ_OP -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_EQ_OP (_menhir_stack, e) = _menhir_stack in
      let MenhirCell0_T_IDENT (_menhir_stack, a) = _menhir_stack in
      let MenhirCell1_SIG (_menhir_stack, _menhir_s) = _menhir_stack in
      let t = _v in
      let _v = _menhir_action_63 a e t in
      let t = _v in
      let _v = _menhir_action_65 t in
      _menhir_goto_toplevel _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_toplevel : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SIG ->
          let _menhir_stack = MenhirCell1_toplevel (_menhir_stack, _menhir_s, _v) in
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState117
      | LET ->
          let _menhir_stack = MenhirCell1_toplevel (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState117
      | EOF ->
          let x = _v in
          let _v = _menhir_action_54 x in
          _menhir_goto_nonempty_list_toplevel_ _menhir_stack _v _menhir_s
      | _ ->
          _eRR 117
  
  and _menhir_run_027 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LET (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_IDENT _v ->
          let _menhir_stack = MenhirCell0_T_IDENT (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | T_IDENT _v ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState028
          | EQ_OP _ ->
              let _v = _menhir_action_48 () in
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState028 _tok
          | _ ->
              _eRR 28)
      | _ ->
          _eRR 27
  
  and _menhir_run_029 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_T_IDENT (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_IDENT _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState029
      | EQ_OP _ ->
          let _v = _menhir_action_48 () in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR 29
  
  and _menhir_run_030 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_T_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_T_IDENT (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_49 x xs in
      _menhir_goto_list_T_IDENT_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_list_T_IDENT_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState040 ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState028 ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_041 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_LET _menhir_cell0_T_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_list_T_IDENT_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ_OP _v_0 ->
          let _menhir_stack = MenhirCell0_EQ_OP (_menhir_stack, _v_0) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | T_STRING _v_1 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = _v_1 in
              let _v = _menhir_action_05 t in
              _menhir_run_065_spec_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | T_INT _v_3 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = _v_3 in
              let _v = _menhir_action_03 t in
              _menhir_run_065_spec_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | T_IDENT _v_5 ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState042
          | T_FLOAT _v_6 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = _v_6 in
              let _v = _menhir_action_04 t in
              _menhir_run_065_spec_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | TILDE_OP _v_8 ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState042
          | LPAREN ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState042
          | LET ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState042
          | IF ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState042
          | BANG_OP _v_9 ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_9 MenhirState042
          | _ ->
              _eRR 42)
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_065_spec_042 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_LET _menhir_cell0_T_IDENT, _menhir_box_program) _menhir_cell1_list_T_IDENT_ _menhir_cell0_EQ_OP -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState042 _tok
  
  and _menhir_run_105 : type  ttv_stack. (((ttv_stack, _menhir_box_program) _menhir_cell1_LET _menhir_cell0_T_IDENT, _menhir_box_program) _menhir_cell1_list_T_IDENT_ _menhir_cell0_EQ_OP as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SUB_OP _v_0 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | SEMICOLON ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POW_OP _v_1 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | PIP_OP _v_2 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | MUL_OP _v_3 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | MOD_OP _v_4 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4
      | LT_OP _v_5 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
      | IN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | T_STRING _v_6 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = _v_6 in
              let _v = _menhir_action_05 t in
              _menhir_run_065_spec_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | T_INT _v_8 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = _v_8 in
              let _v = _menhir_action_03 t in
              _menhir_run_065_spec_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | T_IDENT _v_10 ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_10 MenhirState106
          | T_FLOAT _v_11 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = _v_11 in
              let _v = _menhir_action_04 t in
              _menhir_run_065_spec_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | TILDE_OP _v_13 ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v_13 MenhirState106
          | LPAREN ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState106
          | LET ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState106
          | IF ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState106
          | BANG_OP _v_14 ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_14 MenhirState106
          | _ ->
              _eRR 106)
      | GT_OP _v_15 ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _v_15
      | EQ_OP _v_16 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v_16
      | DOL_OP _v_17 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v_17
      | DIV_OP _v_18 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_18
      | COL_OP _v_19 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v_19
      | CAR_OP _v_20 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_20
      | AT_OP _v_21 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_21
      | AND_OP _v_22 ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v_22
      | ADD_OP _v_23 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v_23
      | _ ->
          _eRR 105
  
  and _menhir_run_061 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_SUB_OP (_menhir_stack, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_0 in
          let _v = _menhir_action_05 t in
          _menhir_run_065_spec_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v_2 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_2 in
          let _v = _menhir_action_03 t in
          _menhir_run_065_spec_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v_4 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState061
      | T_FLOAT _v_5 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_5 in
          let _v = _menhir_action_04 t in
          _menhir_run_065_spec_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TILDE_OP _v_7 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState061
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState061
      | LET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState061
      | IF ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState061
      | BANG_OP _v_8 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState061
      | _ ->
          _eRR 61
  
  and _menhir_run_065_spec_061 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_SUB_OP -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState061 _tok
  
  and _menhir_run_062 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_SUB_OP as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | POW_OP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | MUL_OP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | MOD_OP _v_2 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | DIV_OP _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | ADD_OP _ | AND_OP _ | AT_OP _ | CAR_OP _ | COL_OP _ | COMMA | DOL_OP _ | ELSE | EOF | EQ_OP _ | GT_OP _ | IN | LET | LT_OP _ | PIP_OP _ | RPAREN | SEMICOLON | SIG | SUB_OP _ | THEN ->
          let MenhirCell0_SUB_OP (_menhir_stack, a) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_26 a e1 e2 in
          _menhir_goto_expr5 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 62
  
  and _menhir_run_063 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_POW_OP (_menhir_stack, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_0 in
          let _v = _menhir_action_05 t in
          _menhir_run_065_spec_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v_2 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_2 in
          let _v = _menhir_action_03 t in
          _menhir_run_065_spec_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v_4 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState063
      | T_FLOAT _v_5 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_5 in
          let _v = _menhir_action_04 t in
          _menhir_run_065_spec_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TILDE_OP _v_7 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState063
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | LET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | IF ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | BANG_OP _v_8 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState063
      | _ ->
          _eRR 63
  
  and _menhir_run_065_spec_063 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_POW_OP -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState063 _tok
  
  and _menhir_run_064 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_POW_OP as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | POW_OP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | ADD_OP _ | AND_OP _ | AT_OP _ | CAR_OP _ | COL_OP _ | COMMA | DIV_OP _ | DOL_OP _ | ELSE | EOF | EQ_OP _ | GT_OP _ | IN | LET | LT_OP _ | MOD_OP _ | MUL_OP _ | PIP_OP _ | RPAREN | SEMICOLON | SIG | SUB_OP _ | THEN ->
          let MenhirCell0_POW_OP (_menhir_stack, p) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_19 e1 e2 p in
          _menhir_goto_expr3 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 64
  
  and _menhir_goto_expr3 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let e = _v in
      let _v = _menhir_action_18 e in
      _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr2 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let e = _v in
      let _v = _menhir_action_09 e in
      _menhir_goto_expr1 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr1 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState032 ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState112 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState038 ->
          _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState106 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState042 ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState101 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState043 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState044 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState092 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState090 ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState076 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState074 ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState070 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState068 ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState066 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState063 ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_116 : type  ttv_stack. (((ttv_stack, _menhir_box_program) _menhir_cell1_LET _menhir_cell0_T_IDENT, _menhir_box_program) _menhir_cell1_list_T_IDENT_ _menhir_cell0_EQ_OP as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB_OP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | SEMICOLON ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POW_OP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | PIP_OP _v_2 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | MUL_OP _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | MOD_OP _v_4 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4
      | LT_OP _v_5 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
      | GT_OP _v_6 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6
      | EQ_OP _v_7 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7
      | DOL_OP _v_8 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8
      | DIV_OP _v_9 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_9
      | COL_OP _v_10 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v_10
      | CAR_OP _v_11 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_11
      | AT_OP _v_12 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_12
      | AND_OP _v_13 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v_13
      | ADD_OP _v_14 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v_14
      | EOF | LET | SIG ->
          let MenhirCell0_EQ_OP (_menhir_stack, eq) = _menhir_stack in
          let MenhirCell1_list_T_IDENT_ (_menhir_stack, _, args) = _menhir_stack in
          let MenhirCell0_T_IDENT (_menhir_stack, a) = _menhir_stack in
          let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_01 a args e eq in
          let a = _v in
          let _v = _menhir_action_64 a in
          _menhir_goto_toplevel _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 116
  
  and _menhir_run_072 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v in
          let _v = _menhir_action_05 t in
          _menhir_run_065_spec_072 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v in
          let _v = _menhir_action_03 t in
          _menhir_run_065_spec_072 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState072
      | T_FLOAT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v in
          let _v = _menhir_action_04 t in
          _menhir_run_065_spec_072 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TILDE_OP _v ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState072
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState072
      | LET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState072
      | IF ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState072
      | BANG_OP _v ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState072
      | _ ->
          _eRR 72
  
  and _menhir_run_065_spec_072 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState072 _tok
  
  and _menhir_run_073 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB_OP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | SEMICOLON ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POW_OP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | PIP_OP _v_2 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | MUL_OP _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | MOD_OP _v_4 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4
      | LT_OP _v_5 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
      | GT_OP _v_6 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6
      | EQ_OP _v_7 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7
      | DOL_OP _v_8 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8
      | DIV_OP _v_9 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_9
      | COL_OP _v_10 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v_10
      | CAR_OP _v_11 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_11
      | AT_OP _v_12 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_12
      | AND_OP _v_13 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v_13
      | ADD_OP _v_14 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v_14
      | COMMA | ELSE | EOF | IN | LET | RPAREN | SIG | THEN ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_14 e1 e2 in
          let e = _v in
          let _v = _menhir_action_13 e in
          _menhir_goto_expr10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 73
  
  and _menhir_run_074 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_PIP_OP (_menhir_stack, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_0 in
          let _v = _menhir_action_05 t in
          _menhir_run_065_spec_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v_2 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_2 in
          let _v = _menhir_action_03 t in
          _menhir_run_065_spec_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v_4 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState074
      | T_FLOAT _v_5 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_5 in
          let _v = _menhir_action_04 t in
          _menhir_run_065_spec_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TILDE_OP _v_7 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState074
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | LET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | IF ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | BANG_OP _v_8 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState074
      | _ ->
          _eRR 74
  
  and _menhir_run_065_spec_074 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_PIP_OP -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState074 _tok
  
  and _menhir_run_075 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_PIP_OP as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB_OP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | POW_OP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | MUL_OP _v_2 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | MOD_OP _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | DIV_OP _v_4 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4
      | COL_OP _v_5 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
      | CAR_OP _v_6 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6
      | AT_OP _v_7 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7
      | ADD_OP _v_8 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8
      | AND_OP _ | COMMA | DOL_OP _ | ELSE | EOF | EQ_OP _ | GT_OP _ | IN | LET | LT_OP _ | PIP_OP _ | RPAREN | SEMICOLON | SIG | THEN ->
          let MenhirCell0_PIP_OP (_menhir_stack, a) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_36 a e1 e2 in
          _menhir_goto_expr8 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 75
  
  and _menhir_run_066 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_MUL_OP (_menhir_stack, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_0 in
          let _v = _menhir_action_05 t in
          _menhir_run_065_spec_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v_2 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_2 in
          let _v = _menhir_action_03 t in
          _menhir_run_065_spec_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v_4 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState066
      | T_FLOAT _v_5 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_5 in
          let _v = _menhir_action_04 t in
          _menhir_run_065_spec_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TILDE_OP _v_7 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState066
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | LET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | IF ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | BANG_OP _v_8 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState066
      | _ ->
          _eRR 66
  
  and _menhir_run_065_spec_066 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_MUL_OP -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState066 _tok
  
  and _menhir_run_067 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_MUL_OP as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | POW_OP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | ADD_OP _ | AND_OP _ | AT_OP _ | CAR_OP _ | COL_OP _ | COMMA | DIV_OP _ | DOL_OP _ | ELSE | EOF | EQ_OP _ | GT_OP _ | IN | LET | LT_OP _ | MOD_OP _ | MUL_OP _ | PIP_OP _ | RPAREN | SEMICOLON | SIG | SUB_OP _ | THEN ->
          let MenhirCell0_MUL_OP (_menhir_stack, p) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_21 e1 e2 p in
          _menhir_goto_expr4 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 67
  
  and _menhir_goto_expr4 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let e = _v in
      let _v = _menhir_action_20 e in
      _menhir_goto_expr3 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_035 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ADD_OP _ | AND_OP _ | AT_OP _ | CAR_OP _ | COL_OP _ | COMMA | DIV_OP _ | DOL_OP _ | ELSE | EOF | EQ_OP _ | GT_OP _ | IN | LET | LT_OP _ | MOD_OP _ | MUL_OP _ | PIP_OP _ | POW_OP _ | RPAREN | SEMICOLON | SIG | SUB_OP _ | THEN ->
          let t = _v in
          let _v = _menhir_action_02 t in
          _menhir_goto_base _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | LPAREN | T_FLOAT _ | T_IDENT _ | T_INT _ | T_STRING _ ->
          let t = _v in
          let _v = _menhir_action_43 t in
          _menhir_goto_fexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 35
  
  and _menhir_goto_base : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState045 ->
          _menhir_run_097_spec_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState095 ->
          _menhir_run_097_spec_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState032 ->
          _menhir_run_065_spec_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState037 ->
          _menhir_run_065_spec_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState038 ->
          _menhir_run_065_spec_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState112 ->
          _menhir_run_065_spec_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState042 ->
          _menhir_run_065_spec_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState106 ->
          _menhir_run_065_spec_106 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState043 ->
          _menhir_run_065_spec_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState101 ->
          _menhir_run_065_spec_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState103 ->
          _menhir_run_065_spec_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState044 ->
          _menhir_run_065_spec_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState047 ->
          _menhir_run_065_spec_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState072 ->
          _menhir_run_065_spec_072 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState092 ->
          _menhir_run_065_spec_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState090 ->
          _menhir_run_065_spec_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState088 ->
          _menhir_run_065_spec_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState086 ->
          _menhir_run_065_spec_086 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState084 ->
          _menhir_run_065_spec_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState074 ->
          _menhir_run_065_spec_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState080 ->
          _menhir_run_065_spec_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState082 ->
          _menhir_run_065_spec_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState076 ->
          _menhir_run_065_spec_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState078 ->
          _menhir_run_065_spec_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState061 ->
          _menhir_run_065_spec_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState070 ->
          _menhir_run_065_spec_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState068 ->
          _menhir_run_065_spec_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState066 ->
          _menhir_run_065_spec_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState063 ->
          _menhir_run_065_spec_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_097_spec_045 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_fexpr -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let b = _v in
      let _v = _menhir_action_56 b in
      _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState045 _tok
  
  and _menhir_run_095 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _menhir_stack = MenhirCell1_parenexpr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_0 in
          let _v = _menhir_action_05 t in
          _menhir_run_097_spec_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v_2 ->
          let _menhir_stack = MenhirCell1_parenexpr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_2 in
          let _v = _menhir_action_03 t in
          _menhir_run_097_spec_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v_4 ->
          let _menhir_stack = MenhirCell1_parenexpr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_4 in
          let _v = _menhir_action_02 t in
          _menhir_run_097_spec_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_FLOAT _v_6 ->
          let _menhir_stack = MenhirCell1_parenexpr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_6 in
          let _v = _menhir_action_04 t in
          _menhir_run_097_spec_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | LPAREN ->
          let _menhir_stack = MenhirCell1_parenexpr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState095
      | ADD_OP _ | AND_OP _ | AT_OP _ | CAR_OP _ | COL_OP _ | COMMA | DIV_OP _ | DOL_OP _ | ELSE | EOF | EQ_OP _ | GT_OP _ | IN | LET | LT_OP _ | MOD_OP _ | MUL_OP _ | PIP_OP _ | POW_OP _ | RPAREN | SEMICOLON | SIG | SUB_OP _ | THEN ->
          let x = _v in
          let _v = _menhir_action_52 x in
          _menhir_goto_nonempty_list_parenexpr_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 95
  
  and _menhir_run_097_spec_095 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_parenexpr -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let b = _v in
      let _v = _menhir_action_56 b in
      _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState095 _tok
  
  and _menhir_run_047 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v in
          let _v = _menhir_action_05 t in
          _menhir_run_065_spec_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v in
          let _v = _menhir_action_03 t in
          _menhir_run_065_spec_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState047
      | T_FLOAT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v in
          let _v = _menhir_action_04 t in
          _menhir_run_065_spec_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TILDE_OP _v ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState047
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState047
      | LET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState047
      | IF ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState047
      | BANG_OP _v ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState047
      | _ ->
          _eRR 47
  
  and _menhir_run_065_spec_047 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState047 _tok
  
  and _menhir_run_060 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB_OP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | SEMICOLON ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_57 e in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | POW_OP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | PIP_OP _v_2 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | MUL_OP _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | MOD_OP _v_4 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4
      | LT_OP _v_5 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
      | GT_OP _v_6 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6
      | EQ_OP _v_7 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7
      | DOL_OP _v_8 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8
      | DIV_OP _v_9 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_9
      | COL_OP _v_10 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v_10
      | CAR_OP _v_11 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_11
      | AT_OP _v_12 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_12
      | AND_OP _v_13 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v_13
      | ADD_OP _v_14 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v_14
      | _ ->
          _eRR 60
  
  and _menhir_run_068 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_MOD_OP (_menhir_stack, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_0 in
          let _v = _menhir_action_05 t in
          _menhir_run_065_spec_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v_2 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_2 in
          let _v = _menhir_action_03 t in
          _menhir_run_065_spec_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v_4 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState068
      | T_FLOAT _v_5 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_5 in
          let _v = _menhir_action_04 t in
          _menhir_run_065_spec_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TILDE_OP _v_7 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState068
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState068
      | LET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState068
      | IF ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState068
      | BANG_OP _v_8 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState068
      | _ ->
          _eRR 68
  
  and _menhir_run_065_spec_068 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_MOD_OP -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState068 _tok
  
  and _menhir_run_069 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_MOD_OP as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | POW_OP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | ADD_OP _ | AND_OP _ | AT_OP _ | CAR_OP _ | COL_OP _ | COMMA | DIV_OP _ | DOL_OP _ | ELSE | EOF | EQ_OP _ | GT_OP _ | IN | LET | LT_OP _ | MOD_OP _ | MUL_OP _ | PIP_OP _ | RPAREN | SEMICOLON | SIG | SUB_OP _ | THEN ->
          let MenhirCell0_MOD_OP (_menhir_stack, m) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_23 e1 e2 m in
          _menhir_goto_expr4 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 69
  
  and _menhir_run_037 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_TILDE_OP (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_0 in
          let _v = _menhir_action_05 t in
          _menhir_run_065_spec_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v_2 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_2 in
          let _v = _menhir_action_03 t in
          _menhir_run_065_spec_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v_4 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState037
      | T_FLOAT _v_5 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_5 in
          let _v = _menhir_action_04 t in
          _menhir_run_065_spec_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TILDE_OP _v_7 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState037
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState037
      | LET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState037
      | IF ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState037
      | BANG_OP _v_8 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState037
      | _ ->
          _eRR 37
  
  and _menhir_run_065_spec_037 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_TILDE_OP -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_115 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_TILDE_OP -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_TILDE_OP (_menhir_stack, _menhir_s, t) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_08 e t in
      _menhir_goto_expr1 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_038 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v in
          let _v = _menhir_action_05 t in
          _menhir_run_065_spec_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v in
          let _v = _menhir_action_03 t in
          _menhir_run_065_spec_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState038
      | T_FLOAT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v in
          let _v = _menhir_action_04 t in
          _menhir_run_065_spec_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TILDE_OP _v ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState038
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState038
      | LET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState038
      | IF ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState038
      | BANG_OP _v ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState038
      | _ ->
          _eRR 38
  
  and _menhir_run_065_spec_038 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState038 _tok
  
  and _menhir_run_110 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB_OP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | SEMICOLON ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ADD_OP _ | AND_OP _ | AT_OP _ | CAR_OP _ | COL_OP _ | COMMA | DIV_OP _ | DOL_OP _ | ELSE | EOF | EQ_OP _ | GT_OP _ | IN | LET | LT_OP _ | MOD_OP _ | MUL_OP _ | PIP_OP _ | POW_OP _ | RPAREN | SEMICOLON | SIG | SUB_OP _ | THEN ->
              let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
              let e = _v in
              let _v = _menhir_action_12 e in
              _menhir_goto_expr10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | LPAREN | T_FLOAT _ | T_IDENT _ | T_INT _ | T_STRING _ ->
              let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
              let e = _v in
              let _v = _menhir_action_42 e in
              _menhir_goto_fexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR 111)
      | POW_OP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | PIP_OP _v_2 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | MUL_OP _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | MOD_OP _v_4 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4
      | LT_OP _v_5 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
      | GT_OP _v_6 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6
      | EQ_OP _v_7 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7
      | DOL_OP _v_8 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8
      | DIV_OP _v_9 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_9
      | COMMA ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COL_OP _v_10 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v_10
      | CAR_OP _v_11 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_11
      | AT_OP _v_12 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_12
      | AND_OP _v_13 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v_13
      | ADD_OP _v_14 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v_14
      | _ ->
          _eRR 110
  
  and _menhir_goto_expr10 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let e = _v in
      let _v = _menhir_action_41 e in
      _menhir_goto_expr9 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr9 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let e = _v in
      let _v = _menhir_action_39 e in
      _menhir_goto_expr8 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr8 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let e = _v in
      let _v = _menhir_action_32 e in
      _menhir_goto_expr7 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr7 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let e = _v in
      let _v = _menhir_action_29 e in
      _menhir_goto_expr6 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr6 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let e = _v in
      let _v = _menhir_action_27 e in
      _menhir_goto_expr5 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr5 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let e = _v in
      let _v = _menhir_action_24 e in
      _menhir_goto_expr4 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_fexpr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_fexpr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_0 in
          let _v = _menhir_action_05 t in
          _menhir_run_097_spec_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v_2 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_2 in
          let _v = _menhir_action_03 t in
          _menhir_run_097_spec_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v_4 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_4 in
          let _v = _menhir_action_02 t in
          _menhir_run_097_spec_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_FLOAT _v_6 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_6 in
          let _v = _menhir_action_04 t in
          _menhir_run_097_spec_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | LPAREN ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState045
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_084 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_LT_OP (_menhir_stack, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_0 in
          let _v = _menhir_action_05 t in
          _menhir_run_065_spec_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v_2 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_2 in
          let _v = _menhir_action_03 t in
          _menhir_run_065_spec_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v_4 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState084
      | T_FLOAT _v_5 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_5 in
          let _v = _menhir_action_04 t in
          _menhir_run_065_spec_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TILDE_OP _v_7 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState084
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
      | LET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
      | IF ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
      | BANG_OP _v_8 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState084
      | _ ->
          _eRR 84
  
  and _menhir_run_065_spec_084 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_LT_OP -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState084 _tok
  
  and _menhir_run_085 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_LT_OP as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB_OP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | POW_OP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | MUL_OP _v_2 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | MOD_OP _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | DIV_OP _v_4 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4
      | COL_OP _v_5 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
      | CAR_OP _v_6 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6
      | AT_OP _v_7 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7
      | ADD_OP _v_8 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8
      | AND_OP _ | COMMA | DOL_OP _ | ELSE | EOF | EQ_OP _ | GT_OP _ | IN | LET | LT_OP _ | PIP_OP _ | RPAREN | SEMICOLON | SIG | THEN ->
          let MenhirCell0_LT_OP (_menhir_stack, a) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_34 a e1 e2 in
          _menhir_goto_expr8 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 85
  
  and _menhir_run_070 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_DIV_OP (_menhir_stack, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_0 in
          let _v = _menhir_action_05 t in
          _menhir_run_065_spec_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v_2 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_2 in
          let _v = _menhir_action_03 t in
          _menhir_run_065_spec_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v_4 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState070
      | T_FLOAT _v_5 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_5 in
          let _v = _menhir_action_04 t in
          _menhir_run_065_spec_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TILDE_OP _v_7 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState070
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | LET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | IF ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | BANG_OP _v_8 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState070
      | _ ->
          _eRR 70
  
  and _menhir_run_065_spec_070 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_DIV_OP -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState070 _tok
  
  and _menhir_run_071 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_DIV_OP as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | POW_OP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | ADD_OP _ | AND_OP _ | AT_OP _ | CAR_OP _ | COL_OP _ | COMMA | DIV_OP _ | DOL_OP _ | ELSE | EOF | EQ_OP _ | GT_OP _ | IN | LET | LT_OP _ | MOD_OP _ | MUL_OP _ | PIP_OP _ | RPAREN | SEMICOLON | SIG | SUB_OP _ | THEN ->
          let MenhirCell0_DIV_OP (_menhir_stack, d) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_22 d e1 e2 in
          _menhir_goto_expr4 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 71
  
  and _menhir_run_039 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LET (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_IDENT _v ->
          let _menhir_stack = MenhirCell0_T_IDENT (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | T_IDENT _v ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState040
          | EQ_OP _ ->
              let _v = _menhir_action_48 () in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState040 _tok
          | _ ->
              _eRR 40)
      | _ ->
          _eRR 39
  
  and _menhir_run_043 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v in
          let _v = _menhir_action_05 t in
          _menhir_run_065_spec_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v in
          let _v = _menhir_action_03 t in
          _menhir_run_065_spec_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState043
      | T_FLOAT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v in
          let _v = _menhir_action_04 t in
          _menhir_run_065_spec_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TILDE_OP _v ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState043
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState043
      | LET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState043
      | IF ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState043
      | BANG_OP _v ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState043
      | _ ->
          _eRR 43
  
  and _menhir_run_065_spec_043 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_IF -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState043 _tok
  
  and _menhir_run_100 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | THEN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | T_STRING _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = _v_0 in
              let _v = _menhir_action_05 t in
              _menhir_run_065_spec_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | T_INT _v_2 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = _v_2 in
              let _v = _menhir_action_03 t in
              _menhir_run_065_spec_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | T_IDENT _v_4 ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState101
          | T_FLOAT _v_5 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = _v_5 in
              let _v = _menhir_action_04 t in
              _menhir_run_065_spec_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | TILDE_OP _v_7 ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState101
          | LPAREN ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
          | LET ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
          | IF ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
          | BANG_OP _v_8 ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState101
          | _ ->
              _eRR 101)
      | SUB_OP _v_9 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v_9
      | SEMICOLON ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POW_OP _v_10 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_10
      | PIP_OP _v_11 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v_11
      | MUL_OP _v_12 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v_12
      | MOD_OP _v_13 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_13
      | LT_OP _v_14 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v_14
      | GT_OP _v_15 ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _v_15
      | EQ_OP _v_16 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v_16
      | DOL_OP _v_17 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v_17
      | DIV_OP _v_18 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_18
      | COL_OP _v_19 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v_19
      | CAR_OP _v_20 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_20
      | AT_OP _v_21 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_21
      | AND_OP _v_22 ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v_22
      | ADD_OP _v_23 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v_23
      | _ ->
          _eRR 100
  
  and _menhir_run_065_spec_101 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_IF, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState101 _tok
  
  and _menhir_run_102 : type  ttv_stack. (((ttv_stack, _menhir_box_program) _menhir_cell1_IF, _menhir_box_program) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SUB_OP _v_0 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | SEMICOLON ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POW_OP _v_1 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | PIP_OP _v_2 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | MUL_OP _v_3 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | MOD_OP _v_4 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4
      | LT_OP _v_5 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
      | GT_OP _v_6 ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6
      | EQ_OP _v_7 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7
      | ELSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | T_STRING _v_8 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = _v_8 in
              let _v = _menhir_action_05 t in
              _menhir_run_065_spec_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | T_INT _v_10 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = _v_10 in
              let _v = _menhir_action_03 t in
              _menhir_run_065_spec_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | T_IDENT _v_12 ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_12 MenhirState103
          | T_FLOAT _v_13 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = _v_13 in
              let _v = _menhir_action_04 t in
              _menhir_run_065_spec_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | TILDE_OP _v_15 ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v_15 MenhirState103
          | LPAREN ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState103
          | LET ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState103
          | IF ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState103
          | BANG_OP _v_16 ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_16 MenhirState103
          | _ ->
              _eRR 103)
      | DOL_OP _v_17 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v_17
      | DIV_OP _v_18 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_18
      | COL_OP _v_19 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v_19
      | CAR_OP _v_20 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_20
      | AT_OP _v_21 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_21
      | AND_OP _v_22 ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v_22
      | ADD_OP _v_23 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v_23
      | _ ->
          _eRR 102
  
  and _menhir_run_086 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_GT_OP (_menhir_stack, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_0 in
          let _v = _menhir_action_05 t in
          _menhir_run_065_spec_086 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v_2 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_2 in
          let _v = _menhir_action_03 t in
          _menhir_run_065_spec_086 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v_4 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState086
      | T_FLOAT _v_5 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_5 in
          let _v = _menhir_action_04 t in
          _menhir_run_065_spec_086 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TILDE_OP _v_7 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState086
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState086
      | LET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState086
      | IF ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState086
      | BANG_OP _v_8 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState086
      | _ ->
          _eRR 86
  
  and _menhir_run_065_spec_086 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_GT_OP -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState086 _tok
  
  and _menhir_run_087 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_GT_OP as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB_OP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | POW_OP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | MUL_OP _v_2 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | MOD_OP _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | DIV_OP _v_4 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4
      | COL_OP _v_5 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
      | CAR_OP _v_6 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6
      | AT_OP _v_7 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7
      | ADD_OP _v_8 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8
      | AND_OP _ | COMMA | DOL_OP _ | ELSE | EOF | EQ_OP _ | GT_OP _ | IN | LET | LT_OP _ | PIP_OP _ | RPAREN | SEMICOLON | SIG | THEN ->
          let MenhirCell0_GT_OP (_menhir_stack, a) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_35 a e1 e2 in
          _menhir_goto_expr8 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 87
  
  and _menhir_run_076 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_COL_OP (_menhir_stack, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_0 in
          let _v = _menhir_action_05 t in
          _menhir_run_065_spec_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v_2 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_2 in
          let _v = _menhir_action_03 t in
          _menhir_run_065_spec_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v_4 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState076
      | T_FLOAT _v_5 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_5 in
          let _v = _menhir_action_04 t in
          _menhir_run_065_spec_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TILDE_OP _v_7 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState076
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState076
      | LET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState076
      | IF ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState076
      | BANG_OP _v_8 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState076
      | _ ->
          _eRR 76
  
  and _menhir_run_065_spec_076 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_COL_OP -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState076 _tok
  
  and _menhir_run_077 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_COL_OP as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB_OP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | POW_OP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | MUL_OP _v_2 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | MOD_OP _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | DIV_OP _v_4 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4
      | COL_OP _v_5 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
      | ADD_OP _v_6 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6
      | AND_OP _ | AT_OP _ | CAR_OP _ | COMMA | DOL_OP _ | ELSE | EOF | EQ_OP _ | GT_OP _ | IN | LET | LT_OP _ | PIP_OP _ | RPAREN | SEMICOLON | SIG | THEN ->
          let MenhirCell0_COL_OP (_menhir_stack, a) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_28 a e1 e2 in
          _menhir_goto_expr6 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 77
  
  and _menhir_run_078 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_ADD_OP (_menhir_stack, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_0 in
          let _v = _menhir_action_05 t in
          _menhir_run_065_spec_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v_2 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_2 in
          let _v = _menhir_action_03 t in
          _menhir_run_065_spec_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v_4 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState078
      | T_FLOAT _v_5 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_5 in
          let _v = _menhir_action_04 t in
          _menhir_run_065_spec_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TILDE_OP _v_7 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState078
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState078
      | LET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState078
      | IF ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState078
      | BANG_OP _v_8 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState078
      | _ ->
          _eRR 78
  
  and _menhir_run_065_spec_078 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_ADD_OP -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState078 _tok
  
  and _menhir_run_079 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_ADD_OP as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | POW_OP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | MUL_OP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | MOD_OP _v_2 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | DIV_OP _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | ADD_OP _ | AND_OP _ | AT_OP _ | CAR_OP _ | COL_OP _ | COMMA | DOL_OP _ | ELSE | EOF | EQ_OP _ | GT_OP _ | IN | LET | LT_OP _ | PIP_OP _ | RPAREN | SEMICOLON | SIG | SUB_OP _ | THEN ->
          let MenhirCell0_ADD_OP (_menhir_stack, a) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_25 a e1 e2 in
          _menhir_goto_expr5 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 79
  
  and _menhir_run_044 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_BANG_OP (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_0 in
          let _v = _menhir_action_05 t in
          _menhir_run_065_spec_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v_2 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_2 in
          let _v = _menhir_action_03 t in
          _menhir_run_065_spec_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v_4 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState044
      | T_FLOAT _v_5 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_5 in
          let _v = _menhir_action_04 t in
          _menhir_run_065_spec_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TILDE_OP _v_7 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState044
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState044
      | LET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState044
      | IF ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState044
      | BANG_OP _v_8 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState044
      | _ ->
          _eRR 44
  
  and _menhir_run_065_spec_044 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_BANG_OP -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_099 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_BANG_OP -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_BANG_OP (_menhir_stack, _menhir_s, b) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_07 b e in
      _menhir_goto_expr1 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_080 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_CAR_OP (_menhir_stack, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_0 in
          let _v = _menhir_action_05 t in
          _menhir_run_065_spec_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v_2 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_2 in
          let _v = _menhir_action_03 t in
          _menhir_run_065_spec_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v_4 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState080
      | T_FLOAT _v_5 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_5 in
          let _v = _menhir_action_04 t in
          _menhir_run_065_spec_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TILDE_OP _v_7 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState080
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | LET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | IF ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | BANG_OP _v_8 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState080
      | _ ->
          _eRR 80
  
  and _menhir_run_065_spec_080 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_CAR_OP -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState080 _tok
  
  and _menhir_run_081 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_CAR_OP as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB_OP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | POW_OP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | MUL_OP _v_2 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | MOD_OP _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | DIV_OP _v_4 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4
      | COL_OP _v_5 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
      | CAR_OP _v_6 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6
      | AT_OP _v_7 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7
      | ADD_OP _v_8 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8
      | AND_OP _ | COMMA | DOL_OP _ | ELSE | EOF | EQ_OP _ | GT_OP _ | IN | LET | LT_OP _ | PIP_OP _ | RPAREN | SEMICOLON | SIG | THEN ->
          let MenhirCell0_CAR_OP (_menhir_stack, a) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_31 a e1 e2 in
          _menhir_goto_expr7 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 81
  
  and _menhir_run_082 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_AT_OP (_menhir_stack, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_0 in
          let _v = _menhir_action_05 t in
          _menhir_run_065_spec_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v_2 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_2 in
          let _v = _menhir_action_03 t in
          _menhir_run_065_spec_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v_4 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState082
      | T_FLOAT _v_5 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_5 in
          let _v = _menhir_action_04 t in
          _menhir_run_065_spec_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TILDE_OP _v_7 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState082
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState082
      | LET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState082
      | IF ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState082
      | BANG_OP _v_8 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState082
      | _ ->
          _eRR 82
  
  and _menhir_run_065_spec_082 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_AT_OP -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState082 _tok
  
  and _menhir_run_083 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_AT_OP as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB_OP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | POW_OP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | MUL_OP _v_2 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | MOD_OP _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | DIV_OP _v_4 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4
      | COL_OP _v_5 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
      | CAR_OP _v_6 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6
      | AT_OP _v_7 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7
      | ADD_OP _v_8 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8
      | AND_OP _ | COMMA | DOL_OP _ | ELSE | EOF | EQ_OP _ | GT_OP _ | IN | LET | LT_OP _ | PIP_OP _ | RPAREN | SEMICOLON | SIG | THEN ->
          let MenhirCell0_AT_OP (_menhir_stack, a) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_30 a e1 e2 in
          _menhir_goto_expr7 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 83
  
  and _menhir_run_088 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_EQ_OP (_menhir_stack, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_0 in
          let _v = _menhir_action_05 t in
          _menhir_run_065_spec_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v_2 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_2 in
          let _v = _menhir_action_03 t in
          _menhir_run_065_spec_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v_4 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState088
      | T_FLOAT _v_5 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_5 in
          let _v = _menhir_action_04 t in
          _menhir_run_065_spec_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TILDE_OP _v_7 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState088
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | LET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | IF ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | BANG_OP _v_8 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState088
      | _ ->
          _eRR 88
  
  and _menhir_run_065_spec_088 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_EQ_OP -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState088 _tok
  
  and _menhir_run_089 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_EQ_OP as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB_OP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | POW_OP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | MUL_OP _v_2 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | MOD_OP _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | DIV_OP _v_4 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4
      | COL_OP _v_5 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
      | CAR_OP _v_6 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6
      | AT_OP _v_7 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7
      | ADD_OP _v_8 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8
      | AND_OP _ | COMMA | DOL_OP _ | ELSE | EOF | EQ_OP _ | GT_OP _ | IN | LET | LT_OP _ | PIP_OP _ | RPAREN | SEMICOLON | SIG | THEN ->
          let MenhirCell0_EQ_OP (_menhir_stack, a) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_33 a e1 e2 in
          _menhir_goto_expr8 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 89
  
  and _menhir_run_065_spec_103 : type  ttv_stack. (((ttv_stack, _menhir_box_program) _menhir_cell1_IF, _menhir_box_program) _menhir_cell1_expr, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState103 _tok
  
  and _menhir_run_104 : type  ttv_stack. ((((ttv_stack, _menhir_box_program) _menhir_cell1_IF, _menhir_box_program) _menhir_cell1_expr, _menhir_box_program) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB_OP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | SEMICOLON ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POW_OP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | PIP_OP _v_2 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | MUL_OP _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | MOD_OP _v_4 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4
      | LT_OP _v_5 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
      | GT_OP _v_6 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6
      | EQ_OP _v_7 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7
      | DOL_OP _v_8 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8
      | DIV_OP _v_9 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_9
      | COL_OP _v_10 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v_10
      | CAR_OP _v_11 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_11
      | AT_OP _v_12 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_12
      | AND_OP _v_13 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v_13
      | ADD_OP _v_14 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v_14
      | COMMA | ELSE | EOF | IN | LET | RPAREN | SIG | THEN ->
          let MenhirCell1_expr (_menhir_stack, _, e2) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
          let e3 = _v in
          let _v = _menhir_action_10 e1 e2 e3 in
          _menhir_goto_expr10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 104
  
  and _menhir_run_090 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_DOL_OP (_menhir_stack, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_0 in
          let _v = _menhir_action_05 t in
          _menhir_run_065_spec_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v_2 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_2 in
          let _v = _menhir_action_03 t in
          _menhir_run_065_spec_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v_4 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState090
      | T_FLOAT _v_5 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_5 in
          let _v = _menhir_action_04 t in
          _menhir_run_065_spec_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TILDE_OP _v_7 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState090
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | LET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | IF ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | BANG_OP _v_8 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState090
      | _ ->
          _eRR 90
  
  and _menhir_run_065_spec_090 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_DOL_OP -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState090 _tok
  
  and _menhir_run_091 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_DOL_OP as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB_OP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | POW_OP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | MUL_OP _v_2 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | MOD_OP _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | DIV_OP _v_4 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4
      | COL_OP _v_5 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
      | CAR_OP _v_6 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6
      | AT_OP _v_7 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7
      | ADD_OP _v_8 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8
      | AND_OP _ | COMMA | DOL_OP _ | ELSE | EOF | EQ_OP _ | GT_OP _ | IN | LET | LT_OP _ | PIP_OP _ | RPAREN | SEMICOLON | SIG | THEN ->
          let MenhirCell0_DOL_OP (_menhir_stack, a) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_38 a e1 e2 in
          _menhir_goto_expr8 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 91
  
  and _menhir_run_092 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_AND_OP (_menhir_stack, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_0 in
          let _v = _menhir_action_05 t in
          _menhir_run_065_spec_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v_2 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_2 in
          let _v = _menhir_action_03 t in
          _menhir_run_065_spec_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v_4 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState092
      | T_FLOAT _v_5 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v_5 in
          let _v = _menhir_action_04 t in
          _menhir_run_065_spec_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TILDE_OP _v_7 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState092
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState092
      | LET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState092
      | IF ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState092
      | BANG_OP _v_8 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState092
      | _ ->
          _eRR 92
  
  and _menhir_run_065_spec_092 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_AND_OP -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState092 _tok
  
  and _menhir_run_093 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_expr _menhir_cell0_AND_OP as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB_OP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | POW_OP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | MUL_OP _v_2 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | MOD_OP _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | DIV_OP _v_4 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4
      | COL_OP _v_5 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
      | CAR_OP _v_6 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6
      | AT_OP _v_7 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7
      | ADD_OP _v_8 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8
      | AND_OP _ | COMMA | DOL_OP _ | ELSE | EOF | EQ_OP _ | GT_OP _ | IN | LET | LT_OP _ | PIP_OP _ | RPAREN | SEMICOLON | SIG | THEN ->
          let MenhirCell0_AND_OP (_menhir_stack, a) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_37 a e1 e2 in
          _menhir_goto_expr8 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 93
  
  and _menhir_run_112 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v in
          let _v = _menhir_action_05 t in
          _menhir_run_065_spec_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v in
          let _v = _menhir_action_03 t in
          _menhir_run_065_spec_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState112
      | T_FLOAT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v in
          let _v = _menhir_action_04 t in
          _menhir_run_065_spec_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TILDE_OP _v ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState112
      | LPAREN ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | LET ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | IF ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | BANG_OP _v ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState112
      | _ ->
          _eRR 112
  
  and _menhir_run_065_spec_112 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState112 _tok
  
  and _menhir_run_114 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB_OP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | SEMICOLON ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POW_OP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | PIP_OP _v_2 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | MUL_OP _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | MOD_OP _v_4 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4
      | LT_OP _v_5 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
      | GT_OP _v_6 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6
      | EQ_OP _v_7 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7
      | DOL_OP _v_8 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8
      | DIV_OP _v_9 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_9
      | COMMA ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COL_OP _v_10 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v_10
      | CAR_OP _v_11 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_11
      | AT_OP _v_12 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_12
      | AND_OP _v_13 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v_13
      | ADD_OP _v_14 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v_14
      | RPAREN ->
          let x = _v in
          let _v = _menhir_action_59 x in
          _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR 114
  
  and _menhir_goto_separated_nonempty_list_COMMA_expr_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState112 ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState038 ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_113 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_expr (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_60 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_108 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
      let s = _v in
      let _v = _menhir_action_40 s in
      _menhir_goto_expr9 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_nonempty_list_parenexpr_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState045 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState095 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_098 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_fexpr -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_fexpr (_menhir_stack, _menhir_s, f) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_17 e f in
      _menhir_goto_expr2 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_096 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_parenexpr -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_parenexpr (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_53 x xs in
      _menhir_goto_nonempty_list_parenexpr_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_065_spec_032 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_LET _menhir_cell0_T_IDENT, _menhir_box_program) _menhir_cell1_list_T_IDENT_ _menhir_cell0_EQ_OP -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState032 _tok
  
  and _menhir_run_065_spec_106 : type  ttv_stack. (((ttv_stack, _menhir_box_program) _menhir_cell1_LET _menhir_cell0_T_IDENT, _menhir_box_program) _menhir_cell1_list_T_IDENT_ _menhir_cell0_EQ_OP, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _v =
        let b = _v in
        _menhir_action_16 b
      in
      let _v =
        let e = _v in
        _menhir_action_15 e
      in
      let _v =
        let e = _v in
        _menhir_action_13 e
      in
      let _v =
        let e = _v in
        _menhir_action_41 e
      in
      let _v =
        let e = _v in
        _menhir_action_39 e
      in
      let _v =
        let e = _v in
        _menhir_action_32 e
      in
      let _v =
        let e = _v in
        _menhir_action_29 e
      in
      let _v =
        let e = _v in
        _menhir_action_27 e
      in
      let _v =
        let e = _v in
        _menhir_action_24 e
      in
      let _v =
        let e = _v in
        _menhir_action_20 e
      in
      let _v =
        let e = _v in
        _menhir_action_18 e
      in
      let _v =
        let e = _v in
        _menhir_action_09 e
      in
      let e = _v in
      let _v = _menhir_action_06 e in
      _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState106 _tok
  
  and _menhir_run_107 : type  ttv_stack. ((((ttv_stack, _menhir_box_program) _menhir_cell1_LET _menhir_cell0_T_IDENT, _menhir_box_program) _menhir_cell1_list_T_IDENT_ _menhir_cell0_EQ_OP, _menhir_box_program) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB_OP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | SEMICOLON ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer
      | POW_OP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | PIP_OP _v_2 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | MUL_OP _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | MOD_OP _v_4 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4
      | LT_OP _v_5 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
      | GT_OP _v_6 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6
      | EQ_OP _v_7 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7
      | DOL_OP _v_8 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8
      | DIV_OP _v_9 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v_9
      | COL_OP _v_10 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v_10
      | CAR_OP _v_11 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v_11
      | AT_OP _v_12 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_12
      | AND_OP _v_13 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v_13
      | ADD_OP _v_14 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v_14
      | COMMA | ELSE | EOF | IN | LET | RPAREN | SIG | THEN ->
          let MenhirCell1_expr (_menhir_stack, _, e1) = _menhir_stack in
          let MenhirCell0_EQ_OP (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_list_T_IDENT_ (_menhir_stack, _, args) = _menhir_stack in
          let MenhirCell0_T_IDENT (_menhir_stack, t) = _menhir_stack in
          let MenhirCell1_LET (_menhir_stack, _menhir_s) = _menhir_stack in
          let e2 = _v in
          let _v = _menhir_action_11 args e1 e2 t in
          _menhir_goto_expr10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 107
  
  and _menhir_run_031 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_LET _menhir_cell0_T_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_list_T_IDENT_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EQ_OP _v_0 ->
          let _menhir_stack = MenhirCell0_EQ_OP (_menhir_stack, _v_0) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | T_STRING _v_1 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = _v_1 in
              let _v = _menhir_action_05 t in
              _menhir_run_065_spec_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | T_INT _v_3 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = _v_3 in
              let _v = _menhir_action_03 t in
              _menhir_run_065_spec_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | T_IDENT _v_5 ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState032
          | T_FLOAT _v_6 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let t = _v_6 in
              let _v = _menhir_action_04 t in
              _menhir_run_065_spec_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | TILDE_OP _v_8 ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState032
          | LPAREN ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState032
          | LET ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState032
          | IF ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState032
          | BANG_OP _v_9 ->
              _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v_9 MenhirState032
          | _ ->
              _eRR 32)
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_021 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_typesig as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_typesig (_menhir_stack, _menhir_s, _v) in
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let x = _v in
          let _v = _menhir_action_61 x in
          _menhir_goto_separated_nonempty_list_COMMA_typesig_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR 21
  
  and _menhir_run_020 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_typesig -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let t = _v in
          let _v = _menhir_action_44 t in
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState020 _tok
      | LPAREN ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState020
      | FORALL ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState020
      | _ ->
          _eRR 20
  
  and _menhir_goto_separated_nonempty_list_COMMA_typesig_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState005 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState020 ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_023 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_IDENT _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let (t, b) = (_v, _v_0) in
          let _v = _menhir_action_47 b t in
          _menhir_goto_ktype _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA | EOF | LET | RPAREN | SIG | TS_TO ->
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let t = _v in
          let _v = _menhir_action_71 t in
          _menhir_goto_typesig_i _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 24
  
  and _menhir_run_022 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_typesig -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_typesig (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_62 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_typesig_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_017 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | T_IDENT _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
              let (b, a) = (_v_0, _v) in
              let _v = _menhir_action_46 a b in
              _menhir_goto_ktype _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | COMMA | EOF | LET | RPAREN | SIG | TS_TO ->
              let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
              let t = _v in
              let _v = _menhir_action_70 t in
              _menhir_goto_typesig_i _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR 18)
      | COMMA ->
          let _menhir_stack = MenhirCell1_typesig (_menhir_stack, _menhir_s, _v) in
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR 17
  
  and _menhir_run_013 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_typesig_i as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TS_TO ->
          let _menhir_stack = MenhirCell1_typesig_i (_menhir_stack, _menhir_s, _v) in
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | EOF | LET | RPAREN | SIG ->
          let MenhirCell1_typesig_i (_menhir_stack, _menhir_s, a) = _menhir_stack in
          let b = _v in
          let _v = _menhir_action_69 a b in
          _menhir_goto_typesig_i _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_011 : type  ttv_stack. (((ttv_stack, _menhir_box_program) _menhir_cell1_FORALL, _menhir_box_program) _menhir_cell1_nonempty_list_T_IDENT_ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TS_TO ->
          let _menhir_stack = MenhirCell1_typesig_i (_menhir_stack, _menhir_s, _v) in
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | EOF | LET | RPAREN | SIG ->
          let MenhirCell1_nonempty_list_T_IDENT_ (_menhir_stack, _, f) = _menhir_stack in
          let MenhirCell1_FORALL (_menhir_stack, _menhir_s) = _menhir_stack in
          let a = _v in
          let _v = _menhir_action_67 a f in
          _menhir_goto_typesig _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SIG ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | LET ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | _ ->
          _eRR 0
  
end

let program =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_program v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
