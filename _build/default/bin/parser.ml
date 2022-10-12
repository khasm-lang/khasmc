
module MenhirBasics = struct
  
  exception Error of int
  
  let _eRR =
    fun _s ->
      raise (Error _s)
  
  type token = 
    | T_STRING of (
# 8 "bin/parser.mly"
       (string)
# 15 "bin/parser.ml"
  )
    | T_INT of (
# 6 "bin/parser.mly"
       (string)
# 20 "bin/parser.ml"
  )
    | T_IDENT of (
# 5 "bin/parser.mly"
       (string)
# 25 "bin/parser.ml"
  )
    | T_FLOAT of (
# 7 "bin/parser.mly"
       (string)
# 30 "bin/parser.ml"
  )
    | TS_TO
    | TRUE
    | SUB
    | STRAIGHT
    | SLASH
    | SEMICOLON
    | RPAREN
    | RBRACK
    | RBRACE
    | QMARK
    | PERCENT
    | NOMANGLE
    | MUL
    | LT
    | LPAREN
    | LBRACK
    | LBRACE
    | KW_WHILE
    | KW_RETURN
    | KW_IF
    | KW_FOR
    | INLINE
    | IGNORE
    | HASH
    | GT
    | FALSE
    | EQ
    | EOF
    | DOT
    | COMMA
    | COLON
    | BSLASH
    | BANG
    | AT
    | AND
    | ADD
  
end

include MenhirBasics

# 1 "bin/parser.mly"
  
    open Ast

# 77 "bin/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState000 : ('s, _menhir_box_program) _menhir_state
    (** State 000.
        Stack shape : .
        Start symbol: program. *)

  | MenhirState001 : (('s, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_state
    (** State 001.
        Stack shape : T_IDENT.
        Start symbol: program. *)

  | MenhirState002 : ((('s, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_state
    (** State 002.
        Stack shape : T_IDENT T_IDENT.
        Start symbol: program. *)

  | MenhirState004 : ((('s, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_cell1_STRAIGHT, _menhir_box_program) _menhir_state
    (** State 004.
        Stack shape : T_IDENT STRAIGHT.
        Start symbol: program. *)

  | MenhirState008 : (((('s, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_cell1_STRAIGHT, _menhir_box_program) _menhir_cell1_nonempty_list_dec_, _menhir_box_program) _menhir_state
    (** State 008.
        Stack shape : T_IDENT STRAIGHT nonempty_list(dec).
        Start symbol: program. *)

  | MenhirState010 : (('s, _menhir_box_program) _menhir_cell1_LPAREN, _menhir_box_program) _menhir_state
    (** State 010.
        Stack shape : LPAREN.
        Start symbol: program. *)

  | MenhirState011 : (('s, _menhir_box_program) _menhir_cell1_AT, _menhir_box_program) _menhir_state
    (** State 011.
        Stack shape : AT.
        Start symbol: program. *)

  | MenhirState014 : (('s, _menhir_box_program) _menhir_cell1_typesig, _menhir_box_program) _menhir_state
    (** State 014.
        Stack shape : typesig.
        Start symbol: program. *)

  | MenhirState021 : (('s, _menhir_box_program) _menhir_cell1_dec, _menhir_box_program) _menhir_state
    (** State 021.
        Stack shape : dec.
        Start symbol: program. *)

  | MenhirState023 : ((('s, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_cell1_COLON, _menhir_box_program) _menhir_state
    (** State 023.
        Stack shape : T_IDENT COLON.
        Start symbol: program. *)

  | MenhirState027 : ((('s, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_cell1_list_T_IDENT_, _menhir_box_program) _menhir_state
    (** State 027.
        Stack shape : T_IDENT list(T_IDENT).
        Start symbol: program. *)

  | MenhirState033 : (('s, _menhir_box_program) _menhir_cell1_LPAREN, _menhir_box_program) _menhir_state
    (** State 033.
        Stack shape : LPAREN.
        Start symbol: program. *)

  | MenhirState039 : (('s, _menhir_box_program) _menhir_cell1_unop, _menhir_box_program) _menhir_state
    (** State 039.
        Stack shape : unop.
        Start symbol: program. *)

  | MenhirState046 : (('s, _menhir_box_program) _menhir_cell1_parenexpr _menhir_cell0_binop, _menhir_box_program) _menhir_state
    (** State 046.
        Stack shape : parenexpr binop.
        Start symbol: program. *)

  | MenhirState048 : (('s, _menhir_box_program) _menhir_cell1_LPAREN, _menhir_box_program) _menhir_state
    (** State 048.
        Stack shape : LPAREN.
        Start symbol: program. *)

  | MenhirState049 : (('s, _menhir_box_program) _menhir_cell1_func, _menhir_box_program) _menhir_state
    (** State 049.
        Stack shape : func.
        Start symbol: program. *)

  | MenhirState050 : (('s, _menhir_box_program) _menhir_cell1_parenexpr, _menhir_box_program) _menhir_state
    (** State 050.
        Stack shape : parenexpr.
        Start symbol: program. *)

  | MenhirState058 : ((('s, _menhir_box_program) _menhir_cell1_LPAREN, _menhir_box_program) _menhir_cell1_nonempty_list_unop_, _menhir_box_program) _menhir_state
    (** State 058.
        Stack shape : LPAREN nonempty_list(unop).
        Start symbol: program. *)

  | MenhirState063 : (((('s, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_cell1_list_T_IDENT_, _menhir_box_program) _menhir_cell1_LBRACE, _menhir_box_program) _menhir_state
    (** State 063.
        Stack shape : T_IDENT list(T_IDENT) LBRACE.
        Start symbol: program. *)

  | MenhirState064 : (('s, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_state
    (** State 064.
        Stack shape : T_IDENT.
        Start symbol: program. *)

  | MenhirState065 : ((('s, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_cell1_STRAIGHT, _menhir_box_program) _menhir_state
    (** State 065.
        Stack shape : T_IDENT STRAIGHT.
        Start symbol: program. *)

  | MenhirState067 : (((('s, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_cell1_STRAIGHT, _menhir_box_program) _menhir_cell1_nonempty_list_dec_, _menhir_box_program) _menhir_state
    (** State 067.
        Stack shape : T_IDENT STRAIGHT nonempty_list(dec).
        Start symbol: program. *)

  | MenhirState070 : ((('s, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_cell1_COLON, _menhir_box_program) _menhir_state
    (** State 070.
        Stack shape : T_IDENT COLON.
        Start symbol: program. *)

  | MenhirState074 : ((('s, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_cell1_list_T_IDENT_, _menhir_box_program) _menhir_state
    (** State 074.
        Stack shape : T_IDENT list(T_IDENT).
        Start symbol: program. *)

  | MenhirState075 : (((('s, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_cell1_list_T_IDENT_, _menhir_box_program) _menhir_cell1_LBRACE, _menhir_box_program) _menhir_state
    (** State 075.
        Stack shape : T_IDENT list(T_IDENT) LBRACE.
        Start symbol: program. *)

  | MenhirState076 : (('s, _menhir_box_program) _menhir_cell1_KW_WHILE, _menhir_box_program) _menhir_state
    (** State 076.
        Stack shape : KW_WHILE.
        Start symbol: program. *)

  | MenhirState078 : ((('s, _menhir_box_program) _menhir_cell1_KW_WHILE, _menhir_box_program) _menhir_cell1_expr, _menhir_box_program) _menhir_state
    (** State 078.
        Stack shape : KW_WHILE expr.
        Start symbol: program. *)

  | MenhirState079 : (('s, _menhir_box_program) _menhir_cell1_KW_RETURN, _menhir_box_program) _menhir_state
    (** State 079.
        Stack shape : KW_RETURN.
        Start symbol: program. *)

  | MenhirState082 : (('s, _menhir_box_program) _menhir_cell1_KW_IF, _menhir_box_program) _menhir_state
    (** State 082.
        Stack shape : KW_IF.
        Start symbol: program. *)

  | MenhirState084 : ((('s, _menhir_box_program) _menhir_cell1_KW_IF, _menhir_box_program) _menhir_cell1_expr, _menhir_box_program) _menhir_state
    (** State 084.
        Stack shape : KW_IF expr.
        Start symbol: program. *)

  | MenhirState085 : (('s, _menhir_box_program) _menhir_cell1_IGNORE, _menhir_box_program) _menhir_state
    (** State 085.
        Stack shape : IGNORE.
        Start symbol: program. *)

  | MenhirState089 : (('s, _menhir_box_program) _menhir_cell1_blocksub, _menhir_box_program) _menhir_state
    (** State 089.
        Stack shape : blocksub.
        Start symbol: program. *)

  | MenhirState103 : (('s, _menhir_box_program) _menhir_cell1_toplevel, _menhir_box_program) _menhir_state
    (** State 103.
        Stack shape : toplevel.
        Start symbol: program. *)


and 's _menhir_cell0_binop = 
  | MenhirCell0_binop of 's * (Ast.binop)

and ('s, 'r) _menhir_cell1_blocksub = 
  | MenhirCell1_blocksub of 's * ('s, 'r) _menhir_state * (Ast.block)

and ('s, 'r) _menhir_cell1_dec = 
  | MenhirCell1_dec of 's * ('s, 'r) _menhir_state * (Ast.attr)

and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_func = 
  | MenhirCell1_func of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_list_T_IDENT_ = 
  | MenhirCell1_list_T_IDENT_ of 's * ('s, 'r) _menhir_state * (string list)

and ('s, 'r) _menhir_cell1_nonempty_list_dec_ = 
  | MenhirCell1_nonempty_list_dec_ of 's * ('s, 'r) _menhir_state * (Ast.attr list)

and ('s, 'r) _menhir_cell1_nonempty_list_unop_ = 
  | MenhirCell1_nonempty_list_unop_ of 's * ('s, 'r) _menhir_state * (Ast.unop list)

and ('s, 'r) _menhir_cell1_parenexpr = 
  | MenhirCell1_parenexpr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_toplevel = 
  | MenhirCell1_toplevel of 's * ('s, 'r) _menhir_state * (Ast.toplevel)

and ('s, 'r) _menhir_cell1_typesig = 
  | MenhirCell1_typesig of 's * ('s, 'r) _menhir_state * (Ast.typeSig)

and ('s, 'r) _menhir_cell1_unop = 
  | MenhirCell1_unop of 's * ('s, 'r) _menhir_state * (Ast.unop)

and ('s, 'r) _menhir_cell1_AT = 
  | MenhirCell1_AT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_COLON = 
  | MenhirCell1_COLON of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_IGNORE = 
  | MenhirCell1_IGNORE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_KW_IF = 
  | MenhirCell1_KW_IF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_KW_RETURN = 
  | MenhirCell1_KW_RETURN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_KW_WHILE = 
  | MenhirCell1_KW_WHILE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LBRACE = 
  | MenhirCell1_LBRACE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_STRAIGHT = 
  | MenhirCell1_STRAIGHT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_T_IDENT = 
  | MenhirCell1_T_IDENT of 's * ('s, 'r) _menhir_state * (
# 5 "bin/parser.mly"
       (string)
# 313 "bin/parser.ml"
)

and _menhir_box_program = 
  | MenhirBox_program of (Ast.program) [@@unboxed]

let _menhir_action_01 =
  fun () ->
    (
# 92 "bin/parser.mly"
        (BinOpPlus)
# 324 "bin/parser.ml"
     : (Ast.binop))

let _menhir_action_02 =
  fun () ->
    (
# 93 "bin/parser.mly"
        (BinOpMinus)
# 332 "bin/parser.ml"
     : (Ast.binop))

let _menhir_action_03 =
  fun () ->
    (
# 94 "bin/parser.mly"
        (BinOpMul)
# 340 "bin/parser.ml"
     : (Ast.binop))

let _menhir_action_04 =
  fun () ->
    (
# 95 "bin/parser.mly"
          (BinOpDiv)
# 348 "bin/parser.ml"
     : (Ast.binop))

let _menhir_action_05 =
  fun many ->
    (
# 135 "bin/parser.mly"
                          (Many(many))
# 356 "bin/parser.ml"
     : (Ast.block))

let _menhir_action_06 =
  fun args ex id ->
    (
# 119 "bin/parser.mly"
    (Assign(id, args, ex))
# 364 "bin/parser.ml"
     : (Ast.block))

let _menhir_action_07 =
  fun args bl id ->
    (
# 122 "bin/parser.mly"
    (AssignBlock(id, args, bl))
# 372 "bin/parser.ml"
     : (Ast.block))

let _menhir_action_08 =
  fun id ty ->
    (
# 124 "bin/parser.mly"
                                             (Typesig(id, ty, []))
# 380 "bin/parser.ml"
     : (Ast.block))

let _menhir_action_09 =
  fun d id ty ->
    (
# 125 "bin/parser.mly"
                                                                             (Typesig(id, ty, d))
# 388 "bin/parser.ml"
     : (Ast.block))

let _menhir_action_10 =
  fun bl ex ->
    (
# 127 "bin/parser.mly"
                                             (If(ex, bl))
# 396 "bin/parser.ml"
     : (Ast.block))

let _menhir_action_11 =
  fun bl ex ->
    (
# 129 "bin/parser.mly"
                                                (While(ex, bl))
# 404 "bin/parser.ml"
     : (Ast.block))

let _menhir_action_12 =
  fun ex ->
    (
# 131 "bin/parser.mly"
                                  (Return(ex))
# 412 "bin/parser.ml"
     : (Ast.block))

let _menhir_action_13 =
  fun ex ->
    (
# 132 "bin/parser.mly"
                               (Ignore(ex))
# 420 "bin/parser.ml"
     : (Ast.block))

let _menhir_action_14 =
  fun c ->
    (
# 78 "bin/parser.mly"
            (Int(c))
# 428 "bin/parser.ml"
     : (Ast.const))

let _menhir_action_15 =
  fun c ->
    (
# 79 "bin/parser.mly"
              (Float(c))
# 436 "bin/parser.ml"
     : (Ast.const))

let _menhir_action_16 =
  fun c ->
    (
# 80 "bin/parser.mly"
               (String(c))
# 444 "bin/parser.ml"
     : (Ast.const))

let _menhir_action_17 =
  fun c ->
    (
# 81 "bin/parser.mly"
              (Id(c))
# 452 "bin/parser.ml"
     : (Ast.const))

let _menhir_action_18 =
  fun () ->
    (
# 82 "bin/parser.mly"
         (True)
# 460 "bin/parser.ml"
     : (Ast.const))

let _menhir_action_19 =
  fun () ->
    (
# 83 "bin/parser.mly"
          (False)
# 468 "bin/parser.ml"
     : (Ast.const))

let _menhir_action_20 =
  fun () ->
    (
# 114 "bin/parser.mly"
               (NoMangle(true))
# 476 "bin/parser.ml"
     : (Ast.attr))

let _menhir_action_21 =
  fun () ->
    (
# 115 "bin/parser.mly"
             (Inline(true))
# 484 "bin/parser.ml"
     : (Ast.attr))

let _menhir_action_22 =
  fun e ->
    (
# 107 "bin/parser.mly"
                           (Paren(e))
# 492 "bin/parser.ml"
     : (Ast.expr))

let _menhir_action_23 =
  fun c ->
    (
# 108 "bin/parser.mly"
            (Base(c))
# 500 "bin/parser.ml"
     : (Ast.expr))

let _menhir_action_24 =
  fun ex u ->
    (
# 109 "bin/parser.mly"
                                                   (UnOp(u, ex))
# 508 "bin/parser.ml"
     : (Ast.expr))

let _menhir_action_25 =
  fun b ex1 ex2 ->
    (
# 110 "bin/parser.mly"
                                          (BinOp(ex1, b, ex2))
# 516 "bin/parser.ml"
     : (Ast.expr))

let _menhir_action_26 =
  fun ex1 ex2 ->
    (
# 111 "bin/parser.mly"
                                          (FuncCall(ex1, ex2))
# 524 "bin/parser.ml"
     : (Ast.expr))

let _menhir_action_27 =
  fun c ->
    (
# 98 "bin/parser.mly"
              (Base(Id(c)))
# 532 "bin/parser.ml"
     : (Ast.expr))

let _menhir_action_28 =
  fun e ->
    (
# 99 "bin/parser.mly"
                         (Paren(e))
# 540 "bin/parser.ml"
     : (Ast.expr))

let _menhir_action_29 =
  fun () ->
    (
# 208 "<standard.mly>"
    ( [] )
# 548 "bin/parser.ml"
     : (string list))

let _menhir_action_30 =
  fun x xs ->
    (
# 210 "<standard.mly>"
    ( x :: xs )
# 556 "bin/parser.ml"
     : (string list))

let _menhir_action_31 =
  fun () ->
    (
# 208 "<standard.mly>"
    ( [] )
# 564 "bin/parser.ml"
     : (Ast.block list))

let _menhir_action_32 =
  fun x xs ->
    (
# 210 "<standard.mly>"
    ( x :: xs )
# 572 "bin/parser.ml"
     : (Ast.block list))

let _menhir_action_33 =
  fun () ->
    (
# 208 "<standard.mly>"
    ( [] )
# 580 "bin/parser.ml"
     : (Ast.toplevel list))

let _menhir_action_34 =
  fun x xs ->
    (
# 210 "<standard.mly>"
    ( x :: xs )
# 588 "bin/parser.ml"
     : (Ast.toplevel list))

let _menhir_action_35 =
  fun x ->
    (
# 218 "<standard.mly>"
    ( [ x ] )
# 596 "bin/parser.ml"
     : (unit list))

let _menhir_action_36 =
  fun x xs ->
    (
# 220 "<standard.mly>"
    ( x :: xs )
# 604 "bin/parser.ml"
     : (unit list))

let _menhir_action_37 =
  fun x ->
    (
# 218 "<standard.mly>"
    ( [ x ] )
# 612 "bin/parser.ml"
     : (Ast.attr list))

let _menhir_action_38 =
  fun x xs ->
    (
# 220 "<standard.mly>"
    ( x :: xs )
# 620 "bin/parser.ml"
     : (Ast.attr list))

let _menhir_action_39 =
  fun x ->
    (
# 218 "<standard.mly>"
    ( [ x ] )
# 628 "bin/parser.ml"
     : (Ast.expr list))

let _menhir_action_40 =
  fun x xs ->
    (
# 220 "<standard.mly>"
    ( x :: xs )
# 636 "bin/parser.ml"
     : (Ast.expr list))

let _menhir_action_41 =
  fun x ->
    (
# 218 "<standard.mly>"
    ( [ x ] )
# 644 "bin/parser.ml"
     : (Ast.unop list))

let _menhir_action_42 =
  fun x xs ->
    (
# 220 "<standard.mly>"
    ( x :: xs )
# 652 "bin/parser.ml"
     : (Ast.unop list))

let _menhir_action_43 =
  fun c ->
    (
# 102 "bin/parser.mly"
            (Base(c))
# 660 "bin/parser.ml"
     : (Ast.expr))

let _menhir_action_44 =
  fun e ->
    (
# 103 "bin/parser.mly"
                         (Paren(e))
# 668 "bin/parser.ml"
     : (Ast.expr))

let _menhir_action_45 =
  fun top ->
    (
# 147 "bin/parser.mly"
                           (Prog(top))
# 676 "bin/parser.ml"
     : (Ast.program))

let _menhir_action_46 =
  fun args ex id ->
    (
# 139 "bin/parser.mly"
    (Assign(id, args, ex))
# 684 "bin/parser.ml"
     : (Ast.toplevel))

let _menhir_action_47 =
  fun args bl id ->
    (
# 141 "bin/parser.mly"
    (AssignBlock(id, args, bl))
# 692 "bin/parser.ml"
     : (Ast.toplevel))

let _menhir_action_48 =
  fun id ty ->
    (
# 142 "bin/parser.mly"
                                             (Typesig(id, ty, []))
# 700 "bin/parser.ml"
     : (Ast.toplevel))

let _menhir_action_49 =
  fun d id ty ->
    (
# 143 "bin/parser.mly"
                                                                             (Typesig(id, ty, d))
# 708 "bin/parser.ml"
     : (Ast.toplevel))

let _menhir_action_50 =
  fun x ->
    (
# 70 "bin/parser.mly"
                            (x)
# 716 "bin/parser.ml"
     : (Ast.typeSig))

let _menhir_action_51 =
  fun base c ->
    (
# 71 "bin/parser.mly"
                                      (Ptr(List.length c, base))
# 724 "bin/parser.ml"
     : (Ast.typeSig))

let _menhir_action_52 =
  fun base ->
    (
# 72 "bin/parser.mly"
                 (TSBase(base))
# 732 "bin/parser.ml"
     : (Ast.typeSig))

let _menhir_action_53 =
  fun ty1 ty2 ->
    (
# 73 "bin/parser.mly"
                                    (Arrow(ty1, ty2))
# 740 "bin/parser.ml"
     : (Ast.typeSig))

let _menhir_action_54 =
  fun () ->
    (
# 86 "bin/parser.mly"
        (UnOpDeref)
# 748 "bin/parser.ml"
     : (Ast.unop))

let _menhir_action_55 =
  fun () ->
    (
# 87 "bin/parser.mly"
        (UnOpRef)
# 756 "bin/parser.ml"
     : (Ast.unop))

let _menhir_action_56 =
  fun () ->
    (
# 88 "bin/parser.mly"
        (UnOpPos)
# 764 "bin/parser.ml"
     : (Ast.unop))

let _menhir_action_57 =
  fun () ->
    (
# 89 "bin/parser.mly"
        (UnOpNeg)
# 772 "bin/parser.ml"
     : (Ast.unop))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ADD ->
        "ADD"
    | AND ->
        "AND"
    | AT ->
        "AT"
    | BANG ->
        "BANG"
    | BSLASH ->
        "BSLASH"
    | COLON ->
        "COLON"
    | COMMA ->
        "COMMA"
    | DOT ->
        "DOT"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
    | FALSE ->
        "FALSE"
    | GT ->
        "GT"
    | HASH ->
        "HASH"
    | IGNORE ->
        "IGNORE"
    | INLINE ->
        "INLINE"
    | KW_FOR ->
        "KW_FOR"
    | KW_IF ->
        "KW_IF"
    | KW_RETURN ->
        "KW_RETURN"
    | KW_WHILE ->
        "KW_WHILE"
    | LBRACE ->
        "LBRACE"
    | LBRACK ->
        "LBRACK"
    | LPAREN ->
        "LPAREN"
    | LT ->
        "LT"
    | MUL ->
        "MUL"
    | NOMANGLE ->
        "NOMANGLE"
    | PERCENT ->
        "PERCENT"
    | QMARK ->
        "QMARK"
    | RBRACE ->
        "RBRACE"
    | RBRACK ->
        "RBRACK"
    | RPAREN ->
        "RPAREN"
    | SEMICOLON ->
        "SEMICOLON"
    | SLASH ->
        "SLASH"
    | STRAIGHT ->
        "STRAIGHT"
    | SUB ->
        "SUB"
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

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37-39"]
  
  let rec _menhir_run_106 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_program =
    fun _menhir_stack _v ->
      let top = _v in
      let _v = _menhir_action_45 top in
      MenhirBox_program _v
  
  let rec _menhir_run_104 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_toplevel -> _ -> _menhir_box_program =
    fun _menhir_stack _v ->
      let MenhirCell1_toplevel (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_34 x xs in
      _menhir_goto_list_toplevel_ _menhir_stack _v _menhir_s
  
  and _menhir_goto_list_toplevel_ : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState000 ->
          _menhir_run_106 _menhir_stack _v
      | MenhirState103 ->
          _menhir_run_104 _menhir_stack _v
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_run_001 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_T_IDENT (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_IDENT _v_0 ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState001
      | STRAIGHT ->
          let _menhir_stack = MenhirCell1_STRAIGHT (_menhir_stack, MenhirState001) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | NOMANGLE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_20 () in
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState004 _tok
          | INLINE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_21 () in
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState004 _tok
          | _ ->
              _eRR 4)
      | COLON ->
          let _menhir_stack = MenhirCell1_COLON (_menhir_stack, MenhirState001) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | T_IDENT _v_3 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let base = _v_3 in
              let _v = _menhir_action_52 base in
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState023 _tok
          | LPAREN ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
          | AT ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
          | _ ->
              _eRR 23)
      | EQ ->
          let _v = _menhir_action_29 () in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState001
      | _ ->
          _eRR 1
  
  and _menhir_run_002 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_T_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_T_IDENT (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState002
      | EQ ->
          let _v = _menhir_action_29 () in
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR 2
  
  and _menhir_run_003 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_cell1_T_IDENT -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_T_IDENT (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_30 x xs in
      _menhir_goto_list_T_IDENT_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_list_T_IDENT_ : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_T_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState064 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState001 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState002 ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_073 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_T_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_list_T_IDENT_ (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v_0 in
          let _v = _menhir_action_16 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState074 _tok
      | T_INT _v_2 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v_2 in
          let _v = _menhir_action_14 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState074 _tok
      | T_IDENT _v_4 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState074
      | T_FLOAT _v_5 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v_5 in
          let _v = _menhir_action_15 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState074 _tok
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_18 () in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState074 _tok
      | LPAREN ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | LBRACE ->
          let _menhir_stack = MenhirCell1_LBRACE (_menhir_stack, MenhirState074) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | T_IDENT _v ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState075
          | KW_WHILE ->
              _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
          | KW_RETURN ->
              _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
          | KW_IF ->
              _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
          | IGNORE ->
              _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
          | RBRACE ->
              let _v = _menhir_action_31 () in
              _menhir_run_088_spec_075 _menhir_stack _menhir_lexbuf _menhir_lexer _v
          | _ ->
              _eRR 75)
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_19 () in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState074 _tok
      | _ ->
          _eRR 74
  
  and _menhir_run_056 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LBRACE | RPAREN | SEMICOLON ->
          let c = _v in
          let _v = _menhir_action_23 c in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | ADD | MUL | SLASH | SUB ->
          let c = _v in
          let _v = _menhir_action_43 c in
          _menhir_goto_parenexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 56
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState027 ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState074 ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState085 ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState082 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState079 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState076 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState058 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState048 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_101 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_cell1_list_T_IDENT_ -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_list_T_IDENT_ (_menhir_stack, _, args) = _menhir_stack in
          let MenhirCell1_T_IDENT (_menhir_stack, _menhir_s, id) = _menhir_stack in
          let ex = _v in
          let _v = _menhir_action_46 args ex id in
          _menhir_goto_toplevel _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 101
  
  and _menhir_goto_toplevel : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_toplevel (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | T_IDENT _v_0 ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState103
      | EOF ->
          let _v = _menhir_action_33 () in
          _menhir_run_104 _menhir_stack _v
      | _ ->
          _eRR 103
  
  and _menhir_run_097 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_cell1_list_T_IDENT_ -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_list_T_IDENT_ (_menhir_stack, _, args) = _menhir_stack in
          let MenhirCell1_T_IDENT (_menhir_stack, _menhir_s, id) = _menhir_stack in
          let ex = _v in
          let _v = _menhir_action_06 args ex id in
          _menhir_goto_blocksub _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 97
  
  and _menhir_goto_blocksub : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_blocksub (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | T_IDENT _v_0 ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState089
      | KW_WHILE ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | KW_RETURN ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | KW_IF ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | IGNORE ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | RBRACE ->
          let _v = _menhir_action_31 () in
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR 89
  
  and _menhir_run_064 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_T_IDENT (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_IDENT _v_0 ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState064
      | STRAIGHT ->
          let _menhir_stack = MenhirCell1_STRAIGHT (_menhir_stack, MenhirState064) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | NOMANGLE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_20 () in
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState065 _tok
          | INLINE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_21 () in
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState065 _tok
          | _ ->
              _eRR 65)
      | COLON ->
          let _menhir_stack = MenhirCell1_COLON (_menhir_stack, MenhirState064) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | T_IDENT _v_3 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let base = _v_3 in
              let _v = _menhir_action_52 base in
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState070 _tok
          | LPAREN ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
          | AT ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
          | _ ->
              _eRR 70)
      | EQ ->
          let _v = _menhir_action_29 () in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState064
      | _ ->
          _eRR 64
  
  and _menhir_run_021 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | NOMANGLE ->
          let _menhir_stack = MenhirCell1_dec (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_20 () in
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState021 _tok
      | INLINE ->
          let _menhir_stack = MenhirCell1_dec (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_21 () in
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState021 _tok
      | COLON ->
          let x = _v in
          let _v = _menhir_action_37 x in
          _menhir_goto_nonempty_list_dec_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR 21
  
  and _menhir_goto_nonempty_list_dec_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState065 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState021 ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState004 ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_066 : type  ttv_stack. (((ttv_stack, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_cell1_STRAIGHT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_nonempty_list_dec_ (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_IDENT _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let base = _v_0 in
          let _v = _menhir_action_52 base in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState067 _tok
      | LPAREN ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
      | AT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
      | _ ->
          _eRR 67
  
  and _menhir_run_068 : type  ttv_stack. ((((ttv_stack, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_cell1_STRAIGHT, _menhir_box_program) _menhir_cell1_nonempty_list_dec_ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TS_TO ->
          let _menhir_stack = MenhirCell1_typesig (_menhir_stack, _menhir_s, _v) in
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_nonempty_list_dec_ (_menhir_stack, _, d) = _menhir_stack in
          let MenhirCell1_STRAIGHT (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_T_IDENT (_menhir_stack, _menhir_s, id) = _menhir_stack in
          let ty = _v in
          let _v = _menhir_action_09 d id ty in
          _menhir_goto_blocksub _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 68
  
  and _menhir_run_014 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_typesig -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let base = _v in
          let _v = _menhir_action_52 base in
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState014 _tok
      | LPAREN ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState014
      | AT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState014
      | _ ->
          _eRR 14
  
  and _menhir_run_015 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_typesig as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TS_TO ->
          let _menhir_stack = MenhirCell1_typesig (_menhir_stack, _menhir_s, _v) in
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN | SEMICOLON ->
          let MenhirCell1_typesig (_menhir_stack, _menhir_s, ty1) = _menhir_stack in
          let ty2 = _v in
          let _v = _menhir_action_53 ty1 ty2 in
          _menhir_goto_typesig _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 15
  
  and _menhir_goto_typesig : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState070 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState067 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState023 ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState008 ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState014 ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_071 : type  ttv_stack. (((ttv_stack, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_cell1_COLON as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TS_TO ->
          let _menhir_stack = MenhirCell1_typesig (_menhir_stack, _menhir_s, _v) in
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_COLON (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_T_IDENT (_menhir_stack, _menhir_s, id) = _menhir_stack in
          let ty = _v in
          let _v = _menhir_action_08 id ty in
          _menhir_goto_blocksub _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 71
  
  and _menhir_run_024 : type  ttv_stack. (((ttv_stack, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_cell1_COLON as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TS_TO ->
          let _menhir_stack = MenhirCell1_typesig (_menhir_stack, _menhir_s, _v) in
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_COLON (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_T_IDENT (_menhir_stack, _menhir_s, id) = _menhir_stack in
          let ty = _v in
          let _v = _menhir_action_48 id ty in
          _menhir_goto_toplevel _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 24
  
  and _menhir_run_019 : type  ttv_stack. ((((ttv_stack, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_cell1_STRAIGHT, _menhir_box_program) _menhir_cell1_nonempty_list_dec_ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TS_TO ->
          let _menhir_stack = MenhirCell1_typesig (_menhir_stack, _menhir_s, _v) in
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_nonempty_list_dec_ (_menhir_stack, _, d) = _menhir_stack in
          let MenhirCell1_STRAIGHT (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_T_IDENT (_menhir_stack, _menhir_s, id) = _menhir_stack in
          let ty = _v in
          let _v = _menhir_action_49 d id ty in
          _menhir_goto_toplevel _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 19
  
  and _menhir_run_013 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TS_TO ->
          let _menhir_stack = MenhirCell1_typesig (_menhir_stack, _menhir_s, _v) in
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let x = _v in
          let _v = _menhir_action_50 x in
          _menhir_goto_typesig _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 13
  
  and _menhir_run_010 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let base = _v in
          let _v = _menhir_action_52 base in
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState010 _tok
      | LPAREN ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState010
      | AT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState010
      | _ ->
          _eRR 10
  
  and _menhir_run_011 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | AT ->
          let _menhir_stack = MenhirCell1_AT (_menhir_stack, _menhir_s) in
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState011
      | T_IDENT _ ->
          let x = () in
          let _v = _menhir_action_35 x in
          _menhir_goto_nonempty_list_AT_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 11
  
  and _menhir_goto_nonempty_list_AT_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState070 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState067 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState023 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState008 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState014 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState011 ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_016 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | T_IDENT _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (base, c) = (_v_0, _v) in
          let _v = _menhir_action_51 base c in
          _menhir_goto_typesig _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_012 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_AT -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_AT (_menhir_stack, _menhir_s) = _menhir_stack in
      let (x, xs) = ((), _v) in
      let _v = _menhir_action_36 x xs in
      _menhir_goto_nonempty_list_AT_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_022 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_dec -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_dec (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_38 x xs in
      _menhir_goto_nonempty_list_dec_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_007 : type  ttv_stack. (((ttv_stack, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_cell1_STRAIGHT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_nonempty_list_dec_ (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_IDENT _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let base = _v_0 in
          let _v = _menhir_action_52 base in
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState008 _tok
      | LPAREN ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState008
      | AT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState008
      | _ ->
          _eRR 8
  
  and _menhir_run_076 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_KW_WHILE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v in
          let _v = _menhir_action_16 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState076 _tok
      | T_INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v in
          let _v = _menhir_action_14 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState076 _tok
      | T_IDENT _v ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState076
      | T_FLOAT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v in
          let _v = _menhir_action_15 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState076 _tok
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_18 () in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState076 _tok
      | LPAREN ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState076
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_19 () in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState076 _tok
      | _ ->
          _eRR 76
  
  and _menhir_run_030 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ADD | LBRACE | MUL | RPAREN | SEMICOLON | SLASH | SUB ->
          let c = _v in
          let _v = _menhir_action_17 c in
          _menhir_goto_const _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | FALSE | LPAREN | TRUE | T_FLOAT _ | T_IDENT _ | T_INT _ | T_STRING _ ->
          let c = _v in
          let _v = _menhir_action_27 c in
          _menhir_goto_func _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 30
  
  and _menhir_goto_const : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState027 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState074 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState076 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState085 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState079 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState048 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState046 ->
          _menhir_run_052_spec_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState049 ->
          _menhir_run_052_spec_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState050 ->
          _menhir_run_052_spec_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_052_spec_046 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_parenexpr _menhir_cell0_binop -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let c = _v in
      let _v = _menhir_action_43 c in
      _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_057 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_parenexpr _menhir_cell0_binop -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_binop (_menhir_stack, b) = _menhir_stack in
      let MenhirCell1_parenexpr (_menhir_stack, _menhir_s, ex1) = _menhir_stack in
      let ex2 = _v in
      let _v = _menhir_action_25 b ex1 ex2 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_052_spec_049 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_func -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let c = _v in
      let _v = _menhir_action_43 c in
      _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState049 _tok
  
  and _menhir_run_050 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _menhir_stack = MenhirCell1_parenexpr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v_0 in
          let _v = _menhir_action_16 c in
          _menhir_run_052_spec_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v_2 ->
          let _menhir_stack = MenhirCell1_parenexpr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v_2 in
          let _v = _menhir_action_14 c in
          _menhir_run_052_spec_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v_4 ->
          let _menhir_stack = MenhirCell1_parenexpr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v_4 in
          let _v = _menhir_action_17 c in
          _menhir_run_052_spec_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_FLOAT _v_6 ->
          let _menhir_stack = MenhirCell1_parenexpr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v_6 in
          let _v = _menhir_action_15 c in
          _menhir_run_052_spec_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TRUE ->
          let _menhir_stack = MenhirCell1_parenexpr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_18 () in
          _menhir_run_052_spec_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | LPAREN ->
          let _menhir_stack = MenhirCell1_parenexpr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState050
      | FALSE ->
          let _menhir_stack = MenhirCell1_parenexpr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_19 () in
          _menhir_run_052_spec_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | LBRACE | RPAREN | SEMICOLON ->
          let x = _v in
          let _v = _menhir_action_39 x in
          _menhir_goto_nonempty_list_parenexpr_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 50
  
  and _menhir_run_052_spec_050 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_parenexpr -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let c = _v in
      let _v = _menhir_action_43 c in
      _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState050 _tok
  
  and _menhir_run_048 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v in
          let _v = _menhir_action_16 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState048 _tok
      | T_INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v in
          let _v = _menhir_action_14 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState048 _tok
      | T_IDENT _v ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState048
      | T_FLOAT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v in
          let _v = _menhir_action_15 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState048 _tok
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_18 () in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState048 _tok
      | LPAREN ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState048
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_19 () in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState048 _tok
      | _ ->
          _eRR 48
  
  and _menhir_run_033 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v in
          let _v = _menhir_action_16 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState033 _tok
      | T_INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v in
          let _v = _menhir_action_14 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState033 _tok
      | T_IDENT _v ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState033
      | T_FLOAT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v in
          let _v = _menhir_action_15 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState033 _tok
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_18 () in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState033 _tok
      | SUB ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_57 () in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState033 _tok
      | LPAREN ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState033
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_19 () in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState033 _tok
      | AT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_54 () in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState033 _tok
      | AND ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_55 () in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState033 _tok
      | ADD ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_56 () in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState033 _tok
      | _ ->
          _eRR 33
  
  and _menhir_run_039 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _menhir_stack = MenhirCell1_unop (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_57 () in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState039 _tok
      | AT ->
          let _menhir_stack = MenhirCell1_unop (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_54 () in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState039 _tok
      | AND ->
          let _menhir_stack = MenhirCell1_unop (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_55 () in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState039 _tok
      | ADD ->
          let _menhir_stack = MenhirCell1_unop (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_56 () in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState039 _tok
      | FALSE | LPAREN | TRUE | T_FLOAT _ | T_IDENT _ | T_INT _ | T_STRING _ ->
          let x = _v in
          let _v = _menhir_action_41 x in
          _menhir_goto_nonempty_list_unop_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 39
  
  and _menhir_goto_nonempty_list_unop_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState033 ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState039 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_058 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_nonempty_list_unop_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v_0 in
          let _v = _menhir_action_16 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState058 _tok
      | T_INT _v_2 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v_2 in
          let _v = _menhir_action_14 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState058 _tok
      | T_IDENT _v_4 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState058
      | T_FLOAT _v_5 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v_5 in
          let _v = _menhir_action_15 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState058 _tok
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_18 () in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState058 _tok
      | LPAREN ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState058
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_19 () in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState058 _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_040 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_unop -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_unop (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_42 x xs in
      _menhir_goto_nonempty_list_unop_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_nonempty_list_parenexpr_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState049 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState050 ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_053 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_func -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_func (_menhir_stack, _menhir_s, ex1) = _menhir_stack in
      let ex2 = _v in
      let _v = _menhir_action_26 ex1 ex2 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_051 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_parenexpr -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_parenexpr (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_40 x xs in
      _menhir_goto_nonempty_list_parenexpr_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_func : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_func (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v_0 in
          let _v = _menhir_action_16 c in
          _menhir_run_052_spec_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v_2 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v_2 in
          let _v = _menhir_action_14 c in
          _menhir_run_052_spec_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v_4 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v_4 in
          let _v = _menhir_action_17 c in
          _menhir_run_052_spec_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_FLOAT _v_6 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v_6 in
          let _v = _menhir_action_15 c in
          _menhir_run_052_spec_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_18 () in
          _menhir_run_052_spec_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | LPAREN ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState049
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_19 () in
          _menhir_run_052_spec_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_079 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_KW_RETURN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v in
          let _v = _menhir_action_16 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState079 _tok
      | T_INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v in
          let _v = _menhir_action_14 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState079 _tok
      | T_IDENT _v ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState079
      | T_FLOAT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v in
          let _v = _menhir_action_15 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState079 _tok
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_18 () in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState079 _tok
      | LPAREN ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState079
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_19 () in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState079 _tok
      | _ ->
          _eRR 79
  
  and _menhir_run_082 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_KW_IF (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v in
          let _v = _menhir_action_16 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState082 _tok
      | T_INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v in
          let _v = _menhir_action_14 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState082 _tok
      | T_IDENT _v ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState082
      | T_FLOAT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v in
          let _v = _menhir_action_15 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState082 _tok
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_18 () in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState082 _tok
      | LPAREN ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState082
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_19 () in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState082 _tok
      | _ ->
          _eRR 82
  
  and _menhir_run_085 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_IGNORE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v in
          let _v = _menhir_action_16 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState085 _tok
      | T_INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v in
          let _v = _menhir_action_14 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState085 _tok
      | T_IDENT _v ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState085
      | T_FLOAT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v in
          let _v = _menhir_action_15 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState085 _tok
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_18 () in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState085 _tok
      | LPAREN ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState085
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_19 () in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState085 _tok
      | _ ->
          _eRR 85
  
  and _menhir_run_090 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_blocksub -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_blocksub (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_32 x xs in
      _menhir_goto_list_blocksub_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_list_blocksub_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState089 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState063 ->
          _menhir_run_088_spec_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState075 ->
          _menhir_run_088_spec_075 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState078 ->
          _menhir_run_088_spec_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState084 ->
          _menhir_run_088_spec_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_088_spec_063 : type  ttv_stack. (((ttv_stack, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_cell1_list_T_IDENT_, _menhir_box_program) _menhir_cell1_LBRACE -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _v =
        let many = _v in
        _menhir_action_05 many
      in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LBRACE (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_list_T_IDENT_ (_menhir_stack, _, args) = _menhir_stack in
      let MenhirCell1_T_IDENT (_menhir_stack, _menhir_s, id) = _menhir_stack in
      let bl = _v in
      let _v = _menhir_action_47 args bl id in
      _menhir_goto_toplevel _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_088_spec_075 : type  ttv_stack. (((ttv_stack, _menhir_box_program) _menhir_cell1_T_IDENT, _menhir_box_program) _menhir_cell1_list_T_IDENT_, _menhir_box_program) _menhir_cell1_LBRACE -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _v =
        let many = _v in
        _menhir_action_05 many
      in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LBRACE (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_list_T_IDENT_ (_menhir_stack, _, args) = _menhir_stack in
      let MenhirCell1_T_IDENT (_menhir_stack, _menhir_s, id) = _menhir_stack in
      let bl = _v in
      let _v = _menhir_action_07 args bl id in
      _menhir_goto_blocksub _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_088_spec_078 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_KW_WHILE, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _v =
        let many = _v in
        _menhir_action_05 many
      in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_expr (_menhir_stack, _, ex) = _menhir_stack in
      let MenhirCell1_KW_WHILE (_menhir_stack, _menhir_s) = _menhir_stack in
      let bl = _v in
      let _v = _menhir_action_11 bl ex in
      _menhir_goto_blocksub _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_088_spec_084 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_KW_IF, _menhir_box_program) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _v =
        let many = _v in
        _menhir_action_05 many
      in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_expr (_menhir_stack, _, ex) = _menhir_stack in
      let MenhirCell1_KW_IF (_menhir_stack, _menhir_s) = _menhir_stack in
      let bl = _v in
      let _v = _menhir_action_10 bl ex in
      _menhir_goto_blocksub _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_086 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_IGNORE -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_IGNORE (_menhir_stack, _menhir_s) = _menhir_stack in
          let ex = _v in
          let _v = _menhir_action_13 ex in
          _menhir_goto_blocksub _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 86
  
  and _menhir_run_083 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_KW_IF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LBRACE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | T_IDENT _v ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState084
          | KW_WHILE ->
              _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
          | KW_RETURN ->
              _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
          | KW_IF ->
              _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
          | IGNORE ->
              _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
          | RBRACE ->
              let _v = _menhir_action_31 () in
              _menhir_run_088_spec_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v
          | _ ->
              _eRR 84)
      | _ ->
          _eRR 83
  
  and _menhir_run_080 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_KW_RETURN -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_KW_RETURN (_menhir_stack, _menhir_s) = _menhir_stack in
          let ex = _v in
          let _v = _menhir_action_12 ex in
          _menhir_goto_blocksub _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 80
  
  and _menhir_run_077 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_KW_WHILE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LBRACE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | T_IDENT _v ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState078
          | KW_WHILE ->
              _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState078
          | KW_RETURN ->
              _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState078
          | KW_IF ->
              _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState078
          | IGNORE ->
              _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState078
          | RBRACE ->
              let _v = _menhir_action_31 () in
              _menhir_run_088_spec_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v
          | _ ->
              _eRR 78)
      | _ ->
          _eRR 77
  
  and _menhir_run_061 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACE | RPAREN | SEMICOLON ->
              let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
              let e = _v in
              let _v = _menhir_action_22 e in
              _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | FALSE | LPAREN | TRUE | T_FLOAT _ | T_IDENT _ | T_INT _ | T_STRING _ ->
              let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
              let e = _v in
              let _v = _menhir_action_28 e in
              _menhir_goto_func _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | ADD | MUL | SLASH | SUB ->
              let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
              let e = _v in
              let _v = _menhir_action_44 e in
              _menhir_goto_parenexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR 62)
      | _ ->
          _eRR 61
  
  and _menhir_goto_parenexpr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState046 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState050 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState074 ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState085 ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState079 ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState076 ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState048 ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_041 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_parenexpr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SUB ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_goto_binop _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | SLASH ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_04 () in
          _menhir_goto_binop _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MUL ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_03 () in
          _menhir_goto_binop _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ADD ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_01 () in
          _menhir_goto_binop _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR 41
  
  and _menhir_goto_binop : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_parenexpr -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_binop (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v_0 in
          let _v = _menhir_action_16 c in
          _menhir_run_052_spec_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_INT _v_2 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v_2 in
          let _v = _menhir_action_14 c in
          _menhir_run_052_spec_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_IDENT _v_4 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v_4 in
          let _v = _menhir_action_17 c in
          _menhir_run_052_spec_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | T_FLOAT _v_6 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v_6 in
          let _v = _menhir_action_15 c in
          _menhir_run_052_spec_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_18 () in
          _menhir_run_052_spec_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | LPAREN ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState046
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_19 () in
          _menhir_run_052_spec_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR 46
  
  and _menhir_run_059 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_LPAREN, _menhir_box_program) _menhir_cell1_nonempty_list_unop_ -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_nonempty_list_unop_ (_menhir_stack, _, u) = _menhir_stack in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let ex = _v in
          let _v = _menhir_action_24 ex u in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 59
  
  and _menhir_run_054 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let e = _v in
          let _v = _menhir_action_44 e in
          _menhir_goto_parenexpr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR 54
  
  and _menhir_run_026 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_T_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_list_T_IDENT_ (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_STRING _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v_0 in
          let _v = _menhir_action_16 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState027 _tok
      | T_INT _v_2 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v_2 in
          let _v = _menhir_action_14 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState027 _tok
      | T_IDENT _v_4 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState027
      | T_FLOAT _v_5 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let c = _v_5 in
          let _v = _menhir_action_15 c in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState027 _tok
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_18 () in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState027 _tok
      | LPAREN ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState027
      | LBRACE ->
          let _menhir_stack = MenhirCell1_LBRACE (_menhir_stack, MenhirState027) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | T_IDENT _v ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState063
          | KW_WHILE ->
              _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
          | KW_RETURN ->
              _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
          | KW_IF ->
              _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
          | IGNORE ->
              _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
          | RBRACE ->
              let _v = _menhir_action_31 () in
              _menhir_run_088_spec_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v
          | _ ->
              _eRR 63)
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_19 () in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState027 _tok
      | _ ->
          _eRR 27
  
  let rec _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | T_IDENT _v ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState000
      | EOF ->
          let _v = _menhir_action_33 () in
          _menhir_run_106 _menhir_stack _v
      | _ ->
          _eRR 0
  
end

let program =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_program v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
