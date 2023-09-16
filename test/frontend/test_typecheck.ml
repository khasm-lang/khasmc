let%test_unit "Typechecker" =
  let open Khasmc.Typecheck in
  let open Khasmc.Ast in
  let tm =
    [
      Program
        [
          IntExtern
            ( "`int_add",
              "khasm.Stdlib.iadd",
              2,
              TSForall ("a", TSMap (TSBase "a", TSMap (TSBase "a", TSBase "a")))
            );
          IntExtern
            ( "`int_sub",
              "khasm.Stdlib.isub",
              2,
              TSForall ("a", TSMap (TSBase "a", TSMap (TSBase "a", TSBase "a")))
            );
          IntExtern
            ( "`int_mul",
              "khasm.Stdlib.imul",
              2,
              TSForall ("a", TSMap (TSBase "a", TSMap (TSBase "a", TSBase "a")))
            );
          IntExtern
            ( "`int_div",
              "khasm.Stdlib.idiv",
              2,
              TSForall ("a", TSMap (TSBase "a", TSMap (TSBase "a", TSBase "a")))
            );
          IntExtern
            ( "`float_add",
              "khasm.Stdlib.fadd",
              2,
              TSForall ("a", TSMap (TSBase "a", TSMap (TSBase "a", TSBase "a")))
            );
          IntExtern
            ( "`float_sub",
              "khasm.Stdlib.fsub",
              2,
              TSForall ("a", TSMap (TSBase "a", TSMap (TSBase "a", TSBase "a")))
            );
          IntExtern
            ( "`float_div",
              "khasm.Stdlib.fdiv",
              2,
              TSForall ("a", TSMap (TSBase "a", TSMap (TSBase "a", TSBase "a")))
            );
          IntExtern
            ( "`float_mul",
              "khasm.Stdlib.fmul",
              2,
              TSForall ("a", TSMap (TSBase "a", TSMap (TSBase "a", TSBase "a")))
            );
          IntExtern
            ( "`print_int",
              "khasm.Stdlib.print_int",
              1,
              TSMap (TSBase "int", TSTuple []) );
          IntExtern
            ( "`print_str",
              "khasm.Stdlib.print_str",
              1,
              TSMap (TSBase "string", TSTuple []) );
          IntExtern
            ( "`print_float",
              "khasm.Stdlib.print_float",
              1,
              TSMap (TSBase "float", TSTuple []) );
          IntExtern
            ( "`s_eq",
              "khasm.Stdlib.s_eq",
              2,
              TSForall
                ("a", TSMap (TSBase "a", TSMap (TSBase "a", TSBase "bool"))) );
          Bind ("khasm.Stdlib.=", [], "khasm.Stdlib.s_eq");
          Bind ("khasm.Stdlib.+", [], "khasm.Stdlib.iadd");
          Bind ("khasm.Stdlib.-", [], "khasm.Stdlib.isub");
          Bind ("khasm.Stdlib./", [], "khasm.Stdlib.idiv");
          Bind ("khasm.Stdlib.*", [], "khasm.Stdlib.imul");
          Bind ("khasm.Stdlib.+.", [], "khasm.Stdlib.fadd");
          Bind ("khasm.Stdlib.-.", [], "khasm.Stdlib.fsub");
          Bind ("khasm.Stdlib.*.", [], "khasm.Stdlib.fmul");
          Bind ("khasm.Stdlib./.", [], "khasm.Stdlib.fdiv");
          TopAssign
            ( ( "khasm.Stdlib.pipe",
                TSForall
                  ( "a",
                    TSForall
                      ( "b",
                        TSMap
                          ( TSBase "a",
                            TSMap (TSMap (TSBase "a", TSBase "b"), TSBase "b")
                          ) ) ) ),
              ( "khasm.Stdlib.pipe",
                [ "x0"; "f1" ],
                FCall
                  ( { id = 67; complex = 5 },
                    Base
                      ( { id = 64; complex = 2 },
                        Ident ({ id = 63; complex = 1 }, "f1") ),
                    Base
                      ( { id = 66; complex = 2 },
                        Ident ({ id = 65; complex = 1 }, "x0") ) ) ) );
          Bind ("khasm.Stdlib.|>", [], "khasm.Stdlib.pipe");
          TopAssign
            ( ( "khasm.Stdlib.apply",
                TSForall
                  ( "a",
                    TSForall
                      ( "b",
                        TSMap
                          ( TSMap (TSBase "a", TSBase "b"),
                            TSMap (TSBase "a", TSBase "b") ) ) ) ),
              ( "khasm.Stdlib.apply",
                [ "f2"; "x3" ],
                FCall
                  ( { id = 72; complex = 5 },
                    Base
                      ( { id = 69; complex = 2 },
                        Ident ({ id = 68; complex = 1 }, "f2") ),
                    Base
                      ( { id = 71; complex = 2 },
                        Ident ({ id = 70; complex = 1 }, "x3") ) ) ) );
          Bind ("khasm.Stdlib.$", [], "khasm.Stdlib.apply");
          TopAssign
            ( ( "khasm.Stdlib.compose",
                TSForall
                  ( "a",
                    TSForall
                      ( "b",
                        TSForall
                          ( "c",
                            TSMap
                              ( TSMap (TSBase "b", TSBase "c"),
                                TSMap
                                  ( TSMap (TSBase "a", TSBase "b"),
                                    TSMap (TSBase "a", TSBase "c") ) ) ) ) ) ),
              ( "khasm.Stdlib.compose",
                [ "f4"; "g5"; "x6" ],
                FCall
                  ( { id = 80; complex = 8 },
                    Base
                      ( { id = 74; complex = 2 },
                        Ident ({ id = 73; complex = 1 }, "f4") ),
                    FCall
                      ( { id = 79; complex = 5 },
                        Base
                          ( { id = 76; complex = 2 },
                            Ident ({ id = 75; complex = 1 }, "g5") ),
                        Base
                          ( { id = 78; complex = 2 },
                            Ident ({ id = 77; complex = 1 }, "x6") ) ) ) ) );
          Bind ("khasm.Stdlib.%", [], "khasm.Stdlib.compose");
          TopAssign
            ( ( "khasm.Stdlib.rcompose",
                TSForall
                  ( "a",
                    TSForall
                      ( "b",
                        TSForall
                          ( "c",
                            TSMap
                              ( TSMap (TSBase "a", TSBase "b"),
                                TSMap
                                  ( TSMap (TSBase "b", TSBase "c"),
                                    TSMap (TSBase "a", TSBase "c") ) ) ) ) ) ),
              ( "khasm.Stdlib.rcompose",
                [ "f7"; "g8"; "x9" ],
                FCall
                  ( { id = 88; complex = 8 },
                    Base
                      ( { id = 82; complex = 2 },
                        Ident ({ id = 81; complex = 1 }, "g8") ),
                    FCall
                      ( { id = 87; complex = 5 },
                        Base
                          ( { id = 84; complex = 2 },
                            Ident ({ id = 83; complex = 1 }, "f7") ),
                        Base
                          ( { id = 86; complex = 2 },
                            Ident ({ id = 85; complex = 1 }, "x9") ) ) ) ) );
          Bind ("khasm.Stdlib.%>", [], "khasm.Stdlib.rcompose");
        ];
      Program
        [
          Typedecl
            ( "khasm.Match.List",
              [ "a" ],
              [
                {
                  head = "khasm.Match.Nil";
                  args = [];
                  typ = Ok (TSApp ([ TSBase "a" ], "khasm.Match.List"));
                };
                {
                  head = "khasm.Match.Cons";
                  args =
                    [ TSBase "a"; TSApp ([ TSBase "a" ], "khasm.Match.List") ];
                  typ = Ok (TSApp ([ TSBase "a" ], "khasm.Match.List"));
                };
              ] );
          TopAssignRec
            ( ( "khasm.Match.map",
                TSForall
                  ( "q",
                    TSForall
                      ( "w",
                        TSMap
                          ( TSMap (TSBase "q", TSBase "w"),
                            TSMap
                              ( TSApp ([ TSBase "q" ], "khasm.Match.List"),
                                TSApp ([ TSBase "w" ], "khasm.Match.List") ) )
                      ) ) ),
              ( "khasm.Match.map",
                [ "f"; "x" ],
                Match
                  ( { id = 21; complex = -1 },
                    Base
                      ( { id = 1; complex = -1 },
                        Ident ({ id = 0; complex = -1 }, "x") ),
                    [
                      ( MPApp ("khasm.Match.Nil", []),
                        Base
                          ( { id = 3; complex = -1 },
                            Ident ({ id = 2; complex = -1 }, "khasm.Match.Nil")
                          ) );
                      ( MPApp ("khasm.Match.Cons", [ MPId "q"; MPId "w" ]),
                        FCall
                          ( { id = 20; complex = -1 },
                            FCall
                              ( { id = 11; complex = -1 },
                                Base
                                  ( { id = 5; complex = -1 },
                                    Ident
                                      ( { id = 4; complex = -1 },
                                        "khasm.Match.Cons" ) ),
                                FCall
                                  ( { id = 10; complex = -1 },
                                    Base
                                      ( { id = 7; complex = -1 },
                                        Ident ({ id = 6; complex = -1 }, "f") ),
                                    Base
                                      ( { id = 9; complex = -1 },
                                        Ident ({ id = 8; complex = -1 }, "q") )
                                  ) ),
                            FCall
                              ( { id = 19; complex = -1 },
                                FCall
                                  ( { id = 16; complex = -1 },
                                    Base
                                      ( { id = 13; complex = -1 },
                                        Ident
                                          ( { id = 12; complex = -1 },
                                            "khasm.Match.map" ) ),
                                    Base
                                      ( { id = 15; complex = -1 },
                                        Ident ({ id = 14; complex = -1 }, "f")
                                      ) ),
                                Base
                                  ( { id = 18; complex = -1 },
                                    Ident ({ id = 17; complex = -1 }, "w") ) )
                          ) );
                    ] ) ) );
          TopAssignRec
            ( ( "khasm.Match.make",
                TSMap
                  (TSBase "int", TSApp ([ TSBase "int" ], "khasm.Match.List"))
              ),
              ( "khasm.Match.make",
                [ "i" ],
                IfElse
                  ( { id = 47; complex = 28 },
                    FCall
                      ( { id = 28; complex = 8 },
                        FCall
                          ( { id = 27; complex = 5 },
                            Base
                              ( { id = 26; complex = 2 },
                                Ident
                                  ({ id = 25; complex = 1 }, "khasm.Stdlib.=")
                              ),
                            Base
                              ( { id = 23; complex = 2 },
                                Ident ({ id = 22; complex = 1 }, "i") ) ),
                        Base ({ id = 24; complex = 2 }, Int "0") ),
                    Base
                      ( { id = 30; complex = 2 },
                        Ident ({ id = 29; complex = 1 }, "khasm.Match.Nil") ),
                    FCall
                      ( { id = 46; complex = 17 },
                        FCall
                          ( { id = 35; complex = 5 },
                            Base
                              ( { id = 32; complex = 2 },
                                Ident
                                  ({ id = 31; complex = 1 }, "khasm.Match.Cons")
                              ),
                            Base
                              ( { id = 34; complex = 2 },
                                Ident ({ id = 33; complex = 1 }, "i") ) ),
                        FCall
                          ( { id = 45; complex = 11 },
                            Base
                              ( { id = 37; complex = 2 },
                                Ident
                                  ({ id = 36; complex = 1 }, "khasm.Match.make")
                              ),
                            FCall
                              ( { id = 44; complex = 8 },
                                FCall
                                  ( { id = 43; complex = 5 },
                                    Base
                                      ( { id = 42; complex = 2 },
                                        Ident
                                          ( { id = 41; complex = 1 },
                                            "khasm.Stdlib.-" ) ),
                                    Base
                                      ( { id = 39; complex = 2 },
                                        Ident ({ id = 38; complex = 1 }, "i") )
                                  ),
                                Base ({ id = 40; complex = 2 }, Int "1") ) ) )
                  ) ) );
          TopAssign
            ( ("main", TSMap (TSTuple [], TSTuple [])),
              ( "main",
                [ "q" ],
                LetIn
                  ( { id = 62; complex = 17 },
                    "l",
                    FCall
                      ( { id = 51; complex = 5 },
                        Base
                          ( { id = 49; complex = 2 },
                            Ident ({ id = 48; complex = 1 }, "khasm.Match.make")
                          ),
                        Base ({ id = 50; complex = 2 }, Int "1000") ),
                    LetIn
                      ( { id = 61; complex = 11 },
                        "p",
                        FCall
                          ( { id = 59; complex = 8 },
                            FCall
                              ( { id = 56; complex = 5 },
                                Base
                                  ( { id = 53; complex = 2 },
                                    Ident
                                      ( { id = 52; complex = 1 },
                                        "khasm.Match.map" ) ),
                                Base
                                  ( { id = 55; complex = 2 },
                                    Ident
                                      ( { id = 54; complex = 1 },
                                        "khasm.Stdlib.print_int" ) ) ),
                            Base
                              ( { id = 58; complex = 2 },
                                Ident ({ id = 57; complex = 1 }, "l") ) ),
                        Base ({ id = 60; complex = 2 }, Tuple []) ) ) ) );
        ];
    ]
  in
  typecheck_program_list tm
