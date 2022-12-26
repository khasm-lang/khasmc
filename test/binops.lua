_K = {}
                 _K["if"] = function(c, e1, e2)
                 if c then return e1() else return e2() end
                 end
                 -- END PRELUDE                            
-- BEGIN FILE Binops
-- EXTERN print : ∀a, (a) -> (())
-- TOPASSIGN |> : ∀0_t, ∀1_t, (0_t) -> (((0_t) -> (1_t)) -> (1_t))
_K["7C3E"] = function(x) return function(f) return (f)(x) end
 end
-- TOPASSIGN first : ∀2_t, ∀3_t, ((2_t, 3_t)) -> (2_t)
_K["6669727374"] = function(x) return (x)[1] end
-- TOPASSIGN main : (()) -> (())
_K["6D61696E"] = function(x) return ((_K["7C3E"])(((_K["7C3E"])({1, 2}))(_K["6669727374"])))(print) end

-- END FILE Binops

-- END
_K["6D61696E"]()
