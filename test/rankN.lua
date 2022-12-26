_K = {}
                 _K["if"] = function(c, e1, e2)
                 if c then return e1() else return e2() end
                 end
                 -- END PRELUDE                            
-- BEGIN FILE RankN
-- EXTERN print : ∀a, (a) -> (())
-- TOPASSIGN id : ∀a, (a) -> (a)
_K["6964"] = function(x) return x end
-- TOPASSIGN apply : ∀a, ∀b, (a) -> (((a) -> (b)) -> (b))
_K["6170706C79"] = function(x) return function(f) return (f)(x) end
 end
-- TOPASSIGN main : (()) -> (())
_K["6D61696E"] = function(x) return (print)(((_K["6170706C79"])(10))(_K["6964"])) end

-- END FILE RankN

-- END
_K["6D61696E"]()
