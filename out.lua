_K = {}
                 _K["if"] = function(c, e1, e2)
                 if c then return e1() else return e2() end
                 end
                 -- END PRELUDE                            
-- BEGIN FILE New-syntax
-- EXTERN print : ∀a, a -> ()
-- TOPASSIGN swap : ∀0_tvar, ∀1_tvar, (0_tvar, 1_tvar) -> (1_tvar, 0_tvar)
_K["73776170"] = function(x) return {(x)[2], (x)[1]} end
-- TOPASSIGN main : () -> ()
_K["6D61696E"] = function() return _K["if"]( (true), function() (print)("hi") end, function() (print)("ho") end ) end

-- END FILE New-syntax

-- END
_K["6D61696E"]()
