_K = {}
-- END PRELUDE                            
-- BEGIN FILE New-syntax
-- EXTERN print : ∀a, a -> ()
-- TOPASSIGN swap : ∀0_tvar, ∀1_tvar, (0_tvar, 1_tvar) -> (1_tvar, 0_tvar)
_K["73776170"] = function(x) return {(x)[2], (x)[1]} end
-- TOPASSIGN main : () -> ()
_K["6D61696E"] = function() return (print)(((_K["73776170"])({"first", "second"}))[1]) end

-- END FILE New-syntax

-- END
_K["6D61696E"]()
