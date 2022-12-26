_K = {}
                 _K["if"] = function(c, e1, e2)
                 if c then return e1() else return e2() end
                 end
                 __kadd = function(a) return function(b) return a + b end end
                                                               __ksub = function(a) return function(b) return a - b end end
                                                               __kdiv = function(a) return function(b) return a / b end end
                                                               __kmul = function(a) return function(b) return a * b end end
                                                               __kpow = function(a) return function(b) return a ^ b end end
                                                               __keq = function(a) return function(b) return a == b end end
                                                               

                   -- https://stackoverflow.com/questions/9168058/how-to-dump-a-table-to-console
                   function dump(o)
                   if type(o) == 'table' then
                   local s = '{ '
                   for k,v in pairs(o) do
                   if type(k) ~= 'number' then k = '"'..k..'"' end
                   s = s .. '['..k..'] = ' .. dump(v) .. ','
                   end
                   return s .. '} '
                   else
                   return tostring(o)
                   end
                   end
                   function __kshow(x) print(dump(x)) end
                   
                   -- BEGIN FILE Stdlib

-- EXTERN __kadd : ∀a, (a) -> ((a) -> (a))

-- EXTERN __ksub : ∀a, (a) -> ((a) -> (a))

-- EXTERN __kdiv : ∀a, (a) -> ((a) -> (a))

-- EXTERN __kmul : ∀a, (a) -> ((a) -> (a))

-- EXTERN __kpow : ∀a, (a) -> ((a) -> (a))

-- EXTERN __keq : ∀a, (a) -> ((a) -> (bool))

-- EXTERN __kshow : ∀a, (a) -> (())

-- TOPASSIGN show : ∀a, (a) -> (())
_K["73686F77"] = __kshow
-- TOPASSIGN + : ∀a, (a) -> ((a) -> (a))
_K["2B"] = __kadd
-- TOPASSIGN - : ∀a, (a) -> ((a) -> (a))
_K["2D"] = __ksub
-- TOPASSIGN / : ∀a, (a) -> ((a) -> (a))
_K["2F"] = __kdiv
-- TOPASSIGN * : ∀a, (a) -> ((a) -> (a))
_K["2A"] = __kmul
-- TOPASSIGN ** : ∀a, (a) -> ((a) -> (a))
_K["2A2A"] = __kpow
-- TOPASSIGN = : ∀a, (a) -> ((a) -> (bool))
_K["3D"] = __keq
-- END FILE Stdlib

-- BEGIN FILE Add

-- TOPASSIGN fib : (int) -> (int)
_K["666962"] = function(x) return 6 end

-- TOPASSIGN fib : (int) -> (int)
_K["666962"] = function(n) return _K["if"]( (((_K["3D"])(n))(0)), function() return n end, function() return _K["if"]( (((_K["3D"])(n))(1)), function() return n end, function() return ((_K["2B"])((_K["666962"])(((_K["2D"])(n))(1))))((_K["666962"])(((_K["2D"])(n))(2))) end ) end ) end

-- TOPASSIGN main : (()) -> (())
_K["6D61696E"] = function() return (_K["73686F77"])((_K["666962"])(35)) end

-- END FILE Add

-- END
_K["6D61696E"]()
