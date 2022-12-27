_K = {}
                 _K["if"] = function(c, e1, e2)
                 if c then return e1() else return e2() end
                 end
                 _Kadd = function(a) return function(b) return a + b end end
                                                               _Ksub = function(a) return function(b) return a - b end end
                                                               _Kdiv = function(a) return function(b) return a / b end end
                                                               _Kmul = function(a) return function(b) return a * b end end
                                                               _Kpow = function(a) return function(b) return a ^ b end end
                                                               _Keq = function(a) return function(b) return a == b end end
                                                               

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
                   function _Kshow(x) print(dump(x)) end
                   
                   -- BEGIN FILE Stdlib

-- EXTERN _Kadd : ∀a, (a) -> ((a) -> (a))

-- EXTERN _Ksub : ∀a, (a) -> ((a) -> (a))

-- EXTERN _Kdiv : ∀a, (a) -> ((a) -> (a))

-- EXTERN _Kmul : ∀a, (a) -> ((a) -> (a))

-- EXTERN _Kpow : ∀a, (a) -> ((a) -> (a))

-- EXTERN _Keq : ∀a, (a) -> ((a) -> (bool))

-- EXTERN _Kshow : ∀a, (a) -> (())

-- TOPASSIGN show : ∀a, (a) -> (())
_K["Khasmc_73686F77"] = _Kshow
-- TOPASSIGN + : ∀a, (a) -> ((a) -> (a))
_K["Khasmc_2B"] = _Kadd
-- TOPASSIGN - : ∀a, (a) -> ((a) -> (a))
_K["Khasmc_2D"] = _Ksub
-- TOPASSIGN / : ∀a, (a) -> ((a) -> (a))
_K["Khasmc_2F"] = _Kdiv
-- TOPASSIGN * : ∀a, (a) -> ((a) -> (a))
_K["Khasmc_2A"] = _Kmul
-- TOPASSIGN ** : ∀a, (a) -> ((a) -> (a))
_K["Khasmc_2A2A"] = _Kpow
-- TOPASSIGN = : ∀a, (a) -> ((a) -> (bool))
_K["Khasmc_3D"] = _Keq
-- END FILE Stdlib

-- BEGIN FILE Fib

-- TOPASSIGN fib : (int) -> (int)
_K["Khasmc_666962"] = function(x) return 6 end

-- TOPASSIGN fib : (int) -> (int)
_K["Khasmc_666962"] = function(n) return _K["if"]( (((_K["Khasmc_3D"])(n))(0)), function() return n end, function() return _K["if"]( (((_K["Khasmc_3D"])(n))(1)), function() return n end, function() return ((_K["Khasmc_2B"])((_K["Khasmc_666962"])(((_K["Khasmc_2D"])(n))(1))))((_K["Khasmc_666962"])(((_K["Khasmc_2D"])(n))(2))) end ) end ) end

-- TOPASSIGN main : (()) -> (())
_K["Khasmc_6D61696E"] = function() return (_K["Khasmc_73686F77"])((_K["Khasmc_666962"])(35)) end

-- END FILE Fib

-- END


_K["Khasmc_6D61696E"]()