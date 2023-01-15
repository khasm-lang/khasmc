_K = {}
                 _K["if"] = function(c, e1, e2)
                 if c then return e1() else return e2() end
                 end

_K[";"] = function(a, b) a(); return b() end
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
                   s = s .. ' ['..k..'] = ' .. dump(v) .. ', '
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
_K["Khasmc show"] = _Kshow
-- TOPASSIGN + : ∀a, (a) -> ((a) -> (a))
_K["Khasmc +"] = _Kadd
-- TOPASSIGN - : ∀a, (a) -> ((a) -> (a))
_K["Khasmc -"] = _Ksub
-- TOPASSIGN / : ∀a, (a) -> ((a) -> (a))
_K["Khasmc /"] = _Kdiv
-- TOPASSIGN * : ∀a, (a) -> ((a) -> (a))
_K["Khasmc *"] = _Kmul
-- TOPASSIGN ** : ∀a, (a) -> ((a) -> (a))
_K["Khasmc **"] = _Kpow
-- TOPASSIGN = : ∀a, (a) -> ((a) -> (bool))
_K["Khasmc ="] = _Keq
-- TOPASSIGN |> : ∀a, ∀b, (a) -> (((a) -> (b)) -> (b))
_K["Khasmc |>"] = function(x) return function(f) return (f)(x) end end
-- TOPASSIGN $ : ∀a, ∀b, ((a) -> (b)) -> ((a) -> (b))
_K["Khasmc $"] = function(f) return function(x) return (f)(x) end end
-- TOPASSIGN % : ∀a, ∀b, ∀c, ((b) -> (c)) -> (((a) -> (b)) -> ((a) -> (c)))
_K["Khasmc %"] = function(f) return function(g) return function(x) return (f)((g)(x)) end end end
-- TOPASSIGN %> : ∀a, ∀b, ∀c, ((a) -> (b)) -> (((b) -> (c)) -> ((a) -> (c)))
_K["Khasmc %>"] = function(f) return function(g) return function(x) return (g)((f)(x)) end end end
-- END FILE Stdlib

-- BEGIN FILE Fib

-- TOPASSIGN fib : (int) -> (int)
_K["Khasmc fib"] = function(n) return 6 end
-- TOPASSIGN quad : (float) -> ((float) -> ((float) -> ((float, float))))
_K["Khasmc quad"] = function(a) return function(b) return function(c) return (function( dis) return (function( nb) return (function( ta) return ({((_K["Khasmc /"])(((_K["Khasmc +"])(nb))(dis)))(ta), ((_K["Khasmc /"])(((_K["Khasmc -"])(nb))(dis)))(ta)}) end)( ((_K["Khasmc *"])(2.0))(a) ) end)( ((_K["Khasmc -"])(0.0))(b) ) end)( ((_K["Khasmc **"])(((_K["Khasmc +"])(((_K["Khasmc **"])(b))(2.0)))(((_K["Khasmc *"])(((_K["Khasmc *"])(4.0))(a)))(c))))(0.5) ) end end end
-- TOPASSIGN fib : (int) -> (int)
_K["Khasmc fib"] = function(n) return _K["if"]( (((_K["Khasmc ="])(n))(0)), function() return n end, function() return _K["if"]( (((_K["Khasmc ="])(n))(1)), function() return n end, function() return ((_K["Khasmc +"])((_K["Khasmc fib"])(((_K["Khasmc -"])(n))(1))))((_K["Khasmc fib"])(((_K["Khasmc -"])(n))(2))) end ) end ) end
-- TOPASSIGN main : (()) -> (())
_K["Khasmc main"] = function() return _K[";"](function() (_K["Khasmc show"])("Fib 10:") end, function() _K[";"](function() ((_K["Khasmc $"])(_K["Khasmc show"]))((_K["Khasmc fib"])(10)) end, function() _K[";"](function() (_K["Khasmc show"])("Quadratic x^2 + 5x") end, function() (function( q) return _K[";"](function() ((_K["Khasmc $"])(_K["Khasmc show"]))((q)[1]) end, function() _K[";"](function() ((_K["Khasmc $"])(_K["Khasmc show"]))((q)[2]) end, function() (_K["Khasmc show"])(q) end) end) end)( (((_K["Khasmc quad"])(1.0))(5.0))(0.0) ) end) end) end) end
-- END FILE Fib

-- END


_K["Khasmc main"]()