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
                   
                   -- BEGIN FILE Args

-- EXTERN print : ∀a, (a) -> (())

-- TOPASSIGN id : ∀a, (a) -> (a)
_K["6964"] = function(x) return x end

-- TOPASSIGN apply : ∀a, ∀b, (a) -> (((a) -> (b)) -> (b))
_K["6170706C79"] = function(x) return function(f) return (f)(x) end
 end

-- TOPASSIGN main : (()) -> (())
_K["6D61696E"] = function(x) return (print)(((_K["6170706C79"])(10))(_K["6964"])) end

-- END FILE Args

-- END
_K["6D61696E"]()
