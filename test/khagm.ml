
let rec fib n =
  if n <= 1 then
    n
  else
    fib (n - 1) + fib (n - 2)

let main : () -> () = print_endline 10
