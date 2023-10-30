let count = ref 0

let unique () =
  let tmp = !count in
  count := !count + 1;
  tmp

let unique_curr () = !count
