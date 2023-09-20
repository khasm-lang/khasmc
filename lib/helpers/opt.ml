let to_fixpoint maxi func value =
  let rec to_fixpoint_h acc value =
    let value' = func value in
    if value' = value || acc > maxi then
      (acc, value')
    else
      to_fixpoint_h (acc + 1) value'
  in
  let acc, value' = to_fixpoint_h 0 value in
  print_endline @@ "took: " ^ string_of_int acc ^ " iters for fixpoint";
  value'
