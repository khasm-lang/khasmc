let fresh =
  let i = ref (-1) in
  fun () ->
    incr i;
    !i
