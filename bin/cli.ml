(** Main driver, just hooks into the library part of the code. *)
let () =
  Khasmc.Main.main_proc ();
  print_endline "Done."
