type level =
  | TRACE
  | DEBUG
  | INFO
  | WARN
  | ERROR
  | FATAL

let lvl_to_ident (lvl : level) =
  match lvl with
  | TRACE -> "\x1B[0;32mTrace:"
  | DEBUG -> "\x1B[0;36mDebug:"
  | INFO -> "\x1B[0;34mInfo:"
  | WARN -> "\x1B[0;33mWarn:"
  | ERROR -> "\x1B[0;31mError:"
  | FATAL -> "\x1B[1;31m!!! FATAL !!!"

let reset = "\x1B[0m"

type message = Msg of level * string

let log_ : message list ref = ref []
let init_log () = ()
let append_ msg = log_ := msg :: !log_
let get_ () = List.rev !log_

let msg_to_str (Msg (lvl, msg)) =
  let intro = lvl_to_ident lvl in
  intro ^ "\n" ^ reset ^ msg ^ "\n\n"

let print_log () =
  let l = get_ () in
  match l with
  | [] -> print_endline "\n\x1B[1;35mlog was empty :)\x1B[0m\n"
  | _ ->
      print_string "\n\x1B[1;35m~~~   LOG   ~~~ \x1B[0m\n\n";
      List.iter (fun x -> print_string (msg_to_str x)) l;
      print_string "\x1B[1;35m~~~ END LOG ~~~ \x1B[0m\n"

let log level str rest =
  let m = Msg (level, Printf.sprintf str rest) in
  append_ m;
  match level with
  | FATAL -> raise (Invalid_argument "fatal")
  | _ -> ()

let log_fatal level str rest =
  let m = Msg (level, Printf.sprintf str rest) in
  append_ m;
  match level with
  | FATAL ->
      print_log ();
      print_endline "Khasmc terminated from fatal error!";
      exit 1
  | _ -> raise (Invalid_argument "non-fatal")

let trace x = log TRACE "%s" x
let debug x = log DEBUG "%s" x
let info x = log INFO "%s" x
let warn x = log WARN "%s" x
let error x = log ERROR "%s" x
let fatal x = log_fatal FATAL "%s" x
