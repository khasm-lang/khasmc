open Exp
open Args

(** Compile the C to native code *)

let gen_main () =
  {|
int main(void) {
  kha_obj * empty = make_tuple(0);
  kha_obj * ret = main_____Khasm(ref(empty));
  if (ret->tag != TUPLE) {
  fprintf(stderr, "RETURN VALUE NOT TUPLE - TYPE SYSTEM INVALID\n");
  }  
  unref(ret);
  unref(empty);
  return 0;  
}
|}

let prelude () =
  {|
#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <time.h>
#include <stdatomic.h>
#include <assert.h>
|}

let flags_slow =
  KhasmUTF.utf8_map
    (fun x ->
      if x = "\n" then
        ""
      else
        x)
    {| -O0
        -Wall
        -Wextra
        -Wno-incompatible-pointer-types
        -Wno-sign-compare
        -g 
        |}

let flags_fast =
  KhasmUTF.utf8_map
    (fun x ->
      if x = "\n" then
        ""
      else
        x)
    {| -O3
       -Wno-incompatible-pointer-types
       -Wno-sign-compare
        |}

let compile code (args : Args.cliargs) =
  let code = prelude () ^ Runtime_lib.runtime_c ^ code ^ gen_main () in
  match Sys.os_type with
  | "Win32" | "Cygwin" -> raise @@ NotSupported "Windows"
  | "Unix" ->
      let tmpdir =
        Filename.get_temp_dir_name ()
        ^ "/khasm_workdir_"
        ^ (Random.int 1000000 |> string_of_int)
      in
      Sys.mkdir tmpdir 0o755;
      let filename = tmpdir ^ "/khagm.c" in
      let oc = open_out filename in
      Printf.fprintf oc "%s\n" code;
      close_out oc;
      let code' =
        if args.opt = 0 then
          Sys.command ("cc " ^ flags_slow ^ filename ^ " -o " ^ args.out)
        else
          Sys.command ("cc" ^ flags_fast ^ filename ^ " -o " ^ args.out)
      in
      FileUtil.cp [ filename ] ("./" ^ args.out ^ ".c");
      (match code' with 0 -> () | _ -> raise @@ CompileError "CC failed");
      FileUtil.rm [ filename ];
      Sys.rmdir tmpdir;
      ()
  | _ -> raise @@ Impossible "non-standard os"
