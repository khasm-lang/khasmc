open Exp

(** for unicode *)

exception Malformed_Unicode

let split_unicode s =
  let get, rest =
    match String.get s 0 with
    | x when Char.code x < 128 ->
        let rest = String.sub s 1 (String.length s - 1) in
        let get = String.sub s 0 1 in
        (get, rest)
    | x when Int.shift_right_logical (Char.code x) 5 = 6 ->
        let rest = String.sub s 2 (String.length s - 1) in
        let get = String.sub s 0 2 in
        (get, rest)
    | x when Int.shift_right_logical (Char.code x) 4 = 14 ->
        let rest = String.sub s 3 (String.length s - 1) in
        let get = String.sub s 0 3 in
        (get, rest)
    | x when Int.shift_right_logical (Char.code x) 3 = 30 ->
        let rest = String.sub s 4 (String.length s - 1) in
        let get = String.sub s 0 4 in
        (get, rest)
    | _ -> raise Malformed_Unicode
  in
  (get, rest)

let rec unicode_len s = fst @@ split_unicode s |> String.length

let rec utf8_map f s =
  match String.length s with
  | 0 -> s
  | _ ->
      let s, r = split_unicode s in
      f s ^ utf8_map f r

let rec utf8_mapi f s =
  let rec go n f' s' =
    match String.length s' with
    | 0 -> s
    | _ ->
        let s', r' = split_unicode s' in
        f' n s' ^ go (n + 1) f' r'
  in
  go 0 f s
