let err err = Error [ err ]
let err' err = Error err
let ok ok = Ok ok

let ( let* ) r fn =
  match r with
  | Ok s -> ( match fn s with Ok s -> Ok s | Error k -> Error k)
  | Error k -> Error k

let ( let+ ) r fn =
  match r with Ok s -> Ok (fn s) | Error k -> Error k

let collect (x : ('a, 'b list) result list) :
    ('a list, 'b list) result =
  let maybe_err =
    List.filter (function Ok _ -> false | Error _ -> true) x
  in
  match maybe_err with
  | [] -> ok @@ List.map (fun (Ok s) -> s) x
  | xs -> Error (List.flatten @@ List.map (fun (Error s) -> s) xs)

let ( let/ ) x f = Result.map_error f x
let ( |$> ) x f = Result.map f x
let ( |=> ) x f = Result.bind x f
