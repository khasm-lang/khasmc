let err err = Error [ err ]
let err' err = Error err
let ok ok = Ok ok

let withid (id : Info.id) (e : ('b, 'a) result) : ('b, 'c) result =
  match e with Ok _ -> e | Error e -> Error [ `Id (id, e) ]

let ( let* ) r fn =
  match r with
  | Ok s -> ( match fn s with Ok s -> Ok s | Error k -> Error k)
  | Error k -> Error k

let ( let+ ) r fn =
  match r with Ok s -> Ok (fn s) | Error k -> Error k

let ( and* ) a b =
  match (a, b) with
  | Ok a, Ok b -> Ok (a, b)
  | Error e, Ok _ | Ok _, Error e -> Error e
  | Error q, Error w -> Error (q @ w)

let ( and+ ) a b = ( and* ) a b

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

let option_app (f : 'a -> ('b, 'c) result) (x : 'a option) :
    ('b option, 'c) result =
  match x with
  | None -> ok None
  | Some n -> (
      match f n with Ok s -> ok @@ Some s | Error e -> Error e)

type error_location = Frontend'
[@@deriving show { with_path = false }]

let compiler_error loc str =
  print_string
    ("Compiler error in " ^ show_error_location loc ^ " :\n");
  print_string str;
  print_string "\n\n Aborting compilation.\n";
  exit 1
