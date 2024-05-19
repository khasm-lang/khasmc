module O = struct
  let ( let* ) = Option.bind

  let ( let+ ) x f =
    match x with Some k -> Some (f k) | None -> None

  let ( |$> ) x f = Option.map f x
  let ( |=> ) x f = Option.bind x f
end
