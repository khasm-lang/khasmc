module O = struct
  let ( let* ) = Option.bind
  let ( let+ ) = Option.map
  let ( |$> ) x f = Option.map f x
  let ( |=> ) x f = Option.bind x f
end

module R = struct
  let ( let* ) = Result.bind
  let ( let+ ) = Result.map
  let ( let/ ) = Result.map_error
  let ( |$> ) x f = Result.map f x
  let ( |=> ) x f = Result.bind x f
end
