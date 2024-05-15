module O = struct
  let ( let* ) = Option.bind
  let ( let+ ) = Option.map
end

module R = struct
  let ( let* ) = Result.bind
  let ( let+ ) = Result.map
  let ( let/ ) = Result.map_error
end
