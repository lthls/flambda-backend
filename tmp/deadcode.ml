module M : sig val g : 'a -> 'a option * 'a option end = struct
  let f x = Some x

  external opaque : 'a -> 'a = "%opaque"

  let op = opaque ()

  let g y = op; (f y, f y)
end
