open Effect
open Effect.Deep

type _ Effect.t +=
  | Get : unit -> int Effect.t
  | Put : int -> unit Effect.t

let get () = perform (Get ())
let put s = perform (Put s)

type mode = Initial | Modified

exception InvalidWrite

let with_state (s : int) c =
  let r = ref (Initial, s) in
  try
    c ()
  with
    | effect (Get ()), k -> continue k (snd !r)
    | effect (Put s), k ->
       (match !r with
        | (Initial, _) -> r := (Modified, s) ; continue k ()
        | (Modified, _) -> raise InvalidWrite)

let eightyeight =
  with_state 42
    (fun _ ->
      let a = get () in
      put (a + 4) ;
      a + get ())

let writeTwice =
  with_state 42
    (fun _ ->
      let a = get () in
      put (a + 4) ;
      if a * a > 666 then put 10 ;
      a + get ())
