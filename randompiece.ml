open Piece

let _ = Random.self_init ()

let random_piece unit = match Random.int 7 with
  | 0 -> I (0,0)
  | 1 -> O
  | 2 -> L
  | 3 -> J
  | 4 -> S
  | 5 -> Z
  | _ -> T
