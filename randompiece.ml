open Piece
let random_piece unit = match Random.int 7 with
  | 0 -> print_endline "I"; I (0,0)
  | 1 -> print_endline "O"; O
  | 2 -> print_endline "L"; L
  | 3 -> print_endline "J"; J
  | 4 -> print_endline "S"; S
  | 5 ->print_endline "Z";  Z
  | _ -> print_endline "T"; T
