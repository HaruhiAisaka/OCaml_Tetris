open Command

(* ---- Representation ---- *)
type t = { dunno: int }

let init = { dunno = 1 }

let process deltatime control state =
  match control with
  | Right -> print_endline "right"; state
  | Left -> print_endline "left"; state
  | Down -> print_endline "down"; state
  | Pause -> print_endline "paused"; state
  | Fall _ -> print_endline "fall"; state
  | _ -> state

let is_valid piece state =
  failwith "unimplemted"


(* ---- Rules + Game State of Tetris ---- *)

let game_over state =
  failwith "unimplemted"

let paused state =
  failwith "unimplemted"

let next_piece state =
  failwith "unimplemted"

let points state =
  failwith "unimplemted"

let level state =
  failwith "unimplemted"


let block_speed state =
  failwith "unimplemted"

(* ---- Information ----- *)

(** [current_piece state] is the current piece being dropped by the player *)
let current_piece state =
  failwith "unimplemted"

(** [blocks state] is a list of the blocks in the board  *)
let blocks state =
  failwith "unimplemted"

