open Command
(* open Block *) type block = Block
(* open Piece *) type piece = Piece

(* ---- Representation ---- *)
type t = {
  grid: block list;
  current_piece: piece option;
  next_piece: piece;
  over: bool;
  block_speed: float;
  points: int;
  rows_cleared: int;
  paused: bool;
  level: int;
}



(* ---- Internal ----- *)

(** [random_number_mappings n] maps an number [n] to a tetrimino piece
  Requires: [n] is 0..7
*)
let tetrimino_map = function
  | 0 -> Piece
  | 1 -> Piece
  | 2 -> Piece
  | 3 -> Piece
  | 4 -> Piece
  | 5 -> Piece
  | 6 -> Piece
  | _ -> failwith "Passed a number without a mapping to a piece"

(** [random_piece] is a random tetrimino. Used mostly to start the game with a
    random piece *)
let random_piece () = tetrimino_map (Random.int 7)

(** Determines the next piece of the game, based on the following algorithm:
  Assign 7 of the 8 faces of an 8 sided die to a tetrimino. The result of the
  dice roll is the next piece. If you roll a face
  that is the same as the last piece or you roll an 8, you reroll. This reroll
  is final, and will result in a piece 1-7 (repeats are ok)
*)
let rec calculate_next_piece cur_piece second_roll =
  if second_roll then random_piece ()
  else
    let selected = match Random.int 8 with
      | n when 0 <= n && n < 7 -> Some (tetrimino_map n)
      | 8 -> None
      | _ -> failwith "In selecting, random number out of bounds"
    in
    match selected with
    | None ->
      calculate_next_piece cur_piece true
    | Some a when a = cur_piece ->
      calculate_next_piece cur_piece true
    | Some p -> p

(* ---- Interface ----- *)

let init = {
  grid = [];
  current_piece = None;
  next_piece = random_piece ();
  over = false;
  block_speed = 0.5;
  points = 0;
  rows_cleared = 0;
  paused = false;
  level = 1;
}

let process last_time deltatime control game =
  (* Check for game over *)
  let game = game in

  (* Step cur piece by control *)
  match control with
  | Right -> print_endline "right"; game
  | Left -> print_endline "left"; game
  | Down -> print_endline "down"; game
  | Pause -> print_endline "paused"; game
  | Fall _ -> print_endline "fall"; game
  | _ -> game

let is_valid piece gmae =
  (** Compute this when blocks + piece exist *)
  failwith "unimplemted"


(* ---- Rules + Game State of Tetris ---- *)

let game_over game =
  game.over

let paused game =
  game.paused

let next_piece game =
  game.next_piece

let points game =
  game.points

let level game =
  game.level


let block_speed game =
  game.block_speed

(* ---- Information ----- *)

(** [current_piece game] is the current piece being dropped by the player *)
let current_piece game =
  game.current_piece

(** [blocks game] is a list of the blocks in the board  *)
let blocks game =
  failwith "unimplemted"

