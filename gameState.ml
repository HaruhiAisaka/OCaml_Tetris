open Random
open Command
open Block
open Piece

let _ = Random.self_init ()

(* ---- Representation ---- *)
type t = {
  (* About the grid *)
  grid_width: int;
  grid_height: int;
  (* About the pieces *)
  blocks: Block.t list;
  current_piece: Piece.t option;
  next_piece: Piece.t;
  (* Game State *)
  countdown: float;
  over: bool;
  points: int;
  rows_cleared: int;
  paused: bool;
  level: int;
}



(* ---- Internal ----- *)

(** [random_number_mappings n] maps an number [n] to a tetrimino piece
  Requires: [n] is 0..7
*)
let tetrimino_map (width, height) n =
  let (x, y) = ((width / 2), height) in
  match n with
  | 0 -> Piece.create (x, y) (Piece.I (1, 1))
  | 1 -> Piece.create (x, y) Piece.J
  | 2 -> Piece.create (x, y) Piece.L
  | 3 -> Piece.create (x, y) Piece.O
  | 4 -> Piece.create (x, y) Piece.S
  | 5 -> Piece.create (x, y) Piece.T
  | 6 -> Piece.create (x, y) Piece.Z
  | _ -> failwith "Passed a number without a mapping to a piece"

(** [random_piece] is a random tetrimino. Used mostly to start the game with a
    random piece *)
let random_piece (width, height) =
  tetrimino_map (width, height) (Random.int 7)

(** Determines the next piece of the game, based on the following algorithm:
  Assign 7 of the 8 faces of an 8 sided die to a tetrimino. The result of the
  dice roll is the next piece. If you roll a face
  that is the same as the last piece or you roll an 8, you reroll. This reroll
  is final, and will result in a piece 1-7 (repeats are ok)
*)
let rec calculate_next_piece game is_second_roll =
  if is_second_roll then random_piece (game.grid_width, game.grid_height)
  else
    let selected = match Random.int 8 with
      | n when 0 <= n && n < 7 ->
        Some (tetrimino_map (game.grid_width, game.grid_height) n)
      | 8 -> None
      | _ -> failwith "In selecting, random number out of bounds"
    in
    match selected with
    | None ->
      calculate_next_piece game true
    | Some a when Some a = game.current_piece ->
      calculate_next_piece game true
    | Some p -> p

(** [calculate_level game] is the current level the player is on, as a function
    of the game state *)
let calculate_level game =
  if game.rows_cleared <= 0 then
    1
  else if game.rows_cleared >= 1 && game.rows_cleared <= 90 then
    1 + ((game.rows_cleared - 1) / 10)
  else
    10


(* ---- Interface ----- *)

let init dimensions =
  let (w, h) = dimensions in
  {
  blocks = [];
  grid_width = w;
  grid_height = h;
  current_piece = None;
  next_piece = random_piece dimensions;
  over = false;
  countdown = 1.;
  points = 0;
  rows_cleared = 0;
  paused = false;
  level = 1;
}

let attempt_spawn =
  failwith "A"
let command_step game =
  failwith "A"
let drop_step game =
  failwith "A"
let update_step game =
  failwith "A"

(** [step_game game] is the game state after being updated for each render loop
 * ~ in drop
 *  move the piece by player control
 * ACTIVE PIECE ? -->
 *  time to fall -> step it down
 *      --> if possible, move it down ELSE commit to board, delete rows etc.
 * NO PIECE? --> attempt spawn piece
 *  CAN SPAWN -> make new piece and make it active
 *  CAN'T SPAWN -> game over = true
 *)
let process deltatime command game =
  failwith "todo"
(*
  if game.over then (* Game over *)
    begin print_endline "game over"; game end
  else if game.paused then (* Pause *)
    begin print_endline "paused"; game end
  else  (* Countdown the game timer *)
    let time = game.countdown -. deltatime in
*)

(*
    if time <= 0.0 || command = Down then begin
      match game.current_piece with
      (* Act on piece with [command] *)
      | Some piece ->
        command_step game |>
      (* No piece; attempt to spawn a piece *)
      | None -> failwith "A"
    end
    else
      print_endline "waiting"; game
*)



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
  0.50 -. (0.05 *. (float_of_int game.level -. 1.0))

(* ---- Information ----- *)

(** [current_piece game] is the current piece being dropped by the player *)
let current_piece game =
  game.current_piece

(** [blocks game] is a list of the blocks in the board  *)
let blocks game =
  game.blocks

