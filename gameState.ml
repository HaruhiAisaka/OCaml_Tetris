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
  blocks: Block.t list; (* TODO Map? *)
  current_piece: Piece.t option;
  next_piece: Piece.t;
  (* Game State *)
  time: float;
  input_buffer: float; (* Time between succesive butten inputs *)
  free_fall_iterations: int; (* Number of times a block has fallen *)
  over: bool;
  points: int;
  rows_cleared: int;
  paused: bool;
  level: int;
  (* Settings *)
  standard_rules: bool;
  (* In non-standard tetris clearing lines doesn't give points and pressing down
   * insta-drops the piece *)
}

(* ---- Rules + Game State of Tetris ---- *)

let game_over game =
  game.over

let paused game =
  game.paused

let next_piece game =
  failwith "a"
(*   game.next_piece *)

let points game =
  game.points

let level game =
  game.level

let block_speed game =
  0.50 -. (0.05 *. (float_of_int game.level -. 1.0))

(* ----- Helper ----- *)
(** Range operator from Stack Overflow *)
let (--) i j =
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc)
    in aux j []

(** [get_block game loc] is some block with location [loc] or None if it doesn't
 * exist. *)
let get_block game loc =
  let rec helper = function
  | b :: t -> if Block.to_tuple b = loc then Some b else helper t
  | [] -> None
  in
  helper game.blocks

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
(*
let random_piece (width, height) =
  tetrimino_map (width, height) (Random.int 7)
*)

(** Determines the next piece of the game, based on the following algorithm:
  Assign 7 of the 8 faces of an 8 sided die to a tetrimino. The result of the
  dice roll is the next piece. If you roll a face
  that is the same as the last piece or you roll an 8, you reroll. This reroll
  is final, and will result in a piece 1-7 (repeats are ok)
*)
(*
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
*)

(** [points_for_line number_cleared] is the amount of points the player would be rewarded
  for clearing [number_cleared] lines
  Requires: [number_cleared] is between 0 and 4 *)
let points_for_line number_cleared =
  let _ = assert (number_cleared >= 0 && number_cleared <= 4) in
  match number_cleared with
  | 0 -> 0 | 1 -> 40 | 2 -> 100 | 3 -> 300 | 4 -> 1200
  | _ -> failwith "should be impossible as the condition is asserted"


(** [points_for_drop game] is the amount of points the player would be rewarded
 * for dropping a piece *)
let points_for_drop game =
  21 + (3 * game.level) - game.free_fall_iterations

(** [calculate_level game] is the current level the player is on, as a function
    of the game state *)
let calculate_level game =
  if game.rows_cleared <= 0 then
    1
  else if game.rows_cleared >= 1 && game.rows_cleared <= 90 then
    1 + ((game.rows_cleared - 1) / 10)
  else
    10

(* ---- Blocks Representation ----- *)
let block_tuples game : (int * int) list =
  List.map (fun b -> Block.to_tuple b) game.blocks

(* ---- Managing Board ----- *)

(**[collision piece placed] is true iff a block in [piece] overlaps with a point
   in [placed] or a block is outside of the grid *)
let collision game piece =
  let rec collision_helper piece_positions placed =
    match piece_positions with
    |[]-> false
    |(x,y)::t-> x < 0 ||
                x >= game.grid_width ||
                y < 0 ||
                y >= game.grid_height ||
                List.mem (x,y) placed ||
                collision_helper t placed
  in collision_helper (List.map (fun block -> to_tuple block) (to_blocks piece)) (block_tuples game)

(** [landed piece placed] is true iff a block in [piece] is directly on top of
    a point in placed *)
let landed game piece  =
  let rec landed_helper piece_positions placed =
    match piece_positions with
    |[]-> false
    |(x,y)::t->
      (List.mem (x,y-1) placed) || (* Collision *)
      (List.exists (fun (x, y) -> y <= 0) piece_positions) || (* Bottom *)
      landed_helper t placed
  in landed_helper (List.map (fun block -> to_tuple block) (to_blocks piece)) (block_tuples game)


(* ---- Interface ----- *)

let init dimensions =
  let (w, h) = dimensions in
  let spawn = w / 2, h - 1 in
  let spawn_piece = (Piece.create spawn (Randompiece.random_piece ())) in
  {
  blocks = [];
  grid_width = w;
  grid_height = h;
  current_piece = None;
  next_piece = spawn_piece;
  over = false;
  time = 0.;
  free_fall_iterations = 0;
  input_buffer = 0.05;
  points = 0;
  rows_cleared = 0;
  paused = false;
  level = 1;
  standard_rules = false;
}

(* Moving Piece *)
(** [move_piece game direction piece] is the [game] after moving [piece] by the
 * direction specified in [direction]. Is the same as [game] if moving the
 * [piece] would move it out of bounds or collide with existing block
 * Raises: Failure if the direction is not a movement
 * *)
let move_piece game direction piece =
  let move_result =
    match direction with
    | Left -> Piece.left piece
    | Right -> Piece.right piece
    | Down ->  Piece.down piece
    | _ -> raise (Failure "GameState: direction wasn't left/right/down")
  in
  if not (collision game move_result) then
    { game with current_piece = Some move_result }
  else
    game


(* Rotation and checking validity *)
(** [rotate_piece game direction] is the [game] after moving [piece] by the
 * orientation specified in [direction]. Is the same as [game] if rotating would
 * move it out of bounds or collide with exists blocks
 * Raises: Failure if the direction is not a rotation
 * *)
let rotate_piece game direction piece =
  let rotate_result =
    match direction with
    | Rotate_Right -> Piece.rotate_right piece
    | Rotate_Left -> Piece.rotate_left piece
    | _ -> raise (Failure "GameState: can't rotate by non rotation direction")
  in
  if not (collision game rotate_result) then
    { game with current_piece = Some rotate_result }
  else
    game

(* Commiting a block to the board *)
(** [commit_if_set game piece] is [game] if [piece] was converted to blocks at
 * its current location and the current_piece is reset to [None]
 * Does NOT check if the piece is touching the floor/landed/etc. *)
let commit_if_set game piece =
  let piece_as_blocks = Piece.to_blocks piece in
  { game with
    blocks = game.blocks @ piece_as_blocks;
    current_piece = None;
    points = game.points + points_for_drop game;
    free_fall_iterations = 0
  }

(* Piece Spawning *)
(** [spawn_piece game] is the game after making the [next_piece] the
 * [current_piece] and generating a new next_piece. If the [current_piece]
 * intersects then it is None. *)
let spawn_piece game =
  let start_loc = (game.grid_width / 2, game.grid_height - 1) in
  let new_piece = Piece.create start_loc (Randompiece.random_piece ()) in
  let opt_piece = if
    not (collision game game.next_piece) then Some new_piece
    else None in
  { game with current_piece = opt_piece; next_piece = new_piece }

(* Update Score + Level *)
(** [clean_rows game] is the game after removing full rows and updating the
 * score and rows cleared accordingly. *)
let clean_rows game =
  let row_size = game.grid_width in
  (* Clears [row] and moves all rows above it down 1 block *)
  let cascade (height: int) (blocks_tpl: int * Block.t list) : (int * Block.t list) =
    let (lines_cleared, blocks) = blocks_tpl in
    let is_above_height block =
      let (_, h) = Block.to_tuple block in
      if h > height then Block.down block else block
    in
    (lines_cleared + 1, List.map is_above_height blocks)
  in
  (* is a tuple of the block list filtered of the row. If the difference in size
   * of the 2 is >= a line then treat it as a cleared line and cascade it *)
  let check_row (height: int)  (blocks_tpl: int * Block.t list) : (int * Block.t list) =
    let (lines_cleared, blocks) = blocks_tpl in
    let not_in_line height block =
      let (_, h) = Block.to_tuple block in not (h = height)
    in
    let filtered_blocks = List.filter (not_in_line height) blocks in
    let size_diff = (List.length blocks) - (List.length filtered_blocks) in
    if size_diff < row_size then
      blocks_tpl
      else (lines_cleared, filtered_blocks) |> cascade height
  in
  (* folds all rows from top to bottom, cascading and checking. Is the result of
      the combined cascades, along with a int for number of cascades done *)
  let fold_rows (blocks_tpl : int * Block.t list) : (int * Block.t list) =
    List.fold_right check_row (0 -- (game.grid_height - 1)) blocks_tpl
  in
  (* Cascade/Delete rows and calculate points *)
  let (lines_cleared, new_blocks) = fold_rows (0, game.blocks) in
  let new_points = if game.standard_rules then game.points
    else game.points + points_for_line lines_cleared
  in
  { game with
    blocks = new_blocks;
    rows_cleared = game.rows_cleared + lines_cleared;
    points = new_points
  }

(** [update_level game] is the game after updating the level and changing the
 * block speed accordingly. *)
let update_level game =
  { game with
    level = calculate_level game;
  }

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
let pe s = print_endline s

let process game =
  if game.over then (* Game over *)
    begin print_endline "game over"; game end
  else if game.paused then (* Pause *)
    begin print_endline "paused"; game end
  else
    match game.current_piece with
    | None -> begin
      let game = spawn_piece game in
      (* No active piece; spawn one *)
      match game.current_piece with
      | Some p -> game
      | None -> { game with over = true } (* Can't spawn; game over *)
    end
    (* Active piece exists, move it as normal with input/time *)
    | Some active_piece ->
      let command = Command.get_command game.time (block_speed game) in
      match command with
      | Pause -> { game with paused = true }
      | None -> game
      | Rotate_Right -> rotate_piece game Rotate_Right active_piece
      | Rotate_Left -> rotate_piece game Rotate_Left active_piece
      | Left -> move_piece game Left active_piece
      | Right -> move_piece game Right active_piece
      | Down -> move_piece game Down active_piece
      | Fall new_time ->
        pe (string_of_int (game.points));
        pe ("Lv. " ^ (string_of_int  (game.level)));
        print_endline "------";
        if landed game active_piece then
          commit_if_set game active_piece |> clean_rows
          else
          let game = move_piece game Down active_piece in
          {  game with
            time = new_time;
            free_fall_iterations = game.free_fall_iterations + 1
          }


let is_valid piece gmae =
  (** Compute this when blocks + piece exist *)
  failwith "unimplemted"



(* ---- Information ----- *)

(** [current_piece game] is the current piece being dropped by the player *)
let current_piece game =
  game.current_piece

(** [blocks game] is a list of the blocks in the board  *)
let blocks game =
  game.blocks

