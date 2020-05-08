open Highscores
open Random
open Command
open Block
open Piece

(** What sceen the gamestate is showing *)
type screen =
  | Tetris
  | Title
  | HighScores
  | NewHighScore

type t = {
  (* About the grid *)
  grid_width: int;
  grid_height: int;

  (* About the pieces *)
  blocks: Block.t list; (* TODO Map *)
  current_piece: Piece.t option;
  next_piece: Piece.t;

  (* Game State *)
  time: float;
  input_buffer: float; (* Time between succesive butten inputs *)
  free_fall_iterations: int; (* Number of times a block has fallen *)
  over: bool; (* Game over *)
  points: int;
  rows_cleared: int;
  paused: bool;
  level: int;

  (* Settings *)
  standard_rules: bool;
  (* In non-standard tetris clearing lines doesn't give points and pressing down
    insta-drops the piece *)

  (* Menus *)
  screen: screen;
  (* High Scores *)
  high_scores: Highscores.t;
  high_score_str: string (* String player types in new high score screen *)
}



(* ------ Helper Funcs ----- *)

(** Range operator from Stack Overflow. Is i..j inclusive *)
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

(** [block_tuples game] is a list of locations for [game.blocks] *)
let block_tuples game : (int * int) list =
  List.map (fun b -> Block.to_tuple b) game.blocks



(* -------- Points / Level / Block Speed ----- *)

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

(** [block_speed game] is the speed blocks will drop based on the level *)
let block_speed game =
  0.50 -. (0.05 *. (float_of_int game.level -. 1.0))

(** [update_level game] is the game after updating the level and changing the
 * block speed accordingly. *)
let update_level game =
  { game with
    level = max (calculate_level game) game.level;
  }



(* -------- Board Collisions and Line Solving ----- *)

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

(** [block_collision game blocks] is true iff a block in [blocks] overlaps with
 * a block in the game or is OOb *)
let block_collision game (blocks: Block.t list) =
  let rec collision_helper piece_positions placed =
    match piece_positions with
    |[]-> false
    |(x,y)::t-> x < 0 ||
                x >= game.grid_width ||
                y < 0 ||
                y >= game.grid_height ||
                List.mem (x,y) placed ||
                collision_helper t placed
  in collision_helper (List.map Block.to_tuple blocks) (block_tuples game)

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
    level = max (calculate_level game) game.level;
    blocks = new_blocks;
    rows_cleared = game.rows_cleared + lines_cleared;
    points = new_points
  }



(* ------ Moving Pieces ------------ *)

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

(** [instadrop game piece] is the game after attempting to instantly commit a
 * falling piece to the board, by dropping and commiting it. *)
let instadrop game piece =
  let shift_blocks blocks by =
    List.map
      (fun b -> let (x, y) = Block.to_tuple b in
        Block.create (x, y + by) (Piece.piece_color piece))
      blocks
  in
  let rec bubble (blocks: Block.t list) =
    let shifted = shift_blocks blocks (-1) in
    if block_collision game shifted then
      blocks
    else
      bubble shifted
  in
  let new_blocks_set = bubble (Piece.to_blocks piece) in
  { game with
    current_piece = None;
    points = game.points + points_for_drop game;
    free_fall_iterations = 0;
    blocks = game.blocks @ new_blocks_set;
    time = Unix.gettimeofday ()
  }

(** [spawn_piece game] is the game after making the [next_piece] the
 * [current_piece] and generating a new next_piece. If the [current_piece]
 * intersects then it is None. *)
let spawn_piece game =
  let start_loc = (game.grid_width / 2, game.grid_height - 1) in
  let new_piece = Piece.create start_loc (Randompiece.random_piece ()) in
  let opt_piece = if
    not (collision game game.next_piece) then Some game.next_piece
    else None in
  { game with current_piece = opt_piece; next_piece = new_piece }



(* ------ Signature ------------ *)

let game_over game =
  game.over

let paused game =
  game.paused

let rows_cleared game =
  game.rows_cleared

let points game =
  game.points

let level game =
  game.level

(** [current_piece game] is the current piece being dropped by the player *)
let current_piece game =
  game.current_piece

let next_piece game =
  game.next_piece

(** [blocks game] is a list of the blocks in the board  *)
let blocks game =
  game.blocks

(** [screen game] is the screen of the game record *)
let screen game =
    game.screen

(** [high_scores game] is the [high_scores] of the game record *)
let high_scores game =
  game.high_scores

(** [high_score_str game] is the [high_score_str] in game record *)
let high_score_str game =
  game.high_score_str

(** [init dimensions standard] creates a tetris game with a board of size
    [dimensions] and uses standard rules if [standard] is true or NES rules
    otherwise *)
let init dimensions standard =
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
    standard_rules = standard;
    screen = Title;
    high_scores = Highscores.read_scores ();
    high_score_str = "";
  }

(** [reset game] is a reset version of the game state, retaining board size and
    ruleset. *)
let reset game = init (game.grid_width, game.grid_height) game.standard_rules

let tetris game =
  if game.over then (* Game over *)
    (* High scores or reset *)
      match Highscores.is_new_high_score game.high_scores game.points with
      | false -> { (reset game) with screen = Title; over = false }
      | true -> { game with screen = NewHighScore; over = false }
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
      | None -> game
      | Rotate_Right -> rotate_piece game Rotate_Right active_piece
      | Rotate_Left -> rotate_piece game Rotate_Left active_piece
      | Left -> move_piece game Left active_piece
      | Right -> move_piece game Right active_piece
      | Down ->
        if game.standard_rules then
          instadrop game active_piece |> clean_rows |> update_level
        else
          move_piece game Down active_piece
      | Fall new_time ->
        if landed game active_piece then
          commit_if_set game active_piece |> clean_rows |> update_level
        else
          let game = move_piece game Down active_piece in
          {  game with
             time = new_time;
             free_fall_iterations = game.free_fall_iterations + 1
          }
      | _ -> game



(** Keypress logic for main screen. [main_menu game] is the screen after
    handling player's keypresses. *)
let main_menu game =
  match Command.get_command (Unix.gettimeofday ()) 100.0 with
  | Down ->  { game with standard_rules = false; screen = Tetris }
  | Right -> { game with standard_rules = true; screen = Tetris }
  | Rotate_Left -> { game with screen = HighScores }
  | Left -> Stdlib.exit 0
  (* Levels *)
  | One  -> { game with level = 1; screen = Tetris }
  | Two  -> { game with level = 2; screen = Tetris }
  | Three -> { game with level = 3; screen = Tetris }
  | Four -> { game with level = 4; screen = Tetris }
  | Five -> { game with level = 5; screen = Tetris }
  | Six  -> { game with level = 6; screen = Tetris }
  | Seven -> { game with level = 7; screen = Tetris }
  | Eight -> { game with level = 8; screen = Tetris }
  | Nine -> { game with level = 9; screen = Tetris }
  | Ten  -> { game with level = 10; screen = Tetris }
  | _ -> game

(** Keypress logic for high scores screen. [high_scores_screen game is the
    screen after handling presses *)
let high_scores_screen game =
  match Command.get_command (Unix.gettimeofday ()) 100.0 with
  | Down | Rotate_Left | Left -> { game with screen = Title }
  | _ -> game

let new_high_score game =
  let str, finished = Command.read_letters game.high_score_str in
  if finished
  then
    let new_score = Highscores.make_score str game.points game.level
        game.rows_cleared game.standard_rules in
    let new_hs = Highscores.add  new_score game.high_scores in
    Highscores.write_scores new_hs;
    { (reset game) with screen = HighScores; }
  else { game with high_score_str = String.escaped str }


(** [process game] is the game after updating with player input and the time. *)
let process game =
    match game.screen with
    | Tetris -> tetris game
    | Title -> main_menu game
    | HighScores -> high_scores_screen game
    | NewHighScore -> new_high_score game
