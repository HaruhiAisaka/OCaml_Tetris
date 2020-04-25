open Graphics;;
open Command;;
open Block;;
open Piece;;
open Randompiece;;

Graphics.open_graph " 400x800";;

(** [box_width] is the length of a 1 x 1 block in pixels*)
let box_width=40

(** [time_between_drops] is the number of seconds between each forced drop*)
let time_between_drops=1.0

(** [draw_block color block] draws [block] with the color: [color]*)
let draw_block color (block:Block.t)=
  let (x,y) = to_tuple block in
  set_color color;
  Graphics.fill_rect (x*box_width) (y*box_width) box_width box_width

(** [draw_piece color piece] draws [piece] with the color: [color]*)
let draw_piece color (piece:Piece.t)=
  match piece|>to_blocks with
  |blocks-> List.fold_left (fun unit block-> draw_block color block) () blocks

(**[collision piece placed] is true iff a block in [piece] overlaps with a point
   in [placed] or a block is outside of the grid*)
let collision piece placed =
  let rec collision_helper piece_positions placed =
    match piece_positions with
    |[]-> false
    |(x,y)::t-> x < 0 || x > 9 || List.mem (x,y) placed || collision_helper t placed
  in collision_helper (List.map (fun block -> to_tuple block) (to_blocks piece)) placed

(** [landed piece placed] is true iff a block in [piece] is directly on top of
    a point in placed*)
let landed piece placed =
  let rec landed_helper piece_positions placed =
    match piece_positions with
    |[]-> false
    |(x,y)::t-> List.mem (x,y-1) placed || landed_helper t placed
  in landed_helper (List.map (fun block -> to_tuple block) (to_blocks piece)) placed

(** [move_piece piece dir color] moves [piece] in the direction [dir] with color [color]*)
let move_piece piece dir color=
  draw_piece white piece;
  draw_piece color (dir piece)

(** [move] is the main control point of the game. It generates a game of tetris and
    moves pieces based on user input. *)
let move unit=
  let rec update last_drop placed (current_piece:Piece.t) new_piece =
    if new_piece then let new_piece = random_piece() in
      (update last_drop placed (create (4,19) new_piece) false) else
    if landed current_piece placed then
      update last_drop (List.append placed (current_piece |> to_blocks |> (List.map (fun block-> to_tuple block)))) current_piece true
    else let new_color = piece_color current_piece in
      match get_command last_drop time_between_drops with
      |Left -> if collision (Piece.left current_piece) placed then
          update last_drop placed current_piece false else
          (move_piece current_piece left new_color;
           update last_drop placed (Piece.left current_piece) false)
      |Down -> move_piece current_piece down new_color;
        update last_drop placed (down current_piece) false
      |Right -> if collision (Piece.right current_piece) placed then
          update last_drop placed current_piece false else
          (move_piece current_piece right new_color;
           update last_drop placed (Piece.right current_piece) false)
      |Pause -> ()
      |Fall new_time -> move_piece current_piece down new_color;
        update new_time placed (down current_piece) false
      |Rotate_Right->if collision (rotate_right current_piece) placed then
          update last_drop placed current_piece false else
          (move_piece current_piece rotate_right new_color;
           update last_drop placed (rotate_right current_piece) false)
      |Rotate_Left->if collision (rotate_left current_piece) placed then
          update last_drop placed current_piece false else
          (move_piece current_piece rotate_left new_color;
           update last_drop placed (rotate_left current_piece) false)
      |None ->update last_drop placed current_piece false in
  update (Unix.gettimeofday()) [(0,-1);(1,-1);(2,-1);(3,-1);(4,-1);(5,-1);(6,-1);(7,-1);(8,-1);(9,-1)] (create (4,19) T) true;;







move ();
