open Block
open Command
open GameState
open Graphics
open Piece
open Randompiece

let screen_size = (400, 800)

let _ = Graphics.open_graph " 400x800"
let _ = Graphics.auto_synchronize true

(** [box_width] is the length of a 1 x 1 block in pixels*)
let box_width=40

(** [time_between_drops] is the number of seconds between each forced drop*)
let time_between_drops=1.0

(** [clear_screen color] clears the whole screen with [color] *)
let clear_screen color =
  set_color color;
  let width, height = screen_size in
  Graphics.fill_rect 0 0 width height

(** [draw_block color block] draws [block] with the color: [color]*)
let draw_block color (block:Block.t)=
  let (x,y) = to_tuple block in
  set_color color;
  Graphics.fill_rect (x*box_width) (y*box_width) box_width box_width

(** [draw_piece color piece] draws [piece] with the color: [color]*)
let draw_piece color (piece:Piece.t)=
  match piece|>to_blocks with
  |blocks-> List.fold_left (fun unit block-> draw_block color block) () blocks

let draw_piece_if_exist (piece: Piece.t option) =
    match piece  with
    | Some p -> draw_piece Graphics.red p
    | None -> ()
(*
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
*)

let tetris = GameState.init (10, 20)

let rec play tetris =
  if GameState.game_over tetris then print_endline "game over" else

  let game = GameState.process tetris in
  begin
    clear_screen Graphics.white;
    List.iter (fun b -> draw_block Graphics.blue b) (GameState.blocks game);
    draw_piece_if_exist (GameState.current_piece game);

    play game
  end


let _ = play tetris
