open Block
open Command
open GameState
open Graphics
open Piece
open Randompiece

let screen_size = (400, 800)

let _ = Graphics.open_graph " 400x800"
let _ = Graphics.auto_synchronize false

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


(** Draws [piece] if it is not [None] *)
let draw_piece_if_exist (piece: Piece.t option) =
    match piece  with
    | Some p -> draw_piece (Piece.piece_color p) p
    | None -> ()

(** Draws all blocks and pieces in [game] *)
let draw_game game =
    List.iter (fun b -> draw_block Graphics.blue b) (GameState.blocks game);
    draw_piece_if_exist (GameState.current_piece game)

let tetris = GameState.init (10, 20) false

let rec play tetris =
  if GameState.game_over tetris then print_endline "game over" else

  let game = GameState.process tetris in
  begin
    clear_screen Graphics.white;
    draw_game game;
    Graphics.synchronize ();

    play game
  end


let _ = play tetris
