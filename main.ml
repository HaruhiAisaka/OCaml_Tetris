open Block
open Command
open GameState
open Graphics
open Piece
open Randompiece

let screen_size = (600, 800)

let _ = Graphics.open_graph " 600x800"
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

(** [draw_block color block] draws [block] on the board*)
let draw_block (block:Block.t)=
  let (x,y) = to_tuple block in
  set_color (Block.color block);
  Graphics.fill_rect (x*box_width) (y*box_width) box_width box_width

(** [draw_piec piece] draws [piece] on the board*)
let draw_piece (piece:Piece.t)=
  match piece|>to_blocks with
  |blocks-> List.fold_left (fun unit block-> draw_block block) () blocks


(** Draws [piece] if it is not [None] *)
let draw_piece_if_exist (piece: Piece.t option) =
  match piece  with
  | Some p -> draw_piece  p
  | None -> ()

(** Draws all blocks and pieces in [game] *)
let draw_game game =
  List.iter (fun b -> draw_block  b) (GameState.blocks game);
  draw_piece_if_exist (GameState.current_piece game)

(** [draw_block block] draws [block] in the information box*)
let draw_next_block (block:Block.t)=
  let (x,y) = to_tuple block in
  set_color (Block.color block);
  Graphics.fill_rect (400+ x*box_width/2) (250+y*box_width/2) (box_width/2) (box_width/2)

(** [draw_next_piece game] Draws the next piece in the information box*)
let draw_next_piece game=
  let piece = next_piece game in
  match piece|>to_blocks with
  |blocks-> List.iter (fun block-> draw_next_block block) blocks


(** [display_score game] prints the current score and level onto the screen*)
let display_score game=
  let score= game |> points |> string_of_int in
  let level = game |> level |> string_of_int in
  set_color black;
  moveto 470 725; draw_string ("Score: "^score);
  moveto 470 700; draw_string ("Level: "^ level)

(** [display_info game] displays the score, level, and next piece onto the board*)
let display_info game =
  set_color black; fill_rect 450 600 100 150;
  set_color white; fill_rect 455 605 90 140;
  set_color black; fill_rect 401 0 3 800;
  moveto 470 675; draw_string "Next Piece:";
  draw_next_piece game;
  display_score game

let tetris = GameState.init (10, 20) false

(** [play tetris] is the render loop of the game *)
let rec play tetris =
  if GameState.game_over tetris then print_endline "game over" else

    let game = GameState.process tetris in
    begin
      clear_screen Graphics.white;
      if GameState.screen game = GameState.Tetris then begin
        draw_game game;
        display_info game
      end
      else
        ();
      Graphics.synchronize ();

      play game
    end


let _ = play tetris
