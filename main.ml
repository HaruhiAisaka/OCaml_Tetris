open Highscores
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

(** [display_title ()] displays the title screen *)
let display_title () =
  let name_box_x = 270 in
  let name_box_y = 595 in
  set_color black; fill_rect (name_box_x) (name_box_y) 55 25;
  set_color white; fill_rect (name_box_x + 5) (name_box_y + 5) 45 15;
  set_color black;
  moveto 280 600; draw_string "Tetris";
  moveto 250 570; draw_string "CS 3110 Edition";
  moveto 240 450; draw_string "Press [ S ] to start";
  moveto 190 420; draw_string "Press [ D ] to use standard ruleset";
  moveto 190 400; draw_string "Press a number key to start at a level";
  moveto 215 380; draw_string "Press [ Q ] to see highscores";
  moveto 240 100; draw_string "Press [ A ] to quit"

(* Draws the high score screen *)
let high_scores game =
  (* create spacing of length n *)
  let spacer size str =
    let rec helper size str acc cur =
      if cur < size then helper size str (str ^ acc) (cur + 1) else acc
    in helper size str "" 0
  in
  (* Name spacing *)
  let normal_spacing = spacer 5 " " in
  let header_name_sapcing = spacer (GameState.max_high_score_str_len - 6) " "
  in

  set_color black;
  moveto 240 600; draw_string "High Scores";
  moveto 160 540; draw_string
  ("(name)" ^ header_name_sapcing ^ "   " ^
  "(score) (level) (lines cleared)");
  fill_rect 140 530 330 5;
  let scores = GameState.high_scores game |> Highscores.scores in
  for i = 0 to (List.length scores - 1) do
    let y = 500 - (i * 30) in
    let score = List.nth scores i in

    moveto 160 y;
    (* Color based on ruleset *)
    if score.standard
      then set_color magenta
      else set_color red;

    (* Layout text -- *)
    let row_str =
      let name_spacer =
        (spacer
          ((GameState.max_high_score_str_len) - String.length score.name) " ")
          ^ normal_spacing in
      (* Construct string *)
      let str = string_of_int in
      score.name ^
      name_spacer ^
      (str score.points) ^
      normal_spacing ^
      (str score.level) ^
      normal_spacing ^
      (str score.line_cleared)
    in
    (* Draw it *)
    draw_string row_str
  done

(** [display_enter_high_score_screen] draws the new high score screen *)
let display_enter_high_score_screen game =
  set_color black;
  moveto 240 600; draw_string "New High Score!";
  moveto 160 500; draw_string
    "Enter your name here. Press [ Enter ] when finished";
  let name_box_x = 230 in
  let name_box_y = 380 in
  set_color (Randompiece.random_color ()); fill_rect (name_box_x) (name_box_y) 105 30;
  set_color white; fill_rect (name_box_x + 5) (name_box_y + 5) 95 20;
  set_color magenta;
  moveto (name_box_x + 8) (name_box_y + 8); draw_string (GameState.high_score_str game)


(** [display_game_over] draws the game over box *)
let display_game_over () =
  set_color black; fill_rect 60 555 310 50;
  set_color white; fill_rect 65 560 300 40;
  set_color black;
  moveto 75 575; draw_string "Game Over! Press any Key to Return to the Menu"

let tetris_init = GameState.init (10, 20) false

(** [play tetris] is the render loop of the game *)
let rec play tetris =
  (* Game over *)
  if GameState.game_over tetris then let _ = display_game_over () in
    let _= synchronize() in
    let _ = wait_for_key () in
    play (GameState.process tetris) else

    (* Regular Processing *)
    let game = GameState.process tetris in
    begin
      clear_screen Graphics.white;

      let _ = match GameState.screen game with
        | Tetris -> begin
            draw_game game;
            display_info game;
          end
        | Title -> display_title ();
        | HighScores -> high_scores game;
        | NewHighScore -> display_enter_high_score_screen game;
      in

      Graphics.synchronize ();

      play game
    end


let _ = play tetris_init
