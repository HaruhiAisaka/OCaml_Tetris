open Graphics;;
open Command;;
open Block;;
open Piece;;
Graphics.open_graph " 400x800";;

let box_width=40
let time_between_drops=1.0


let draw_block2 color (block:Block.t)= 
  let (x,y) = to_tuple block in
  set_color color;
  Graphics.fill_rect (x*box_width) (y*box_width) box_width box_width

let draw_piece color (piece:Piece.t)=
  match piece|>to_blocks with
  |blocks-> List.fold_left (fun unit block-> draw_block2 color block) () blocks

let collision piece placed = 
  let rec collision_helper piece_positions placed = 
    match piece_positions with 
    |[]-> false
    |(x,y)::t-> x < 0 || x > 9 || List.mem (x,y) placed || collision_helper t placed
  in collision_helper (List.map (fun block -> to_tuple block) (to_blocks piece)) placed

let landed piece placed = 
  let rec landed_helper piece_positions placed = 
    match piece_positions with 
    |[]-> false
    |(x,y)::t-> List.mem (x,y-1) placed || landed_helper t placed
  in landed_helper (List.map (fun block -> to_tuple block) (to_blocks piece)) placed

let move_piece piece dir=
  draw_piece white piece;
  draw_piece red (dir piece)

let move (last_drop:float) placed= 
  let rec update last_drop placed (current_piece:Piece.t) new_piece = 
    if new_piece then (draw_piece Graphics.red current_piece;update last_drop placed (create (4,18) L) false) else 
    if landed current_piece placed then 
      update last_drop (List.append placed (current_piece |> to_blocks |> (List.map (fun block-> to_tuple block)))) current_piece true
    else match get_command last_drop time_between_drops with
      |Left -> if collision (Piece.left current_piece) placed then update last_drop placed current_piece false else (move_piece current_piece Piece.left;update last_drop placed (Piece.left current_piece) false)
      |Down -> move_piece current_piece down; update last_drop placed (down current_piece) false
      |Right -> if collision (Piece.right current_piece) placed then update last_drop placed current_piece false else (move_piece current_piece Piece.right;update last_drop placed (Piece.right current_piece) false)
      |Pause -> ()
      |Fall new_time -> move_piece current_piece down; update new_time placed (down current_piece) false
      |Rotate_Right->if collision (rotate_right current_piece) placed then update last_drop placed current_piece false else (move_piece current_piece rotate_right;update last_drop placed (rotate_right current_piece) false)
      |Rotate_Left->if collision (rotate_left current_piece) placed then update last_drop placed current_piece false else (move_piece current_piece rotate_left;update last_drop placed (rotate_left current_piece) false)
      |None ->update last_drop placed current_piece false in
  update last_drop placed (create (4,18) L) true;;







move (Unix.gettimeofday()) [(0,-1);(1,-1);(2,-1);(3,-1);(4,-1);(5,-1);(6,-1);(7,-1);(8,-1);(9,-1)];
