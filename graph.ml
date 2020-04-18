
#use "topfind";;
#require "graphics";;


open Graphics;;
Graphics.open_graph "";;
let box_width=50;;
set_color Graphics.blue;;
let x = Graphics.size_x () in 
let y = Graphics.size_y () in 
Graphics.fill_rect (x/2 - box_width/2) (y-box_width) box_width box_width;
moveto (x/2 - box_width/2) (y-box_width);;

let drop unit = let (x,y) = current_point () in 
  set_color Graphics.white;
  fill_rect x y box_width box_width;
  set_color Graphics.blue;
  moveto x (y-box_width);
  fill_rect x (y-box_width) box_width box_width;;


let right unit = let (x,y) = current_point () in 
  set_color Graphics.white;
  fill_rect x y box_width box_width;
  set_color Graphics.blue;
  moveto (x+box_width) y;
  fill_rect (x+box_width) y box_width box_width;;


let left unit = let (x,y) = current_point () in 
  set_color Graphics.white;
  fill_rect x y box_width box_width;
  set_color Graphics.blue;
  moveto (x-box_width) y;
  fill_rect (x-box_width) y box_width box_width;;

let rec move unit = 
  match (wait_next_event [Key_pressed]).key with 
  |'a'-> left();move();
  |'s'-> drop();move();
  |'d'-> right();move();
  |'0'-> ();
  |_->move();