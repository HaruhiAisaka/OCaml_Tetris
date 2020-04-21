open Graphics;;
open Command;;
Graphics.open_graph " 400x800";;

let box_width=40
let time_between_drops=1.0


let draw_block unit=
  set_color Graphics.blue;
  let y = Graphics.size_y () in
  Graphics.fill_rect (4*box_width) (y-box_width) box_width box_width;
  moveto (4*box_width) (y-box_width)

let drop unit = let (x,y) = current_point () in
  set_color Graphics.white;
  fill_rect x y box_width box_width;
  set_color Graphics.blue;
  moveto x (y-box_width);
  fill_rect x (y-box_width) box_width box_width


let right unit = let (x,y) = current_point () in
  if x =(400-box_width) then () else(
    set_color Graphics.white;
    fill_rect x y box_width box_width;
    set_color Graphics.blue;
    moveto (x+box_width) y;
    fill_rect (x+box_width) y box_width box_width)


let left unit = let (x,y) = current_point () in
  if x =0 then () else(
    set_color Graphics.white;
    fill_rect x y box_width box_width;
    set_color Graphics.blue;
    moveto (x-box_width) y;
    fill_rect (x-box_width) y box_width box_width)

let rec move  placed = 
  (**Unix.sleep 1; drop();*)
  let (x,y) = current_point() in
  match (x,y) with
  |(x,0)->draw_block();move ((x,0)::placed);
  |_-> if (List.mem (x,y-box_width) placed )then (draw_block(); move ((x,y)::placed);)
    else(
      match get_command()  with
      |Left -> if (List.mem (x-box_width,y) placed) then () else left();move placed
      |Down -> drop();move placed
      |Right -> if (List.mem (x+box_width,y) placed) then () else right();move placed
      |Pause -> ()
      |Fall new_time -> drop(); move placed;
      |None ->move placed);;



draw_block();
move [];
