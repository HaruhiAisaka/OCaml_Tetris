open Block
open Graphics
type piece_name = 
  | I of (int*int)
  | O
  | L
  | J
  | S
  | Z
  | T

type t = (piece_name*Block.t list)

type rotation = 
  | Zero
  | One
  | Two 
  | Three

(* -----------------------Helper Functions for Create------------------------ *)

let piece_color piece = match fst (piece:t) with
  |I (x,y) -> red
  |O -> blue
  |L -> green
  |J -> yellow
  |S -> black
  |Z -> magenta
  |T -> cyan

(**[create_I xy] is an I piece with the pivot at cordinate xy
   Requires: [xy] be a tuple of non-negative ints.*)
let create_I xy : Block.t list = 
  let (x,y) = xy in
  let color = piece_color I(0,0) in
  [Block.create (x,y) color;
   Block.create (x-1,y) color;
   Block.create (x-2,y) color;
   Block.create (x+1,y) color;]

(**[create_O xy] is an O piece with the pivot at cordinate xy
   Requires: [xy] be a tuple of non-negative ints.*)
let create_O xy : Block.t list = 
  let (x,y) = xy in
  let color = piece_color O in
  [Block.create (x,y) color;
   Block.create (x-1,y) color;
   Block.create (x,y-1) color;
   Block.create (x-1,y-1) color;]

(**[create_L xy] is an L piece with the pivot at cordinate xy
   Requires: [xy] be a tuple of non-negative ints.*)
let create_L xy : Block.t list = 
  let (x,y) = xy in
  let color = piece_color L in
  [Block.create (x,y) color;
   Block.create (x+1,y) color;
   Block.create (x-1,y-1) color;
   Block.create (x-1,y) color;]

(**[create_J xy] is an J piece with the pivot at cordinate xy
   Requires: [xy] be a tuple of non-negative ints.*)
let create_J xy : Block.t list = 
  let (x,y) = xy in
  let color = piece_color J in
  [Block.create (x,y) color;
   Block.create (x-1,y) color;
   Block.create (x+1,y-1) color;
   Block.create (x+1,y) color;]

(**[create_S xy] is an S piece with the pivot at cordinate xy
   Requires: [xy] be a tuple of non-negative ints.*)
let create_S xy : Block.t list = 
  let (x,y) = xy in
  let color = piece_color S in
  [Block.create (x,y) color;
   Block.create (x,y-1) color;
   Block.create (x+1,y) color;
   Block.create (x-1,y-1) color;]

(**[create_Z xy] is an Z piece with the pivot at cordinate xy
   Requires: [xy] be a tuple of non-negative ints.*)
let create_Z xy : Block.t list = 
  let (x,y) = xy in
  let color = piece_color Z in
  [Block.create (x,y) color;
   Block.create (x-1,y) color;
   Block.create (x,y-1) color;
   Block.create (x+1,y-1) color;]

(**[create_T xy] is an T piece with the pivot at cordinate xy
   Requires: [xy] be a tuple of non-negative ints.*)
let create_T xy : Block.t list = 
  let (x,y) = xy in
  let color = piece_color T in
  [Block.create (x,y) color;
   Block.create (x+1,y) color;
   Block.create (x-1,y) color;
   Block.create (x,y-1) color;]

(* ------------------------------------------------------------------------- *)

(* ------------------Helper Functions for Rotate --------------------------- *)

let tuple_int_to_float (tuple:(int*int)) : (float*float) =
  let (x,y) = tuple in
  ((float_of_int x),(float_of_int y))

let tuple_float_to_block_cordinate (tuple:(float*float)) : (int*int) =
  let (x,y) = tuple in
  let new_x = if (x<0.) then x -. 1.0 else x in
  let new_y = if (y<0.) then y -. 1.0 else y in
  (int_of_float new_x, int_of_float new_y)

let tuple_arithmatic f (tuple1:('a*'a)) (tuple2:('a*'a)) : ('a*'a) =
  let (x1,y1) = tuple1 in
  let (x2,y2) = tuple2 in
  ((f x1 x2), (f y1 y2))

let tuple_rotation_90 (left:bool) (tuple:(float*float)) : (float*float)  =
  let (x,y) = tuple in
  if (left) then (-.y,x)
  else (y,-.x)

let piece_list_of_tuples (piece:t) : (float*float) list =
  let (piece_name,blocks) = piece in
  List.map (Block.to_tuple) blocks
  |> List.map (tuple_int_to_float)
  |> List.map (tuple_arithmatic (+.) (0.5,0.5))

(* ------------------------------------------------------------------------- *)

let create xy piece =
  match piece with
  | I _ -> (I xy, (create_I xy))
  | O -> (O, (create_O xy))
  | L -> (L, (create_L xy))
  | J -> (J, (create_J xy))
  | S -> (S, (create_S xy))
  | Z -> (Z, (create_Z xy))
  | T -> (T, (create_T xy))

let left piece =
  match piece with
  | (I xy, y) -> (I (tuple_arithmatic (+) (-1,0) xy), List.map(Block.left) y)
  | (x,y) -> (x, List.map(Block.left) y)


let right piece =
  match piece with
  | (I xy, y) -> (I (tuple_arithmatic (+) (1,0) xy), List.map(Block.right) y)
  | (x,y) -> (x, List.map(Block.right) y)

let down piece =
  match piece with
  | (I xy, y) -> (I (tuple_arithmatic (+) (0,-1) xy), List.map(Block.down) y)
  | (x,y) -> (x, List.map(Block.down) y)

let to_blocks piece = snd piece


let rotate_left piece =
  let (piece_name,blocks) = piece in
  let centered_tuples = List.map (Block.to_tuple) blocks
                        |> List.map (tuple_int_to_float)
                        |> List.map (tuple_arithmatic (+.) (0.5,0.5))
  in
  match piece_name with
  | O -> piece
  | I xy ->
    let origin = tuple_int_to_float xy in
    let new_blocks = 
      List.map (fun x -> tuple_arithmatic (-.) x origin) centered_tuples 
      |> List.map (tuple_rotation_90 true)
      |> List.map (tuple_arithmatic (+.) origin)
      |> List.map (tuple_float_to_block_cordinate)
      |> List.map (fun xy -> Block.create xy (piece_color I(0,0))) in
    (I xy, new_blocks)
  | piece_name -> let origin = List.hd centered_tuples in
    let new_blocks = 
      List.map (fun x -> tuple_arithmatic (-.) x origin) centered_tuples 
      |> List.map (tuple_rotation_90 true)
      |> List.map (tuple_arithmatic (+.) origin)
      |> List.map (tuple_float_to_block_cordinate)
      |> List.map (fun xy -> Block.create xy (piece_color piece_name)) in
    (piece_name, new_blocks)

let rotate_right piece =
  let (piece_name,blocks) = piece in
  let centered_tuples = List.map (Block.to_tuple) blocks
                        |> List.map (tuple_int_to_float)
                        |> List.map (tuple_arithmatic (+.) (0.5,0.5))
  in
  match piece_name with
  | O -> piece
  | I xy ->
    let origin = tuple_int_to_float xy  in
    let new_blocks = 
      List.map (fun x -> tuple_arithmatic (-.) x origin) centered_tuples 
      |> List.map (tuple_rotation_90 false)
      |> List.map (tuple_arithmatic (+.) origin)
      |> List.map (tuple_float_to_block_cordinate)
      |> List.map (Block.create) in
    (I xy, new_blocks)
  | piece_name -> let origin = List.hd centered_tuples in
    let new_blocks = 
      List.map (fun x -> tuple_arithmatic (-.) x origin) centered_tuples 
      |> List.map (tuple_rotation_90 false)
      |> List.map (tuple_arithmatic (+.) origin)
      |> List.map (tuple_float_to_block_cordinate)
      |> List.map (Block.create) in
    (piece_name, new_blocks)


