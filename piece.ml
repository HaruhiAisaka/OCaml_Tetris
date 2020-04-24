open Block

type piece_name = 
  | I
  | O
  | L
  | J
  | S
  | Z
  | T

type rotation = int

type t = (piece_name*Block.t list)

(* -----------------------Helper Functions for Create------------------------ *)

(**[create_I xy] is an I piece with the pivot at cordinate xy
  Requires: [xy] be a tuple of non-negative ints.*)
let create_I xy : Block.t list = let (x,y) = xy in
  [Block.create (x,y);
  Block.create (x+1,y);
  Block.create (x+2,y);
  Block.create (x-1,y);]

(**[create_O xy] is an O piece with the pivot at cordinate xy
  Requires: [xy] be a tuple of non-negative ints.*)
let create_O xy : Block.t list = let (x,y) = xy in
  [Block.create (x,y);
  Block.create (x+1,y);
  Block.create (x,y+1);
  Block.create (x+1,y+1);]

(**[create_L xy] is an L piece with the pivot at cordinate xy
  Requires: [xy] be a tuple of non-negative ints.*)
let create_L xy : Block.t list = let (x,y) = xy in
  [Block.create (x,y);
  Block.create (x+1,y);
  Block.create (x+1,y+1);
  Block.create (x-1,y);]

(**[create_J xy] is an J piece with the pivot at cordinate xy
  Requires: [xy] be a tuple of non-negative ints.*)
let create_J xy : Block.t list = let (x,y) = xy in
  [Block.create (x,y);
  Block.create (x-1,y);
  Block.create (x-1,y-1);
  Block.create (x+1,y);]

(**[create_S xy] is an S piece with the pivot at cordinate xy
  Requires: [xy] be a tuple of non-negative ints.*)
let create_S xy : Block.t list = let (x,y) = xy in
  [Block.create (x,y);
  Block.create (x,y+1);
  Block.create (x+1,y+1);
  Block.create (x-1,y);]

(**[create_Z xy] is an Z piece with the pivot at cordinate xy
  Requires: [xy] be a tuple of non-negative ints.*)
let create_Z xy : Block.t list = let (x,y) = xy in
  [Block.create (x,y);
  Block.create (x,y+1);
  Block.create (x-1,y+1);
  Block.create (x+1,y);]

(**[create_T xy] is an T piece with the pivot at cordinate xy
  Requires: [xy] be a tuple of non-negative ints.*)
let create_T xy : Block.t list = let (x,y) = xy in
  [Block.create (x,y);
  Block.create (x+1,y);
  Block.create (x-1,y);
  Block.create (x,y+1);]

(* ------------------------------------------------------------------------- *)


let create xy piece = 
  match piece with
  | I -> (I, (create_I xy))
  | O -> (O, (create_O xy))
  | L -> (L, (create_L xy))
  | J -> (J, (create_J xy))
  | S -> (S, (create_S xy))
  | Z -> (Z, (create_Z xy))
  | T -> (T, (create_T xy))

let left piece = List.map(Block.left) piece

let right piece = List.map(Block.right) piece

let down piece = List.map(Block.down) piece

let to_blocks piece = snd piece

(* ------------------Helper Functions for Rotate --------------------------- *)

let tuple_int_to_float tuple = 
  let (x,y) in
  ((float_of_int x),(float_of_int y))

let tuple_float_to_int tuple = 
  let (x,y) in
  ((int_of_float x), (int_of_float y))

let tuple_arithmatic f tuple1 tuple2 = 
  let (x1,y1) = tuple1 in
  let (x2,y2) = tuple2 in
  ((f x1 x2), (f y1,y2))

let tuple_rotation_90 left tuple  =
  let (x,y) = tuple in
    if (left) then (y,x)
    else (y,-x)

(* ------------------------------------------------------------------------- *)

let rotate_left piece =
  let (piece_name,blocks) = piece in
  let centered_tuples = List.map (Block.to_tuple) blocks
    |> List.map (tuple_int_to_float)
    |> List.map (tuple_arithmatic (+.) (0.5,0.5))
  in
  match piece_name with
  | O -> piece
  | I -> 
    let origin = tuple_arithmatic (+.) (0.5,-0.5) (fst centered_tuples) in
    List.map (tuple_arithmatic (-.) centered_tuples origin) 
    |> List.map (tuple_rotation_90 true)
    |> List.map (tuple_arithmatic (+.) origin)
    |> List.map (tuple_float_to_int)
    |> List.map (Block.create)
  | _ -> let origin = fst centered_tuples in
    List.map (tuple_arithmatic (-.) centered_tuples origin) 
    |> List.map (tuple_rotation_90 true)
    |> List.map (tuple_arithmatic (+.) origin)
    |> List.map (tuple_float_to_int)
    |> List.map (Block.create)

let rotate_right piece =
  let (piece_name,blocks) = piece in
  let centered_tuples = List.map (Block.to_tuple) blocks
    |> List.map (tuple_int_to_float)
    |> List.map (tuple_arithmatic (+.) (0.5,0.5))
  in
  match piece_name with
  | O -> piece
  | I -> 
    let origin = tuple_arithmatic (+.) (0.5,-0.5) (fst centered_tuples) in
    List.map (tuple_arithmatic (-.) centered_tuples origin) 
    |> List.map (tuple_rotation_90 false)
    |> List.map (tuple_arithmatic (+.) origin)
    |> List.map (tuple_float_to_int)
    |> List.map (Block.create)
  | _ -> let origin = fst centered_tuples in
    List.map (tuple_arithmatic (-.) centered_tuples origin) 
    |> List.map (tuple_rotation_90 false)
    |> List.map (tuple_arithmatic (+.) origin)
    |> List.map (tuple_float_to_int)
    |> List.map (Block.create)