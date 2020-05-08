open Block
open Graphics

(**
   Representation of a piece in tetris.

   This module represents the state of a piece block in tetris. A piece is a
   colection of 4 blocks where the blocks are adjacent to one of more blocks in
   that same colection. Tetris has 7 pieces, all of which can be created using
   this module.
*)

(** The type [piece_name] represents the names of all seven possible 
    tetris blocks.*)
type piece_name = 
  | I of (int*int)
  | O
  | L
  | J
  | S
  | Z
  | T



(**The asbtract type of values representing a piece.
   The first element is the piece name of the given piece. 
   This value needs to be stored in order for the piece to properly rotate.
   A piece is a tuple with the second element being a list of 4
   blocks with the "pivot" piece as the first value in the list. 
*)
type t

(** [create xy piece] is the piece created at corodinate xy.
    Requires: [xy] be a tuple of non-negative ints.*)
val create :  (int*int) -> piece_name -> t

(** [left piece] is the piece moved one unit to the left.*)
val left : t -> t

(** [right piece] is the piece moved one unit to the right.*)
val right : t -> t

(** [down piece] is the piece moved one unit down.*)
val down : t -> t

(** [to_block piece] is the list of blocks that make the piece.*)
val to_blocks : t -> Block.t list

(** [rotate_right piece] is the piece rotated to the right.*)
val rotate_right : t -> t

(** [rotate_left piece] is the piece rotated to the left.*)
val rotate_left : t -> t

(** [piece_color piece] is the color of the piece *)
val piece_color: t-> Graphics.color
