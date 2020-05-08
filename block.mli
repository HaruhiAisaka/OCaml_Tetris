(**  Representation of a block in tetris.

     This module represents a block in a game of tetris. A block is a single
     square that occupies one square in 10x35 grid game space that tetris is
     played on.

     This module might change to a functor if we want to styalize different blocks
     in tetris, in terms of color.
*)
open Graphics

(**The abstraction type of values representing a block in tetris.
   The type is represented as a single tuple containing two int's representing
   the x axis and y axis. The int's can not be negative.*)
type t

(** The different colors of blocks.*)
type color = Graphics.color

(** [create xy color] is the initial state of the block given two int cordinates.
    Requires: [xy] be a tuple of non-negative ints.*)
val create : (int*int) -> color -> t

(** [left block] is the block moved one square to the left. *)
val left : t -> t

(** [right block] is the block moved one square to the right. *)
val right : t -> t

(** [down block] is the block moved one square down. *)
val down : t -> t

(** [up block] is the block moved up square down. *)
val up : t -> t

(**[to_tuple block] is the cordinates of the block
   represented as a list of tuples*)
val to_tuple : t -> (int*int)

(**[color block] is the color of the block*)
val color : t -> color
