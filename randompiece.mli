(** 
   This module chooses a tetris piece at random.
*)

(** [random_piece] is the piece_name choosen at random.*)
val random_piece : unit -> Piece.piece_name

(** [random_color] is a graphics random color *)
val random_color : unit -> Graphics.color
