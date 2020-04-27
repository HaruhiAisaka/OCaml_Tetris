open Command
open Block
open Piece

(**
 * The game state represents the abstract state of the entire game.
 *)

(**  The type of the Game State representation *)
type t

(** The initialized game state *)
val init : int * int -> t


(* ---- Processing ----- *)

(** TODO rewrite *)
val process : t -> t


(* ---- Rules + Game State of Tetris ---- *)

(** [is_valid piece] is true if the [piece] with its given orientation can exist in the board without
    violating the rules of tetris *)
val is_valid : _ -> t -> bool

(** [game_over state] is true if the state of the game cannot allow for any more
    tetris pieces to be dropped (AKA game over in NES tetris) *)
val game_over : t -> bool

(** [paused state] is true if the game is in a paused state *)
val paused : t -> bool

(** [next_piece state] is the next piece to be dropped once the current one has
    been placed by the player *)
val next_piece : t -> Piece.t
(* NOTE might end up nmoving this to another module, depends if we want game
 * state to be dictatin this *)

(** [points state] is the amount of points a player has gotten by playing *)
val points : t -> int

(** [level state] is the level the player is currently on *)
val level : t -> int

(** [block_speed state] is speed at which blocks are dropping, in seconds (?) *)
val block_speed : t -> float


(* ---- Information ----- *)

(** [current_piece state] is the current piece being dropped by the player *)
val current_piece : t -> Piece.t option

(** [blocks state] is a list of the blocks in the board  *)
val blocks: t -> Block.t list

