open Command
open Block
open Piece

(**
 * The game state represents the abstract state of the entire game.
*)

(**  The type of the Game State representation *)
type t

type screen =
  | Tetris
  | Title

(** The initialized game state *)
val init : int * int -> bool -> t

(* ---- Processing ----- *)

(** [process game] is the game after being updated with the player's action and
    time. *)
val process : t -> t


(* ---- Rules + Game State of Tetris ---- *)

(** [game_over state] is true if the state of the game cannot allow for any more
    tetris pieces to be dropped (AKA game over in NES tetris) *)
val game_over : t -> bool

(** [paused state] is true if the game is in a paused state *)
val paused : t -> bool

(** [points state] is the amount of points a player has gotten by playing *)
val points : t -> int

(** [level state] is the level the player is currently on *)
val level : t -> int

(* ---- Information ----- *)

(** [current_piece state] is the current piece being dropped by the player *)
val current_piece : t -> Piece.t option

(** [next_piece state] is the next piece that will be dropped *)
val next_piece : t -> Piece.t

(** [blocks state] is a list of the blocks in the board  *)
val blocks: t -> Block.t list

val screen: t -> screen

