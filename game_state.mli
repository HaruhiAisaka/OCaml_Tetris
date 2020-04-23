(**
 * The game state represents the abstract state of the entire game.
 *)

(**  The type of the Game State *)
type t


(** [process deltatime control state] is the updated state of the game based on
 * [deltatime] passed and [command] pressed. *)
val process : float -> _ -> t -> t
