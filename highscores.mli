(* Representation of highscores *)

(** A single score *)
type score = {
  name : string;
  points : int;
  level : int;
  line_cleared : int;
  standard : bool;
}

(** Type of a high score table. *)
type t

(** [read_scores ()] is the representation of the high score json file *)
val read_scores : unit -> t

(** [write_scores table] writes the high score type to a json file *)
val write_scores : t -> unit

(** [add scpre table] adds a high score to the high score table *)
val add : score -> t -> t

(** [export table] is a list of [score]s for each entry in the table *)
val scores : t -> score list

(** [make_score name points level line_cleared standard] is the [score]
    corresponding to the arguments *)
val make_score : string -> int -> int -> int -> bool -> score


