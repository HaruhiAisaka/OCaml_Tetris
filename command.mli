(** Command provides functions that handle user input through the keyboard *)
(** the type [command] represents the action that the game should take*)
type command=
  |Left
  |Right
  |Down
  |Fall of float
  |Rotate_Right
  |Rotate_Left
  |None
  (* Menu *)
  |One
  |Two
  |Three
  |Four
  |Five
  |Six
  |Seven
  |Eight
  |Nine
  |Ten

(**[get_command last_drop time_between_drops] is the command that will be
   executed based on user input. [last_drop] is the unix time that the last
   forced drop occured. [time_between_drops] is the number of seconds between
   each forced drop.*)
val get_command : float -> float -> command

val wait_for_key: unit -> unit

(** [read_key str] is a tuple with the typed key appended to [str] and a boolean
    represneting whether the user is done typing or not. *)
val read_letters : string -> string * bool
