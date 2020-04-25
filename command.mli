(** the type [command] represents the action that the game should take*)
type command=
  |Left
  |Right
  |Down
  |Pause
  |Fall of float
  |Rotate_Right
  |Rotate_Left
  |None

(**[get_command last_drop time_between_drops] is the command that will be 
   executed based on user input. [last_drop] is the unix time that the last 
   forced drop occured. [time_between_drops] is the number of seconds between
   each forced drop.*)
val get_command : float -> float -> command
