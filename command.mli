(** the type [command] represents the action that the game should take*)
type command=
  |Left
  |Right
  |Down
  |Pause
  |Fall of float
  |None

(**[get_command()] is the command that will be executed based on user input *)
val get_command : unit -> command
