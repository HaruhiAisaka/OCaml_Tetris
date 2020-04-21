open Graphics

type command=
  | Left
  | Right
  | Down
  | Pause
  | Fall of float
  | None


let get_command unit =
  let status=wait_next_event [Key_pressed] in
  match status.key with
  |'a'-> Left
  |'s'-> Down
  |'d'-> Right
  |'0'-> Pause
  |_-> None

