open Graphics

type command=
  | Left
  | Right
  | Down
  | Fall of float
  | Rotate_Right
  | Rotate_Left
  | None


let get_command last_drop time_between_drops =
  let current=Unix.gettimeofday() in
  if (current-.last_drop) <time_between_drops then
    if key_pressed() then 
      match read_key() with
      |'a'-> Left
      |'s'-> Down
      |'d'-> Right
      |'q'-> Rotate_Left
      |'e'-> Rotate_Right
      |_-> None
    else None
  else Fall current
