open Graphics

type command=
  | Left
  | Right
  | Down
  | Pause
  | Fall of float
  | None


let get_command last_drop time_between_drops =
  let current=Unix.gettimeofday() in
  if (current-.last_drop) <time_between_drops then
    if key_pressed() then 
      match read_key() with
      |'a'-> Left
      |'s'-> Down
      |'d'-> Right
      |'0'-> Pause
      |_-> None
    else None
  else Fall current
