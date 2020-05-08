open Graphics

type command=
  | Left
  | Right
  | Down
  | Fall of float
  | Rotate_Right
  | Rotate_Left
  | None
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

let wait_for_key () =
  match wait_next_event[Key_pressed] with _ -> ()


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
      (* Menu *)
      | '0' -> One
      | '1' -> Two
      | '2' -> Three
      | '3' -> Four
      | '4' -> Five
      | '5' -> Six
      | '6' -> Seven
      | '7' -> Eight
      | '8' -> Nine
      | '9' -> Ten
      | _ -> None
    else None
  else Fall current

(** [read_letters str] is str with the letter the player typed appended. Removes
    some bad chars like quotes and end of files *)
let read_letters str =
  match read_key () with
  (* Remove the bad inputs (attempt to) *)
  | '"'
  | '' (* Ctrl-D *)
  | '' (* Ctrl-R *)
  | '' (* Ctrl-N *)
    -> (str, false)
  (* Backspace *)
  | '' | '' (* Backspace and Delete ? *) ->
    let str = match String.length str with
    | 0 -> str
    | n -> String.sub str 0 (n - 1)
    in (str, false)
  (* Usual inputs *)
  | ' ' -> (str, true) (* Enter : done typing *)
  | c -> (str ^ (String.make 1 c), false)
