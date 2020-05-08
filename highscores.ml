(* Implementation of the highscore table *)
open Yojson.Basic
open Yojson.Basic.Util



type score = {
  name : string;
  points : int;
  level : int;
  line_cleared : int;
  standard : bool;
}

type t = score list

(* Name of highscore file *)
let highscore_file = "highscores.json"
(* Number of total names that can be stored at once *)
let num_high_scores = 10
(* prepoluated list if file not found *)
let default_scores =
  let score = {
    name = "AAA";
    points = 100;
    level  = 1;
    line_cleared = 1;
    standard = true
  }
  in
  [ score; score; score; score; score; score; score; score; score; score; ]

(** [scores_to_json t] is the converted score list to json formatted string
    Validates that it is not malformed. *)
let scores_to_json t =
  let str = string_of_int in
  let score_to_json score : string =
    {|
      {
      "name": "|} ^ score.name ^ {|",
      "points": |} ^ str score.points ^ {|,
      "level": |} ^ str score.level ^ {|,
      "line_cleared": |} ^ str score.line_cleared ^ {|,
      "standard": |} ^ string_of_bool score.standard ^ {|
      }
    |}
  in
  let rec list_body scores : string =
    match scores with
    | h :: [] -> (score_to_json h)
    | h :: t -> (score_to_json h) ^ "," ^ list_body t
    | [] -> ""
  in
  let prologue = {| { "table": [ |} in
  let epilogue = {| ]} |} in
  (prologue ^ (list_body t) ^ epilogue)
|> Yojson.Basic.from_string
|> Yojson.Basic.pretty_to_string (* Pretty print + verify valid json *)

(** Writes the high score table to [highscore_file] *)
let write_scores table =
  let str = scores_to_json table in
  let oc = open_out "highscores.json" in
  Printf.fprintf oc "%s" str;
  close_out oc

(** Is the parsed high scores from the file name specified by
    [highscore_file] *)
let read_scores () =
  let make_score json =
    let json = to_assoc json in
    {
      name = json |> List.assoc "name" |> to_string;
      points = json  |> List.assoc "points" |> to_int;
      level = json |> List.assoc "level" |> to_int;
      line_cleared = json |> List.assoc "line_cleared" |> to_int;
      standard = json |> List.assoc "standard" |> to_bool;
    }
  in
  let make_table json =
    let j_list = to_assoc json |> List.assoc "table" |> to_list in
    List.map make_score (j_list)
  in
  try
  make_table (Yojson.Basic.from_file highscore_file)
  with
  | Sys_error str | Yojson.Json_error str ->
    print_endline "Error in loading highscores:";
    print_endline str;
    print_endline "Using default score table";
    default_scores |> write_scores; default_scores


(** Adds a high score to the table, while keeping its size leq [num_high_scores]
    and sorted *)
let add score table =
  let added = score :: table in
  let sorted = List.sort (fun a b -> b.points - a.points) added in
  (* assuming its sorted greates to least *)
  let rec truncate lst i cur acc =
    if cur >= i then acc else match lst with
    | h :: t -> truncate t i (cur + 1) (h :: acc)
    | [] -> acc
  in
  truncate sorted num_high_scores 0 []
  |> List.rev

(** [scores tables] is a list of all scores stored in the table *)
let scores table =
  table

(** [is_new_high_score table score] is true if the [score] is greater than the
 * lowest score. *)
let is_new_high_score table score =
  match List.rev table with
  | h :: _ -> h.points < score
  | [] -> true

(** [make_score name points level line_cleared standard] creates a score entry
    from the arguments *)
let make_score name points level line_cleared standard =
{
  name = name;
  points = points;
  level = level;
  line_cleared = line_cleared;
  standard = standard;
}
