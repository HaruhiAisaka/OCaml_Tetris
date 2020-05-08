(* Implementation of the highscore table *)
open Yojson.Basic
open Yojson.Basic.Util


let highscore_file = "highscores.json"

type score = {
  name : string;
  points : int;
  level : int;
  line_cleared : int;
  standard : bool;
}

type t = score list

(* --- Json Handling *)

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
  make_table (Yojson.Basic.from_file highscore_file)


let write_scores table =
  failwith "unimplemented"

let add score table = failwith "?"

let scores table = table

let make_score name points level line_cleared standard =
{
  name = name;
  points = points;
  level = level;
  line_cleared = line_cleared;
  standard = standard;
}
