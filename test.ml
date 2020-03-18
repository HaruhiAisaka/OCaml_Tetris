open OUnit2
(* Open the modules we want to test here
open ???
*)

(* Helper Functions --------------------------------------------------------- *)

(*
 let helper_func = ...
*)

(* Testing Functions -------------------------------------------------------- *)

(*
 let test_func =
   (name: string)
   (func: 'a)
   (expected_output: 'b) : test =
  name >:: (fun _ ->
   assert_equal expected_output (func ()) ~printer: pp_string)
*)

(* Test Lists --------------------------------------------------------------- *)
let tests =
  [
  ]

let suite =
  "test suite for A2"  >::: List.flatten [
    tests;
  ]
