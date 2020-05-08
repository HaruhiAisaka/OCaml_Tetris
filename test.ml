open OUnit2 
open Block
open Piece

(* Block Function Tests*)
let test_block_create
    (name: string)
    (xy: (int*int))
    (expected) : test =
  name >:: (fun _ ->
      assert_equal (expected) (Block.create xy Graphics.red |>  Block.to_tuple)
    )

let test_block_left
    (name: string)
    (xy: (int*int))
    (expected) : test =
  name >:: (fun _ ->
      assert_equal (expected) (Block.create xy Graphics.red |> Block.left |> Block.to_tuple)
    )

let test_block_right
    (name: string)
    (xy: (int*int))
    (expected) : test =
  name >:: (fun _ ->
      assert_equal (expected) (Block.create xy Graphics.red |> Block.right |> Block.to_tuple)
    )

let test_block_down
    (name: string)
    (xy: (int*int))
    (expected) : test =
  name >:: (fun _ ->
      assert_equal (expected) (Block.create xy Graphics.red |> Block.down |> Block.to_tuple)
    )

let test_piece_create
    (name: string)
    (xy: (int*int))
    (piece : Piece.piece_name)
    (expected) : test =
  name >:: (fun _ ->
      assert_equal (expected)
        (Piece.create xy piece |> Piece.to_blocks |> List.map (Block.to_tuple))
    )

let test_piece_left
    (name: string)
    (piece : Piece.t)
    (expected) : test =
  name >:: (fun _ ->
      assert_equal (expected)
        (Piece.left piece |> Piece.to_blocks |> List.map (Block.to_tuple)))

let test_piece_right
    (name: string)
    (piece : Piece.t)
    (expected) : test =
  name >:: (fun _ ->
      assert_equal (expected)
        (Piece.right piece |> Piece.to_blocks |> List.map (Block.to_tuple)))

let test_piece_down
    (name: string)
    (piece : Piece.t)
    (expected) : test =
  name >:: (fun _ ->
      assert_equal (expected)
        (Piece.down piece |> Piece.to_blocks |> List.map (Block.to_tuple)))

let test_piece_rotate_right
    (name: string)
    (piece : Piece.t)
    (expected) : test =
  name >:: (fun _ ->
      assert_equal (expected)
        (Piece.rotate_right piece |> Piece.to_blocks |> List.map (Block.to_tuple)))

let test_piece_rotate_left
    (name: string)
    (piece : Piece.t)
    (expected) : test =
  name >:: (fun _ ->
      assert_equal (expected)
        (Piece.rotate_left piece |> Piece.to_blocks |> List.map (Block.to_tuple)))

(* Test Lists --------------------------------------------------------------- *)
let block_tests =
  [
    test_block_create "create block" (0,0) (0,0);
    test_block_create "create block" (-3,-3) (-3,-3);
    test_block_left "Block.left" (0,0) (-1,0);
    test_block_right "Block.right" (0,0) (1,0);
    test_block_down "Block.down" (0,0) (0,-1);
  ]

let piece_tests_I =
  [
    test_piece_create "create I piece" (0,0) (I(0,0))
      ([(0,0);(-1,0);(-2,0);(1,0)]);
    test_piece_left "move I to left" (Piece.create (0,0) (I(0,0)))
      ([(-1,0);(-2,0);(-3,0);(0,0)]);
    test_piece_right "move I to right" (Piece.create (0,0) (I(0,0)))
      ([(1,0);(0,0);(-1,0);(2,0)]);
    test_piece_down "move I to down" (Piece.create (0,0) (I(0,0)))
      ([(0,-1);(-1,-1);(-2,-1);(1,-1)]);

    test_piece_rotate_right "rotate I 1 right" (Piece.create (0,0) (I(0,0)))
      ([(0,-1);(0,0);(0,1);(0,-2)]);
    test_piece_rotate_right "rotate I 2 right"
      (Piece.create (0,0) (I(0,0))|> Piece.rotate_right)
      ([(-1,-1);(0,-1);(1,-1);(-2,-1)]);
    test_piece_rotate_right "rotate I 3 right"
      (Piece.create (0,0) (I(0,0))|> Piece.rotate_right|> Piece.rotate_right)
      ([(-1,0);(-1,-1);(-1,-2);(-1,1)]);
    test_piece_rotate_right "rotate I 4 right"
      (Piece.create (0,0) (I(0,0))
       |> Piece.rotate_right|> Piece.rotate_right|> Piece.rotate_right)
      ([(0,0);(-1,0);(-2,0);(1,0)]);

    test_piece_rotate_left "rotate I 1 left" (Piece.create (0,0) (I(0,0)))
      ([(-1,0);(-1,-1);(-1,-2);(-1,1)]);
    test_piece_rotate_left "rotate I 2 left"
      (Piece.create (0,0) (I(0,0))|> Piece.rotate_left)
      ([(-1,-1);(0,-1);(1,-1);(-2,-1)]);
    test_piece_rotate_left "rotate I 3 left"
      (Piece.create (0,0) (I(0,0))|> Piece.rotate_left|> Piece.rotate_left)
      ([(0,-1);(0,0);(0,1);(0,-2)]);
    test_piece_rotate_left "rotate I 4 left"
      (Piece.create (0,0) (I(0,0))
       |> Piece.rotate_left|> Piece.rotate_left|> Piece.rotate_left)
      ([(0,0);(-1,0);(-2,0);(1,0)]);
  ]

let piece_tests_O =
  [
    test_piece_create "create O piece" (0,0) (O)
      ([(0,0);(-1,0);(0,-1);(-1,-1)]);
    test_piece_left "move O to left" (Piece.create (0,0) (O))
      ([(-1,0);(-2,0);(-1,-1);(-2,-1)]);
    test_piece_right "move O to right" (Piece.create (0,0) (O))
      ([(1,0);(0,0);(1,-1);(0,-1)]);
    test_piece_down "move O to down" (Piece.create (0,0) (O))
      ([(0,-1);(-1,-1);(0,-2);(-1,-2)]);
    test_piece_rotate_right "rotate O 1 right" (Piece.create (0,0) (O))
      ([(0,0);(-1,0);(0,-1);(-1,-1)]);
    test_piece_rotate_right "rotate O 1 left" (Piece.create (0,0) (O))
      ([(0,0);(-1,0);(0,-1);(-1,-1)]);
  ]

let piece_tests_L =
  [
    test_piece_create "create L piece" (0,0) (L)
      ([(0,0);(1,0);(-1,-1);(-1,0)]);
    test_piece_left "move L to left" (Piece.create (0,0) (L))
      ([(-1,0);(0,0);(-2,-1);(-2,0)]);
    test_piece_right "move L to right" (Piece.create (0,0) (L))
      ([(1,0);(2,0);(0,-1);(0,0)]);
    test_piece_down "move L to down" (Piece.create (0,0) (L))
      ([(0,-1);(1,-1);(-1,-2);(-1,-1)]);

    test_piece_rotate_right "rotate L 1 right" (Piece.create (0,0) (L))
      ([(0,0);(0,-1);(-1,1);(0,1)]);
    test_piece_rotate_right "rotate L 2 right"
      (Piece.create (0,0) (L)|> Piece.rotate_right)
      ([(0,0);(-1,0);(1,1);(1,0)]);
    test_piece_rotate_right "rotate L 3 right"
      (Piece.create (0,0) (L)|> Piece.rotate_right|> Piece.rotate_right)
      ([(0,0);(0,1);(1,-1);(0,-1)]);
    test_piece_rotate_right "rotate L 4 right"
      (Piece.create (0,0) (L)
       |> Piece.rotate_right|> Piece.rotate_right|> Piece.rotate_right)
      ([(0,0);(1,0);(-1,-1);(-1,0)]);

    test_piece_rotate_left "rotate L 1 left" (Piece.create (0,0) (L))
      ([(0,0);(0,1);(1,-1);(0,-1)]);
    test_piece_rotate_left "rotate L 2 left"
      (Piece.create (0,0) (L)|> Piece.rotate_left)
      ([(0,0);(-1,0);(1,1);(1,0)]);
    test_piece_rotate_left "rotate L 3 left"
      (Piece.create (0,0) (L)|> Piece.rotate_left|> Piece.rotate_left)
      ([(0,0);(0,-1);(-1,1);(0,1)]);
    test_piece_rotate_left "rotate L 4 left"
      (Piece.create (0,0) (L)
       |> Piece.rotate_left|> Piece.rotate_left|> Piece.rotate_left)
      ([(0,0);(1,0);(-1,-1);(-1,0)]);
  ]

let piece_tests_J =
  [
    test_piece_create "create J piece" (0,0) (J)
      ([(0,0);(-1,0);(1,-1);(1,0)]);
    test_piece_left "move J to left" (Piece.create (0,0) (J))
      ([(-1,0);(-2,0);(0,-1);(0,0)]);
    test_piece_right "move J to right" (Piece.create (0,0) (J))
      ([(1,0);(0,0);(2,-1);(2,0)]);
    test_piece_down "move J to down" (Piece.create (0,0) (J))
      ([(0,-1);(-1,-1);(1,-2);(1,-1)]);

    test_piece_rotate_right "rotate J 1 right" (Piece.create (0,0) (J))
      ([(0,0);(0,1);(1,1);(0,-1)]);
    test_piece_rotate_right "rotate J 2 right"
      (Piece.create (0,0) (J)|> Piece.rotate_right)
      ([(0,0);(1,0);(1,-1);(-1,0)]);
    test_piece_rotate_right "rotate J 3 right"
      (Piece.create (0,0) (J)|> Piece.rotate_right|> Piece.rotate_right)
      ([(0,0);(0,-1);(-1,-1);(0,1)]);
    test_piece_rotate_right "rotate J 4 right"
      (Piece.create (0,0) (J)
       |> Piece.rotate_right|> Piece.rotate_right|> Piece.rotate_right)
      ([(0,0);(-1,0);(-1,1);(1,0)]);

    test_piece_rotate_left "rotate J 1 left" (Piece.create (0,0) (J))
      ([(0,0);(0,-1);(-1,-1);(0,1)]);
    test_piece_rotate_left "rotate J 2 left"
      (Piece.create (0,0) (J)|> Piece.rotate_left)
      ([(0,0);(1,0);(1,-1);(-1,0)]);
    test_piece_rotate_left "rotate J 3 left"
      (Piece.create (0,0) (J)|> Piece.rotate_left|> Piece.rotate_left)
      ([(0,0);(0,1);(1,1);(0,-1)]);
    test_piece_rotate_left "rotate J 4 left"
      (Piece.create (0,0) (J)
       |> Piece.rotate_left|> Piece.rotate_left|> Piece.rotate_left)
      ([(0,0);(-1,0);(-1,1);(1,0)]);
  ]

let piece_tests_S =
  [
    test_piece_create "create S piece" (0,0) (S)
      ([(0,0);(0,-1);(1,0);(-1,-1)]);
    test_piece_left "move S to left" (Piece.create (0,0) (S))
      ([(-1,0);(-1,-1);(0,0);(-2,-1)]);
    test_piece_right "move S to right" (Piece.create (0,0) (S))
      ([(1,0);(1,-1);(2,0);(0,-1)]);
    test_piece_down "move S to down" (Piece.create (0,0) (S))
      ([(0,-1);(0,-2);(1,-1);(-1,-2)]);

    test_piece_rotate_right "rotate S 1 right" (Piece.create (0,0) (S))
      ([(0,0);(1,0);(1,-1);(0,1)]);
    test_piece_rotate_right "rotate S 2 right"
      (Piece.create (0,0) (S)|> Piece.rotate_right)
      ([(0,0);(0,-1);(-1,-1);(1,0)]);
    test_piece_rotate_right "rotate S 3 right"
      (Piece.create (0,0) (S)|> Piece.rotate_right|> Piece.rotate_right)
      ([(0,0);(-1,0);(-1,1);(0,-1)]);
    test_piece_rotate_right "rotate S 4 right"
      (Piece.create (0,0) (S)
       |> Piece.rotate_right|> Piece.rotate_right|> Piece.rotate_right)
      ([(0,0);(0,1);(1,1);(-1,0)]);

    test_piece_rotate_left "rotate S 1 left" (Piece.create (0,0) (S))
      ([(0,0);(-1,0);(-1,1);(0,-1)]);
    test_piece_rotate_left "rotate S 2 left"
      (Piece.create (0,0) (S)|> Piece.rotate_left)
      ([(0,0);(0,-1);(-1,-1);(1,0)]);
    test_piece_rotate_left "rotate S 3 left"
      (Piece.create (0,0) (S)|> Piece.rotate_left|> Piece.rotate_left)
      ([(0,0);(1,0);(1,-1);(0,1)]);
    test_piece_rotate_left "rotate S 4 left"
      (Piece.create (0,0) (S)
       |> Piece.rotate_left|> Piece.rotate_left|> Piece.rotate_left)
      ([(0,0);(0,1);(1,1);(-1,0)]);
  ]

let piece_tests_Z =
  [
    test_piece_create "create Z piece" (0,0) (Z)
      ([(0,0);(-1,0);(0,-1);(1,-1)]);
    test_piece_left "move Z to left" (Piece.create (0,0) (Z))
      ([(-1,0);(-2,0);(-1,-1);(0,-1)]);
    test_piece_right "move Z to right" (Piece.create (0,0) (Z))
      ([(1,0);(0,0);(1,-1);(2,-1)]);
    test_piece_down "move Z to down" (Piece.create (0,0) (Z))
      ([(0,-1);(-1,-1);(0,-2);(1,-2)]);

    test_piece_rotate_right "rotate Z 1 right" (Piece.create (0,0) (Z))
      ([(0,0);(1,0);(1,1);(0,-1)]);
    test_piece_rotate_right "rotate Z 2 right"
      (Piece.create (0,0) (Z)|> Piece.rotate_right)
      ([(0,0);(0,-1);(1,-1);(-1,0)]);
    test_piece_rotate_right "rotate Z 3 right"
      (Piece.create (0,0) (Z)|> Piece.rotate_right|> Piece.rotate_right)
      ([(0,0);(-1,0);(-1,-1);(0,1)]);
    test_piece_rotate_right "rotate Z 4 right"
      (Piece.create (0,0) (Z)
       |> Piece.rotate_right|> Piece.rotate_right|> Piece.rotate_right)
      ([(0,0);(0,1);(-1,1);(1,0)]);

    test_piece_rotate_left "rotate Z 1 left" (Piece.create (0,0) (Z))
      ([(0,0);(-1,0);(-1,-1);(0,1)]);
    test_piece_rotate_left "rotate Z 2 left"
      (Piece.create (0,0) (Z)|> Piece.rotate_left)
      ([(0,0);(0,-1);(1,-1);(-1,0)]);
    test_piece_rotate_left "rotate Z 3 left"
      (Piece.create (0,0) (Z)|> Piece.rotate_left|> Piece.rotate_left)
      ([(0,0);(1,0);(1,1);(0,-1)]);
    test_piece_rotate_left "rotate Z 4 left"
      (Piece.create (0,0) (Z)
       |> Piece.rotate_left|> Piece.rotate_left|> Piece.rotate_left)
      ([(0,0);(0,1);(-1,1);(1,0)]);
  ]

let piece_tests_T =
  [
    test_piece_create "create T piece" (0,0) (T)
      ([(0,0);(1,0);(-1,0);(0,-1)]);
    test_piece_left "move T to left" (Piece.create (0,0) (T))
      ([(-1,0);(0,0);(-2,0);(-1,-1)]);
    test_piece_right "move T to right" (Piece.create (0,0) (T))
      ([(1,0);(2,0);(0,0);(1,-1)]);
    test_piece_down "move T to down" (Piece.create (0,0) (T))
      ([(0,-1);(1,-1);(-1,-1);(0,-2)]);

    test_piece_rotate_right "rotate T 1 right" (Piece.create (0,0) (T))
      ([(0,0);(0,-1);(0,1);(1,0)]);
    test_piece_rotate_right "rotate T 2 right"
      (Piece.create (0,0) (T)|> Piece.rotate_right)
      ([(0,0);(-1,0);(1,0);(0,-1)]);
    test_piece_rotate_right "rotate T 3 right"
      (Piece.create (0,0) (T)|> Piece.rotate_right|> Piece.rotate_right)
      ([(0,0);(0,1);(0,-1);(-1,0)]);
    test_piece_rotate_right "rotate T 4 right"
      (Piece.create (0,0) (T)
       |> Piece.rotate_right|> Piece.rotate_right|> Piece.rotate_right)
      ([(0,0);(1,0);(-1,0);(0,1)]);

    test_piece_rotate_left "rotate T 1 left" (Piece.create (0,0) (T))
      ([(0,0);(0,1);(0,-1);(-1,0)]);
    test_piece_rotate_left "rotate T 2 left"
      (Piece.create (0,0) (T)|> Piece.rotate_left)
      ([(0,0);(-1,0);(1,0);(0,-1)]);
    test_piece_rotate_left "rotate T 3 left"
      (Piece.create (0,0) (T)|> Piece.rotate_left|> Piece.rotate_left)
      ([(0,0);(0,-1);(0,1);(1,0)]);
    test_piece_rotate_left "rotate T 4 left"
      (Piece.create (0,0) (T)
       |> Piece.rotate_left|> Piece.rotate_left|> Piece.rotate_left)
      ([(0,0);(1,0);(-1,0);(0,1)]);
  ]

let suite =
  "test suite for A2"  >::: List.flatten [
    block_tests;
    piece_tests_I;
    piece_tests_O;
    piece_tests_L;
    piece_tests_J;
    piece_tests_S;
    piece_tests_Z;
    piece_tests_T;

  ]

let _ = run_test_tt_main suite
