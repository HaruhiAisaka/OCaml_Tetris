open Graphics

type color = Graphics.color

type t = ((int*int)*color)


let create xy color = (xy, color)

let left block =
  let cordinate = fst block in
  create ((fst(cordinate)-1),(snd(cordinate))) (snd block)

let right block =
  let cordinate = fst block in
  create ((fst(cordinate) +1),(snd(cordinate))) (snd block)

let down block =
  let cordinate = fst block in
  create ((fst(cordinate)),(snd(cordinate)-1)) (snd block)

let up block =
  let cordinate = fst block in
  create ((fst(cordinate)),(snd(cordinate)+1)) (snd block)

let to_tuple block = fst block

let color block = snd block
