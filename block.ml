type t = (int*int)

let create xy = xy

let left block = create ((fst(block)-1),(snd(block)))

let right block = create ((fst(block)+1),(snd(block)))

let down block = create ((fst(block)),(snd(block)-1))

let to_tuple block = block
