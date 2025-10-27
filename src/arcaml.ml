(* src/arcaml.ml *)

type color = {r: int; g: int; b: int}

type frame = color array array

let empty_frame width height : frame =
  Array.make_matrix height width {r= 0; g= 0; b= 0}

let set_pixel frame ~x ~y color =
  let new_frame = Array.map Array.copy frame in
  new_frame.(y).(x) <- color ;
  new_frame

let fill_rect frame ~x ~y ~w ~h color =
  let new_frame = Array.map Array.copy frame in
  for i = y to y + h - 1 do
    for j = x to x + w - 1 do
      new_frame.(i).(j) <- color
    done
  done ;
  new_frame
