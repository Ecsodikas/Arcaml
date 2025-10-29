(* src/arcaml.ml *)

type color = {r: int; g: int; b: int}

type frame = color array array

type draw_cmd = Text of {x: int; y: int; color: color; text: string}

let empty_frame width height : frame =
  Array.make_matrix height width {r= 0; g= 0; b= 0}

let width (frame : frame) = Array.length frame.(0)

let height (frame : frame) = Array.length frame

let set_pixel frame ~x ~y color =
  if x < 0 || y < 0 || y >= height frame || x >= width frame then frame
  else
    let new_frame = Array.map Array.copy frame in
    new_frame.(y).(x) <- color ;
    new_frame

let fill_rect frame ~x ~y ~w ~h color =
  let new_frame = Array.map Array.copy frame in
  let fw = width frame in
  let fh = height frame in
  for i = max 0 y to min (y + h - 1) (fh - 1) do
    for j = max 0 x to min (x + w - 1) (fw - 1) do
      new_frame.(i).(j) <- color
    done
  done ;
  new_frame
