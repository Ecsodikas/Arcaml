type color = {r: int; g: int; b: int}

type sprite = color option array array

type frame = color array array

type draw_cmd = Text of {x: int; y: int; color: color; text: string}

type palette = (char, color option) Hashtbl.t

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

let create_sprite ~width ~height : sprite = Array.make_matrix height width None

let fill_rect_sprite sprite ~x ~y ~w ~h color =
  let hmax = Array.length sprite in
  let wmax = Array.length sprite.(0) in
  for j = y to y + h - 1 do
    for i = x to x + w - 1 do
      if j >= 0 && j < hmax && i >= 0 && i < wmax then
        sprite.(j).(i) <- Some color
    done
  done

let draw_sprite frame ~x ~y sprite =
  let fh = Array.length frame in
  let fw = Array.length frame.(0) in
  let sh = Array.length sprite in
  let sw = Array.length sprite.(0) in
  for j = 0 to sh - 1 do
    for i = 0 to sw - 1 do
      let fx = x + i in
      let fy = y + j in
      if fx >= 0 && fx < fw && fy >= 0 && fy < fh then
        match sprite.(j).(i) with Some c -> frame.(fy).(fx) <- c | None -> ()
    done
  done ;
  frame

let draw_sprite_scaled frame ~x ~y ~scale sprite =
  let fh = Array.length frame in
  let fw = Array.length frame.(0) in
  let sh = Array.length sprite in
  let sw = Array.length sprite.(0) in
  for j = 0 to sh - 1 do
    for i = 0 to sw - 1 do
      let fx = x + (i * scale) in
      let fy = y + (j * scale) in
      match sprite.(j).(i) with
      | Some c ->
          for dy = 0 to scale - 1 do
            for dx = 0 to scale - 1 do
              let px = fx + dx in
              let py = fy + dy in
              if px >= 0 && px < fw && py >= 0 && py < fh then
                frame.(py).(px) <- c
            done
          done
      | None ->
          ()
    done
  done ;
  frame

let default_palette =
  let tbl = Hashtbl.create 8 in
  Hashtbl.add tbl '0' None ;
  Hashtbl.add tbl '1' (Some {r= 0; g= 0; b= 0}) ;
  Hashtbl.add tbl '2' (Some {r= 255; g= 0; b= 0}) ;
  Hashtbl.add tbl '3' (Some {r= 0; g= 255; b= 0}) ;
  Hashtbl.add tbl '4' (Some {r= 0; g= 0; b= 255}) ;
  tbl

let sprite_of_strings ?(palette = default_palette) rows : sprite =
  let height = List.length rows in
  let width = match rows with [] -> 0 | r :: _ -> String.length r in
  let arr = Array.make_matrix height width None in
  List.iteri
    (fun y row ->
      String.iteri
        (fun x ch ->
          if Hashtbl.mem palette ch then arr.(y).(x) <- Hashtbl.find palette ch
          else arr.(y).(x) <- None )
        row )
    rows ;
  arr
