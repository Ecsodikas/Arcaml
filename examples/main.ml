open Arcaml
open Arcaml_sdl

type state = {x: int; y: int; fps: float}

let init () = {x= 0; y= 0; fps= 0.0}

let update s dt =
  let x =
    if is_key_down Left then s.x - 1
    else if is_key_down Right then s.x + 1
    else s.x
  in
  let y =
    if is_key_down Up then s.y - 1
    else if is_key_down Down then s.y + 1
    else s.y
  in
  {x; y; fps= 1.0 /. dt}

let draw s frame = fill_rect frame ~x:s.x ~y:s.y ~w:1 ~h:1 {r= 0; g= 255; b= 0}

let draw_ui s =
  [ Text
      { x= 0
      ; y= 0
      ; color= {r= 255; g= 255; b= 255}
      ; text= Printf.sprintf "FPS: %.1f" s.fps } ]

let () =
  run
    { title= "Arcaml Demo"
    ; width= 800
    ; height= 800
    ; fps= 60
    ; scale= 10
    ; clear_color= {r= 0; g= 0; b= 0}
    ; font_path= "/home/ecsodikas/Repositories/arcaml/examples/assets/bbtnf.ttf"
    ; font_size= 18
    ; init
    ; update
    ; draw
    ; draw_ui }
