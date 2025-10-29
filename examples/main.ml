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

let smiley_sprite =
  sprite_of_strings
    ["0033300"; "0333330"; "3343433"; "0333330"; "0322230"; "0033300"; "0003000"]

let draw s frame = draw_sprite frame ~x:s.x ~y:s.y smiley_sprite

let draw_ui s =
  [ Text
      { x= 75
      ; y= 33
      ; color= {r= 255; g= 255; b= 255}
      ; text= Printf.sprintf "FPS: %.1f" s.fps } ]

let random_notes =
  "E5q B4q C5q D5q C5q B4q A4q A4q C5q E5q D5q C5q B4q B4q C5q D5q E5q C5q A4q \
   A4q D5q F5q E5q C5q D5q B4q"

let () =
  Sound.play ~bpm:120 ~vol:1 random_notes ;
  run
    { title= "Arcaml Demo"
    ; width= 800
    ; height= 800
    ; fps= 60
    ; scale= 10
    ; clear_color= {r= 0; g= 0; b= 0}
    ; font_path= "./examples/assets/bbtnf.ttf"
    ; font_size= 18
    ; init
    ; update
    ; draw
    ; draw_ui }
