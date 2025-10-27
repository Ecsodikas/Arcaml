open Arcaml
open Arcaml_sdl

type state = {x: int; dx: int}

let init () = {x= 0; dx= 1}

let update s _dt = {s with x= (s.x + s.dx) mod 10}

let draw s =
  let frame = empty_frame 10 5 in
  fill_rect frame ~x:s.x ~y:2 ~w:1 ~h:1 {r= 0; g= 255; b= 0}

let () =
  run
    { title= "Arcaml Demo"
    ; fps= 60
    ; width= 300
    ; height= 150
    ; init
    ; update
    ; draw
    ; scale= 30 }
