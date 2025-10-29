open Arcaml
module Sound = Arcaml_sound

type renderer_t = {window: Tsdl.Sdl.window; renderer: Tsdl.Sdl.renderer}

type key =
  | Left
  | Right
  | Up
  | Down
  | Space
  | Escape
  | A
  | D
  | W
  | S
  | Other of int

type 'a game_config =
  { title: string
  ; width: int
  ; height: int
  ; init: unit -> 'a
  ; update: 'a -> float -> 'a
  ; draw: 'a -> frame -> frame
  ; draw_ui: 'a -> Arcaml.draw_cmd list
  ; fps: int
  ; scale: int
  ; clear_color: Arcaml.color
  ; font_path: string
  ; font_size: int }

val init : title:string -> width:int -> height:int -> renderer_t

val render_frame : frame -> scale:int -> renderer_t -> unit

val poll_event : unit -> Tsdl.Sdl.event option

val run : 'a game_config -> unit

val is_key_down : key -> bool

val is_key_pressed : key -> bool
