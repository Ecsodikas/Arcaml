open Arcaml

type renderer_t = {window: Tsdl.Sdl.window; renderer: Tsdl.Sdl.renderer}

type 'a game_config =
  { title: string
  ; width: int
  ; height: int
  ; init: unit -> 'a
  ; update: 'a -> float -> 'a
  ; draw: 'a -> frame
  ; fps: int
  ; scale: int }

val init : title:string -> width:int -> height:int -> renderer_t

val render_frame : frame -> scale:int -> renderer_t -> unit

val poll_event : unit -> Tsdl.Sdl.event option

val run : 'a game_config -> unit
