type color = {r: int; g: int; b: int}

type frame = color array array

type sprite = color option array array

type palette = (char, color option) Hashtbl.t

type draw_cmd = Text of {x: int; y: int; color: color; text: string}

val empty_frame : int -> int -> frame

val set_pixel : frame -> x:int -> y:int -> color -> frame

val fill_rect : frame -> x:int -> y:int -> w:int -> h:int -> color -> frame

val create_sprite : width:int -> height:int -> sprite

val fill_rect_sprite :
  sprite -> x:int -> y:int -> w:int -> h:int -> color -> unit

val draw_sprite : frame -> x:int -> y:int -> sprite -> frame

val draw_sprite_scaled : frame -> x:int -> y:int -> scale:int -> sprite -> frame

val sprite_of_strings : ?palette:palette -> string list -> sprite
