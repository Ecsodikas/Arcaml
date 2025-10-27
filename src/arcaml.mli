type color = {r: int; g: int; b: int}

type frame = color array array

val empty_frame : int -> int -> frame

val set_pixel : frame -> x:int -> y:int -> color -> frame

val fill_rect : frame -> x:int -> y:int -> w:int -> h:int -> color -> frame
