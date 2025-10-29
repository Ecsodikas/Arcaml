type wave = Sine | Square | Triangle | Noise

type note = {freq: float; duration: float; wave: wave; volume: int}

val play : bpm:int -> vol:int -> string -> unit
