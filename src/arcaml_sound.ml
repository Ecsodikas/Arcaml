type wave = Sine | Square | Triangle | Noise

type note = {freq: float; duration: float; wave: wave}

let generate_samples (n : note) sample_rate =
  let samples = int_of_float (n.duration *. float sample_rate) in
  Array.init samples (fun i ->
      let t = float i /. float sample_rate in
      let phase = 2.0 *. Float.pi *. n.freq *. t in
      match n.wave with
      | Sine ->
          sin phase
      | Square ->
          if sin phase >= 0.0 then 1.0 else -1.0
      | Triangle ->
          2.0 /. Float.pi *. asin (sin phase)
      | Noise ->
          Random.float 2.0 -. 1.0 )
