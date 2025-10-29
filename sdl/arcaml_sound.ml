open Tsdl_mixer

type wave = Sine | Square | Triangle | Noise

type note = {freq: float; duration: float; wave: wave; volume: int}

let sample_rate = 44100

let () =
  match Mixer.open_audio 44100 Mixer.default_format 1 1024 with
  | Ok () ->
      ()
  | Error (`Msg e) ->
      failwith ("SDL_mixer init failed: " ^ e)

let freq_of_note = function
  | "C4" ->
      261.63
  | "C#4" ->
      277.18
  | "D4" ->
      293.66
  | "D#4" ->
      311.13
  | "E4" ->
      329.63
  | "F4" ->
      349.23
  | "F#4" ->
      369.99
  | "G4" ->
      392.00
  | "G#4" ->
      415.30
  | "A4" ->
      440.00
  | "A#4" ->
      466.16
  | "B4" ->
      493.88
  | "C5" ->
      523.25
  | _ ->
      440.0

let note_length = function
  | 'w' ->
      4.0
  | 'h' ->
      2.0
  | 'q' ->
      1.0
  | 'e' ->
      0.5
  | 's' ->
      0.25
  | _ ->
      1.0

let parse_token ~bpm ~vol token =
  let wave, rest =
    if String.contains token ':' then
      let w = String.sub token 0 (String.index token ':') in
      let t =
        String.sub token
          (String.index token ':' + 1)
          (String.length token - String.index token ':' - 1)
      in
      let wv =
        match w with
        | "s" ->
            Sine
        | "sq" ->
            Square
        | "t" ->
            Triangle
        | "n" ->
            Noise
        | _ ->
            Square
      in
      (wv, t)
    else (Square, token)
  in
  let len_char = rest.[String.length rest - 1] in
  let notes_part = String.sub rest 0 (String.length rest - 1) in
  let dur = note_length len_char *. (60.0 /. float bpm) in
  if notes_part = "R" then None
  else
    let note_names = String.split_on_char '+' notes_part in
    Some
      (List.map
         (fun name ->
           {freq= freq_of_note name; duration= dur; wave; volume= vol} )
         note_names )

let parse_pattern ~bpm ~vol str =
  str |> String.split_on_char ' ' |> List.filter_map (parse_token ~bpm ~vol)

let generate_samples (n : note) :
    (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t =
  let samples = int_of_float (n.duration *. float sample_rate) in
  let arr =
    Bigarray.Array1.create Bigarray.int16_signed Bigarray.c_layout samples
  in
  for i = 0 to samples - 1 do
    let t = float i /. float sample_rate in
    let phase = 2.0 *. Float.pi *. n.freq *. t in
    let sample =
      match n.wave with
      | Sine ->
          sin phase
      | Square ->
          if sin phase >= 0.0 then 1.0 else -1.0
      | Triangle ->
          2.0 /. Float.pi *. asin (sin phase)
      | Noise ->
          Random.float 2.0 -. 1.0
    in
    let s = int_of_float (sample *. 32767.0) in
    Bigarray.Array1.set arr i s
  done ;
  arr

let ptr_of_bigarray arr : Unsigned.uint8 Ctypes_static.ptr =
  let open Ctypes in
  let raw = bigarray_start array1 arr in
  from_voidp uint8_t (to_voidp raw)

let play_note_safe (n : note) =
  let samples = generate_samples n in
  let len = Unsigned.UInt32.of_int (Bigarray.Array1.dim samples * 2) in
  match Mixer.quickload_raw (ptr_of_bigarray samples) len with
  | Ok chunk ->
      (* Set volume *)
      Mixer.volume_chunk chunk n.volume |> ignore ;
      let _ = Mixer.play_channel (-1) chunk 0 |> Result.get_ok in
      let _cleanup =
        let open Thread in
        create
          (fun () ->
            (* Wait for duration + small buffer *)
            let ms = int_of_float ((n.duration *. 1000.0) +. 50.0) in
            Unix.sleepf (float ms /. 1000.0) ;
            Mixer.free_chunk chunk )
          ()
      in
      ()
  | Error (`Msg e) ->
      prerr_endline ("Failed to load sound: " ^ e)

let play ~bpm ~vol pattern =
  let chords = parse_pattern ~bpm ~vol pattern in
  let rec play_chords = function
    | [] ->
        ()
    | chord :: rest ->
        let _ = Thread.create (fun () -> List.iter play_note_safe chord) () in
        let dur = (List.hd chord).duration in
        let _ =
          Thread.create (fun () -> Unix.sleepf dur ; play_chords rest) ()
        in
        ()
  in
  play_chords chords
