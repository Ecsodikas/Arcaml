open Tsdl

type renderer_t = {window: Sdl.window; renderer: Sdl.renderer}

type 'a game_config =
  { title: string
  ; width: int
  ; height: int
  ; init: unit -> 'a
  ; update: 'a -> float -> 'a
  ; draw: 'a -> Arcaml.frame
  ; fps: int
  ; scale: int }

let init ~title ~width ~height =
  match Sdl.init Sdl.Init.video with
  | Ok () ->
      let window =
        match
          Sdl.create_window ~w:width ~h:height title Sdl.Window.windowed
        with
        | Ok w ->
            w
        | Error (`Msg e) ->
            failwith e
      in
      let renderer =
        match
          Sdl.create_renderer window ~index:(-1)
            ~flags:Sdl.Renderer.(accelerated)
        with
        | Ok r ->
            r
        | Error (`Msg e) ->
            failwith e
      in
      {window; renderer}
  | Error (`Msg e) ->
      failwith e

let render_frame (frame : Arcaml.frame) ~scale r =
  let w = Array.length frame.(0) in
  let h = Array.length frame in
  Sdl.set_render_draw_color r.renderer 0 0 0 255 |> Result.get_ok ;
  Sdl.render_clear r.renderer |> Result.get_ok ;
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let c = frame.(y).(x) in
      Sdl.set_render_draw_color r.renderer c.r c.g c.b 255 |> Result.get_ok ;
      Sdl.render_fill_rect r.renderer
        (Some (Sdl.Rect.create ~x:(x * scale) ~y:(y * scale) ~w:scale ~h:scale))
      |> ignore
    done
  done ;
  Sdl.render_present r.renderer

let poll_event () =
  let e = Sdl.Event.create () in
  if Sdl.poll_event (Some e) then Some e else None

let run (config : 'a game_config) =
  let sdl =
    init ~title:config.title ~width:config.width ~height:config.height
  in
  let state = ref (config.init ()) in
  let quit = ref false in
  let last_time = ref (Sdl.get_ticks ()) in
  while not !quit do
    (* --- handle events --- *)
    let event = Sdl.Event.create () in
    while Sdl.poll_event (Some event) do
      match Sdl.Event.(enum (get event typ)) with
      | `Quit ->
          quit := true
      | _ ->
          ()
    done ;
    (* --- update --- *)
    let now = Sdl.get_ticks () in
    let dt =
      float_of_int (Int32.to_int now - Int32.to_int !last_time) /. 1000.0
    in
    last_time := now ;
    state := config.update !state dt ;
    (* --- draw --- *)
    let frame = config.draw !state in
    render_frame frame ~scale:config.scale sdl ;
    Sdl.delay (Int32.of_float (1000.0 /. Float.of_int config.fps))
  done ;
  Sdl.quit ()
