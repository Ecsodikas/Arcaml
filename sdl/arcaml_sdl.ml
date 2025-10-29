open Tsdl
open Tsdl_ttf

type renderer_t = {window: Sdl.window; renderer: Sdl.renderer}

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
  ; draw: 'a -> Arcaml.frame -> Arcaml.frame
  ; draw_ui: 'a -> Arcaml.draw_cmd list
  ; fps: int
  ; scale: int
  ; clear_color: Arcaml.color
  ; font_path: string
  ; font_size: int }

let key_of_sdl scancode =
  match Sdl.Scancode.enum scancode with
  | `Left ->
      Some Left
  | `Right ->
      Some Right
  | `Up ->
      Some Up
  | `Down ->
      Some Down
  | `Space ->
      Some Space
  | `Escape ->
      Some Escape
  | `A ->
      Some A
  | `D ->
      Some D
  | `W ->
      Some W
  | `S ->
      Some S
  | _ ->
      None

let keys_down : (key, bool) Hashtbl.t = Hashtbl.create 32

let keys_pressed : (key, bool) Hashtbl.t = Hashtbl.create 32

let init ~title ~width ~height =
  (match Ttf.init () with Ok () -> () | Error (`Msg e) -> failwith e) ;
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

let render_draw_cmd renderer font = function
  | Arcaml.Text {x; y; color; text} ->
      let open Ttf in
      let surface =
        match
          render_text_solid font text
            (Sdl.Color.create ~r:color.r ~g:color.g ~b:color.b ~a:255)
        with
        | Ok r ->
            r
        | Error (`Msg e) ->
            failwith e
      in
      let texture =
        match Sdl.create_texture_from_surface renderer surface with
        | Ok r ->
            r
        | Error (`Msg e) ->
            failwith e
      in
      let w, h =
        match Sdl.query_texture texture with
        | Ok (_, _, (w, h)) ->
            (w, h)
        | Error (`Msg e) ->
            failwith e
      in
      let dst = Sdl.Rect.create ~x ~y ~w ~h in
      ( match Sdl.render_copy renderer ~src:dst ~dst texture with
      | Ok _ ->
          ()
      | Error (`Msg e) ->
          failwith e ) ;
      Sdl.destroy_texture texture ;
      Sdl.free_surface surface

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
  let font =
    match Ttf.open_font config.font_path config.font_size with
    | Ok e ->
        e
    | Error (`Msg e) ->
        failwith e
  in
  let state = ref (config.init ()) in
  let quit = ref false in
  let last_time = ref (Sdl.get_ticks ()) in
  let logical_w = config.width / config.scale in
  let logical_h = config.height / config.scale in
  let frame = ref (Arcaml.empty_frame logical_w logical_h) in
  while not !quit do
    (* --- handle events --- *)
    let event = Sdl.Event.create () in
    while Sdl.poll_event (Some event) do
      match Sdl.Event.(enum (get event typ)) with
      | `Quit ->
          quit := true
      | `Key_down -> (
          let sc = Sdl.Event.(get event keyboard_scancode) in
          match key_of_sdl sc with
          | Some k ->
              Hashtbl.replace keys_down k true ;
              Hashtbl.replace keys_pressed k true
          | None ->
              () )
      | `Key_up -> (
          let sc = Sdl.Event.(get event keyboard_scancode) in
          match key_of_sdl sc with
          | Some k ->
              Hashtbl.remove keys_down k
          | None ->
              () )
      | _ ->
          ()
    done ;
    (* --- update --- *)
    let now = Sdl.get_ticks () in
    let dt =
      float_of_int (Int32.to_int now - Int32.to_int !last_time) /. 1000.0
    in
    let sc_w = config.width / config.scale in
    let sc_h = config.height / config.scale in
    last_time := now ;
    state := config.update !state dt ;
    (* --- draw --- *)
    frame :=
      Arcaml.fill_rect
        (Arcaml.empty_frame sc_w sc_h)
        ~x:0 ~y:0 ~w:sc_w ~h:sc_h config.clear_color ;
    frame := config.draw !state !frame ;
    render_frame !frame ~scale:config.scale sdl ;
    let cmds = config.draw_ui !state in
    List.iter (render_draw_cmd sdl.renderer font) cmds ;
    Sdl.render_present sdl.renderer ;
    Hashtbl.reset keys_pressed ;
    Sdl.delay (Int32.of_float (1000.0 /. Float.of_int config.fps))
  done ;
  Ttf.quit () ;
  Sdl.quit ()

let is_key_down k = Hashtbl.mem keys_down k

let is_key_pressed k = Hashtbl.mem keys_pressed k
