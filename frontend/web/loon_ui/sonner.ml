open! Import
open Js_of_ocaml

module Type = struct
  type t =
    | Default
    | Success
    | Info
    | Warning
    | Error
    | Loading
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Default -> None
    | Success -> Some "success"
    | Info -> Some "info"
    | Warning -> Some "warning"
    | Error -> Some "error"
    | Loading -> Some "loading"
  ;;

  (* shadcn's icons. *)
  let icon = function
    | Default | Loading -> None
    | Success -> Some (Icon.view ~class_:"size-4" Circle_check)
    | Info -> Some (Icon.view ~class_:"size-4" Info)
    | Warning -> Some (Icon.view ~class_:"size-4" Triangle_alert)
    | Error -> Some (Icon.view ~class_:"size-4" Octagon_x)
  ;;
end

module Position = struct
  type t =
    | Top_left
    | Top_center
    | Top_right
    | Bottom_left
    | Bottom_center
    | Bottom_right
  [@@deriving enumerate, sexp_of, compare, equal]

  let y = function
    | Top_left | Top_center | Top_right -> "top"
    | Bottom_left | Bottom_center | Bottom_right -> "bottom"
  ;;

  let x = function
    | Top_left | Bottom_left -> "left"
    | Top_center | Bottom_center -> "center"
    | Top_right | Bottom_right -> "right"
  ;;

  let name t = y t ^ "-" ^ x t
end

module Theme = struct
  type t =
    | Light
    | Dark
    | System
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Light -> "light"
    | Dark -> "dark"
    | System -> "system"
  ;;
end

let infinity = Time_ns.Span.max_value_for_1us_rounding
let toast_lifetime = Time_ns.Span.of_int_ms 4000
let time_before_unmount = Time_ns.Span.of_int_ms 200
let toast_width = 356

module Toast = struct
  type t =
    { id : string
    ; title : string
    ; description : string option
    ; type_ : Type.t
    ; action : (string * unit Effect.t) option
    ; cancel : (string * unit Effect.t) option
    ; duration : Time_ns.Span.t option
    ; dismissible : bool
    ; close_button : bool option
    ; rich_colors : bool option
    ; class_ : string option
    ; promise : bool
    }
end

module Command = struct
  type t =
    | Create of Toast.t
    | Dismiss of string option
end

(* Sonner's ToastState: the toasters in the page (the last put in first) and
   the commands sent before there was one. *)
let toasters : (string * (Command.t -> unit Effect.t)) list ref = ref []
let pending : Command.t Queue.t = Queue.create ()
let counter = ref 0

let send command =
  let%bind.Effect target =
    Effect.of_thunk (fun () ->
      match !toasters with
      | (_, f) :: _ -> Some f
      | [] ->
        Queue.enqueue pending command;
        None)
  in
  match target with
  | Some f -> f command
  | None -> Effect.Ignore
;;

let fresh_id id =
  Effect.of_thunk (fun () ->
    match id with
    | Some id -> id
    | None ->
      incr counter;
      Int.to_string !counter)
;;

let create
      ~type_
      ?(promise = false)
      ?id
      ?description
      ?action
      ?cancel
      ?duration
      ?(dismissible = true)
      ?close_button
      ?rich_colors
      ?class_
      title
  =
  let%bind.Effect id = fresh_id id in
  send
    (Create
       { id
       ; title
       ; description
       ; type_
       ; action
       ; cancel
       ; duration
       ; dismissible
       ; close_button
       ; rich_colors
       ; class_
       ; promise
       })
;;

let toast = create ~type_:Default ~promise:false
let success = create ~type_:Success ~promise:false
let info = create ~type_:Info ~promise:false
let warning = create ~type_:Warning ~promise:false
let error = create ~type_:Error ~promise:false
let loading = create ~type_:Loading ~promise:false

let promise ?description ?duration ~loading ~success ~error eff =
  let%bind.Effect id = fresh_id None in
  let%bind.Effect () =
    create ~type_:Loading ~promise:true ~id ?description ?duration loading
  in
  match%bind.Effect eff with
  | Ok value ->
    create
      ~type_:Success
      ~promise:true
      ~id
      ?description
      ?duration
      (success value)
  | Error e ->
    create ~type_:Error ~promise:true ~id ?description ?duration (error e)
;;

let dismiss ?id () = send (Dismiss id)

module Settings = struct
  type t =
    { duration : Time_ns.Span.t
    ; gap : int
    }
end

module Entry = struct
  type t =
    { toast : Toast.t
    ; mounted : bool
    ; removed : bool
    ; height : float (* its height when it appeared, or last changed *)
    ; offset_before_remove : float
    ; remaining : Time_ns.Span.t
    ; started : Time_ns.t option (* its timer's, while it runs *)
    ; timer : int (* bumped when its timer stops, so a late expiry is ignored *)
    }

  let duration (settings : Settings.t) t =
    Option.value t.toast.duration ~default:settings.duration
  ;;

  let create settings toast =
    let t =
      { toast
      ; mounted = false
      ; removed = false
      ; height = 0.
      ; offset_before_remove = 0.
      ; remaining = Time_ns.Span.zero
      ; started = None
      ; timer = 0
      }
    in
    { t with remaining = duration settings t }
  ;;
end

module Model = struct
  type t =
    { toasts : Entry.t list (* newest first *)
    ; heights : (string * float) list (* newest first; not those removed *)
    ; expanded : bool
    ; interacting : bool
    }

  let empty =
    { toasts = []; heights = []; expanded = false; interacting = false }
  ;;

  (* Sonner's offset: the heights and gaps of the toasts in front. *)
  let offset t ~gap id =
    let rec go index before = function
      | [] -> 0.
      | (id', h) :: rest ->
        if String.equal id id'
        then (Float.of_int index *. Float.of_int gap) +. before
        else go (index + 1) (before +. h) rest
    in
    go 0 0. t.heights
  ;;
end

module Action = struct
  type t =
    | Command of Command.t
    | Measured of string * float
    | Expire of string * int
    | Remove of string
    | Set_expanded of bool
    | Set_interacting of bool
end

module Input = struct
  type t =
    { sleep : Time_ns.Span.t -> unit Effect.t
    ; now : Time_ns.t Effect.t
    ; settings : Settings.t
    }
end

let apply_action
      ctx
      (input : Input.t Bonsai.Computation_status.t)
      (model : Model.t)
      ((now : Time_ns.t), (action : Action.t))
  =
  match input with
  | Inactive -> model
  | Active input ->
    let settings = input.settings in
    let after span action =
      Bonsai.Apply_action_context.schedule_event
        ctx
        (let%bind.Effect () = input.sleep span in
         let%bind.Effect now = input.now in
         Bonsai.Apply_action_context.inject ctx (now, action))
    in
    let map_entry (model : Model.t) id ~f =
      { model with
        toasts =
          List.map model.toasts ~f:(fun (e : Entry.t) ->
            if String.equal e.toast.id id then f e else e)
      }
    in
    let delete (model : Model.t) id =
      match
        List.find model.toasts ~f:(fun e ->
          String.equal e.toast.id id && not e.removed)
      with
      | None -> model
      | Some _ ->
        let offset = Model.offset model ~gap:settings.gap id in
        after time_before_unmount (Action.Remove id);
        let model =
          map_entry model id ~f:(fun e ->
            { e with removed = true; offset_before_remove = offset })
        in
        { model with
          heights =
            List.filter model.heights ~f:(fun (id', _) ->
              not (String.equal id id'))
        }
    in
    let model =
      match action with
      | Command (Create toast) ->
        (match
           List.find model.toasts ~f:(fun e ->
             String.equal e.toast.id toast.id && not e.removed)
         with
         | Some old ->
           map_entry model toast.id ~f:(fun e ->
             let e = { e with toast } in
             if
               Type.equal old.toast.type_ Loading
               && not (Type.equal toast.type_ Loading)
             then { e with remaining = Entry.duration settings e }
             else e)
         | None ->
           { model with
             toasts =
               Entry.create settings toast
               :: List.filter model.toasts ~f:(fun e ->
                 not (String.equal e.toast.id toast.id))
           })
      | Command (Dismiss None) ->
        List.fold model.toasts ~init:model ~f:(fun model e ->
          delete model e.toast.id)
      | Command (Dismiss (Some id)) -> delete model id
      | Measured (id, height) ->
        let model =
          map_entry model id ~f:(fun e -> { e with height; mounted = true })
        in
        let removed =
          List.exists model.toasts ~f:(fun e ->
            String.equal e.toast.id id && e.removed)
        in
        if removed
        then model
        else if List.Assoc.mem model.heights id ~equal:String.equal
        then
          { model with
            heights =
              List.map model.heights ~f:(fun (id', h) ->
                if String.equal id id' then id', height else id', h)
          }
        else { model with heights = (id, height) :: model.heights }
      | Expire (id, timer) ->
        (match
           List.find model.toasts ~f:(fun e -> String.equal e.toast.id id)
         with
         | Some e when e.timer = timer && Option.is_some e.started ->
           delete model id
         | _ -> model)
      | Remove id ->
        { model with
          toasts =
            List.filter model.toasts ~f:(fun e ->
              not (String.equal e.toast.id id))
        }
      | Set_expanded expanded -> { model with expanded }
      | Set_interacting interacting -> { model with interacting }
    in
    let model =
      if List.length model.toasts <= 1
      then { model with expanded = false }
      else model
    in
    (* Timers run while the stack is neither hovered nor touched; a pause
       keeps what is left. *)
    let paused = model.expanded || model.interacting in
    let timer (e : Entry.t) =
      let runs =
        (not e.removed)
        && (not (Type.equal e.toast.type_ Loading))
        && not (Time_ns.Span.equal (Entry.duration settings e) infinity)
      in
      match e.started with
      | Some started when paused || not runs ->
        { e with
          started = None
        ; timer = e.timer + 1
        ; remaining = Time_ns.Span.(e.remaining - Time_ns.diff now started)
        }
      | None when runs && not paused ->
        let timer = e.timer + 1 in
        after
          (Time_ns.Span.max e.remaining Time_ns.Span.zero)
          (Action.Expire (e.toast.id, timer));
        { e with started = Some now; timer }
      | Some _ | None -> e
    in
    { model with toasts = List.map model.toasts ~f:timer }
;;

(* Sonner measures a toast when it appears (then shows it: data-mounted), and
   again when its content changes. *)
module Measure = struct
  module Input = struct
    type t =
      { key : string
      ; on_height : float -> unit Effect.t
      }

    let sexp_of_t t = [%sexp (t.key : string)]
    let combine _ t = t
  end

  let height (el : Dom_html.element Js.t) =
    Js.to_float el##getBoundingClientRect##.height
  ;;

  module Hook = Vdom.Attr.Hooks.Make (struct
      module State = Unit
      module Input = Input

      let init _ _ = ()

      let on_mount =
        `Schedule_immediately_after_this_dom_patch_completes
          (fun (input : Input.t) () el ->
            Effect.Expert.handle_non_dom_event_exn (input.on_height (height el)))
      ;;

      let update ~(old_input : Input.t) ~(new_input : Input.t) () el =
        if not (String.equal old_input.key new_input.key)
        then (
          let original = el##.style##.height in
          el##.style##.height := Js.string "auto";
          let h = height el in
          el##.style##.height := original;
          Effect.Expert.handle_non_dom_event_exn (new_input.on_height h))
      ;;

      let destroy _ () _ = ()
    end)

  let attr ~key ~on_height =
    Vdom.Attr.create_hook
      "loon-sonner-measure"
      (Hook.create { Input.key; on_height })
  ;;
end

(* Sonner's close icon. *)
let close_icon =
  let svg tag attrs children =
    Node.create_svg
      tag
      ~attrs:(List.map attrs ~f:(fun (k, v) -> Attr.create k v))
      children
  in
  let line x1 y1 x2 y2 =
    svg "line" [ "x1", x1; "y1", y1; "x2", x2; "y2", y2 ] []
  in
  svg
    "svg"
    [ "xmlns", "http://www.w3.org/2000/svg"
    ; "width", "12"
    ; "height", "12"
    ; "viewBox", "0 0 24 24"
    ; "fill", "none"
    ; "stroke", "currentColor"
    ; "stroke-width", "1.5"
    ; "stroke-linecap", "round"
    ; "stroke-linejoin", "round"
    ]
    [ line "18" "6" "6" "18"; line "6" "6" "18" "18" ]
;;

let bool b = Bool.to_string b
let px f = sprintf "%gpx" f

(* As a plain attribute: Attr.style drops custom properties in the browser. *)
let style fields =
  Attr.create
    "style"
    (String.concat
       ~sep:" "
       (List.map fields ~f:(fun (field, value) -> sprintf "%s: %s;" field value)))
;;

let toast_view
      ~(model : Model.t)
      ~settings:({ gap; _ } : Settings.t)
      ~position
      ~expand
      ~rich_colors
      ~close_button
      ~visible_toasts
      ~count
      ~inject
      index
      (entry : Entry.t)
  =
  let toast = entry.toast in
  let id = toast.id in
  let front = index = 0 in
  let delete = inject (Action.Command (Dismiss (Some id))) in
  let offset =
    if entry.removed
    then entry.offset_before_remove
    else Model.offset model ~gap id
  in
  let close_button = Option.value toast.close_button ~default:close_button in
  let loading = Type.equal toast.type_ Loading in
  let button ~kind (label, on_click) =
    Node.button
      ~attrs:
        [ data "button" "true"
        ; data kind "true"
        ; Attr.on_click (fun _ ->
            Effect.Many
              [ on_click
              ; (if toast.dismissible then delete else Effect.Ignore)
              ])
        ]
      [ Node.text label ]
  in
  let icon =
    if Type.equal toast.type_ Default && not toast.promise
    then None
    else
      Some
        (Node.div
           ~attrs:[ data "icon" "" ]
           (List.filter_opt
              [ (if toast.promise || loading
                 then
                   Some
                     (Node.div
                        ~attrs:
                          [ Attr.class_ "sonner-loader"
                          ; data "visible" (bool loading)
                          ]
                        [ Icon.view ~class_:"size-4 animate-spin" Loader_circle
                        ])
                 else None)
              ; Type.icon toast.type_
              ]))
  in
  Node.li
    ~key:id
    ~attrs:
      ([ Attr.tabindex 0
       ; Attr.classes ("rounded-2xl" :: Option.to_list toast.class_)
       ]
       @ [ data "sonner-toast" ""
         ; data
             "rich-colors"
             (bool (Option.value toast.rich_colors ~default:rich_colors))
         ; data "styled" "true"
         ; data "mounted" (bool entry.mounted)
         ; data "promise" (bool toast.promise)
         ; data "swiped" "false"
         ; data "removed" (bool entry.removed)
         ; data "visible" (bool (index + 1 <= visible_toasts))
         ; data "y-position" (Position.y position)
         ; data "x-position" (Position.x position)
         ; data "index" (Int.to_string index)
         ; data "front" (bool front)
         ; data "swiping" "false"
         ; data "dismissible" (bool toast.dismissible)
         ]
       @ Option.value_map (Type.name toast.type_) ~default:[] ~f:(fun n ->
         [ data "type" n ])
       @ [ data "invert" "false"
         ; data "swipe-out" "false"
         ; data "expanded" (bool (model.expanded || (expand && entry.mounted)))
         ; style
             [ "--index", Int.to_string index
             ; "--toasts-before", Int.to_string index
             ; "--z-index", Int.to_string (count - index)
             ; "--offset", px offset
             ; ("--initial-height", if expand then "auto" else px entry.height)
             ]
         ; Measure.attr
             ~key:
               (String.concat
                  ~sep:"\n"
                  [ toast.title
                  ; Option.value toast.description ~default:""
                  ; Option.value_map toast.action ~default:"" ~f:fst
                  ; Option.value_map toast.cancel ~default:"" ~f:fst
                  ; Sexp.to_string (Type.sexp_of_t toast.type_)
                  ])
             ~on_height:(fun h -> inject (Measured (id, h)))
         ])
    (List.filter_opt
       [ (if close_button && not loading
          then
            Some
              (Node.button
                 ~attrs:
                   [ aria "label" "Close toast"
                   ; data "disabled" "false"
                   ; data "close-button" "true"
                   ; Attr.on_click (fun _ ->
                       if toast.dismissible then delete else Effect.Ignore)
                   ]
                 [ close_icon ])
          else None)
       ; icon
       ; Some
           (Node.div
              ~attrs:[ data "content" "" ]
              (Node.div ~attrs:[ data "title" "" ] [ Node.text toast.title ]
               :: Option.value_map toast.description ~default:[] ~f:(fun d ->
                 [ Node.div ~attrs:[ data "description" "" ] [ Node.text d ] ])
              ))
       ; Option.map toast.cancel ~f:(button ~kind:"cancel")
       ; Option.map toast.action ~f:(button ~kind:"action")
       ])
;;

let toaster
      ?class_
      ?(position = Position.Bottom_right)
      ?(theme = Theme.System)
      ?(expand = false)
      ?(rich_colors = false)
      ?(close_button = false)
      ?(visible_toasts = 3)
      ?(duration = toast_lifetime)
      ?(gap = 14)
      (local_ graph)
  =
  let open Bonsai.Let_syntax in
  let settings = { Settings.duration; gap } in
  let sleep = Bonsai.Clock.sleep graph in
  let now = Bonsai.Clock.get_current_time graph in
  let input =
    let%arr sleep
    and now in
    { Input.sleep; now; settings }
  in
  let model, inject_at =
    Bonsai.state_machine_with_input
      ~default_model:Model.empty
      ~apply_action
      input
      graph
  in
  let inject =
    let%arr inject_at
    and now in
    fun action ->
      let%bind.Effect now = now in
      inject_at (now, action)
  in
  let key = Bonsai.path_id graph in
  Bonsai.Edge.lifecycle
    ~on_activate:
      (let%arr inject
       and key in
       let%bind.Effect commands =
         Effect.of_thunk (fun () ->
           toasters
           := (key, fun c -> inject (Action.Command c))
              :: List.filter !toasters ~f:(fun (k, _) ->
                not (String.equal k key));
           let commands = Queue.to_list pending in
           Queue.clear pending;
           commands)
       in
       Effect.Many (List.map commands ~f:(fun c -> inject (Command c))))
    ~on_deactivate:
      (let%arr key in
       Effect.of_thunk (fun () ->
         toasters
         := List.filter !toasters ~f:(fun (k, _) -> not (String.equal k key))))
    graph;
  let%arr model
  and inject in
  let count = List.length model.toasts in
  let list =
    if count = 0
    then []
    else
      [ Node.ol
          ~attrs:
            [ Attr.create "dir" "ltr"
            ; Attr.tabindex (-1)
            ; Cn.attr ([ "toaster group" ] @ Option.to_list class_)
            ; data "sonner-toaster" "true"
            ; data "sonner-theme" (Theme.name theme)
            ; data "y-position" (Position.y position)
            ; data "x-position" (Position.x position)
            ; style
                ([ ( "--front-toast-height"
                   , px
                       (Option.value_map
                          (List.hd model.heights)
                          ~default:0.
                          ~f:snd) )
                 ; "--width", sprintf "%dpx" toast_width
                 ; "--gap", sprintf "%dpx" gap
                 ; "--normal-bg", "var(--popover)"
                 ; "--normal-text", "var(--popover-foreground)"
                 ; "--normal-border", "var(--border)"
                 ; "--border-radius", "var(--radius)"
                 ]
                 @ List.concat_map
                     [ "top"; "right"; "bottom"; "left" ]
                     ~f:(fun side ->
                       [ "--offset-" ^ side, "24px"
                       ; "--mobile-offset-" ^ side, "16px"
                       ]))
            ; Attr.on_mouseenter (fun _ -> inject (Set_expanded true))
            ; Attr.on_mousemove (fun _ ->
                if model.expanded
                then Effect.Ignore
                else inject (Set_expanded true))
            ; Attr.on_mouseleave (fun _ ->
                if model.interacting
                then Effect.Ignore
                else inject (Set_expanded false))
            ; Attr.on_pointerdown (fun _ -> inject (Set_interacting true))
            ; Attr.on_pointerup (fun _ -> inject (Set_interacting false))
            ]
          (List.mapi
             model.toasts
             ~f:
               (toast_view
                  ~model
                  ~settings
                  ~position
                  ~expand
                  ~rich_colors
                  ~close_button
                  ~visible_toasts
                  ~count
                  ~inject))
      ]
  in
  (* Alt+T expands the toasts and focuses them; Esc in them collapses. *)
  let on_keydown (ev : Dom_html.keyboardEvent Js.t) =
    match Dom_util.key ev with
    | KeyT when Js.to_bool ev##.altKey && count > 0 ->
      Effect.Many
        [ inject (Set_expanded true)
        ; Dom_util.focus_selector "[data-sonner-toaster]"
        ]
    | Escape when model.expanded -> inject (Set_expanded false)
    | _ -> Effect.Ignore
  in
  Node.section
    ~attrs:
      [ aria "label" "Notifications alt+T"
      ; Attr.tabindex (-1)
      ; aria "live" "polite"
      ; aria "relevant" "additions text"
      ; aria "atomic" "false"
      ; Vdom.Attr.Global_listeners.keydown ~phase:Bubbling ~f:on_keydown
      ]
    list
;;
