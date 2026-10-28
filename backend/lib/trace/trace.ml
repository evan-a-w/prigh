open! Core
open! Import

module Level = struct
  type t =
    | Off
    | Default
    | Detailed
  [@@deriving sexp_of, equal, compare, enumerate]

  let to_string = function
    | Off -> "off"
    | Default -> "default"
    | Detailed -> "detailed"
  ;;

  let of_string s =
    match List.find all ~f:(fun t -> String.equal (to_string t) s) with
    | Some t -> Ok t
    | None ->
      Or_error.errorf
        "unknown trace level %S: use %s"
        s
        (String.concat ~sep:", " (List.map all ~f:to_string))
  ;;

  let effective ~configured =
    match Sys.getenv "PRIGH_TRACE" with
    | None -> configured
    | Some s ->
      (match of_string (String.lowercase (String.strip s)) with
       | Ok t -> t
       | Error _ -> configured)
  ;;
end

module Value = struct
  type t =
    | Int of int
    | Float of float
    | String of string
    | Bool of bool
  [@@deriving sexp_of, equal]

  let to_json = function
    | Int i -> `Number (Int.to_string i)
    | Float f -> `Number (sprintf "%.6g" f)
    | String s -> `String s
    | Bool b -> if b then `True else `False
  ;;

  let of_json : Json.t -> t option = function
    | `Number s ->
      if String.exists s ~f:(fun c -> Char.equal c '.' || Char.equal c 'e')
      then Option.map (Float.of_string_opt s) ~f:(fun f -> Float f)
      else Option.map (Int.of_string_opt s) ~f:(fun i -> Int i)
    | `String s -> Some (String s)
    | `True -> Some (Bool true)
    | `False -> Some (Bool false)
    | `Null | `Array _ | `Object _ -> None
  ;;

  let to_string_hum = function
    | Int i -> Int.to_string i
    | Float f -> sprintf "%g" f
    | String s -> s
    | Bool b -> Bool.to_string b
  ;;
end

module Status = struct
  type t =
    | Ok
    | Cancelled
    | Error
  [@@deriving sexp_of, equal, enumerate]

  let to_string = function
    | Ok -> "ok"
    | Cancelled -> "cancelled"
    | Error -> "error"
  ;;
end

module Span = struct
  type t =
    { id : string
    ; parent : string option
    ; name : string
    ; track : string
    ; wall : float
    ; start : float
    ; stop : float
    ; status : Status.t
    ; attrs : (string * Value.t) list
    }
  [@@deriving sexp_of]

  let duration t = Float.max 0. (t.stop -. t.start)
  let attr t name = List.Assoc.find t.attrs ~equal:String.equal name
  let seconds f = `Number (sprintf "%.6f" f)

  let to_json t : Json.t =
    `Object
      ([ "id", `String t.id ]
       @ Option.value_map t.parent ~default:[] ~f:(fun p ->
         [ "parent", `String p ])
       @ [ "name", `String t.name
         ; "track", `String t.track
         ; "wall", seconds t.wall
         ; "start", seconds t.start
         ; "stop", seconds t.stop
         ; "status", `String (Status.to_string t.status)
         ; ( "attrs"
           , `Object (List.map t.attrs ~f:(fun (k, v) -> k, Value.to_json v)) )
         ])
  ;;

  let of_json (json : Json.t) =
    let field name = Json.member name json in
    let string name =
      match field name with
      | Some (`String s) -> Ok s
      | _ -> Or_error.errorf "span: %s must be a string" name
    in
    let number name =
      match field name with
      | Some (`Number s) ->
        (match Float.of_string_opt s with
         | Some f -> Ok f
         | None -> Or_error.errorf "span: %s is not a number" name)
      | _ -> Or_error.errorf "span: %s must be a number" name
    in
    let open Or_error.Let_syntax in
    let%bind id = string "id" in
    let%bind name = string "name" in
    let%bind track = string "track" in
    let%bind wall = number "wall" in
    let%bind start = number "start" in
    let%bind stop = number "stop" in
    let%bind status =
      let%bind s = string "status" in
      match
        List.find Status.all ~f:(fun st -> String.equal (Status.to_string st) s)
      with
      | Some st -> Ok st
      | None -> Or_error.errorf "span: unknown status %S" s
    in
    let parent =
      match field "parent" with
      | Some (`String p) -> Some p
      | _ -> None
    in
    let attrs =
      match field "attrs" with
      | Some (`Object fields) ->
        List.filter_map fields ~f:(fun (k, v) ->
          Option.map (Value.of_json v) ~f:(fun v -> k, v))
      | _ -> []
    in
    return { id; parent; name; track; wall; start; stop; status; attrs }
  ;;
end

module Clock = struct
  type t =
    { mono : unit -> float
    ; wall : unit -> float
    }

  let of_env (env : Env.t) =
    let mono = Eio.Stdenv.mono_clock env in
    let clock = Eio.Stdenv.clock env in
    { mono =
        (fun () ->
          Int64.to_float (Mtime.to_uint64_ns (Eio.Time.Mono.now mono)) /. 1e9)
    ; wall = (fun () -> Eio.Time.now clock)
    }
  ;;
end

type t =
  { clock : Clock.t
  ; id_prefix : string
  ; mutable level : Level.t
  ; mutable next_id : int
  ; write : Span.t -> unit
  ; mutable write_failed : bool
  }

(* Unique to the tracer: its creation time (ms) and a count of the tracers
   this process made. *)
let tracers = ref 0

let fresh_prefix (clock : Clock.t) =
  incr tracers;
  sprintf "%x.%d." (Float.iround_down_exn (clock.wall () *. 1000.)) !tracers
;;

let create ?id_prefix ~clock ~level ~write () =
  { clock
  ; id_prefix = Option.value_or_thunk id_prefix ~default:(fun () -> fresh_prefix clock)
  ; level
  ; next_id = 1
  ; write
  ; write_failed = false
  }
;;

let level t = t.level
let set_level t level = t.level <- level

module Open = struct
  type tracer = t

  type t =
    { tracer : tracer
    ; id : string
    ; parent : t option
    ; name : string
    ; track : string
    ; wall : float
    ; start : float
    ; mutable attrs : (string * Value.t) list (** newest first *)
    ; phased : bool
    ; mutable phase : t option
    ; mutable closed : bool
    }

  (* Where children go: its current phase, if it has one. *)
  let container t = Option.value t.phase ~default:t

  let rec find t ~f =
    if f t then Some t else Option.bind t.parent ~f:(fun p -> find p ~f)
  ;;
end

module Ctx = struct
  type t =
    { tracer : Open.tracer
    ; current : Open.t option
    ; track : string
    }
end

let key : Ctx.t Fiber.key = Fiber.create_key ()

let run t ?(track = "main") f =
  Fiber.with_binding key { Ctx.tracer = t; current = None; track } f
;;

let allowed (tracer : t) ~detailed =
  match tracer.level with
  | Off -> false
  | Default -> not detailed
  | Detailed -> true
;;

let enabled ?(detailed = false) () =
  match Fiber.get key with
  | None -> false
  | Some ctx -> allowed ctx.tracer ~detailed
;;

let now () =
  match Fiber.get key with
  | Some ctx when allowed ctx.tracer ~detailed:false ->
    Some (ctx.tracer.clock.mono ())
  | Some _ | None -> None
;;

let open_span (tracer : t) ~parent ~track ~at ~phased ~attrs name =
  let now = tracer.clock.mono () in
  let start = Option.value at ~default:now in
  let id = tracer.id_prefix ^ Int.to_string tracer.next_id in
  tracer.next_id <- tracer.next_id + 1;
  { Open.tracer
  ; id
  ; parent
  ; name
  ; track
  ; wall = tracer.clock.wall () -. (now -. start)
  ; start
  ; attrs = List.rev attrs
  ; phased
  ; phase = None
  ; closed = false
  }
;;

let write (tracer : t) span =
  match tracer.write span with
  | () -> ()
  | exception exn ->
    if not tracer.write_failed
    then (
      tracer.write_failed <- true;
      eprintf "prigh: writing a trace span failed: %s\n%!" (Exn.to_string exn))
;;

let rec close ?(status = Status.Ok) ?(attrs = []) (o : Open.t) =
  if not o.closed
  then (
    o.closed <- true;
    Option.iter o.phase ~f:(fun p -> close p ~status);
    o.phase <- None;
    let stop = Float.max o.start (o.tracer.clock.mono ()) in
    write
      o.tracer
      { Span.id = o.id
      ; parent = Option.map o.parent ~f:(fun p -> p.id)
      ; name = o.name
      ; track = o.track
      ; wall = o.wall
      ; start = o.start
      ; stop
      ; status
      ; attrs =
          List.rev_append
            (List.filter o.attrs ~f:(fun (k, _) ->
               not (List.Assoc.mem attrs ~equal:String.equal k)))
            attrs
      })
;;

let start_phase (o : Open.t) name =
  match o.phase with
  | Some p when String.equal p.name name -> ()
  | _ ->
    Option.iter o.phase ~f:(fun p -> close p);
    o.phase
    <- Some
         (open_span
            o.tracer
            ~parent:(Some o)
            ~track:o.track
            ~at:None
            ~phased:false
            ~attrs:[]
            name)
;;

let start
      ?(detailed = false)
      ?(root = false)
      ?under
      ?track
      ?at
      ?phase
      ?(attrs = [])
      name
  =
  match Fiber.get key with
  | None -> None
  | Some ctx when not (allowed ctx.tracer ~detailed) -> None
  | Some ctx ->
    let parent =
      if root
      then None
      else (
        match under, ctx.current with
        | _, None -> None
        | None, Some current -> Some (Open.container current)
        | Some under, Some current ->
          (match
             Open.find current ~f:(fun o -> String.equal o.name under)
           with
           | Some o -> Some o
           | None -> Some (Open.container current)))
    in
    let track =
      match track, parent with
      | Some track, _ -> track
      | None, Some p -> p.track
      | None, None -> ctx.track
    in
    let o =
      open_span
        ctx.tracer
        ~parent
        ~track
        ~at
        ~phased:(Option.is_some phase)
        ~attrs
        name
    in
    Option.iter phase ~f:(start_phase o);
    Some o
;;

let finish ?status ?attrs o = Option.iter o ~f:(fun o -> close ?status ?attrs o)

let status_of_exn = function
  | Eio.Cancel.Cancelled _ -> Status.Cancelled
  | _ -> Error
;;

let within o f =
  match o with
  | None -> f ()
  | Some (o : Open.t) ->
    (match
       Fiber.with_binding
         key
         { Ctx.tracer = o.tracer; current = Some o; track = o.track }
         f
     with
     | x -> x
     | exception exn ->
       let backtrace = Backtrace.Exn.most_recent () in
       close o ~status:(status_of_exn exn);
       Exn.raise_with_original_backtrace exn backtrace)
;;

let span ?detailed ?root ?under ?track ?phase ?attrs name f =
  match start ?detailed ?root ?under ?track ?phase ?attrs name with
  | None -> f ()
  | Some _ as o ->
    let x = within o f in
    finish o;
    x
;;

let set_attrs (o : Open.t) attrs =
  if not o.closed
  then
    o.attrs
    <- List.rev_append
         attrs
         (List.filter o.attrs ~f:(fun (k, _) ->
            not (List.Assoc.mem attrs ~equal:String.equal k)))
;;

let add_attrs_to o attrs = Option.iter o ~f:(fun o -> set_attrs o attrs)

let add_attrs ?under attrs =
  Option.iter (Fiber.get key) ~f:(fun (ctx : Ctx.t) ->
    Option.iter ctx.current ~f:(fun current ->
      match under with
      | None -> set_attrs current attrs
      | Some name ->
        Option.iter
          (Open.find current ~f:(fun o -> String.equal o.name name))
          ~f:(fun o -> set_attrs o attrs)))
;;

let phased () =
  Option.bind (Fiber.get key) ~f:(fun (ctx : Ctx.t) ->
    Option.bind ctx.current ~f:(fun current ->
      Open.find current ~f:(fun o -> o.phased && not o.closed)))
;;

let phase name = Option.iter (phased ()) ~f:(fun o -> start_phase o name)

let current_phase () =
  Option.bind (phased ()) ~f:(fun o ->
    Option.map o.phase ~f:(fun (p : Open.t) -> p.name))
;;

let track () =
  match Fiber.get key with
  | None -> "main"
  | Some ctx -> ctx.track
;;

let short ?(max = 120) s =
  Value.String
    (if String.length s <= max
     then s
     else fst (Utf8.split_incomplete_suffix (String.prefix s (max - 3))) ^ "...")
;;
