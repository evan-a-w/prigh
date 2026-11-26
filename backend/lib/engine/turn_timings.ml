open! Core
open! Import

module Phase = struct
  type t =
    { name : string
    ; seconds : float
    ; alongside : bool
    ; children : t list
    }
  [@@deriving sexp_of]
end

module Frame = struct
  type t =
    { started : float
    ; mutable children : Phase.t list (* newest first *)
    }
end

type t =
  { started : float
  ; mutable phases : Phase.t list (* newest first *)
  ; mutable stack : Frame.t list
  }

let now () = Time_float.(Span.to_sec (to_span_since_epoch (now ())))
let create () = { started = now (); phases = []; stack = [] }
let key : t Fiber.key = Fiber.create_key ()
let with_ t f = Fiber.with_binding key t f
let current () = Fiber.get key
let elapsed t = now () -. t.started

let phases t =
  let alongside, after =
    List.partition_tf (List.rev t.phases) ~f:(fun (p : Phase.t) -> p.alongside)
  in
  after @ alongside
;;

let add t (p : Phase.t) =
  match t.stack with
  | frame :: _ when not p.alongside -> frame.children <- p :: frame.children
  | _ -> t.phases <- p :: t.phases
;;

let record t name ~seconds =
  t.phases <- { name; seconds; alongside = false; children = [] } :: t.phases
;;

let phase ?(alongside = false) name f =
  Trace.span name
  @@ fun () ->
  match current () with
  | None -> f ()
  | Some t ->
    let frame = { Frame.started = now (); children = [] } in
    if not alongside then t.stack <- frame :: t.stack;
    Exn.protect ~f ~finally:(fun () ->
      if not alongside
      then t.stack <- List.filter t.stack ~f:(fun f -> not (phys_equal f frame));
      add
        t
        { name
        ; seconds = now () -. frame.started
        ; alongside
        ; children = List.rev frame.children
        })
;;

let rec to_string_hum phases =
  List.map phases ~f:(fun (p : Phase.t) ->
    let notes =
      (if p.alongside then [ "alongside" ] else [])
      @
      match to_string_hum p.children with
      | "" -> []
      | children -> [ children ]
    in
    sprintf
      "%s %.1f s%s"
      p.name
      p.seconds
      (match notes with
       | [] -> ""
       | notes -> sprintf " (%s)" (String.concat ~sep:"; " notes)))
  |> String.concat ~sep:", "
;;

let notice ~harness ~total phases =
  sprintf
    "%s took %.1f s to start answering: %s"
    harness
    total
    (match to_string_hum phases with
     | "" -> "no phases"
     | s -> s)
;;
