open! Core
open! Import

module Event = struct
  type t =
    | Auth_url of
        { url : string
        ; instructions : string
        }
    | Prompt of
        { id : string
        ; prompt : Auth_interaction.Prompt.t
        }
    | Prompt_cancelled of { id : string }
    | Progress of string
    | Done of
        { provider : Provider_id.t
        ; method_ : Provider_auth.Method.t
        }
    | Failed of
        { provider : Provider_id.t
        ; error : string
        }
    | Logged_out of Provider_id.t
  [@@deriving sexp_of]
end

module Pending = struct
  type t =
    { id : string
    ; resolver : string option Promise.u
    }
end

module Flow = struct
  type t =
    { cancel : Cancellation.t
    ; mutable pending : Pending.t option
    ; finished : unit Promise.t
    }
end

type t =
  { env : Env.t
  ; sw : Switch.t
  ; store : Auth_store.t
  ; getenv : string -> string option
  ; mutable flow : Flow.t option
  ; mutable subscribers : (Event.t -> unit) list
  ; mutable next_id : int
  }

let create ~env ~sw ?(getenv = Sys.getenv) ~store () =
  { env; sw; store; getenv; flow = None; subscribers = []; next_id = 1 }
;;

let store t = t.store
let subscribe t ~f = t.subscribers <- f :: t.subscribers
let emit t event = List.iter (List.rev t.subscribers) ~f:(fun f -> f event)
let in_progress t = Option.is_some t.flow
let status t = Provider_auth.status ~getenv:t.getenv t.store

let interaction t (flow : Flow.t) : Auth_interaction.t =
  let prompt prompt =
    let id = sprintf "p%d" t.next_id in
    t.next_id <- t.next_id + 1;
    let promise, resolver = Promise.create () in
    flow.pending <- Some { id; resolver };
    emit t (Prompt { id; prompt });
    let answered = ref false in
    Fun.protect
      ~finally:(fun () ->
        (match flow.pending with
         | Some p when String.equal p.id id -> flow.pending <- None
         | _ -> ());
        if not !answered then emit t (Prompt_cancelled { id }))
      (fun () ->
         let answer = Promise.await promise in
         answered := true;
         match answer with
         | Some value -> Ok value
         | None -> Auth_interaction.cancelled ())
  in
  let notify : Auth_interaction.Notice.t -> unit = function
    | Auth_url { url; instructions } -> emit t (Auth_url { url; instructions })
    | Progress message -> emit t (Progress message)
  in
  { prompt; notify; cancel = flow.cancel }
;;

let start t provider method_ =
  if in_progress t
  then Or_error.error_string "a login is already in progress"
  else if
    not
      (List.mem
         (Provider_auth.methods provider)
         method_
         ~equal:Provider_auth.Method.equal)
  then
    Or_error.error_s
      [%message
        "login method not supported by provider"
          (provider : Provider_id.t)
          (method_ : Provider_auth.Method.t)]
  else (
    let finished, resolve = Promise.create () in
    let flow =
      { Flow.cancel = Cancellation.create (); pending = None; finished }
    in
    t.flow <- Some flow;
    Fiber.fork ~sw:t.sw (fun () ->
      let result =
        try
          Provider_auth.login
            ~env:t.env
            t.store
            provider
            method_
            (interaction t flow)
        with
        | exn -> Or_error.error_s [%message "login failed" (exn : exn)]
      in
      t.flow <- None;
      (match result with
       | Ok () -> emit t (Done { provider; method_ })
       | Error e -> emit t (Failed { provider; error = Error.to_string_hum e }));
      Promise.resolve resolve ());
    Ok ())
;;

let respond t ~id value =
  match t.flow with
  | None -> Or_error.error_string "no login in progress"
  | Some flow ->
    (match flow.pending with
     | Some pending when String.equal pending.id id ->
       flow.pending <- None;
       Promise.resolve pending.resolver (Some value);
       Ok ()
     | _ -> Or_error.errorf "no pending login prompt %S" id)
;;

let cancel t =
  Option.iter t.flow ~f:(fun flow ->
    Option.iter flow.pending ~f:(fun p ->
      flow.pending <- None;
      Promise.resolve p.resolver None);
    Cancellation.cancel flow.cancel)
;;

let wait t = Option.iter t.flow ~f:(fun flow -> Promise.await flow.finished)

let logout t provider =
  Or_error.map (Provider_auth.logout t.store provider) ~f:(fun () ->
    emit t (Logged_out provider))
;;
