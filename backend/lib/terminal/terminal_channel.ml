open! Core
open! Import

module Frame = struct
  type t =
    [ `Binary of string
    | `Text of string
    ]
  [@@deriving sexp_of, equal]

  let to_fields : t -> _ = function
    | `Binary data ->
      [ "kind", `String "binary"; "data", `String (Base64.encode_string data) ]
    | `Text text -> [ "kind", `String "text"; "data", `String text ]
  ;;

  let of_json json =
    match Json.member "kind" json, Json.member "data" json with
    | Some (`String "text"), Some (`String text) -> Ok (`Text text)
    | Some (`String "binary"), Some (`String data) ->
      (match Base64.decode data with
       | Ok data -> Ok (`Binary data)
       | Error (`Msg msg) -> Or_error.errorf "bad base64 in \"data\": %s" msg)
    | _ ->
      Or_error.error_string
        "a frame needs \"kind\" (\"binary\" or \"text\") and a string \"data\""
  ;;
end

type t =
  { send : Frame.t -> unit
  ; read : unit -> Frame.t option
  ; is_closed : unit -> bool
  }

let of_websocket ws =
  { send =
      (function
        | `Binary data -> Websocket.send_binary ws data
        | `Text text -> Websocket.send_text ws text)
  ; read = (fun () -> Websocket.read ws)
  ; is_closed = (fun () -> Websocket.is_closed ws)
  }
;;

let send t frame = if not (t.is_closed ()) then t.send frame
let send_text t text = send t (`Text text)
let read t = t.read ()
let is_closed t = t.is_closed ()

module Fed = struct
  type nonrec t =
    { incoming : Frame.t option Eio.Stream.t
    ; closed : bool ref
    ; channel : t
    }

  let create ~send =
    let incoming = Eio.Stream.create Int.max_value in
    let closed = ref false in
    let ended = ref false in
    let read () =
      if !ended
      then None
      else (
        match Eio.Stream.take incoming with
        | Some frame -> Some frame
        | None ->
          ended := true;
          None)
    in
    { incoming
    ; closed
    ; channel = { send; read; is_closed = (fun () -> !closed) }
    }
  ;;

  let channel t = t.channel

  let push t frame =
    if not !(t.closed) then Eio.Stream.add t.incoming (Some frame)
  ;;

  let close t =
    if not !(t.closed)
    then (
      t.closed := true;
      Eio.Stream.add t.incoming None)
  ;;
end
