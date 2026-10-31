open! Core
open Js_of_ocaml
module App = Prigh_web.App
module Dictation = Prigh_web.Dictation
module Action = App.Action

let schedule = ref (fun (_ : Action.t) -> ())
let get o name = Js.Unsafe.get o (Js.string name)
let set o name v = Js.Unsafe.set o (Js.string name) v
let meth o name args = Js.Unsafe.meth_call o name args
let window = Js.Unsafe.coerce Dom_html.window

let present (v : Js.Unsafe.any) =
  not (Js.Unsafe.equals v Js.undefined || Js.Unsafe.equals v Js.null)
;;

let recognition_class () =
  let standard = get window "SpeechRecognition" in
  if present standard
  then Some standard
  else (
    let webkit = get window "webkitSpeechRecognition" in
    if present webkit then Some webkit else None)
;;

let browser_speech () = Option.is_some (recognition_class ())

(* One dictation at a time: what to stop or throw away. *)
type session =
  { mutable cancelled : bool
  ; mutable stream : Js.Unsafe.any option
  ; mutable recorder : Js.Unsafe.any option
  ; mutable recognition : Js.Unsafe.any option
  ; mutable audio : Js.Unsafe.any option
  ; mutable timer : Dom_html.interval_id option
  ; mutable heard : string list (** the browser's recognised phrases *)
  ; started : float
  }

let current : session option ref = ref None
let now () = Js.to_float (new%js Js.date_now)##getTime

let release s =
  Option.iter s.timer ~f:(fun t -> Dom_html.window##clearInterval t);
  s.timer <- None;
  Option.iter s.stream ~f:(fun stream ->
    let tracks = meth stream "getTracks" [||] in
    let n : int = get tracks "length" in
    for i = 0 to n - 1 do
      ignore (meth (Js.Unsafe.get tracks i) "stop" [||] : Js.Unsafe.any)
    done);
  s.stream <- None;
  Option.iter s.audio ~f:(fun ctx ->
    ignore (meth ctx "close" [||] : Js.Unsafe.any));
  s.audio <- None;
  match !current with
  | Some c when phys_equal c s -> current := None
  | _ -> ()
;;

let fail s message =
  if not s.cancelled
  then (
    s.cancelled <- true;
    release s;
    !schedule (Action.Dictation_failed message))
;;

let error_message (e : Js.Unsafe.any) =
  if present e
  then (
    let name = get e "name" in
    let message = get e "message" in
    match present name, present message with
    | true, _ when String.equal (Js.to_string name) "NotAllowedError" ->
      "The microphone is blocked: allow it for this site in the browser"
    | true, _ when String.equal (Js.to_string name) "NotFoundError" ->
      "There is no microphone"
    | _, true -> "Microphone: " ^ Js.to_string message
    | _ -> "The microphone could not be opened")
  else "The microphone could not be opened"
;;

(* Every 100 ms: how long, and (when there is a stream) how loud. *)
let tick s ~analyser =
  let level =
    match analyser with
    | None -> 0.
    | Some analyser ->
      let n : int = get analyser "fftSize" in
      let buffer = new%js Typed_array.uint8Array n in
      ignore
        (meth analyser "getByteTimeDomainData" [| Js.Unsafe.inject buffer |]
         : Js.Unsafe.any);
      let sum = ref 0. in
      for i = 0 to n - 1 do
        let v =
          (Float.of_int (Typed_array.unsafe_get buffer i) -. 128.) /. 128.
        in
        sum := !sum +. (v *. v)
      done;
      Float.min 1. (Float.sqrt (!sum /. Float.of_int n) *. 4.)
  in
  let elapsed_ms = Float.to_int (now () -. s.started) in
  if elapsed_ms >= Dictation.max_ms
  then !schedule Action.Dictate
  else !schedule (Action.Dictation_progress { elapsed_ms; level })
;;

let start_timer s ~analyser =
  s.timer
  <- Some
       (Dom_html.window##setInterval
          (Js.wrap_callback (fun () -> if not s.cancelled then tick s ~analyser))
          (Js.float 100.))
;;

let analyser_of s stream =
  let ctor =
    let a = get window "AudioContext" in
    if present a
    then Some a
    else (
      let w = get window "webkitAudioContext" in
      if present w then Some w else None)
  in
  Option.bind ctor ~f:(fun ctor ->
    try
      let ctx = Js.Unsafe.new_obj ctor [||] in
      s.audio <- Some ctx;
      let source =
        meth ctx "createMediaStreamSource" [| Js.Unsafe.inject stream |]
      in
      let analyser = meth ctx "createAnalyser" [||] in
      set analyser "fftSize" 512;
      ignore
        (meth source "connect" [| Js.Unsafe.inject analyser |] : Js.Unsafe.any);
      Some analyser
    with
    | _ -> None)
;;

let recorder_type () =
  let recorder = get window "MediaRecorder" in
  List.find
    [ "audio/webm;codecs=opus"
    ; "audio/webm"
    ; "audio/ogg;codecs=opus"
    ; "audio/mp4"
    ]
    ~f:(fun t ->
      Js.to_bool
        (meth recorder "isTypeSupported" [| Js.Unsafe.inject (Js.string t) |]))
;;

let deliver_blob s blob ~mime_type =
  let reader = new%js File.fileReader in
  reader##.onload
  := Dom.handler (fun _ ->
       (match Js.Opt.to_option (File.CoerceTo.string reader##.result) with
        | Some url when not s.cancelled ->
          (match String.lsplit2 (Js.to_string url) ~on:',' with
           | Some (_, data) when not (String.is_empty data) ->
             !schedule (Action.Dictation_audio { mime_type; data })
           | _ -> !schedule (Action.Dictation_text ""))
        | _ -> ());
       Js._true);
  reader##readAsDataURL blob
;;

let record s =
  let devices = get (get window "navigator") "mediaDevices" in
  if not (present devices && present (get window "MediaRecorder"))
  then fail s "This browser cannot record audio (it needs a secure https page)"
  else (
    let constraints = Js.Unsafe.obj [| "audio", Js.Unsafe.inject Js._true |] in
    let promise =
      meth devices "getUserMedia" [| Js.Unsafe.inject constraints |]
    in
    let on_stream stream =
      if s.cancelled
      then (
        s.stream <- Some stream;
        release s)
      else (
        s.stream <- Some stream;
        let options =
          match recorder_type () with
          | Some t ->
            Js.Unsafe.obj [| "mimeType", Js.Unsafe.inject (Js.string t) |]
          | None -> Js.Unsafe.obj [||]
        in
        let recorder =
          Js.Unsafe.new_obj
            (get window "MediaRecorder")
            [| Js.Unsafe.inject stream; Js.Unsafe.inject options |]
        in
        s.recorder <- Some recorder;
        let chunks = new%js Js.array_empty in
        set
          recorder
          "ondataavailable"
          (Js.wrap_callback (fun ev ->
             let data = get ev "data" in
             if present data && (get data "size" : int) > 0
             then ignore (chunks##push data : int)));
        set
          recorder
          "onstop"
          (Js.wrap_callback (fun _ ->
             let mime_type =
               let t : Js.js_string Js.t = get recorder "mimeType" in
               let t = Js.to_string t in
               if String.is_empty t then "audio/webm" else t
             in
             release s;
             if not s.cancelled
             then (
               let blob =
                 Js.Unsafe.new_obj
                   (get window "Blob")
                   [| Js.Unsafe.inject chunks
                    ; Js.Unsafe.inject
                        (Js.Unsafe.obj
                           [| "type", Js.Unsafe.inject (Js.string mime_type) |])
                   |]
               in
               deliver_blob s blob ~mime_type)));
        ignore
          (meth recorder "start" [| Js.Unsafe.inject 250 |] : Js.Unsafe.any);
        start_timer s ~analyser:(analyser_of s stream);
        tick s ~analyser:None)
    in
    ignore
      (meth
         promise
         "then"
         [| Js.Unsafe.inject (Js.wrap_callback on_stream)
          ; Js.Unsafe.inject
              (Js.wrap_callback (fun e -> fail s (error_message e)))
         |]
       : Js.Unsafe.any))
;;

let listen s =
  match recognition_class () with
  | None -> fail s "This browser has no speech recognition"
  | Some ctor ->
    let r = Js.Unsafe.new_obj ctor [||] in
    s.recognition <- Some r;
    set r "continuous" Js._true;
    set r "interimResults" Js._false;
    set r "lang" (get (get window "navigator") "language");
    set
      r
      "onresult"
      (Js.wrap_callback (fun ev ->
         let results = get ev "results" in
         let first : int = get ev "resultIndex" in
         let n : int = get results "length" in
         for i = first to n - 1 do
           let result = Js.Unsafe.get results i in
           if Js.to_bool (get result "isFinal")
           then (
             let alt = Js.Unsafe.get result 0 in
             s.heard <- Js.to_string (get alt "transcript") :: s.heard)
         done));
    set
      r
      "onerror"
      (Js.wrap_callback (fun ev ->
         let e = Js.to_string (get ev "error") in
         match e with
         | "no-speech" | "aborted" -> ()
         | "not-allowed" | "service-not-allowed" ->
           fail
             s
             "The microphone is blocked: allow it for this site in the browser"
         | e -> fail s ("Speech recognition failed: " ^ e)));
    (* It ends when stopped, or by itself after a silence: either way what
       it heard goes in. *)
    set
      r
      "onend"
      (Js.wrap_callback (fun _ ->
         release s;
         if not s.cancelled
         then (
           s.cancelled <- true;
           !schedule
             (Action.Dictation_text (String.concat ~sep:" " (List.rev s.heard))))));
    (try
       ignore (meth r "start" [||] : Js.Unsafe.any);
       start_timer s ~analyser:None;
       tick s ~analyser:None
     with
     | _ -> fail s "Speech recognition could not start")
;;

let cancel () =
  Option.iter !current ~f:(fun s ->
    s.cancelled <- true;
    Option.iter s.recorder ~f:(fun r ->
      if not (String.equal (Js.to_string (get r "state")) "inactive")
      then ignore (meth r "stop" [||] : Js.Unsafe.any));
    Option.iter s.recognition ~f:(fun r ->
      ignore (meth r "abort" [||] : Js.Unsafe.any));
    release s)
;;

let start (engine : Dictation.Engine.t) =
  cancel ();
  let s =
    { cancelled = false
    ; stream = None
    ; recorder = None
    ; recognition = None
    ; audio = None
    ; timer = None
    ; heard = []
    ; started = now ()
    }
  in
  current := Some s;
  match engine with
  | Server -> record s
  | Browser -> listen s
;;

let stop () =
  Option.iter !current ~f:(fun s ->
    Option.iter s.timer ~f:(fun t -> Dom_html.window##clearInterval t);
    s.timer <- None;
    match s.recorder, s.recognition with
    | Some r, _ ->
      if String.equal (Js.to_string (get r "state")) "inactive"
      then fail s "The recording had stopped"
      else ignore (meth r "stop" [||] : Js.Unsafe.any)
    | None, Some r -> ignore (meth r "stop" [||] : Js.Unsafe.any)
    | None, None ->
      (* Still waiting for the microphone: nothing was said. *)
      s.cancelled <- true;
      release s;
      !schedule (Action.Dictation_text ""))
;;

let install ~schedule:f =
  schedule := f;
  f (Action.Browser_speech (browser_speech ()))
;;
