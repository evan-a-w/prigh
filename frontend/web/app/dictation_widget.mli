open! Core

(** The composer's microphone ([Prigh_web.Dictation]): records with
    MediaRecorder and hands the audio over ([Dictation_audio], for the
    backend's [transcribe]), or has the browser's SpeechRecognition listen
    ([Dictation_text]); meanwhile [Dictation_progress] every 100 ms says how
    long and how loud. Failures come back as [Dictation_failed]. *)

val start : Prigh_web.Dictation.Engine.t -> unit

(** Stops listening, and transcribes what was heard. *)
val stop : unit -> unit

(** Stops listening, and throws away what was heard. *)
val cancel : unit -> unit

(** Says whether the browser has speech recognition ([Browser_speech]). *)
val install : schedule:(Prigh_web.App.Action.t -> unit) -> unit
