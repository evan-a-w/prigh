open! Core
open! Import

(** Wire encoding for the frontend: plain tagged JSON objects rather than the
    derived [["Ctor", ...]] form. *)

val message : Message.t -> Json.t
val delta : Assistant_event.t -> Json.t
val state : Agent.State.t -> Json.t
val config : Config.t -> Json.t
val model : Model.t -> Json.t
val session_summary : Session.Summary.t -> Json.t
val session_stats : Agent.Session_stats.t -> Json.t
val entry : Session.Entry.t -> Json.t
val thinking : Thinking.t -> Json.t
val thinking_of_string : string -> Thinking.t Or_error.t
val auth_status : Provider_auth.Status.t -> Json.t

(** [("type", "event"); ("event", name); ...fields]. *)
val event : Agent.Event.t -> Json.t

(** [("type", "event"); ("event", "auth"); ("kind", ...); ...fields]. *)
val login_event : Login_manager.Event.t -> Json.t
