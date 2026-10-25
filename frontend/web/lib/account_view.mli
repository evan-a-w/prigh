open! Core
open! Import

(** The account switcher: the button at the bottom of the sidebar (who we are,
    and who we act as) and the menu it opens. *)

module Who : sig
  type t =
    { name : string
    ; acting_as : string option (** a superuser's [/setusr] *)
    ; host : string option (** the backend, for a saved account *)
    }
end

(** Who we are signed in as; [None] on an open backend. *)
val who : App.Model.t -> Who.t option

(** The avatar's letter. *)
val initial : string -> string

(** [None] when there is no account to show (an open backend). *)
val trigger
  :  App.Model.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t option

(** The account menu, from its [Dialog.Picker { kind = Accounts; _ }]. *)
val menu
  :  App.Model.t
  -> Picker.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t
