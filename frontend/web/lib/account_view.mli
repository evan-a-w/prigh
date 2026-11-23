open! Core
open! Import

(** Who we are signed in as, their avatar, and the account menu. *)

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

(** Their letter in a circle (loon's Avatar fallback). *)
val avatar : ?size:[ `Small | `Default | `Large ] -> string -> Node.t

(** The account menu, from its [Dialog.Picker { kind = Accounts; _ }]. *)
val menu
  :  App.Model.t
  -> Picker.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t
