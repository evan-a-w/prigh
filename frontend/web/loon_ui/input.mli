(** Displays a form input field or a component that looks like an input
    field. Port of shadcn/ui's [Input]: an [<input>].

    {[
      Input.view ~type_:Email ~placeholder:"Email" ()
    ]} *)

open! Import

module Type : sig
  type t =
    | Text
    | Email
    | Password
    | Search
    | Number
    | Url
    | Tel
    | File
    | Date
    | Time
  [@@deriving enumerate, sexp_of, compare, equal]

  (** The [type] attribute: [text], [email], ... *)
  val name : t -> string
end

(** shadcn's classes for an input, then [class_]. *)
val classes : ?class_:string -> unit -> string

(** An [<input>] ([type="text"] unless [type_] says otherwise). With
    [value] it is controlled (give [on_input] too); without, the browser
    keeps what is typed. [invalid] sets [aria-invalid="true"], which draws
    it in the destructive colour. *)
val view
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?type_:Type.t
  -> ?id:string
  -> ?name:string
  -> ?placeholder:string
  -> ?disabled:bool
  -> ?required:bool
  -> ?invalid:bool
  -> ?value:string
  -> ?on_input:(string -> unit Effect.t)
  -> unit
  -> Node.t

(** Keeps the text, [default_value] (or empty) at first. *)
val component
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?type_:Type.t
  -> ?id:string
  -> ?name:string
  -> ?placeholder:string
  -> ?disabled:bool
  -> ?required:bool
  -> ?invalid:bool
  -> ?default_value:string
  -> local_ Bonsai.graph
  -> string Control.t Bonsai.t
