open! Core
open! Import

(** The file browser (a panel next to the chat) and the files attached to the
    next prompt, on the session's active tool host. Pure: [update] and
    [reply] return what to do as [Out.t]s, which [App] turns into requests
    ([fs_list], [fs_preview], [fs_mkdir], [fs_rename], [fs_delete],
    [fs_download], [fs_upload]), toasts and [Page.t] commands. The page keeps
    the browser's [File]s, by the ids it gives them, and moves their bytes to
    the upload links ([PUT], with progress) and from the download links. *)

module Dest : sig
  (** Where chosen files go. *)
  type t =
    | Here (** the directory the panel shows *)
    | Chat (** the uploads directory, attached to the next prompt *)
  [@@deriving sexp_of, equal]
end

module Local_file : sig
  (** A file the user picked or dropped, kept by the page. *)
  type t =
    { file : int (** the page's id for it *)
    ; name : string
    ; size : int
    }
  [@@deriving sexp_of, equal]
end

module Sort : sig
  module Key : sig
    type t =
      | Name
      | Size
      | Modified
    [@@deriving sexp_of, equal, enumerate]

    val label : t -> string
  end

  (** Directories always come first. *)
  type t =
    { key : Key.t
    ; descending : bool
    }
  [@@deriving sexp_of, equal]
end

module Preview : sig
  type t =
    | Loading of string (** the path *)
    | Shown of File_preview.t
    | Failed of
        { path : string
        ; error : string
        }
  [@@deriving sexp_of, equal]
end

module Upload : sig
  (** A file going into the panel's directory. *)
  type t =
    { local : Local_file.t
    ; dir : string
    ; sent : int option (** bytes, once the transfer started *)
    }
  [@@deriving sexp_of, equal]
end

module Attachment : sig
  module Status : sig
    type t =
      | Uploading of int option (** bytes sent, once started *)
      | Ready of string (** its path on the host *)
      | Failed of string
    [@@deriving sexp_of, equal]
  end

  (** A chip in the composer. *)
  type t =
    { file : int option (** the page's file, for an upload *)
    ; name : string
    ; size : int option
    ; status : Status.t
    }
  [@@deriving sexp_of, equal]
end

module Ask : sig
  (** The panel's dialogs: they own the keyboard; Esc closes them without
      doing anything. *)
  type t =
    | New_folder of
        { dir : string
        ; input : string
        ; error : string option
        ; busy : bool
        }
    | Rename of
        { path : string
        ; input : string
        ; error : string option
        ; busy : bool
        }
    | Delete of
        { path : string
        ; dir : bool
        ; error : string option
        ; busy : bool
        }
    | Overwrite of Upload.t (** the upload's name exists there *)
  [@@deriving sexp_of, equal]
end

module Where : sig
  (** What the panel belongs to: the session, its host and its cwd. *)
  type t =
    { session : string
    ; host : string (** the active host's id *)
    ; host_name : string
    ; cwd : string
    }
  [@@deriving sexp_of, equal]
end

type t =
  { open_ : bool
  ; base : string
    (** the backend's web server ([http://host:port]) for transfer links;
        empty: the page's own origin *)
  ; where : Where.t option
  ; listing : File_listing.t option
  ; loading : string option (** the path asked for (empty: the cwd) *)
  ; error : string option (** the listing failed: why, and what to do *)
  ; editing_path : string option (** the path field, while typing one *)
  ; hidden : bool (** dot files shown *)
  ; sort : Sort.t
  ; selected : string option (** an entry's name *)
  ; preview : Preview.t option
  ; uploads : Upload.t list
  ; conflicts : Upload.t list (** waiting for the user, after [ask] *)
  ; ask : Ask.t option
  ; attachments : Attachment.t list
  ; busy : string option (** what the panel is doing, acknowledged at once *)
  }
[@@deriving sexp_of]

val closed : t

module Page : sig
  (** What the page does for us. *)
  type t =
    | Pick of Dest.t (** the file picker *)
    | Upload of
        { file : int
        ; url : string
        } (** [PUT] the file, reporting [Upload_progress], [Upload_done] *)
    | Abort_upload of int
    | Forget of int (** the file will not be sent *)
    | Download of
        { url : string
        ; name : string
        }
    | Focus of string (** an element's id, once rendered *)
  [@@deriving sexp_of, equal]
end

module Reply : sig
  (** Why we called the backend. *)
  type t =
    | Listed of string (** the path asked for *)
    | Previewed of string
    | Upload_link of
        { file : int
        ; overwrite : bool
        }
    | Download_link
    | Changed of
        { notice : string
        ; select : string option
        } (** a mkdir, rename or delete: the listing again *)
  [@@deriving sexp_of, equal]
end

module Out : sig
  type t =
    | Rpc of
        { method_ : string
        ; params : (string * Json.t) list
        ; reply : Reply.t
        }
    | Page of Page.t
    | Toast of
        { text : string
        ; error : bool
        }
    | Copy of string
    | Insert of string (** into the composer, at its end *)
    | Focus_editor
  [@@deriving sexp_of]
end

module Action : sig
  type t =
    | Set_base of string (** the backend's address, from the page *)
    | Toggle
    | Open of string option (** at a path; [None]: where it was, or the cwd *)
    | Close
    | Go of string (** a directory: absolute, [~/...] or relative to it *)
    | Up
    | Refresh
    | Edit_path of string option (** the path field opened, typed, closed *)
    | Submit_path
    | Toggle_hidden
    | Sort_by of Sort.Key.t (** again: the other direction *)
    | Select of string (** clicked: a directory opens, a file shows *)
    | Move of int (** the selection, by rows *)
    | Open_selected
    | Close_preview
    | Download of string (** an entry's name; a directory as an archive *)
    | Pick of Dest.t
    | Chosen of
        { dest : Dest.t
        ; files : Local_file.t list
        } (** picked or dropped *)
    | Upload_progress of
        { file : int
        ; sent : int
        }
    | Upload_done of
        { file : int
        ; result : (Json.t, string) Result.t
        }
    | New_folder
    | Rename of string
    | Delete of string
    | Ask_input of string
    | Ask_accept
    | Ask_cancel
    | Copy_path of string
    | Insert_path of string
    | Attach of string (** an entry's name *)
    | Remove_attachment of int (** by its place in the list *)
  [@@deriving sexp_of]
end

(** The panel's keys (with the focus in its list): ↑↓ select, Enter opens,
    Backspace goes up, Delete deletes, F2 renames, Esc closes the preview or
    the panel. With a dialog open, Enter accepts and Esc cancels. *)
val key : t -> key:string -> shift:bool -> ctrl:bool -> alt:bool -> Action.t option

(** Ctrl+Shift+F (by the key's position). *)
val toggle_key : code:string -> shift:bool -> ctrl:bool -> alt:bool -> meta:bool -> bool

val update : t -> Action.t -> t * Out.t list
val reply : t -> Reply.t -> (Json.t, string) Result.t -> t * Out.t list

(** The session, host or cwd changed: a new session or host drops the
    attachments (their paths were on the old one); an open panel lists the
    new cwd. *)
val set_where : t -> Where.t option -> t * Out.t list

(** A dialog is open: it has the keyboard. *)
val asking : t -> bool

(** The paths to send with the prompt, or why it cannot be sent yet. *)
val prompt_attachments : t -> string list Or_error.t

(** The attachments were sent. *)
val sent : t -> t

(** Back after a prompt failed. *)
val restore : t -> Attachment.t list -> t

(** The entries as shown: hidden ones filtered by the backend, sorted. *)
val entries : t -> File_listing.Entry.t list

(** A path as the user reads it: [~/...] under the host's home. *)
val display_path : t -> string -> string

(** [path] made absolute against the directory shown. *)
val resolve : t -> string -> string

(** The link the page uses for a backend path ([/files/...]). *)
val link : t -> string -> string

(** [ws://h:p/ws?...] → [http://h:p] ([wss] → [https]). *)
val http_base : string -> string
