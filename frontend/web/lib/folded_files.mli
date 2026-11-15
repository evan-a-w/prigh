open! Core
open! Import

(** Changed files a reviewer rarely reads line by line, which a phone's
    review folds: generated ones (lock files, minified code, build output)
    and both halves of a pure rename (the diff has no renames: a file
    deleted and one added with the same text). *)

module Why : sig
  type t =
    | Generated
    | Renamed_to of string (** the deleted half: where the file went *)
    | Renamed_from of string (** the added half: where it came from *)
  [@@deriving sexp_of, equal]

  (** A short label: "generated", "renamed". *)
  val label : t -> string
end

(** Lock files ([*.lock], [package-lock.json], [pnpm-lock.yaml],
    [*.opam.locked], ...), minified code and source maps, and anything
    under [_build/], [node_modules/], [dist/] or [vendor/]. *)
val generated : string -> bool

val classify : Prigh_ui.Diff_files.File.t list -> Why.t String.Map.t
