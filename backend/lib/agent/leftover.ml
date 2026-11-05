open! Core
open! Import

module Kind = struct
  type t =
    | Upload
    | Output
    | Worktree
  [@@deriving sexp, jsonaf, equal, compare]
end

type t =
  { host : string
  ; kind : Kind.t
  ; path : string
  ; root : string
  }
[@@deriving sexp, jsonaf, equal, compare]
