open! Core
open! Import

(** Claude Code, driven over [claude] stream-json and its control protocol
    ({!Claude_codec}). *)

include Adapter.S
