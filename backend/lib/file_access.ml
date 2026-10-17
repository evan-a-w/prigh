open! Core
open! Import

type t =
  { read : string -> string
  ; write : string -> data:string -> unit
  ; is_local : bool
  }

let local =
  { read = (fun path -> In_channel.read_all path)
  ; write = (fun path ~data -> Out_channel.write_all path ~data)
  ; is_local = true
  }
;;

let create ~read ~write = { read; write; is_local = false }
let is_local t = t.is_local
let read t path = t.read path
let write t path ~data = t.write path ~data
