open! Core

external clear_iexten
  :  Core_unix.File_descr.t
  -> unit
  = "prigh_tty_clear_iexten"
