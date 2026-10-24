open! Core

external set_iexten
  :  Core_unix.File_descr.t
  -> bool
  -> bool
  = "prigh_tty_set_iexten"
