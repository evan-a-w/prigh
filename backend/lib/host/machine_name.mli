open! Core

(** What this machine is called in prigh: [$PRIGH_HOST_NAME] if set and not
    empty, else the hostname. The backend's own tool host goes by it, and
    [prigh tool-host] and [prigh web] without [-name]. *)
val get : unit -> string
