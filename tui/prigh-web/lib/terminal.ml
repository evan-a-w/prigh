open! Core
open! Import

module Status = struct
  type t =
    | Connecting
    | Connected
    | Reconnecting
    | Exited
    | Failed of string
  [@@deriving sexp_of, equal]
end

module Target = struct
  type t =
    { session : string
    ; host : string
    ; online : bool
    ; as_user : string option
    ; generation : int
    }
  [@@deriving sexp_of, equal]

  let key t = Sexp.to_string (sexp_of_t t)
end

type t =
  { open_ : bool
  ; height : int option
  ; generation : int
  ; status : (string * Status.t) option
  }
[@@deriving sexp_of]

let closed = { open_ = false; height = None; generation = 0; status = None }
let min_height = 120

let active_host (state : State.t) =
  List.find state.hosts ~f:(fun h -> String.equal h.id state.active_host)
;;

let target t (state : State.t) ~hello =
  { Target.session = state.session_id
  ; host = state.active_host
  ; online = Option.is_some (active_host state)
  ; as_user = Option.bind hello ~f:Hello_reply.acting_as
  ; generation = t.generation
  }
;;

let status t target =
  match t.status with
  | Some (key, status) when String.equal key (Target.key target) -> status
  | _ -> Connecting
;;

let where (state : State.t) =
  match active_host state with
  | Some host -> host.name, host.cwd
  | None ->
    let name =
      if String.is_empty state.active_host
      then "no tool host"
      else state.active_host ^ " (offline)"
    in
    name, state.cwd
;;

let advice message =
  let has s = String.is_substring message ~substring:s in
  if has "unauthorised"
  then "Sign in again (the account menu, or /signout), then reopen it."
  else if
    has "not connected" || has "no tool host" || has "tool host is disabled"
  then "Pick a connected tool host with /host, or Retry once it is back."
  else if has "not found"
  then
    "The shell needs tmux on the tool host: install it there (or point \
     $PRIGH_TMUX at it) and Retry, or pick another host with /host."
  else "Retry, or pick another tool host with /host."
;;
