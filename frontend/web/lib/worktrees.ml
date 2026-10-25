open! Core
open! Import

type t =
  { place : string option (** of the newest request; [None]: to list *)
  ; session : string option
  ; listing : Worktree.Listing.t option
  ; loading : (int * bool) option (** the request, and whether [quiet] *)
  ; quiet : bool (** the next request is a check in the background *)
  ; seq : int
  ; entering : string option
  }
[@@deriving sexp_of]

let empty =
  { place = None
  ; session = None
  ; listing = None
  ; loading = None
  ; quiet = false
  ; seq = 0
  ; entering = None
  }
;;

let reset t = { empty with seq = t.seq }

let place (state : State.t option) =
  Option.map state ~f:(fun s ->
    String.concat ~sep:"\n" [ s.session_id; s.active_host; s.cwd ])
;;

let wanted t state =
  match place state with
  | Some p when not (Option.equal String.equal t.place (Some p)) -> Some p
  | Some _ | None -> None
;;

let requested t (state : State.t) =
  let seq = t.seq + 1 in
  let same = Option.equal String.equal t.session (Some state.session_id) in
  ( { place = place (Some state)
    ; session = Some state.session_id
    ; listing = (if same then t.listing else None)
    ; loading = Some (seq, t.quiet)
    ; quiet = false
    ; seq
    ; entering = t.entering
    }
  , seq )
;;

let loaded t ~place ?seq listing =
  let newest = Option.for_all seq ~f:(Int.equal t.seq) in
  if Option.equal String.equal t.place (Some place) && newest
  then
    { t with
      listing = Some listing
    ; loading = (if Option.is_some seq then None else t.loading)
    }
  else t
;;

let failed t ~seq =
  if seq = t.seq then { t with listing = None; loading = None } else t
;;

let stale ?(quiet = false) t =
  let quiet = quiet && (Option.is_some t.place || t.quiet) in
  { t with place = None; quiet }
;;

let sessions_moved t =
  if Option.is_some t.listing && Option.is_none t.loading
  then stale ~quiet:true t
  else t
;;

let listing t = t.listing

let loading t =
  match t.loading with
  | Some (_, quiet) -> not quiet
  | None -> false
;;

let entering t = t.entering
let set_entering t entering = { t with entering }

let find t path =
  Option.bind t.listing ~f:(fun (l : Worktree.Listing.t) ->
    List.find l.worktrees ~f:(fun w -> String.equal w.path path))
;;

let force_label w =
  if Worktree.dirty w then "Discard changes and remove" else "Force remove"
;;
