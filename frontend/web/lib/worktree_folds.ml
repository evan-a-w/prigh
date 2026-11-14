open! Core
open! Import

module Times = struct
  (* When, by session id. *)
  type t = Time_ns.t String.Map.t [@@deriving sexp_of, equal]

  (* [2026-10-05 10:00:00Z]: to the second. *)
  let to_string at =
    let at =
      Time_ns.of_span_since_epoch
        (Time_ns.Span.of_sec
           (Float.round_down
              (Time_ns.Span.to_sec (Time_ns.to_span_since_epoch at))))
    in
    String.substr_replace_first
      (Time_ns.to_string_utc at)
      ~pattern:".000000000Z"
      ~with_:"Z"
  ;;

  let to_pref t =
    if Map.is_empty t
    then None
    else
      Some
        (`Object
            (Map.to_alist t
             |> List.map ~f:(fun (id, at) -> id, `String (to_string at))))
  ;;

  let of_pref : Json.t option -> t = function
    | Some (`Object fields) ->
      List.filter_map fields ~f:(fun (id, at) ->
        match at with
        | `String at -> Option.map (Rel_time.parse at) ~f:(fun at -> id, at)
        | _ -> None)
      |> String.Map.of_alist_reduce ~f:Time_ns.max
    | _ -> String.Map.empty
  ;;

  (* Those of sessions still listed (all of them before the list came). *)
  let keep t ~listed =
    if Set.is_empty listed then t else Map.filter_keys t ~f:(Set.mem listed)
  ;;
end

type t =
  { hours : int
  ; folded : Times.t
  ; unfolded : Times.t
  ; opened : Times.t
  }
[@@deriving sexp_of, equal]

let default_hours = 12
let max_hours = 720

let empty =
  { hours = default_hours
  ; folded = String.Map.empty
  ; unfolded = String.Map.empty
  ; opened = String.Map.empty
  }
;;

let hours t = t.hours

let parse_hours text =
  let word = String.lowercase (String.strip text) in
  let number =
    List.find_map [ " hours"; "hours"; " hour"; "hour"; "h" ] ~f:(fun suffix ->
      String.chop_suffix word ~suffix)
    |> Option.value ~default:word
    |> String.strip
  in
  match word with
  | "off" | "never" -> Ok 0
  | _ ->
    (match Int.of_string_opt number with
     | Some n when n >= 0 && n <= max_hours -> Ok n
     | Some _ | None ->
       Error
         (sprintf
            "“%s” isn't a number of hours from 0 to %d: type one like 12, or \
             off to fold every session's worktrees."
            (String.strip text)
            max_hours))
;;

let hours_string = function
  | 0 -> "off"
  | 1 -> "1 hour"
  | n -> sprintf "%d hours" n
;;

module Current = struct
  type t =
    { id : string
    ; cwd : string
    }
end

let later a b =
  match a, b with
  | Some a, Some b -> Some (Time_ns.max a b)
  | Some x, None | None, Some x -> Some x
  | None, None -> None
;;

module Use = struct
  (* A session's last use, and when the user last went into it (or into a
     session working in one of its worktrees). *)
  type t =
    { used : Time_ns.t option
    ; opened : Time_ns.t option
    }

  let none = { used = None; opened = None }

  let merge a b =
    { used = later a.used b.used; opened = later a.opened b.opened }
  ;;
end

(* The sessions whose worktrees hold [dir]: [owners] by worktree path, up
   from [dir] ([Session_worktree.contains]). *)
let rec owners_of owners dir =
  let here = Map.find_multi owners dir in
  match Filename.dirname dir with
  | parent when String.equal parent dir -> here
  | parent -> here @ owners_of owners parent
;;

let uses t ~now ~(current : Current.t option) sessions =
  let is_current (s : Session_summary.t) =
    Option.exists current ~f:(fun c -> String.equal c.id s.id)
  in
  let own (s : Session_summary.t) =
    let opened = Map.find t.opened s.id in
    let changed =
      Rel_time.parse (Option.value s.updated_at ~default:s.created_at)
    in
    let busy = if s.running || is_current s then now else None in
    { Use.used = later (later changed opened) busy; opened }
  in
  let owners =
    List.fold
      sessions
      ~init:String.Map.empty
      ~f:(fun owners (s : Session_summary.t) ->
        List.fold
          s.worktrees
          ~init:owners
          ~f:(fun owners (w : Session_worktree.t) ->
            Map.add_multi owners ~key:w.path ~data:s.id))
  in
  List.fold
    sessions
    ~init:String.Map.empty
    ~f:(fun uses (s : Session_summary.t) ->
      let use = own s in
      let cwd =
        match current with
        | Some c when is_current s -> c.cwd
        | _ -> s.cwd
      in
      List.fold (s.id :: owners_of owners cwd) ~init:uses ~f:(fun uses id ->
        Map.update uses id ~f:(fun before ->
          Use.merge use (Option.value before ~default:Use.none))))
;;

let shown t ~now ~current sessions =
  let uses = uses t ~now ~current sessions in
  List.filter sessions ~f:(fun (s : Session_summary.t) ->
    let use = Option.value (Map.find uses s.id) ~default:Use.none in
    (* A choice made since the session was last opened. *)
    let chosen choices =
      Option.filter (Map.find choices s.id) ~f:(fun at ->
        Option.for_all use.opened ~f:(fun opened -> Time_ns.( >= ) at opened))
    in
    match chosen t.folded, chosen t.unfolded with
    | Some folded, Some unfolded -> Time_ns.( > ) unfolded folded
    | Some _, None -> false
    | None, Some _ -> true
    | None, None ->
      t.hours > 0
      && (Session_list.is_pinned s
          ||
          match now, use.used with
          | None, _ -> true
          | Some _, None -> false
          | Some now, Some used ->
            Time_ns.Span.( <= )
              (Time_ns.diff now used)
              (Time_ns.Span.of_hr (Float.of_int t.hours))))
  |> List.map ~f:(fun (s : Session_summary.t) -> s.id)
  |> String.Set.of_list
;;

let toggle t ~now ~listed ~shown id =
  let at = Option.value now ~default:Time_ns.epoch in
  let add choices = Map.set (Times.keep choices ~listed) ~key:id ~data:at in
  let remove choices = Map.remove (Times.keep choices ~listed) id in
  if shown
  then { t with folded = add t.folded; unfolded = remove t.unfolded }
  else { t with folded = remove t.folded; unfolded = add t.unfolded }
;;

let open_ t ~now ~listed id =
  { t with opened = Map.set (Times.keep t.opened ~listed) ~key:id ~data:now }
;;

let folded_pref t = Times.to_pref t.folded
let unfolded_pref t = Times.to_pref t.unfolded
let opened_pref t = Times.to_pref t.opened

let hours_pref t =
  if t.hours = default_hours then None else Some (Json.int t.hours)
;;

let set_folded t value = { t with folded = Times.of_pref value }
let set_unfolded t value = { t with unfolded = Times.of_pref value }

let merge_opened t value =
  { t with
    opened =
      Map.merge_skewed
        t.opened
        (Times.of_pref value)
        ~combine:(fun ~key:_ a b -> Time_ns.max a b)
  }
;;

let opened_ahead t value =
  let theirs = Times.of_pref value in
  Map.existsi t.opened ~f:(fun ~key ~data ->
    match Map.find theirs key with
    | Some at -> Time_ns.( > ) data at
    | None -> true)
;;

let set_hours t hours =
  { t with hours = Int.clamp_exn hours ~min:0 ~max:max_hours }
;;

let set_hours_pref t (value : Json.t option) =
  match value with
  | Some (`Number n) ->
    (match Float.of_string_opt n with
     | Some h when Float.is_finite h -> set_hours t (Float.iround_nearest_exn h)
     | _ -> { t with hours = default_hours })
  | _ -> { t with hours = default_hours }
;;
