open! Core
open! Import

type t =
  { named_only : bool
  ; sessions : P.Session_summary.t list
  }
[@@deriving sexp_of, equal]

let create ?(named_only = false) sessions = { named_only; sessions }
let title t = if t.named_only then "Sessions (named)" else "Sessions"

let shown t =
  if t.named_only
  then List.filter t.sessions ~f:(fun s -> Option.is_some s.name)
  else t.sessions
;;

let item ~current (s : P.Session_summary.t) =
  let name = Option.value s.name ~default:"(unnamed)" in
  let date = String.prefix (Option.value s.updated_at ~default:"") 16 in
  let first = Text_width.truncate (P.Session_summary.blurb s) ~width:60 in
  let background =
    if s.background > 0 then sprintf " ∣ %d in background" s.background else ""
  in
  Picker.Item.create
    ~id:s.path
    ~detail:s.cwd
    ~search:(name ^ " " ^ first ^ " " ^ s.cwd ^ " " ^ date)
    ~marked:(Option.value_map current ~default:false ~f:(String.equal s.path))
    ~dimmed:s.archived
    ~section:(if s.archived then "archived" else "")
    (sprintf
       "%s%s ∣ %s ∣ %d msgs%s ∣ %s"
       (if s.running then "▶ " else if s.live then "● " else "")
       name
       date
       s.message_count
       background
       first)
;;

let picker ?previous ?select t ~current =
  let items = List.map (shown t) ~f:(item ~current) in
  match previous with
  | None -> Picker.create ~title:(title t) items
  | Some previous -> Picker.set_items ~title:(title t) ?select previous items
;;

let find t path =
  List.find t.sessions ~f:(fun (s : P.Session_summary.t) ->
    String.equal s.path path)
;;

let targets t picker =
  match
    List.filter t.sessions ~f:(fun s -> Set.mem (Picker.checked picker) s.path)
  with
  | [] ->
    Option.to_list
      (Option.bind (Picker.selected_item picker) ~f:(fun item -> find t item.id))
  | marked -> marked
;;

let set_archived t ~path ~archived =
  { t with
    sessions =
      List.map t.sessions ~f:(fun s ->
        if String.equal s.path path then { s with archived } else s)
  }
;;

let remove t ~paths =
  { t with
    sessions =
      List.filter t.sessions ~f:(fun s ->
        not (List.mem paths s.path ~equal:String.equal))
  }
;;

let archived t = List.filter t.sessions ~f:(fun s -> s.archived)

let describe (s : P.Session_summary.t) =
  match s.name with
  | Some name -> sprintf "session %S" name
  | None -> "session " ^ s.id
;;

let size sessions =
  P.Session_summary.size_string (P.Session_summary.total_bytes sessions)
;;

let delete_question = function
  | [ s ] ->
    sprintf
      "Delete %s (%s)? Enter deletes, Esc keeps it"
      (describe s)
      (size [ s ])
  | sessions ->
    sprintf
      "Delete %d sessions (%s)? Enter deletes, Esc keeps them"
      (List.length sessions)
      (size sessions)
;;

let delete_archived_question t =
  match archived t with
  | [] -> None
  | [ s ] ->
    Some
      (sprintf
         "Delete the archived %s (%s)? Enter deletes, Esc keeps it"
         (describe s)
         (size [ s ]))
  | archived ->
    Some
      (sprintf
         "Delete all %d archived sessions (%s)? Enter deletes, Esc keeps them"
         (List.length archived)
         (size archived))
;;

let marked_summary t picker =
  match
    List.filter t.sessions ~f:(fun s -> Set.mem (Picker.checked picker) s.path)
  with
  | [] -> None
  | marked -> Some (sprintf "%d marked (%s)" (List.length marked) (size marked))
;;

let resolve sessions args =
  let resolve_one arg =
    let exact =
      List.filter sessions ~f:(fun (s : P.Session_summary.t) ->
        String.equal s.path arg
        || String.equal s.id arg
        || Option.exists s.name ~f:(String.equal arg))
    in
    let candidates =
      match exact with
      | [] ->
        List.filter sessions ~f:(fun (s : P.Session_summary.t) ->
          String.is_prefix s.id ~prefix:arg)
      | exact -> exact
    in
    match candidates with
    | [ s ] -> Ok s
    | [] ->
      Error
        (sprintf
           "no session matches %S: /sessions lists them, or Tab after the \
            command completes them"
           arg)
    | several ->
      Error
        (sprintf
           "%S matches %d sessions (%s): give more of the id"
           arg
           (List.length several)
           (String.concat
              ~sep:", "
              (List.map several ~f:(fun (s : P.Session_summary.t) -> s.id))))
  in
  Result.all (List.map args ~f:resolve_one)
  |> Result.map ~f:(fun found ->
    List.fold found ~init:[] ~f:(fun acc (s : P.Session_summary.t) ->
      if
        List.exists acc ~f:(fun (o : P.Session_summary.t) ->
          String.equal o.path s.path)
      then acc
      else s :: acc)
    |> List.rev)
;;
