open! Core
open! Import

module Source = struct
  type t =
    | Command
    | Argument of Commands.Spec.t
    | Path
  [@@deriving sexp_of, equal]
end

type t =
  { source : Source.t
  ; prefix : string
  ; line : int
  ; start : int (* byte offset of [prefix] within [line] *)
  ; items : Picker.Item.t list
  ; selected : int
  ; navigated : bool
  ; dirs_only : bool
  }
[@@deriving sexp_of]

let source t = t.source
let prefix t = t.prefix
let items t = t.items
let selected t = t.selected
let navigated t = t.navigated

let set_items t items =
  let items =
    if t.dirs_only
    then
      List.filter items ~f:(fun (i : Picker.Item.t) ->
        String.is_suffix i.id ~suffix:"/")
    else items
  in
  { t with items; selected = 0 }
;;

let accepts_on_enter t =
  match t.source with
  | Source.Command -> true
  | Argument _ | Path -> t.navigated || not (String.is_empty t.prefix)
;;

let clamp t =
  let n = List.length t.items in
  { t with selected = Int.max 0 (Int.min t.selected (n - 1)) }
;;

let up t = clamp { t with selected = t.selected - 1; navigated = true }
let down t = clamp { t with selected = t.selected + 1; navigated = true }
let selected_item t = List.nth t.items t.selected

let rank ~prefix items =
  Fuzzy.rank ~query:prefix items ~key:(fun i -> i.Picker.Item.search)
;;

let command_items ~prefix =
  Commands.all
  |> Fuzzy.rank ~query:prefix ~key:(fun (c : Commands.Spec.t) -> c.name)
  |> List.map ~f:(fun (c : Commands.Spec.t) ->
    Picker.Item.create ~id:c.name ~detail:c.help ~search:c.name c.name)
;;

let has_whitespace s = String.exists s ~f:Char.is_whitespace

let compute_command ~line ~line_index =
  match String.chop_prefix line ~prefix:"/" with
  | Some rest when not (has_whitespace rest) ->
    let prefix = rest in
    let items = command_items ~prefix in
    Option.some_if
      (not (List.is_empty items))
      { source = Source.Command
      ; prefix
      ; line = line_index
      ; start = 1
      ; items
      ; selected = 0
      ; navigated = false
      ; dirs_only = false
      }
  | _ -> None
;;

let thinking_items () =
  List.map Commands.thinking_levels ~f:(fun level ->
    Picker.Item.create ~id:level level)
;;

let verbosity_items () =
  List.map [ "quiet"; "normal"; "verbose" ] ~f:(fun name ->
    Picker.Item.create ~id:name (String.capitalize name))
;;

let confirm_items () =
  List.map [ "on"; "off" ] ~f:(fun name ->
    Picker.Item.create ~id:name (String.capitalize name))
;;

let provider_items (auth : P.Auth_status.t list) =
  List.map auth ~f:(fun (s : P.Auth_status.t) ->
    Picker.Item.create
      ~id:s.provider
      ~detail:s.provider
      ~search:(s.name ^ " " ^ s.provider)
      s.name)
;;

let model_items ~logged_in (models : P.Model.t list) =
  List.map models ~f:(fun (model : P.Model.t) ->
    Picker.Item.create
      ~id:model.key
      ~detail:model.key
      ~search:(model.name ^ " " ^ model.key)
      ~dimmed:(not (logged_in model.provider))
      model.name)
;;

let session_items (sessions : P.Session_summary.t list) =
  List.map sessions ~f:(fun (s : P.Session_summary.t) ->
    let first =
      Option.value_map s.first_prompt ~default:"(empty)" ~f:(fun p ->
        Text_width.truncate
          (String.concat ~sep:" " (String.split_lines p))
          ~width:60)
    in
    Picker.Item.create
      ~id:s.path
      ~detail:s.cwd
      ~search:(first ^ " " ^ s.path ^ " " ^ s.cwd ^ " " ^ s.created_at)
      first)
;;

let argument_items ~kind ~models ~auth ~sessions ~logged_in =
  match (kind : Commands.Argument.t) with
  | Model -> Some (model_items ~logged_in models)
  | Thinking -> Some (thinking_items ())
  | Verbosity -> Some (verbosity_items ())
  | Confirm -> Some (confirm_items ())
  | Login | Logout -> Some (provider_items auth)
  | Sessions -> Some (Option.value_map sessions ~default:[] ~f:session_items)
  | Path -> None
;;

let argument_start ~line ~name =
  let after = String.length name + 1 in
  let rec skip i =
    if i < String.length line && Char.is_whitespace line.[i]
    then skip (i + 1)
    else i
  in
  skip after
;;

let compute_argument ~line ~line_index ~models ~auth ~sessions ~logged_in =
  match String.chop_prefix line ~prefix:"/" with
  | None -> None
  | Some body ->
    (match String.lsplit2 body ~on:' ' with
     | None -> None
     | Some (name, rest) ->
       (match Commands.find name with
        | None -> None
        | Some spec ->
          (match spec.argument with
           | None -> None
           | Some kind ->
             let prefix = String.strip rest in
             let start = argument_start ~line ~name in
             (match kind with
              | Commands.Argument.Path ->
                (* Filled in asynchronously by the platform. *)
                Some
                  { source = Source.Path
                  ; prefix
                  ; line = line_index
                  ; start
                  ; items = []
                  ; selected = 0
                  ; navigated = false
                  ; dirs_only = String.equal spec.name "cd"
                  }
              | Sessions when Option.is_none sessions ->
                (* Loading; the app fetches the session list. *)
                Some
                  { source = Source.Argument spec
                  ; prefix
                  ; line = line_index
                  ; start
                  ; items = []
                  ; selected = 0
                  ; navigated = false
                  ; dirs_only = false
                  }
              | _ ->
                let items =
                  Option.value
                    (argument_items ~kind ~models ~auth ~sessions ~logged_in)
                    ~default:[]
                in
                let items = rank ~prefix items in
                Option.some_if
                  (not (List.is_empty items))
                  { source = Source.Argument spec
                  ; prefix
                  ; line = line_index
                  ; start
                  ; items
                  ; selected = 0
                  ; navigated = false
                  ; dirs_only = false
                  }))))
;;

let compute_at ~line ~col ~line_index =
  let before = String.prefix line col in
  let start = ref 0 in
  String.iteri before ~f:(fun i c ->
    if Char.is_whitespace c then start := i + 1);
  let after = String.drop_prefix line !start in
  let word =
    match String.lfindi after ~f:(fun _ c -> Char.is_whitespace c) with
    | Some j -> String.prefix after j
    | None -> after
  in
  match String.chop_prefix word ~prefix:"@" with
  | Some path ->
    Some
      { source = Source.Path
      ; prefix = path
      ; line = line_index
      ; start = !start + 1
      ; items = []
      ; selected = 0
      ; navigated = false
      ; dirs_only = false
      }
  | None -> None
;;

let compute ~line ~col ~line_index ~models ~auth ~sessions ~logged_in =
  match compute_command ~line ~line_index with
  | Some _ as t -> t
  | None ->
    (match
       compute_argument ~line ~line_index ~models ~auth ~sessions ~logged_in
     with
     | Some _ as t -> t
     | None -> compute_at ~line ~col ~line_index)
;;

let accept t ~editor_text =
  let lines = String.split editor_text ~on:'\n' in
  let line = Option.value (List.nth lines t.line) ~default:"" in
  let replacement =
    match selected_item t with
    | None -> t.prefix
    | Some item ->
      (match t.source with
       | Source.Command -> item.id ^ " "
       | Argument _ | Path -> item.id)
  in
  let before = String.prefix line t.start in
  let after = String.drop_prefix line (t.start + String.length t.prefix) in
  let line = before ^ replacement ^ after in
  String.concat
    ~sep:"\n"
    (List.mapi lines ~f:(fun i text -> if i = t.line then line else text))
;;
