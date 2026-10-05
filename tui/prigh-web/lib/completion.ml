open! Core
open! Import

module Source = struct
  type t =
    | Command
    | Argument of Slash.Argument.t
    | Path
  [@@deriving sexp_of, equal]
end

type t =
  { source : Source.t
  ; prefix : string
  ; start : int
  ; items : Picker.Item.t list
  ; selected : int
  }
[@@deriving sexp_of, equal]

let source t = t.source
let prefix t = t.prefix
let items t = t.items
let selected t = t.selected
let selected_item t = List.nth t.items t.selected

let rank ~prefix items =
  Prigh_ui.Fuzzy.rank ~query:prefix items ~key:(fun (i : Picker.Item.t) ->
    i.search)
;;

let command_items ~prefix =
  Prigh_ui.Fuzzy.rank ~query:prefix Slash.all ~key:(fun (s : Slash.Spec.t) ->
    s.name)
  |> List.map ~f:(fun (s : Slash.Spec.t) ->
    Picker.Item.create
      ~id:s.name
      ~detail:s.help
      ~search:s.name
      (String.strip ("/" ^ s.name ^ " " ^ s.args)))
;;

let logged_in (auth : Auth_status.t list) provider =
  List.exists auth ~f:(fun s ->
    String.equal s.provider provider && Option.is_some s.configured)
;;

let argument_items
      (kind : Slash.Argument.t)
      ~(models : Llm.t list)
      ~(auth : Auth_status.t list)
      ~current_model
      ~(sessions : Session_summary.t list)
      ~(hosts : Host.t list)
      ~users
  =
  let simple items =
    List.map items ~f:(fun (id, detail) -> Picker.Item.create ~id ~detail id)
  in
  match kind with
  | Model ->
    let usable (model : Llm.t) =
      List.is_empty auth || logged_in auth model.provider
    in
    List.stable_sort models ~compare:(fun a b ->
      Bool.compare (usable b) (usable a))
    |> List.map ~f:(fun model ->
      Picker.Item.create
        ~id:model.key
        ~detail:model.provider
        ~search:(model.name ^ " " ^ model.key)
        ~marked:(Option.equal String.equal current_model (Some model.key))
        ~dimmed:(not (usable model))
        model.name)
  | Thinking ->
    List.map Prigh_ui.Commands.thinking_levels ~f:(fun level ->
      Picker.Item.create ~id:level level)
  | Login ->
    List.map auth ~f:(fun s ->
      Picker.Item.create
        ~id:s.provider
        ~detail:
          (match s.custom, s.configured with
           | Some c, _ -> "custom · " ^ c.base_url
           | None, Some _ -> "logged in"
           | None, None -> "")
        ~search:(s.name ^ " " ^ s.provider)
        s.name)
    @ [ Picker.Item.create
          ~id:"custom"
          ~detail:"add an OpenAI-compatible endpoint"
          "custom"
      ]
  | Logout ->
    List.filter_map auth ~f:(fun s ->
      Option.map s.configured ~f:(fun c ->
        Picker.Item.create
          ~id:s.provider
          ~detail:
            (match s.custom with
             | Some custom -> "custom · " ^ custom.base_url
             | None -> sprintf "%s via %s" c.method_ c.source)
          ~search:(s.name ^ " " ^ s.provider)
          s.name))
  | Verbosity ->
    simple
      [ "quiet", "hide tool output and thinking"
      ; "normal", "tool output folded"
      ; "verbose", "everything unfolded"
      ]
  | Confirm ->
    simple
      [ "on", "ask before bash, write and edit"
      ; "off", "run tools without asking"
      ]
  | Session ->
    List.map sessions ~f:(fun s ->
      Picker.Item.create
        ~id:s.path
        ~detail:s.cwd
        ~search:(Session_list.title s ^ " " ^ s.path)
        (Session_list.title s))
  | Host ->
    List.map hosts ~f:(fun h ->
      Picker.Item.create
        ~id:h.name
        ~detail:h.cwd
        ~search:(h.name ^ " " ^ h.id)
        h.name)
  | User -> List.map users ~f:(fun u -> Picker.Item.create ~id:u u)
  | Directory | Path -> []
;;

let word_at text ~cursor =
  let cursor = Int.max 0 (Int.min cursor (String.length text)) in
  let start =
    match
      String.rfindi (String.prefix text cursor) ~f:(fun _ c ->
        Char.is_whitespace c)
    with
    | Some i -> i + 1
    | None -> 0
  in
  let stop =
    match
      String.lfindi text ~pos:cursor ~f:(fun _ c -> Char.is_whitespace c)
    with
    | Some i -> i
    | None -> String.length text
  in
  start, String.sub text ~pos:start ~len:(stop - start)
;;

let make source ~prefix ~start items =
  { source; prefix; start; items; selected = 0 }
;;

let compute
      ?(sessions = [])
      ?(hosts = [])
      ?(users = [])
      ~text
      ~cursor
      ~models
      ~auth
      ~current_model
      ()
  =
  let in_first_line = not (String.mem (String.prefix text cursor) '\n') in
  let slash =
    match String.chop_prefix text ~prefix:"/" with
    | Some body when in_first_line && not (String.mem body '\n') ->
      (match String.lsplit2 body ~on:' ' with
       | None when cursor >= 1 ->
         (match command_items ~prefix:body with
          | [] -> None
          | items -> Some (make Command ~prefix:body ~start:1 items))
       | Some (name, rest) when cursor > String.length name ->
         (match Slash.find name with
          | Some { argument = Some kind; _ } ->
            let prefix = String.strip rest in
            let start =
              String.length text - String.length (String.lstrip rest)
            in
            (match kind with
             | Directory | Path -> Some (make (Argument kind) ~prefix ~start [])
             | _ ->
               (match
                  rank
                    ~prefix
                    (argument_items
                       kind
                       ~models
                       ~auth
                       ~current_model
                       ~sessions
                       ~hosts
                       ~users)
                with
                | [] -> None
                | items -> Some (make (Argument kind) ~prefix ~start items)))
          | _ -> None)
       | _ -> None)
    | _ -> None
  in
  match slash with
  | Some _ -> slash
  | None ->
    let start, word = word_at text ~cursor in
    (match String.chop_prefix word ~prefix:"@" with
     | Some prefix -> Some (make Path ~prefix ~start:(start + 1) [])
     | None -> None)
;;

let same a b = Source.equal a.source b.source && String.equal a.prefix b.prefix

let request t =
  match t.source with
  | Path -> Some ("list_paths", t.prefix)
  | Argument Directory -> Some ("list_dirs", t.prefix)
  | Argument Path -> Some ("list_paths", t.prefix)
  | Command | Argument _ -> None
;;

let set_results t ~prefix results =
  if not (String.equal prefix t.prefix)
  then t
  else (
    let items =
      List.map results ~f:(fun path -> Picker.Item.create ~id:path path)
      |> fun items ->
      match t.source with
      | Path -> rank ~prefix items
      | _ -> items
    in
    { t with items; selected = 0 })
;;

let move t delta =
  let n = List.length t.items in
  { t with selected = Int.max 0 (Int.min (n - 1) (t.selected + delta)) }
;;

let accept t ~text =
  match selected_item t with
  | None -> text, String.length text
  | Some item ->
    let replacement =
      match t.source with
      | Command -> item.id ^ " "
      | Path when not (String.is_suffix item.id ~suffix:"/") -> item.id ^ " "
      | Path | Argument _ -> item.id
    in
    let before = String.prefix text t.start in
    let after = String.drop_prefix text (t.start + String.length t.prefix) in
    ( before ^ replacement ^ after
    , String.length before + String.length replacement )
;;
