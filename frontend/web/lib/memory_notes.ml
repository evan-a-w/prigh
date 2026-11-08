open! Core
open! Import

module Status = struct
  type t =
    | Unknown
    | Loading
    | Loaded
    | Failed of string
  [@@deriving sexp_of, equal]
end

module Draft = struct
  type t =
    { id : string option
    ; title : string
    ; text : string
    ; saving : bool
    ; error : string option
    }
  [@@deriving sexp_of, equal]
end

type t =
  { status : Status.t
  ; memory : Memory.t
  ; filter : string
  ; draft : Draft.t option
  ; forgetting : string option
  ; error : string option
  }
[@@deriving sexp_of, equal]

let empty =
  { status = Unknown
  ; memory = { enabled = true; notes = [] }
  ; filter = ""
  ; draft = None
  ; forgetting = None
  ; error = None
  }
;;

let loading t =
  match t.status with
  | Loaded -> t
  | Unknown | Loading | Failed _ -> { t with status = Loading }
;;

let has (memory : Memory.t) id =
  List.exists memory.notes ~f:(fun (n : Memory.Note.t) -> String.equal n.id id)
;;

let changed t (memory : Memory.t) =
  let draft =
    Option.filter t.draft ~f:(fun (d : Draft.t) ->
      Option.for_all d.id ~f:(has memory))
  in
  let forgetting = Option.filter t.forgetting ~f:(has memory) in
  { t with status = Loaded; memory; draft; forgetting }
;;

let loaded t = function
  | Ok memory -> changed t memory
  | Error e -> { t with status = Failed e }
;;

let visible t =
  let newest =
    List.rev
      (List.stable_sort t.memory.notes ~compare:(fun (a : Memory.Note.t) b ->
         String.compare a.saved b.saved))
  in
  match String.strip t.filter with
  | "" -> newest
  | query ->
    Prigh_ui.Fuzzy.rank ~query newest ~key:(fun (n : Memory.Note.t) ->
      Option.value_map n.title ~default:n.text ~f:(fun title ->
        title ^ " " ^ n.text))
;;

let set_filter t filter = { t with filter }

let edit t id =
  let note =
    Option.bind id ~f:(fun id ->
      List.find t.memory.notes ~f:(fun (n : Memory.Note.t) ->
        String.equal n.id id))
  in
  match id, note with
  | Some _, None -> t
  | _ ->
    { t with
      draft =
        Some
          { id
          ; title =
              Option.value (Option.bind note ~f:(fun n -> n.title)) ~default:""
          ; text = Option.value_map note ~default:"" ~f:(fun n -> n.text)
          ; saving = false
          ; error = None
          }
    ; forgetting = None
    }
;;

let map_draft t ~f = { t with draft = Option.map t.draft ~f }
let set_title t title = map_draft t ~f:(fun d -> { d with title; error = None })
let set_text t text = map_draft t ~f:(fun d -> { d with text; error = None })
let cancel t = { t with draft = None }

let save t =
  match t.draft with
  | None -> t, None
  | Some { saving = true; _ } -> t, None
  | Some d ->
    let text = String.strip d.text in
    if String.is_empty text
    then
      ( map_draft t ~f:(fun d ->
          { d with error = Some "Write the note first, or Cancel." })
      , None )
    else (
      let title = String.strip d.title in
      ( map_draft t ~f:(fun d -> { d with saving = true; error = None })
      , Some
          (Request.Method.Save_memory
             { id = d.id
             ; title = Option.some_if (not (String.is_empty title)) title
             ; text
             }) ))
;;

let saved t = function
  | Ok (_ : Memory.Note.t) -> { t with draft = None; error = None }
  | Error e ->
    map_draft t ~f:(fun d ->
      { d with saving = false; error = Some (String.capitalize e) })
;;

let ask_forget t id = { t with forgetting = Some id; draft = None }
let cancel_forget t = { t with forgetting = None }

let forget t =
  match t.forgetting with
  | None -> t, None
  | Some id ->
    { t with forgetting = None }, Some (Request.Method.Forget_memory { id })
;;

let forgotten t = function
  | Ok () -> { t with error = None }
  | Error e -> { t with error = Some (String.capitalize e) }
;;

let failed t e = { t with error = Some (String.capitalize e) }
