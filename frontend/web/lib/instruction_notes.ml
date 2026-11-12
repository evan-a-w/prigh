open! Core
open! Import
module Status = Memory_notes.Status
module Draft = Memory_notes.Draft

type t =
  { status : Status.t
  ; instructions : Custom_instructions.t
  ; draft : Draft.t option
  ; removing : string option
  ; error : string option
  }
[@@deriving sexp_of, equal]

let empty =
  { status = Unknown
  ; instructions =
      { enabled = true
      ; notes = []
      ; limits = { text = 1000; title = 80; count = 50 }
      }
  ; draft = None
  ; removing = None
  ; error = None
  }
;;

let loading t =
  match t.status with
  | Loaded -> t
  | Unknown | Loading | Failed _ -> { t with status = Loading }
;;

let find (ci : Custom_instructions.t) id =
  List.find ci.notes ~f:(fun (n : Custom_instructions.Note.t) ->
    String.equal n.id id)
;;

let has ci id = Option.is_some (find ci id)

let changed t (instructions : Custom_instructions.t) =
  let draft =
    Option.filter t.draft ~f:(fun (d : Draft.t) ->
      Option.for_all d.id ~f:(has instructions))
  in
  let removing = Option.filter t.removing ~f:(has instructions) in
  { t with status = Loaded; instructions; draft; removing }
;;

let loaded t = function
  | Ok ci -> changed t ci
  | Error e -> { t with status = Failed e }
;;

let tokens t = Custom_instructions.tokens t.instructions.notes
let warn_tokens = 1500

let edit t id =
  let note = Option.bind id ~f:(find t.instructions) in
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
    ; removing = None
    }
;;

let map_draft t ~f = { t with draft = Option.map t.draft ~f }
let set_title t title = map_draft t ~f:(fun d -> { d with title; error = None })
let set_text t text = map_draft t ~f:(fun d -> { d with text; error = None })
let cancel t = { t with draft = None }

let save t =
  match t.draft with
  | None | Some { saving = true; _ } -> t, None
  | Some d ->
    let text = String.strip d.text in
    let limits = t.instructions.limits in
    let error =
      if String.is_empty text
      then Some "Write the instruction first, or Cancel."
      else if String.length text > limits.text
      then
        Some
          (sprintf
             "It is %d bytes and one can be at most %d: they are in every \
              session's context, so keep each to the point."
             (String.length text)
             limits.text)
      else if
        Option.is_none d.id && List.length t.instructions.notes >= limits.count
      then
        Some
          (sprintf
             "There are %d, the most there can be: remove or merge some first."
             limits.count)
      else None
    in
    (match error with
     | Some e -> map_draft t ~f:(fun d -> { d with error = Some e }), None
     | None ->
       let title = String.strip d.title in
       ( map_draft t ~f:(fun d -> { d with saving = true; error = None })
       , Some
           (Request.Method.Save_custom_instruction
              { id = d.id
              ; title = Option.some_if (not (String.is_empty title)) title
              ; text
              }) ))
;;

let saved t = function
  | Ok (_ : Custom_instructions.Note.t) -> { t with draft = None; error = None }
  | Error e ->
    map_draft t ~f:(fun d ->
      { d with saving = false; error = Some (String.capitalize e) })
;;

let ask_remove t id = { t with removing = Some id; draft = None }
let cancel_remove t = { t with removing = None }

let remove t =
  match t.removing with
  | None -> t, None
  | Some id ->
    ( { t with removing = None }
    , Some (Request.Method.Remove_custom_instruction { id }) )
;;

let removed t = function
  | Ok () -> { t with error = None }
  | Error e -> { t with error = Some (String.capitalize e) }
;;

let failed t e = { t with error = Some (String.capitalize e) }
