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

type t =
  { status : Status.t
  ; info : User_info.t
  ; draft : string option
  ; saving : bool
  ; saved : bool
  ; elsewhere : bool
  ; error : string option
  }
[@@deriving sexp_of, equal]

let empty =
  { status = Unknown
  ; info = { text = ""; max = 0 }
  ; draft = None
  ; saving = false
  ; saved = false
  ; elsewhere = false
  ; error = None
  }
;;

let loading t =
  match t.status with
  | Loaded -> t
  | Unknown | Loading | Failed _ -> { t with status = Loading }
;;

let same draft (info : User_info.t) =
  String.equal (String.strip draft) info.text
;;

let changed t (info : User_info.t) =
  let t = { t with status = Loaded; info } in
  match t.draft with
  | Some draft when not (same draft info) -> { t with elsewhere = not t.saving }
  | Some _ when t.saving -> t
  | Some _ | None -> { t with draft = None; elsewhere = false }
;;

let loaded t = function
  | Ok info -> changed t info
  | Error e -> { t with status = Failed e }
;;

let text t = Option.value t.draft ~default:t.info.text
let dirty t = Option.exists t.draft ~f:(fun draft -> not (same draft t.info))
let set_text t text = { t with draft = Some text; saved = false; error = None }
let revert t = { t with draft = None; elsewhere = false; error = None }

let save t =
  match t.draft with
  | None -> t, None
  | Some _ when t.saving -> t, None
  | Some draft ->
    let text = String.strip draft in
    let size = String.length text in
    if t.info.max > 0 && size > t.info.max
    then
      ( { t with
          error =
            Some
              (sprintf
                 "This is %d bytes and the limit is %d: shorten it, and keep \
                  what is only sometimes needed in memory notes."
                 size
                 t.info.max)
        }
      , None )
    else
      ( { t with saving = true; error = None }
      , Some (Request.Method.Set_user_info { text }) )
;;

let was_saved (t : t) = function
  | Ok (info : User_info.t) ->
    { status = Loaded
    ; info
    ; draft = None
    ; saving = false
    ; saved = true
    ; elsewhere = false
    ; error = None
    }
  | Error e -> { t with saving = false; error = Some e }
;;
