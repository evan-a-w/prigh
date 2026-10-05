open! Core
open! Import

module Purpose = struct
  type t =
    | Login
    | Logout
  [@@deriving sexp_of, equal]
end

type t =
  { provider : string
  ; purpose : Purpose.t
  ; url : (string * string) option
  ; progress : string list
  ; prompt : (string * Auth_event.Prompt.t) option
  ; input : string
  ; selected : int
  ; failed : string option
  }
[@@deriving sexp_of, equal]

let start ?(purpose = Purpose.Login) provider =
  { provider
  ; purpose
  ; url = None
  ; progress = []
  ; prompt = None
  ; input = ""
  ; selected = 0
  ; failed = None
  }
;;

let apply t (event : Auth_event.t) =
  match event with
  | Auth_url { url; instructions } -> { t with url = Some (url, instructions) }
  | Prompt { id; prompt } ->
    let input =
      match prompt with
      | Text { default; _ } -> default
      | Secret _ | Manual_code _ | Select _ -> ""
    in
    { t with prompt = Some (id, prompt); input; selected = 0 }
  | Prompt_cancelled { id } ->
    (match t.prompt with
     | Some (current, _) when String.equal current id ->
       { t with prompt = None; input = "" }
     | _ -> t)
  | Progress message -> { t with progress = t.progress @ [ message ] }
  | Failed { error; _ } -> { t with failed = Some error; prompt = None }
  | Done _ | Logged_out _ -> t
;;

let options t =
  match t.prompt with
  | Some (_, Select { options; _ }) -> options
  | _ -> []
;;

let move t delta =
  let n = List.length (options t) in
  { t with selected = Int.max 0 (Int.min (n - 1) (t.selected + delta)) }
;;

let answer t =
  match t.prompt with
  | None -> None
  | Some (id, Select { options; _ }) ->
    Option.map (List.nth options t.selected) ~f:(fun (value, _) -> id, value)
  | Some (id, prompt) ->
    let allow_empty =
      match prompt with
      | Text _ | Secret { allow_empty = true; _ } -> true
      | Secret { allow_empty = false; _ } | Manual_code _ | Select _ -> false
    in
    let value = String.strip t.input in
    Option.some_if (allow_empty || not (String.is_empty value)) (id, value)
;;
