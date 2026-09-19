open! Core

module Spec = struct
  type t =
    { name : string
    ; args : string
    ; help : string
    }
  [@@deriving sexp_of]
end

let c name args help = { Spec.name; args; help }

let all =
  [ c "help" "" "show commands and keys"
  ; c "model" "[name|id|provider/id]" "pick or switch the model"
  ; c "thinking" "[off|on|low|high|max]" "pick or set the thinking level"
  ; c "verbosity" "[quiet|normal|verbose]" "set the transcript verbosity"
  ; c "login" "[provider] [api_key|oauth]" "log in to a provider"
  ; c "logout" "[provider]" "remove a provider's stored credential"
  ; c "auth" "" "show which providers are configured"
  ; c "compact" "" "summarise older messages to free context"
  ; c "new" "" "start a new session"
  ; c "sessions" "" "pick a saved session to switch to"
  ; c "switch" "[path]" "switch to a saved session"
  ; c "fork" "" "fork the current session"
  ; c "abort" "" "abort the current run"
  ; c "state" "" "show session state"
  ; c "clear" "" "clear the transcript"
  ; c "quit" "" "exit"
  ]
;;

let find name = List.find all ~f:(fun s -> String.equal s.name name)

module Parsed = struct
  type t =
    { name : string
    ; args : string list
    ; rest : string
    }
  [@@deriving sexp_of]
end

let parse input =
  let trimmed = String.strip input in
  match String.chop_prefix trimmed ~prefix:"/" with
  | None -> None
  | Some body ->
    (match
       String.split body ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
     with
     | [] -> Some { Parsed.name = ""; args = []; rest = "" }
     | name :: args ->
       let rest = String.strip (String.drop_prefix body (String.length name)) in
       Some { name; args; rest })
;;

module Completion = struct
  type t =
    | Unique of string
    | Common_prefix of string
    | Candidates of Spec.t list
    | Nothing
  [@@deriving sexp_of]
end

let common_prefix a b =
  let n = Int.min (String.length a) (String.length b) in
  let rec go i = if i < n && Char.equal a.[i] b.[i] then go (i + 1) else i in
  String.prefix a (go 0)
;;

let complete input : Completion.t =
  match String.chop_prefix input ~prefix:"/" with
  | Some prefix when not (String.contains prefix ' ') ->
    (match List.filter all ~f:(fun s -> String.is_prefix s.name ~prefix) with
     | [] -> Nothing
     | [ one ] -> Unique ("/" ^ one.name ^ " ")
     | many ->
       let common =
         List.fold many ~init:(List.hd_exn many).name ~f:(fun acc s ->
           common_prefix acc s.name)
       in
       if String.length common > String.length prefix
       then Common_prefix ("/" ^ common)
       else Candidates many)
  | _ -> Nothing
;;

let closest name =
  List.filter_map all ~f:(fun s ->
    let d = Edit_distance.distance s.name name in
    Option.some_if (d <= 2 || String.is_prefix s.name ~prefix:name) (d, s))
  |> List.min_elt ~compare:(fun (a, _) (b, _) -> Int.compare a b)
  |> Option.map ~f:snd
;;

let help : Content.t =
  let rows =
    List.map all ~f:(fun s ->
      String.strip ("/" ^ s.name ^ " " ^ s.args), s.help)
  in
  let width =
    List.fold rows ~init:0 ~f:(fun acc (k, _) -> Int.max acc (String.length k))
  in
  List.map rows ~f:(fun (k, help) ->
    [ { Content.Span.text = Text_width.pad_right k ~width
      ; style = Style.bold Style.plain
      }
    ; { text = "  " ^ help; style = Style.plain }
    ])
;;
