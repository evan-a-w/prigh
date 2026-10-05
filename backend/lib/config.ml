open! Core
open! Import

type t =
  { scoped_models : string list
  ; confirm_tools : bool
  ; default_model : string option
  ; default_thinking : Thinking.t option
  }
[@@deriving sexp_of]

let default =
  { scoped_models = []
  ; confirm_tools = false
  ; default_model = None
  ; default_thinking = None
  }
;;

let path ~home = Filename.concat home ".prigh/config.json"

let to_json t =
  let option f = Option.value_map ~default:`Null ~f in
  `Object
    [ "scoped_models", `Array (List.map t.scoped_models ~f:(fun s -> `String s))
    ; ("confirm_tools", if t.confirm_tools then `True else `False)
    ; "default_model", option (fun s -> `String s) t.default_model
    ; ( "default_thinking"
      , option (fun th -> `String (Thinking.to_string th)) t.default_thinking )
    ]
;;

let of_json json =
  match json with
  | `Object fields ->
    let open Or_error.Let_syntax in
    let find name = List.Assoc.find fields ~equal:String.equal name in
    let%bind scoped_models =
      match find "scoped_models" with
      | None -> Ok []
      | Some (`Array items) ->
        Or_error.all
          (List.map items ~f:(function
             | `String s -> Ok s
             | _ ->
               Or_error.error_string
                 "config.scoped_models must be an array of strings"))
      | Some _ ->
        Or_error.error_string "config.scoped_models must be an array of strings"
    in
    let%bind confirm_tools =
      match find "confirm_tools" with
      | None -> Ok false
      | Some `True -> Ok true
      | Some `False -> Ok false
      | Some _ -> Or_error.error_string "config.confirm_tools must be a boolean"
    in
    let%bind default_model =
      match find "default_model" with
      | None | Some `Null -> Ok None
      | Some (`String s) -> Ok (Some s)
      | Some _ -> Or_error.error_string "config.default_model must be a string"
    in
    let%map default_thinking =
      match find "default_thinking" with
      | None | Some `Null -> Ok None
      | Some (`String s) ->
        Or_error.map (Thinking.of_string s) ~f:Option.some
        |> Or_error.tag ~tag:"config.default_thinking"
      | Some _ ->
        Or_error.error_string "config.default_thinking must be a string"
    in
    { scoped_models; confirm_tools; default_model; default_thinking }
  | _ -> Or_error.error_string "config must be a JSON object"
;;

let read_fields ~home =
  let path = path ~home in
  if not (Sys_unix.file_exists_exn path)
  then Ok []
  else
    Or_error.bind
      (Or_error.try_with (fun () -> In_channel.read_all path))
      ~f:(fun data ->
        if String.is_empty (String.strip data)
        then Ok []
        else (
          match Json.parse data with
          | Ok (`Object fields) -> Ok fields
          | Ok _ -> Or_error.errorf "%s must be a JSON object" path
          | Error e -> Error e))
;;

let write_fields ~home fields =
  let path = path ~home in
  Or_error.try_with (fun () ->
    Core_unix.mkdir_p (Filename.dirname path);
    let tmp = path ^ ".tmp" in
    Out_channel.write_all tmp ~data:(Json.to_string_hum (`Object fields));
    Core_unix.rename ~src:tmp ~dst:path)
;;

let load ~home = Or_error.bind (read_fields ~home) ~f:(fun f -> of_json (`Object f))

(* Fields this module does not own ([providers], anything hand-added) are
   kept, in place. *)
let save ~home t =
  Or_error.bind (read_fields ~home) ~f:(fun existing ->
    let ours =
      match to_json t with
      | `Object fields -> fields
      | _ -> assert false
    in
    let replaced =
      List.map existing ~f:(fun (name, value) ->
        name, Option.value (List.Assoc.find ours ~equal:String.equal name) ~default:value)
    in
    let added =
      List.filter ours ~f:(fun (name, _) ->
        not (List.Assoc.mem existing ~equal:String.equal name))
    in
    write_fields ~home (replaced @ added))
;;
