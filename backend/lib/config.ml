open! Core
open! Import

type t =
  { scoped_models : string list
  ; confirm_tools : bool
  ; default_model : string option
  ; default_thinking : Thinking.t option
  ; fallback_models : string list
  ; default_cwd : string option
  }
[@@deriving sexp_of]

let default =
  { scoped_models = []
  ; confirm_tools = false
  ; default_model = None
  ; default_thinking = None
  ; fallback_models = []
  ; default_cwd = None
  }
;;

let start_model t =
  match t.default_model with
  | Some _ as model -> model
  | None -> List.hd t.fallback_models
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
    ; ( "fallback_models"
      , `Array (List.map t.fallback_models ~f:(fun s -> `String s)) )
    ; "default_cwd", option (fun s -> `String s) t.default_cwd
    ]
;;

let of_json json =
  match json with
  | `Object fields ->
    let open Or_error.Let_syntax in
    let find name = List.Assoc.find fields ~equal:String.equal name in
    let strings name =
      let error () =
        Or_error.errorf "config.%s must be an array of strings" name
      in
      match find name with
      | None -> Ok []
      | Some (`Array items) ->
        Or_error.all
          (List.map items ~f:(function
             | `String s -> Ok s
             | _ -> error ()))
      | Some _ -> error ()
    in
    let%bind scoped_models = strings "scoped_models" in
    let%bind fallback_models = strings "fallback_models" in
    let%bind default_cwd =
      match find "default_cwd" with
      | None | Some `Null -> Ok None
      | Some (`String "") -> Ok None
      | Some (`String s) -> Ok (Some s)
      | Some _ -> Or_error.error_string "config.default_cwd must be a string"
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
    { scoped_models
    ; confirm_tools
    ; default_model
    ; default_thinking
    ; fallback_models
    ; default_cwd
    }
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
          | Error e ->
            Or_error.errorf
              "%s is not valid JSON (%s)"
              path
              (Error.to_string_hum e)))
;;

let write_fields ~home fields =
  let path = path ~home in
  Or_error.try_with (fun () ->
    Core_unix.mkdir_p (Filename.dirname path);
    let tmp = path ^ ".tmp" in
    Out_channel.write_all tmp ~data:(Json.to_string_hum (`Object fields));
    Core_unix.rename ~src:tmp ~dst:path)
;;

let load ~home =
  Or_error.bind (read_fields ~home) ~f:(fun f -> of_json (`Object f))
;;

let known_fields =
  "providers"
  ::
  (match to_json default with
   | `Object fields -> List.map fields ~f:fst
   | _ -> [])
;;

let problems ~home =
  match read_fields ~home with
  | Error _ -> []
  | Ok fields ->
    let invalid =
      match of_json (`Object fields) with
      | Ok _ -> []
      | Error e ->
        [ sprintf
            "%s (in %s); prigh ignores the file's settings until it is fixed"
            (Error.to_string_hum e)
            (path ~home)
        ]
    in
    let unknown =
      List.filter_map fields ~f:(fun (name, _) ->
        if List.mem known_fields name ~equal:String.equal
        then None
        else
          Some
            (sprintf
               "unknown setting %S in %s is ignored%s"
               name
               (path ~home)
               (match Edit_distance.closest ~n:1 known_fields name with
                | [ close ] when Edit_distance.caseless close name <= 3 ->
                  sprintf "; did you mean %S?" close
                | _ ->
                  sprintf " (known: %s)" (String.concat ~sep:", " known_fields))))
    in
    invalid @ unknown
;;

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
        ( name
        , Option.value
            (List.Assoc.find ours ~equal:String.equal name)
            ~default:value ))
    in
    let added =
      List.filter ours ~f:(fun (name, _) ->
        not (List.Assoc.mem existing ~equal:String.equal name))
    in
    write_fields ~home (replaced @ added))
;;
