open! Core
open! Import

type t =
  { scoped_models : string list
  ; approval_mode : Approval_mode.t
  ; permissions : Approval.Rules.t
  ; default_model : string option
  ; default_thinking : Thinking.t option
  ; fallback_models : string list
  ; default_cwd : string option
  ; max_budget_usd : float option
  ; goal_max_continuations : int
  ; resume_on_limit_reset : bool
  ; archive_after_days : float
  }
[@@deriving sexp_of]

let default =
  { scoped_models = []
  ; approval_mode = Auto
  ; permissions = Approval.Rules.empty
  ; default_model = None
  ; default_thinking = None
  ; fallback_models = []
  ; default_cwd = None
  ; max_budget_usd = None
  ; goal_max_continuations = Goal.default_max_continuations
  ; resume_on_limit_reset = true
  ; archive_after_days = 7.
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
    ; "approval_mode", `String (Approval_mode.to_string t.approval_mode)
    ; "permissions", Approval.Rules.to_json t.permissions
    ; "default_model", option (fun s -> `String s) t.default_model
    ; ( "default_thinking"
      , option (fun th -> `String (Thinking.to_string th)) t.default_thinking )
    ; ( "fallback_models"
      , `Array (List.map t.fallback_models ~f:(fun s -> `String s)) )
    ; "default_cwd", option (fun s -> `String s) t.default_cwd
    ; ( "max_budget_usd"
      , option
          (fun usd -> `Number (Float.to_string_hum ~strip_zero:true usd))
          t.max_budget_usd )
    ; "goal_max_continuations", `Number (Int.to_string t.goal_max_continuations)
    ; ( "resume_on_limit_reset"
      , if t.resume_on_limit_reset then `True else `False )
    ; "archive_after_days", `Number (sprintf "%g" t.archive_after_days)
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
    (* [confirm_tools] is what [approval_mode] replaced: on is [ask], off
       is [auto]. *)
    let%bind approval_mode =
      match find "approval_mode", find "confirm_tools" with
      | Some (`String s), _ ->
        Approval_mode.of_string s |> Or_error.tag ~tag:"config.approval_mode"
      | Some _, _ ->
        Or_error.error_string "config.approval_mode must be a string"
      | None, (None | Some `False) -> Ok Approval_mode.Auto
      | None, Some `True -> Ok Approval_mode.Ask
      | None, Some _ ->
        Or_error.error_string "config.confirm_tools must be a boolean"
    in
    let%bind permissions =
      Or_error.map
        (Approval.Rules.of_json
           (Option.value (find "permissions") ~default:`Null))
        ~f:fst
    in
    let%bind archive_after_days =
      let error () =
        Or_error.error_string
          "config.archive_after_days must be a number of days (0: never)"
      in
      match find "archive_after_days" with
      | None | Some `Null -> Ok default.archive_after_days
      | Some (`Number n) ->
        (match Float.of_string_opt n with
         | Some days when Float.(days >= 0.) -> Ok days
         | Some _ | None -> error ())
      | Some _ -> error ()
    in
    let%bind default_model =
      match find "default_model" with
      | None | Some `Null -> Ok None
      | Some (`String s) -> Ok (Some s)
      | Some _ -> Or_error.error_string "config.default_model must be a string"
    in
    let%bind max_budget_usd =
      match find "max_budget_usd" with
      | None | Some `Null -> Ok None
      | Some (`Number n) ->
        (match Float.of_string_opt n with
         | Some usd when Float.( > ) usd 0. -> Ok (Some usd)
         | Some _ | None ->
           Or_error.error_string
             "config.max_budget_usd must be a positive number of dollars (or \
              null for none)")
      | Some _ ->
        Or_error.error_string
          "config.max_budget_usd must be a number of dollars (or null for none)"
    in
    let%bind goal_max_continuations =
      match find "goal_max_continuations" with
      | None | Some `Null -> Ok Goal.default_max_continuations
      | Some (`Number n) ->
        (match Int.of_string_opt n with
         | Some n when n >= 0 -> Ok n
         | Some _ | None ->
           Or_error.error_string
             "config.goal_max_continuations must be a whole number, 0 or more")
      | Some _ ->
        Or_error.error_string
          "config.goal_max_continuations must be a whole number"
    in
    let%bind resume_on_limit_reset =
      match find "resume_on_limit_reset" with
      | None | Some `Null -> Ok true
      | Some `True -> Ok true
      | Some `False -> Ok false
      | Some _ ->
        Or_error.error_string "config.resume_on_limit_reset must be a boolean"
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
    ; approval_mode
    ; permissions
    ; default_model
    ; default_thinking
    ; fallback_models
    ; default_cwd
    ; max_budget_usd
    ; goal_max_continuations
    ; resume_on_limit_reset
    ; archive_after_days
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
  :: "confirm_tools"
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
    let rules =
      match List.Assoc.find fields ~equal:String.equal "permissions" with
      | None -> []
      | Some json ->
        (match Approval.Rules.of_json json with
         | Ok (_, problems) ->
           List.map problems ~f:(fun p -> sprintf "%s in %s" p (path ~home))
         | Error _ -> [])
    in
    invalid @ rules @ unknown
;;

(* Fields this module does not own ([providers], anything hand-added) are
   kept, in place. *)
let save ~home t =
  Or_error.bind (read_fields ~home) ~f:(fun existing ->
    (* [permissions] is the user's to edit by hand: it is written as they
       wrote it, invalid rules included. *)
    let ours =
      match to_json t with
      | `Object fields ->
        List.filter fields ~f:(fun (name, _) ->
          not (String.equal name "permissions"))
      | _ -> assert false
    in
    (* [approval_mode] replaces it. *)
    let existing =
      List.filter existing ~f:(fun (name, _) ->
        not (String.equal name "confirm_tools"))
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
