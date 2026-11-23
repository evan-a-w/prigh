open! Core
open! Import

module Item = struct
  type t =
    | Expect of Json.t
    | Emit of Json.t
    | Exit of int * string
    | Skip
end

module Command_entry = struct
  type t =
    { args : string list
    ; exit : int
    ; stdout : string
    ; stderr : string
    }
end

module Default_reply = struct
  type t =
    | Method of string * Json.t
    | Subtype of string * Json.t
end

module Script = struct
  type t =
    { items : Item.t list
    ; commands : Command_entry.t list
    ; defaults : Default_reply.t list
    }

  let string_field name json =
    match Json.member name json with
    | Some (`String s) -> Some s
    | _ -> None
  ;;

  let int_field name json =
    match Json.member name json with
    | Some (`Number n) -> Int.of_string_opt n
    | _ -> None
  ;;

  let parse text =
    let open Or_error.Let_syntax in
    let%map lines =
      String.split_lines text
      |> List.filter ~f:(fun l -> not (String.is_empty (String.strip l)))
      |> List.mapi ~f:(fun i line ->
        Json.parse line |> Or_error.tag ~tag:(sprintf "script line %d" (i + 1)))
      |> Or_error.all
    in
    List.fold_right
      lines
      ~init:{ items = []; commands = []; defaults = [] }
      ~f:(fun json t ->
        let frame () = Option.value (Json.member "frame" json) ~default:`Null in
        match string_field "type" json with
        | Some "expect_outbound" ->
          { t with items = Expect (frame ()) :: t.items }
        | Some "emit_inbound" -> { t with items = Emit (frame ()) :: t.items }
        | Some "exit" ->
          { t with
            items =
              Exit
                ( Option.value (int_field "code" json) ~default:1
                , Option.value (string_field "stderr" json) ~default:"" )
              :: t.items
          }
        | Some "command" ->
          let args =
            match Json.member "args" json with
            | Some (`Array args) ->
              List.filter_map args ~f:(function
                | `String s -> Some s
                | _ -> None)
            | _ -> []
          in
          { t with
            commands =
              { args
              ; exit = Option.value (int_field "exit" json) ~default:0
              ; stdout = Option.value (string_field "stdout" json) ~default:""
              ; stderr = Option.value (string_field "stderr" json) ~default:""
              }
              :: t.commands
          }
        | Some "default_reply" ->
          let reply =
            match string_field "method" json, string_field "subtype" json with
            | Some m, _ ->
              Some
                (Default_reply.Method
                   ( m
                   , Option.value
                       (Json.member "result" json)
                       ~default:(`Object []) ))
            | None, Some s ->
              Some
                (Subtype
                   ( s
                   , Option.value
                       (Json.member "response" json)
                       ~default:(`Object []) ))
            | None, None -> None
          in
          { t with defaults = Option.to_list reply @ t.defaults }
        | _ -> { t with items = Skip :: t.items })
  ;;
end

let command (script : Script.t) args =
  List.find script.commands ~f:(fun c -> List.equal String.equal c.args args)
  |> Option.map ~f:(fun (c : Command_entry.t) -> c.exit, c.stdout, c.stderr)
;;

type t =
  { script : Script.t
  ; mutable items : Item.t list
  ; ids : Json.t String.Table.t (* a recorded request id -> prigh's *)
  ; mutable exit : (int * string) option
  }

let create (script : Script.t) =
  { script; items = script.items; ids = String.Table.create (); exit = None }
;;

let exit_requested t = t.exit
let field = Script.string_field

let path names json =
  List.fold names ~init:(Some json) ~f:(fun j n ->
    Option.bind j ~f:(Json.member n))
;;

let path_string names json =
  match path names json with
  | Some (`String s) -> Some s
  | _ -> None
;;

let key json = Json.to_string json

let set_field obj name value =
  match obj with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (k, v) ->
         if String.equal k name then k, value else k, v))
  | other -> other
;;

let is_response json =
  Option.is_none (Json.member "method" json)
  && Option.is_some (Json.member "id" json)
  && (Option.is_some (Json.member "result" json)
      || Option.is_some (Json.member "error" json))
;;

let same name expected actual =
  match field name expected with
  | None -> true
  | Some v -> Option.equal String.equal (Some v) (field name actual)
;;

let matches ~expected ~actual =
  match Json.member "method" expected, Json.member "type" expected with
  | Some _, _ -> same "method" expected actual
  | None, Some _ ->
    same "type" expected actual
    && (match path_string [ "request"; "subtype" ] expected with
        | None -> true
        | Some s ->
          Option.equal
            String.equal
            (Some s)
            (path_string [ "request"; "subtype" ] actual))
    &&
      (match path [ "response"; "request_id" ] expected with
      | None -> true
      | Some id ->
        Option.equal
          Json.exactly_equal
          (Some id)
          (path [ "response"; "request_id" ] actual))
  | None, None ->
    (match Json.member "id" expected with
     | Some id ->
       is_response actual
       && Option.equal Json.exactly_equal (Some id) (Json.member "id" actual)
     | None -> false)
;;

let remember t ~expected ~actual =
  (match
     ( Json.member "method" expected
     , Json.member "id" expected
     , Json.member "id" actual )
   with
   | Some _, Some recorded, Some id ->
     Hashtbl.set t.ids ~key:(key recorded) ~data:id
   | _ -> ());
  match Json.member "request_id" expected, Json.member "request_id" actual with
  | Some recorded, Some id -> Hashtbl.set t.ids ~key:(key recorded) ~data:id
  | _ -> ()
;;

let rewrite t frame =
  let mapped id = Hashtbl.find t.ids (key id) |> Option.value ~default:id in
  if is_response frame
  then (
    match Json.member "id" frame with
    | Some id -> set_field frame "id" (mapped id)
    | None -> frame)
  else if
    Option.equal String.equal (field "type" frame) (Some "control_response")
  then (
    match Json.member "response" frame with
    | Some response ->
      (match Json.member "request_id" response with
       | Some id ->
         set_field
           frame
           "response"
           (set_field response "request_id" (mapped id))
       | None -> frame)
    | None -> frame)
  else frame
;;

(* Emits up to the next expectation (or an exit, or the end). *)
let advance t =
  let rec go acc =
    match t.items with
    | Emit frame :: rest ->
      t.items <- rest;
      go (Json.to_string (rewrite t frame) :: acc)
    | Skip :: rest ->
      t.items <- rest;
      go acc
    | Exit (code, stderr) :: rest ->
      t.items <- rest;
      t.exit <- Some (code, stderr);
      List.rev acc
    | Expect _ :: _ | [] -> List.rev acc
  in
  go []
;;

let start t = advance t

let default_answer t json =
  match field "method" json, Json.member "id" json with
  | Some meth, Some id ->
    let result =
      List.find_map t.script.defaults ~f:(function
        | Method (m, r) when String.equal m meth -> Some r
        | _ -> None)
      |> Option.value ~default:(`Object [])
    in
    [ Json.to_string (`Object [ "id", id; "result", result ]) ]
  | _ ->
    (match field "type" json, Json.member "request_id" json with
     | Some "control_request", Some request_id ->
       let subtype = path_string [ "request"; "subtype" ] json in
       let response =
         List.find_map t.script.defaults ~f:(function
           | Subtype (s, r) when Option.equal String.equal (Some s) subtype ->
             Some r
           | _ -> None)
         |> Option.value ~default:(`Object [])
       in
       [ Json.to_string
           (`Object
               [ "type", `String "control_response"
               ; ( "response"
                 , `Object
                     [ "subtype", `String "success"
                     ; "request_id", request_id
                     ; "response", response
                     ] )
               ])
       ]
     | _ -> [])
;;

let handle_line t line =
  if Option.is_some t.exit
  then []
  else (
    match Json.parse line with
    | Error _ -> []
    | Ok actual ->
      (match t.items with
       | Expect expected :: rest when matches ~expected ~actual ->
         t.items <- rest;
         remember t ~expected ~actual;
         advance t
       | _ -> default_answer t actual))
;;

let usage () =
  eprintf "usage: prigh fake-cli NAME --script FILE [-- ARGS...]\n";
  exit 2
;;

let main args =
  let name, rest =
    match args with
    | name :: rest -> name, rest
    | [] -> usage ()
  in
  let file, args =
    match rest with
    | "--script" :: file :: "--" :: args -> file, args
    | "--script" :: file :: args -> file, args
    | _ -> usage ()
  in
  let log =
    Option.map (Sys.getenv "PRIGH_FAKE_CLI_LOG") ~f:(fun path ->
      Out_channel.create ~append:true path)
  in
  let note dir line =
    Option.iter log ~f:(fun oc -> fprintf oc "%s %s %s\n%!" name dir line)
  in
  match Script.parse (In_channel.read_all file) with
  | Error e ->
    eprintf "prigh fake-cli %s: %s\n" name (Error.to_string_hum e);
    exit 2
  | Ok script ->
    note "argv" (String.concat ~sep:" " args);
    (match command script args with
     | Some (code, stdout, stderr) ->
       Out_channel.output_string Out_channel.stdout stdout;
       Out_channel.output_string Out_channel.stderr stderr;
       Out_channel.flush Out_channel.stdout;
       Out_channel.flush Out_channel.stderr;
       exit code
     | None ->
       let t = create script in
       let write lines =
         List.iter lines ~f:(fun line ->
           note "<" line;
           Out_channel.output_string Out_channel.stdout line;
           Out_channel.output_char Out_channel.stdout '\n');
         Out_channel.flush Out_channel.stdout;
         Option.iter t.exit ~f:(fun (code, stderr) ->
           Out_channel.output_string Out_channel.stderr stderr;
           Out_channel.flush Out_channel.stderr;
           exit code)
       in
       write (start t);
       let rec loop () =
         match In_channel.input_line In_channel.stdin with
         | None -> ()
         | Some line ->
           note ">" line;
           write (handle_line t line);
           loop ()
       in
       loop ())
;;
