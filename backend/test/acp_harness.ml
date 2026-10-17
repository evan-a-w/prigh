open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* An ACP client on an in-memory line pair, talking to [Acp.serve] in front
   of an in-process backend whose only tool host is the adapter. The client
   answers the agent's requests with [answer] (or holds them for the test to
   answer); [dump] prints what went over the wire since the last dump. *)

module Answer = struct
  type t =
    | Result of string (** JSON *)
    | Error of int * string
    | Hold (** the test answers it later, with [respond] *)
end

type t =
  { sandbox : Tool_test_helpers.t
  ; input : string option Eio.Stream.t
  ; log : string Queue.t
  ; responses : Json.t Int.Table.t
  ; held : (string * Json.t) Queue.t (** method, id *)
  ; mutable answer : method_:string -> params:Json.t -> Answer.t
  ; mutable next_id : int
  ; mutable session : string
  ; prompts : string Queue.t (** the user messages the model got *)
  }

let field name json = Option.value (Json.member name json) ~default:`Null

let string_field name json =
  Option.value (Json.string (field name json)) ~default:""
;;

let drop_fields names = function
  | `Object fields ->
    `Object
      (List.filter fields ~f:(fun (k, _) ->
         not (List.mem names k ~equal:String.equal)))
  | json -> json
;;

let iso_time_re = Re.compile (Re.Perl.re {|\d{4}-\d{2}-\d{2}T[0-9:.]+Z|})

let mask_line t line =
  mask t.sandbox line
  |> Re.replace_string iso_time_re ~by:"<time>"
  |> String.substr_replace_all ~pattern:Version.to_string ~with_:"<version>"
;;

let verbose = ref false

(* Config options as [id=current[values]], modes as [current[ids]],
   commands as their names (unless [verbose]). *)
let rec abbreviate json =
  let strings items ~f = String.concat ~sep:"|" (List.map items ~f) in
  let list name json = Option.value (Json.list (field name json)) ~default:[] in
  match json with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, value) ->
         match key, value with
         | "configOptions", `Array options ->
           ( key
           , `String
               (String.concat
                  ~sep:" "
                  (List.map options ~f:(fun o ->
                     sprintf
                       "%s=%s[%s]"
                       (string_field "id" o)
                       (string_field "currentValue" o)
                       (strings (list "options" o) ~f:(string_field "value")))))
           )
         | "modes", (`Object _ as modes) ->
           ( key
           , `String
               (sprintf
                  "%s[%s]"
                  (string_field "currentModeId" modes)
                  (strings (list "availableModes" modes) ~f:(string_field "id")))
           )
         | "availableCommands", `Array commands ->
           ( key
           , `String
               (String.concat
                  ~sep:" "
                  (List.map commands ~f:(string_field "name"))) )
         | _ -> key, abbreviate value))
  | `Array items -> `Array (List.map items ~f:abbreviate)
  | json -> json
;;

(* One line as the test shows it: who sent it and what, without the
   [jsonrpc] field and the session id of updates. *)
let show t ~from json =
  let json = if !verbose then json else abbreviate json in
  let body =
    match json with
    | `Object _ when Option.is_some (Json.member "method" json) ->
      let method_ = string_field "method" json in
      let params = field "params" json in
      (match Json.member "id" json, method_ with
       | None, "session/update" ->
         let update = field "update" params in
         sprintf
           "update %s %s"
           (string_field "sessionUpdate" update)
           (Json.to_string (drop_fields [ "sessionUpdate" ] update))
       | None, _ -> sprintf "%s %s" method_ (Json.to_string params)
       | Some id, _ ->
         sprintf
           "request %s %s %s"
           (Json.to_string id)
           method_
           (Json.to_string params))
    | _ ->
      let id = Json.to_string (field "id" json) in
      (match Json.member "error" json with
       | Some error -> sprintf "error %s %s" id (Json.to_string error)
       | None ->
         sprintf "result %s %s" id (Json.to_string (field "result" json)))
  in
  Queue.enqueue t.log (mask_line t (sprintf "%s %s" from body))
;;

let send_json t json =
  show t ~from:">" json;
  Eio.Stream.add t.input (Some (Json.to_string json))
;;

let respond t id answer =
  let reply =
    match (answer : Answer.t) with
    | Result json -> [ "result", Json.of_string json ]
    | Error (code, message) ->
      [ ( "error"
        , `Object
            [ "code", `Number (Int.to_string code); "message", `String message ]
        )
      ]
    | Hold -> assert false
  in
  send_json t (`Object (("jsonrpc", `String "2.0") :: ("id", id) :: reply))
;;

(* The held request for [method_], oldest first. *)
let respond_held t method_ answer =
  match Queue.find t.held ~f:(fun (m, _) -> String.equal m method_) with
  | None -> print_endline ("(no held " ^ method_ ^ ")")
  | Some ((_, id) as held) ->
    Queue.filter_inplace t.held ~f:(fun h -> not (phys_equal h held));
    respond t id answer
;;

let received t line =
  let json = Json.of_string line in
  show t ~from:"<" json;
  match Json.member "method" json, Json.member "id" json with
  | Some (`String method_), Some id ->
    (match t.answer ~method_ ~params:(field "params" json) with
     | Hold -> Queue.enqueue t.held (method_, id)
     | answer -> respond t id answer)
  | None, Some (`Number n) ->
    Hashtbl.set t.responses ~key:(Int.of_string n) ~data:json
  | _ -> ()
;;

let settle t =
  let clock = Eio.Stdenv.clock t.sandbox.env in
  for _ = 1 to 5 do
    Eio.Time.sleep clock 0.002;
    for _ = 1 to 20 do
      Eio.Fiber.yield ()
    done
  done
;;

let wait_until ?(what = "") t f =
  let clock = Eio.Stdenv.clock t.sandbox.env in
  let rec go n =
    if f ()
    then ()
    else if n = 0
    then printf "(timed out waiting for %s)\n" what
    else (
      Eio.Time.sleep clock 0.005;
      go (n - 1))
  in
  go 2000;
  settle t
;;

let wait_for t substring =
  wait_until t ~what:substring (fun () ->
    Queue.exists t.log ~f:(String.is_substring ~substring))
;;

let dump t =
  Queue.iter t.log ~f:print_endline;
  Queue.clear t.log
;;

(* Sends a request; with [wait] (the default), waits for its response and
   returns it. *)
let request ?(wait = true) t method_ params =
  let id = t.next_id in
  t.next_id <- id + 1;
  send_json
    t
    (`Object
        [ "jsonrpc", `String "2.0"
        ; "id", `Number (Int.to_string id)
        ; "method", `String method_
        ; "params", Json.of_string params
        ]);
  if wait
  then (
    wait_until t ~what:method_ (fun () -> Hashtbl.mem t.responses id);
    Option.value (Hashtbl.find t.responses id) ~default:`Null)
  else `Null
;;

let notify t method_ params =
  send_json
    t
    (`Object
        [ "jsonrpc", `String "2.0"
        ; "method", `String method_
        ; "params", Json.of_string params
        ]);
  settle t
;;

(* [%s] in [params] is the current session's id. *)
let session_request ?wait t method_ params =
  request
    ?wait
    t
    method_
    (String.substr_replace_all params ~pattern:"%s" ~with_:t.session)
;;

let initialize ?(caps = {|{"_meta": {"prigh": {"state": true}}}|}) t =
  ignore
    (request
       t
       "initialize"
       (sprintf
          {|{"protocolVersion": 1, "clientCapabilities": %s, "clientInfo": {"name": "test-editor", "version": "1"}}|}
          caps)
     : Json.t)
;;

let new_session t =
  let response =
    request
      t
      "session/new"
      (sprintf {|{"cwd": %S, "mcpServers": []}|} t.sandbox.dir)
  in
  t.session <- string_field "sessionId" (field "result" response);
  settle t
;;

let prompt ?wait t text =
  session_request
    ?wait
    t
    "session/prompt"
    (sprintf
       {|{"sessionId": "%%s", "prompt": [{"type": "text", "text": %S}]}|}
       text)
;;

let default_answer ~method_:_ ~params:_ =
  Answer.Error (-32601, "not in this test")
;;

let reply_of_json s =
  Or_error.ok_exn (Faux_provider.Reply.of_json (Json.of_string s))
;;

(* [replies] are Faux_provider replies as JSON (see its [Reply.of_json]).
   With [initialized] (the default), [initialize] and [session/new] have
   been done (not dumped). *)
let run ?(initialized = true) ?caps ?config replies f =
  verbose := false;
  with_sandbox
  @@ fun sandbox ->
  Option.iter config ~f:(write sandbox ".prigh/config.json");
  (* The adapter is a tool host: its skills and instructions come from
     $HOME. *)
  let home = Sys.getenv "HOME" in
  Core_unix.putenv ~key:"HOME" ~data:sandbox.dir;
  Exn.protect
    ~finally:(fun () ->
      match home with
      | Some home -> Core_unix.putenv ~key:"HOME" ~data:home
      | None -> Core_unix.unsetenv "HOME")
    ~f:(fun () ->
      Eio.Switch.run
      @@ fun sw ->
      let prompts = Queue.create () in
      let provider =
        Faux_provider.create
          ~on_request:(fun r ->
            match List.last r.messages with
            | Some (Message.User u) -> Queue.enqueue prompts u.text
            | _ -> ())
          (List.map replies ~f:reply_of_json)
      in
      let login =
        Login_manager.create
          ~env:sandbox.env
          ~sw
          ~getenv:(fun _ -> None)
          ~store:
            (Auth_store.create ~path:(Filename.concat sandbox.dir "auth.json"))
          ()
      in
      let sessions_dir = Filename.concat sandbox.dir "sessions" in
      let new_agent ?session ~cwd () =
        Agent.create
          ~env:sandbox.env
          ~sw
          ~provider
          ~tools:(Tools.all @ Tools.interaction)
          ~sessions_dir
          ~home:sandbox.dir
          ?session
          ~backend_host:false
          ~cwd
          ()
      in
      let server =
        Rpc_server.create
          ~env:sandbox.env
          ~sw
          ~backend_host:false
          ~login
          ~sessions_dir
          ~cwd:sandbox.dir
          ~new_agent
          ()
      in
      let t =
        { sandbox
        ; input = Eio.Stream.create 1000
        ; log = Queue.create ()
        ; responses = Int.Table.create ()
        ; held = Queue.create ()
        ; answer = default_answer
        ; next_id = 0
        ; session = ""
        ; prompts
        }
      in
      let finished, resolve = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
        Acp.serve
          ~env:sandbox.env
          { connector = Rpc_client.Connector.in_process server
          ; token = None
          ; user = None
          ; tool_host =
              Some
                { host_id = "host-test"; name = "testhost"; cwd = sandbox.dir }
          }
          ~read_line:(fun () -> Eio.Stream.take t.input)
          ~write_line:(received t);
        Eio.Promise.resolve resolve ());
      if initialized
      then (
        initialize ?caps t;
        new_session t;
        Queue.clear t.log);
      f t;
      Eio.Stream.add t.input None;
      Eio.Promise.await finished;
      Rpc_server.shutdown server)
;;

(* The user messages the model got since the last call. *)
let print_prompts t =
  Queue.iter t.prompts ~f:(fun p ->
    print_endline (mask t.sandbox ("model got: " ^ p)));
  Queue.clear t.prompts
;;
