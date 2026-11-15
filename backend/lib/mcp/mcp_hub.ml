open! Core
open! Import

module Status = struct
  type t =
    | Ready of Mcp_client.Tool.t list
    | Failed of string
    | Needs_approval
    | Needs_sign_in of string
  [@@deriving sexp_of]
end

module Server_status = struct
  type t =
    { server : Mcp_config.Server.t
    ; status : Status.t
    }
  [@@deriving sexp_of]
end

module Entry = struct
  type t =
    { server : Mcp_config.Server.t
    ; client : (Mcp_client.t, Mcp_client.Connect_error.t) Result.t Promise.t
    ; bearer : string option (** the token it started with *)
    }

  let same_server t (server : Mcp_config.Server.t) =
    String.equal t.server.name server.name
    && String.equal t.server.source server.source
  ;;
end

type t =
  { env : Env.t
  ; sw : Switch.t
  ; entries : Entry.t String.Table.t (** by {!Mcp_config.Server.key} *)
  ; mutable tokens : string String.Map.t (** by server URL *)
  ; mutable closed : bool
  }

let create ~env ~sw () =
  { env
  ; sw
  ; entries = String.Table.create ()
  ; tokens = String.Map.empty
  ; closed = false
  }
;;

let bearer t (server : Mcp_config.Server.t) =
  match server.transport with
  | Http { url; _ } -> Map.find t.tokens url
  | Stdio _ -> None
;;

let close_entry t (entry : Entry.t) =
  match Promise.peek entry.client with
  | Some (Ok client) -> Mcp_client.close client
  | Some (Error _) -> ()
  | None ->
    Fiber.fork ~sw:t.sw (fun () ->
      Result.iter (Promise.await entry.client) ~f:Mcp_client.close)
;;

(* Replaces the server's entries, including those for its old definitions,
   with a new start that every caller shares. *)
let start t (server : Mcp_config.Server.t) =
  let replaced =
    Hashtbl.data t.entries
    |> List.filter ~f:(fun entry -> Entry.same_server entry server)
  in
  List.iter replaced ~f:(fun entry ->
    Hashtbl.remove t.entries (Mcp_config.Server.key entry.server));
  let client, resolver = Promise.create () in
  Hashtbl.set
    t.entries
    ~key:(Mcp_config.Server.key server)
    ~data:{ server; client; bearer = bearer t server };
  let error message =
    Error { Mcp_client.Connect_error.message; needs_sign_in = false }
  in
  Fiber.fork ~sw:t.sw (fun () ->
    let result =
      match
        Mcp_client.connect
          ~env:t.env
          ~sw:t.sw
          ~bearer:(fun () -> bearer t server)
          server
      with
      | result -> result
      | exception (Eio.Cancel.Cancelled _ as exn) ->
        Promise.resolve resolver (error "the MCP hub was shut down");
        raise exn
      | exception exn -> error (Exn.to_string exn)
    in
    if t.closed then Result.iter result ~f:Mcp_client.close;
    Promise.resolve resolver result);
  List.iter replaced ~f:(close_entry t);
  client
;;

let client t ~reconnect (server : Mcp_config.Server.t) =
  let promise =
    match Hashtbl.find t.entries (Mcp_config.Server.key server) with
    | None -> start t server
    | Some entry ->
      (* A new token retries a server that wanted a sign-in; losing it
         (signing out) disconnects one that had it. *)
      let bearer = bearer t server in
      let changed = not (Option.equal String.equal bearer entry.bearer) in
      (match Promise.peek entry.client with
       | None -> entry.client
       | Some (Ok client) when Option.is_none (Mcp_client.failure client) ->
         if changed && Option.is_none bearer
         then start t server
         else entry.client
       | Some (Ok _ | Error _) ->
         if reconnect || changed then start t server else entry.client)
  in
  match Promise.await promise with
  | Error e -> Error e
  | Ok client ->
    (match Mcp_client.failure client with
     | None -> Ok client
     | Some message ->
       Error { message; needs_sign_in = Mcp_client.needs_sign_in client })
;;

let status_of_error ({ message; needs_sign_in } : Mcp_client.Connect_error.t)
  : Status.t
  =
  if needs_sign_in then Needs_sign_in message else Failed message
;;

let set_tokens t = function
  | None -> ()
  | Some tokens ->
    t.tokens <- String.Map.of_alist_reduce tokens ~f:(fun _ latest -> latest)
;;

(* Stops the servers from the settings that the settings no longer define
   as they were started: removed, disabled or edited ones. *)
let prune_settings t ~home settings =
  let current =
    (Mcp_config.settings_servers ~home settings).servers
    |> List.map ~f:Mcp_config.Server.key
    |> String.Set.of_list
  in
  let stale =
    Hashtbl.to_alist t.entries
    |> List.filter ~f:(fun (key, (entry : Entry.t)) ->
      String.equal entry.server.source Mcp_config.settings_source
      && not (Set.mem current key))
  in
  List.iter stale ~f:(fun (key, _) -> Hashtbl.remove t.entries key);
  List.iter stale ~f:(fun (_, entry) -> close_entry t entry)
;;

let servers
      t
      ?(reconnect = false)
      ?(settings = `Object [])
      ?tokens
      ~cwd
      ~home
      ()
  =
  set_tokens t tokens;
  prune_settings t ~home settings;
  let { Mcp_config.Discovered.servers; problems } =
    Mcp_config.discover ~settings ~cwd ~home ()
  in
  let statuses =
    Fiber.List.map
      (fun server ->
         let status : Status.t =
           if not (Mcp_config.is_approved ~home server)
           then Needs_approval
           else (
             match client t ~reconnect server with
             | Error e -> status_of_error e
             | Ok client ->
               (match Mcp_client.tools client with
                | Ok tools -> Ready tools
                | Error e ->
                  if Mcp_client.needs_sign_in client
                  then Needs_sign_in (Error.to_string_hum e)
                  else Failed (Error.to_string_hum e)))
         in
         { Server_status.server; status })
      servers
  in
  statuses, problems
;;

let call
      t
      ~cancel
      ~settings
      ?tokens
      ~source
      ~server:name
      ~home
      ~tool
      ~arguments
      ()
  =
  set_tokens t tokens;
  match Mcp_config.find ~settings ~home ~source name with
  | Error e -> Tool_result.error (Error.to_string_hum e)
  | Ok server when not (Mcp_config.is_approved ~home server) ->
    Tool_result.error
      (sprintf
         "the MCP server %S (from %s) is not approved; approve it with /mcp"
         name
         source)
  | Ok server ->
    (match
       Cancellation.protect cancel ~f:(fun () ->
         client t ~reconnect:false server)
     with
     | None -> Tool_result.error "[cancelled]"
     | Some (Error { message; needs_sign_in = true }) ->
       Tool_result.error
         (sprintf
            "the MCP server %S is not running: %s; the user can sign in with \
             /mcp signin %s (or in Settings → MCP servers)"
            name
            message
            name)
     | Some (Error { message; needs_sign_in = false }) ->
       Tool_result.error
         (sprintf
            "the MCP server %S is not running: %s; once that is fixed, \
             reconnect it with /mcp"
            name
            message)
     | Some (Ok client) -> Mcp_client.call client ~cancel ~tool ~arguments)
;;

let close t =
  t.closed <- true;
  let entries = Hashtbl.data t.entries in
  Hashtbl.clear t.entries;
  Fiber.List.iter
    (fun (entry : Entry.t) ->
       match Promise.peek entry.client with
       | Some (Ok client) -> Mcp_client.close client
       | Some (Error _) | None -> ())
    entries
;;
