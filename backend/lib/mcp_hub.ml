open! Core
open! Import

module Status = struct
  type t =
    | Ready of Mcp_client.Tool.t list
    | Failed of string
    | Needs_approval
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
    ; client : (Mcp_client.t, string) Result.t Promise.t
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
  ; mutable closed : bool
  }

let create ~env ~sw () =
  { env; sw; entries = String.Table.create (); closed = false }
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
    ~data:{ server; client };
  Fiber.fork ~sw:t.sw (fun () ->
    let result =
      match Mcp_client.connect ~env:t.env ~sw:t.sw server with
      | Ok client -> Ok client
      | Error e -> Error (Error.to_string_hum e)
      | exception (Eio.Cancel.Cancelled _ as exn) ->
        Promise.resolve resolver (Error "the MCP hub was shut down");
        raise exn
      | exception exn -> Error (Exn.to_string exn)
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
      (match Promise.peek entry.client with
       | None -> entry.client
       | Some (Ok client) when Option.is_none (Mcp_client.failure client) ->
         entry.client
       | Some (Ok _ | Error _) ->
         if reconnect then start t server else entry.client)
  in
  match Promise.await promise with
  | Error e -> Error e
  | Ok client ->
    (match Mcp_client.failure client with
     | None -> Ok client
     | Some failure -> Error failure)
;;

let servers t ?(reconnect = false) ~cwd ~home () =
  let { Mcp_config.Discovered.servers; problems } =
    Mcp_config.discover ~cwd ~home ()
  in
  let statuses =
    Fiber.List.map
      (fun server ->
         let status : Status.t =
           if not (Mcp_config.is_approved ~home server)
           then Needs_approval
           else (
             match client t ~reconnect server with
             | Error e -> Failed e
             | Ok client ->
               (match Mcp_client.tools client with
                | Ok tools -> Ready tools
                | Error e -> Failed (Error.to_string_hum e)))
         in
         { Server_status.server; status })
      servers
  in
  statuses, problems
;;

let call t ~cancel ~source ~server:name ~home ~tool ~arguments =
  match Mcp_config.find ~home ~source name with
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
     | Some (Error e) ->
       Tool_result.error
         (sprintf
            "the MCP server %S is not running: %s; once that is fixed, \
             reconnect it with /mcp"
            name
            e)
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
