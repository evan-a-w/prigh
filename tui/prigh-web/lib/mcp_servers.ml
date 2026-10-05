open! Core
open! Import

let id (s : Mcp_server.t) = s.source ^ "#" ^ s.name

let find (l : Mcp_list.t) ~id:wanted =
  List.find l.servers ~f:(fun s -> String.equal (id s) wanted)
;;

let none =
  "No MCP servers: configure them under \"mcpServers\" in ~/.prigh/mcp.json, \
   or in a project's .mcp.json."
;;

let tools n = Chat_html.plural n "tool"

let detail (s : Mcp_server.t) =
  let status =
    match s.status with
    | Ready -> sprintf "ready · %s" (tools (List.length s.tools))
    | Failed ->
      sprintf
        "failed: %s · Enter restarts it"
        (Option.value s.error ~default:"no reason given")
    | Needs_approval -> "needs your approval · Enter starts it"
  in
  sprintf "%s · %s" status s.source
;;

let rank : Mcp_server.Status.t -> int = function
  | Needs_approval -> 0
  | Failed -> 1
  | Ready -> 2
;;

let picker ?highlight (l : Mcp_list.t) =
  let servers =
    List.stable_sort l.servers ~compare:(fun (a : Mcp_server.t) b ->
      Int.compare (rank a.status) (rank b.status))
  in
  Dialog.Picker
    { kind = Mcp l
    ; picker =
        Picker.create
          ?highlight
          ~title:"MCP servers"
          (List.map servers ~f:(fun (s : Mcp_server.t) ->
             Picker.Item.create
               ~id:(id s)
               ~detail:(detail s)
               ~search:(s.name ^ " " ^ s.source)
               ~dimmed:(Mcp_server.Status.equal s.status Failed)
               s.name))
    }
;;

let summary (l : Mcp_list.t) =
  let with_status status =
    List.filter l.servers ~f:(fun (s : Mcp_server.t) ->
      Mcp_server.Status.equal s.status status)
  in
  let ready = with_status Ready in
  let failed = with_status Failed in
  let waiting = with_status Needs_approval in
  let parts =
    List.filter_opt
      [ (match ready with
         | [] -> None
         | ready ->
           Some
             (sprintf
                "%d ready (%s)"
                (List.length ready)
                (tools
                   (List.sum
                      (module Int)
                      ready
                      ~f:(fun (s : Mcp_server.t) -> List.length s.tools)))))
      ; (match failed with
         | [] -> None
         | failed ->
           Some
             (sprintf
                "%d failed (%s)"
                (List.length failed)
                (String.concat
                   ~sep:"; "
                   (List.map failed ~f:(fun (s : Mcp_server.t) ->
                      sprintf
                        "%s: %s"
                        s.name
                        (Option.value s.error ~default:"no reason given"))))))
      ; Option.some_if
          (not (List.is_empty waiting))
          (sprintf "%d awaiting approval" (List.length waiting))
      ; Option.some_if
          (not (List.is_empty l.problems))
          (Chat_html.plural (List.length l.problems) "configuration problem")
      ]
  in
  match parts with
  | [] -> none, `Error false
  | parts ->
    let wrong = not (List.is_empty failed && List.is_empty l.problems) in
    ( sprintf
        "MCP servers: %s%s"
        (String.concat ~sep:", " parts)
        (if wrong || not (List.is_empty waiting)
         then " — /mcp shows them"
         else "")
    , `Error wrong )
;;

let outcome l ~id =
  match find l ~id with
  | None ->
    "That MCP server is no longer configured: /mcp lists them.", `Error true
  | Some s ->
    (match s.status with
     | Ready ->
       ( sprintf
           "%s is ready with %s: Enter lists them"
           s.name
           (tools (List.length s.tools))
       , `Error false )
     | Failed ->
       ( sprintf
           "%s failed: %s. Fix it in %s, then /mcp reconnect."
           s.name
           (Option.value s.error ~default:"no reason given")
           s.source
       , `Error true )
     | Needs_approval ->
       ( sprintf "%s still needs approval: Enter in /mcp starts it." s.name
       , `Error true ))
;;
