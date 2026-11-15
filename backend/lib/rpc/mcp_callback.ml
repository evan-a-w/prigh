open! Core
open! Import

let route (request : Web_server.Request.t) =
  Option.some_if
    (String.equal request.path Mcp_auth.callback_path)
    (fun (request : Web_server.Request.t) ~reader:_ flow ->
       Web_server.respond flow (Mcp_auth.callback request.query))
;;

let listen ~env ~sw =
  Web_server.listen
    ~env
    ~sw
    ~routes:route
    ~addr:Eio.Net.Ipaddr.V4.loopback
    ~port:0
    ~root:None
    ~websockets:[]
    ~on_lines:(fun ~read_line:_ ~write_line:_ -> ())
    ()
;;
