open! Core
open! Import

module Action = struct
  type t =
    | Show
    | Set of string option
    | Error of string
  [@@deriving sexp_of, equal]
end

let usage = "usage: /default-host or /default-host NAME|off"

let parse rest =
  match String.strip rest with
  | "" -> Action.Show
  | "off" | "none" -> Set None
  | name when String.exists name ~f:Char.is_whitespace ->
    Error ("one host, by its name or id; " ^ usage)
  | name -> Set (Some name)
;;

let fields : Action.t -> _ = function
  | Set host ->
    [ ( "default_host"
      , Option.value_map host ~default:`Null ~f:(fun s -> `String s) )
    ]
  | Show | Error _ -> []
;;

let name ~hosts id =
  match List.find hosts ~f:(fun (h : P.Host.t) -> String.equal h.id id) with
  | Some h when String.equal h.id P.Host.backend_id -> h.name ^ " (the backend)"
  | Some h -> h.name
  | None -> id
;;

let summary ~hosts (config : P.Config.t) =
  match config.default_host with
  | Some id ->
    let connected =
      List.exists hosts ~f:(fun (h : P.Host.t) -> String.equal h.id id)
    in
    sprintf
      "new sessions from prigh-web start on %s%s (/default-host off: where \
       your session is; a terminal's start on its machine)"
      (name ~hosts id)
      (if connected
       then ""
       else ", not connected now: until it is, where your session is")
  | None ->
    "no default host: new sessions from prigh-web start where your session is, \
     a terminal's on its machine; /default-host NAME picks one"
;;
