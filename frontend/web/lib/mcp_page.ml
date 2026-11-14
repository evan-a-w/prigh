open! Core
open! Import

module Status = struct
  type t =
    | Unknown
    | Loading
    | Loaded
    | Failed of string
  [@@deriving sexp_of, equal]
end

module Pair = struct
  type t =
    { key : string
    ; value : string
    ; shown : bool
    }
  [@@deriving equal]

  let sexp_of_t { key; value; shown } =
    [%sexp
      (key : string)
      , (if shown || String.is_empty value then value else "<hidden>" : string)]
  ;;
end

module Kind = struct
  type t =
    | Stdio
    | Http
  [@@deriving sexp_of, equal]
end

module Draft = struct
  type t =
    { editing : string option
    ; name : string
    ; kind : Kind.t
    ; command : string
    ; args : string
    ; url : string
    ; pairs : Pair.t list
    ; saving : bool
    ; error : string option
    }
  [@@deriving sexp_of, equal]

  let empty =
    { editing = None
    ; name = ""
    ; kind = Stdio
    ; command = ""
    ; args = ""
    ; url = ""
    ; pairs = []
    ; saving = false
    ; error = None
    }
  ;;

  let of_server (s : Mcp_settings.Server.t) =
    let pairs l =
      List.map l ~f:(fun (key, value) -> { Pair.key; value; shown = false })
    in
    match s.transport with
    | Stdio { command; args; env } ->
      { empty with
        editing = Some s.name
      ; name = s.name
      ; command
      ; args = String.concat ~sep:"\n" args
      ; pairs = pairs env
      }
    | Http { url; headers } ->
      { empty with
        editing = Some s.name
      ; name = s.name
      ; kind = Http
      ; url
      ; pairs = pairs headers
      }
  ;;

  let transport t : (Mcp_settings.Transport.t, string) Result.t =
    let pairs =
      List.filter_map t.pairs ~f:(fun (p : Pair.t) ->
        let key = String.strip p.key in
        if String.is_empty key && String.is_empty p.value
        then None
        else Some (key, p.value))
    in
    let what = match t.kind with Stdio -> "variable" | Http -> "header" in
    if List.exists pairs ~f:(fun (k, _) -> String.is_empty k)
    then Error (sprintf "give every %s a name, or clear its value" what)
    else (
      match t.kind with
      | Stdio ->
        let command = String.strip t.command in
        if String.is_empty command
        then Error "give the command that starts the server, e.g. npx"
        else (
          let args =
            String.split_lines t.args
            |> List.map ~f:String.strip
            |> List.filter ~f:(Fn.non String.is_empty)
          in
          Ok (Stdio { command; args; env = pairs }))
      | Http ->
        let url = String.strip t.url in
        if String.is_empty url
        then Error "give the server's URL, e.g. https://example.com/mcp"
        else Ok (Http { url; headers = pairs }))
  ;;
end

module Paste = struct
  type t =
    { text : string
    ; saving : bool
    ; error : string option
    }
  [@@deriving sexp_of, equal]
end

module Field = struct
  type t =
    | Name of string
    | Kind of Kind.t
    | Command of string
    | Args of string
    | Url of string
    | Pair_key of int * string
    | Pair_value of int * string
    | Pair_shown of int
    | Pair_add
    | Pair_remove of int
  [@@deriving sexp_of, equal]
end

module Health = struct
  type t =
    | Off
    | Checking
    | Ready of Mcp_server.Tool.t list
    | Failed of string
    | Overridden of string
    | Not_listed
  [@@deriving sexp_of, equal]
end

type t =
  { status : Status.t
  ; servers : Mcp_settings.t
  ; host : Mcp_list.t option
  ; host_error : string option
  ; draft : Draft.t option
  ; paste : Paste.t option
  ; removing : string option
  ; expanded : String.Set.t
  ; busy : String.Set.t
  ; error : string option
  }
[@@deriving sexp_of, equal]

let empty =
  { status = Unknown
  ; servers = []
  ; host = None
  ; host_error = None
  ; draft = None
  ; paste = None
  ; removing = None
  ; expanded = String.Set.empty
  ; busy = String.Set.empty
  ; error = None
  }
;;

let loading t =
  let status : Status.t =
    match t.status with
    | Loaded -> Loaded
    | Unknown | Loading | Failed _ -> Loading
  in
  { t with status; host = None; host_error = None }
;;

let has servers name = Option.is_some (Mcp_settings.find servers name)

let changed t servers =
  let draft =
    Option.filter t.draft ~f:(fun (d : Draft.t) ->
      Option.for_all d.editing ~f:(has servers))
  in
  let removing = Option.filter t.removing ~f:(has servers) in
  { t with status = Loaded; servers; draft; removing }
;;

let loaded t = function
  | Ok servers -> changed t servers
  | Error e -> { t with status = Failed e }
;;

let host_listed t = function
  | Ok l -> { t with host = Some l; host_error = None; busy = String.Set.empty }
  | Error e -> { t with host_error = Some e; busy = String.Set.empty }
;;

let health t (s : Mcp_settings.Server.t) : Health.t =
  if not s.enabled
  then Off
  else (
    match t.host with
    | None -> if Option.is_some t.host_error then Not_listed else Checking
    | Some l ->
      (match
         List.find l.servers ~f:(fun (h : Mcp_server.t) ->
           String.equal h.name s.name)
       with
       | None -> Not_listed
       | Some h when not (String.equal h.source Mcp_settings.source) ->
         Overridden h.source
       | Some h ->
         (match h.status with
          | Ready -> Ready h.tools
          | Failed ->
            Failed (Option.value h.error ~default:"it did not start")
          | Needs_approval -> Not_listed)))
;;

let host_servers t =
  match t.host with
  | None -> []
  | Some l ->
    List.filter l.servers ~f:(fun (h : Mcp_server.t) ->
      not (String.equal h.source Mcp_settings.source))
;;

let edit t name =
  let draft =
    match name with
    | None -> Some Draft.empty
    | Some name ->
      Option.map (Mcp_settings.find t.servers name) ~f:Draft.of_server
  in
  { t with draft; paste = None; removing = None; error = None }
;;

let update_pair (d : Draft.t) i ~f =
  { d with pairs = List.mapi d.pairs ~f:(fun j p -> if i = j then f p else p) }
;;

let set t (field : Field.t) =
  match t.draft with
  | None -> t
  | Some d ->
    let d =
      match field with
      | Name name -> { d with name }
      | Kind kind ->
        if Kind.equal kind d.kind then d else { d with kind; pairs = [] }
      | Command command -> { d with command }
      | Args args -> { d with args }
      | Url url -> { d with url }
      | Pair_key (i, key) -> update_pair d i ~f:(fun p -> { p with key })
      | Pair_value (i, value) -> update_pair d i ~f:(fun p -> { p with value })
      | Pair_shown i ->
        update_pair d i ~f:(fun p -> { p with shown = not p.shown })
      | Pair_add ->
        { d with pairs = d.pairs @ [ { key = ""; value = ""; shown = true } ] }
      | Pair_remove i ->
        { d with pairs = List.filteri d.pairs ~f:(fun j _ -> j <> i) }
    in
    { t with draft = Some { d with error = None } }
;;

let cancel t = { t with draft = None; paste = None }

let save t =
  match t.draft with
  | None | Some { saving = true; _ } -> t, None
  | Some d ->
    let name = String.strip d.name in
    let refuse error = { t with draft = Some { d with error = Some error } }, None in
    if String.is_empty name
    then refuse "give the server a name: its tools are called mcp__NAME__TOOL"
    else (
      match Draft.transport d with
      | Error e -> refuse e
      | Ok transport ->
        let request : Request.Method.t =
          match d.editing with
          | None -> Add_mcp_servers [ name, transport ]
          | Some old ->
            Update_mcp_server
              { name = old
              ; rename = (if String.equal old name then None else Some name)
              ; transport
              }
        in
        ( { t with draft = Some { d with saving = true; error = None } }
        , Some request ))
;;

let saved t = function
  | Ok () -> { t with draft = None }
  | Error e ->
    { t with
      draft =
        Option.map t.draft ~f:(fun d -> { d with saving = false; error = Some e })
    }
;;

let open_paste t =
  { t with
    paste = Some { text = ""; saving = false; error = None }
  ; draft = None
  ; removing = None
  ; error = None
  }
;;

let set_paste t text =
  { t with
    paste = Option.map t.paste ~f:(fun p -> { p with text; error = None })
  }
;;

let save_paste t =
  match t.paste with
  | None | Some { saving = true; _ } -> t, None
  | Some p ->
    let text = String.strip p.text in
    if String.is_empty text
    then
      ( { t with
          paste =
            Some
              { p with
                error =
                  Some
                    {|paste an mcpServers object, e.g. {"docs": {"url": "https://example.com/mcp"}}|}
              }
        }
      , None )
    else
      ( { t with paste = Some { p with saving = true; error = None } }
      , Some (Request.Method.Add_mcp_servers_text { text }) )
;;

let pasted t = function
  | Ok () -> { t with paste = None }
  | Error e ->
    { t with
      paste =
        Option.map t.paste ~f:(fun p -> { p with saving = false; error = Some e })
    }
;;

let ask_remove t name =
  { t with removing = Some name; draft = None; paste = None; error = None }
;;

let cancel_remove t = { t with removing = None }

let remove t =
  match t.removing with
  | None -> t, None
  | Some name ->
    { t with removing = None }, Some (Request.Method.Remove_mcp_server { name })
;;

let toggle_tools t name =
  { t with
    expanded =
      (if Set.mem t.expanded name
       then Set.remove t.expanded name
       else Set.add t.expanded name)
  }
;;

let set_busy t name busy =
  { t with
    busy = (if busy then Set.add t.busy name else Set.remove t.busy name)
  }
;;

let toggle_enabled t name =
  match Mcp_settings.find t.servers name with
  | None -> t, None
  | Some s ->
    ( set_busy { t with error = None } name true
    , Some
        (Request.Method.Set_mcp_server_enabled { name; enabled = not s.enabled })
    )
;;

let failed t e = { t with error = Some e }

let escape t =
  if Option.is_some t.removing
  then Some (cancel_remove t)
  else if Option.is_some t.paste
  then Some { t with paste = None }
  else if Option.is_some t.draft
  then Some (cancel t)
  else None
;;
