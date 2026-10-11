open! Core
open! Import

module Dest = struct
  type t =
    | Here
    | Chat
  [@@deriving sexp_of, equal]
end

module Local_file = struct
  type t =
    { file : int
    ; name : string
    ; size : int
    }
  [@@deriving sexp_of, equal]
end

module Sort = struct
  module Key = struct
    type t =
      | Name
      | Size
      | Modified
    [@@deriving sexp_of, equal, enumerate]

    let label = function
      | Name -> "Name"
      | Size -> "Size"
      | Modified -> "Modified"
    ;;
  end

  type t =
    { key : Key.t
    ; descending : bool
    }
  [@@deriving sexp_of, equal]
end

module Preview = struct
  type t =
    | Loading of string
    | Shown of File_preview.t
    | Failed of
        { path : string
        ; error : string
        }
  [@@deriving sexp_of, equal]
end

module Upload = struct
  type t =
    { local : Local_file.t
    ; dir : string
    ; sent : int option
    }
  [@@deriving sexp_of, equal]
end

module Attachment = struct
  module Status = struct
    type t =
      | Uploading of int option
      | Ready of string
      | Failed of string
    [@@deriving sexp_of, equal]
  end

  type t =
    { file : int option
    ; name : string
    ; size : int option
    ; status : Status.t
    }
  [@@deriving sexp_of, equal]
end

module Ask = struct
  type t =
    | New_folder of
        { dir : string
        ; input : string
        ; error : string option
        ; busy : bool
        }
    | Rename of
        { path : string
        ; input : string
        ; error : string option
        ; busy : bool
        }
    | Delete of
        { path : string
        ; dir : bool
        ; error : string option
        ; busy : bool
        }
    | Overwrite of Upload.t
  [@@deriving sexp_of, equal]
end

module Where = struct
  type t =
    { session : string
    ; host : string
    ; host_name : string
    ; cwd : string
    }
  [@@deriving sexp_of, equal]
end

type t =
  { open_ : bool
  ; base : string
  ; where : Where.t option
  ; listing : File_listing.t option
  ; loading : string option
  ; error : string option
  ; editing_path : string option
  ; hidden : bool
  ; sort : Sort.t
  ; selected : string option
  ; preview : Preview.t option
  ; uploads : Upload.t list
  ; conflicts : Upload.t list
  ; ask : Ask.t option
  ; attachments : Attachment.t list
  ; busy : string option
  }
[@@deriving sexp_of]

let closed =
  { open_ = false
  ; base = ""
  ; where = None
  ; listing = None
  ; loading = None
  ; error = None
  ; editing_path = None
  ; hidden = false
  ; sort = { key = Name; descending = false }
  ; selected = None
  ; preview = None
  ; uploads = []
  ; conflicts = []
  ; ask = None
  ; attachments = []
  ; busy = None
  }
;;

module Page = struct
  type t =
    | Pick of Dest.t
    | Upload of
        { file : int
        ; url : string
        }
    | Abort_upload of int
    | Forget of int
    | Download of
        { url : string
        ; name : string
        }
    | Focus of string
  [@@deriving sexp_of, equal]
end

module Reply = struct
  type t =
    | Listed of string
    | Previewed of string
    | Upload_link of
        { file : int
        ; overwrite : bool
        }
    | Download_link
    | Changed of
        { notice : string
        ; select : string option
        }
  [@@deriving sexp_of, equal]
end

module Out = struct
  type t =
    | Rpc of
        { method_ : string
        ; params : (string * Json.t) list
        ; reply : Reply.t
        }
    | Page of Page.t
    | Toast of
        { text : string
        ; error : bool
        }
    | Copy of string
    | Insert of string
    | Focus_editor
  [@@deriving sexp_of]
end

module Action = struct
  type t =
    | Set_base of string
    | Toggle
    | Open of string option
    | Close
    | Go of string
    | Up
    | Refresh
    | Edit_path of string option
    | Submit_path
    | Toggle_hidden
    | Sort_by of Sort.Key.t
    | Select of string
    | Move of int
    | Open_selected
    | Close_preview
    | Download of string
    | Pick of Dest.t
    | Chosen of
        { dest : Dest.t
        ; files : Local_file.t list
        }
    | Upload_progress of
        { file : int
        ; sent : int
        }
    | Upload_done of
        { file : int
        ; result : (Json.t, string) Result.t
        }
    | New_folder
    | Rename of string
    | Delete of string
    | Ask_input of string
    | Ask_accept
    | Ask_cancel
    | Copy_path of string
    | Insert_path of string
    | Attach of string
    | Remove_attachment of int
  [@@deriving sexp_of]
end

let asking t = Option.is_some t.ask
let list_id = "files-list"

(* ---- paths *)

let join dir name =
  if String.equal dir "/" then "/" ^ name else dir ^ "/" ^ name
;;

let current_dir t =
  match t.listing, t.where with
  | Some l, _ -> Some l.path
  | None, Some w -> Some w.cwd
  | None, None -> None
;;

let resolve t path =
  let path = String.strip path in
  if String.is_prefix path ~prefix:"/" || String.is_prefix path ~prefix:"~"
  then path
  else (
    match current_dir t with
    | Some dir when String.is_empty path -> dir
    | Some dir -> join dir path
    | None -> path)
;;

let home t = Option.map t.listing ~f:(fun l -> l.home)

let display_path t path =
  match home t with
  | Some home when String.equal path home -> "~"
  | Some home when (not (String.equal home "/")) && String.is_prefix path ~prefix:(home ^ "/")
    -> "~" ^ String.drop_prefix path (String.length home)
  | _ -> path
;;

(* Relative to the session's directory when inside it, for the composer. *)
let mention t path =
  match t.where with
  | Some { cwd; _ } when String.is_prefix path ~prefix:(cwd ^ "/") ->
    String.drop_prefix path (String.length cwd + 1)
  | _ -> path
;;

let http_base backend =
  let scheme, rest =
    match String.lsplit2 backend ~on:':' with
    | Some ("wss", rest) -> "https", rest
    | Some ("ws", rest) -> "http", rest
    | Some ("https", rest) -> "https", rest
    | Some ("http", rest) -> "http", rest
    | _ -> "", backend
  in
  if String.is_empty scheme
  then ""
  else (
    let rest = String.chop_prefix rest ~prefix:"//" |> Option.value ~default:rest in
    let authority =
      String.take_while rest ~f:(fun c -> not (Char.equal c '/' || Char.equal c '?'))
    in
    sprintf "%s://%s" scheme authority)
;;

let link t path = t.base ^ path

(* ---- the listing *)

let entry_named t name =
  Option.bind t.listing ~f:(fun l ->
    List.find l.entries ~f:(fun (e : File_listing.Entry.t) -> String.equal e.name name))
;;

let entries t =
  match t.listing with
  | None -> []
  | Some l ->
    let compare_key (a : File_listing.Entry.t) (b : File_listing.Entry.t) =
      match t.sort.key with
      | Name -> String.Caseless.compare a.name b.name
      | Size -> Option.compare Int.compare a.size b.size
      | Modified -> Option.compare Time_ns.compare a.mtime b.mtime
    in
    let compare a b =
      let c = compare_key a b in
      let c = if c = 0 then String.compare a.name b.name else c in
      if t.sort.descending then -c else c
    in
    let dirs, files =
      List.partition_tf l.entries ~f:(fun (e : File_listing.Entry.t) ->
        File_listing.Kind.is_dir e.kind)
    in
    List.sort dirs ~compare @ List.sort files ~compare
;;

let list t path =
  let key = Option.value path ~default:"" in
  ( { t with loading = Some key; error = None; editing_path = None }
  , [ Out.Rpc
        { method_ = "fs_list"
        ; params =
            Option.value_map path ~default:[] ~f:(fun p -> [ "path", `String p ])
            @ [ "hidden", Json.bool t.hidden ]
        ; reply = Listed key
        }
    ] )
;;

let refresh t = list t (current_dir t)

let preview t path =
  ( { t with preview = Some (Loading path) }
  , [ Out.Rpc
        { method_ = "fs_preview"
        ; params = [ "path", `String path ]
        ; reply = Previewed path
        }
    ] )
;;

let toast ?(error = false) text = Out.Toast { text; error }

let open_ t path =
  let t = { t with open_ = true } in
  let t, cmds =
    match path, t.listing with
    | Some path, _ -> list t (Some (resolve t path))
    | None, None -> list t None
    | None, Some _ -> t, []
  in
  t, cmds @ [ Out.Page (Focus list_id) ]
;;

let close t =
  { t with open_ = false; ask = None; editing_path = None }, [ Out.Focus_editor ]
;;

let select t name =
  match entry_named t name, current_dir t with
  | Some entry, Some dir when File_listing.Kind.is_dir entry.kind ->
    list { t with selected = None; preview = None } (Some (join dir name))
  | Some _, Some dir -> preview { t with selected = Some name } (join dir name)
  | _ -> t, []
;;

let move t by =
  match entries t with
  | [] -> t, []
  | entries ->
    let n = List.length entries in
    let index =
      match
        Option.bind t.selected ~f:(fun name ->
          List.findi entries ~f:(fun _ e -> String.equal e.name name))
      with
      | Some (i, _) -> Int.clamp_exn (i + by) ~min:0 ~max:(n - 1)
      | None -> if by > 0 then 0 else n - 1
    in
    let entry = List.nth_exn entries index in
    (* Moving shows files as it goes; directories open on Enter. *)
    if File_listing.Kind.is_dir entry.kind
    then { t with selected = Some entry.name; preview = None }, []
    else (
      match current_dir t with
      | Some dir -> preview { t with selected = Some entry.name } (join dir entry.name)
      | None -> { t with selected = Some entry.name }, [])
;;

let up t =
  match t.listing with
  | Some { parent = Some parent; path; _ } ->
    let t, cmds = list { t with preview = None } (Some parent) in
    (* Back in the parent, on the directory we came from. *)
    { t with selected = Some (Filename.basename path) }, cmds
  | _ -> t, []
;;

(* ---- transfers *)

let replace_attachment t file ~f =
  { t with
    attachments =
      List.map t.attachments ~f:(fun (a : Attachment.t) ->
        if Option.equal Int.equal a.file (Some file) then f a else a)
  }
;;

let replace_upload t file ~f =
  { t with
    uploads =
      List.map t.uploads ~f:(fun (u : Upload.t) ->
        if u.local.file = file then f u else u)
  }
;;

let upload_link (u : Upload.t) ~overwrite =
  Out.Rpc
    { method_ = "fs_upload"
    ; params =
        [ "name", `String u.local.name
        ; "size", Json.int u.local.size
        ; "dir", `String u.dir
        ]
        @ if overwrite then [ "overwrite", Json.bool true ] else []
    ; reply = Upload_link { file = u.local.file; overwrite }
    }
;;

let chosen t ~(dest : Dest.t) (files : Local_file.t list) =
  match dest, files with
  | _, [] -> t, []
  | Chat, files ->
    let attachments =
      List.map files ~f:(fun (f : Local_file.t) ->
        { Attachment.file = Some f.file
        ; name = f.name
        ; size = Some f.size
        ; status = Uploading None
        })
    in
    ( { t with attachments = t.attachments @ attachments }
    , List.map files ~f:(fun (f : Local_file.t) ->
        Out.Rpc
          { method_ = "fs_upload"
          ; params = [ "name", `String f.name; "size", Json.int f.size ]
          ; reply = Upload_link { file = f.file; overwrite = false }
          }) )
  | Here, files ->
    (match current_dir t with
     | None ->
       ( t
       , toast ~error:true "Open the file browser on a directory first (Ctrl+Shift+F)."
         :: List.map files ~f:(fun f -> Out.Page (Forget f.file)) )
     | Some dir ->
       let uploads =
         List.map files ~f:(fun local -> { Upload.local; dir; sent = None })
       in
       ( { t with uploads = t.uploads @ uploads }
       , List.map uploads ~f:(upload_link ~overwrite:false) ))
;;

let next_conflict t =
  match t.ask, t.conflicts with
  | None, u :: rest -> { t with ask = Some (Overwrite u); conflicts = rest }
  | _ -> t
;;

let upload_link_reply t ~file ~overwrite result =
  let attachment =
    List.find t.attachments ~f:(fun a -> Option.equal Int.equal a.file (Some file))
  in
  let upload = List.find t.uploads ~f:(fun u -> u.local.file = file) in
  match attachment, upload, result with
  | None, None, _ -> t, [ Out.Page (Forget file) ]
  | Some _, _, Ok json ->
    (match Json.string_field json "url" with
     | Ok url -> t, [ Out.Page (Upload { file; url = link t url }) ]
     | Error e ->
       ( replace_attachment t file ~f:(fun a ->
           { a with status = Failed (Error.to_string_hum e) })
       , [ Out.Page (Forget file) ] ))
  | Some a, _, Error e ->
    ( replace_attachment t file ~f:(fun a -> { a with status = Failed e })
    , [ toast ~error:true (sprintf "Could not attach %s: %s" a.name e)
      ; Out.Page (Forget file)
      ] )
  | None, Some _, Ok json ->
    (match Json.string_field json "url" with
     | Ok url ->
       ( replace_upload t file ~f:(fun u -> { u with sent = Some 0 })
       , [ Out.Page (Upload { file; url = link t url }) ] )
     | Error e ->
       ( { t with uploads = List.filter t.uploads ~f:(fun u -> u.local.file <> file) }
       , [ toast ~error:true (Error.to_string_hum e); Out.Page (Forget file) ] ))
  | None, Some u, Error e
    when (not overwrite) && String.is_substring e ~substring:"already exists" ->
    let t =
      { t with
        uploads = List.filter t.uploads ~f:(fun u -> u.local.file <> file)
      ; conflicts = t.conflicts @ [ u ]
      }
    in
    next_conflict t, [ Out.Page (Focus "files-dialog") ]
  | None, Some u, Error e ->
    ( { t with uploads = List.filter t.uploads ~f:(fun u -> u.local.file <> file) }
    , [ toast ~error:true (sprintf "Could not upload %s: %s" u.local.name e)
      ; Out.Page (Forget file)
      ] )
;;

let upload_done t ~file result =
  let error_text json_or_text =
    match Json.parse json_or_text with
    | Ok json ->
      (match Json.string_field json "error" with
       | Ok e -> e
       | Error _ -> json_or_text)
    | Error _ -> json_or_text
  in
  match
    ( List.find t.attachments ~f:(fun a -> Option.equal Int.equal a.file (Some file))
    , List.find t.uploads ~f:(fun u -> u.local.file = file) )
  with
  | Some _, _ ->
    (match result with
     | Ok json ->
       (match Json.string_field json "path" with
        | Ok path ->
          replace_attachment t file ~f:(fun a -> { a with status = Ready path }), []
        | Error e ->
          ( replace_attachment t file ~f:(fun a ->
              { a with status = Failed (Error.to_string_hum e) })
          , [] ))
     | Error e ->
       let e = error_text e in
       ( replace_attachment t file ~f:(fun a -> { a with status = Failed e })
       , [ toast ~error:true (sprintf "Upload failed: %s" e) ] ))
  | None, Some u ->
    let t =
      { t with uploads = List.filter t.uploads ~f:(fun u -> u.local.file <> file) }
    in
    (match result with
     | Ok _ ->
       let shown =
         Option.value_map (current_dir t) ~default:false ~f:(String.equal u.dir)
       in
       let t, cmds =
         if shown && t.open_
         then (
           let t, cmds = refresh t in
           { t with selected = Some u.local.name }, cmds)
         else t, []
       in
       t, toast (sprintf "Uploaded %s" u.local.name) :: cmds
     | Error e ->
       ( t
       , [ toast
             ~error:true
             (sprintf "Could not upload %s: %s" u.local.name (error_text e))
         ] ))
  | None, None -> t, []
;;

let remove_attachment t i =
  match List.nth t.attachments i with
  | None -> t, []
  | Some a ->
    ( { t with attachments = List.filteri t.attachments ~f:(fun j _ -> j <> i) }
    , match a.file, a.status with
      | Some file, Uploading _ -> [ Out.Page (Abort_upload file) ]
      | _ -> [] )
;;

let prompt_attachments t =
  let uploading =
    List.count t.attachments ~f:(fun a ->
      match a.status with
      | Uploading _ -> true
      | Ready _ | Failed _ -> false)
  in
  let failed =
    List.filter_map t.attachments ~f:(fun a ->
      match a.status with
      | Failed _ -> Some a.name
      | Uploading _ | Ready _ -> None)
  in
  if uploading > 0
  then
    Or_error.errorf
      "Wait for %s to upload (or remove %s with ×)."
      (if uploading = 1 then "the attachment" else sprintf "%d attachments" uploading)
      (if uploading = 1 then "it" else "them")
  else if not (List.is_empty failed)
  then
    Or_error.errorf
      "%s could not be uploaded: remove %s (×) and attach %s again."
      (String.concat ~sep:", " failed)
      (if List.length failed = 1 then "it" else "them")
      (if List.length failed = 1 then "it" else "them")
  else
    Ok
      (List.filter_map t.attachments ~f:(fun a ->
         match a.status with
         | Ready path -> Some path
         | Uploading _ | Failed _ -> None))
;;

let sent t = { t with attachments = [] }
let restore t attachments = { t with attachments = attachments @ t.attachments }

(* ---- dialogs *)

let check_name name =
  if String.is_empty name
  then Some "Type a name."
  else if String.equal name "." || String.equal name ".."
  then Some (sprintf "%S is not a name: type another." name)
  else if String.mem name '/'
  then Some "A name cannot contain /: type it without."
  else None
;;

let ask_accept t =
  match t.ask with
  | None -> t, []
  | Some (New_folder ({ dir; input; _ } as a)) ->
    let name = String.strip input in
    (match check_name name with
     | Some error -> { t with ask = Some (New_folder { a with error = Some error }) }, []
     | None ->
       ( { t with ask = Some (New_folder { a with busy = true; error = None }) }
       , [ Out.Rpc
             { method_ = "fs_mkdir"
             ; params = [ "path", `String (join dir name) ]
             ; reply = Changed { notice = sprintf "Created %s" name; select = Some name }
             }
         ] ))
  | Some (Rename ({ path; input; _ } as a)) ->
    let name = String.strip input in
    (match check_name name with
     | Some error -> { t with ask = Some (Rename { a with error = Some error }) }, []
     | None when String.equal name (Filename.basename path) -> { t with ask = None }, []
     | None ->
       ( { t with ask = Some (Rename { a with busy = true; error = None }) }
       , [ Out.Rpc
             { method_ = "fs_rename"
             ; params =
                 [ "from", `String path
                 ; "to", `String (join (Filename.dirname path) name)
                 ]
             ; reply =
                 Changed
                   { notice = sprintf "Renamed %s to %s" (Filename.basename path) name
                   ; select = Some name
                   }
             }
         ] ))
  | Some (Delete ({ path; _ } as a)) ->
    ( { t with ask = Some (Delete { a with busy = true; error = None }) }
    , [ Out.Rpc
          { method_ = "fs_delete"
          ; params = [ "path", `String path ]
          ; reply = Changed { notice = sprintf "Deleted %s" (Filename.basename path); select = None }
          }
      ] )
  | Some (Overwrite u) ->
    let t = next_conflict { t with ask = None; uploads = t.uploads @ [ u ] } in
    t, [ upload_link u ~overwrite:true ]
;;

let ask_cancel t =
  match t.ask with
  | None -> t, []
  | Some (Overwrite u) ->
    next_conflict { t with ask = None }, [ Out.Page (Forget u.local.file) ]
  | Some (New_folder _ | Rename _ | Delete _) ->
    { t with ask = None }, [ Out.Page (Focus list_id) ]
;;

let ask_input t input =
  match t.ask with
  | Some (New_folder a) -> { t with ask = Some (New_folder { a with input; error = None }) }
  | Some (Rename a) -> { t with ask = Some (Rename { a with input; error = None }) }
  | Some (Delete _ | Overwrite _) | None -> t
;;

let with_entry t name ~f =
  match entry_named t name, current_dir t with
  | Some entry, Some dir -> f entry (join dir name)
  | _ -> t, [ toast ~error:true (sprintf "%s is not listed here: refresh the list." name) ]
;;

let download t name =
  with_entry t name ~f:(fun entry path ->
    let archive = File_listing.Kind.is_dir entry.kind in
    ( { t with
        busy =
          Some
            (if archive
             then sprintf "Packing %s as .tar.gz…" name
             else sprintf "Preparing %s…" name)
      }
    , [ Out.Rpc
          { method_ = "fs_download"
          ; params = [ "path", `String path ] @ if archive then [ "archive", Json.bool true ] else []
          ; reply = Download_link
          }
      ] ))
;;

let attach t name =
  with_entry t name ~f:(fun entry path ->
    if File_listing.Kind.is_dir entry.kind
    then
      ( t
      , [ toast
            ~error:true
            (sprintf
               "%s is a directory: attach its files, or insert its path (@) for \
                the agent to look at."
               name)
        ] )
    else (
      let a = { Attachment.file = None; name; size = entry.size; status = Ready path } in
      ( { t with attachments = t.attachments @ [ a ] }
      , [ toast (sprintf "Attached %s to the next message" name) ] )))
;;

let key t ~key ~shift ~ctrl ~alt : Action.t option =
  let plain = not (shift || ctrl || alt) in
  match t.ask, key with
  | Some _, "Escape" -> Some Ask_cancel
  | Some _, "Enter" when plain -> Some Ask_accept
  | Some _, _ -> None
  | None, "ArrowDown" when plain -> Some (Move 1)
  | None, "ArrowUp" when plain -> Some (Move (-1))
  | None, "ArrowUp" when alt && not (shift || ctrl) -> Some Up
  | None, ("Enter" | "ArrowRight") when plain -> Some Open_selected
  | None, ("Backspace" | "ArrowLeft") when plain -> Some Up
  | None, "Delete" when plain -> Option.map t.selected ~f:(fun n -> Action.Delete n)
  | None, "F2" when plain -> Option.map t.selected ~f:(fun n -> Action.Rename n)
  | None, "Escape" -> Some (if Option.is_some t.preview then Close_preview else Close)
  | _ -> None
;;

let toggle_key ~code ~shift ~ctrl ~alt ~meta =
  ctrl && shift && (not (alt || meta)) && String.equal code "KeyF"
;;

let update t (action : Action.t) =
  match action with
  | Set_base base -> { t with base }, []
  | Toggle -> if t.open_ then close t else open_ t None
  | Open path -> open_ t path
  | Close -> close t
  | Go path -> list { t with preview = None; selected = None } (Some (resolve t path))
  | Up -> up t
  | Refresh -> refresh t
  | Edit_path editing ->
    ( { t with editing_path = editing }
    , if Option.is_some editing then [ Out.Page (Focus "files-path") ] else [] )
  | Submit_path ->
    (match t.editing_path with
     | Some path when not (String.is_empty (String.strip path)) ->
       list { t with preview = None; selected = None } (Some (resolve t path))
     | _ -> { t with editing_path = None }, [])
  | Toggle_hidden ->
    let t = { t with hidden = not t.hidden } in
    refresh t
  | Sort_by key ->
    let descending =
      if Sort.Key.equal key t.sort.key
      then not t.sort.descending
      else (
        match key with
        | Name -> false
        | Size | Modified -> true)
    in
    { t with sort = { key; descending } }, []
  | Select name -> select t name
  | Move by -> move t by
  | Open_selected ->
    (match t.selected with
     | Some name -> select t name
     | None -> move t 1)
  | Close_preview -> { t with preview = None }, [ Out.Page (Focus list_id) ]
  | Download name -> download t name
  | Pick dest -> t, [ Out.Page (Pick dest) ]
  | Chosen { dest; files } -> chosen t ~dest files
  | Upload_progress { file; sent } ->
    let t =
      replace_attachment t file ~f:(fun a ->
        match a.status with
        | Uploading _ -> { a with status = Uploading (Some sent) }
        | Ready _ | Failed _ -> a)
    in
    replace_upload t file ~f:(fun u -> { u with sent = Some sent }), []
  | Upload_done { file; result } -> upload_done t ~file result
  | New_folder ->
    (match current_dir t with
     | Some dir ->
       ( { t with ask = Some (New_folder { dir; input = ""; error = None; busy = false }) }
       , [ Out.Page (Focus "files-ask-input") ] )
     | None -> t, [])
  | Rename name ->
    with_entry t name ~f:(fun _ path ->
      ( { t with ask = Some (Rename { path; input = name; error = None; busy = false }) }
      , [ Out.Page (Focus "files-ask-input") ] ))
  | Delete name ->
    with_entry t name ~f:(fun entry path ->
      ( { t with
          ask =
            Some
              (Delete
                 { path
                 ; dir = File_listing.Kind.is_dir entry.kind
                 ; error = None
                 ; busy = false
                 })
        }
      , [ Out.Page (Focus "files-dialog") ] ))
  | Ask_input input -> ask_input t input, []
  | Ask_accept -> ask_accept t
  | Ask_cancel -> ask_cancel t
  | Copy_path path -> t, [ Out.Copy path; toast (sprintf "Copied %s" (display_path t path)) ]
  | Insert_path path -> t, [ Out.Insert ("@" ^ mention t path) ]
  | Attach name -> attach t name
  | Remove_attachment i -> remove_attachment t i
;;

let reply t (reply : Reply.t) result =
  match reply, result with
  | Listed key, _ when not ([%equal: string option] t.loading (Some key)) -> t, []
  | Listed _, Ok json ->
    (match File_listing.of_json json with
     | Error e -> { t with loading = None; error = Some (Error.to_string_hum e) }, []
     | Ok listing ->
       let still name =
         List.exists listing.entries ~f:(fun e -> String.equal e.name name)
       in
       let moved =
         Option.value_map t.listing ~default:true ~f:(fun l ->
           not (String.equal l.path listing.path))
       in
       let selected =
         match listing.select with
         | Some name -> Some name
         | None -> Option.filter t.selected ~f:still
       in
       let shown =
         match t.preview with
         | Some _ when moved -> None
         | shown -> shown
       in
       let t =
         { t with listing = Some listing; loading = None; error = None; selected; preview = shown }
       in
       (match listing.select with
        | Some name -> preview t (join listing.path name)
        | None -> t, []))
  | Listed path, Error e ->
    ( { t with loading = None; error = Some e }
    , if String.is_empty path then [] else [ Out.Page (Focus "files-path") ] )
  | Previewed path, _
    when not
           (match t.preview with
            | Some (Loading p) -> String.equal p path
            | _ -> false) -> t, []
  | Previewed _, Ok json ->
    (match File_preview.of_json json with
     | Ok p -> { t with preview = Some (Shown p) }, []
     | Error e ->
       ( { t with
           preview =
             Some (Failed { path = ""; error = Error.to_string_hum e })
         }
       , [] ))
  | Previewed path, Error error -> { t with preview = Some (Failed { path; error }) }, []
  | Upload_link { file; overwrite }, result ->
    upload_link_reply t ~file ~overwrite result
  | Download_link, Ok json ->
    let t = { t with busy = None } in
    (match Json.string_field json "url", Json.string_field json "name" with
     | Ok url, Ok name ->
       t, [ Out.Page (Download { url = link t url; name }); toast (sprintf "Downloading %s" name) ]
     | _ -> t, [ toast ~error:true "The backend sent no download link: try again." ])
  | Download_link, Error e -> { t with busy = None }, [ toast ~error:true e ]
  | Changed { notice; select }, Ok _ ->
    let t, cmds = refresh { t with ask = None; preview = None; selected = select } in
    t, (toast notice :: cmds) @ [ Out.Page (Focus list_id) ]
  | Changed _, Error e ->
    (match t.ask with
     | Some (New_folder a) ->
       { t with ask = Some (New_folder { a with busy = false; error = Some e }) }, []
     | Some (Rename a) ->
       { t with ask = Some (Rename { a with busy = false; error = Some e }) }, []
     | Some (Delete a) ->
       { t with ask = Some (Delete { a with busy = false; error = Some e }) }, []
     | Some (Overwrite _) | None -> t, [ toast ~error:true e ])
;;

let set_where t where =
  if [%equal: Where.t option] t.where where
  then t, []
  else (
    let moved_away =
      match t.where, where with
      | Some old, Some w ->
        not (String.equal old.session w.session && String.equal old.host w.host)
      | None, _ -> false
      | Some _, None -> true
    in
    let aborts =
      if moved_away
      then
        List.filter_map t.attachments ~f:(fun a ->
          match a.file, a.status with
          | Some file, Uploading _ -> Some (Out.Page (Abort_upload file))
          | _ -> None)
      else []
    in
    let t =
      { t with
        where
      ; listing = None
      ; loading = None
      ; error = None
      ; selected = None
      ; preview = None
      ; editing_path = None
      ; ask = None
      ; conflicts = []
      ; attachments = (if moved_away then [] else t.attachments)
      }
    in
    if t.open_ && Option.is_some where
    then (
      let t, cmds = list t None in
      t, aborts @ cmds)
    else t, aborts)
;;
