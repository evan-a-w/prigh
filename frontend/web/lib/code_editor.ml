open! Core
open! Import

module Lang = struct
  type t =
    | Ocaml
    | Markdown
    | Javascript
    | Typescript
    | Rust
    | Python
    | Nix
    | Shell
    | Json
    | Css
    | Html
    | Dune
    | Plain
  [@@deriving sexp_of, equal]

  let of_path path =
    let base = Filename.basename path in
    match String.lowercase base with
    | "dune" | "dune-project" | "dune-workspace" | "opam" -> Dune
    | "makefile" | "dockerfile" | ".bashrc" | ".profile" | ".envrc" -> Shell
    | _ ->
      (match
         Option.map (snd (Filename.split_extension base)) ~f:String.lowercase
       with
       | Some ("ml" | "mli" | "mll" | "mly" | "re") -> Ocaml
       | Some ("md" | "markdown" | "mdx") -> Markdown
       | Some ("js" | "mjs" | "cjs" | "jsx") -> Javascript
       | Some ("ts" | "tsx" | "mts" | "cts") -> Typescript
       | Some "rs" -> Rust
       | Some ("py" | "pyi") -> Python
       | Some "nix" -> Nix
       | Some ("sh" | "bash" | "zsh" | "t") -> Shell
       | Some ("json" | "jsonc" | "webmanifest") -> Json
       | Some "css" -> Css
       | Some ("html" | "htm" | "svg" | "xml") -> Html
       | Some ("sexp" | "el" | "scm" | "lisp" | "clj") -> Dune
       | _ -> Plain)
  ;;

  let name = function
    | Ocaml -> "ocaml"
    | Markdown -> "markdown"
    | Javascript -> "javascript"
    | Typescript -> "typescript"
    | Rust -> "rust"
    | Python -> "python"
    | Nix -> "nix"
    | Shell -> "shell"
    | Json -> "json"
    | Css -> "css"
    | Html -> "html"
    | Dune -> "scheme"
    | Plain -> "plain"
  ;;
end

module Hunk = struct
  type t =
    { line : int
    ; removed : string list
    ; added : int
    }
  [@@deriving sexp_of, equal]

  let of_texts ~before ~after =
    let lines = Line_diff.diff ~old:before ~new_:after in
    (* [line]: the next line of [after]; [open_]: the hunk being built. *)
    let close open_ acc =
      match open_ with
      | Some h -> { h with removed = List.rev h.removed } :: acc
      | None -> acc
    in
    let _, open_, acc =
      List.fold
        lines
        ~init:(1, None, [])
        ~f:(fun (line, open_, acc) (l : Line_diff.Line.t) ->
          let start () =
            Option.value open_ ~default:{ line; removed = []; added = 0 }
          in
          match l with
          | Same _ -> line + 1, None, close open_ acc
          | Removed s ->
            let h = start () in
            line, Some { h with removed = s :: h.removed }, acc
          | Added _ ->
            let h = start () in
            line + 1, Some { h with added = h.added + 1 }, acc)
    in
    List.rev (close open_ acc)
  ;;
end

module Conflict = struct
  type t =
    | Changed_on_disk
    | Save_refused of string
  [@@deriving sexp_of, equal]
end

module Buffer = struct
  type t =
    { path : string
    ; text : string
    ; disk : string
    ; md5 : string option
    ; revision : int
    ; saving : bool
    ; conflict : Conflict.t option
    ; agent : string option
    ; hunks : Hunk.t list
    ; marks : (int * int) list
    }
  [@@deriving sexp_of, equal]

  let dirty t = not (String.equal t.text t.disk)

  let marks_of hunks =
    List.filter_map hunks ~f:(fun (h : Hunk.t) ->
      if h.added = 0 then None else Some (h.line, h.line + h.added - 1))
  ;;
end

module Selection = struct
  type t =
    { first : int
    ; last : int
    ; text : string
    }
  [@@deriving sexp_of, equal]
end

module Chip = struct
  type t =
    { path : string
    ; first : int
    ; last : int
    ; text : string
    }
  [@@deriving sexp_of, equal]

  let lines t =
    if t.first = t.last
    then Int.to_string t.first
    else sprintf "%d–%d" t.first t.last
  ;;

  let label t = sprintf "%s:%s" t.path (lines t)

  let prompt t text =
    if String.is_empty t.text
    then sprintf "[%s] %s" (label t) text, []
    else
      ( sprintf "[%s] %s" (label t) text
      , [ sprintf
            "%s, line%s %s:\n%s"
            t.path
            (if t.first = t.last then "" else "s")
            (lines t)
            t.text
        ] )
  ;;
end

module Command = struct
  type t =
    | Insert of string
    | Indent
    | Undo
    | Redo
    | Left
    | Right
    | Goto_line of int
    | Find
    | Focus
  [@@deriving sexp_of, equal]
end

module Reply = struct
  type t =
    | Opened of string
    | Checked of string
    | Saved of string
    | Listed
    | Paths of string
  [@@deriving sexp_of, equal]
end

module Out = struct
  type t =
    | Rpc of
        { method_ : string
        ; params : (string * Json.t) list
        ; reply : Reply.t
        }
    | Command of Command.t
    | Toast of
        { text : string
        ; error : bool
        }
    | Focus of string
    | Focus_composer
    | Refresh_changes
  [@@deriving sexp_of]
end

type t =
  { open_ : bool
  ; cwd : string
  ; buffers : Buffer.t list
  ; current : string option
  ; selection : Selection.t option
  ; chip : Chip.t option
  ; tree : bool
  ; drawer : bool
  ; dir : File_listing.t option
  ; filter : string
  ; paths : string list
  ; chat : bool
  ; sheet : bool
  ; menu : bool
  ; goto : string option
  ; strip_hidden : bool
  ; loading : string option
  }
[@@deriving sexp_of]

let empty =
  { open_ = false
  ; cwd = ""
  ; buffers = []
  ; current = None
  ; selection = None
  ; chip = None
  ; tree = true
  ; drawer = false
  ; dir = None
  ; filter = ""
  ; paths = []
  ; chat = true
  ; sheet = false
  ; menu = false
  ; goto = None
  ; strip_hidden = false
  ; loading = None
  }
;;

let find t path =
  List.find t.buffers ~f:(fun b -> String.equal b.Buffer.path path)
;;

let current t = Option.bind t.current ~f:(find t)

let map_buffer t path ~f =
  { t with
    buffers =
      List.map t.buffers ~f:(fun b ->
        if String.equal b.Buffer.path path then f b else b)
  }
;;

let relative t path =
  match String.chop_prefix path ~prefix:(t.cwd ^ "/") with
  | Some rel when not (String.is_empty t.cwd) -> rel
  | _ -> path
;;

let absolute t path =
  if String.is_prefix path ~prefix:"/" || String.is_prefix path ~prefix:"~"
  then path
  else Filename.concat t.cwd path
;;

let toast ?(error = false) text = Out.Toast { text; error }
let rpc method_ params reply = Out.Rpc { method_; params; reply }
let preview path reply = rpc "fs_preview" [ "path", `String path ] reply

let list t path =
  ( { t with filter = "" }
  , [ rpc
        "fs_list"
        (Option.value_map path ~default:[] ~f:(fun p -> [ "path", `String p ])
         @ [ "hidden", Json.bool false ])
        Listed
    ] )
;;

let open_ t path =
  match find t path with
  | Some _ -> { t with current = Some path; selection = None }, []
  | None -> { t with loading = Some path }, [ preview path (Opened path) ]
;;

let write (b : Buffer.t) ~force =
  rpc
    "fs_write"
    ([ "path", `String b.path; "text", `String b.text ]
     @
     match b.md5 with
     | Some md5 when not force -> [ "expected", `String md5 ]
     | Some _ | None -> [])
    (Saved b.path)
;;

let save t ~force =
  match current t with
  | None -> t, []
  | Some b when Option.is_none b.md5 ->
    t, [ toast ~error:true (sprintf "%s is too large to edit here" b.path) ]
  | Some b when b.saving -> t, []
  | Some b ->
    ( map_buffer t b.path ~f:(fun b ->
        { b with
          saving = true
        ; conflict =
            (match b.conflict with
             | Some (Save_refused _) -> None
             | c -> c)
        })
    , [ write b ~force ] )
;;

let ask t =
  match current t with
  | None -> t, []
  | Some b ->
    let selection =
      Option.value t.selection ~default:{ Selection.first = 1; last = 1; text = "" }
    in
    ( { t with
        chip =
          Some
            { path = relative t b.path
            ; first = selection.first
            ; last = selection.last
            ; text = selection.text
            }
      }
    , [ Out.Focus_composer ] )
;;

let close t path =
  let rec after = function
    | (b : Buffer.t) :: next :: _ when String.equal b.path path -> Some next
    | [ (b : Buffer.t) ] when String.equal b.path path -> None
    | _ :: rest -> after rest
    | [] -> None
  in
  let rec before prev = function
    | (b : Buffer.t) :: _ when String.equal b.path path -> prev
    | b :: rest -> before (Some b) rest
    | [] -> None
  in
  let current =
    match t.current with
    | Some c when String.equal c path ->
      Option.first_some (after t.buffers) (before None t.buffers)
      |> Option.map ~f:(fun (b : Buffer.t) -> b.path)
    | c -> c
  in
  { t with
    buffers = List.filter t.buffers ~f:(fun b -> not (String.equal b.path path))
  ; current
  ; selection = None
  }
;;

let step t n =
  match t.buffers, t.current with
  | [], _ | _, None -> t
  | buffers, Some c ->
    let len = List.length buffers in
    let i =
      Option.value_map
        (List.findi buffers ~f:(fun _ b -> String.equal b.path c))
        ~default:0
        ~f:fst
    in
    let b = List.nth_exn buffers (((i + n) % len + len) % len) in
    { t with current = Some b.path; selection = None }
;;

module Action = struct
  type t =
    | Enter of string option 
    | Leave
    | Open of string 
    | Switch of string
    | Step of int 
    | Close of string
    | Edited of
        { path : string
        ; text : string
        }
    | Selected of Selection.t option
    | Save
    | Overwrite 
    | Reload 
    | Keep 
    | Undo_agent 
    | Ask 
    | Drop_chip
    | Toggle_tree
    | Toggle_drawer
    | List of string option 
    | Filter of string
    | Find_file (** the Go to file field *)
    | Toggle_chat
    | Toggle_sheet
    | Toggle_menu
    | Goto_open
    | Goto_input of string
    | Goto_line
    | Goto_cancel
    | Scrolled of [ `Down | `Up ]
    | Command of Command.t
  [@@deriving sexp_of]
end

let update t (action : Action.t) =
  match action with
  | Enter path ->
    let t = { t with open_ = true; menu = false } in
    let t, listing =
      if Option.is_none t.dir then list t None else t, []
    in
    (match path with
     | Some p when not (String.is_empty (String.strip p)) ->
       let t, outs = open_ t (absolute t (String.strip p)) in
       t, listing @ outs @ [ Out.Refresh_changes ]
     | Some _ | None ->
       let t =
         if Option.is_none t.current && List.is_empty t.buffers
         then { t with drawer = true }
         else t
       in
       t, listing @ [ Out.Refresh_changes ])
  | Leave ->
    ( { t with open_ = false; menu = false; goto = None; drawer = false; sheet = false }
    , [] )
  | Open path ->
    (* On a phone the drawer gives way to the file. *)
    open_ { t with drawer = false; filter = "" } path
  | Switch path ->
    if Option.is_some (find t path)
    then { t with current = Some path; selection = None; menu = false }, []
    else t, []
  | Step n -> step t n, []
  | Close path ->
    (match find t path with
     | Some b when Buffer.dirty b ->
       ( t
       , [ toast
             ~error:true
             (sprintf
                "%s has unsaved changes: save it (Ctrl+S) or reload it (⋯ → \
                 Reload) first"
                (relative t path))
         ] )
     | Some _ -> { (close t path) with menu = false }, []
     | None -> t, [])
  | Edited { path; text } ->
    ( map_buffer t path ~f:(fun b ->
        if String.equal b.text text then b else { b with text })
    , [] )
  | Selected selection -> { t with selection }, []
  | Save -> save { t with menu = false } ~force:false
  | Overwrite -> save { t with menu = false } ~force:true
  | Reload ->
    (match current t with
     | None -> t, []
     | Some b -> { t with menu = false }, [ preview b.path (Opened b.path) ])
  | Keep ->
    (match current t with
     | None -> t, []
     | Some b ->
       map_buffer t b.path ~f:(fun b -> { b with agent = None; hunks = [] }), [])
  | Undo_agent ->
    (match current t with
     | Some ({ agent = Some before; _ } as b) ->
       let b =
         { b with
           text = before
         ; agent = None
         ; hunks = []
         ; marks = []
         ; revision = b.revision + 1
         ; saving = true
         }
       in
       map_buffer t b.path ~f:(Fn.const b), [ write b ~force:false ]
     | Some _ | None -> t, [])
  | Ask -> ask { t with menu = false }
  | Drop_chip -> { t with chip = None }, []
  | Toggle_tree -> { t with tree = not t.tree; menu = false }, []
  | List path -> list t path
  | Filter filter ->
    (* The host matches a substring: the paths with the query's first
       letter are the fuzzy matches' superset (up to the host's limit), the
       query itself its closest. *)
    let filter_q = String.strip filter in
    let first = String.prefix filter_q 1 in
    let same_first = String.equal first (String.prefix (String.strip t.filter) 1) in
    let paths q = rpc "list_paths" [ "prefix", `String q ] (Paths q) in
    ( { t with filter; paths = (if same_first then t.paths else []) }
    , if String.is_empty filter_q
      then []
      else if same_first
      then [ paths filter_q ]
      else List.dedup_and_sort ~compare:String.compare [ first; filter_q ] |> List.map ~f:paths
    )
  | Find_file ->
    { t with tree = true; drawer = true; menu = false }, [ Focus "code-filter" ]
  | Toggle_chat -> { t with chat = not t.chat; menu = false }, []
  | Toggle_sheet -> { t with sheet = not t.sheet; menu = false }, []
  | Toggle_drawer -> { t with drawer = not t.drawer; menu = false }, []
  | Toggle_menu -> { t with menu = not t.menu }, []
  | Goto_open -> { t with goto = Some ""; menu = false }, [ Focus "code-goto" ]
  | Goto_input s -> { t with goto = Some s }, []
  | Goto_line ->
    (match Option.bind t.goto ~f:(fun s -> Int.of_string_opt (String.strip s)) with
     | Some n when n > 0 -> { t with goto = None }, [ Command (Goto_line n) ]
     | Some _ | None ->
       ( t
       , [ toast ~error:true "Go to line: type a line number (1 or more)" ] ))
  | Goto_cancel -> { t with goto = None }, [ Command Focus ]
  | Scrolled dir ->
    { t with strip_hidden = (match dir with `Down -> true | `Up -> false) }, []
  | Command c -> t, [ Command c ]
;;

let of_preview json =
  match File_preview.of_json json with
  | Error e -> Error (Error.to_string_hum e)
  | Ok { content = Text { text; truncated = false; md5 }; path; _ } ->
    Ok (path, text, md5)
  | Ok { content = Text { truncated = true; _ }; path; _ } ->
    Error (sprintf "%s is too large to edit here (over 256 KiB)" path)
  | Ok { content = Image _ | Binary; path; _ } ->
    Error (sprintf "%s is not a text file" path)
;;

let reply t (reply : Reply.t) (result : (Json.t, string) Result.t) =
  match reply, result with
  | Opened requested, result ->
    let t =
      { t with
        loading =
          (if Option.equal String.equal t.loading (Some requested)
           then None
           else t.loading)
      }
    in
    (match Result.bind result ~f:of_preview with
     | Error e -> t, [ toast ~error:true e ]
     | Ok (path, text, md5) ->
       let t =
         match find t path with
         | Some _ ->
           map_buffer t path ~f:(fun b ->
             { b with
               text
             ; disk = text
             ; md5
             ; revision = b.revision + 1
             ; conflict = None
             ; agent = None
             ; hunks = []
             ; marks = []
             })
         | None ->
           { t with
             buffers =
               t.buffers
               @ [ { Buffer.path
                   ; text
                   ; disk = text
                   ; md5
                   ; revision = 0
                   ; saving = false
                   ; conflict = None
                   ; agent = None
                   ; hunks = []
                   ; marks = []
                   }
                 ]
           }
       in
       { t with current = Some path; selection = None }, [])
  | Checked path, Ok json ->
    (match find t path, of_preview json with
     | None, _ | _, Error _ -> t, []
     | Some b, Ok (_, text, md5) ->
       if Option.equal String.equal md5 b.md5 || String.equal text b.disk
       then t, []
       else if Buffer.dirty b
       then
         ( map_buffer t path ~f:(fun b ->
             { b with conflict = Some Changed_on_disk })
         , [ toast
               ~error:true
               (sprintf
                  "%s changed on the host while you were editing it: reload \
                   it or overwrite it (⋯)"
                  (relative t path))
           ] )
       else (
         let before = Option.value b.agent ~default:b.disk in
         let hunks = Hunk.of_texts ~before ~after:text in
         ( map_buffer t path ~f:(fun b ->
             { b with
               text
             ; disk = text
             ; md5
             ; revision = b.revision + 1
             ; agent = Some before
             ; hunks
             ; marks = Buffer.marks_of hunks
             ; conflict = None
             })
         , [] )))
  | Checked _, Error _ -> t, []
  | Saved path, Error e ->
    ( map_buffer t path ~f:(fun b ->
        { b with
          saving = false
        ; conflict =
            Some
              (if String.is_substring e ~substring:"changed since you opened it"
               then Changed_on_disk
               else Save_refused e)
        })
    , [ toast ~error:true e ] )
  | Saved path, Ok json ->
    let md5 = Result.ok (Json.string_field json "md5") in
    (match find t path with
     | None -> t, []
     | Some b ->
       ( map_buffer t path ~f:(fun b ->
           { b with
             saving = false
           ; disk = b.text
           ; md5 = Option.first_some md5 b.md5
           ; conflict = None
           })
       , [ toast (sprintf "Saved %s" (relative t b.path)) ] ))
  | Listed, Ok json ->
    (match File_listing.of_json json with
     | Ok listing -> { t with dir = Some listing }, []
     | Error e -> t, [ toast ~error:true (Error.to_string_hum e) ])
  | Listed, Error e -> t, [ toast ~error:true e ]
  | Paths query, Ok (`Array items) ->
    if String.is_prefix (String.strip t.filter) ~prefix:query
    then
      ( { t with
          paths =
            List.dedup_and_sort
              ~compare:String.compare
              (t.paths
               @ List.filter_map items ~f:(function
                 | `String p when not (String.is_suffix p ~suffix:"/") -> Some p
                 | _ -> None))
        }
      , [] )
    else t, []
  | Paths _, (Ok _ | Error _) -> t, []
;;

let set_where t ~session_changed ~cwd =
  if session_changed || not (String.equal cwd t.cwd)
  then { empty with cwd; chat = t.chat; tree = t.tree }
  else t
;;

let changed t =
  ( t
  , List.map t.buffers ~f:(fun (b : Buffer.t) ->
      preview b.path (Checked b.path)) )
;;

let sent t = { t with chip = None }
