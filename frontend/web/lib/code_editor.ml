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
    ; added : string list
    }
  [@@deriving sexp_of, equal]

  let lines text =
    if String.is_empty text then [||] else Array.of_list (String.split_lines text)
  ;;

  (* The lines both texts start and end with are not diffed: an agent's
     edit of a long file is a short diff in its middle. *)
  let of_texts ~before ~after =
    let a = lines before in
    let b = lines after in
    let n = Array.length a in
    let m = Array.length b in
    let prefix = ref 0 in
    while !prefix < n && !prefix < m && String.equal a.(!prefix) b.(!prefix) do
      incr prefix
    done;
    let suffix = ref 0 in
    while
      !suffix < n - !prefix
      && !suffix < m - !prefix
      && String.equal a.(n - 1 - !suffix) b.(m - 1 - !suffix)
    do
      incr suffix
    done;
    let middle x len =
      String.concat ~sep:"\n" (Array.to_list (Array.sub x ~pos:!prefix ~len))
    in
    let lines =
      Line_diff.diff
        ~old:(middle a (n - !prefix - !suffix))
        ~new_:(middle b (m - !prefix - !suffix))
    in
    let close open_ acc =
      match open_ with
      | Some h ->
        { h with removed = List.rev h.removed; added = List.rev h.added } :: acc
      | None -> acc
    in
    let _, open_, acc =
      List.fold
        lines
        ~init:(!prefix + 1, None, [])
        ~f:(fun (line, open_, acc) (l : Line_diff.Line.t) ->
          let start () =
            Option.value open_ ~default:{ line; removed = []; added = [] }
          in
          match l with
          | Same _ -> line + 1, None, close open_ acc
          | Removed s ->
            let h = start () in
            line, Some { h with removed = s :: h.removed }, acc
          | Added s ->
            let h = start () in
            line + 1, Some { h with added = s :: h.added }, acc)
    in
    List.rev (close open_ acc)
  ;;

  (* Where the hunk's added lines are in [text]: at [line] if they are still
     there, else the nearest place they are. *)
  let locate t (text : string array) =
    let len = List.length t.added in
    let added = Array.of_list t.added in
    let at i =
      i >= 0
      && i + len <= Array.length text
      && Array.for_alli added ~f:(fun k l -> String.equal text.(i + k) l)
    in
    let start = t.line - 1 in
    if at start
    then Some start
    else if len = 0
    then None
    else
      List.find
        (List.concat_map (List.range 1 (Array.length text + 1)) ~f:(fun d ->
           [ start - d; start + d ]))
        ~f:at
  ;;

  (* [text] with the hunk taken back: its added lines replaced by the
     removed ones, and how many lines that adds. *)
  let revert t text =
    let a = lines text in
    match locate t a with
    | None -> None
    | Some i ->
      let len = List.length t.added in
      let out =
        Array.to_list (Array.sub a ~pos:0 ~len:i)
        @ t.removed
        @ Array.to_list (Array.sub a ~pos:(i + len) ~len:(Array.length a - i - len))
      in
      let joined = String.concat ~sep:"\n" out in
      let joined =
        if String.is_suffix text ~suffix:"\n" && not (List.is_empty out)
        then joined ^ "\n"
        else joined
      in
      Some (joined, i + 1, List.length t.removed - len)
  ;;

  (* A line number after an edit that kept [prefix] lines at the start and
     [suffix] at the end of [old_len] lines, now [new_len]. *)
  let shift_line line ~prefix ~suffix ~old_len ~new_len =
    if line <= prefix
    then line
    else if line > old_len - suffix
    then line + new_len - old_len
    else prefix + 1
  ;;

  let remap hunks ~before ~after =
    if List.is_empty hunks
    then hunks
    else (
      let a = lines before in
      let b = lines after in
      let n = Array.length a in
      let m = Array.length b in
      let prefix = ref 0 in
      while !prefix < n && !prefix < m && String.equal a.(!prefix) b.(!prefix) do
        incr prefix
      done;
      let suffix = ref 0 in
      while
        !suffix < n - !prefix
        && !suffix < m - !prefix
        && String.equal a.(n - 1 - !suffix) b.(m - 1 - !suffix)
      do
        incr suffix
      done;
      List.map hunks ~f:(fun h ->
        { h with
          line =
            shift_line h.line ~prefix:!prefix ~suffix:!suffix ~old_len:n ~new_len:m
        }))
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
    ; hunks : Hunk.t list
    ; kept : Hunk.t list
    }
  [@@deriving sexp_of, equal]

  let dirty t = not (String.equal t.text t.disk)

  let marks t =
    List.filter_map (t.hunks @ t.kept) ~f:(fun (h : Hunk.t) ->
      match h.added with
      | [] -> None
      | added -> Some (h.line, h.line + List.length added - 1))
    |> List.sort ~compare:[%compare: int * int]
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
    | Files of string (** the cwd it was asked for *)
  [@@deriving sexp_of, equal]
end

module Index = struct
  type t =
    { files : string array
    ; truncated : bool
    ; loading : bool
    ; loaded : bool
    }
  [@@deriving sexp_of]

  let empty = { files = [||]; truncated = false; loading = false; loaded = false }

  let of_json json =
    match
      Json.string_field json "files", Json.bool_field json "truncated"
    with
    | Ok files, truncated ->
      Ok
        { files =
            Array.of_list
              (List.filter (String.split files ~on:'\n') ~f:(fun f ->
                 not (String.is_empty f)))
        ; truncated = Result.ok truncated |> Option.value ~default:false
        ; loading = false
        ; loaded = true
        }
    | Error e, _ -> Error (Error.to_string_hum e)
  ;;
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
    | Send of
        { chip : Chip.t
        ; text : string
        }
  [@@deriving sexp_of]
end

type t =
  { open_ : bool
  ; cwd : string
  ; buffers : Buffer.t list
  ; current : string option
  ; selection : Selection.t option
  ; chip : Chip.t option
  ; asking : Chip.t option
  ; tree : bool
  ; drawer : bool
  ; dir : File_listing.t option
  ; filter : string
  ; index : Index.t
  ; matches : Prigh_ui.Fuzzy_path.Match.t array
  ; matched : string
  ; list_top : int
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
  ; asking = None
  ; tree = true
  ; drawer = false
  ; dir = None
  ; filter = ""
  ; index = Index.empty
  ; matches = [||]
  ; matched = ""
  ; list_top = 0
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

(* The matches for [t.filter], from the last ones when the query only grew. *)
let rematch t =
  let query = String.strip t.filter in
  if String.is_empty query || not t.index.loaded
  then { t with matches = [||]; matched = query; list_top = 0 }
  else (
    let from =
      if
        (not (String.is_empty t.matched))
        && Prigh_ui.Fuzzy_path.narrows ~query:t.matched ~query':query
      then Array.map t.matches ~f:(fun (m : Prigh_ui.Fuzzy_path.Match.t) -> m.path)
      else t.index.files
    in
    { t with
      matches = Prigh_ui.Fuzzy_path.rank ~query from
    ; matched = query
    ; list_top = 0
    })
;;

let index_files t =
  if t.index.loading
  then t, []
  else
    ( { t with index = { t.index with loading = true } }
    , [ rpc "list_files" [] (Files t.cwd) ] )
;;

let load_index t = if t.index.loaded then t, [] else index_files t

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
  | Some _ -> { t with current = Some path; selection = None; asking = None }, []
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

let keep t ~f =
  match current t with
  | None -> t
  | Some b ->
    map_buffer t b.path ~f:(fun b ->
      let kept, hunks = List.partition_tf (List.mapi b.hunks ~f:(fun i h -> i, h)) ~f:(fun (i, h) -> f i h) in
      { b with
        hunks = List.map hunks ~f:snd
      ; kept =
          List.sort (b.kept @ List.map kept ~f:snd) ~compare:(fun (a : Hunk.t) b ->
            Int.compare a.line b.line)
      })
;;

(* Takes back the changes [f] picks, last first so that the earlier ones'
   lines stay put, and saves the file. *)
let undo t ~f =
  match current t with
  | None -> t, []
  | Some b ->
    let picked = List.filteri b.hunks ~f:(fun i h -> f i h) in
    if List.is_empty picked
    then t, []
    else (
      let rest = List.filteri b.hunks ~f:(fun i h -> not (f i h)) in
      let text, rest, kept, failed =
        List.fold
          (List.rev picked)
          ~init:(b.text, rest, b.kept, 0)
          ~f:(fun (text, rest, kept, failed) h ->
            match Hunk.revert h text with
            | Some (text', _, _) ->
              let shift = Hunk.remap ~before:text ~after:text' in
              text', shift rest, shift kept, failed
            | None -> text, rest, kept, failed + 1)
      in
      let b =
        { b with
          text
        ; hunks = rest
        ; kept
        ; revision = b.revision + 1
        ; saving = Option.is_some b.md5
        }
      in
      ( map_buffer t b.path ~f:(Fn.const b)
      , (if Option.is_some b.md5 then [ write b ~force:false ] else [])
        @
        if failed > 0
        then
          [ toast
              ~error:true
              "Some of the change was edited since: undo those lines by hand"
          ]
        else [] ))
;;

let selected_chip t =
  Option.map (current t) ~f:(fun b ->
    let selection =
      Option.value t.selection ~default:{ Selection.first = 1; last = 1; text = "" }
    in
    { Chip.path = relative t b.path
    ; first = selection.first
    ; last = selection.last
    ; text = selection.text
    })
;;

let ask t =
  match current t with
  | None -> t, []
  | Some b ->
    let selection =
      Option.value
        t.selection
        ~default:{ Selection.first = 1; last = 1; text = "" }
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
  ; asking = None
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
    let b = List.nth_exn buffers ((((i + n) % len) + len) % len) in
    { t with current = Some b.path; selection = None; asking = None }
;;

module Action = struct
  type t =
    | Enter of string option
    | Leave
    | Open of string
    | Switch of string
    | Step of int
    | Swiped of int
    | Close of string
    | Edited of
        { path : string
        ; text : string
        }
    | Selected of Selection.t option
    | Save
    | Overwrite
    | Reload
    | Keep of int (** the agent's change at that index *)
    | Undo of int
    | Keep_all
    | Undo_all
    | Reopen of int (** a kept change, by its index *)
    | Ask
    | Ask_inline
    | Ask_send of string
    | Ask_cancel
    | Drop_chip
    | Toggle_tree
    | Toggle_drawer
    | List of string option
    | Filter of string
    | List_scrolled of int (** the first row of the matches on screen *)
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
    let t, listing = if Option.is_none t.dir then list t None else t, [] in
    let t, index = if t.index.loaded then t, [] else index_files t in
    let listing = listing @ index in
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
    ( { t with
        open_ = false
      ; menu = false
      ; goto = None
      ; drawer = false
      ; sheet = false
      }
    , [] )
  | Open path ->
    (* On a phone the drawer gives way to the file. *)
    open_ { t with drawer = false; filter = "" } path
  | Switch path ->
    if Option.is_some (find t path)
    then { t with current = Some path; selection = None; asking = None; menu = false }, []
    else t, []
  | Step n -> step t n, []
  | Swiped n ->
    (match t.buffers with
     | [] | [ _ ] -> t, []
     | _ :: _ :: _ -> { (step t n) with asking = None }, [])
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
        if String.equal b.text text
        then b
        else
          { b with
            text
          ; hunks = Hunk.remap b.hunks ~before:b.text ~after:text
          ; kept = Hunk.remap b.kept ~before:b.text ~after:text
          })
    , [] )
  | Selected selection -> { t with selection }, []
  | Save -> save { t with menu = false } ~force:false
  | Overwrite -> save { t with menu = false } ~force:true
  | Reload ->
    (match current t with
     | None -> t, []
     | Some b -> { t with menu = false }, [ preview b.path (Opened b.path) ])
  | Keep i -> keep t ~f:(fun k _ -> k = i), []
  | Keep_all -> keep t ~f:(fun _ _ -> true), []
  | Undo i -> undo t ~f:(fun k _ -> k = i)
  | Undo_all -> undo t ~f:(fun _ _ -> true)
  | Reopen i ->
    (match current t with
     | None -> t, []
     | Some b ->
       ( map_buffer t b.path ~f:(fun b ->
           let reopened = List.filteri b.kept ~f:(fun k _ -> k = i) in
           let kept = List.filteri b.kept ~f:(fun k _ -> k <> i) in
           { b with
             hunks =
               List.sort (b.hunks @ reopened) ~compare:(fun (a : Hunk.t) b ->
                 Int.compare a.line b.line)
           ; kept
           })
       , [] ))
  | Ask -> ask { t with menu = false; asking = None }
  | Ask_inline ->
    (match selected_chip t with
     | None -> t, []
     | Some chip -> { t with asking = Some chip; menu = false; chip = None }, [])
  | Ask_send text ->
    (match t.asking with
     | Some chip when not (String.is_empty (String.strip text)) ->
       { t with asking = None }, [ Send { chip; text = String.strip text } ]
     | Some _ -> t, [ toast ~error:true "Type a question or an instruction first" ]
     | None -> t, [])
  | Ask_cancel -> { t with asking = None }, [ Command Focus ]
  | Drop_chip -> { t with chip = None }, []
  | Toggle_tree -> { t with tree = not t.tree; menu = false }, []
  | List path -> list t path
  | Filter filter ->
    let t = rematch { t with filter } in
    if t.index.loaded then t, [] else index_files t
  | List_scrolled top -> { t with list_top = Int.max 0 top }, []
  | Find_file ->
    let t, index = index_files t in
    ( { t with tree = true; drawer = true; menu = false }
    , index @ [ Focus "code-filter" ] )
  | Toggle_chat -> { t with chat = not t.chat; menu = false }, []
  | Toggle_sheet -> { t with sheet = not t.sheet; menu = false }, []
  | Toggle_drawer -> { t with drawer = not t.drawer; menu = false }, []
  | Toggle_menu -> { t with menu = not t.menu }, []
  | Goto_open -> { t with goto = Some ""; menu = false }, [ Focus "code-goto" ]
  | Goto_input s -> { t with goto = Some s }, []
  | Goto_line ->
    (match
       Option.bind t.goto ~f:(fun s -> Int.of_string_opt (String.strip s))
     with
     | Some n when n > 0 -> { t with goto = None }, [ Command (Goto_line n) ]
     | Some _ | None ->
       t, [ toast ~error:true "Go to line: type a line number (1 or more)" ])
  | Goto_cancel -> { t with goto = None }, [ Command Focus ]
  | Scrolled dir ->
    ( { t with
        strip_hidden =
          (match dir with
           | `Down -> true
           | `Up -> false)
      }
    , [] )
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
             ; hunks = []
             ; kept = []
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
                   ; hunks = []
                   ; kept = []
                   }
                 ]
           }
       in
       { t with current = Some path; selection = None; asking = None }, [])
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
                  "%s changed on the host while you were editing it: reload it \
                   or overwrite it (⋯)"
                  (relative t path))
           ] )
       else (
         let shift = Hunk.remap ~before:b.text ~after:text in
         let hunks =
           List.sort
             (shift b.hunks @ Hunk.of_texts ~before:b.text ~after:text)
             ~compare:(fun (a : Hunk.t) b -> Int.compare a.line b.line)
         in
         ( map_buffer t path ~f:(fun b ->
             { b with
               text
             ; disk = text
             ; md5
             ; revision = b.revision + 1
             ; hunks
             ; kept = shift b.kept
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
  | Files cwd, result when String.equal cwd t.cwd ->
    (match Result.bind result ~f:Index.of_json with
     | Ok index ->
       (* A refresh starts afresh: the paths may have gone. *)
       rematch { t with index; matched = "" }, []
     | Error e ->
       ( { t with index = { t.index with loading = false } }
       , [ toast ~error:true (sprintf "Go to file: could not list the files: %s" e) ] ))
  | Files _, _ -> t, []
;;

let set_where t ~session_changed ~cwd =
  if session_changed || not (String.equal cwd t.cwd)
  then { empty with cwd; chat = t.chat; tree = t.tree }
  else t
;;

let changed t =
  let t, index = if t.index.loaded then index_files t else t, [] in
  ( t
  , index
    @ List.map t.buffers ~f:(fun (b : Buffer.t) ->
      preview b.path (Checked b.path)) )
;;

let sent t = { t with chip = None }
