open! Core
open! Import

type t =
  { tool : string
  ; pattern : string option
  }
[@@deriving sexp_of, equal, compare]

let valid_tool_char c = Char.is_alphanum c || String.mem "_-.*" c

let of_string s =
  let s = String.strip s in
  let tool, pattern =
    match String.lsplit2 s ~on:'(' with
    | None -> s, Ok None
    | Some (tool, rest) ->
      ( tool
      , (match String.chop_suffix rest ~suffix:")" with
         | None -> Or_error.errorf "rule %S: the pattern must end with \")\"" s
         | Some pattern when String.is_empty (String.strip pattern) ->
           Or_error.errorf
             "rule %S: empty pattern; write %S to match every call"
             s
             (String.strip tool)
         | Some pattern -> Ok (Some pattern)) )
  in
  let tool = String.lowercase (String.strip tool) in
  if String.is_empty tool
  then Or_error.errorf "rule %S: missing the tool name, e.g. bash(ls:*)" s
  else if not (String.for_all tool ~f:valid_tool_char)
  then
    Or_error.errorf
      "rule %S: %S is not a tool name (letters, digits, _, -, . and *)"
      s
      tool
  else Or_error.map pattern ~f:(fun pattern -> { tool; pattern })
;;

let to_string t =
  match t.pattern with
  | None -> t.tool
  | Some p -> sprintf "%s(%s)" t.tool p
;;

(* ---- globs ------------------------------------------------------------ *)

module Token = struct
  type t =
    | Lit of char
    | One (* ? *)
    | Star (* * *)
    | Globstar (* ** *)
    | Globstar_dir (* **/ : nothing, or anything ending in / *)
end

let tokens ~paths pattern =
  let n = String.length pattern in
  let rec go i acc =
    if i >= n
    then List.rev acc
    else (
      match pattern.[i] with
      | '*' when paths && i + 1 < n && Char.equal pattern.[i + 1] '*' ->
        if i + 2 < n && Char.equal pattern.[i + 2] '/'
        then go (i + 3) (Token.Globstar_dir :: acc)
        else go (i + 2) (Globstar :: acc)
      | '*' -> go (i + 1) ((if paths then Token.Star else Globstar) :: acc)
      | '?' when paths -> go (i + 1) (One :: acc)
      | c -> go (i + 1) (Lit c :: acc))
  in
  Array.of_list (go 0 [])
;;

let glob ~paths ~pattern s =
  let toks = tokens ~paths pattern in
  let m = Array.length toks in
  let n = String.length s in
  let memo = Array.make_matrix ~dimx:(m + 1) ~dimy:(n + 1) None in
  let rec matches i j =
    match memo.(i).(j) with
    | Some r -> r
    | None ->
      let r =
        if i = m
        then j = n
        else (
          match toks.(i) with
          | Lit c -> j < n && Char.equal s.[j] c && matches (i + 1) (j + 1)
          | One ->
            j < n && (not (Char.equal s.[j] '/')) && matches (i + 1) (j + 1)
          | Star ->
            matches (i + 1) j
            || (j < n && (not (Char.equal s.[j] '/')) && matches i (j + 1))
          | Globstar -> matches (i + 1) j || (j < n && matches i (j + 1))
          | Globstar_dir ->
            matches (i + 1) j
            || List.exists (List.range j n) ~f:(fun k ->
              Char.equal s.[k] '/' && matches (i + 1) (k + 1)))
      in
      memo.(i).(j) <- Some r;
      r
  in
  matches 0 0
;;

let glob_matches ~pattern s = glob ~paths:true ~pattern s

(* ---- shell commands ------------------------------------------------------ *)

let segments command =
  let n = String.length command in
  let parts = ref [] in
  let buf = Buffer.create n in
  let complex = ref false in
  let flush () =
    let s = String.strip (Buffer.contents buf) in
    if not (String.is_empty s) then parts := s :: !parts;
    Buffer.clear buf
  in
  let add_range i j =
    Buffer.add_string buf (String.sub command ~pos:i ~len:(j - i))
  in
  let next i = if i + 1 < n then Some command.[i + 1] else None in
  let rec skip_blanks i =
    if i < n && (Char.equal command.[i] ' ' || Char.equal command.[i] '\t')
    then skip_blanks (i + 1)
    else i
  in
  let word_end j =
    let rec word j =
      if
        j < n
        && (not (Char.is_whitespace command.[j]))
        && not (String.mem ";&|<>()" command.[j])
      then word (j + 1)
      else j
    in
    word j
  in
  (* [i] is just after [>] or [>>]: [>&N] and [>&-] duplicate or close a
     descriptor and [/dev/null] discards; any other target writes a file. *)
  let redirect start i =
    let dup = i < n && Char.equal command.[i] '&' in
    let i' = skip_blanks (if dup then i + 1 else i) in
    let j = word_end i' in
    let target = String.sub command ~pos:i' ~len:(j - i') in
    let is_fd =
      match String.chop_suffix target ~suffix:"-" with
      | Some digits -> String.for_all digits ~f:Char.is_digit
      | None -> (not (String.is_empty target)) && String.for_all target ~f:Char.is_digit
    in
    if not ((dup && is_fd) || String.equal target "/dev/null")
    then complex := true;
    add_range start j;
    j
  in
  let rec skip_comment i =
    if i < n && not (Char.equal command.[i] '\n') then skip_comment (i + 1) else i
  in
  (* [word_start]: whether an unquoted [#] here starts a comment. *)
  let rec go i quote ~word_start =
    if i < n
    then (
      let c = command.[i] in
      match quote with
      | `Single ->
        Buffer.add_char buf c;
        go (i + 1) (if Char.equal c '\'' then `None else `Single) ~word_start:false
      | `Double ->
        if Char.equal c '\\' && i + 1 < n
        then (
          add_range i (i + 2);
          go (i + 2) `Double ~word_start:false)
        else (
          if
            Char.equal c '`'
            || (Char.equal c '$' && Option.equal Char.equal (next i) (Some '('))
          then complex := true;
          Buffer.add_char buf c;
          go (i + 1) (if Char.equal c '"' then `None else `Double) ~word_start:false)
      | `None ->
        let after = String.mem " \t\n;&|()<>" c in
        (match c, next i with
         | '#', _ when word_start -> go (skip_comment i) `None ~word_start:true
         | '\\', Some _ ->
           add_range i (i + 2);
           go (i + 2) `None ~word_start:false
         | '\'', _ ->
           Buffer.add_char buf c;
           go (i + 1) `Single ~word_start:false
         | '"', _ ->
           Buffer.add_char buf c;
           go (i + 1) `Double ~word_start:false
         | '`', _
         | '$', Some ('(' | '\'')
         | ('<' | '>'), Some '('
         | ('(' | ')'), _ ->
           (* substitutions, ANSI-C quotes ([\'] does not end them),
              subshells and function definitions *)
           complex := true;
           Buffer.add_char buf c;
           go (i + 1) `None ~word_start:after
         | '<', Some '<' ->
           if i + 2 < n && Char.equal command.[i + 2] '<'
           then (
             add_range i (i + 3);
             go (i + 3) `None ~word_start:true)
           else (
             (* a here-document's body is not commands *)
             complex := true;
             add_range i (i + 2);
             go (i + 2) `None ~word_start:true)
         | '>', Some '>' -> go (redirect i (i + 2)) `None ~word_start:true
         | '>', _ -> go (redirect i (i + 1)) `None ~word_start:true
         | '&', Some '>' ->
           Buffer.add_char buf c;
           go (i + 1) `None ~word_start:true
         | (';' | '\n'), _ ->
           flush ();
           go (i + 1) `None ~word_start:true
         | '&', Some '&' | '|', Some ('|' | '&') ->
           flush ();
           go (i + 2) `None ~word_start:true
         | ('&' | '|'), _ ->
           flush ();
           go (i + 1) `None ~word_start:true
         | _ ->
           Buffer.add_char buf c;
           go (i + 1) `None ~word_start:after))
  in
  go 0 `None ~word_start:true;
  flush ();
  List.rev !parts, if !complex then `Complex else `Simple
;;

let squash_blanks s =
  String.split_on_chars s ~on:[ ' '; '\t'; '\n' ]
  |> List.filter ~f:(Fn.non String.is_empty)
  |> String.concat ~sep:" "
;;

let command_matches ~pattern segment =
  let segment = squash_blanks segment in
  match String.chop_suffix pattern ~suffix:":*" with
  | Some prefix ->
    let prefix = squash_blanks prefix in
    String.equal segment prefix
    || String.is_prefix segment ~prefix:(prefix ^ " ")
  | None -> glob ~paths:false ~pattern:(squash_blanks pattern) segment
;;

(* ---- calls ---------------------------------------------------------------- *)

let edit_tools = [ "edit"; "write"; "apply_patch" ]
let read_tools = [ "read"; "ls"; "grep"; "find" ]
let file_tools = edit_tools @ read_tools

(* [..] and [.] resolved lexically, so [src/../../x] is not under [src]. *)
let normalize path =
  let parts =
    List.fold (String.split path ~on:'/') ~init:[] ~f:(fun acc part ->
      match part with
      | "" | "." -> acc
      | ".." -> List.tl acc |> Option.value ~default:[]
      | p -> p :: acc)
  in
  "/" ^ String.concat ~sep:"/" (List.rev parts)
;;

module Call = struct
  type t =
    { tool : string
    ; command : string option
    ; paths : string list
    }
  [@@deriving sexp_of]

  let create ~cwd ~tool args =
    let field name =
      match args with
      | `Object fields ->
        (match List.Assoc.find fields ~equal:String.equal name with
         | Some (`String s) -> Some s
         | _ -> None)
      | _ -> None
    in
    let resolve p = normalize (Tool.resolve ~cwd p) in
    let paths =
      match tool with
      | "read" | "write" | "edit" ->
        Option.to_list (Option.map (field "path") ~f:resolve)
      | "ls" | "grep" | "find" ->
        [ resolve (Option.value (field "path") ~default:".") ]
      | "apply_patch" ->
        List.map
          (Apply_patch.paths (Option.value (field "input") ~default:""))
          ~f:resolve
      | _ -> []
    in
    let command = if String.equal tool "bash" then field "command" else None in
    { tool; command; paths }
  ;;
end

let tool_matches t (call : Call.t) =
  let family members =
    String.equal t.tool (List.hd_exn members)
    && List.mem members call.tool ~equal:String.equal
  in
  family edit_tools
  || family read_tools
  || glob ~paths:false ~pattern:t.tool (String.lowercase call.tool)
;;

let path_pattern ~cwd ~home pattern =
  let pattern =
    match String.chop_prefix pattern ~prefix:"~/" with
    | Some rest -> Filename.concat home rest
    | None -> pattern
  in
  let absolute =
    if Filename.is_absolute pattern
    then pattern
    else Filename.concat cwd pattern
  in
  (* Keep a trailing [/**]: [normalize] would drop its slash. *)
  let trailing = if String.is_suffix absolute ~suffix:"/" then "/" else "" in
  normalize absolute ^ trailing
;;

let pattern_matches ~cwd ~home (call : Call.t) pattern ~part =
  match call.command with
  | Some _ -> command_matches ~pattern part
  | None ->
    List.mem file_tools call.tool ~equal:String.equal
    && glob ~paths:true ~pattern:(path_pattern ~cwd ~home pattern) part
;;

(* The parts a call is matched by: a command's segments, a file tool's
   paths. *)
let parts (call : Call.t) =
  match call.command with
  | Some command ->
    let parts, kind = segments command in
    parts, kind
  | None -> call.paths, `Simple
;;

let covers rules ~cwd ~home (call : Call.t) =
  let rules = List.filter rules ~f:(fun t -> tool_matches t call) in
  List.exists rules ~f:(fun t -> Option.is_none t.pattern)
  ||
  match parts call with
  | _, `Complex | [], _ -> false
  | parts, `Simple ->
    List.for_all parts ~f:(fun part ->
      List.exists rules ~f:(fun t ->
        match t.pattern with
        | None -> true
        | Some pattern -> pattern_matches ~cwd ~home call pattern ~part))
;;

let touches t ~cwd ~home (call : Call.t) =
  tool_matches t call
  &&
  match t.pattern with
  | None -> true
  | Some pattern ->
    let parts, _ = parts call in
    List.exists parts ~f:(fun part ->
      pattern_matches ~cwd ~home call pattern ~part)
    ||
      (match call.command with
      | Some command -> command_matches ~pattern command
      | None -> false)
;;

(* Programs that run their arguments as a command: a rule for them would
   allow anything. *)
let wrappers =
  [ "env"
  ; "sudo"
  ; "doas"
  ; "su"
  ; "xargs"
  ; "exec"
  ; "command"
  ; "builtin"
  ; "eval"
  ; "source"
  ; "."
  ; "nohup"
  ; "time"
  ; "nice"
  ; "ionice"
  ; "timeout"
  ; "stdbuf"
  ; "setsid"
  ; "watch"
  ; "chroot"
  ; "unshare"
  ; "nsenter"
  ; "flock"
  ; "parallel"
  ; "bash"
  ; "sh"
  ; "zsh"
  ; "dash"
  ; "ksh"
  ; "fish"
  ]
;;

(* A program name worth a [PREFIX:*] rule: not a wrapper, an assignment
   ([CI=1 make]), or a name written so that it can mean something else
   ([\rm], ['rm'], [$CMD]). *)
let suggestable_program first =
  (not (List.mem wrappers first ~equal:String.equal))
  && not (String.exists first ~f:(fun c -> String.mem "=\\'\"$*?[{~" c))
;;

(* Programs whose first argument is a subcommand worth keeping apart:
   [git status] is not [git push]. *)
let with_subcommands =
  [ "git"
  ; "npm"
  ; "npx"
  ; "pnpm"
  ; "yarn"
  ; "bun"
  ; "cargo"
  ; "dune"
  ; "opam"
  ; "nix"
  ; "go"
  ; "docker"
  ; "podman"
  ; "kubectl"
  ; "pip"
  ; "uv"
  ; "gh"
  ; "make"
  ; "systemctl"
  ; "brew"
  ; "apt"
  ; "jj"
  ; "hg"
  ; "terraform"
  ]
;;

let suggest (call : Call.t) =
  match call.command with
  | Some command ->
    let program part = List.hd (String.split (squash_blanks part) ~on:' ') in
    (match segments command with
     | _, `Complex -> []
     | parts, `Simple
       when List.exists parts ~f:(fun part ->
              match program part with
              | None -> false
              | Some first -> not (suggestable_program first)) -> []
     | parts, `Simple ->
       List.filter_map parts ~f:(fun part ->
         match String.split (squash_blanks part) ~on:' ' with
         | [] | [ "" ] -> None
         | first :: rest ->
           let prefix =
             match rest with
             | sub :: _
               when List.mem with_subcommands first ~equal:String.equal
                    && not (String.is_prefix sub ~prefix:"-") ->
               first ^ " " ^ sub
             | _ -> first
           in
           Some { tool = "bash"; pattern = Some (prefix ^ ":*") })
       |> List.dedup_and_sort ~compare
       |> List.sort ~compare:(fun a b ->
         (* in the order they appear *)
         let index r =
           List.findi parts ~f:(fun _ p ->
             command_matches ~pattern:(Option.value_exn r.pattern) p)
           |> Option.value_map ~default:0 ~f:fst
         in
         Int.compare (index a) (index b)))
  | None ->
    if List.mem edit_tools call.tool ~equal:String.equal
    then [ { tool = "edit"; pattern = None } ]
    else [ { tool = String.lowercase call.tool; pattern = None } ]
;;
