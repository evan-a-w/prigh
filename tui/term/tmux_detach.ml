open! Core

type t =
  { tmux : string
  ; session : string
  ; args : string list
  }
[@@deriving sexp_of]

let tmux ~prigh_tmux =
  match prigh_tmux with
  | Some tmux when not (String.is_empty tmux) -> tmux
  | Some _ | None -> "tmux"
;;

let sanitise name =
  String.map name ~f:(fun c ->
    if
      Char.is_alphanum c
      || Char.equal c '-'
      || Char.equal c '_'
      || Char.to_int c >= 128
    then c
    else '_')
;;

let base_name ~dir =
  match Filename.basename dir with
  | "/" | "." | "" -> "prigh"
  | base -> "prigh-" ^ sanitise base
;;

let free_name base ~exists =
  let rec from n =
    let name = sprintf "%s-%d" base n in
    if exists name then from (n + 1) else name
  in
  if exists base then from 2 else base
;;

let exact =
  [ "PATH"
  ; "HOME"
  ; "USER"
  ; "LOGNAME"
  ; "SHELL"
  ; "LANG"
  ; "LANGUAGE"
  ; "TZ"
  ; "EDITOR"
  ; "VISUAL"
  ; "TMPDIR"
  ; "TMUX_TMPDIR"
  ; "SSL_CERT_FILE"
  ; "SSL_CERT_DIR"
  ; "NIX_SSL_CERT_FILE"
  ; "ANTHROPIC_OAUTH_TOKEN"
  ]
;;

let prefixes = [ "PRIGH_"; "LC_"; "XDG_" ]
let suffixes = [ "_API_KEY"; "_proxy"; "_PROXY" ]

let passed name =
  List.mem exact name ~equal:String.equal
  || List.exists prefixes ~f:(fun prefix -> String.is_prefix name ~prefix)
  || List.exists suffixes ~f:(fun suffix -> String.is_suffix name ~suffix)
;;

let environment env =
  List.filter_map env ~f:(fun binding ->
    match String.lsplit2 binding ~on:'=' with
    | Some (name, value) when passed name -> Some (name, value)
    | Some _ | None -> None)
;;

(* tmux reads an argument ending in [;] as the end of a command, and [\;]
   at the end as [;]. *)
let escape arg =
  match String.chop_suffix arg ~suffix:";" with
  | Some rest -> rest ^ "\\;"
  | None -> arg
;;

let keep_on_failure =
  {|"$@"
status=$?
if [ "$status" -ne 0 ]; then
  printf '\nprigh-tui exited with status %s: press Enter to close this session\n' "$status"
  read -r _
fi
exit "$status"|}
;;

let create ~tmux ~exists ?name ~environment:env ~dir ~command () =
  let session =
    free_name
      (Option.value_map name ~f:sanitise ~default:(base_name ~dir))
      ~exists
  in
  let args =
    [ "new-session"
    ; "-d"
    ; "-s"
    ; session
    ; "-c"
    ; (* A format: [#] is tmux's escape. *)
      String.substr_replace_all dir ~pattern:"#" ~with_:"##"
    ]
    @ List.concat_map (environment env) ~f:(fun (name, value) ->
      [ "-e"; name ^ "=" ^ value ])
    @ [ "--"; "/bin/sh"; "-c"; keep_on_failure; "prigh-tui" ]
    @ command
  in
  { tmux; session; args = List.map args ~f:escape }
;;

let shell_quote s =
  if
    (not (String.is_empty s))
    && String.for_all s ~f:(fun c ->
      Char.is_alphanum c || String.mem "/._-+:,@%=" c)
  then s
  else Filename.quote s
;;

let message t ~inside_tmux =
  sprintf
    "prigh is running in tmux session %s: %s %s -t %s"
    t.session
    (shell_quote t.tmux)
    (if inside_tmux then "switch-client" else "attach")
    t.session
;;
