open! Core
open! Import

let github = "github.com"

module Login = struct
  type t =
    | Host of
        { host : string
        ; kind : string option
        ; method_ : string option
        }
    | Ask_host of
        { kind : string
        ; method_ : string option
        }
  [@@deriving sexp_of, equal]

  let method_word = function
    | "token" | "oauth" -> true
    | _ -> false
  ;;

  let looks_like_host word = String.mem word '.'

  let parse (accounts : P.Forge_account.t list) args =
    let usage text = Some (Error ("usage: " ^ text)) in
    let host ?kind host method_ = Some (Ok (Host { host; kind; method_ })) in
    match args with
    | "github" :: rest ->
      (match rest with
       | [] -> host ~kind:"github" github None
       | [ m ] when method_word m -> host ~kind:"github" github (Some m)
       | [ h ] -> host ~kind:"github" h None
       | [ h; m ] when method_word m -> host ~kind:"github" h (Some m)
       | _ -> usage "/login github [HOST] [token], e.g. /login github token")
    | ("forgejo" | "gitea") :: rest ->
      (match rest with
       | [] -> Some (Ok (Ask_host { kind = "forgejo"; method_ = None }))
       | [ m ] when method_word m ->
         Some (Ok (Ask_host { kind = "forgejo"; method_ = Some m }))
       | [ h ] -> host ~kind:"forgejo" h None
       | [ h; m ] when method_word m -> host ~kind:"forgejo" h (Some m)
       | _ ->
         usage "/login forgejo HOST [token], e.g. /login forgejo codeberg.org")
    | word :: rest ->
      let listed =
        List.find accounts ~f:(fun (a : P.Forge_account.t) ->
          String.equal a.host word)
      in
      if Option.is_none listed && not (looks_like_host word)
      then None
      else (
        let kind = Option.map listed ~f:(fun a -> a.kind) in
        match rest with
        | [] -> host ?kind word None
        | [ m ] when method_word m -> host ?kind word (Some m)
        | _ -> usage (sprintf "/login %s [token]" word))
    | [] -> None
  ;;

  let host_question ~kind =
    match kind with
    | "github" -> "GitHub Enterprise host (e.g. github.example.com)"
    | _ -> "Forgejo or Gitea host (e.g. codeberg.org, git.example.com)"
  ;;
end

let logout_host (accounts : P.Forge_account.t list) word =
  if String.equal word "github"
  then Some github
  else if
    String.mem word '.'
    || List.exists accounts ~f:(fun a -> String.equal a.host word)
  then Some word
  else None
;;

let find (accounts : P.Forge_account.t list) host =
  List.find accounts ~f:(fun a -> String.equal a.host host)
;;

let name (a : P.Forge_account.t) = sprintf "%s (%s)" a.label a.host

let signed_in_as (a : P.Forge_account.t) =
  match a.user, a.method_ with
  | Some user, Some m -> sprintf "signed in as %s (%s)" user m
  | None, Some m -> sprintf "signed in (%s)" m
  | _, None -> "not signed in"
;;

let section = "Git hosting"

let login_items accounts =
  List.concat_map accounts ~f:(fun (a : P.Forge_account.t) ->
    let detail =
      if P.Forge_account.signed_in a
      then signed_in_as a
      else if a.oauth
      then "OAuth in the browser"
      else "paste a token"
    in
    let main =
      Picker.Item.create
        ~id:("forge:" ^ a.host)
        ~detail
        ~search:(sprintf "%s %s %s" a.label a.kind a.host)
        ~marked:(P.Forge_account.signed_in a)
        ~section
        (name a)
    in
    if a.oauth
    then
      [ main
      ; Picker.Item.create
          ~id:("forge-token:" ^ a.host)
          ~detail:"paste a token"
          ~search:(sprintf "%s %s %s token" a.label a.kind a.host)
          ~section
          (name a ^ " with a token")
      ]
    else [ main ])
  @ [ Picker.Item.create
        ~id:"forge-host:forgejo"
        ~detail:"Forgejo or Gitea on any host: asks for it"
        ~search:"forgejo gitea another host"
        ~section
        "Forgejo: another host…"
    ]
;;

let login_of_item_id accounts id =
  let kind host =
    Option.map (find accounts host) ~f:(fun (a : P.Forge_account.t) -> a.kind)
  in
  match String.lsplit2 id ~on:':' with
  | Some ("forge", host) ->
    Some (Login.Host { host; kind = kind host; method_ = None })
  | Some ("forge-token", host) ->
    Some (Host { host; kind = kind host; method_ = Some "token" })
  | Some ("forge-host", kind) -> Some (Ask_host { kind; method_ = None })
  | _ -> None
;;

let logout_items accounts =
  List.filter_map accounts ~f:(fun (a : P.Forge_account.t) ->
    Option.some_if
      (P.Forge_account.signed_in a)
      (Picker.Item.create
         ~id:("forge:" ^ a.host)
         ~detail:(signed_in_as a)
         ~search:(sprintf "%s %s %s" a.label a.kind a.host)
         ~section
         (name a)))
;;

let logout_of_item_id id =
  match String.lsplit2 id ~on:':' with
  | Some ("forge", host) -> Some host
  | _ -> None
;;

let login_completions accounts =
  [ "github", "GitHub (github.com); github HOST for GitHub Enterprise"
  ; "forgejo", "forgejo HOST: Forgejo or Gitea, e.g. codeberg.org"
  ]
  @ List.filter_map accounts ~f:(fun (a : P.Forge_account.t) ->
    Option.some_if
      (not (String.equal a.host github))
      (a.host, sprintf "%s · %s" a.label (signed_in_as a)))
;;

let logout_completions accounts =
  List.filter_map accounts ~f:(fun (a : P.Forge_account.t) ->
    Option.some_if
      (P.Forge_account.signed_in a)
      (a.host, sprintf "%s · %s" a.label (signed_in_as a)))
;;

let describe (accounts : P.Forge_account.t list) : Content.t =
  let width =
    List.fold accounts ~init:0 ~f:(fun acc a ->
      Int.max acc (String.length a.host))
  in
  let label_width =
    List.fold accounts ~init:0 ~f:(fun acc a ->
      Int.max acc (String.length a.label))
  in
  let dim = Style.dim Style.plain in
  let rows =
    List.concat_map accounts ~f:(fun (a : P.Forge_account.t) ->
      let state : Content.Line.t =
        if P.Forge_account.signed_in a
        then [ { text = signed_in_as a; style = Style.fg Green } ]
        else [ { text = "not signed in"; style = Style.fg Gray } ]
      in
      let note : Content.t =
        match a.oauth, a.oauth_note with
        | false, Some note -> [ [ { text = "  " ^ note; style = dim } ] ]
        | _ -> []
      in
      ([ { Content.Span.text = Text_width.pad_right a.host ~width ^ "  "
         ; style = Style.bold Style.plain
         }
       ; { text = Text_width.pad_right a.label ~width:label_width ^ "  "
         ; style = Style.plain
         }
       ]
       @ state)
      :: note)
  in
  [ [ { Content.Span.text = "Git hosting"; style = Style.bold Style.plain } ] ]
  @ rows
  @ [ [ { Content.Span.text =
            "/login github or /login forgejo HOST signs in; /logout HOST signs \
             out"
        ; style = dim
        }
      ]
    ]
;;
