open! Core

type t =
  | List of { state : string option }
  | View of int
  | New of
      { title : string option
      ; generate : bool
      }
  | Checkout of int
[@@deriving sexp_of, equal]

let states = [ "open"; "closed"; "all" ]

let number word =
  let digits =
    Option.value (String.chop_prefix word ~prefix:"#") ~default:word
  in
  if (not (String.is_empty digits)) && String.for_all digits ~f:Char.is_digit
  then Int.of_string_opt digits
  else None
;;

let needs_number sub = sprintf "/pr %s needs a number: /pr %s 24" sub sub

let with_number sub = function
  | [ word ] ->
    (match number word with
     | Some n -> Ok n
     | None ->
       Error
         (sprintf
            "not a pull request number: %s; %s (/pr lists them)"
            word
            (needs_number sub)))
  | [] -> Error (needs_number sub ^ " (/pr lists them)")
  | _ -> Error (sprintf "usage: /pr %s N, e.g. /pr %s 24" sub sub)
;;

let parse rest =
  let words =
    String.split_on_chars rest ~on:[ ' '; '\t'; '\n' ]
    |> List.filter ~f:(Fn.non String.is_empty)
  in
  match words with
  | [] | [ "list" ] -> Ok (List { state = None })
  | [ state ] when List.mem states state ~equal:String.equal ->
    Ok (List { state = Some state })
  | [ "list"; state ] when List.mem states state ~equal:String.equal ->
    Ok (List { state = Some state })
  | "list" :: _ ->
    Error
      (sprintf
         "usage: /pr list [open|closed|all], not /pr %s"
         (String.concat ~sep:" " words))
  | "view" :: args -> Result.map (with_number "view" args) ~f:(fun n -> View n)
  | "checkout" :: args ->
    Result.map (with_number "checkout" args) ~f:(fun n -> Checkout n)
  | "new" :: words ->
    let title =
      String.strip
        (String.drop_prefix (String.lstrip rest) (String.length "new"))
    in
    let generate, title =
      match words with
      | "--generate" :: _ ->
        ( true
        , String.strip (String.drop_prefix title (String.length "--generate")) )
      | _ -> false, title
    in
    Ok
      (New
         { title = Option.some_if (not (String.is_empty title)) title
         ; generate
         })
  | [ word ] when Option.is_some (number word) ->
    Ok (View (Option.value_exn (number word)))
  | word :: _ ->
    Error
      (sprintf
         "unknown /pr subcommand %S: use list [open|closed|all], view N, new \
          [--generate] [title] or checkout N"
         word)
;;

let subcommands =
  [ "list", "pick a pull request (open; closed or all after it)"
  ; "view", "N: show a pull request, its checks and its diff"
  ; ( "new"
    , "[--generate] [title]: create a pull request from the current branch \
       (--generate: the model drafts it)" )
  ; "checkout", "N: check a pull request out in a git worktree"
  ]
;;
