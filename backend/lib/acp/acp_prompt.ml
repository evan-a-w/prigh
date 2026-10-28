open! Core
open! Import

type t =
  { text : string
  ; attachments : string list
  ; images : Json.t list [@sexp.opaque]
  }
[@@deriving sexp_of]

let field name json = Option.bind (Json.member name json) ~f:Json.string

module Target = struct
  type t =
    | File of
        { path : string
        ; lines : (int * int) option
        }
    | Other of string

  (* [L12-L30], [L12-30] or [L12]. *)
  let lines fragment =
    let line s =
      Option.bind (String.chop_prefix s ~prefix:"L") ~f:Int.of_string_opt
    in
    match String.lsplit2 fragment ~on:'-' with
    | Some (a, b) ->
      (match line a, Option.first_some (line b) (Int.of_string_opt b) with
       | Some a, Some b when a >= 1 && b >= a -> Some (a, b)
       | _ -> None)
    | None -> Option.map (line fragment) ~f:(fun l -> l, l)
  ;;

  let of_uri uri =
    let parsed = Uri.of_string uri in
    match Uri.scheme parsed with
    | Some "file" ->
      File
        { path = Uri.pct_decode (Uri.path parsed)
        ; lines = Option.bind (Uri.fragment parsed) ~f:lines
        }
    | None when Filename.is_absolute uri ->
      (match String.rsplit2 uri ~on:'#' with
       | Some (path, fragment) when Option.is_some (lines fragment) ->
         File { path; lines = lines fragment }
       | _ -> File { path = uri; lines = None })
    | _ -> Other uri
  ;;

  let mention ?name t =
    match t with
    | File { path; lines = None } -> "@" ^ path
    | File { path; lines = Some (a, b) } when a = b ->
      sprintf "@%s (line %d)" path a
    | File { path; lines = Some (a, b) } -> sprintf "@%s (lines %d-%d)" path a b
    | Other uri -> sprintf "[%s](%s)" (Option.value name ~default:uri) uri
  ;;
end

let selection_note = function
  | Target.File { lines = Some _; _ } -> true
  | File { lines = None; _ } | Other _ -> false
;;

let file_block ~path contents =
  sprintf
    "<file path=%S>\n%s%s</file>"
    path
    contents
    (if String.is_suffix contents ~suffix:"\n" then "" else "\n")
;;

let image ~mime_type ~data =
  `Object [ "mime_type", `String mime_type; "data", `String data ]
;;

let is_image mime = String.is_prefix mime ~prefix:"image/"

(* Pieces of text, joined with a space where neither side has one. *)
let join pieces =
  List.fold pieces ~init:"" ~f:(fun acc piece ->
    if String.is_empty acc || String.is_empty piece
    then acc ^ piece
    else if
      Char.is_whitespace (String.get acc (String.length acc - 1))
      || Char.is_whitespace (String.get piece 0)
    then acc ^ piece
    else acc ^ " " ^ piece)
;;

let of_blocks blocks =
  let open Or_error.Let_syntax in
  let%map parts =
    List.mapi blocks ~f:(fun i block ->
      let uri = Option.value (field "uri" block) ~default:"" in
      match field "type" block with
      | Some "text" ->
        Ok (Option.value (field "text" block) ~default:"", [], [], [], false)
      | Some "image" ->
        (match field "data" block, field "mimeType" block with
         | Some data, Some mime_type ->
           Ok ("", [], [ image ~mime_type ~data ], [], false)
         | _ ->
           Or_error.errorf "prompt block %d: an image needs data and mimeType" i)
      | Some "resource_link" ->
        let target = Target.of_uri uri in
        let mention = Target.mention ?name:(field "name" block) target in
        (match target with
         | File { path; _ } ->
           Ok (mention, [ path ], [], [], selection_note target)
         | Other _ -> Ok (mention, [], [], [], false))
      | Some "resource" ->
        let resource =
          Option.value (Json.member "resource" block) ~default:`Null
        in
        let uri = Option.value (field "uri" resource) ~default:"" in
        let target = Target.of_uri uri in
        let mention = Target.mention target in
        let path =
          match target with
          | File { path; _ } -> path
          | Other uri -> uri
        in
        (match field "text" resource, field "blob" resource with
         | Some text, _ ->
           Ok (mention, [], [], [ file_block ~path text ], selection_note target)
         | None, Some data ->
           (match field "mimeType" resource with
            | Some mime_type when is_image mime_type ->
              Ok (mention, [], [ image ~mime_type ~data ], [], false)
            | mime ->
              Ok
                ( sprintf
                    "%s (a binary resource%s, not included)"
                    mention
                    (Option.value_map
                       mime
                       ~default:""
                       ~f:(sprintf " of type %s"))
                , []
                , []
                , []
                , false ))
         | None, None ->
           Or_error.errorf "prompt block %d: a resource needs text or blob" i)
      | Some "audio" ->
        Or_error.errorf "prompt block %d: prigh does not take audio" i
      | Some other ->
        Or_error.errorf
          "prompt block %d: unknown content type %S (prigh takes text, image, \
           resource_link and resource)"
          i
          other
      | None -> Or_error.errorf "prompt block %d: missing type" i)
    |> Or_error.all
  in
  let texts, attachments, images, files, selections =
    List.fold_right
      parts
      ~init:([], [], [], [], false)
      ~f:
        (fun
          (text, a, i, f, s) (texts, attachments, images, files, selections) ->
        text :: texts, a @ attachments, i @ images, f @ files, s || selections)
  in
  let text = join texts in
  let text =
    if selections
    then text ^ "\n\n(The line ranges are the user's selection.)"
    else text
  in
  let text =
    match files with
    | [] -> text
    | files -> text ^ "\n\n" ^ String.concat ~sep:"\n\n" files
  in
  { text
  ; attachments = List.stable_dedup attachments ~compare:String.compare
  ; images
  }
;;
