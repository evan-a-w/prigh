open! Core
open! Import

let methods =
  [ "get_review"
  ; "review_comment"
  ; "review_reply"
  ; "review_set_status"
  ; "review_delete"
  ; "review_submit"
  ; "review_decide"
  ; "review_approve"
  ; "review_unapprove"
  ]
;;

let param params name =
  match params with
  | `Object fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None
;;

let string params name =
  match param params name with
  | Some (`String s) -> Ok s
  | Some _ -> Or_error.errorf "param %S must be a string" name
  | None -> Or_error.errorf "missing param %S" name
;;

let int params name =
  match param params name with
  | Some (`Number n) ->
    (match Int.of_string_opt n with
     | Some n -> Ok n
     | None -> Or_error.errorf "param %S must be a whole number" name)
  | Some _ -> Or_error.errorf "param %S must be a whole number" name
  | None -> Or_error.errorf "missing param %S" name
;;

let optional f params name =
  match param params name with
  | None | Some `Null -> Ok None
  | Some _ -> Or_error.map (f params name) ~f:Option.some
;;

(* [side] defaults to new; [start_col] and [end_col] come together. *)
let range params ~first ~last : Review_comment.Range.t Or_error.t =
  let open Or_error.Let_syntax in
  let%bind side =
    match%bind optional string params "side" with
    | None -> Ok Review_comment.Side.New
    | Some s -> Review_comment.Side.of_string s
  in
  let%bind start_col = optional int params "start_col" in
  let%bind end_col = optional int params "end_col" in
  let%map columns =
    match start_col, end_col with
    | None, None -> Ok None
    | Some start_col, Some end_col ->
      Ok (Some { Review_comment.Columns.start_col; end_col })
    | Some _, None | None, Some _ ->
      Or_error.error_string
        "a selection needs both start_col and end_col (or neither, for whole \
         lines)"
  in
  { Review_comment.Range.first; last; side; columns }
;;

let bool params name =
  match param params name with
  | Some `True -> Ok true
  | Some `False -> Ok false
  | _ -> Or_error.errorf "missing param %S (true or false)" name
;;

let now agent = Eio.Time.now (Eio.Stdenv.clock (Agent.env agent))

let comment agent ~f =
  Or_error.map (Agent.update_review agent ~f) ~f:Rpc_json.review_comment
;;

let review agent ~f =
  Or_error.map
    (Agent.update_review agent ~f:(fun r -> Ok ((), f r)))
    ~f:(fun () -> Rpc_json.review (Agent.review_state agent))
;;

let dispatch agent ~meth ~params =
  let open Or_error.Let_syntax in
  match meth with
  | "get_review" -> Some (Ok (Rpc_json.review (Agent.review_state agent)))
  | "review_comment" ->
    Some
      (let%bind root = string params "root" in
       let%bind path = string params "path" in
       let%bind first = int params "first" in
       let%bind last = int params "last" in
       let%bind range = range params ~first ~last in
       let%bind anchor = string params "anchor" in
       let%bind text = string params "text" in
       comment agent ~f:(fun r ->
         Review_state.add r ~root ~path ~range ~anchor ~text ~at:(now agent)))
  | "review_reply" ->
    Some
      (let%bind id = string params "id" in
       let%bind text = string params "text" in
       comment agent ~f:(fun r ->
         Review_state.reply r ~id ~author:User ~text ~at:(now agent) ()))
  | "review_set_status" ->
    Some
      (let%bind id = string params "id" in
       let%bind status =
         Or_error.bind
           (string params "status")
           ~f:Review_comment.Status.of_string
       in
       comment agent ~f:(fun r -> Review_state.set_status r ~id status))
  | "review_delete" ->
    Some
      (let%bind id = string params "id" in
       let%map () =
         Agent.update_review agent ~f:(fun r ->
           Or_error.map (Review_state.delete r ~id) ~f:(fun c -> (), c))
       in
       `Object [])
  | "review_submit" ->
    Some
      (let%bind root = optional string params "root" in
       let%map count = Agent.submit_review ?root agent in
       `Object [ "count", `Number (Int.to_string count) ])
  | "review_decide" ->
    Some
      (let%bind id = string params "id" in
       let%bind post = int params "post" in
       let%bind accept = bool params "accept" in
       Or_error.map
         (Agent.decide_proposal agent ~id ~post ~accept)
         ~f:Rpc_json.review_comment)
  | "review_approve" ->
    Some
      (let%bind root = optional string params "root" in
       let%bind path = string params "path" in
       let%bind hash = string params "hash" in
       review agent ~f:(fun _ -> Review_state.approve ?root ~path ~hash ()))
  | "review_unapprove" ->
    Some
      (let%bind root = optional string params "root" in
       let%bind path = string params "path" in
       review agent ~f:(fun r -> Review_state.unapprove ?root r ~path))
  | _ -> None
;;
