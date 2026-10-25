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
       let%bind anchor = string params "anchor" in
       let%bind text = string params "text" in
       comment agent ~f:(fun r ->
         Review_state.add
           r
           ~root
           ~path
           ~range:{ first; last }
           ~anchor
           ~text
           ~at:(now agent)))
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
      (let%map count = Agent.submit_review agent in
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
      (let%bind path = string params "path" in
       let%bind hash = string params "hash" in
       review agent ~f:(fun _ -> Review_state.approve ~path ~hash))
  | "review_unapprove" ->
    Some
      (let%bind path = string params "path" in
       review agent ~f:(fun r -> Review_state.unapprove r ~path))
  | _ -> None
;;
