open! Core
open! Import

module Party = struct
  type t =
    { id : string
    ; name : string option
    }
  [@@deriving sexp_of]

  let to_string t =
    match t.name with
    | Some name -> sprintf "%s (%s)" t.id name
    | None -> t.id
  ;;
end

module Question = struct
  type t =
    { id : string
    ; asker : Party.t
    ; target : Party.t
    ; question : string
    }
  [@@deriving sexp_of]
end

module Outcome = struct
  type t =
    | Said of string
    | Silent
    | Aborted
    | Failed of string
  [@@deriving sexp_of]
end

module Open = struct
  type t =
    { question : Question.t
    ; mutable seen : bool (** given to the asked session's model *)
    }
end

module Mail = struct
  type t =
    | Question of Open.t
    | Answer of
        { question : Question.t
        ; answered : bool
        ; text : string
        }
end

type t =
  { mutable seq : int
  ; mutable open_ : Open.t list (** oldest first *)
  ; mail : Mail.t Queue.t String.Table.t (** by the receiving session's id *)
  ; mutable wake : (session:string -> unit Or_error.t) option
  }

let max_open = 4
let answer_cut = 6000

let create () =
  { seq = 0; open_ = []; mail = String.Table.create (); wake = None }
;;

let set_wake t wake = t.wake <- Some wake

let wake t ~session =
  match t.wake with
  | None ->
    Or_error.error_string
      "sessions cannot reach each other in this mode of prigh: read that \
       session with the sessions tool instead"
  | Some wake ->
    (match wake ~session with
     | result -> result
     | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
     | exception exn -> Or_error.of_exn exn)
;;

let post t ~session mail =
  Queue.enqueue (Hashtbl.find_or_add t.mail session ~default:Queue.create) mail
;;

let unpost t ~session mail =
  Option.iter (Hashtbl.find t.mail session) ~f:(fun queue ->
    Queue.filter_inplace queue ~f:(fun m -> not (phys_equal m mail)))
;;

let asked t ~session =
  List.filter_map t.open_ ~f:(fun (o : Open.t) ->
    Option.some_if (String.equal o.question.target.id session) o.question)
;;

let close t (question : Question.t) =
  t.open_
  <- List.filter t.open_ ~f:(fun o -> not (phys_equal o.question question))
;;

let ask t ~(asker : Party.t) ~(target : Party.t) ~question =
  let mine =
    List.filter t.open_ ~f:(fun o -> String.equal o.question.asker.id asker.id)
  in
  if List.length mine >= max_open
  then
    Or_error.errorf
      "%d questions of this session are still open (%s): wait for their \
       answers, which arrive as messages"
      max_open
      (String.concat ~sep:", " (List.map mine ~f:(fun o -> o.question.id)))
  else (
    t.seq <- t.seq + 1;
    let question =
      { Question.id = sprintf "q%d" t.seq; asker; target; question }
    in
    let opened = { Open.question; seen = false } in
    let mail = Mail.Question opened in
    t.open_ <- t.open_ @ [ opened ];
    post t ~session:target.id mail;
    match wake t ~session:target.id with
    | Ok () -> Ok question
    | Error _ as error ->
      close t question;
      unpost t ~session:target.id mail;
      error)
;;

let reply t (question : Question.t) ~answered text =
  close t question;
  let cut = Truncate.head ~max_bytes:answer_cut ~max_lines:Int.max_value text in
  let text =
    if cut.truncated
    then cut.text ^ "\n[the answer was cut here: ask for less]"
    else text
  in
  let mail = Mail.Answer { question; answered; text } in
  let session = question.asker.id in
  post t ~session mail;
  match wake t ~session with
  | Ok () -> ()
  | Error error ->
    unpost t ~session mail;
    eprintf
      "prigh: the answer to %s of session %s is dropped: %s\n%!"
      question.id
      session
      (Error.to_string_hum error)
;;

let answer t ~session ~id text =
  let asked = asked t ~session in
  match List.find asked ~f:(fun q -> String.equal q.id id) with
  | Some question -> Ok (reply t question ~answered:true text)
  | None ->
    (match asked with
     | [] ->
       Or_error.errorf
         "no question %S is open: no session waits for an answer of this one"
         id
     | asked ->
       Or_error.errorf
         "no question %S is open; open: %s"
         id
         (String.concat ~sep:", " (List.map asked ~f:(fun q -> q.id))))
;;

let has_mail t ~session =
  Option.exists (Hashtbl.find t.mail session) ~f:(Fn.non Queue.is_empty)
;;

let message : Mail.t -> Message.t = function
  | Question { question = { id; asker; question; target = _ }; seen = _ } ->
    Message.user
      ~origin:
        { Injected.empty with
          source =
            Some
              (Session_question { session = asker.id; name = asker.name; id })
        }
      (sprintf
         "[session %s asks %s]\n\n\
          %s\n\n\
          [Answer with answer_session (id %s), using your tools first if it \
          takes them: the answer goes to that session, not to the user. Then \
          carry on with your work.]"
         (Party.to_string asker)
         id
         question
         id)
  | Answer { question = { id; target; _ }; answered; text } ->
    Message.user
      ~origin:
        { Injected.empty with
          source =
            Some
              (Session_answer
                 { session = target.id; name = target.name; id; answered })
        }
      (sprintf
         "[session %s %s %s]\n\n%s"
         (Party.to_string target)
         (if answered then "answered" else "did not answer")
         id
         text)
;;

let take_mail t ~session =
  match Hashtbl.find_and_remove t.mail session with
  | None -> []
  | Some queue ->
    List.map (Queue.to_list queue) ~f:(fun mail ->
      (match mail with
       | Question opened -> opened.seen <- true
       | Answer _ -> ());
      message mail)
;;

let run_ended t ~session (outcome : Outcome.t) =
  List.filter t.open_ ~f:(fun o ->
    o.seen && String.equal o.question.target.id session)
  |> List.iter ~f:(fun (o : Open.t) ->
    reply
      t
      o.question
      ~answered:false
      (match outcome with
       | Said text ->
         "Its run ended without answer_session; its last message:\n\n" ^ text
       | Silent -> "Its run ended without an answer."
       | Aborted -> "Its run was aborted before it answered."
       | Failed error -> "Its run failed before it answered: " ^ error))
;;
