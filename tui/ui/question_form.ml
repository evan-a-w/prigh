open! Core
open! Import

module Answer = struct
  type t =
    { picked : Int.Set.t
    ; own : string
    }
  [@@deriving sexp_of, equal]

  let empty = { picked = Int.Set.empty; own = "" }
end

type t =
  { questions : P.Question.t list
  ; answers : Answer.t list
  ; current : int
  ; cursor : int
  ; deadline : Time_ns.Alternate_sexp.t option
  }
[@@deriving sexp_of, equal]

let create ?deadline questions =
  { questions
  ; answers = List.map questions ~f:(fun _ -> Answer.empty)
  ; current = 0
  ; cursor = 0
  ; deadline
  }
;;

let questions t = t.questions
let deadline t = t.deadline

let deadline_note t ~utc_offset =
  Option.map t.deadline ~f:(fun at ->
    sprintf
      "unanswered, the agent carries on by itself at %s"
      (Loop_status.clock at ~utc_offset))
;;

let current t = t.current
let cursor t = t.cursor
let question t = List.nth_exn t.questions t.current
let rows t = List.length (question t).choices + 1
let on_own_answer t = t.cursor = rows t - 1

let is_picked t ~question ~choice =
  Set.mem (List.nth_exn t.answers question).picked choice
;;

let own_answer t ~question = (List.nth_exn t.answers question).own

let update_answer t ~f =
  { t with
    answers =
      List.mapi t.answers ~f:(fun i a -> if i = t.current then f a else a)
  }
;;

let up t = { t with cursor = Int.max 0 (t.cursor - 1) }
let down t = { t with cursor = Int.min (rows t - 1) (t.cursor + 1) }

let go_to t ~row =
  { t with cursor = Int.clamp_exn row ~min:0 ~max:(rows t - 1) }
;;

(* On arriving at a question the cursor is on its answer, if it has one. *)
let show t current =
  let t = { t with current } in
  let answer = List.nth_exn t.answers current in
  let cursor =
    match Set.min_elt answer.picked with
    | Some i -> i
    | None -> if String.is_empty answer.own then 0 else rows t - 1
  in
  { t with cursor }
;;

let prev t = if t.current = 0 then t else show t (t.current - 1)

let next t =
  if t.current >= List.length t.questions - 1 then t else show t (t.current + 1)
;;

let insert t text =
  let t = { t with cursor = rows t - 1 } in
  update_answer t ~f:(fun a -> { a with own = a.own ^ text })
;;

let toggle t =
  if on_own_answer t
  then insert t " "
  else if (question t).multiple
  then
    update_answer t ~f:(fun a ->
      { a with
        picked =
          (if Set.mem a.picked t.cursor
           then Set.remove a.picked t.cursor
           else Set.add a.picked t.cursor)
      })
  else
    update_answer t ~f:(fun a -> { a with picked = Int.Set.singleton t.cursor })
;;

(* By character: UTF-8 continuation bytes go with their lead byte. *)
let drop_last_char s =
  let rec back i =
    if i > 0 && Char.to_int s.[i] land 0xC0 = 0x80 then back (i - 1) else i
  in
  if String.is_empty s then s else String.prefix s (back (String.length s - 1))
;;

let backspace t =
  update_answer
    { t with cursor = rows t - 1 }
    ~f:(fun a -> { a with own = drop_last_char a.own })
;;

let set_own t own =
  update_answer { t with cursor = rows t - 1 } ~f:(fun a -> { a with own })
;;

let answers t =
  List.map2_exn t.questions t.answers ~f:(fun q a ->
    List.filter_mapi q.choices ~f:(fun i c ->
      if Set.mem a.picked i then Some c.label else None)
    @
    if String.is_empty (String.strip a.own) then [] else [ String.strip a.own ])
;;

let answered t ~question =
  not (List.is_empty (List.nth_exn (answers t) question))
;;

let accept t =
  let t =
    if (question t).multiple
    then t
    else if on_own_answer t
    then update_answer t ~f:(fun a -> { a with picked = Int.Set.empty })
    else
      update_answer t ~f:(fun _ ->
        { Answer.picked = Int.Set.singleton t.cursor; own = "" })
  in
  if t.current >= List.length t.questions - 1
  then `Done (answers t)
  else `Next (show t (t.current + 1))
;;
