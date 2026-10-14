open! Core
open! Import
open Html
module Action = App.Action

let what (p : Jump.Point.t) ~n =
  match p.kind with
  | User -> sprintf "Your message %d" n
  | Compaction -> "Context compacted"
  | Error -> "The model returned an error"
;;

let tick ~inject times ~n (p : Jump.Point.t) =
  let time = Option.map p.at ~f:(Message_time.short times) in
  let text =
    match p.kind, p.text with
    | User, "" -> "(an image)"
    | (User | Error | Compaction), text -> text
  in
  let label =
    String.concat
      ~sep:", "
      (List.filter
         (what p ~n :: text :: Option.to_list time)
         ~f:(Fn.non String.is_empty))
  in
  Node.button
    ~attrs:
      [ classes
          [ "tick"
          ; (match p.kind with
             | User -> "user"
             | Compaction -> "compaction"
             | Error -> "error")
          ]
          [ ( "unloaded"
            , match p.target with
              | Message _ -> true
              | Entry _ -> false )
          ]
      ; Attr.type_ "button"
      ; Attr.tabindex (-1)
      ; Attr.create "aria-label" label
      ; (match p.target with
         | Entry e -> Attr.create "data-jump-entry" (Int.to_string e)
         | Message _ -> Attr.empty)
      ; Attr.on_click (fun _ -> inject (Action.Jump_to p.target))
      ]
    [ Node.span
        ~attrs:[ Attr.class_ "tip"; Attr.create "aria-hidden" "true" ]
        [ (match p.kind with
           | User -> Node.none
           | Compaction | Error -> span ~cls:"tip-what" (what p ~n))
        ; (if String.is_empty text then Node.none else span ~cls:"tip-text" text)
        ; Option.value_map time ~default:Node.none ~f:(span ~cls:"tip-time")
        ]
    ]
;;

(* About this many marks at most: a few px each down the chat's height. Past it,
   evenly picked (the newest always); the jump list has every one. *)
let most = 120

let thin points =
  let n = List.length points in
  if n <= most
  then points
  else
    List.filteri points ~f:(fun i _ ->
      i = 0 || i = n - 1 || i * most / n <> (i - 1) * most / n)
;;

let render ~inject ~narrow times points =
  let list_button =
    button
      ~cls:"icon ghost jump-list"
      ~title:"Jump to a message (Ctrl+J)"
      ~label:"Jump to a message"
      ~attrs:
        [ Attr.create "aria-keyshortcuts" "Control+J"
        ; Attr.create "aria-haspopup" "dialog"
        ]
      ~on_click:(inject Action.Open_jump_list)
      [ icon Toc ]
  in
  let _, ticks =
    List.fold_map points ~init:0 ~f:(fun users (p : Jump.Point.t) ->
      let users =
        match p.kind with
        | User -> users + 1
        | Compaction | Error -> users
      in
      users, (p, users))
  in
  let ticks =
    List.map (thin ticks) ~f:(fun (p, n) -> tick ~inject times ~n p)
  in
  div
    ~cls:"jump-nav"
    [ list_button
    ; (if narrow
       then Node.none
       else
         Node.nav
           ~attrs:
             [ Attr.class_ "rail"; Attr.create "aria-label" "Your messages" ]
           ticks)
    ]
;;

module Memo = struct
  type t =
    { points : Jump.Point.t list
    ; narrow : bool
    ; times : Message_time.t
    ; inject : Action.t -> unit Effect.t
    ; node : Node.t
    }
end

let last : Memo.t option ref = ref None

let view (m : App.Model.t) ~inject =
  let points =
    Jump.points
      m.jump
      ~path:(Option.map m.state ~f:(fun s -> s.session_path))
      ~paging:m.paging
      m.chat
  in
  let users =
    List.count points ~f:(fun (p : Jump.Point.t) ->
      Outline.Kind.equal p.kind User)
  in
  if users < 2
  then Node.none
  else (
    let times = App.Model.message_time m in
    match !last with
    | Some memo
      when phys_equal memo.inject inject
           && Bool.equal memo.narrow m.narrow
           && Message_time.equal memo.times times
           && [%equal: Jump.Point.t list] memo.points points -> memo.node
    | Some _ | None ->
      let node =
        Node.lazy_ (lazy (render ~inject ~narrow:m.narrow times points))
      in
      last := Some { points; narrow = m.narrow; times; inject; node };
      node)
;;
