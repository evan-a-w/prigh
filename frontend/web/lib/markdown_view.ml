open! Core
open! Import
module Inline = Markdown.Inline
module Block = Markdown.Block

let cls name = [ Attr.class_ name ]

let copy_button =
  Node.button
    ~attrs:[ Attr.class_ "copy"; Attr.type_ "button"; Attr.title "Copy" ]
    [ Node.text "Copy" ]
;;

let code_block ?title ~lang ~text ~closed () =
  Node.div
    ~attrs:
      [ Attr.classes ([ "code"; "copyable" ] @ if closed then [] else [ "open" ])
      ]
    [ Node.div
        ~attrs:(cls "code-head")
        (Option.to_list
           (Option.map title ~f:(fun title ->
              Node.span ~attrs:(cls "code-title") [ Node.text title ]))
         @ [ Node.span
               ~attrs:(cls "code-lang")
               [ Node.text (if String.is_empty lang then "text" else lang) ]
           ; copy_button
           ])
    ; Node.pre
        ~attrs:(cls "copy-text")
        [ Node.code
            ~attrs:
              (if String.is_empty lang then [] else cls ("language-" ^ lang))
            (Highlight_view.code (Highlight.Lang.of_name lang) text)
        ]
    ]
;;

(* Inside a link, text is not linked again. *)
let rec inline ?(linked = false) (i : Inline.t) =
  match i with
  | Text s when linked -> Node.text s
  | Text s -> Node.fragment (Session_link_view.linkify s)
  | Code s when linked -> Node.code [ Node.text s ]
  | Code s ->
    Option.value_or_thunk (Session_link_view.whole s) ~default:(fun () ->
      Node.code [ Node.text s ])
  | Strong l -> Node.strong (inlines ~linked l)
  | Emph l -> Node.em (inlines ~linked l)
  | Strike l -> Node.del (inlines ~linked l)
  | Break -> Node.br ()
  | Math { tex; display } -> Math_view.render ~display tex
  | Link { href; children } ->
    (match Session_link.of_href href with
     | Some id -> Session_link_view.anchor id (inlines ~linked:true children)
     | None ->
       Node.a
         ~attrs:
           [ Attr.href href
           ; Attr.target "_blank"
           ; Attr.create "rel" "noopener noreferrer"
           ]
         (inlines ~linked:true children))

and inlines ?linked l = List.map l ~f:(inline ?linked)

let align_attr (align : Markdown.Align.t) =
  match align with
  | Default -> []
  | Left -> cls "left"
  | Center -> cls "center"
  | Right -> cls "right"
;;

(* Each block takes its direction from its text (Hebrew, Arabic...). *)
let auto = [ Attr.create "dir" "auto" ]

let rec block ?(tight = false) (b : Block.t) =
  match b with
  | Paragraph l when tight -> Node.fragment (inlines l)
  | Paragraph l -> Node.p ~attrs:auto (inlines l)
  | Heading (level, l) ->
    let h =
      match level with
      | 1 -> Node.h1
      | 2 -> Node.h2
      | 3 -> Node.h3
      | 4 -> Node.h4
      | 5 -> Node.h5
      | _ -> Node.h6
    in
    h ~attrs:auto (inlines l)
  | Math tex | Code { lang = "math"; text = tex; closed = true } ->
    Math_view.render ~display:true tex
  | Code { lang; text; closed } -> code_block ~lang ~text ~closed ()
  | Quote blocks -> Node.blockquote ~attrs:auto (List.map blocks ~f:block)
  | Rule -> Node.hr ()
  | List { start; tight; items } ->
    let item ({ checked; blocks } : Block.item) =
      let content = List.map blocks ~f:(block ~tight) in
      match checked with
      | None -> Node.li ~attrs:auto content
      | Some checked ->
        Node.li
          ~attrs:(auto @ cls "task")
          (Node.input
             ~attrs:
               ([ Attr.type_ "checkbox"; Attr.disabled ]
                @ if checked then [ Attr.checked ] else [])
             ()
           :: content)
    in
    (match start with
     | None -> Node.ul (List.map items ~f:item)
     | Some start ->
       Node.ol
         ~attrs:
           (if start = 1
            then []
            else [ Attr.create "start" (Int.to_string start) ])
         (List.map items ~f:item))
  | Table { aligns; header; rows } ->
    let row ~head cells =
      Node.tr
        (List.map2_exn aligns cells ~f:(fun align c ->
           let attrs = align_attr align in
           if head
           then Node.th ~attrs (inlines c)
           else Node.td ~attrs (inlines c)))
    in
    Node.div
      ~attrs:(cls "table-wrap")
      [ Node.table
          [ Node.thead [ row ~head:true header ]
          ; Node.tbody (List.map rows ~f:(row ~head:false))
          ]
      ]
;;

let blocks ~streaming text =
  List.map (Markdown.parse ~partial:streaming text) ~f:block
;;

(* Text as parts, one per top-level block that a streaming reply has
   finished ([Markdown.block_starts]): each parsed once, as a lazy node that
   virtual_dom skips when it is reused. A streaming reply's last part, still
   arriving, is parsed again at each delta; once the reply ends it becomes a
   part like the others, so the DOM drawn while it streamed is patched rather
   than drawn again (which lost the selection and the code blocks' state). *)
let parts = String.Table.create ()

let part text =
  if Hashtbl.length parts > 10_000 then Hashtbl.clear parts;
  (* Drawn with the session list: its ids are links. *)
  let known = Session_link_view.known () in
  let key = sprintf "%d:%s" (Session_link.Known.stamp known) text in
  Hashtbl.find_or_add parts key ~default:(fun () ->
    Node.lazy_
      (lazy
        (Session_link_view.within known (fun () ->
           Node.div ~attrs:(cls "md-part") (blocks ~streaming:false text)))))
;;

(* The streaming reply last rendered: its text, block starts and finished
   parts (with their bounds), which the next delta, extending the text,
   keeps. *)
module Streamed = struct
  type t =
    { text : string
    ; links : int
    ; starts : int list
    ; settled : ((int * int) * Node.t) list
    }
end

let streamed : Streamed.t option ref = ref None

let render ?(streaming = false) text =
  let links = Session_link.Known.stamp (Session_link_view.known ()) in
  let prev =
    match !streamed with
    | Some p
      when streaming && p.links = links && String.is_prefix text ~prefix:p.text
      -> Some p
    | _ -> None
  in
  let starts =
    match prev with
    | Some p ->
      Markdown.block_starts_extending ~prev:p.text ~prev_starts:p.starts text
    | None -> Markdown.block_starts text
  in
  let bounds = List.zip_exn (0 :: starts) (starts @ [ String.length text ]) in
  let settled_bounds = List.drop_last_exn bounds in
  let prev_settled =
    Option.value_map prev ~default:[] ~f:(fun p -> p.settled)
  in
  let rec settle bounds prev =
    match bounds, prev with
    | [], _ -> []
    | b :: bounds, (b', node) :: prev when [%equal: int * int] b b' ->
      (b, node) :: settle bounds prev
    | ((pos, stop) as b) :: bounds, _ ->
      (b, part (String.sub text ~pos ~len:(stop - pos))) :: settle bounds []
  in
  let settled = settle settled_bounds prev_settled in
  if streaming then streamed := Some { text; links; starts; settled };
  let last_pos, _ = List.last_exn bounds in
  let last = String.drop_prefix text last_pos in
  Node.div
    ~attrs:(cls "markdown")
    (List.map settled ~f:snd
     @ [ (if streaming
          then Node.div ~attrs:(cls "md-part") (blocks ~streaming:true last)
          else part last)
       ])
;;
