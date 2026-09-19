open! Core
open! Import

module Block = struct
  type t =
    | Text of Buffer.t
    | Thinking of Buffer.t
    | Tool_call of
        { index : int
        ; id : string
        ; name : string
        ; arguments : Buffer.t
        }

  let to_content = function
    | Text b -> Content.Text (Buffer.contents b)
    | Thinking b -> Thinking (Buffer.contents b)
    | Tool_call { id; name; arguments; index = _ } ->
      Tool_call { id; name; arguments = Buffer.contents arguments }
  ;;
end

type t =
  { model : string
  ; mutable blocks : Block.t list (* reversed *)
  }

let create ~model = { model; blocks = [] }

let apply t (event : Assistant_event.t) =
  match event, t.blocks with
  | Text_delta s, Text b :: _ -> Buffer.add_string b s
  | Text_delta s, _ ->
    let b = Buffer.create 256 in
    Buffer.add_string b s;
    t.blocks <- Text b :: t.blocks
  | Thinking_delta s, Thinking b :: _ -> Buffer.add_string b s
  | Thinking_delta s, _ ->
    let b = Buffer.create 256 in
    Buffer.add_string b s;
    t.blocks <- Thinking b :: t.blocks
  | Tool_call_start { index; id; name }, _ ->
    t.blocks
    <- Tool_call { index; id; name; arguments = Buffer.create 64 } :: t.blocks
  | Tool_call_delta { index; arguments }, _ ->
    (match
       List.find t.blocks ~f:(function
         | Tool_call c -> c.index = index
         | Text _ | Thinking _ -> false)
     with
     | Some (Tool_call c) -> Buffer.add_string c.arguments arguments
     | Some (Text _ | Thinking _) | None -> ())
;;

let content t = List.rev_map t.blocks ~f:Block.to_content

let snapshot t =
  { Message.Assistant.content = content t
  ; stop_reason = End_turn
  ; usage = Usage.zero
  ; model = t.model
  }
;;

let finish t ~stop_reason ~usage =
  { Message.Assistant.content = content t; stop_reason; usage; model = t.model }
;;
