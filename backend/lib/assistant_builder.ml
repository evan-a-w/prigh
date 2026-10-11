open! Core
open! Import

module Block = struct
  type t =
    | Text of Buffer.t
    | Thinking of
        { text : Buffer.t
        ; mutable signature : string option
        }
    | Tool_call of
        { index : int
        ; id : string
        ; name : string
        ; arguments : Buffer.t
        }
    | Server_tool of Content.Server_tool.t ref

  let to_content = function
    | Text b -> Content.Text (Buffer.contents b)
    | Thinking { text; signature } ->
      Thinking { text = Buffer.contents text; signature }
    | Tool_call { id; name; arguments; index = _ } ->
      Tool_call { id; name; arguments = Buffer.contents arguments }
    | Server_tool tool -> Server_tool !tool
  ;;
end

type t =
  { model : string
  ; mutable blocks : Block.t list (* reversed *)
  }

let create ~model = { model; blocks = [] }

let server_tool t id =
  List.find_map t.blocks ~f:(function
    | Server_tool tool when String.equal !tool.id id -> Some tool
    | Text _ | Thinking _ | Tool_call _ | Server_tool _ -> None)
;;

let apply t (event : Assistant_event.t) =
  match event, t.blocks with
  | Text_delta s, Text b :: _ -> Buffer.add_string b s
  | Text_delta s, _ ->
    let b = Buffer.create 256 in
    Buffer.add_string b s;
    t.blocks <- Text b :: t.blocks
  | Thinking_delta s, Thinking b :: _ -> Buffer.add_string b.text s
  | Thinking_delta s, _ ->
    let b = Buffer.create 256 in
    Buffer.add_string b s;
    t.blocks <- Thinking { text = b; signature = None } :: t.blocks
  | Thinking_signature s, Thinking b :: _ -> b.signature <- Some s
  | Thinking_signature s, _ ->
    t.blocks
    <- Thinking { text = Buffer.create 0; signature = Some s } :: t.blocks
  | Tool_call_start { index; id; name }, _ ->
    t.blocks
    <- Tool_call { index; id; name; arguments = Buffer.create 64 } :: t.blocks
  | Tool_call_delta { index; arguments }, _ ->
    (match
       List.find t.blocks ~f:(function
         | Tool_call c -> c.index = index
         | Text _ | Thinking _ | Server_tool _ -> false)
     with
     | Some (Tool_call c) -> Buffer.add_string c.arguments arguments
     | Some (Text _ | Thinking _ | Server_tool _) | None -> ())
  | Server_tool_use { id; name; input }, _ ->
    (match server_tool t id with
     | Some tool -> tool := { !tool with name; input }
     | None ->
       t.blocks
       <- Server_tool
            (ref
               { Content.Server_tool.id
               ; name
               ; input
               ; output = None
               ; is_error = false
               ; raw = None
               })
          :: t.blocks)
  | Server_tool_result { id; output; is_error; raw }, _ ->
    (match server_tool t id with
     | Some tool -> tool := { !tool with output = Some output; is_error; raw }
     | None ->
       t.blocks
       <- Server_tool
            (ref
               { Content.Server_tool.id
               ; name = ""
               ; input = "{}"
               ; output = Some output
               ; is_error
               ; raw
               })
          :: t.blocks)
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
