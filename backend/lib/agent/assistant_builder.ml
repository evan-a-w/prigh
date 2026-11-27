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
      Tool_call
        { id; name; arguments = Buffer.contents arguments; extra = None }
    | Server_tool tool -> Server_tool !tool
  ;;
end

type t =
  { model : string
  ; mutable blocks : Block.t list (* reversed *)
  ; mutable count : int (* of [blocks] *)
  }

let create ~model = { model; blocks = []; count = 0 }

let push t block =
  t.blocks <- block :: t.blocks;
  t.count <- t.count + 1
;;

(* The block [f] selects, with its index. *)
let find t ~f =
  List.find_mapi t.blocks ~f:(fun i block ->
    Option.map (f block) ~f:(fun found -> found, t.count - 1 - i))
;;

let server_tool t id =
  find t ~f:(function
    | Server_tool tool when String.equal !tool.id id -> Some tool
    | Text _ | Thinking _ | Tool_call _ | Server_tool _ -> None)
;;

let apply_block t (event : Assistant_event.t) =
  let last = t.count - 1 in
  match event, t.blocks with
  | Text_delta s, Text b :: _ ->
    Buffer.add_string b s;
    last
  | Text_delta s, _ ->
    let b = Buffer.create 256 in
    Buffer.add_string b s;
    push t (Text b);
    last + 1
  | Thinking_delta s, Thinking ({ signature = None; _ } as b) :: _ ->
    Buffer.add_string b.text s;
    last
  | Thinking_delta s, _ ->
    let b = Buffer.create 256 in
    Buffer.add_string b s;
    push t (Thinking { text = b; signature = None });
    last + 1
  | Thinking_signature s, Thinking ({ signature = None; _ } as b) :: _ ->
    b.signature <- Some s;
    last
  | Thinking_signature s, _ ->
    push t (Thinking { text = Buffer.create 0; signature = Some s });
    last + 1
  | Tool_call_start { index; id; name }, _ ->
    push t (Tool_call { index; id; name; arguments = Buffer.create 64 });
    last + 1
  | Tool_call_delta { index; arguments }, _ ->
    (match
       find t ~f:(function
         | Tool_call c when c.index = index -> Some c.arguments
         | Text _ | Thinking _ | Tool_call _ | Server_tool _ -> None)
     with
     | Some (b, i) ->
       Buffer.add_string b arguments;
       i
     | None -> last)
  | Server_tool_use { id; name; input }, _ ->
    (match server_tool t id with
     | Some (tool, i) ->
       tool := { !tool with name; input };
       i
     | None ->
       push
         t
         (Server_tool
            (ref
               { Content.Server_tool.id
               ; name
               ; input
               ; output = None
               ; is_error = false
               ; raw = None
               }));
       last + 1)
  | Server_tool_result { id; output; is_error; raw }, _ ->
    (match server_tool t id with
     | Some (tool, i) ->
       tool := { !tool with output = Some output; is_error; raw };
       i
     | None ->
       push
         t
         (Server_tool
            (ref
               { Content.Server_tool.id
               ; name = ""
               ; input = "{}"
               ; output = Some output
               ; is_error
               ; raw
               }));
       last + 1)
;;

let apply t event = ignore (apply_block t event : int)
let content t = List.rev_map t.blocks ~f:Block.to_content

let snapshot t =
  { Message.Assistant.content = content t
  ; stop_reason = End_turn
  ; usage = Usage.zero
  ; model = t.model
  ; account = None
  }
;;

let finish t ~stop_reason ~usage =
  { Message.Assistant.content = content t
  ; stop_reason
  ; usage
  ; model = t.model
  ; account = None
  }
;;
