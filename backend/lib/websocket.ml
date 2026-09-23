open! Core
open! Import

let guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

let accept_key key =
  Base64.encode_exn
    (Digestif.SHA1.to_raw_string (Digestif.SHA1.digest_string (key ^ guid)))
;;

module Opcode = struct
  type t =
    | Continuation
    | Text
    | Binary
    | Close
    | Ping
    | Pong
  [@@deriving sexp_of, equal]

  let of_int = function
    | 0 -> Some Continuation
    | 1 -> Some Text
    | 2 -> Some Binary
    | 8 -> Some Close
    | 9 -> Some Ping
    | 10 -> Some Pong
    | _ -> None
  ;;

  let to_int = function
    | Continuation -> 0
    | Text -> 1
    | Binary -> 2
    | Close -> 8
    | Ping -> 9
    | Pong -> 10
  ;;
end

module Frame = struct
  type t =
    { fin : bool
    ; opcode : Opcode.t
    ; payload : string
    }
  [@@deriving sexp_of, equal]
end

let apply_mask ~mask payload =
  String.mapi payload ~f:(fun i c ->
    Char.of_int_exn (Char.to_int c lxor Char.to_int mask.[i land 3]))
;;

let encode ?mask (frame : Frame.t) =
  let buf = Buffer.create (String.length frame.payload + 14) in
  Buffer.add_char
    buf
    (Char.of_int_exn
       ((if frame.fin then 0x80 else 0) lor Opcode.to_int frame.opcode));
  let len = String.length frame.payload in
  let mask_bit = if Option.is_some mask then 0x80 else 0 in
  if len < 126
  then Buffer.add_char buf (Char.of_int_exn (mask_bit lor len))
  else if len < 65536
  then (
    Buffer.add_char buf (Char.of_int_exn (mask_bit lor 126));
    Buffer.add_char buf (Char.of_int_exn (len lsr 8));
    Buffer.add_char buf (Char.of_int_exn (len land 0xff)))
  else (
    Buffer.add_char buf (Char.of_int_exn (mask_bit lor 127));
    for shift = 7 downto 0 do
      Buffer.add_char buf (Char.of_int_exn ((len lsr (shift * 8)) land 0xff))
    done);
  (match mask with
   | None -> Buffer.add_string buf frame.payload
   | Some mask ->
     Buffer.add_string buf mask;
     Buffer.add_string buf (apply_mask ~mask frame.payload));
  Buffer.contents buf
;;

exception Protocol_error of string

let max_payload = 64 * 1024 * 1024

let read_frame reader : Frame.t =
  let b0 = Char.to_int (Eio.Buf_read.any_char reader) in
  let b1 = Char.to_int (Eio.Buf_read.any_char reader) in
  let fin = b0 land 0x80 <> 0 in
  let opcode =
    match Opcode.of_int (b0 land 0x0f) with
    | Some op -> op
    | None ->
      raise (Protocol_error (sprintf "unknown opcode %d" (b0 land 0x0f)))
  in
  let masked = b1 land 0x80 <> 0 in
  let len =
    match b1 land 0x7f with
    | 126 -> Eio.Buf_read.BE.uint16 reader
    | 127 ->
      let n = Eio.Buf_read.BE.uint64 reader in
      if Int64.(n < 0L || n > of_int max_payload)
      then raise (Protocol_error "frame too large")
      else Int64.to_int_exn n
    | n -> n
  in
  if len > max_payload then raise (Protocol_error "frame too large");
  let mask = if masked then Some (Eio.Buf_read.take 4 reader) else None in
  let payload = Eio.Buf_read.take len reader in
  let payload =
    match mask with
    | None -> payload
    | Some mask -> apply_mask ~mask payload
  in
  { fin; opcode; payload }
;;

module Message = struct
  type t =
    | Text of string
    | Binary of string
    | Close of int option
    | Ping of string
    | Pong of string
  [@@deriving sexp_of, equal]
end

let close_code payload =
  if String.length payload >= 2
  then Some ((Char.to_int payload.[0] lsl 8) lor Char.to_int payload.[1])
  else None
;;

module Message_reader = struct
  type t =
    { reader : Eio.Buf_read.t
    ; mutable partial : (Opcode.t * Buffer.t) option
    }

  let create reader = { reader; partial = None }

  let data opcode payload : Message.t =
    match opcode with
    | Opcode.Text -> Text payload
    | _ -> Binary payload
  ;;

  (* Reassembles fragmented data messages; control frames may interleave. *)
  let step t : Message.t =
    let frame = read_frame t.reader in
    match frame.opcode, t.partial with
    | Close, _ -> Close (close_code frame.payload)
    | Ping, _ -> Ping frame.payload
    | Pong, _ -> Pong frame.payload
    | (Text | Binary), Some _ ->
      raise (Protocol_error "data frame while a fragmented message is pending")
    | Continuation, None ->
      raise (Protocol_error "continuation frame without a start")
    | (Text | Binary), None ->
      if frame.fin
      then data frame.opcode frame.payload
      else (
        let buf = Buffer.create (String.length frame.payload * 2) in
        Buffer.add_string buf frame.payload;
        t.partial <- Some (frame.opcode, buf);
        raise_notrace Exit)
    | Continuation, Some (opcode, buf) ->
      Buffer.add_string buf frame.payload;
      if Buffer.length buf > max_payload
      then raise (Protocol_error "message too large");
      if frame.fin
      then (
        t.partial <- None;
        data opcode (Buffer.contents buf))
      else raise_notrace Exit
  ;;

  let rec next t =
    match step t with
    | message -> message
    | exception Exit -> next t
  ;;
end

let close_payload code =
  String.of_char_list
    [ Char.of_int_exn ((code lsr 8) land 0xff)
    ; Char.of_int_exn (code land 0xff)
    ]
;;

type t =
  { reader : Message_reader.t
  ; flow : Eio.Flow.sink_ty Eio.Resource.t
  ; write_mutex : Eio.Mutex.t
  ; mutable closed : bool
  ; mask : unit -> string option
  }

let write t frame =
  Eio.Mutex.use_rw ~protect:true t.write_mutex (fun () ->
    if not t.closed
    then (
      match Eio.Flow.copy_string (encode ?mask:(t.mask ()) frame) t.flow with
      | () -> ()
      | exception _ -> t.closed <- true))
;;

let create ?(role = `Server) ~reader ~flow () =
  let mask =
    match role with
    | `Server -> fun () -> None
    | `Client ->
      fun () ->
        Some (String.init 4 ~f:(fun _ -> Char.of_int_exn (Random.int 256)))
  in
  { reader = Message_reader.create reader
  ; flow :> Eio.Flow.sink_ty Eio.Resource.t
  ; write_mutex = Eio.Mutex.create ()
  ; closed = false
  ; mask
  }
;;

let send_text t text = write t { fin = true; opcode = Text; payload = text }

let close ?(code = 1000) t =
  if not t.closed
  then (
    write t { fin = true; opcode = Close; payload = close_payload code };
    t.closed <- true)
;;

let rec read_text t =
  if t.closed
  then None
  else (
    match (Message_reader.next t.reader : Message.t) with
    | exception (End_of_file | Eio.Io _ | Eio.Buf_read.Buffer_limit_exceeded) ->
      t.closed <- true;
      None
    | exception Protocol_error _ ->
      close ~code:1002 t;
      None
    | Text text -> Some text
    | Binary _ -> read_text t
    | Ping payload ->
      write t { fin = true; opcode = Pong; payload };
      read_text t
    | Pong _ -> read_text t
    | Close code ->
      close ?code t;
      None)
;;

let is_closed t = t.closed
