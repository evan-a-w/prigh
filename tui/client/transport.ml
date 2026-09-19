open! Core
open! Async

type t =
  { send_line : string -> unit
  ; lines : string Pipe.Reader.t
  ; stderr_lines : string Pipe.Reader.t
  ; close : unit -> unit
  ; closed : unit Deferred.t
  }

module In_memory = struct
  module Backend = struct
    type t =
      { requests : string Pipe.Reader.t
      ; to_client : string Pipe.Writer.t
      }

    let requests t = t.requests
    let send t line = Pipe.write_without_pushback_if_open t.to_client line
    let close t = Pipe.close t.to_client
  end

  let create () =
    let requests_r, requests_w = Pipe.create () in
    let lines_r, lines_w = Pipe.create () in
    let stderr_r, _stderr_w = Pipe.create () in
    let t =
      { send_line =
          (fun line -> Pipe.write_without_pushback_if_open requests_w line)
      ; lines = lines_r
      ; stderr_lines = stderr_r
      ; close =
          (fun () ->
            Pipe.close requests_w;
            Pipe.close lines_w)
      ; closed = Pipe.closed lines_w
      }
    in
    t, { Backend.requests = requests_r; to_client = lines_w }
  ;;
end
