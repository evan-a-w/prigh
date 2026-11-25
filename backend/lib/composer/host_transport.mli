open! Core
open! Import

(** An external harness's process on a tool host ({!Host_stream}, the
    [$process] host tool) as a {!Line_transport}. With [launch.mcp] it also
    gets the MCP side channel, served (once [Line_transport.Mcp.serve] gave
    it the tools) a call at a time in fibers of their own, as a tool may
    wait for the user.

    Waits until the process started: fails, saying what to do, when the
    program is not installed there, the host's prigh is too old to run it
    or the host went away. [host]: the host's name, for messages.

    [launch.secret] goes on a descriptor; a host that did not hand it so (its
    prigh predates that) has the program started again with it in the
    environment, and a {!Line_transport.notice} saying so. *)
val spawn
  :  sw:Switch.t
  -> exec:Host_stream.Exec.t
  -> host:string
  -> ?on_stderr:(string -> unit)
  -> Adapter.Launch.t
  -> Line_transport.t Or_error.t
