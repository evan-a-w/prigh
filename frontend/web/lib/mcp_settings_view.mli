open! Core
open! Import

(** Settings → MCP servers: the servers in the user's settings (each with how
    it is doing on the session's host, and its switch, Edit and Remove), the
    form adding or editing one, the pasted [mcpServers] JSON, and the host's
    own servers from its files. *)
val groups
  :  App.Model.t
  -> inject:(App.Action.t -> unit Vdom.Effect.t)
  -> Vdom.Node.t list
