open! Core
open! Import

module Tool_kind = struct
  type t =
    | Command
    | File_change
    | Read
    | Search
    | Web_search
    | Mcp of
        { server : string
        ; tool : string
        }
    | Todo
    | Subagent
    | Other of string
  [@@deriving sexp_of, equal]
end

module Change = struct
  module Kind = struct
    type t =
      | Add
      | Delete
      | Update of { move_to : string option }
    [@@deriving sexp_of, equal]
  end

  type t =
    { path : string
    ; kind : Kind.t
    ; diff : string
    }
  [@@deriving sexp_of, equal]
end

module Error_kind = struct
  type t =
    | Usage_limit of { resets_at : float option  }
    | Auth_required of { hint : string  }
    | Not_installed of { hint : string }
    | Crashed of { stderr_tail : string }
    | Protocol
    | Other
  [@@deriving sexp_of, equal]
end

module Stop = struct
  type t =
    | Done
    | Interrupted
    | Failed of
        { kind : Error_kind.t
        ; message : string
        }
  [@@deriving sexp_of, equal]
end

module Approval_reply = struct
  type t =
    | Allow
    | Allow_always
    | Deny of string option
    | Cancel
  [@@deriving sexp_of, equal]
end

module Usage_report = struct
  type t =
    { usage : Usage.t
    ; cost_usd : float option
    ; context_tokens : int option
    ; context_window : int option
    }
  [@@deriving sexp_of, equal]
end

type t =
  | Thread_started of
      { thread_id : string
      ; model : string option
      }
  | Turn_started of { turn_id : string }
  | Text_delta of
      { item : string
      ; text : string
      }
  | Text_done of
      { item : string
      ; text : string
      }
  | Thinking_delta of
      { item : string
      ; text : string
      }
  | Thinking_done of
      { item : string
      ; text : string
      }
  | Tool_start of
      { id : string
      ; kind : Tool_kind.t
      ; native_name : string
      ; title : string
      ; input : Json.t
      ; parent : string option
      }
  | Tool_update of
      { id : string
      ; output : string
      }
  | Tool_end of
      { id : string
      ; ok : bool
      ; output : string
      ; changes : Change.t list
      ; images : Image.t list
      }
  | Approval_request of
      { request_id : string
      ; tool_id : string option
      ; kind : Tool_kind.t
      ; summary : string
      ; input : Json.t
      ; offers_always : bool
      }
  | User_question of
      { request_id : string
      ; questions : User_question.t list
      }
  | Request_withdrawn of { request_id : string }

  | Plan of Plan.t
  | Turn_diff of { unified : string }
  | Usage of Usage_report.t
  | Compacted of
      { tokens_before : int option
      ; tokens_after : int option
      }
  | Retrying of
      { attempt : int option
      ; message : string
      }
  | Notice of string
  | Turn_end of
      { turn_id : string option
      ; stop : Stop.t
      }
[@@deriving sexp_of]
