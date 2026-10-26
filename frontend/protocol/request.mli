open! Core

type t =
  { id : int
  ; method_ : string
  ; params : (string * Json.t) list
  }
[@@deriving sexp_of]

(** Typed constructors for methods whose parameters are worth spelling out. *)
module Method : sig
  type t =
    | List_skills
    | List_mcp of { reconnect : bool } (** restart failed/dead servers *)
    | Mcp_approve of
        { source : string (** the [.mcp.json] that defines the server *)
        ; server : string
        }
    | Set_archived of
        { path : string (** a session's path or id *)
        ; archived : bool
        }
    | Delete_sessions of string list (** paths; see {!Delete_result} *)
    | Delete_archived (** every session shown as archived *)
    | Get_messages_page of
        { session : string (** a path or id; need not be the client's *)
        ; before : string option
          (** {!Message_page.before}; [None]: the newest page *)
        ; from : string option
          (** an entry id ({!Message_page.before} of a page): the messages
              from that one to the newest, whatever [limit] *)
        ; limit : int
        } (** see {!Message_page} *)
    | Get_outline of { session : string (** a path or id *) }
    (** see {!Outline} *)
    | Get_tool_output of
        { call_id : string
        ; agent_id : string option (** in this subagent's transcript *)
        } (** a whole output ([{text, is_error}]) that was {!Elided} *)
    | Change_dir of
        { target : Dir_change.Target.t
        ; path : string
        ; if_missing : Dir_change.If_missing.t
        } (** [set_cwd] or [set_active_host]; see {!Dir_change.Outcome} *)
    | Forge_status (** see {!Forge_account.list_of_json} *)
    | Forge_login of
        { host : string
        ; kind : string option (** [github] or [forgejo]; [None]: guess *)
        ; method_ : string option
          (** [oauth] or [token]; [None]: OAuth when available *)
        } (** a login flow: [auth] events, ending in [Forge_done] or [Failed] *)
    | Forge_logout of string (** the host; [Forge_logged_out] follows *)
    | Pr_list of
        { state : string option (** [open] (default), [closed], [all] *) }
    (** see {!Pr_list} *)
    | Pr_view of int (** see {!Pr_detail} *)
    | Pr_defaults (** see {!Pr_defaults} *)
    | Pr_create of
        { title : string
        ; body : string
        ; base : string option (** default: the repository's default branch *)
        ; head : string option (** default: the current branch *)
        ; draft : bool
        ; push : bool (** push the branch to the remote first *)
        } (** answers {!Pr_detail} without checks or diff *)
    | Pr_checkout of int (** see {!Pr_checkout} *)
    | List_worktrees of
        { cwd : string option
          (** in its repository (default: the session cwd's) *)
        } (** see {!Worktree.Listing} *)
    | Create_worktree of
        { branch : string
        ; base : string option
          (** a new branch from it; [None]: an existing branch, the one
              remote branch of that name, or a new one from [HEAD] *)
        ; cwd : string option
          (** in its repository (default: the session cwd's) *)
        } (** see {!Worktree.Created} *)
    | Remove_worktree of
        { path : string
        ; force : bool (** also with uncommitted changes, or locked *)
        ; cwd : string option
          (** in its repository (default: the session cwd's) *)
        } (** see {!Worktree.removed_of_json} *)
    | Get_review (** {!Review} *)
    | Review_comment of
        { root : string
        ; path : string (** relative to [root] *)
        ; range : Review_comment.Range.t
        ; anchor : string (** the lines' text *)
        ; text : string
        } (** a new comment: the {!Review_comment} *)
    | Review_reply of
        { id : string
        ; text : string
        }
    | Review_set_status of
        { id : string
        ; status : Review_comment.Status.t
        }
    | Review_delete of { id : string }
    | Review_submit of { root : string option }
    (** [{count}]: the comments sent to the agent (with [root]: those on
        that worktree) *)
    | Review_decide of
        { id : string
        ; post : int (** in the thread, from 0 *)
        ; accept : bool
        } (** a proposal: accepted (applied) or rejected *)
    | Review_approve of
        { root : string option (** the worktree, as [git_diff] says *)
        ; path : string
        ; hash : string
        } (** the {!Review} *)
    | Review_unapprove of
        { root : string option
        ; path : string
        }
    | Fs_preview of { path : string } (** {!File_preview} *)
    | Fs_write of
        { path : string
        ; text : string
        ; expected : string option (** the [md5] it was read with *)
        } (** [{path, md5}] *)
  [@@deriving sexp_of, equal]

  val name : t -> string
  val params : t -> (string * Json.t) list
end

val create : id:int -> Method.t -> t
val to_json : t -> Json.t
val to_line : t -> string
