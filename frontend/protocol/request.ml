open! Core

type t =
  { id : int
  ; method_ : string
  ; params : (string * Json.t) list
  }
[@@deriving sexp_of]

module Method = struct
  type t =
    | List_skills
    | List_mcp of { reconnect : bool }
    | Mcp_approve of
        { source : string
        ; server : string
        }
    | Set_archived of
        { path : string
        ; archived : bool
        }
    | Delete_sessions of string list
    | Delete_archived
    | Get_messages_page of
        { session : string
        ; before : string option
        ; from : string option
        ; limit : int
        }
    | Get_outline of { session : string }
    | Get_tool_output of
        { call_id : string
        ; agent_id : string option
        }
    | Change_dir of
        { target : Dir_change.Target.t
        ; path : string
        ; if_missing : Dir_change.If_missing.t
        }
    | Enter_worktree of string
    | Worktree_back of { main : bool }
    | Forge_status
    | Forge_login of
        { host : string
        ; kind : string option
        ; method_ : string option
        }
    | Forge_logout of string
    | Pr_list of { state : string option }
    | Pr_view of int
    | Pr_defaults
    | Pr_create of
        { title : string
        ; body : string
        ; base : string option
        ; head : string option
        ; draft : bool
        ; push : bool
        }
    | Pr_checkout of int
    | List_worktrees of { cwd : string option }
    | Create_worktree of
        { branch : string
        ; base : string option
        ; cwd : string option
        }
    | Remove_worktree of
        { path : string
        ; force : bool
        ; cwd : string option
        }
    | Get_review
    | Review_comment of
        { root : string
        ; path : string
        ; range : Review_comment.Range.t
        ; anchor : string
        ; text : string
        }
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
    | Review_decide of
        { id : string
        ; post : int
        ; accept : bool
        }
    | Review_approve of
        { root : string option
        ; path : string
        ; hash : string
        }
    | Review_unapprove of
        { root : string option
        ; path : string
        }
    | Review_since of
        { root : string
        ; path : string option
        ; index : int option
        }
    | Fs_preview of { path : string }
    | Git_show of
        { dir : string option
        ; rev : string
        ; path : string
        }
    | Fs_write of
        { path : string
        ; text : string
        ; expected : string option
        ; review_root : string option
        }
    | Nix_store_status
    | Nix_store_gc of { delete_generations : Nix_generations.t option }
  [@@deriving sexp_of, equal]

  let name = function
    | List_skills -> "list_skills"
    | List_mcp _ -> "list_mcp"
    | Mcp_approve _ -> "mcp_approve"
    | Set_archived _ -> "set_archived"
    | Delete_sessions _ -> "delete_sessions"
    | Delete_archived -> "delete_archived"
    | Get_messages_page _ -> "get_messages_page"
    | Get_outline _ -> "get_outline"
    | Get_tool_output _ -> "get_tool_output"
    | Change_dir { target = Cwd; _ } -> "set_cwd"
    | Change_dir { target = Host _; _ } -> "set_active_host"
    | Enter_worktree _ -> "set_cwd"
    | Worktree_back _ -> "worktree_back"
    | Forge_status -> "forge_status"
    | Forge_login _ -> "forge_login"
    | Forge_logout _ -> "forge_logout"
    | Pr_list _ -> "pr_list"
    | Pr_view _ -> "pr_view"
    | Pr_defaults -> "pr_defaults"
    | Pr_create _ -> "pr_create"
    | Pr_checkout _ -> "pr_checkout"
    | List_worktrees _ -> "list_worktrees"
    | Create_worktree _ -> "create_worktree"
    | Remove_worktree _ -> "remove_worktree"
    | Get_review -> "get_review"
    | Review_comment _ -> "review_comment"
    | Review_reply _ -> "review_reply"
    | Review_set_status _ -> "review_set_status"
    | Review_delete _ -> "review_delete"
    | Review_submit _ -> "review_submit"
    | Review_decide _ -> "review_decide"
    | Review_approve _ -> "review_approve"
    | Review_unapprove _ -> "review_unapprove"
    | Review_since _ -> "review_since"
    | Fs_preview _ -> "fs_preview"
    | Git_show _ -> "git_show"
    | Fs_write _ -> "fs_write"
    | Nix_store_status -> "nix_store_status"
    | Nix_store_gc _ -> "nix_store_gc"
  ;;

  let cwd_param = function
    | None -> []
    | Some cwd -> [ "cwd", Json.str cwd ]
  ;;

  let root_param = function
    | None -> []
    | Some root -> [ "root", Json.str root ]
  ;;

  let params = function
    | List_skills
    | List_mcp { reconnect = false }
    | Delete_archived
    | Forge_status
    | Pr_defaults
    | Get_review
    | Nix_store_status -> []
    | Nix_store_gc { delete_generations = None } -> []
    | Nix_store_gc { delete_generations = Some _ as generations } ->
      [ "delete_generations", Nix_generations.to_json generations ]
    | Review_submit { root = None } -> []
    | Review_submit { root = Some root } -> [ "root", Json.str root ]
    | Review_comment { root; path; range; anchor; text } ->
      [ "root", Json.str root
      ; "path", Json.str path
      ; "first", Json.int range.first
      ; "last", Json.int range.last
      ]
      @ Review_comment.Range.params range
      @ [ "anchor", Json.str anchor; "text", Json.str text ]
    | Review_reply { id; text } -> [ "id", Json.str id; "text", Json.str text ]
    | Review_set_status { id; status } ->
      [ "id", Json.str id
      ; "status", Json.str (Review_comment.Status.to_string status)
      ]
    | Review_delete { id } -> [ "id", Json.str id ]
    | Review_decide { id; post; accept } ->
      [ "id", Json.str id; "post", Json.int post; "accept", Json.bool accept ]
    | Review_approve { root; path; hash } ->
      root_param root @ [ "path", Json.str path; "hash", Json.str hash ]
    | Review_unapprove { root; path } ->
      root_param root @ [ "path", Json.str path ]
    | Fs_preview { path } -> [ "path", Json.str path ]
    | Review_since { root; path; index } ->
      [ "root", Json.str root ]
      @ Option.value_map path ~default:[] ~f:(fun p -> [ "path", Json.str p ])
      @ Option.value_map index ~default:[] ~f:(fun i -> [ "index", Json.int i ])
    | Git_show { dir; rev; path } ->
      Option.value_map dir ~default:[] ~f:(fun d -> [ "dir", Json.str d ])
      @ Git_diff.show_params ~rev ~path
    | Fs_write { path; text; expected; review_root } ->
      [ "path", Json.str path; "text", Json.str text ]
      @ Option.value_map expected ~default:[] ~f:(fun e ->
        [ "expected", Json.str e ])
      @ Option.value_map review_root ~default:[] ~f:(fun r ->
        [ "review_root", Json.str r ])
    | List_worktrees { cwd = None } -> []
    | List_worktrees { cwd = Some cwd } -> [ "cwd", Json.str cwd ]
    | List_mcp { reconnect = true } -> [ "reconnect", Json.bool true ]
    | Mcp_approve { source; server } ->
      [ "source", Json.str source; "server", Json.str server ]
    | Set_archived { path; archived } ->
      [ "path", Json.str path; "archived", Json.bool archived ]
    | Delete_sessions paths -> [ "paths", `Array (List.map paths ~f:Json.str) ]
    | Get_messages_page { session; before; from; limit } ->
      [ "session", Json.str session ]
      @ Option.value_map before ~default:[] ~f:(fun b ->
        [ "before", Json.str b ])
      @ Option.value_map from ~default:[] ~f:(fun f -> [ "from", Json.str f ])
      @ [ "limit", Json.int limit ]
    | Get_outline { session } -> [ "session", Json.str session ]
    | Get_tool_output { call_id; agent_id } ->
      [ "call_id", Json.str call_id ]
      @ Option.value_map agent_id ~default:[] ~f:(fun a ->
        [ "agent_id", Json.str a ])
    | Change_dir { target; path; if_missing } ->
      let if_missing =
        match if_missing with
        | Fail -> []
        | Ask | Create ->
          [ "if_missing", Dir_change.If_missing.to_json if_missing ]
      in
      (match target with
       | Cwd -> [ "path", Json.str path ]
       | Host { id; name = _ } -> [ "host", Json.str id; "cwd", Json.str path ])
      @ if_missing
    | Enter_worktree path ->
      [ "path", Json.str path; "worktree", Json.bool true ]
    | Worktree_back { main } -> if main then [ "main", Json.bool true ] else []
    | Forge_login { host; kind; method_ } ->
      [ "host", Json.str host ]
      @ Option.value_map kind ~default:[] ~f:(fun k -> [ "kind", Json.str k ])
      @ Option.value_map method_ ~default:[] ~f:(fun m ->
        [ "method", Json.str m ])
    | Forge_logout host -> [ "host", Json.str host ]
    | Pr_list { state } ->
      Option.value_map state ~default:[] ~f:(fun s -> [ "state", Json.str s ])
    | Pr_view number | Pr_checkout number -> [ "number", Json.int number ]
    | Pr_create { title; body; base; head; draft; push } ->
      [ "title", Json.str title; "body", Json.str body ]
      @ Option.value_map base ~default:[] ~f:(fun b -> [ "base", Json.str b ])
      @ Option.value_map head ~default:[] ~f:(fun h -> [ "head", Json.str h ])
      @ [ "draft", Json.bool draft; "push", Json.bool push ]
    | Create_worktree { branch; base; cwd } ->
      [ "branch", Json.str branch ]
      @ Option.value_map base ~default:[] ~f:(fun b -> [ "base", Json.str b ])
      @ cwd_param cwd
    | Remove_worktree { path; force; cwd } ->
      [ "path", Json.str path ]
      @ (if force then [ "force", Json.bool true ] else [])
      @ cwd_param cwd
  ;;
end

let create ~id method_ =
  { id; method_ = Method.name method_; params = Method.params method_ }
;;

let to_json { id; method_; params } =
  Json.obj
    [ "id", Json.int id; "method", Json.str method_; "params", Json.obj params ]
;;

let to_line t = Json.to_string (to_json t)
