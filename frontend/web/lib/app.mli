open! Core
open! Import

(** prigh-web as a pure state machine: [update] performs no I/O, it returns
    [Command.t]s for the page to execute, whose results come back as
    [Action.Reply]. *)

module Auth_purpose : sig
  (** Why we asked for [auth_status]. *)
  type t =
    | Refresh
    | Login_picker
    | Logout_picker
    | Show
  [@@deriving sexp_of, equal]
end

module Entries_purpose : sig
  (** Why we asked for [get_entries]. *)
  type t =
    | Fork
    | Rewind
    | Tree
  [@@deriving sexp_of, equal]
end

module Users_purpose : sig
  type t =
    | Probe (** at startup: only superusers may list them *)
    | Picker
  [@@deriving sexp_of, equal]
end

module Skills_purpose : sig
  (** Why we asked for [list_skills]. *)
  type t =
    | Complete (** [/skill:] in the editor *)
    | Picker of string (** [/skills], filtered by this *)
  [@@deriving sexp_of, equal]
end

module Mcp_purpose : sig
  (** Why we asked for [list_mcp] or [mcp_approve]. *)
  type t =
    | Picker (** [/mcp] *)
    | Reconnect (** [/mcp reconnect]: report it *)
    | Refreshed of string
    (** a server ([Mcp_servers.id]) approved or restarted from the picker *)
  [@@deriving sexp_of, equal]
end

module Forge_reply : sig
  (** Git hosting's replies. [session]s are ids: replies for another
      session are ignored. *)
  type t =
    | Status (** [forge_status] *)
    | Logout of string (** the host *)
    | List of
        { session : string
        ; filter : Pull_requests.Filter.t
        } (** [pr_list], for the [/pr] dialog *)
    | View of
        { session : string
        ; number : int
        }
    | Defaults of string (** [pr_defaults], for the create dialog *)
    | Created (** [pr_create]: said whatever is on show by then *)
    | Checkout of
        { session : string
        ; number : int
        }
  [@@deriving sexp_of, equal]
end

module Worktrees_purpose : sig
  (** Why we asked for [list_worktrees]. *)
  type t =
    | Sidebar of int (** the [Worktrees] request's number *)
    | Picker (** [/worktree] *)
    | Review_pick (** the review page's picker of what to review *)
    | Target of
        { command : string (** as typed: back to the editor if it fails *)
        ; action : Prigh_ui.Worktree_command.Action.t
        } (** [/worktree open] or [rm]: the worktree it names *)
    | Review_target of
        { command : string
        ; name : string
        ; base : string option
        ; direct : bool
        ; model : string option
        } (** [/review NAME]: the worktree it names, if any *)
  [@@deriving sexp_of, equal]
end

module Trim_purpose : sig
  (** Why we asked for [get_trim_points]. *)
  type t =
    | Picker (** [/trim] *)
    | Before of
        { from_end : int
        (** the user's message, from the newest ({!Jump.from_end}) *)
        ; at : Time_ns.Alternate_sexp.t option
        (** when it was sent: the point found must be the same *)
        } (** its "Trim everything before this" *)
  [@@deriving sexp_of, equal]
end

module Points_purpose : sig
  (** Why we asked for [get_message_points]. *)
  type t =
    | Delete_picker (** [/delete] *)
    | Trim_after_picker (** [/trim after] *)
    | Delete of
        { entry : int (** the transcript's entry, hidden meanwhile *)
        ; index : int (** its message ({!Message_index.Span.first}) *)
        ; at : Time_ns.Alternate_sexp.t option
        (** when it was sent: the point found must be the same *)
        } (** a message's "Delete" *)
    | Trim_after of
        { index : int (** the entry's message *)
        ; last : int (** its last ({!Message_index.Span.next} - 1): the
                         trim is after it, after the entry's tool results *)
        ; at : Time_ns.Alternate_sexp.t option
        } (** a message's "Trim everything after this" *)
  [@@deriving sexp_of, equal]
end

module Reply_tag : sig
  type t =
    | Ignore
    | Show_error (** only failures are shown *)
    | State
    | Page of
        { path : string
        ; before : string option (** [None]: the newest page *)
        }
    | Tool_output of
        { agent_id : string option
        ; call_id : string
        }
    | Outline of string (** [get_outline] of this session's path *)
    | Pending
    | Sent of
        { text : string
        ; images : Image.t list
        } (** a prompt, steer or follow-up: back to the editor if it fails *)
    | Steered of
        { agent : string
        ; draft : string (** what was sent, as typed *)
        } (** [steer_agent]: back to the editor if it fails *)
    | Sessions
    | Models
    | Reload_state (** the client's session changed: fetch its state *)
    | Subagent of string
    (** [get_subagent] for this [subagent] call or agent id *)
    | Subagents (** [list_subagents], for the agents panel *)
    | Jobs (** [list_jobs] *)
    | Job_output of string (** the job's id *)
    | Job_started (** [!&command]: the job's id *)
    | Auth_status of Auth_purpose.t
    | Paths of string (** the completion prefix listed *)
    | Restored (** [abort]: queued prompts back to the editor *)
    | Dequeued (** [dequeue], or [remove_queued] to edit it *)
    | Unqueued of string (** [remove_queued]: its text *)
    | Sessions_deleted (** [delete_sessions] or [delete_archived] *)
    | Archived_set of
        { title : string
        ; archived : bool
        }
    | Refresh_sessions (** on success, list the sessions again *)
    | Login_started
    | Notice of string (** shown on success *)
    | Reminder of
        { command : string (** back into the editor if refused *)
        ; notice : string option
          (** shown on success; [None]: the reminder added, by its name *)
        }
    | Reconnect of int (** generation; stale replies are ignored *)
    | Config (** [get_config] *)
    | Config_saved of string (** the new config; the notice is shown *)
    | Fallback_saved (** the new config; its chain is shown *)
    | Default_dir_saved
    | Compaction_saved (** the new config; its settings are shown *)
    | Default_saved
    | Session_stats
    | Entries of Entries_purpose.t
    | Reload_messages (** the head moved: fetch state and messages *)
    | Exported
    | Imported
    | Dir_changed of Dir_change.Target.t
    (** [Missing] asks to create it; the prompt or dialog closes once changed *)
    (** closes the prompt dialog with this notice; failures show in it *)
    | Prompt_paths of string (** the prompt's input listed *)
    | Btw of string (** the side question's id *)
    | Users of Users_purpose.t
    | User_switched (** [set_user]'s reply, like [hello]'s *)
    | Skills of
        { place : string (** [Skills.place]: stale replies are ignored *)
        ; purpose : Skills_purpose.t
        }
    | Mcp of Mcp_purpose.t
    | Worktrees of
        { place : string (** [Worktrees.place] *)
        ; purpose : Worktrees_purpose.t
        }
    | Worktree_created of { command : string (** as typed *) }
    | Review_checked_out of Code_review.Target.t
    (** [create_worktree] of the branch the review page shows read-only:
        then the page reviews the new worktree, compared the same way *)
    (** [create_worktree]: the session moves there *)
    | Worktree_entered of string
    (** [set_cwd] into a worktree: the notice shown once there *)
    | Worktree_went_back of
        { command : string (** as typed *)
        ; home : Session_home.t option
        } (** [worktree_back], from that home *)
    | Worktree_session of
        { path : string
        ; notice : string
        } (** [new_session] to work in the worktree at [path] *)
    | Worktree_removed of
        { target : string
        ; force : bool
        }
    | Repo_worktrees of string
    (** [list_worktrees] of this repository (its root), for its worktrees
        under sessions in the sidebar *)
    | Plan_switched of Context_menu.Plan.t
    (** [switch_session] for a context menu's item: the plan's next steps
        follow *)
    | Plan_entered of Context_menu.Plan.t (** [set_cwd], likewise *)
    | Mode_set of string (** [set_mode]: the new state; the notice is shown *)
    | Model_set (** [set_model]: the choice is no longer pending *)
    | Thinking_set
    | Git_diff
    | Command_prompt of string (** [/review] or [/init] as typed *)
    | Files of Files.Reply.t
    | Review of Code_review.Reply.t
    | Sent_with_files of
        { text : string
        ; images : Image.t list
        ; attachments : Files.Attachment.t list
        } (** [Sent] with attachments: back to the composer if it fails *)
    | Resync of
        { session : string (** its id *)
        ; from : string option
        }
    (** the session's messages after reconnecting, from the first one on show
        ([from], when older ones are not loaded): merged into the transcript
        on show *)
    | Resync_pending
    | Answer
    (** to a confirmation or a question: asked again when it does not
        reach the agent *)
    (** [get_pending] after reconnecting: what is no longer waited for
        closes, the rest stays as it was *)
    | Session_gone of
        { generation : int
        ; session : string
        }
    (** like [Reconnect generation], but the backend no longer has [session]
        (it restarted before saving it): we are on another one *)
    | Forge of Forge_reply.t
    | Prefs (** [get_prefs]: the user's preferences ({!Pref_sync}) *)
    | Generated of string (** [generate_text]: the draft's id *)
    | Git of Git_bar.Reply.t (** the review's git bar *)
    | Nix_status of { open_settings : bool }
    (** [nix_store_status]; [/nix-gc] opens Settings once it is in *)
    | Nix_collected (** [nix_store_gc] *)
    | Trim_points of Trim_purpose.t
    | Trimmed (** [trim]: the confirmation closes, or says why not *)
    | Untrimmed
    | Trimmed_messages of string (** [get_trimmed] of this session's path *)
    | Message_points of Points_purpose.t
    | Deleted of { entry : int option }
    (** [delete]: the entry hidden meanwhile, shown again if it fails *)
    | Trimmed_after (** [trim_after]: the confirmation closes, or says why not *)
    | Range_restored of string (** [restore] of this {!Hidden.entry} *)
    | Hidden_messages of
        { path : string
        ; entry : string
        } (** [get_hidden] of this session's range ({!Hidden.entry}) *)
  [@@deriving sexp_of, equal]
end

module Command : sig
  type t =
    | Rpc of
        { method_ : string
        ; params : (string * Json.t) list
        ; tag : Reply_tag.t
        }
    | Reconnect of
        { generation : int
        ; delay_ms : int
        ; session : string option
        }
    (** After [delay_ms], connect again and send [hello] (rejoining [session]);
        answered by [Reply (Reconnect generation, hello reply)], or by
        [Reply (Session_gone _, hello reply)]. Without a [session], the one
        in the page's URL. *)
    | Set_url_session of string
    (** the page's [?session=], so a reload rejoins *)
    | Expire_toast of
        { id : int
        ; after_ms : int
        } (** [Dismiss_toast id] after [after_ms] *)
    | Focus of string (** the element with this id, once rendered *)
    | Clear_text_selection (** the page's selected text, unselected *)
    | Place_popover
    (** a chip's menu, once rendered: above the composer, or below its chip
        when only there is room; inside the composer sideways *)
    | Place_menu of Context_menu.Anchor.t
    (** the context menu, once rendered: at the anchor, kept in the
        window *)
    | Open_tab of string (** a session (its id) in a new browser tab *)
    | Set_caret of int
    (** the editor's caret, at this byte offset of the draft, once rendered
        (and focused): after text was put in at the caret *)
    | Sign_out (** forget this account's saved login and reload *)
    | Reveal of string list
    (** scroll the main chat to a subagent's card, opening the transcripts
        it is in: the [subagent] call ids from the top-level one down *)
    | Copy of string (** to the clipboard *)
    | Switch_account of Accounts.Account.t (** reload signed in as it *)
    | Add_account (** the sign-in form, keeping the saved accounts *)
    | Scroll_chat of int (** by pages *)
    | Jump_to_user_message of int
    (** the previous ([-1]) or next ([1]) user message in view *)
    | Scroll_to_entry of int
    (** the main chat to this entry ([Jump.Target.Entry]), once rendered,
        marking it for a moment *)
    | Scroll_to_bottom (** and follow new output again *)
    | Scroll_to_trimmed
    (** the main chat to the divider of what is trimmed from the context,
        once rendered, marking it for a moment *)
    | Scroll_to_hidden of string
    (** the main chat to the divider of what this trim after a message
        ({!Hidden.entry}) keeps out of the context, once rendered *)
    | Scroll_to_message of int
    (** the main chat to this entry (any message, {!Chat_view.message_attribute}),
        once rendered *)
    | Focus_terminal (** the terminal panel's shell, once mounted *)
    | Notify of Prigh_ui.Notify.t (** a browser notification *)
    | Request_notification_permission
    (** answered by [Notification_permission] *)
    | Set_title of string (** the tab's, with the count of unseen events *)
    | Files of Files.Page.t
    | Cache_prefs of Prefs.Change.t list
    (** the user's preferences ({!Pref_sync}) as they are now, kept in the
        browser for the next page load (its first paint, or a backend that
        keeps none) *)
    | Apply_theme of Theme.t
    (** on the document, now (and for the next page load's first paint) *)
    | Keep_scroll_position
    (** older messages are being added above: what shows stays in place *)
    | Probe_connection
    (** the socket may have died unnoticed (a suspended page, a new network):
        drop it ([Backend_closed]) unless the backend answers soon *)
    | Drop_connection (** the network went away: [Backend_closed] follows *)
    | Reload_page of { keep_session : bool }
    (** the connection keeps dropping: load the page again, in this session
        or without one *)
    | Auth_failed of string
    (** the backend refused the saved sign-in: the sign-in page, saying
        this, keeping the session to return to *)
  [@@deriving sexp_of, equal]
end

module Connection : sig
  type t =
    | Connected
    | Reconnecting of
        { attempt : int
        ; generation : int
        }
  [@@deriving sexp_of, equal]

  (** 250ms doubling, capped at 10s. *)
  val delay_ms : attempt:int -> int
end

module Toast = Toast
module Drops = Drops

module Action : sig
  type t =
    | Start (** the initial requests, after [hello] *)
    | Hello of Hello_reply.t
    | Saved_login (** we signed in with a saved user name or token *)
    | Event of Event.t
    | Protocol_error of string
    | Backend_closed
    | Reply of Reply_tag.t * (Json.t, string) Result.t
    | Tick of Time_ns.t (** the clock, for ages; also refreshes the sessions *)
    | Set_utc_offset of Time_ns.Span.t
    (** the browser's time zone, for messages' times *)
    | Set_narrow of bool (** a phone-sized window: the sidebar is a drawer *)
    | Page_hidden of bool
    (** the tab was hidden or shown again (reconnect now, or check the socket) *)
    | Restore_prefs of
        { prefs : Prefs.Change.t list
        ; legacy : bool
        (** kept by a page from before the backend kept them: uploaded
            unless the backend has its own *)
        }
    (** at startup: the preferences the browser cached
        ([Command.Cache_prefs]), until the backend's arrive *)
    | Set_notification_permission of string
    (** at startup: [Notification.permission] *)
    | Notification_permission of string
    (** the browser's answer to [Request_notification_permission] *)
    | Set_draft of string (** the caret at the end *)
    | Edit of
        { text : string
        ; cursor : int
        }
    | Send (** prompt, or steer while running; slash commands run *)
    | Send_follow_up (** after the run, or now when idle *)
    | Abort
    | History_older
    | History_newer
    | Complete_move of int
    | Complete_accept of { run : bool }
    (** [run]: a command without arguments, or an argument, runs at once *)
    | Complete_choose of int (** clicked *)
    | Complete_close
    | New_session
    | Switch_session of string (** path *)
    | Ask_delete of string
    (** path; the marked sessions instead when it is one of them *)
    | Select_sessions of bool (** the sidebar's selection mode, on or off *)
    | Mark_session of
        { path : string
        ; range : bool (** shift-click: from the last one clicked *)
        }
    | Delete_marked
    | Ask_delete_archived
    | Set_archived of
        { path : string
        ; archived : bool
        }
    | Set_session_query of string
    | Toggle_archived (** fold or unfold the sidebar's archived sessions *)
    | Open_sessions (** the sidebar, with its search focused *)
    | Session_nav of
        { from : string option (** a session's id; [None]: the search *)
        ; by : int
        } (** the focus to another of the sidebar's sessions *)
    | Open_first_session (** the search's best match *)
    | Leave_sidebar (** back to the editor *)
    | Set_model of string (** [provider/id] *)
    | Set_thinking of string
    | Open_chip of Chip.t
    (** its menu over the composer (Ctrl+L: the model's); again closes it *)
    | Chip_step of int
    (** the next ([1]) or previous ([-1]) chip's menu, from the open one *)
    | Open_snippets of int option
    (** Alt+N: pick a snippet to insert at this caret (else the last edit's) *)
    | Open_help
    | Open_settings (** the gear, Ctrl+, *)
    | Open_rename
    | Open_subagents of string option
    (** the agents panel: its list, or the agent or job with this number
        (in the list), id or call id *)
    | Toggle_subagents
    | Select_item of Agents.Item.t (** shown in full in the panel *)
    | Agents_back (** from an agent to the list; from the list, closed *)
    | Focus_agent of int (** the [n]th listed, from 1 *)
    | Cycle_agent of int (** the next (1) or previous (-1) listed *)
    | Show_in_chat of string (** a subagent's card in the main chat *)
    | Clock of Time_ns.t
    (** every second while [Model.ticking]: elapsed times, polling jobs *)
    | Picker_query of string
    | Picker_move of int
    | Picker_accept
    | Picker_choose of string (** an item's id, clicked *)
    | Dialog_input of string (** rename, login answers *)
    | Dialog_move of int (** login select options *)
    | Dialog_accept
    | Close_dialog (** no side effects, except cancelling a login *)
    | Login_choose of int (** a login select option, clicked *)
    | Start_login of string (** provider, with its default method *)
    | Logout of string (** provider *)
    | Cancel_subagent of string (** agent id *)
    | Kill_job of string (** job id *)
    | Dequeue
    (** the last message queued for the composer's recipient back into the
        editor *)
    | Edit_queued of Recipient.t * Queued.Message.t (** back into the editor *)
    | Remove_queued of Recipient.t * Queued.Message.t
    | Set_recipient of Recipient.t
    (** the composer sends to the main agent or steers a running subagent *)
    | Remove_reminder of string (** by name, from the [/remind] picker *)
    | Toggle_folded of string
    (** fold or unfold the worktrees under a session of the sidebar (by its
        id) *)
    | New_session_in of string
    (** a new session working in this directory (a worktree) *)
    | New_worktree (** its dialog: the session moves into the worktree *)
    | New_worktree_input of
        { field : New_worktree.Field.t
        ; text : string
        }
    | Open_worktree of
        { path : string
        ; new_session : bool
        } (** the session moves there, or a new one starts there *)
    | Ask_remove_worktree of string (** its path: confirmed first *)
    | Remove_worktree of { force : bool }
    (** the confirmation's answer; [force]: with uncommitted changes *)
    | Open_menu of Context_menu.t
    (** a session's or worktree's context menu (right-click, Shift+F10, the
        context menu key, a long press, the row's ⋯) *)
    | Toggle_sidebar
    | Answer_confirm of
        { call_id : string
        ; agent_id : string option
        ; answer : Confirm_prompt.Answer.t
        } (** [Deny_with_reason] opens the reason's field *)
    | Confirm_reason_input of string
    | Send_confirm_reason (** deny with the reason typed *)
    | Cancel_confirm_reason (** back to the answers, nothing sent *)
    | Question of Question_dialog.Action.t (** the oldest [ask_user] dialog *)
    | Cycle_mode (** Alt+M: ask → auto-edit → auto → plan *)
    | Add_image of Image.t
    | Remove_image of int
    | Show_toast of
        { text : string
        ; error : bool
        }
    | Dismiss_toast of int
    | Sign_out
    | Set_accounts of
        { accounts : Accounts.Account.t list
        ; current : Accounts.Account.t option
        } (** the saved sign-ins, from the page *)
    | Open_accounts (** the account menu *)
    | Cycle_verbosity
    | Toggle_fold of string
    (** a tool output's [Fold.key], clicked: shown in full, or folded again *)
    | Cycle_model of int (** through the scoped models, forwards or back *)
    | Cycle_thinking
    | Copy_last (** the last reply, to the clipboard *)
    | Close_btw
    | Dialog_toggle (** check or uncheck the highlighted scoped model *)
    | Toggle_scoped of string (** a model key, clicked *)
    | Dialog_complete (** a prompt's highlighted completion *)
    | Choose_suggestion of int (** a prompt's completion, clicked *)
    | Retry_connection
    | Reload_page of { keep_session : bool }
    | Scroll_chat of int
    | Jump_to_user_message of int
    | Jump_to of Jump.Target.t
    (** a landmark of the transcript (the rail or the jump list), loading the
        pages down to it first when it is not loaded *)
    | Jump_older
    (** from the page: Ctrl+↑ found none of the user's messages above, the
        previous one is not loaded *)
    | Open_jump_list (** Ctrl+J: the user's messages, a picker *)
    | Ask_trim of Jump.Target.t
    (** trim the context before this message of the user's (its "Trim
        everything before this", or Delete in the jump list): confirmed
        first *)
    | Show_trimmed of bool
    (** the messages trimmed from the context, above the divider, or folded
        again *)
    | Untrim
    (** the divider's Undo: what is trimmed before a message back ([/trim
        undo] brings back the latest trim, before or after a message) *)
    | Ask_delete_message of int
    (** this entry's "Delete": its message (a reply with its tool calls'
        results) out of the transcript and the context, at once *)
    | Ask_trim_after of int
    (** this entry's "Trim everything after this": confirmed first *)
    | Restore_hidden of Hidden.t
    (** back into the context: a deletion's toast's Undo *)
    | Restore_range of string
    (** a trim after a message's divider's Undo: the range of
        [State.hidden] with this {!Hidden.entry} *)
    | Show_hidden of
        { entry : string (** {!Hidden.entry} *)
        ; show : bool
        } (** a trim after a message's divider's Show (or Hide) *)
    | Chat_scrolled of { at_bottom : bool }
    (** from the page: the user scrolled the chat away from (or back to) its
        end *)
    | Jump_to_bottom
    | Run of string (** a slash command, e.g. from a button *)
    | Toggle_terminal
    | Close_terminal
    | New_shell (** after the shell exited or failed to start *)
    | Terminal_status of
        { key : string (** the [Terminal.Target.key] it is about *)
        ; status : Terminal.Status.t
        } (** from the page's widget *)
    | Files of Files.Action.t (** the file browser and the attachments *)
    | Review of Code_review.Action.t
    (** the review page, comments and approvals *)
    | Review_pick of Review_picker.Which.t
    (** the review page's menu of what to review or compare with (again
        closes it) *)
    | Review_direct of bool
    (** compare with the base itself ([true]) or since the merge base *)
    | Review_checkout
    (** check the branch reviewed read-only out into a worktree, recorded
        on the session (as [/worktree new] does), and review it there *)
    | Review_worktree of
        { path : string
        ; base : string option
        }
    (** the review page on the worktree at [path] (any worktree of any
        repository on the tool host), against [base] (default: its own, see
        [git_diff]); the slash form is [/review PATH [--base REV]]. For the
        sidebar's and the pickers' "Review changes" *)
    | Resize of Resize.Action.t
    (** a panel's handle dragged or moved with the keys, the window resized,
        the sizes kept in the browser *)
    | Load_older (** the page of the transcript before those loaded *)
    | Prefetch_session of string
    (** a session's path: the pointer rests on it in the sidebar *)
    | Load_output of string (** a [Fold] key: the whole of an elided output *)
    | Network of bool
    (** the browser's [online] (reconnect now) and [offline] events *)
    | Pr_filter of Pull_requests.Filter.t (** the [/pr] list's state *)
    | Pr_open of int (** a pull request, from the list *)
    | Pr_checkout of int
    | New_pr (** the create dialog, from the list (Esc goes back to it) *)
    | Pr_form of Pr_create.Action.t (** typed or toggled in it *)
    | Preview_accent of string
    (** a colour, while the settings' colour input is being dragged: shown,
        not saved ([Run "/accent #hex"] saves it) *)
    | Generate of Generate.Target.t
    (** ✦ Generate (Alt+G): the model drafts the field's text *)
    | Generate_stop (** Stop, or Esc while it drafts: what came stays *)
    | Generate_undo of Generate.Target.t
    (** the field's text from before its last draft *)
    | Git of Git_bar.Action.t (** the review page's git bar *)
  [@@deriving sexp_of]
end

module Model : sig
  type t =
    { connection : Connection.t
    ; generation : int
    ; drops : Drops.t
    ; hello : Hello_reply.t option
    ; saved_login : bool (** so signing out means something *)
    ; state : State.t option
    ; chat : Chat.t
    ; sessions : Session_summary.t list
    ; models : Llm.t list
    ; auth : Auth_status.t list
    ; now : Time_ns.t option
    ; utc_offset : Time_ns.Span.t (** local time minus UTC *)
    ; narrow : bool
    ; draft : string
    ; cursor : int (** the caret's byte offset in [draft] *)
    ; completion : Completion.t option
    ; history : History.t
    ; images : Image.t list (** attached to the next prompt *)
    ; queue : Queued.t
    ; agent_queues : Agent_queue.t list
      (** the background subagents' queues, those not empty *)
    ; recipient : Recipient.t
      (** who the composer sends to: a subagent while steering it *)
    ; confirms : Pending.Confirm.t list
    ; confirm_reason : string option
      (** the oldest confirmation's reason, while typing it ([r]) *)
    ; questions : Question_dialog.t list (** [ask_user] calls, oldest first *)
    ; dialog : Dialog.t option
    ; cancelled_login : string option
      (** the provider whose login we cancelled: its failure is expected *)
    ; toasts : Toast.t list
    ; next_toast : int
    ; sidebar_open : bool
    ; scrolled_up : bool (** the chat is not following new output *)
    ; session_query : string
    ; archived_open : bool (** the sidebar shows the archived sessions *)
    ; agents : Agents.t (** the agents panel, per session *)
    ; verbosity : Prigh_ui.Verbosity.t
    ; folds : String.Set.t
      (** tool outputs ([Fold.key]s) toggled from how [verbosity] shows them *)
    ; config : Config.t option
    ; btw : Btw.t option
    ; btw_seq : int
    ; accounts : Accounts.Account.t list (** saved in this browser *)
    ; account : Accounts.Account.t option (** the one signed in *)
    ; users : string list option
      (** the users we may act as: [Some] for superusers *)
    ; terminal : Terminal.t
    ; skills : Skills.t (** for [/skill:]'s completion *)
    ; worktrees : Worktrees.t (** the session's, [/worktree]'s completion *)
    ; repo_worktrees : Repo_listings.t
      (** the repositories of the worktrees under the sidebar's sessions *)
    ; folded : String.Set.t
      (** the sidebar's sessions whose worktrees are folded away, by id (a
          preference) *)
    ; pending : In_flight.t (** what we wait on the backend for, on show *)
    ; marks : Session_marks.t (** the sidebar's selection *)
    ; notifications : Page_notify.t
    ; files : Files.t
    ; review : Code_review.t
    ; review_memory : Review_memory.t
      (** what the review page compared last, per worktree (a
          preference) *)
    ; layout : Resize.t (** the panels' sizes *)
    ; paging : Paging.t (** how much of the transcript is loaded *)
    ; chat_cache : Chat_cache.t
    ; jump : Jump.t
    ; trimmed : Trimmed.t
    ; outputs_loading : String.Set.t
    (** [Fold] keys of elided outputs being fetched whole *)
    ; session_gone : string option
    (** reconnected to a backend that no longer has this session: its
        transcript stays on show, read-only, until another is chosen *)
    ; forges : Forges.t (** the user's git hosts *)
    ; nix_gc : Nix_gc.t (** superusers': the Nix store *)
    ; return_to_settings : bool
    (** the login dialog showing was opened from the settings page: closing
        it goes back there *)
    ; theme : Theme.t (** the user's ([/theme], [/accent]) *)
    ; theme_preview : Theme.t option
      (** an accent being picked in the settings, shown but not saved *)
    ; prefs : Pref_sync.t
    ; generate : Generate.t (** the field being drafted by the model *)
    ; git_bar : Git_bar.t (** the review page's *)
    }
  [@@deriving sexp_of]

  (** The session's turn runs. *)
  val running : t -> bool

  (** The sidebar is open beside the chat (not a phone's drawer): it shows
      the session's status, which the header over the chat shows otherwise. *)
  val docked : t -> bool

  (** A tool confirmation or an [ask_user] question is showing: it owns the
      keyboard, above any dialog. *)
  val waiting : t -> bool
  (** The turn or background subagents or jobs run: the session is busy. *)
  val active : t -> bool

  (** The agents panel is open with something running: the page sends
      [Clock] every second. *)
  val ticking : t -> bool

  (** How messages' times read: in [utc_offset], relative to [now]'s day. *)
  val message_time : t -> Message_time.t

  (** The completion popup when it has something to show. *)
  val popup : t -> Completion.t option

  (** How tool outputs fold in the transcripts. *)
  val fold : t -> Fold.t

  (** Where the transcript's entries are in the context. *)
  val message_index : t -> Message_index.t

  (** What the panels' sizes depend on. *)
  val shown : t -> Resize.Shown.t

  (** The theme on show: the one highlighted in the [/theme] or [/accent]
      picker, or being previewed, else the user's. *)
  val look : t -> Theme.t

  (** Why the backend keeps a session asked to be deleted, if it does. *)
  val kept_from_deletion : t -> Session_summary.t -> string option
end

val thinking_levels : string list

(** A token count in a few characters: [200k], [1.2M]. *)
val compact_tokens : int -> string

(** Whether the user signed in as a superuser. *)
val superuser : Model.t -> bool

val init : Model.t
val update : Model.t -> Action.t -> Model.t * Command.t list
