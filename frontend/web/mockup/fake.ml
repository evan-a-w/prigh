module Project = struct
  type t =
    { name : string
    ; tag : string
    ; tag_class : string
    }

  let prigh =
    { name = "prigh"
    ; tag = "PR"
    ; tag_class = "bg-sky-400/15 text-sky-700 dark:text-sky-300"
    }
  ;;

  let loon =
    { name = "loon"
    ; tag = "LN"
    ; tag_class = "bg-teal-400/15 text-teal-700 dark:text-teal-300"
    }
  ;;

  let co =
    { name = "co"
    ; tag = "CO"
    ; tag_class = "bg-amber-400/15 text-amber-700 dark:text-amber-300"
    }
  ;;
end

module Status = struct
  type t =
    | Running
    | Waiting
    | Unread
    | Idle
end

module Session = struct
  type t =
    { project : Project.t
    ; title : string
    ; age : string
    ; status : Status.t
    }

  let current =
    { project = Project.prigh
    ; title = "Rebuild prigh-web on loon"
    ; age = "now"
    ; status = Running
    }
  ;;

  let recent =
    [ current
    ; { project = Project.loon
      ; title = "Port shadcn's Resizable"
      ; age = "12m"
      ; status = Waiting
      }
    ; { project = Project.prigh
      ; title = "Flaky tmux-test on posix backend"
      ; age = "1h"
      ; status = Unread
      }
    ; { project = Project.co
      ; title = "Weekly report draft"
      ; age = "3h"
      ; status = Idle
      }
    ; { project = Project.loon
      ; title = "Chart tooltips on touch"
      ; age = "1d"
      ; status = Idle
      }
    ]
  ;;

  let worktree =
    ( "feat/composer-chips"
    , [ { project = Project.prigh
        ; title = "Composer chips as a toggle group"
        ; age = "2h"
        ; status = Idle
        }
      ; { project = Project.prigh
        ; title = "Review: chips on 320px"
        ; age = "2h"
        ; status = Unread
        }
      ] )
  ;;

  let settled = 4
end

module Tool_call = struct
  type t =
    { tool : string
    ; summary : string
    ; duration : string
    ; ok : bool
    ; output : string option
    }

  let call ?output ?(ok = true) tool summary duration =
    { tool; summary; duration; ok; output }
  ;;

  let first_turn =
    [ call "read" "frontend/web/lib/composer_view.ml" "0.1s"
    ; call "grep" "chip  frontend/web/lib" "0.2s"
    ; call "read" "frontend/web/lib/session_chips_view.ml" "0.1s"
    ]
  ;;

  let second_turn =
    [ call "read" "frontend/web/loon_ui/input_group.mli" "0.1s"
    ; call "edit" "frontend/web/lib/composer_view.ml  +34 −12" "0.3s"
    ; call
        "bash"
        "dune build @web/test/runtest"
        "41s"
        ~output:
          "File \"web/test/test_composer.ml\", line 88, characters 0-1:\n\
          \ ┌ composer\n\
          \ │ [+] [Opus 4.5 ▾] [High ▾] [Auto ▾] [desktop ▾]      42%  $1.84  ↑\n\
          \ └\n\
           Promoted 1 file.\n\
           Done: 1284/1284 (jobs: 16)"
    ; call
        "bash"
        "./test"
        "1m 02s"
        ~ok:false
        ~output:
          "web e2e (chromium): 1 diff\n\
          \  mobile.mjs: composer chip at 318px is 38px tall (min 44px)"
    ; call "subagent" "Fix the chips' touch targets" "running"
    ]
  ;;
end

module User = struct
  let name = "Lloyd"
  let initials = "LG"
  let host = "desktop"
  let model = "Opus 4.5"
  let thinking = "High"
  let mode = "Auto"
  let cwd = "~/.prigh/worktrees/prigh/feat-new-ui"
  let branch = "feat/new-ui"
end
