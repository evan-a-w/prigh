module Project = struct
  type t = { name : string }

  let prigh = { name = "prigh" }
  let loon = { name = "loon" }
  let co = { name = "co" }
end

module Status = struct
  type t =
    | Thinking
    | Working
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
    ; status = Thinking
    }
  ;;

  let recent =
    [ current
    ; { project = Project.loon
      ; title = "Port shadcn's Resizable"
      ; age = "12m"
      ; status = Working
      }
    ; { project = Project.prigh
      ; title = "Flaky tmux-test on posix backend"
      ; age = "1h"
      ; status = Idle
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
    ; { project = Project.prigh
      ; title = "Composer chips as a toggle group"
      ; age = "2d"
      ; status = Idle
      }
    ]
  ;;

  let settled = 4
end

module Subagent = struct
  type t =
    { name : string
    ; task : string
    ; status : Status.t
    ; age : string
    }

  let all =
    [ { name = "Touch targets"
      ; task = "Fix the chips' touch targets"
      ; status = Working
      ; age = "now"
      }
    ; { name = "Phone review"
      ; task = "Check the ⋯ sheet on 320px"
      ; status = Thinking
      ; age = "now"
      }
    ; { name = "Inventory"
      ; task = "List prigh-web's UI surfaces"
      ; status = Idle
      ; age = "2m"
      }
    ; { name = "Overlays"
      ; task = "Mock the palette and popovers"
      ; status = Idle
      ; age = "1h"
      }
    ]
  ;;
end

module Worktree = struct
  type t =
    { branch : string
    ; path : string
    ; sessions : int
    ; added : int
    ; removed : int
    }

  let all =
    [ { branch = "feat/new-ui"
      ; path = "~/.prigh/worktrees/prigh/feat-new-ui"
      ; sessions = 1
      ; added = 12
      ; removed = 3
      }
    ; { branch = "feat/composer-chips"
      ; path = "~/.prigh/worktrees/prigh/composer-chips"
      ; sessions = 2
      ; added = 95
      ; removed = 60
      }
    ; { branch = "fix/posix-hang"
      ; path = "~/.prigh/worktrees/prigh/posix-hang"
      ; sessions = 1
      ; added = 4
      ; removed = 1
      }
    ]
  ;;
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
  let mode = "Full access"
  let cwd = "~/.prigh/worktrees/prigh/feat-new-ui"
  let branch = "feat/new-ui"
end
