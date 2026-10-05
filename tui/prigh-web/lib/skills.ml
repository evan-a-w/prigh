open! Core
open! Import

module Status = struct
  type t =
    | Requested
    | Loaded of Skill.t list
  [@@deriving sexp_of]
end

type t = (string * Status.t) option [@@deriving sexp_of]

let empty = None

let place (state : State.t option) =
  Option.map state ~f:(fun s ->
    String.concat ~sep:"\n" [ s.session_id; s.active_host; s.cwd ])
;;

let find t ~place =
  match t with
  | Some (p, Status.Loaded skills) when String.equal p place -> Some skills
  | _ -> None
;;

let unknown t ~place =
  not (Option.exists t ~f:(fun (p, _) -> String.equal p place))
;;

let requested ~place = Some (place, Status.Requested)
let loaded ~place skills = Some (place, Status.Loaded skills)

let none =
  "No skills here: put one in .prigh/skills/<name>/SKILL.md (or \
   .claude/skills/, .agents/skills/) in the project, or in ~/.prigh/skills/."
;;

let picker ?query skills =
  Dialog.Picker
    { kind = Skills
    ; picker =
        Picker.create
          ?query
          ~title:"Skills"
          (List.map skills ~f:(fun (s : Skill.t) ->
             Picker.Item.create
               ~id:s.name
               ~detail:
                 (String.concat
                    ~sep:" · "
                    ([ s.description; Filename.dirname s.path ]
                     @
                     if s.model_invocable then [] else [ "only you invoke it" ]
                    ))
               ~search:(s.name ^ " " ^ s.description)
               s.name))
    }
;;
