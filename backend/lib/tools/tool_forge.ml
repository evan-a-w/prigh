open! Core
open! Import

let host_op op ~run =
  { Tool.spec =
      { Tool_spec.name = op
      ; description = "git hosting"
      ; parameters = `Object []
      ; parallel_safe = true
      ; destructive = Never
      ; on_host = true
      ; freeform = None
      ; native = None
      }
  ; run
  }
;;

let git_forge =
  host_op Git_forge.op ~run:(fun (context : Tool.Context.t) arguments ->
    Git_forge.run
      ~env:context.env
      ~cancel:context.cancel
      ~cwd:context.cwd
      ~arguments)
;;

let git_location =
  host_op Git_location.op ~run:(fun (context : Tool.Context.t) _ ->
    Git_location.run ~cwd:context.cwd)
;;

let on_host (context : Tool.Context.t) ~cwd tool arguments =
  Tool.execute_via { context with cwd; on_output = ignore } tool arguments
;;

let cwd (context : Tool.Context.t) args =
  match Tool_args.string_opt args "cwd" with
  | None -> context.cwd
  | Some cwd -> Tool.resolve_path context cwd
;;

let git context ~cwd request =
  on_host context ~cwd git_forge (Git_forge.Request.to_json request)
;;

let for_the_model error =
  List.fold
    [ "/pr lists", "action=list lists"
    ; "/cd into one", "cwd names one"
    ; ": sign in under", ": ask the user to sign in under"
    ; ": sign in again with", ": ask the user to sign in again with"
    ]
    ~init:error
    ~f:(fun error (pattern, with_) ->
      String.substr_replace_all error ~pattern ~with_)
;;

let number ~what args =
  match Tool_args.int_opt args "number" with
  | Some n -> n
  | None -> raise (Tool_args.Invalid (sprintf "missing \"number\": %s" what))
;;

let int_list args name =
  match Tool_args.list_opt args name with
  | None -> []
  | Some items ->
    List.map items ~f:(fun j ->
      match Json.int j with
      | Some n -> n
      | None ->
        (match j with
         | `String s ->
           (match
              Int.of_string_opt (String.chop_prefix_if_exists s ~prefix:"#")
            with
            | Some n -> n
            | None ->
              raise
                (Tool_args.Invalid (sprintf "%S must be a list of numbers" name)))
         | _ ->
           raise
             (Tool_args.Invalid (sprintf "%S must be a list of numbers" name))))
;;

let strings args name =
  Option.value (Tool_args.string_list_opt args name) ~default:[]
;;

let required_string args name ~why =
  match Tool_args.string_opt args name with
  | Some s when not (String.is_empty (String.strip s)) -> s
  | _ -> raise (Tool_args.Invalid (sprintf "missing %S: %s" name why))
;;

let repo_name (repo : Forge_repo.t) =
  sprintf "%s/%s on %s" repo.owner repo.name repo.instance.host
;;

let clip_lines n s =
  let lines = String.split_lines s in
  let total = List.length lines in
  if total <= n
  then s
  else
    String.concat ~sep:"\n" (List.take lines n)
    ^ sprintf "\n[... %d more lines]" (total - n)
;;
