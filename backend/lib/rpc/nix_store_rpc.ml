open! Core
open! Import

let methods = [ "nix_store_status"; "nix_store_gc" ]

let authorise ~(signed_in : User_access.Signed_in.t option) =
  match signed_in with
  | None ->
    Or_error.error_string
      "cleaning the Nix store from prigh is for superusers, and this backend \
       has no users: run `nix store gc` in a shell, or serve with -tokens and \
       -superusers"
  | Some s when s.superuser -> Ok s.user
  | Some s ->
    (match User_access.Signed_in.superusers s with
     | [] ->
       Or_error.errorf
         "cleaning the Nix store is for superusers, and there are none: ask \
          whoever runs prigh to make %s one (PRIGH_SUPERUSERS, or -superusers)"
         s.user
     | superusers ->
       Or_error.errorf
         "cleaning the Nix store is for superusers: ask %s to run /nix-gc"
         (String.concat ~sep:" or " superusers))
;;

let param params name =
  match params with
  | `Object fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None
;;

let dispatch nix_store ~signed_in ~send ~meth ~params =
  if not (List.mem methods meth ~equal:String.equal)
  then None
  else
    Option.some
    @@
    let open Or_error.Let_syntax in
    let%bind by = authorise ~signed_in in
    let%bind nix_store =
      match nix_store with
      | Some nix_store -> Ok nix_store
      | None -> Or_error.error_string "this backend does not manage a Nix store"
    in
    match meth with
    | "nix_store_status" ->
      Nix_store.status nix_store >>| Nix_store_status.to_json
    | _ ->
      let%bind generations =
        Nix_generations.of_json (param params "delete_generations")
      in
      Nix_store.gc nix_store ~by generations ~on_progress:(fun progress ->
        send (Nix_gc_progress.to_json progress))
      >>| Nix_gc_report.to_json
;;
