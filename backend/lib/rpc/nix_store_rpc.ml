open! Core
open! Import

let methods = [ "nix_store_status"; "nix_store_gc" ]

let param params name =
  match params with
  | `Object fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None
;;

let dispatch nix_store ~signed_in ~send ~meth ~params =
  match (signed_in : User_access.Signed_in.t option) with
  | Some { superuser = true; user = by; _ }
    when List.mem methods meth ~equal:String.equal ->
    Option.some
    @@
    let open Or_error.Let_syntax in
    let%bind nix_store =
      match nix_store with
      | Some nix_store -> Ok nix_store
      | None -> Or_error.error_string "this backend does not manage a Nix store"
    in
    (match meth with
     | "nix_store_status" ->
       Nix_store.status nix_store >>| Nix_store_status.to_json
     | _ ->
       let%bind generations =
         Nix_generations.of_json (param params "delete_generations")
       in
       Nix_store.gc nix_store ~by generations ~on_progress:(fun progress ->
         send (Nix_gc_progress.to_json progress))
       >>| Nix_gc_report.to_json)
  | _ -> None
;;
