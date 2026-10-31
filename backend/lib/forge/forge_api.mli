open! Core
open! Import

(** The few REST calls prigh makes on GitHub ([api.github.com], or
    [HOST/api/v3]) and Forgejo/Gitea ([HOST/api/v1]). Failures say what
    happened and what to do (sign in again, push the branch). *)

type t =
  { http : Forge_http.t
  ; instance : Forge_instance.t
  ; authorization : string (** the [Authorization] header *)
  ; cancel : Cancellation.t
  }

val create
  :  ?cancel:Cancellation.t
  -> http:Forge_http.t
  -> Forge_instance.t
  -> Forge_credential.t
  -> t

(** {2 Requests} Failures say what failed ([what]) and what to do: 401
    asks to sign in again, 403 names a missing scope, 404 adds [not_found]
    (or is [on_404]). *)

(** [Authorization], [Accept] (default: the host's JSON), [User-Agent],
    GitHub's API version and, with [json_body], [Content-Type]. *)
val headers : ?accept:string -> ?json_body:bool -> t -> (string * string) list

(** The [message] (and [errors]) of an error reply. *)
val message_of_body : string -> string option

(** The error for a reply that is not a success. *)
val http_error
  :  t
  -> what:string
  -> ?not_found:string
  -> ?on_404:string
  -> Forge_http.Response.t
  -> 'a Or_error.t

(** The body of a 2xx reply. *)
val send
  :  t
  -> ?accept:string
  -> ?body:Json.t
  -> what:string
  -> ?not_found:string
  -> ?on_404:string
  -> Forge_http.Meth.t
  -> string
  -> string Or_error.t

(** {!send}, parsed; an empty body is [`Null]. *)
val send_json
  :  t
  -> ?body:Json.t
  -> what:string
  -> ?not_found:string
  -> ?on_404:string
  -> Forge_http.Meth.t
  -> string
  -> Json.t Or_error.t

val get_json
  :  t
  -> what:string
  -> ?not_found:string
  -> ?on_404:string
  -> string
  -> Json.t Or_error.t

(** The items of a list, following the [Link: <...>; rel="next"] pages both
    hosts send, up to [limit] (default 100) items. [items] finds the list in
    a page (default: the page is the list). Asks for the host's largest page
    ([per_page=100], Forgejo [limit=50], which its MAX_RESPONSE_ITEMS may
    lower) unless [url] names a size. *)
val get_list
  :  ?limit:int
  -> ?items:(Json.t -> Json.t option)
  -> t
  -> what:string
  -> ?not_found:string
  -> string
  -> Json.t list Or_error.t

(** GitHub's GraphQL API: the reply's [data], or its errors. Forgejo has
    none (an error). *)
val graphql
  :  t
  -> what:string
  -> query:string
  -> variables:Json.t
  -> Json.t Or_error.t

(** A GET whose redirect (GitHub's job logs go to signed storage URLs) is
    followed once, without the [Authorization] header. *)
val get_following : t -> what:string -> string -> string Or_error.t

(** [": check the URL of the git remote ..."], for [not_found]. *)
val repo_hint : Forge_repo.t -> string

(** The signed-in account's login. *)
val current_user : t -> string Or_error.t

(** [state]: [open], [closed] or [all]; newest first. *)
val list_pulls
  :  t
  -> Forge_repo.t
  -> state:string
  -> Pull_request.t list Or_error.t

val get_pull : t -> Forge_repo.t -> int -> Pull_request.t Or_error.t

(** The whole unified diff, cut at {!max_diff_bytes} (then [true]). *)
val pull_diff : t -> Forge_repo.t -> int -> (string * bool) Or_error.t

val max_diff_bytes : int
val checks : t -> Forge_repo.t -> sha:string -> Pr_checks.t Or_error.t
val default_branch : t -> Forge_repo.t -> string Or_error.t

(** A draft on Forgejo is a [WIP:] title. *)
val create_pull
  :  t
  -> Forge_repo.t
  -> title:string
  -> body:string
  -> head:string
  -> base:string
  -> draft:bool
  -> Pull_request.t Or_error.t
