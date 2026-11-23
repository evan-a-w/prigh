(** A rendered page to query with CSS selectors, as [Node_helpers.select]
    does, converting the page for the selector engine once however often it
    is queried. *)

open! Core

type t

val of_node : Virtual_dom_test_helpers.Node_helpers.t -> t
val root : t -> Virtual_dom_test_helpers.Node_helpers.t

val select
  :  t
  -> selector:string
  -> Virtual_dom_test_helpers.Node_helpers.t list
