(** A responsive table component. Port of shadcn/ui's [Table].

    {[
      Table.root
        [ Table.caption [ Node.text "A list of your recent invoices." ]
        ; Table.header
            [ Table.row [ Table.head [ Node.text "Invoice" ]; ... ] ]
        ; Table.body
            [ Table.row [ Table.cell [ Node.text "INV001" ]; ... ] ]
        ]
    ]} *)

open! Import

(** A [<table>] in a [div] that scrolls sideways when the table is too wide
    ([class_] goes on the table). *)
val root : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** [<thead>]. *)
val header : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** [<tbody>]. *)
val body : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** [<tfoot>]. *)
val footer : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** [<tr>]; [selected] sets [data-state="selected"]. *)
val row
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?selected:bool
  -> Node.t list
  -> Node.t

(** [<th>]. *)
val head : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** [<td>]. *)
val cell
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?col_span:int
  -> Node.t list
  -> Node.t

(** [<caption>]. *)
val caption : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
