(** Powerful table and datagrids built using TanStack Table. Port of
    shadcn/ui's [Data Table]: a small port of TanStack Table's state and row
    models (sorting, column filters, column visibility, row selection and
    pagination) whose [header] and [cell] renderers you lay out with
    [Table]'s parts, as shadcn's example does.

    {[
      let table = Data_table.component ~columns ~get_row_id data graph in
      let%arr table in
      Table.root
        [ Table.header
            [ Table.row
                (List.map (Data_table.visible_columns table) ~f:(fun column ->
                   Table.head (Data_table.header table column)))
            ]
        ; Table.body
            (List.map (Data_table.rows table) ~f:(fun row ->
               Table.row
                 ~selected:(Data_table.is_selected table row)
                 (List.map (Data_table.visible_columns table) ~f:(fun column ->
                    Table.cell (Data_table.cell table column row)))))
        ]
    ]} *)

open! Import

module Sort_direction : sig
  type t =
    | Asc
    | Desc
  [@@deriving enumerate, sexp_of, compare, equal]

  (** TanStack's name: [asc] or [desc]. *)
  val name : t -> string
end

(** TanStack's table state. *)
module State : sig
  type t =
    { sorting : (string * Sort_direction.t) list
      (** the columns sorted by, first the most significant *)
    ; column_filters : string String.Map.t (** each column's filter value *)
    ; column_visibility : bool String.Map.t (** absent: visible *)
    ; row_selection : String.Set.t (** the selected rows' ids *)
    ; page_index : int
    ; page_size : int
    }
  [@@deriving sexp_of, equal]

  (** Nothing sorted, filtered, hidden or selected; the first page of 10
      rows. *)
  val default : t
end

(** A table: its columns and data, its state, and the rows they give. *)
type 'row t

module Row : sig
  type 'row t =
    { id : string (** [get_row_id]'s, or the row's index in the data *)
    ; index : int (** in the data *)
    ; original : 'row
    }
end

module Column : sig
  type 'row table := 'row t
  type 'row t

  (** A column. [header] renders its heading (by default the [id]);
      [cell] a row's cell. [sort] makes it sortable; [filter] gives the text
      its filter value is looked for in (case-insensitively, as TanStack's
      [includesString]). [hideable] (default [true]): whether
      [toggle_visibility] may hide it ([false] for display columns such as
      a selection or actions column). *)
  val create
    :  ?header:('row table -> Node.t list)
    -> ?sort:('row -> 'row -> int)
    -> ?filter:('row -> string)
    -> ?hideable:bool
    -> id:string
    -> cell:('row table -> 'row Row.t -> Node.t list)
    -> unit
    -> 'row t

  val id : _ t -> string
end

(** TanStack's [sortFn_text]: compares the text case-insensitively. *)
val sort_text : ('row -> string) -> 'row -> 'row -> int

(** Controlled: [on_change] gets an updater of the state (as TanStack's
    [on*Change]; with [Bonsai.state'], pass its setter). Rows are named by
    [get_row_id] (by default their index). *)
val create
  :  ?get_row_id:('row -> string)
  -> columns:'row Column.t list
  -> state:State.t
  -> on_change:((State.t -> State.t) -> unit Effect.t)
  -> 'row list
  -> 'row t

(** Keeps the state in Bonsai ([default_state], {!State.default} at
    first). *)
val component
  :  ?get_row_id:('row -> string)
  -> ?default_state:State.t
  -> columns:'row Column.t list Bonsai.t
  -> 'row list Bonsai.t
  -> local_ Bonsai.graph
  -> 'row t Bonsai.t

val state : _ t -> State.t

(** {2 Columns} *)

val all_columns : 'row t -> 'row Column.t list
val visible_columns : 'row t -> 'row Column.t list
val column : 'row t -> string -> 'row Column.t option
val can_hide : _ Column.t -> bool
val is_visible : _ t -> string -> bool
val toggle_visibility : _ t -> string -> bool -> unit Effect.t

(** The column's heading and a row's cell: TanStack's [flexRender]. *)
val header : 'row t -> 'row Column.t -> Node.t list

val cell : 'row t -> 'row Column.t -> 'row Row.t -> Node.t list

(** {2 Filtering} *)

(** The column's filter value ([""] when none). *)
val filter_value : _ t -> string -> string

(** Sets the column's filter value (back to the first page). *)
val set_filter_value : _ t -> string -> string -> unit Effect.t

(** {2 Sorting} *)

val is_sorted : _ t -> string -> Sort_direction.t option

(** Sorts by the column alone (back to the first page): descending if
    [desc], ascending if not; without [desc], cycles from ascending to
    descending to unsorted. *)
val toggle_sorting : ?desc:bool -> _ t -> string -> unit Effect.t

(** {2 Rows} *)

(** Every row, in the data's order. *)
val core_rows : 'row t -> 'row Row.t list

(** The rows the column filters keep. *)
val filtered_rows : 'row t -> 'row Row.t list

(** The current page of the filtered, sorted rows: those to show. *)
val rows : 'row t -> 'row Row.t list

(** {2 Selection} *)

val is_selected : _ t -> _ Row.t -> bool
val toggle_selected : _ t -> _ Row.t -> bool -> unit Effect.t

(** The filtered rows that are selected. *)
val filtered_selected_rows : 'row t -> 'row Row.t list

val is_all_page_rows_selected : _ t -> bool
val is_some_page_rows_selected : _ t -> bool

(** Selects (or deselects) every row on the current page. *)
val toggle_all_page_rows_selected : _ t -> bool -> unit Effect.t

(** {2 Pagination} *)

val page_count : _ t -> int
val can_previous_page : _ t -> bool
val can_next_page : _ t -> bool
val previous_page : _ t -> unit Effect.t
val next_page : _ t -> unit Effect.t
val set_page_index : _ t -> int -> unit Effect.t
val set_page_size : _ t -> int -> unit Effect.t
