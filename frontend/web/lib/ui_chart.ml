open! Core
open! Import

let view (_ : Ui_context.t) ~path:_ (chart : Ui_spec.Chart.t) =
  Node.figure
    ~attrs:[ Attr.class_ "ui-chart" ]
    [ Node.text (Option.value chart.title ~default:"Chart") ]
;;
