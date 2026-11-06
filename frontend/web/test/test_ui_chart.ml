open! Core
open Prigh_web
module C = Prigh_protocol.Ui_spec.Chart

let ctx =
  Ui_context.create Ui_context.Page.none ~call_id:"c1" ~fold:Fold.default
;;

let chart
      ?title
      ?subtitle
      ?unit
      ?x_label
      ?y_label
      ?(height = C.Height.Medium)
      kind
      data
  =
  { C.kind; title; subtitle; unit; x_label; y_label; height; data }
;;

let show ?selector c = Render.html ?selector (Ui_chart.view ctx ~path:"0" c)

let count selector c =
  printf
    "%d %s\n"
    (List.length
       (Virtual_dom_test_helpers.Node_helpers.select
          (Virtual_dom_test_helpers.Node_helpers.unsafe_convert_exn
             (Ui_chart.view ctx ~path:"0" c))
          ~selector))
    selector
;;

let categories labels series =
  C.Data.Categories
    { labels
    ; series =
        List.map series ~f:(fun (name, values) -> { C.Series.name; values })
    }
;;

let%expect_test "ticks and positions" =
  let ticks ?zero values =
    print_s [%sexp (Ui_chart.axis ?zero values : float list)]
  in
  ticks [ 3.; 17. ];
  ticks ~zero:true [ 52.; 97. ];
  ticks [ -12.; 40. ];
  ticks [ 5. ];
  ticks [];
  ticks [ 0.001; 0.0042 ];
  ticks [ 1_200_000.; 9_800_000. ];
  [%expect
    {|
    (0 5 10 15 20)
    (0 20 40 60 80 100)
    (-20 -10 0 10 20 30 40)
    (0 1 2 3 4 5)
    (0 0.2 0.4 0.6 0.8 1)
    (0 0.001 0.002 0.003 0.004 0.005)
    (0 2000000 4000000 6000000 8000000 10000000)
    |}];
  List.iter [ 0.; 5.; 20.; -5. ] ~f:(fun v ->
    printf "%g " (Ui_chart.position ~lo:0. ~hi:20. v));
  printf "%g\n" (Ui_chart.position ~lo:3. ~hi:3. 3.);
  [%expect {| 0 25 100 -25 0 |}];
  List.iter [ None; Some "%"; Some "$"; Some "ms"; Some "€" ] ~f:(fun unit ->
    printf "%s | " (Ui_chart.with_unit ~unit (Ui_format.number (-1200.))));
  [%expect {| −1,200 | −1,200% | −$1,200 | −1,200 ms | −€1,200 | |}]
;;

let%expect_test "slice paths" =
  print_endline (Ui_chart.slice_path ~hole:0. ~start:0. ~stop:0.25);
  print_endline (Ui_chart.slice_path ~hole:30. ~start:0.25 ~stop:1.);
  (* One slice of all: a whole ring, all but a hair. *)
  print_endline (Ui_chart.slice_path ~hole:0. ~start:0. ~stop:1.);
  [%expect
    {|
    M 50 2 A 48 48 0 0 1 98 50 L 50 50 Z
    M 98 50 A 48 48 0 1 1 50 2 L 50 20 A 30 30 0 1 0 80 50 Z
    M 50 2 A 48 48 0 1 1 50 2 L 50 50 Z
    |}]
;;

let%expect_test "line: axes, columns with markers and tooltips, legend, table" =
  show
    (chart
       Line
       ~title:"Build times"
       ~subtitle:"Last four weeks"
       ~unit:"s"
       ~y_label:"Duration"
       (categories
          [ "W1"; "W2"; "W3"; "W4" ]
          [ "main", [ Some 12.; Some 15.5; None; Some 9. ]
          ; "pr", [ Some 20.; None; Some 18.; Some 22. ]
          ]));
  [%expect
    {|
    <figure data-kind="line"
            role="figure"
            aria-label="Line chart: Build times"
            class="h-medium ui-chart">
      <div class="ui-title"> Build times </div>
      <div class="ui-chart-sub"> Last four weeks </div>
      <div aria-hidden="true" class="ui-chart-yname"> Duration (s) </div>
      <div style="--gutter: 3ch; --rows: 4;" class="ui-chart-frame">
        <div aria-hidden="true" class="ui-chart-yaxis">
          <span style="bottom: 0%;" class="ui-chart-tick"> 5 </span>
          <span style="bottom: 25%;" class="ui-chart-tick"> 10 </span>
          <span style="bottom: 50%;" class="ui-chart-tick"> 15 </span>
          <span style="bottom: 75%;" class="ui-chart-tick"> 20 </span>
          <span style="bottom: 100%;" class="ui-chart-tick"> 25 </span>
        </div>
        <div class="ui-chart-plot">
          <div aria-hidden="true" class="ui-chart-grid">
            <span style="bottom: 0%;" class="ui-chart-gridline"> </span>
            <span style="bottom: 25%;" class="ui-chart-gridline"> </span>
            <span style="bottom: 50%;" class="ui-chart-gridline"> </span>
            <span style="bottom: 75%;" class="ui-chart-gridline"> </span>
            <span style="bottom: 100%;" class="ui-chart-gridline"> </span>
          </div>
          <svg class="ui-chart-svg"
               viewBox="0 0 100 100"
               aria-hidden="true"
               focusable="false"
               preserveAspectRatio="none">
            <path class="s1 ui-chart-line" d="M12.5 65 L37.5 47.5 M87.5 80"> </path>
            <path class="s2 ui-chart-line" d="M12.5 25 M62.5 35 L87.5 15"> </path>
          </svg>
          <div style="left: 0%; width: 25%;"
               tabindex="0"
               role="img"
               aria-label="W1: main 12 s, pr 20 s"
               class="ui-chart-col">
            <span class="ui-chart-guide"> </span>
            <span style="bottom: 35%;" class="s1 ui-chart-dot"> </span>
            <span style="bottom: 75%;" class="lone s2 ui-chart-dot"> </span>
          </div>
          <div aria-hidden="true" style="--x: 12.5%;" class="right ui-chart-tip">
            <div class="ui-chart-tip-head"> W1 </div>
            <div class="s1 ui-chart-tip-row">
              <span class="ui-chart-swatch"> </span>
              <span class="ui-chart-name"> main </span>
              <span class="ui-chart-value"> 12 s </span>
            </div>
            <div class="s2 ui-chart-tip-row">
              <span class="ui-chart-swatch"> </span>
              <span class="ui-chart-name"> pr </span>
              <span class="ui-chart-value"> 20 s </span>
            </div>
          </div>
          <div style="left: 25%; width: 25%;"
               tabindex="0"
               role="img"
               aria-label="W2: main 15.5 s, pr no value"
               class="ui-chart-col">
            <span class="ui-chart-guide"> </span>
            <span style="bottom: 52.5%;" class="s1 ui-chart-dot"> </span>
          </div>
          <div aria-hidden="true" style="--x: 37.5%;" class="right ui-chart-tip">
            <div class="ui-chart-tip-head"> W2 </div>
            <div class="s1 ui-chart-tip-row">
              <span class="ui-chart-swatch"> </span>
              <span class="ui-chart-name"> main </span>
              <span class="ui-chart-value"> 15.5 s </span>
            </div>
            <div class="s2 ui-chart-tip-row">
              <span class="ui-chart-swatch"> </span>
              <span class="ui-chart-name"> pr </span>
              <span class="ui-chart-value"> — </span>
            </div>
          </div>
          <div style="left: 50%; width: 25%;"
               tabindex="0"
               role="img"
               aria-label="W3: main no value, pr 18 s"
               class="ui-chart-col">
            <span class="ui-chart-guide"> </span>
            <span style="bottom: 65%;" class="s2 ui-chart-dot"> </span>
          </div>
          <div aria-hidden="true" style="--x: 62.5%;" class="left ui-chart-tip">
            <div class="ui-chart-tip-head"> W3 </div>
            <div class="s1 ui-chart-tip-row">
              <span class="ui-chart-swatch"> </span>
              <span class="ui-chart-name"> main </span>
              <span class="ui-chart-value"> — </span>
            </div>
            <div class="s2 ui-chart-tip-row">
              <span class="ui-chart-swatch"> </span>
              <span class="ui-chart-name"> pr </span>
              <span class="ui-chart-value"> 18 s </span>
            </div>
          </div>
          <div style="left: 75%; width: 25%;"
               tabindex="0"
               role="img"
               aria-label="W4: main 9 s, pr 22 s"
               class="ui-chart-col">
            <span class="ui-chart-guide"> </span>
            <span style="bottom: 20%;" class="lone s1 ui-chart-dot"> </span>
            <span style="bottom: 85%;" class="s2 ui-chart-dot"> </span>
          </div>
          <div aria-hidden="true" style="--x: 87.5%;" class="left ui-chart-tip">
            <div class="ui-chart-tip-head"> W4 </div>
            <div class="s1 ui-chart-tip-row">
              <span class="ui-chart-swatch"> </span>
              <span class="ui-chart-name"> main </span>
              <span class="ui-chart-value"> 9 s </span>
            </div>
            <div class="s2 ui-chart-tip-row">
              <span class="ui-chart-swatch"> </span>
              <span class="ui-chart-name"> pr </span>
              <span class="ui-chart-value"> 22 s </span>
            </div>
          </div>
        </div>
        <div aria-hidden="true" class="ui-chart-xaxis">
          <span style="left: 12.5%; --w: 25%;" class="ui-chart-tick"> W1 </span>
          <span style="left: 37.5%; --w: 25%;" class="minor ui-chart-tick"> W2 </span>
          <span style="left: 62.5%; --w: 25%;" class="ui-chart-tick"> W3 </span>
          <span style="left: 87.5%; --w: 25%;" class="minor ui-chart-tick"> W4 </span>
        </div>
      </div>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <ul aria-hidden="true" class="ui-chart-legend">
        <li class="s1">
          <span class="ui-chart-swatch"> </span>
          <span class="ui-chart-name"> main </span>
        </li>
        <li class="s2">
          <span class="ui-chart-swatch"> </span>
          <span class="ui-chart-name"> pr </span>
        </li>
      </ul>
      <table class="sr-only">
        <caption> Build times </caption>
        <thead>
          <tr>
            <th>  </th>
            <th> main </th>
            <th> pr </th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td> W1 </td>
            <td> 12 s </td>
            <td> 20 s </td>
          </tr>
          <tr>
            <td> W2 </td>
            <td> 15.5 s </td>
            <td>  </td>
          </tr>
          <tr>
            <td> W3 </td>
            <td>  </td>
            <td> 18 s </td>
          </tr>
          <tr>
            <td> W4 </td>
            <td> 9 s </td>
            <td> 22 s </td>
          </tr>
        </tbody>
      </table>
    </figure>
    |}]
;;

let%expect_test "area from zero" =
  show
    ~selector:".ui-chart-svg"
    (chart
       Area
       (categories [ "a"; "b"; "c" ] [ "x", [ Some 4.; Some 8.; Some 6. ] ]));
  [%expect
    {|
    <svg class="ui-chart-svg"
         viewBox="0 0 100 100"
         aria-hidden="true"
         focusable="false"
         preserveAspectRatio="none">
      <path class="s1 ui-chart-fill" d="M 16.67 100 L 16.67 50 L50 0 L83.33 25 L 83.33 100 Z"> </path>
      <path class="s1 ui-chart-line" d="M16.67 50 L50 0 L83.33 25"> </path>
    </svg>
    |}]
;;

let%expect_test "bars: grouped, negatives from the zero line" =
  show
    ~selector:".ui-chart-plot"
    (chart
       Bar
       ~unit:"%"
       (categories
          [ "Q1"; "Q2" ]
          [ "a", [ Some 10.; Some (-5.) ]; "b", [ Some 3.; None ] ]));
  [%expect
    {|
    <div class="ui-chart-plot">
      <div aria-hidden="true" class="ui-chart-grid">
        <span style="bottom: 0%;" class="ui-chart-gridline"> </span>
        <span style="bottom: 33.33%;" class="ui-chart-gridline zero"> </span>
        <span style="bottom: 66.67%;" class="ui-chart-gridline"> </span>
        <span style="bottom: 100%;" class="ui-chart-gridline"> </span>
      </div>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <div style="left: 0%; width: 50%;"
           tabindex="0"
           role="img"
           aria-label="Q1: a 10%, b 3%"
           class="ui-chart-col">
        <span class="ui-chart-group">
          <span class="ui-chart-slot">
            <span style="bottom: 33.33%; height: 66.67%;" class="end s1 ui-chart-bar"> </span>
          </span>
          <span class="ui-chart-slot">
            <span style="bottom: 33.33%; height: 20%;" class="end s2 ui-chart-bar"> </span>
          </span>
        </span>
      </div>
      <div aria-hidden="true" style="--x: 25%;" class="right ui-chart-tip">
        <div class="ui-chart-tip-head"> Q1 </div>
        <div class="s1 ui-chart-tip-row">
          <span class="ui-chart-swatch"> </span>
          <span class="ui-chart-name"> a </span>
          <span class="ui-chart-value"> 10% </span>
        </div>
        <div class="s2 ui-chart-tip-row">
          <span class="ui-chart-swatch"> </span>
          <span class="ui-chart-name"> b </span>
          <span class="ui-chart-value"> 3% </span>
        </div>
      </div>
      <div style="left: 50%; width: 50%;"
           tabindex="0"
           role="img"
           aria-label="Q2: a −5%, b no value"
           class="ui-chart-col">
        <span class="ui-chart-group">
          <span class="ui-chart-slot">
            <span style="bottom: 0%; height: 33.33%;" class="end neg s1 ui-chart-bar"> </span>
          </span>
          <span class="ui-chart-slot"> </span>
        </span>
      </div>
      <div aria-hidden="true" style="--x: 75%;" class="left ui-chart-tip">
        <div class="ui-chart-tip-head"> Q2 </div>
        <div class="s1 ui-chart-tip-row">
          <span class="ui-chart-swatch"> </span>
          <span class="ui-chart-name"> a </span>
          <span class="ui-chart-value"> −5% </span>
        </div>
        <div class="s2 ui-chart-tip-row">
          <span class="ui-chart-swatch"> </span>
          <span class="ui-chart-name"> b </span>
          <span class="ui-chart-value"> — </span>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "stacked bars: a total in the tooltip" =
  show
    ~selector:".ui-chart-plot"
    (chart
       Stacked_bar
       ~unit:"$"
       (categories
          [ "Mon" ]
          [ "a", [ Some 2. ]; "b", [ Some 3. ]; "c", [ Some (-1.) ] ]));
  [%expect
    {|
    <div class="ui-chart-plot">
      <div aria-hidden="true" class="ui-chart-grid">
        <span style="bottom: 0%;" class="ui-chart-gridline"> </span>
        <span style="bottom: 25%;" class="ui-chart-gridline zero"> </span>
        <span style="bottom: 50%;" class="ui-chart-gridline"> </span>
        <span style="bottom: 75%;" class="ui-chart-gridline"> </span>
        <span style="bottom: 100%;" class="ui-chart-gridline"> </span>
      </div>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <div style="left: 0%; width: 100%;"
           tabindex="0"
           role="img"
           aria-label="Mon: a $2, b $3, c −$1"
           class="ui-chart-col">
        <span class="ui-chart-group">
          <span class="ui-chart-slot">
            <span style="bottom: 25%; height: 25%;" class="s1 ui-chart-bar"> </span>
            <span style="bottom: 50%; height: 37.5%;" class="end s2 ui-chart-bar"> </span>
            <span style="bottom: 12.5%; height: 12.5%;" class="end neg s3 ui-chart-bar"> </span>
          </span>
        </span>
      </div>
      <div aria-hidden="true" style="--x: 50%;" class="left ui-chart-tip">
        <div class="ui-chart-tip-head"> Mon </div>
        <div class="s1 ui-chart-tip-row">
          <span class="ui-chart-swatch"> </span>
          <span class="ui-chart-name"> a </span>
          <span class="ui-chart-value"> $2 </span>
        </div>
        <div class="s2 ui-chart-tip-row">
          <span class="ui-chart-swatch"> </span>
          <span class="ui-chart-name"> b </span>
          <span class="ui-chart-value"> $3 </span>
        </div>
        <div class="s3 ui-chart-tip-row">
          <span class="ui-chart-swatch"> </span>
          <span class="ui-chart-name"> c </span>
          <span class="ui-chart-value"> −$1 </span>
        </div>
        <div class="total ui-chart-tip-row">
          <span class="ui-chart-swatch"> </span>
          <span class="ui-chart-name"> Total </span>
          <span class="ui-chart-value"> $4 </span>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "horizontal bars: labels down the side" =
  show
    (chart
       Horizontal_bar
       ~x_label:"Test"
       ~y_label:"Seconds"
       (categories [ "parser"; "typer" ] [ "time", [ Some 4.2; Some 11. ] ]));
  [%expect
    {|
    <figure data-kind="horizontal_bar"
            role="figure"
            aria-label="Horizontal bar chart"
            class="h-medium ui-chart">
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <div aria-hidden="true" class="ui-chart-yname"> Test </div>
      <div style="--gutter: 7ch; --rows: 2;" class="ui-chart-frame">
        <div aria-hidden="true" class="ui-chart-cats ui-chart-yaxis">
          <span style="top: 25%;" class="ui-chart-tick"> parser </span>
          <span style="top: 75%;" class="ui-chart-tick"> typer </span>
        </div>
        <div class="ui-chart-plot">
          <div aria-hidden="true" class="ui-chart-grid">
            <span style="left: 0%;" class="ui-chart-gridline zero"> </span>
            <span style="left: 16.67%;" class="ui-chart-gridline"> </span>
            <span style="left: 33.33%;" class="ui-chart-gridline"> </span>
            <span style="left: 50%;" class="ui-chart-gridline"> </span>
            <span style="left: 66.67%;" class="ui-chart-gridline"> </span>
            <span style="left: 83.33%;" class="ui-chart-gridline"> </span>
            <span style="left: 100%;" class="ui-chart-gridline"> </span>
          </div>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          <div style="top: 0%; height: 50%;"
               tabindex="0"
               role="img"
               aria-label="parser: 4.2"
               class="ui-chart-col">
            <span class="ui-chart-group">
              <span class="ui-chart-slot">
                <span style="left: 0%; width: 35%;" class="end s1 ui-chart-bar"> </span>
              </span>
            </span>
          </div>
          <div aria-hidden="true" style="--y: 50%;" class="below ui-chart-tip">
            <div class="ui-chart-tip-head"> parser </div>
            <div class="s1 ui-chart-tip-row">
              <span class="ui-chart-swatch"> </span>
              <span class="ui-chart-name"> time </span>
              <span class="ui-chart-value"> 4.2 </span>
            </div>
          </div>
          <div style="top: 50%; height: 50%;"
               tabindex="0"
               role="img"
               aria-label="typer: 11"
               class="ui-chart-col">
            <span class="ui-chart-group">
              <span class="ui-chart-slot">
                <span style="left: 0%; width: 91.67%;" class="end s1 ui-chart-bar"> </span>
              </span>
            </span>
          </div>
          <div aria-hidden="true" style="--y: 50%;" class="above ui-chart-tip">
            <div class="ui-chart-tip-head"> typer </div>
            <div class="s1 ui-chart-tip-row">
              <span class="ui-chart-swatch"> </span>
              <span class="ui-chart-name"> time </span>
              <span class="ui-chart-value"> 11 </span>
            </div>
          </div>
        </div>
        <div aria-hidden="true" class="ui-chart-values ui-chart-xaxis">
          <span style="left: 0%;" class="ui-chart-tick"> 0 </span>
          <span style="left: 16.67%;" class="minor ui-chart-tick"> 2 </span>
          <span style="left: 33.33%;" class="ui-chart-tick"> 4 </span>
          <span style="left: 50%;" class="minor ui-chart-tick"> 6 </span>
          <span style="left: 66.67%;" class="ui-chart-tick"> 8 </span>
          <span style="left: 83.33%;" class="minor ui-chart-tick"> 10 </span>
          <span style="left: 100%;" class="ui-chart-tick"> 12 </span>
        </div>
      </div>
      <div aria-hidden="true" class="ui-chart-xname"> Seconds </div>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <table class="sr-only">
        <caption> Horizontal bar chart </caption>
        <thead>
          <tr>
            <th> Test </th>
            <th> time </th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td> parser </td>
            <td> 4.2 </td>
          </tr>
          <tr>
            <td> typer </td>
            <td> 11 </td>
          </tr>
        </tbody>
      </table>
    </figure>
    |}]
;;

let%expect_test "many labels: every few on the axis, dense markers" =
  let labels = List.init 50 ~f:(sprintf "d%d") in
  show
    ~selector:".ui-chart-xaxis"
    (chart
       Line
       (categories
          labels
          [ "v", List.init 50 ~f:(fun i -> Some (Float.of_int i)) ]));
  [%expect
    {|
    <div aria-hidden="true" class="ui-chart-xaxis">
      <span style="left: 1%; --w: 14%;" class="ui-chart-tick"> d0 </span>
      <span style="left: 15%; --w: 14%;" class="minor ui-chart-tick"> d7 </span>
      <span style="left: 29%; --w: 14%;" class="ui-chart-tick"> d14 </span>
      <span style="left: 43%; --w: 14%;" class="minor ui-chart-tick"> d21 </span>
      <span style="left: 57%; --w: 14%;" class="ui-chart-tick"> d28 </span>
      <span style="left: 71%; --w: 14%;" class="minor ui-chart-tick"> d35 </span>
      <span style="left: 85%; --w: 14%;" class="ui-chart-tick"> d42 </span>
      <span style="left: 99%; --w: 14%;" class="minor ui-chart-tick"> d49 </span>
    </div>
    |}];
  let c =
    chart
      Line
      (categories
         labels
         [ "v", List.init 50 ~f:(fun i -> Some (Float.of_int i)) ])
  in
  count "figure.dense" c;
  count ".ui-chart-col[tabindex]" c;
  [%expect
    {|
    1 figure.dense
    50 .ui-chart-col[tabindex]
    |}]
;;

let%expect_test "a single point, and gaps leave lone points marked" =
  show
    ~selector:".ui-chart-plot"
    (chart Line ~height:Small (categories [ "only" ] [ "v", [ Some 3. ] ]));
  [%expect
    {|
    <div class="ui-chart-plot">
      <div aria-hidden="true" class="ui-chart-grid">
        <span style="bottom: 0%;" class="ui-chart-gridline zero"> </span>
        <span style="bottom: 50%;" class="ui-chart-gridline"> </span>
        <span style="bottom: 100%;" class="ui-chart-gridline"> </span>
      </div>
      <svg class="ui-chart-svg"
           viewBox="0 0 100 100"
           aria-hidden="true"
           focusable="false"
           preserveAspectRatio="none">
        <path class="s1 ui-chart-line" d="M50 25"> </path>
      </svg>
      <div style="left: 0%; width: 100%;"
           tabindex="0"
           role="img"
           aria-label="only: 3"
           class="ui-chart-col">
        <span class="ui-chart-guide"> </span>
        <span style="bottom: 75%;" class="lone s1 ui-chart-dot"> </span>
      </div>
      <div aria-hidden="true" style="--x: 50%;" class="left ui-chart-tip">
        <div class="ui-chart-tip-head"> only </div>
        <div class="s1 ui-chart-tip-row">
          <span class="ui-chart-swatch"> </span>
          <span class="ui-chart-name"> v </span>
          <span class="ui-chart-value"> 3 </span>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "nothing to chart" =
  show (chart Line (categories [] []));
  show (chart Bar (categories [ "a"; "b" ] [ "v", [ None; None ] ]));
  show (chart Pie (Slices [ { label = "none"; value = 0. } ]));
  show (chart Scatter (Points []));
  [%expect
    {|
    <figure data-kind="line" role="figure" aria-label="Line chart" class="h-medium ui-chart">
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <div class="ui-chart-empty"> No data to chart </div>
    </figure>
    <figure data-kind="bar" role="figure" aria-label="Bar chart" class="h-medium ui-chart">
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <div class="ui-chart-empty"> No data to chart </div>
    </figure>
    <figure data-kind="pie" role="figure" aria-label="Pie chart" class="h-medium ui-chart">
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <div class="ui-chart-empty"> No data to chart </div>
    </figure>
    <figure data-kind="scatter" role="figure" aria-label="Scatter chart" class="h-medium ui-chart">
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <div class="ui-chart-empty"> No data to chart </div>
    </figure>
    |}]
;;

let%expect_test "donut: slices, the total in the hole, legend with shares" =
  show
    (chart
       Donut
       ~title:"Disk"
       ~unit:"GB"
       (Slices
          [ { label = "Images"; value = 30. }
          ; { label = "Code"; value = 10. }
          ; { label = "Empty"; value = 0. }
          ]));
  [%expect
    {|
    <figure data-kind="donut" role="figure" aria-label="Donut chart: Disk" class="h-medium ui-chart">
      <div class="ui-title"> Disk </div>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <div class="ui-chart-round">
        <div class="ui-chart-disc">
          <svg class="ui-chart-svg" viewBox="0 0 100 100" role="group">
            <g class="n1 s1 ui-chart-slice" tabindex="0" role="img" aria-label="Images: 30 GB (75.0%)">
              <path d="M 50 2 A 48 48 0 1 1 2 50 L 20 50 A 30 30 0 1 0 50 20 Z"> </path>
            </g>
            <g class="n2 s2 ui-chart-slice" tabindex="0" role="img" aria-label="Code: 10 GB (25.0%)">
              <path d="M 2 50 A 48 48 0 0 1 50 2 L 50 20 A 30 30 0 0 0 20 50 Z"> </path>
            </g>
          </svg>
          <div aria-hidden="true" class="ui-chart-centre">
            <div class="ui-chart-total">
              <span class="ui-chart-value"> 40 GB </span>
              <span class="ui-chart-name"> Total </span>
            </div>
            <div class="n1 s1 ui-chart-centre-item">
              <span class="ui-chart-name"> Images </span>
              <span class="ui-chart-value"> 30 GB </span>
              <span class="ui-chart-share"> 75.0% </span>
            </div>
            <div class="n2 s2 ui-chart-centre-item">
              <span class="ui-chart-name"> Code </span>
              <span class="ui-chart-value"> 10 GB </span>
              <span class="ui-chart-share"> 25.0% </span>
            </div>
            <div class="n3 s3 ui-chart-centre-item">
              <span class="ui-chart-name"> Empty </span>
              <span class="ui-chart-value"> 0 GB </span>
              <span class="ui-chart-share"> 0.0% </span>
            </div>
          </div>
        </div>
        <ul aria-hidden="true" class="ui-chart-legend">
          <li class="n1 s1">
            <span class="ui-chart-swatch"> </span>
            <span class="ui-chart-name"> Images </span>
            <span class="ui-chart-value"> 30 GB </span>
            <span class="ui-chart-share"> 75.0% </span>
          </li>
          <li class="n2 s2">
            <span class="ui-chart-swatch"> </span>
            <span class="ui-chart-name"> Code </span>
            <span class="ui-chart-value"> 10 GB </span>
            <span class="ui-chart-share"> 25.0% </span>
          </li>
          <li class="n3 s3">
            <span class="ui-chart-swatch"> </span>
            <span class="ui-chart-name"> Empty </span>
            <span class="ui-chart-value"> 0 GB </span>
            <span class="ui-chart-share"> 0.0% </span>
          </li>
        </ul>
      </div>
      <table class="sr-only">
        <caption> Disk </caption>
        <thead>
          <tr>
            <th>  </th>
            <th> Value </th>
            <th> Share </th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td> Images </td>
            <td> 30 GB </td>
            <td> 75.0% </td>
          </tr>
          <tr>
            <td> Code </td>
            <td> 10 GB </td>
            <td> 25.0% </td>
          </tr>
          <tr>
            <td> Empty </td>
            <td> 0 GB </td>
            <td> 0.0% </td>
          </tr>
        </tbody>
      </table>
    </figure>
    |}]
;;

let%expect_test "pie: past eight slices the colours come round lighter" =
  show
    ~selector:".ui-chart-legend"
    (chart
       Pie
       (Slices
          (List.init 10 ~f:(fun i ->
             { C.Slice.label = sprintf "s%d" i; value = 1. }))));
  [%expect
    {|
    <ul aria-hidden="true" class="ui-chart-legend">
      <li class="n1 s1">
        <span class="ui-chart-swatch"> </span>
        <span class="ui-chart-name"> s0 </span>
        <span class="ui-chart-value"> 1 </span>
        <span class="ui-chart-share"> 10.0% </span>
      </li>
      <li class="n2 s2">
        <span class="ui-chart-swatch"> </span>
        <span class="ui-chart-name"> s1 </span>
        <span class="ui-chart-value"> 1 </span>
        <span class="ui-chart-share"> 10.0% </span>
      </li>
      <li class="n3 s3">
        <span class="ui-chart-swatch"> </span>
        <span class="ui-chart-name"> s2 </span>
        <span class="ui-chart-value"> 1 </span>
        <span class="ui-chart-share"> 10.0% </span>
      </li>
      <li class="n4 s4">
        <span class="ui-chart-swatch"> </span>
        <span class="ui-chart-name"> s3 </span>
        <span class="ui-chart-value"> 1 </span>
        <span class="ui-chart-share"> 10.0% </span>
      </li>
      <li class="n5 s5">
        <span class="ui-chart-swatch"> </span>
        <span class="ui-chart-name"> s4 </span>
        <span class="ui-chart-value"> 1 </span>
        <span class="ui-chart-share"> 10.0% </span>
      </li>
      <li class="n6 s6">
        <span class="ui-chart-swatch"> </span>
        <span class="ui-chart-name"> s5 </span>
        <span class="ui-chart-value"> 1 </span>
        <span class="ui-chart-share"> 10.0% </span>
      </li>
      <li class="n7 s7">
        <span class="ui-chart-swatch"> </span>
        <span class="ui-chart-name"> s6 </span>
        <span class="ui-chart-value"> 1 </span>
        <span class="ui-chart-share"> 10.0% </span>
      </li>
      <li class="n8 s8">
        <span class="ui-chart-swatch"> </span>
        <span class="ui-chart-name"> s7 </span>
        <span class="ui-chart-value"> 1 </span>
        <span class="ui-chart-share"> 10.0% </span>
      </li>
      <li class="alt n9 s1">
        <span class="ui-chart-swatch"> </span>
        <span class="ui-chart-name"> s8 </span>
        <span class="ui-chart-value"> 1 </span>
        <span class="ui-chart-share"> 10.0% </span>
      </li>
      <li class="alt n10 s2">
        <span class="ui-chart-swatch"> </span>
        <span class="ui-chart-name"> s9 </span>
        <span class="ui-chart-value"> 1 </span>
        <span class="ui-chart-share"> 10.0% </span>
      </li>
    </ul>
    |}]
;;

let%expect_test "scatter: points with tooltips, both axes" =
  show
    (chart
       Scatter
       ~x_label:"Size"
       ~y_label:"Time"
       ~unit:"ms"
       (Points
          [ { name = "runs"; points = [ 1., 10.; 4., 30. ] }
          ; { name = "cached"; points = [ 2., 2. ] }
          ]));
  [%expect
    {|
    <figure data-kind="scatter" role="figure" aria-label="Scatter chart" class="h-medium ui-chart">
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <div aria-hidden="true" class="ui-chart-yname"> Time (ms) </div>
      <div style="--gutter: 3ch;" class="ui-chart-frame">
        <div aria-hidden="true" class="ui-chart-yaxis">
          <span style="bottom: 0%;" class="ui-chart-tick"> 0 </span>
          <span style="bottom: 33.33%;" class="ui-chart-tick"> 10 </span>
          <span style="bottom: 66.67%;" class="ui-chart-tick"> 20 </span>
          <span style="bottom: 100%;" class="ui-chart-tick"> 30 </span>
        </div>
        <div class="ui-chart-plot">
          <div aria-hidden="true" class="ui-chart-grid">
            <span style="bottom: 0%;" class="ui-chart-gridline zero"> </span>
            <span style="bottom: 33.33%;" class="ui-chart-gridline"> </span>
            <span style="bottom: 66.67%;" class="ui-chart-gridline"> </span>
            <span style="bottom: 100%;" class="ui-chart-gridline"> </span>
          </div>
          <div aria-hidden="true" class="ui-chart-vgrid">
            <div aria-hidden="true" class="ui-chart-grid">
              <span style="left: 0%;" class="ui-chart-gridline zero"> </span>
              <span style="left: 25%;" class="ui-chart-gridline"> </span>
              <span style="left: 50%;" class="ui-chart-gridline"> </span>
              <span style="left: 75%;" class="ui-chart-gridline"> </span>
              <span style="left: 100%;" class="ui-chart-gridline"> </span>
            </div>
          </div>
          <span style="left: 25%; bottom: 33.33%;"
                tabindex="0"
                role="img"
                aria-label="runs: Size 1, Time 10 ms"
                class="s1 ui-chart-pt">
            <div aria-hidden="true" class="above right ui-chart-tip">
              <div class="ui-chart-tip-head"> runs </div>
              <div class="plain ui-chart-tip-row">
                <span class="ui-chart-swatch"> </span>
                <span class="ui-chart-name"> Size </span>
                <span class="ui-chart-value"> 1 </span>
              </div>
              <div class="plain ui-chart-tip-row">
                <span class="ui-chart-swatch"> </span>
                <span class="ui-chart-name"> Time </span>
                <span class="ui-chart-value"> 10 ms </span>
              </div>
            </div>
          </span>
          <span style="left: 100%; bottom: 100%;"
                tabindex="0"
                role="img"
                aria-label="runs: Size 4, Time 30 ms"
                class="s1 ui-chart-pt">
            <div aria-hidden="true" class="below left ui-chart-tip">
              <div class="ui-chart-tip-head"> runs </div>
              <div class="plain ui-chart-tip-row">
                <span class="ui-chart-swatch"> </span>
                <span class="ui-chart-name"> Size </span>
                <span class="ui-chart-value"> 4 </span>
              </div>
              <div class="plain ui-chart-tip-row">
                <span class="ui-chart-swatch"> </span>
                <span class="ui-chart-name"> Time </span>
                <span class="ui-chart-value"> 30 ms </span>
              </div>
            </div>
          </span>
          <span style="left: 50%; bottom: 6.67%;"
                tabindex="0"
                role="img"
                aria-label="cached: Size 2, Time 2 ms"
                class="s2 ui-chart-pt">
            <div aria-hidden="true" class="above left ui-chart-tip">
              <div class="ui-chart-tip-head"> cached </div>
              <div class="plain ui-chart-tip-row">
                <span class="ui-chart-swatch"> </span>
                <span class="ui-chart-name"> Size </span>
                <span class="ui-chart-value"> 2 </span>
              </div>
              <div class="plain ui-chart-tip-row">
                <span class="ui-chart-swatch"> </span>
                <span class="ui-chart-name"> Time </span>
                <span class="ui-chart-value"> 2 ms </span>
              </div>
            </div>
          </span>
        </div>
        <div aria-hidden="true" class="ui-chart-values ui-chart-xaxis">
          <span style="left: 0%;" class="ui-chart-tick"> 0 </span>
          <span style="left: 25%;" class="minor ui-chart-tick"> 1 </span>
          <span style="left: 50%;" class="ui-chart-tick"> 2 </span>
          <span style="left: 75%;" class="minor ui-chart-tick"> 3 </span>
          <span style="left: 100%;" class="ui-chart-tick"> 4 </span>
        </div>
      </div>
      <div aria-hidden="true" class="ui-chart-xname"> Size </div>
      <ul aria-hidden="true" class="ui-chart-legend">
        <li class="s1">
          <span class="ui-chart-swatch"> </span>
          <span class="ui-chart-name"> runs </span>
        </li>
        <li class="s2">
          <span class="ui-chart-swatch"> </span>
          <span class="ui-chart-name"> cached </span>
        </li>
      </ul>
      <table class="sr-only">
        <caption> Scatter chart </caption>
        <thead>
          <tr>
            <th>  </th>
            <th> Size </th>
            <th> Time </th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td> runs </td>
            <td> 1 </td>
            <td> 10 ms </td>
          </tr>
          <tr>
            <td> runs </td>
            <td> 4 </td>
            <td> 30 ms </td>
          </tr>
          <tr>
            <td> cached </td>
            <td> 2 </td>
            <td> 2 ms </td>
          </tr>
        </tbody>
      </table>
    </figure>
    |}]
;;

let%expect_test "scatter: too many points to focus each" =
  let c =
    chart
      Scatter
      (Points
         [ { name = "p"
           ; points = List.init 301 ~f:(fun i -> Float.of_int i, 1.)
           }
         ])
  in
  count ".ui-chart-pt" c;
  count ".ui-chart-pt[tabindex]" c;
  count "tbody tr" c;
  [%expect
    {|
    301 .ui-chart-pt
    0 .ui-chart-pt[tabindex]
    300 tbody tr
    |}]
;;
