open! Core
module H = Harness

(* Every component render_ui has, as the model sends them (e2e/ui.mjs shows
   the same call). *)
let components =
  {|{"components":[{"type":"callout","tone":"info","title":"Build health","text":"Builds got **18% faster** since `dune 3.22`."},{"type":"grid","columns":3,"children":[{"type":"stat","label":"Median build","value":42.5,"unit":"s","delta":"-18%","trend":"down","good":"down","caption":"last 30 days","spark":[60,58,55,51,49,47,44,42.5]},{"type":"stat","label":"Pass rate","value":"97.2%","delta":"+1.4","trend":"up","good":"up"},{"type":"stat","label":"Flaky tests","value":3,"trend":"flat"}]},{"type":"chart","kind":"line","title":"Build times by week","unit":"s","labels":["W1","W2","W3","W4","W5","W6"],"series":[{"name":"main","values":[61,58,null,51,47,44]},{"name":"PRs","values":[70,66,63,60,55,52]}]},{"type":"card","title":"Breakdown","subtitle":"Where the time goes","children":[{"type":"chart","kind":"donut","slices":[{"label":"Compile","value":24},{"label":"Link","value":9},{"label":"Tests","value":12}]},{"type":"progress","label":"Coverage","value":78,"tone":"success","caption":"target 80%"}]},{"type":"table","id":"slow","title":"Slowest tests","columns":[{"key":"name","label":"Test"},{"key":"time","label":"Time","format":"number","decimals":1},{"key":"share","label":"Share","format":"percent"},{"key":"ok","label":"Passing"}],"rows":[{"name":"e2e/mobile","time":31.2,"share":0.21,"ok":true},{"name":"tmux/agents","time":18.04,"share":0.12,"ok":false},{"name":"web/test","time":9.5,"share":0.064,"ok":true}]},{"type":"tabs","tabs":[{"label":"Steps","children":[{"type":"steps","items":[{"title":"Profile","status":"done"},{"title":"Cache deps","status":"current","text":"Use `nix` binary cache"},{"title":"Split e2e","status":"todo"}]}]},{"label":"Timeline","children":[{"type":"timeline","items":[{"title":"Started","time":"09:12"},{"title":"Regression found","time":"10:40","tone":"danger","text":"in `tmux` tests"}]}]}]},{"type":"kv","title":"Machine","items":[{"key":"CPU","value":"16 cores"},{"key":"OS","value":"NixOS"}]},{"type":"badges","items":[{"text":"ocaml"},{"text":"passing","tone":"success"},{"text":"flaky","tone":"warning"}]},{"type":"calculator","title":"Cache savings","inputs":[{"name":"builds","label":"Builds a day","default":40,"min":0,"max":200,"step":1,"slider":true},{"name":"minutes","label":"Minutes saved","default":1.5,"unit":"min"}],"outputs":[{"label":"Hours a month","expr":"builds * minutes * 30 / 60","decimals":1,"unit":"h"},{"label":"Worth it","expr":"builds * minutes > 30 ? 1 : 0","format":"integer"}]},{"type":"quiz","questions":[{"question":"Which tool formats OCaml?","options":["ocamlformat","prettier"],"answer":0,"explanation":"`ocamlformat`."}]},{"type":"flashcards","cards":[{"front":"What is **Eio**?","back":"Effects-based IO for OCaml 5."},{"front":"Bonsai?","back":"Incremental UI."}]},{"type":"choices","id":"next","prompt":"What next?","options":[{"label":"Cache deps","description":"fastest win"},{"label":"Split e2e","tone":"warning"}]},{"type":"form","id":"deploy","title":"Deploy options","fields":[{"name":"env","label":"Environment","kind":"select","options":["staging","prod"],"default":"staging"},{"name":"replicas","label":"Replicas","kind":"number","default":2,"min":1,"max":9},{"name":"notify","label":"Notify team","kind":"toggle","default":true},{"name":"note","label":"Note","kind":"textarea","placeholder":"Optional"}],"submit":"Deploy"},{"type":"checklist","title":"Before release","items":[{"text":"Run `./test --full`","checked":true},{"text":"Update docs"}]},{"type":"accordion","items":[{"title":"Code","open":true,"children":[{"type":"code","lang":"sh","code":"dune build @runtest"}]},{"title":"Diff","children":[{"type":"diff","diff":"--- a/x\n+++ b/x\n@@ -1 +1 @@\n-old\n+new\n"}]}]},{"type":"tree","entries":[{"path":"frontend/"},{"path":"frontend/web/lib/ui_view.ml","note":"new","tone":"success"},{"path":"backend/"}]},{"type":"divider"},{"type":"markdown","text":"That's all \u2014 see the **table** above."},{"type":"sparkle"}]}|}
;;

let call ?(id = "u1") arguments =
  sprintf
    {|{"id":"%s","name":"render_ui","arguments":%s}|}
    id
    (Jsonaf.to_string (`String arguments))
;;

let assistant ?(id = "u1") arguments =
  sprintf
    {|{"role":"assistant","content":[{"type":"tool_call","id":"%s","name":"render_ui","arguments":%s}],"stop_reason":{"type":"tool_use"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}|}
    id
    (Jsonaf.to_string (`String arguments))
;;

let result ?(id = "u1") ?(is_error = false) text =
  sprintf
    {|{"role":"tool_result","tool_call_id":"%s","tool_name":"render_ui","text":%s,"is_error":%b}|}
    id
    (Jsonaf.to_string (`String text))
    is_error
;;

(* The model shows [arguments]; the backend accepts them. *)
let shown ?id ?(finish = true) h arguments =
  H.event h {|{"event":"agent_start"}|};
  H.event h (sprintf {|{"event":"message_end","message":%s}|} (assistant ?id arguments));
  H.event h (sprintf {|{"event":"tool_start","call":%s}|} (call ?id arguments));
  H.event
    h
    (sprintf
       {|{"event":"tool_end","call":%s,"result":%s}|}
       (call ?id arguments)
       (result ?id "Shown."));
  if finish then H.event h {|{"event":"agent_end","messages":[]}|}
;;

let one json = sprintf {|{"components":[%s]}|} json

let%expect_test "every component" =
  let h = H.create () in
  shown h components;
  H.show h ~selector:".tool-ui";
  [%expect {|
    <div data-call="u1" class="tool-ui">
      <div class="genui ui-stack">
        <div role="note" class="tone-info ui-callout">
          <div class="ui-callout-title"> Build health </div>
          <div class="ui-md">
            <div class="markdown">
              <div class="md-part">
                <p dir="auto">
                  Builds got
                  <strong> 18% faster </strong>
                   since
                  <code> dune 3.22 </code>
                  .
                </p>
              </div>
            </div>
          </div>
        </div>
        <div class="ui-grid" style={ --cols: 3; }>
          <div class="ui-stat">
            <div class="ui-stat-label"> Median build </div>
            <div class="ui-stat-row">
              <span class="ui-stat-value">
                42.5
                <span class="ui-stat-unit"> s </span>
              </span>
              <span class="good ui-delta"> ↓ -18% </span>
            </div>
            <svg class="ui-spark"
                 viewBox="0 0 100 32"
                 aria-hidden="true"
                 focusable="false"
                 preserveAspectRatio="none">
              <path class="ui-spark-area"
                    d="M0 2 L14.29 5.2 L28.57 10 L42.86 16.4 L57.14 19.6 L71.43 22.8 L85.71 27.6 L100 30 L 100 32 L 0 32 Z"> </path>
              <path class="ui-spark-line"
                    d="M0 2 L14.29 5.2 L28.57 10 L42.86 16.4 L57.14 19.6 L71.43 22.8 L85.71 27.6 L100 30"
                    vector-effect="non-scaling-stroke"> </path>
            </svg>
            <div class="ui-md ui-stat-caption">
              <div class="markdown">
                <div class="md-part">
                  <p dir="auto"> last 30 days </p>
                </div>
              </div>
            </div>
          </div>
          <div class="ui-stat">
            <div class="ui-stat-label"> Pass rate </div>
            <div class="ui-stat-row">
              <span class="ui-stat-value">
                97.2%
                <Vdom.Node.none-widget> </Vdom.Node.none-widget>
              </span>
              <span class="good ui-delta"> ↑ +1.4 </span>
            </div>
            <Vdom.Node.none-widget> </Vdom.Node.none-widget>
            <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          </div>
          <div class="ui-stat">
            <div class="ui-stat-label"> Flaky tests </div>
            <div class="ui-stat-row">
              <span class="ui-stat-value">
                3
                <Vdom.Node.none-widget> </Vdom.Node.none-widget>
              </span>
              <Vdom.Node.none-widget> </Vdom.Node.none-widget>
            </div>
            <Vdom.Node.none-widget> </Vdom.Node.none-widget>
            <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          </div>
        </div>
        <figure data-kind="line"
                role="figure"
                aria-label="Line chart: Build times by week"
                class="h-medium ui-chart">
          <div class="ui-title"> Build times by week </div>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          <div aria-hidden="true" class="ui-chart-yname"> s </div>
          <div class="ui-chart-frame" style={ --gutter: 3ch; --rows: 6; }>
            <div aria-hidden="true" class="ui-chart-yaxis">
              <span class="ui-chart-tick" style={ bottom: 0%; }> 40 </span>
              <span class="ui-chart-tick" style={ bottom: 16.67%; }> 45 </span>
              <span class="ui-chart-tick" style={ bottom: 33.33%; }> 50 </span>
              <span class="ui-chart-tick" style={ bottom: 50%; }> 55 </span>
              <span class="ui-chart-tick" style={ bottom: 66.67%; }> 60 </span>
              <span class="ui-chart-tick" style={ bottom: 83.33%; }> 65 </span>
              <span class="ui-chart-tick" style={ bottom: 100%; }> 70 </span>
            </div>
            <div class="ui-chart-plot">
              <div aria-hidden="true" class="ui-chart-grid">
                <span class="ui-chart-gridline" style={ bottom: 0%; }> </span>
                <span class="ui-chart-gridline" style={ bottom: 16.67%; }> </span>
                <span class="ui-chart-gridline" style={ bottom: 33.33%; }> </span>
                <span class="ui-chart-gridline" style={ bottom: 50%; }> </span>
                <span class="ui-chart-gridline" style={ bottom: 66.67%; }> </span>
                <span class="ui-chart-gridline" style={ bottom: 83.33%; }> </span>
                <span class="ui-chart-gridline" style={ bottom: 100%; }> </span>
              </div>
              <svg class="ui-chart-svg"
                   viewBox="0 0 100 100"
                   aria-hidden="true"
                   focusable="false"
                   preserveAspectRatio="none">
                <path class="s1 ui-chart-line" d="M8.33 30 L25 40 M58.33 63.33 L75 76.67 L91.67 86.67"> </path>
                <path class="s2 ui-chart-line"
                      d="M8.33 0 L25 13.33 L41.67 23.33 L58.33 33.33 L75 50 L91.67 60"> </path>
              </svg>
              <div tabindex="0"
                   role="img"
                   aria-label="W1: main 61 s, PRs 70 s"
                   class="ui-chart-col"
                   style={
                     left: 0%;
                     width: 16.67%;
                   }>
                <span class="ui-chart-guide"> </span>
                <span class="s1 ui-chart-dot" style={ bottom: 70%; }> </span>
                <span class="s2 ui-chart-dot" style={ bottom: 100%; }> </span>
              </div>
              <div aria-hidden="true" class="right ui-chart-tip" style={ --x: 8.33%; }>
                <div class="ui-chart-tip-head"> W1 </div>
                <div class="s1 ui-chart-tip-row">
                  <span class="ui-chart-swatch"> </span>
                  <span class="ui-chart-name"> main </span>
                  <span class="ui-chart-value"> 61 s </span>
                </div>
                <div class="s2 ui-chart-tip-row">
                  <span class="ui-chart-swatch"> </span>
                  <span class="ui-chart-name"> PRs </span>
                  <span class="ui-chart-value"> 70 s </span>
                </div>
              </div>
              <div tabindex="0"
                   role="img"
                   aria-label="W2: main 58 s, PRs 66 s"
                   class="ui-chart-col"
                   style={
                     left: 16.67%;
                     width: 16.67%;
                   }>
                <span class="ui-chart-guide"> </span>
                <span class="s1 ui-chart-dot" style={ bottom: 60%; }> </span>
                <span class="s2 ui-chart-dot" style={ bottom: 86.67%; }> </span>
              </div>
              <div aria-hidden="true" class="right ui-chart-tip" style={ --x: 25%; }>
                <div class="ui-chart-tip-head"> W2 </div>
                <div class="s1 ui-chart-tip-row">
                  <span class="ui-chart-swatch"> </span>
                  <span class="ui-chart-name"> main </span>
                  <span class="ui-chart-value"> 58 s </span>
                </div>
                <div class="s2 ui-chart-tip-row">
                  <span class="ui-chart-swatch"> </span>
                  <span class="ui-chart-name"> PRs </span>
                  <span class="ui-chart-value"> 66 s </span>
                </div>
              </div>
              <div tabindex="0"
                   role="img"
                   aria-label="W3: main no value, PRs 63 s"
                   class="ui-chart-col"
                   style={
                     left: 33.33%;
                     width: 16.67%;
                   }>
                <span class="ui-chart-guide"> </span>
                <span class="s2 ui-chart-dot" style={ bottom: 76.67%; }> </span>
              </div>
              <div aria-hidden="true" class="right ui-chart-tip" style={ --x: 41.67%; }>
                <div class="ui-chart-tip-head"> W3 </div>
                <div class="s1 ui-chart-tip-row">
                  <span class="ui-chart-swatch"> </span>
                  <span class="ui-chart-name"> main </span>
                  <span class="ui-chart-value"> — </span>
                </div>
                <div class="s2 ui-chart-tip-row">
                  <span class="ui-chart-swatch"> </span>
                  <span class="ui-chart-name"> PRs </span>
                  <span class="ui-chart-value"> 63 s </span>
                </div>
              </div>
              <div tabindex="0"
                   role="img"
                   aria-label="W4: main 51 s, PRs 60 s"
                   class="ui-chart-col"
                   style={
                     left: 50%;
                     width: 16.67%;
                   }>
                <span class="ui-chart-guide"> </span>
                <span class="s1 ui-chart-dot" style={ bottom: 36.67%; }> </span>
                <span class="s2 ui-chart-dot" style={ bottom: 66.67%; }> </span>
              </div>
              <div aria-hidden="true" class="left ui-chart-tip" style={ --x: 58.33%; }>
                <div class="ui-chart-tip-head"> W4 </div>
                <div class="s1 ui-chart-tip-row">
                  <span class="ui-chart-swatch"> </span>
                  <span class="ui-chart-name"> main </span>
                  <span class="ui-chart-value"> 51 s </span>
                </div>
                <div class="s2 ui-chart-tip-row">
                  <span class="ui-chart-swatch"> </span>
                  <span class="ui-chart-name"> PRs </span>
                  <span class="ui-chart-value"> 60 s </span>
                </div>
              </div>
              <div tabindex="0"
                   role="img"
                   aria-label="W5: main 47 s, PRs 55 s"
                   class="ui-chart-col"
                   style={
                     left: 66.67%;
                     width: 16.67%;
                   }>
                <span class="ui-chart-guide"> </span>
                <span class="s1 ui-chart-dot" style={ bottom: 23.33%; }> </span>
                <span class="s2 ui-chart-dot" style={ bottom: 50%; }> </span>
              </div>
              <div aria-hidden="true" class="left ui-chart-tip" style={ --x: 75%; }>
                <div class="ui-chart-tip-head"> W5 </div>
                <div class="s1 ui-chart-tip-row">
                  <span class="ui-chart-swatch"> </span>
                  <span class="ui-chart-name"> main </span>
                  <span class="ui-chart-value"> 47 s </span>
                </div>
                <div class="s2 ui-chart-tip-row">
                  <span class="ui-chart-swatch"> </span>
                  <span class="ui-chart-name"> PRs </span>
                  <span class="ui-chart-value"> 55 s </span>
                </div>
              </div>
              <div tabindex="0"
                   role="img"
                   aria-label="W6: main 44 s, PRs 52 s"
                   class="ui-chart-col"
                   style={
                     left: 83.33%;
                     width: 16.67%;
                   }>
                <span class="ui-chart-guide"> </span>
                <span class="s1 ui-chart-dot" style={ bottom: 13.33%; }> </span>
                <span class="s2 ui-chart-dot" style={ bottom: 40%; }> </span>
              </div>
              <div aria-hidden="true" class="left ui-chart-tip" style={ --x: 91.67%; }>
                <div class="ui-chart-tip-head"> W6 </div>
                <div class="s1 ui-chart-tip-row">
                  <span class="ui-chart-swatch"> </span>
                  <span class="ui-chart-name"> main </span>
                  <span class="ui-chart-value"> 44 s </span>
                </div>
                <div class="s2 ui-chart-tip-row">
                  <span class="ui-chart-swatch"> </span>
                  <span class="ui-chart-name"> PRs </span>
                  <span class="ui-chart-value"> 52 s </span>
                </div>
              </div>
            </div>
            <div aria-hidden="true" class="ui-chart-xaxis">
              <span class="ui-chart-tick" style={ left: 8.33%; --w: 16.67%; }> W1 </span>
              <span class="minor ui-chart-tick" style={ left: 25%; --w: 16.67%; }> W2 </span>
              <span class="ui-chart-tick" style={ left: 41.67%; --w: 16.67%; }> W3 </span>
              <span class="minor ui-chart-tick" style={ left: 58.33%; --w: 16.67%; }> W4 </span>
              <span class="ui-chart-tick" style={ left: 75%; --w: 16.67%; }> W5 </span>
              <span class="minor ui-chart-tick" style={ left: 91.67%; --w: 16.67%; }> W6 </span>
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
              <span class="ui-chart-name"> PRs </span>
            </li>
          </ul>
          <table class="sr-only">
            <caption> Build times by week </caption>
            <thead>
              <tr>
                <th>  </th>
                <th> main </th>
                <th> PRs </th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td> W1 </td>
                <td> 61 s </td>
                <td> 70 s </td>
              </tr>
              <tr>
                <td> W2 </td>
                <td> 58 s </td>
                <td> 66 s </td>
              </tr>
              <tr>
                <td> W3 </td>
                <td>  </td>
                <td> 63 s </td>
              </tr>
              <tr>
                <td> W4 </td>
                <td> 51 s </td>
                <td> 60 s </td>
              </tr>
              <tr>
                <td> W5 </td>
                <td> 47 s </td>
                <td> 55 s </td>
              </tr>
              <tr>
                <td> W6 </td>
                <td> 44 s </td>
                <td> 52 s </td>
              </tr>
            </tbody>
          </table>
        </figure>
        <section class="ui-card">
          <header>
            <h3 class="ui-card-title"> Breakdown </h3>
            <div class="ui-md ui-sub">
              <div class="markdown">
                <div class="md-part">
                  <p dir="auto"> Where the time goes </p>
                </div>
              </div>
            </div>
          </header>
          <div class="ui-stack">
            <figure data-kind="donut" role="figure" aria-label="Donut chart" class="h-medium ui-chart">
              <Vdom.Node.none-widget> </Vdom.Node.none-widget>
              <Vdom.Node.none-widget> </Vdom.Node.none-widget>
              <div class="ui-chart-round">
                <div class="ui-chart-disc">
                  <svg class="ui-chart-svg" viewBox="0 0 100 100" role="group">
                    <g class="n1 s1 ui-chart-slice"
                       tabindex="0"
                       role="img"
                       aria-label="Compile: 24 (53.3%)">
                      <path d="M 50 2 A 48 48 0 1 1 40.02 96.95 L 43.76 79.34 A 30 30 0 1 0 50 20 Z"> </path>
                    </g>
                    <g class="n2 s2 ui-chart-slice"
                       tabindex="0"
                       role="img"
                       aria-label="Link: 9 (20.0%)">
                      <path d="M 40.02 96.95 A 48 48 0 0 1 2.26 55.02 L 20.16 53.14 A 30 30 0 0 0 43.76 79.34 Z"> </path>
                    </g>
                    <g class="n3 s3 ui-chart-slice"
                       tabindex="0"
                       role="img"
                       aria-label="Tests: 12 (26.7%)">
                      <path d="M 2.26 55.02 A 48 48 0 0 1 50 2 L 50 20 A 30 30 0 0 0 20.16 53.14 Z"> </path>
                    </g>
                  </svg>
                  <div aria-hidden="true" class="ui-chart-centre">
                    <div class="ui-chart-total">
                      <span class="ui-chart-value"> 45 </span>
                      <span class="ui-chart-name"> Total </span>
                    </div>
                    <div class="n1 s1 ui-chart-centre-item">
                      <span class="ui-chart-name"> Compile </span>
                      <span class="ui-chart-value"> 24 </span>
                      <span class="ui-chart-share"> 53.3% </span>
                    </div>
                    <div class="n2 s2 ui-chart-centre-item">
                      <span class="ui-chart-name"> Link </span>
                      <span class="ui-chart-value"> 9 </span>
                      <span class="ui-chart-share"> 20.0% </span>
                    </div>
                    <div class="n3 s3 ui-chart-centre-item">
                      <span class="ui-chart-name"> Tests </span>
                      <span class="ui-chart-value"> 12 </span>
                      <span class="ui-chart-share"> 26.7% </span>
                    </div>
                  </div>
                </div>
                <ul aria-hidden="true" class="ui-chart-legend">
                  <li class="n1 s1">
                    <span class="ui-chart-swatch"> </span>
                    <span class="ui-chart-name"> Compile </span>
                    <span class="ui-chart-value"> 24 </span>
                    <span class="ui-chart-share"> 53.3% </span>
                  </li>
                  <li class="n2 s2">
                    <span class="ui-chart-swatch"> </span>
                    <span class="ui-chart-name"> Link </span>
                    <span class="ui-chart-value"> 9 </span>
                    <span class="ui-chart-share"> 20.0% </span>
                  </li>
                  <li class="n3 s3">
                    <span class="ui-chart-swatch"> </span>
                    <span class="ui-chart-name"> Tests </span>
                    <span class="ui-chart-value"> 12 </span>
                    <span class="ui-chart-share"> 26.7% </span>
                  </li>
                </ul>
              </div>
              <table class="sr-only">
                <caption> Donut chart </caption>
                <thead>
                  <tr>
                    <th>  </th>
                    <th> Value </th>
                    <th> Share </th>
                  </tr>
                </thead>
                <tbody>
                  <tr>
                    <td> Compile </td>
                    <td> 24 </td>
                    <td> 53.3% </td>
                  </tr>
                  <tr>
                    <td> Link </td>
                    <td> 9 </td>
                    <td> 20.0% </td>
                  </tr>
                  <tr>
                    <td> Tests </td>
                    <td> 12 </td>
                    <td> 26.7% </td>
                  </tr>
                </tbody>
              </table>
            </figure>
            <div class="tone-success ui-progress">
              <div class="ui-progress-head">
                <span class="ui-progress-label"> Coverage </span>
                <span class="ui-progress-value"> 78% </span>
              </div>
              <div role="progressbar"
                   aria-valuemin="0"
                   aria-valuemax="100"
                   aria-valuenow="78"
                   aria-label="Coverage"
                   class="ui-bar">
                <div class="ui-bar-fill" style={ width: 78.00%; }> </div>
              </div>
              <div class="ui-caption ui-md">
                <div class="markdown">
                  <div class="md-part">
                    <p dir="auto"> target 80% </p>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </section>
        <figure class="ui-table">
          <div class="ui-title"> Slowest tests </div>
          <div tabindex="0" role="region" aria-label="Slowest tests" class="ui-table-scroll">
            <table>
              <thead>
                <tr>
                  <th scope="col" aria-sort="none" class="left">
                    <button type="button" title="Sort by Test" class="ui-sort" @on_click>
                      <span> Test </span>
                      <span aria-hidden="true" class="ui-sort-mark"> ▲ </span>
                    </button>
                  </th>
                  <th scope="col" aria-sort="none" class="right">
                    <button type="button" title="Sort by Time" class="ui-sort" @on_click>
                      <span> Time </span>
                      <span aria-hidden="true" class="ui-sort-mark"> ▲ </span>
                    </button>
                  </th>
                  <th scope="col" aria-sort="none" class="right">
                    <button type="button" title="Sort by Share" class="ui-sort" @on_click>
                      <span> Share </span>
                      <span aria-hidden="true" class="ui-sort-mark"> ▲ </span>
                    </button>
                  </th>
                  <th scope="col" aria-sort="none" class="left">
                    <button type="button" title="Sort by Passing" class="ui-sort" @on_click>
                      <span> Passing </span>
                      <span aria-hidden="true" class="ui-sort-mark"> ▲ </span>
                    </button>
                  </th>
                </tr>
              </thead>
              <tbody>
                <tr>
                  <td class="left"> e2e/mobile </td>
                  <td class="right"> 31.2 </td>
                  <td class="right"> 21% </td>
                  <td class="left">
                    <span aria-label="yes" class="ui-yes"> ✓ </span>
                  </td>
                </tr>
                <tr>
                  <td class="left"> tmux/agents </td>
                  <td class="right"> 18.0 </td>
                  <td class="right"> 12% </td>
                  <td class="left">
                    <span aria-label="no" class="ui-no"> ✗ </span>
                  </td>
                </tr>
                <tr>
                  <td class="left"> web/test </td>
                  <td class="right"> 9.5 </td>
                  <td class="right"> 6.4% </td>
                  <td class="left">
                    <span aria-label="yes" class="ui-yes"> ✓ </span>
                  </td>
                </tr>
              </tbody>
            </table>
          </div>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        </figure>
        <div class="ui-tabs">
          <div role="tablist" class="ui-tablist">
            <button type="button"
                    id="ui-u1-5-tab0"
                    role="tab"
                    aria-selected="true"
                    aria-controls="ui-u1-5-tab0-panel"
                    tabindex="0"
                    class="on ui-tab"
                    @on_click
                    @on_keydown> Steps </button>
            <button type="button"
                    id="ui-u1-5-tab1"
                    role="tab"
                    aria-selected="false"
                    aria-controls="ui-u1-5-tab1-panel"
                    tabindex="-1"
                    class="ui-tab"
                    @on_click
                    @on_keydown> Timeline </button>
          </div>
          <div id="ui-u1-5-tab0-panel"
               role="tabpanel"
               aria-labelledby="ui-u1-5-tab0"
               class="ui-stack ui-tabpanel">
            <div class="ui-steps">
              <Vdom.Node.none-widget> </Vdom.Node.none-widget>
              <ol>
                <li class="done ui-step">
                  <span title="Done" role="img" aria-label="Done" class="done mark">
                    <icon class="check"> </icon>
                  </span>
                  <div class="ui-step-body">
                    <span class="ui-step-title"> Profile </span>
                    <Vdom.Node.none-widget> </Vdom.Node.none-widget>
                  </div>
                </li>
                <li aria-current="step" class="now ui-step">
                  <span title="In progress" role="img" aria-label="In progress" class="mark now">
                    <icon class="next"> </icon>
                  </span>
                  <div class="ui-step-body">
                    <span class="ui-step-title"> Cache deps </span>
                    <div class="ui-md ui-step-text">
                      <div class="markdown">
                        <div class="md-part">
                          <p dir="auto">
                            Use
                            <code> nix </code>
                             binary cache
                          </p>
                        </div>
                      </div>
                    </div>
                  </div>
                </li>
                <li class="todo ui-step">
                  <span title="To do" role="img" aria-label="To do" class="mark todo">
                    <icon class="ring"> </icon>
                  </span>
                  <div class="ui-step-body">
                    <span class="ui-step-title"> Split e2e </span>
                    <Vdom.Node.none-widget> </Vdom.Node.none-widget>
                  </div>
                </li>
              </ol>
            </div>
          </div>
        </div>
        <div class="ui-kv">
          <div class="ui-title"> Machine </div>
          <dl>
            <dt> CPU </dt>
            <dd>
              <div class="ui-md">
                <div class="markdown">
                  <div class="md-part">
                    <p dir="auto"> 16 cores </p>
                  </div>
                </div>
              </div>
            </dd>
            <dt> OS </dt>
            <dd>
              <div class="ui-md">
                <div class="markdown">
                  <div class="md-part">
                    <p dir="auto"> NixOS </p>
                  </div>
                </div>
              </div>
            </dd>
          </dl>
        </div>
        <div class="ui-badges">
          <span class="tone-neutral ui-badge"> ocaml </span>
          <span class="tone-success ui-badge"> passing </span>
          <span class="tone-warning ui-badge"> flaky </span>
        </div>
        <div class="ui-calc">
          <div class="ui-title"> Cache savings </div>
          <div class="ui-calc-body">
            <div class="ui-calc-inputs">
              <div class="ui-calc-input">
                <label for="ui-u1-8-builds"> Builds a day </label>
                <div class="ui-calc-field">
                  <input id="ui-u1-8-builds"
                         type="number"
                         inputmode="decimal"
                         min="0"
                         max="200"
                         step="1"
                         #value="40"
                         @on_input/>
                  <Vdom.Node.none-widget> </Vdom.Node.none-widget>
                </div>
                <input type="range"
                       aria-label="Builds a day"
                       min="0"
                       max="200"
                       step="1"
                       class="ui-slider"
                       #value="40"
                       @on_input/>
              </div>
              <div class="ui-calc-input">
                <label for="ui-u1-8-minutes"> Minutes saved </label>
                <div class="ui-calc-field">
                  <input id="ui-u1-8-minutes"
                         type="number"
                         inputmode="decimal"
                         #value="1.5"
                         @on_input/>
                  <span class="ui-unit"> min </span>
                </div>
                <Vdom.Node.none-widget> </Vdom.Node.none-widget>
              </div>
            </div>
            <div aria-live="polite" class="ui-calc-outputs">
              <div class="ui-calc-output">
                <div class="ui-calc-label"> Hours a month </div>
                <output class="ui-calc-value"> 30.0 h </output>
              </div>
              <div class="ui-calc-output">
                <div class="ui-calc-label"> Worth it </div>
                <output class="ui-calc-value"> 1 </output>
              </div>
            </div>
          </div>
        </div>
        <div class="ui-quiz">
          <div class="ui-quiz-head">
            <Vdom.Node.none-widget> </Vdom.Node.none-widget>
            <span aria-live="polite" class="ui-score"> 1 question </span>
            <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          </div>
          <div class="ui-questions">
            <div class="ui-question">
              <div class="ui-question-head">
                <span class="ui-question-n"> 1 </span>
                <div class="ui-md">
                  <div class="markdown">
                    <div class="md-part">
                      <p dir="auto"> Which tool formats OCaml? </p>
                    </div>
                  </div>
                </div>
              </div>
              <div role="radiogroup" class="ui-options">
                <button type="button" role="radio" aria-checked="false" class="ui-option" @on_click>
                  <span class="ui-option-key"> A </span>
                  <span class="ui-option-text"> ocamlformat </span>
                  <Vdom.Node.none-widget> </Vdom.Node.none-widget>
                </button>
                <button type="button" role="radio" aria-checked="false" class="ui-option" @on_click>
                  <span class="ui-option-key"> B </span>
                  <span class="ui-option-text"> prettier </span>
                  <Vdom.Node.none-widget> </Vdom.Node.none-widget>
                </button>
              </div>
              <Vdom.Node.none-widget> </Vdom.Node.none-widget>
            </div>
          </div>
        </div>
        <div class="ui-flash">
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          <button type="button"
                  aria-pressed="false"
                  aria-label="Card 1 of 2: front. Flip"
                  class="ui-flash-card"
                  @on_click>
            <div class="ui-flash-inner">
              <div aria-hidden="false" class="front ui-face">
                <span class="ui-face-label"> Question </span>
                <div class="ui-md">
                  <div class="markdown">
                    <div class="md-part">
                      <p dir="auto">
                        What is
                        <strong> Eio </strong>
                        ?
                      </p>
                    </div>
                  </div>
                </div>
              </div>
              <div aria-hidden="true" class="back ui-face">
                <span class="ui-face-label"> Answer </span>
                <div class="ui-md">
                  <div class="markdown">
                    <div class="md-part">
                      <p dir="auto"> Effects-based IO for OCaml 5. </p>
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </button>
          <div class="ui-flash-nav">
            <button type="button"
                    aria-label="Previous card"
                    disabled=""
                    class="ghost ui-btn"
                    @on_click> ‹ Prev </button>
            <span aria-live="polite" class="ui-count"> 1 / 2 </span>
            <button type="button" aria-label="Next card" class="ghost ui-btn" @on_click> Next › </button>
          </div>
        </div>
        <div class="ui-choices ui-reply">
          <div class="ui-reply-head">
            <div class="ui-md ui-prompt">
              <div class="markdown">
                <div class="md-part">
                  <p dir="auto"> What next? </p>
                </div>
              </div>
            </div>
            <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          </div>
          <div role="group" class="ui-choice-list">
            <button type="button" class="tone-neutral ui-choice" @on_click>
              <Vdom.Node.none-widget> </Vdom.Node.none-widget>
              <span class="ui-choice-text">
                <span class="ui-choice-label"> Cache deps </span>
                <span class="ui-choice-desc"> fastest win </span>
              </span>
            </button>
            <button type="button" class="tone-warning ui-choice" @on_click>
              <Vdom.Node.none-widget> </Vdom.Node.none-widget>
              <span class="ui-choice-text">
                <span class="ui-choice-label"> Split e2e </span>
                <Vdom.Node.none-widget> </Vdom.Node.none-widget>
              </span>
            </button>
          </div>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        </div>
        <div class="ui-form ui-reply">
          <div class="ui-reply-head">
            <div class="ui-title"> Deploy options </div>
            <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          </div>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          <div class="ui-fields">
            <div class="ui-field ui-field-select">
              <label for="ui-u1-12-env" class="ui-label">
                Environment
                <Vdom.Node.none-widget> </Vdom.Node.none-widget>
              </label>
              <select id="ui-u1-12-env" name="env" class="ui-input" @on_change>
                <option selected="" #value="staging"> staging </option>
                <option #value="prod"> prod </option>
              </select>
              <Vdom.Node.none-widget> </Vdom.Node.none-widget>
            </div>
            <div class="ui-field ui-field-number">
              <label for="ui-u1-12-replicas" class="ui-label">
                Replicas
                <Vdom.Node.none-widget> </Vdom.Node.none-widget>
              </label>
              <input id="ui-u1-12-replicas"
                     name="replicas"
                     type="number"
                     inputmode="decimal"
                     min="1"
                     max="9"
                     class="ui-input"
                     #value="2"
                     @on_input/>
              <Vdom.Node.none-widget> </Vdom.Node.none-widget>
            </div>
            <div class="ui-field ui-field-toggle">
              <label for="ui-u1-12-notify" class="ui-toggle-row">
                <span class="ui-label"> Notify team </span>
                <button id="ui-u1-12-notify"
                        type="button"
                        role="switch"
                        aria-checked="true"
                        class="ui-switch"
                        @on_click>
                  <span class="ui-knob"> </span>
                </button>
              </label>
              <Vdom.Node.none-widget> </Vdom.Node.none-widget>
            </div>
            <div class="ui-field ui-field-textarea">
              <label for="ui-u1-12-note" class="ui-label">
                Note
                <Vdom.Node.none-widget> </Vdom.Node.none-widget>
              </label>
              <textarea id="ui-u1-12-note"
                        name="note"
                        placeholder="Optional"
                        rows="3"
                        class="ui-input"
                        #value=""
                        @on_input> </textarea>
              <Vdom.Node.none-widget> </Vdom.Node.none-widget>
            </div>
          </div>
          <div class="ui-actions">
            <Vdom.Node.none-widget> </Vdom.Node.none-widget>
            <button type="button" class="primary ui-btn" @on_click> Deploy </button>
          </div>
        </div>
        <div class="ui-checklist">
          <div class="ui-reply-head">
            <div class="ui-title"> Before release </div>
            <span class="ui-count"> 1/2 </span>
            <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          </div>
          <ul>
            <li>
              <button type="button"
                      role="checkbox"
                      aria-checked="true"
                      class="on ui-check-item"
                      @on_click>
                <span aria-hidden="true" class="check on">
                  <icon class="check"> </icon>
                </span>
                <div class="ui-md">
                  <div class="markdown">
                    <div class="md-part">
                      <p dir="auto">
                        Run
                        <code> ./test --full </code>
                      </p>
                    </div>
                  </div>
                </div>
              </button>
            </li>
            <li>
              <button type="button"
                      role="checkbox"
                      aria-checked="false"
                      class="ui-check-item"
                      @on_click>
                <span aria-hidden="true" class="check"> </span>
                <div class="ui-md">
                  <div class="markdown">
                    <div class="md-part">
                      <p dir="auto"> Update docs </p>
                    </div>
                  </div>
                </div>
              </button>
            </li>
          </ul>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        </div>
        <div class="ui-accordion">
          <div class="open ui-acc-item">
            <button type="button"
                    aria-expanded="true"
                    aria-controls="ui-u1-14-acc0"
                    class="ui-acc-head"
                    @on_click>
              <span class="ui-acc-title"> Code </span>
              <span class="chev">  </span>
            </button>
            <div id="ui-u1-14-acc0" class="ui-acc-body ui-stack">
              <div class="ui-code">
                <Vdom.Node.none-widget> </Vdom.Node.none-widget>
                <div class="markdown">
                  <div class="md-part">
                    <div class="code copyable">
                      <div class="code-head">
                        <span class="code-lang"> sh </span>
                        <button type="button" title="Copy" class="copy"> Copy </button>
                      </div>
                      <pre class="copy-text">
                        <code class="language-sh"> dune build @runtest </code>
                      </pre>
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>
          <div class="ui-acc-item">
            <button type="button"
                    aria-expanded="false"
                    aria-controls="ui-u1-14-acc1"
                    class="ui-acc-head"
                    @on_click>
              <span class="ui-acc-title"> Diff </span>
              <span class="chev">  </span>
            </button>
            <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          </div>
        </div>
        <div class="ui-tree">
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          <ul>
            <li title="frontend/" class="dir tone-neutral ui-tree-row" style={ --depth: 0; }>
              <span aria-hidden="true" class="ui-tree-icon">
                <icon class="folder"> </icon>
              </span>
              <span class="ui-tree-name"> frontend/ </span>
              <Vdom.Node.none-widget> </Vdom.Node.none-widget>
            </li>
            <li title="frontend/web/" class="dir tone-neutral ui-tree-row" style={ --depth: 1; }>
              <span aria-hidden="true" class="ui-tree-icon">
                <icon class="folder"> </icon>
              </span>
              <span class="ui-tree-name"> web/ </span>
              <Vdom.Node.none-widget> </Vdom.Node.none-widget>
            </li>
            <li title="frontend/web/lib/" class="dir tone-neutral ui-tree-row" style={ --depth: 2; }>
              <span aria-hidden="true" class="ui-tree-icon">
                <icon class="folder"> </icon>
              </span>
              <span class="ui-tree-name"> lib/ </span>
              <Vdom.Node.none-widget> </Vdom.Node.none-widget>
            </li>
            <li title="frontend/web/lib/ui_view.ml"
                class="file tone-success ui-tree-row"
                style={
                  --depth: 3;
                }>
              <span aria-hidden="true" class="ui-tree-icon">
                <Vdom.Node.none-widget> </Vdom.Node.none-widget>
              </span>
              <span class="ui-tree-name"> ui_view.ml </span>
              <div class="ui-md ui-tree-note">
                <div class="markdown">
                  <div class="md-part">
                    <p dir="auto"> new </p>
                  </div>
                </div>
              </div>
            </li>
            <li title="backend/" class="dir tone-neutral ui-tree-row" style={ --depth: 0; }>
              <span aria-hidden="true" class="ui-tree-icon">
                <icon class="folder"> </icon>
              </span>
              <span class="ui-tree-name"> backend/ </span>
              <Vdom.Node.none-widget> </Vdom.Node.none-widget>
            </li>
          </ul>
        </div>
        <hr class="ui-divider"/>
        <div class="ui-markdown">
          <div class="markdown">
            <div class="md-part">
              <p dir="auto">
                That's all — see the
                <strong> table </strong>
                 above.
              </p>
            </div>
          </div>
        </div>
        <div class="ui-unknown"> A "sparkle" component this page can't show: update prigh-web </div>
      </div>
    </div>
    |}]
;;

let%expect_test "every component, as text" =
  let h = H.create () in
  shown h components;
  H.text h ~selector:".tool-ui";
  [%expect {|
    Build health
    Builds got 18% faster since dune 3.22 .
    Median build
    42.5 s ↓ -18%
    last 30 days
    Pass rate
    97.2% ↑ +1.4
    Flaky tests
    3
    Build times by week
    s
    40 45 50 55 60 65 70
    W1
    main 61 s
    PRs 70 s
    W2
    main 58 s
    PRs 66 s
    W3
    main —
    PRs 63 s
    W4
    main 51 s
    PRs 60 s
    W5
    main 47 s
    PRs 55 s
    W6
    main 44 s
    PRs 52 s
    W1 W2 W3 W4 W5 W6
    main
    PRs
    Build times by week
    main PRs
    W1 61 s 70 s
    W2 58 s 66 s
    W3 63 s
    W4 51 s 60 s
    W5 47 s 55 s
    W6 44 s 52 s
    Breakdown
    Where the time goes
    45 Total
    Compile 24 53.3%
    Link 9 20.0%
    Tests 12 26.7%
    Compile 24 53.3%
    Link 9 20.0%
    Tests 12 26.7%
    Donut chart
    Value Share
    Compile 24 53.3%
    Link 9 20.0%
    Tests 12 26.7%
    Coverage 78%
    target 80%
    Slowest tests
    (Test ▲) (Time ▲) (Share ▲) (Passing ▲)
    e2e/mobile 31.2 21% ✓
    tmux/agents 18.0 12% ✗
    web/test 9.5 6.4% ✓
    (Steps) (Timeline)
    ✓
    Profile
    ▸
    Cache deps
    Use nix binary cache
    ○
    Split e2e
    Machine
    CPU
    16 cores
    OS
    NixOS
    ocaml passing flaky
    Cache savings
    Builds a day
    [40]
    [40]
    Minutes saved
    [1.5] min
    Hours a month
    30.0 h
    Worth it
    1
    1 question
    1
    Which tool formats OCaml?
    (A ocamlformat) (B prettier)
    (Question What is Eio ? Answer Effects-based IO for OCaml 5.)
    (‹ Prev) 1 / 2 (Next ›)
    What next?
    (Cache deps fastest win) (Split e2e)
    Deploy options
    Environment
    staging prod
    Replicas
    [2]
    Notify team ()
    Note
    []
    (Deploy)
    Before release
    1/2
    (☑ Run ./test --full)
    (☐ Update docs)
    (Code)
    sh (Copy)
    dune build @runtest
    (Diff)
    frontend/
    web/
    lib/
    ui_view.ml
    new
    backend/
    That's all — see the table above.
    A "sparkle" component this page can't show: update prigh-web
    |}]
;;

let%expect_test "tree rows imply directories" =
  let rows paths =
    Prigh_web.Ui_tree.rows (List.map paths ~f:(fun p -> p, ()))
    |> List.iter ~f:(fun (r : _ Prigh_web.Ui_tree.Row.t) ->
      printf
        "%s%s%s\n"
        (String.make (2 * r.depth) ' ')
        r.name
        (if Option.is_none r.info then "  (implied)" else ""))
  in
  rows [ "frontend/"; "frontend/web/lib/ui_view.ml"; "backend/"; "frontend/web/x.ml" ];
  rows [ "/home/u/p/a.png"; "/home/u/q/" ];
  [%expect {|
    frontend/
      web/  (implied)
        lib/  (implied)
          ui_view.ml
        x.ml
    backend/
    home/  (implied)
      u/  (implied)
        p/  (implied)
          a.png
        q/
    |}]
;;

let table =
  one
    {|{"type":"table","title":"Slowest","columns":[{"key":"name","label":"Test"},{"key":"time","label":"Time","format":"number","decimals":1}],"rows":[{"name":"b","time":3},{"name":"a","time":10},{"name":"c","time":null}]}|}
;;

let%expect_test "sorting a table" =
  let h = H.create () in
  shown h table;
  let rows () = H.text h ~selector:".ui-table tbody" in
  rows ();
  [%expect {|
    b 3.0
    a 10.0
    c —
    |}];
  H.click h ~nth:1 ~selector:".ui-sort";
  rows ();
  H.elements h ~selector:"th[aria-sort=ascending]";
  [%expect {|
    (Ui (Set (key u1/0/sort) (value time:desc)))
    a 10.0
    b 3.0
    c —
    |}];
  H.click h ~nth:1 ~selector:".ui-sort";
  rows ();
  H.elements h ~selector:"th[aria-sort=descending]";
  [%expect {|
    (Ui (Set (key u1/0/sort) (value time:asc)))
    b 3.0
    a 10.0
    c —
    |}];
  H.click h ~selector:".ui-sort";
  rows ();
  [%expect {|
    (Ui (Set (key u1/0/sort) (value name:asc)))
    a 10.0
    b 3.0
    c —
    |}]
;;

let calculator =
  one
    {|{"type":"calculator","title":"Loan","inputs":[{"name":"p","label":"Principal","default":1000,"min":0,"max":5000,"step":100,"slider":true},{"name":"r","label":"Rate","default":5,"unit":"%"}],"outputs":[{"label":"Interest","expr":"p * r / 100","format":"currency"},{"label":"Big?","expr":"p > 2000 ? 1 : 0","format":"integer"},{"label":"Odd","expr":"p / (r - 5)"}]}|}
;;

let%expect_test "a calculator works out as you type" =
  let h = H.create () in
  shown h calculator;
  let out () = H.text h ~selector:".ui-calc-outputs" in
  out ();
  [%expect {|
    Interest
    $50.00
    Big?
    0
    Odd
    — division by zero
    |}];
  H.input h ~selector:".ui-calc-field input" "3000";
  out ();
  H.show h ~selector:".ui-slider";
  [%expect {|
    (Ui (Set (key u1/0/in.p) (value 3000)))
    Interest
    $150.00
    Big?
    1
    Odd
    — division by zero
    <input type="range"
           aria-label="Principal"
           min="0"
           max="5000"
           step="100"
           class="ui-slider"
           #value="3000"
           @on_input/>
    |}];
  H.input h ~nth:1 ~selector:".ui-calc-field input" "7.5";
  out ();
  [%expect {|
    (Ui (Set (key u1/0/in.r) (value 7.5)))
    Interest
    $225.00
    Big?
    1
    Odd
    1,200
    |}];
  H.input h ~nth:1 ~selector:".ui-calc-field input" "abc";
  out ();
  [%expect {|
    (Ui (Set (key u1/0/in.r) (value abc)))
    Interest
    — enter a number for Rate
    Big?
    1
    Odd
    — enter a number for Rate
    |}]
;;

let%expect_test "flashcards flip and move on" =
  let h = H.create () in
  shown h (one {|{"type":"flashcards","cards":[{"front":"One","back":"Uno"},{"front":"Two","back":"Dos"}]}|});
  let card () =
    H.names h ~selector:".ui-flash-card";
    H.elements h ~selector:".ui-flash-card";
    H.text h ~selector:".ui-count"
  in
  card ();
  [%expect {|
    button "Card 1 of 2: front. Flip"
    <button class="ui-flash-card">
    1 / 2
    |}];
  H.click h ~selector:".ui-flash-card";
  card ();
  [%expect {|
    (Ui (Set (key u1/0/flip) (value 1)))
    button "Card 1 of 2: back. Flip"
    <button class="flipped ui-flash-card">
    1 / 2
    |}];
  H.click h ~selector:"[aria-label='Next card']";
  card ();
  H.text h ~selector:".ui-face.front";
  [%expect {|
    (Ui (Set (key u1/0/i) (value 1)))
    (Ui (Set (key u1/0/flip) (value 0)))
    button "Card 2 of 2: front. Flip"
    <button class="ui-flash-card">
    2 / 2
    Question
    Two
    |}]
;;

let%expect_test "a quiz grades each answer and keeps score" =
  let h = H.create () in
  shown
    h
    (one
       {|{"type":"quiz","title":"OCaml","questions":[{"question":"2+2?","options":["3","4"],"answer":1,"explanation":"Arithmetic."},{"question":"Capital of France?","options":["Paris","Rome","Oslo"],"answer":0}]}|});
  H.click h ~selector:".ui-option";
  H.text h ~selector:".ui-quiz";
  [%expect {|
    (Ui (Set (key u1/0/q0) (value 0)))
    OCaml
    0 / 1 correct · 1 left (Retry)
    1
    2+2?
    (A 3 ✗) (B 4 ✓)
    Not quite.
    Arithmetic.
    2
    Capital of France?
    (A Paris) (B Rome) (C Oslo)
    |}];
  H.click h ~nth:2 ~selector:".ui-option";
  H.text h ~selector:".ui-quiz";
  H.elements h ~selector:".ui-option";
  [%expect {|
    (Ui (Set (key u1/0/q1) (value 0)))
    OCaml
    1 / 2 correct (Retry)
    1
    2+2?
    (A 3 ✗) (B 4 ✓)
    Not quite.
    Arithmetic.
    2
    Capital of France?
    (A Paris ✓) (B Rome) (C Oslo)
    Correct.
    <button class="ui-option wrong">
    <button class="correct ui-option">
    <button class="correct ui-option">
    <button class="other ui-option">
    <button class="other ui-option">
    |}]
;;

let%expect_test "tabs and accordions" =
  let h = H.create () in
  shown
    h
    (one
       {|{"type":"tabs","tabs":[{"label":"A","children":[{"type":"markdown","text":"first"}]},{"label":"B","children":[{"type":"markdown","text":"second"}]}]}|});
  H.text h ~selector:".ui-tabs";
  H.click h ~nth:1 ~selector:".ui-tab";
  H.text h ~selector:".ui-tabs";
  H.names h ~selector:"[role=tab][aria-selected=true]";
  [%expect {|
    (A) (B)
    first
    (Ui (Set (key u1/0/tab) (value 1)))
    (A) (B)
    second
    [tab] button "B"
    |}];
  let h = H.create () in
  shown
    h
    (one
       {|{"type":"accordion","items":[{"title":"One","children":[{"type":"markdown","text":"inside"}]}]}|});
  H.text h ~selector:".ui-accordion";
  H.click h ~selector:".ui-acc-head";
  H.text h ~selector:".ui-accordion";
  [%expect {|
    (One)
    (Ui (Set (key u1/0/open0) (value 1)))
    (One)
    inside
    |}]
;;

let%expect_test "a local checklist ticks" =
  let h = H.create () in
  shown h (one {|{"type":"checklist","title":"Todo","items":[{"text":"a"},{"text":"b","checked":true}]}|});
  H.click h ~selector:".ui-check-item";
  H.text h ~selector:".ui-checklist";
  [%expect {|
    (Ui (Set (key u1/0/c0) (value 1)))
    Todo
    2/2
    (☑ a)
    (☑ b)
    |}]
;;

let choices =
  one
    {|{"type":"choices","id":"next","prompt":"What next?","options":[{"label":"Cache deps","value":"cache"},{"label":"Split e2e"}]}|}
;;

let%expect_test "a choice is sent with ui_action, then shows as sent" =
  let h = H.create () in
  shown h choices;
  H.click h ~selector:".ui-choice";
  H.text h ~selector:".ui-choices";
  [%expect {|
    (Ui
     (Send ((call_id u1) (component next) (title "What next?") (values cache))))
    Scroll_to_bottom
    (Rpc (method_ prompt)
     (params
      ((text "")
       (ui_action
        ((call_id u1) (component next) (title "What next?") (values cache)))))
     (tag (Ui_sent (session s1) (call_id u1) (component next))))
    What next?
    ✓ Sent
    (Cache deps) (Split e2e)
    |}];
  (* The backend's message for it marks it sent for good. *)
  H.reply h "prompt" "{}";
  H.event
    h
    {|{"event":"message_start","message":{"role":"user","text":"The user responded through \"What next?\" (render_ui call u1, component next):\n\"cache\"","origin":{"source":{"kind":"ui_action","title":"Sent \"What next?\": cache"}}}}|};
  H.text h ~selector:".entries";
  H.elements h ~selector:".ui-choice[disabled]";
  [%expect {|
    What next?
    ✓ Sent
    (Cache deps) (Split e2e)
    ↵ Sent "What next?": cache
    The user responded through "What next?" (render_ui call u1, component next):
    "cache"
    <button class="on tone-neutral ui-choice">
    <button class="tone-neutral ui-choice">
    |}]
;;

let%expect_test "while the agent works, a reply is a follow-up" =
  let h = H.create () in
  shown ~finish:false h choices;
  H.event h (sprintf {|{"event":"state","state":%s}|} (H.state_json ~fields:[ "running", `True ] ()));
  H.click h ~nth:1 ~selector:".ui-choice";
  [%expect {|
    (Ui
     (Send
      ((call_id u1) (component next) (title "What next?") (values "Split e2e"))))
    Scroll_to_bottom
    (Rpc (method_ follow_up)
     (params
      ((text "")
       (ui_action
        ((call_id u1) (component next) (title "What next?") (values "Split e2e")))))
     (tag (Ui_sent (session s1) (call_id u1) (component next))))
    |}]
;;

let%expect_test "a failed send can be sent again" =
  let h = H.create () in
  shown h choices;
  H.click h ~selector:".ui-choice";
  H.fail h "prompt" "no such render_ui call: u1";
  H.text h ~selector:".ui-choices";
  [%expect {|
    (Ui
     (Send ((call_id u1) (component next) (title "What next?") (values cache))))
    Scroll_to_bottom
    (Rpc (method_ prompt)
     (params
      ((text "")
       (ui_action
        ((call_id u1) (component next) (title "What next?") (values cache)))))
     (tag (Ui_sent (session s1) (call_id u1) (component next))))
    What next?
    (Cache deps) (Split e2e)
    |}]
;;

let form =
  one
    {|{"type":"form","id":"deploy","title":"Deploy options","fields":[{"name":"env","label":"Environment","kind":"select","options":["staging","prod"],"default":"staging"},{"name":"replicas","label":"Replicas","kind":"number","default":2},{"name":"notify","label":"Notify","kind":"toggle"},{"name":"tags","label":"Tags","kind":"checkbox","options":["a","b"]},{"name":"who","label":"Who","kind":"text","required":true}],"submit":"Deploy"}|}
;;

let%expect_test "a form checks required fields and sends typed values" =
  let h = H.create () in
  shown h form;
  H.click h ~selector:".ui-actions .ui-btn";
  H.text h ~selector:".ui-actions";
  [%expect {|
    (Ui (Set (key u1/0/tried) (value 1)))
    Who is required (Deploy)
    |}];
  H.change h ~selector:"select.ui-input" "prod";
  H.input h ~selector:"input[name=replicas]" "5";
  H.click h ~selector:".ui-switch";
  H.change h ~nth:1 ~selector:".ui-group input" "on";
  H.input h ~selector:"input[name=who]" "me";
  H.click h ~selector:".ui-actions .ui-btn";
  H.text h ~selector:".ui-form";
  [%expect {|
    (Ui (Set (key u1/0/f.env) (value prod)))
    (Ui (Set (key u1/0/f.replicas) (value 5)))
    (Ui (Set (key u1/0/f.notify) (value true)))
    (Ui (Set (key u1/0/f.tags) (value "[\"b\"]")))
    (Ui (Set (key u1/0/f.who) (value me)))
    (Ui
     (Send
      ((call_id u1) (component deploy) (title "Deploy options")
       (values ((env prod) (replicas 5) (notify true) (tags (b)) (who me))))))
    Scroll_to_bottom
    (Rpc (method_ prompt)
     (params
      ((text "")
       (ui_action
        ((call_id u1) (component deploy) (title "Deploy options")
         (values ((env prod) (replicas 5) (notify true) (tags (b)) (who me)))))))
     (tag (Ui_sent (session s1) (call_id u1) (component deploy))))
    Deploy options
    ✓ Sent
    Environment
    staging prod
    Replicas
    [5]
    Notify ()
    Tags
    [] a
    [] b
    Who *
    [me]
    |}]
;;

let%expect_test "after a reload, answered components show what was sent" =
  let h = H.create () in
  (* What a reload loads: the newest page, with the reply in it. *)
  H.act
    h
    (Reply
       ( H.newest "s1"
       , Ok
           (Jsonaf.of_string
              (H.page
                 (sprintf
          {|[%s,%s,{"role":"user","text":"The user responded through \"Deploy options\" (render_ui call u1, component deploy):\n{\"env\":\"prod\",\"replicas\":3,\"notify\":true,\"tags\":[\"b\"],\"who\":\"ops\"}","origin":{"source":{"kind":"ui_action","title":"Sent \"Deploy options\": env = prod, replicas = 3"}}}]|}
          (assistant form)
          (result "Shown: form \"Deploy options\"."))))));
  H.text h ~selector:".entries";
  H.elements h ~selector:".ui-form [disabled]";
  H.show h ~selector:".ui-switch, .ui-group input";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  ("Soup.Parse_error(\"expected tag name or '*'\")")
  Trailing output
  ---------------
  Scroll_to_bottom
  Deploy options
  ✓ Sent
  Environment
  staging prod
  Replicas
  [3]
  Notify ()
  Tags
  [] a
  [] b
  Who *
  [ops]
  ↵ Sent "Deploy options": env = prod, replicas = 3
  The user responded through "Deploy options" (render_ui call u1, component deploy):
  {"env":"prod","replicas":3,"notify":true,"tags":["b"],"who":"ops"}
  <select class="ui-input">
  <input class="ui-input">
  <button class="ui-switch">
  <input class="">
  <input class="">
  <input class="ui-input">
  |}]
;;

let%expect_test "streaming, refused and cancelled calls" =
  let h = H.create () in
  H.event h {|{"event":"agent_start"}|};
  H.event
    h
    (sprintf
       {|{"event":"message_update","partial":%s,"delta":{"type":"tool_call_delta","index":0,"arguments":""}}|}
       (assistant {|{"components":[{"type":"ch|}));
  H.show h ~selector:".tool-ui";
  [%expect {|
    <div class="tool-ui">
      <div role="status" aria-label="Preparing components" class="genui ui-skeleton">
        <div class="short ui-skel-line"> </div>
        <div class="ui-skel-block"> </div>
        <div class="ui-skel-line"> </div>
      </div>
    </div>
    |}];
  let h = H.create () in
  H.event h {|{"event":"agent_start"}|};
  H.event h (sprintf {|{"event":"message_end","message":%s}|} (assistant table));
  H.event
    h
    (sprintf
       {|{"event":"tool_end","call":%s,"result":%s}|}
       (call table)
       (result ~is_error:true "components[0].rows[2].time: give a number"));
  H.event h {|{"event":"agent_end","messages":[]}|};
  H.text h ~selector:".entries";
  H.elements h ~selector:".tool-ui";
  [%expect {|
    (✕ render_ui)
    components[0].rows[2].time: give a number
    |}]
;;

let%expect_test "images ask for a link as they show, and open in the viewer" =
  let h = H.create () in
  shown
    h
    (one
       {|{"type":"gallery","title":"Shots","images":[{"path":"/p/a.png","caption":"Before"},{"path":"/p/b.png","alt":"After"}]}|});
  H.show h ~selector:".ui-gallery";
  [%expect {|
    (Rpc (method_ fs_preview) (params ((path /p/a.png)))
     (tag (Ui_image (session s1) (path /p/a.png))))
    (Rpc (method_ fs_preview) (params ((path /p/b.png)))
     (tag (Ui_image (session s1) (path /p/b.png))))
    <div data-viewer-group="" class="ui-gallery">
      <figure class="ui-image">
        <div role="img" aria-label="Loading a.png" aria-busy="true" class="loading ui-image-frame"> </div>
        <figcaption class="ui-caption">
          <div class="ui-md">
            <div class="markdown">
              <div class="md-part">
                <p dir="auto"> Before </p>
              </div>
            </div>
          </div>
        </figcaption>
      </figure>
      <figure class="ui-image">
        <div role="img" aria-label="Loading After" aria-busy="true" class="loading ui-image-frame"> </div>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      </figure>
    </div>
    |}];
  H.reply
    h
    "fs_preview"
    {|{"path":"/p/a.png","name":"a.png","size":10,"kind":"image","mime":"image/png","url":"/files/download?ticket=t1"}|};
  H.fail h "fs_preview" "no such file: /p/b.png";
  H.show h ~selector:".ui-gallery";
  [%expect {|
    <div data-viewer-group="" class="ui-gallery">
      <figure class="ui-image">
        <button type="button"
                title="/p/a.png"
                aria-label="View a.png"
                data-viewer=""
                class="ui-image-frame">
          <img src="/files/download?ticket=t1" alt="a.png" loading="lazy"/>
        </button>
        <figcaption class="ui-caption">
          <div class="ui-md">
            <div class="markdown">
              <div class="md-part">
                <p dir="auto"> Before </p>
              </div>
            </div>
          </div>
        </figcaption>
      </figure>
      <figure class="ui-image">
        <div role="img" aria-label="After" class="failed ui-image-frame">
          <span class="ui-image-error"> no such file: /p/b.png </span>
          <span class="ui-image-path"> /p/b.png </span>
        </div>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      </figure>
    </div>
    |}]
;;
