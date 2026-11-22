Vendored, unmodified apart from removing the `sourceMappingURL` comments:

- `xterm.js`, `xterm.css`: @xterm/xterm 6.0.0 (`lib/xterm.js`, `css/xterm.css`)
- `addon-fit.js`: @xterm/addon-fit 0.11.0 (`lib/addon-fit.js`)

Both are MIT licensed; see `LICENSE-xterm` and `LICENSE-addon-fit`.

- `katex/`: KaTeX 0.18.7 (`dist/katex.min.js`, `dist/katex.min.css`,
  `dist/contrib/copy-tex.min.js`, `dist/fonts/*.woff2`), from nixpkgs'
  `katex` (`nix build --inputs-from . nixpkgs#katex`, under
  `lib/node_modules/katex`). The stylesheet's `@font-face` rules are cut down
  to the woff2 fonts, which sit next to it (`url(KaTeX_X.woff2)`, not
  `fonts/`): every browser prigh-web supports reads woff2. MIT licensed; see
  `katex/LICENSE-katex`.

- `temporal-polyfill.js`: temporal-polyfill 1.0.8 (`global.js`), loaded by
  `index.html` only where the browser has no `Temporal` (e.g. WebKitGTK):
  Core's time zones (`Timezone_js_loader`) need it, and Bonsai_web's start
  asks for the local zone. MIT licensed; see `LICENSE-temporal-polyfill`.

- `codemirror.js`: CodeMirror 6 for the editor mode, one IIFE defining
  `window.PrighCodeMirror` with what `codemirror-entry.mjs` exports, bundled
  and minified by esbuild 0.27.2 from @codemirror/state 6.7.6, view 6.43.15,
  commands 6.11.1, language 6.13.1, search 6.7.2, lang-javascript 6.2.5,
  lang-markdown 6.5.2, lang-rust 6.0.2, lang-python 6.2.1, lang-json 6.0.2,
  lang-css 6.3.1, lang-html 6.4.12, legacy-modes 6.5.5, @lezer/highlight
  1.2.5 and @replit/codemirror-lang-nix 6.0.1 (and their dependencies:
  @lezer/*, style-mod, crelt, w3c-keyname). To rebuild, in an empty
  directory with `codemirror-entry.mjs` copied into it as `entry.mjs`:

  ```sh
  nix shell nixpkgs#nodejs nixpkgs#esbuild -c sh -c '
    npm install --no-audit --no-fund @codemirror/state @codemirror/view \
      @codemirror/commands @codemirror/language @codemirror/search \
      @codemirror/lang-javascript @codemirror/lang-markdown \
      @codemirror/lang-rust @codemirror/lang-python @codemirror/lang-json \
      @codemirror/lang-css @codemirror/lang-html @codemirror/legacy-modes \
      @replit/codemirror-lang-nix @lezer/highlight &&
    esbuild entry.mjs --bundle --minify --format=iife \
      --global-name=PrighCodeMirror --legal-comments=none \
      --outfile=codemirror.js'
  ```

  MIT licensed; see `LICENSE-codemirror` (CodeMirror and Lezer) and
  `LICENSE-codemirror-lang-nix`.
