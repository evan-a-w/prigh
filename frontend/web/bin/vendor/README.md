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
