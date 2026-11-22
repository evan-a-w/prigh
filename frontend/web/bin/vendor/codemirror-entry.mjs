// The CodeMirror 6 modules prigh-web's editor mode uses, as one global
// (window.PrighCodeMirror). Rebuild: see frontend/web/bin/vendor/README.md.
export {
  EditorState, EditorSelection, Compartment, StateField, StateEffect,
  RangeSetBuilder, Text
} from "@codemirror/state";
export {
  EditorView, keymap, lineNumbers, highlightActiveLine,
  highlightActiveLineGutter, drawSelection, Decoration, WidgetType, gutter,
  GutterMarker, ViewPlugin
} from "@codemirror/view";
export {
  history, defaultKeymap, historyKeymap, indentWithTab, undo, redo,
  indentMore, indentLess, cursorCharLeft, cursorCharRight, cursorGroupLeft,
  cursorGroupRight, insertNewlineAndIndent
} from "@codemirror/commands";
export {
  HighlightStyle, syntaxHighlighting, StreamLanguage, indentUnit,
  bracketMatching, indentOnInput
} from "@codemirror/language";
export { searchKeymap, highlightSelectionMatches, openSearchPanel } from "@codemirror/search";
export { tags } from "@lezer/highlight";
export { javascript } from "@codemirror/lang-javascript";
export { markdown } from "@codemirror/lang-markdown";
export { rust } from "@codemirror/lang-rust";
export { python } from "@codemirror/lang-python";
export { json } from "@codemirror/lang-json";
export { css } from "@codemirror/lang-css";
export { html } from "@codemirror/lang-html";
export { nix } from "@replit/codemirror-lang-nix";
export { oCaml } from "@codemirror/legacy-modes/mode/mllike";
export { shell } from "@codemirror/legacy-modes/mode/shell";
export { yaml } from "@codemirror/legacy-modes/mode/yaml";
export { toml } from "@codemirror/legacy-modes/mode/toml";
export { go } from "@codemirror/legacy-modes/mode/go";
export { c, cpp, java } from "@codemirror/legacy-modes/mode/clike";
export { diff } from "@codemirror/legacy-modes/mode/diff";
export { scheme } from "@codemirror/legacy-modes/mode/scheme";
