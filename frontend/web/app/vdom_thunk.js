// Makes virtual_dom's lazy nodes (Node.lazy_, which View_cache returns)
// reusable. A VdomThunk is reused, neither rendered nor diffed again, when
// the previous frame's thunk at its place has the same function and the same
// arguments (by identity), but virtual_dom's Thunk.create wraps its function
// in a new JavaScript closure every time (`Js.wrap_callback (fun a -> f a)`
// in virtual_dom/thunk.ml), so the functions never matched and every lazy
// node was rendered and diffed anew on every frame.
//
// The function is always the same one (Node.lazy_ is the only maker of
// thunks, and its function forces the OCaml lazy value it is given), so the
// arguments, that lazy value, decide alone: the same value is the same
// node, already rendered.
//
// Recheck when upgrading virtual_dom; drop this once Thunk.create keeps its
// function, or compares the arguments only, upstream.

//Provides: prigh_reuse_vdom_thunks
function prigh_reuse_vdom_thunks() {
  var Thunk = globalThis.VdomThunk;
  if (!Thunk || Thunk.prototype.prighReuse) return 0;
  Thunk.prototype.render = function (prev) {
    if (prev && this.args === prev.args) return prev.vnode;
    return this.fn(this.args);
  };
  Thunk.prototype.prighReuse = true;
  return 0;
}
