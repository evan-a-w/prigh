// The browser end of a prigh terminal: xterm.js on a WebSocket to the
// backend's /terminal, whose protocol is described in
// backend/lib/terminal/terminals.mli. prigh-web mounts it through
// [window.prighTerminal.mount] (see ../app/terminal_widget.ml).
"use strict";

(() => {
  const PING_MS = 10_000;
  // The backend drops a socket after 30s of silence; we give up on one that
  // has not answered our pings for as long and reconnect.
  const SILENT_MS = 30_000;
  const MAX_RETRY_MS = 5_000;

  const whenConnected = (element, f) => {
    if (element.isConnected) f();
    else requestAnimationFrame(() => whenConnected(element, f));
  };

  /**
   * @param {HTMLElement} host
   * @param {string} url ws(s)://.../terminal?token=...&session=...
   * @param {{
   *   onStatus: (status: "connecting" | "open" | "reconnecting" | "exited"
   *     | "failed", message?: string) => void,
   *   focus?: boolean,
   *   theme?: object,
   *   transform?: (data: string) => string,
   *   copy?: (text: string) => void,
   *   menuKey?: () => boolean,
   * }} options [onStatus] reports the connection's state; [focus] (default
   *   true) focuses the terminal once it is open; [theme] is xterm.js's;
   *   [transform] rewrites what is typed (the extra keys' latched Ctrl and Alt);
   *   [copy] puts Ctrl+Shift+C's selection on the clipboard; [menuKey]: whether
   *   Shift+F10 and the menu key open a context menu (prigh's, in the desktop
   *   app's browser window) rather than going to the shell.
   * @returns {{ focus(): void, dispose(): void, setTheme(theme: object): void,
   *   press(data: string): void, appCursor(): boolean }} [press] sends
   *   [data] as typed, untransformed; [appCursor] is the cursor keys' mode
   *   (DECCKM).
   */
  const mount = (host, url, options) => {
    const report = (state, message) => options.onStatus(state, message);
    let focusWhenOpen = options.focus ?? true;

    const encoder = new TextEncoder();
    let term;
    let fit;
    let ws;
    let lastHeard = 0;
    let retryMs = 250;
    let retryTimer;
    let resizeTimer;
    let pingTimer;
    let observer;
    // "open" | "exited" (a key starts a new shell) | "failed" | "disposed"
    let state = "open";
    // While [press] feeds xterm.js its bytes, which are not to be transformed.
    let pressing = false;

    const send = data => {
      if (ws?.readyState === WebSocket.OPEN) ws.send(data);
    };

    const connect = () => {
      const target = new URL(url);
      target.searchParams.set("cols", String(term.cols));
      target.searchParams.set("rows", String(term.rows));
      const socket = new WebSocket(target);
      ws = socket;
      if (retryMs === 250) report("connecting");
      socket.binaryType = "arraybuffer";
      socket.onopen = () => {
        // The first message is a replay of the whole screen.
        term.reset();
        retryMs = 250;
        lastHeard = Date.now();
        report("open");
      };
      socket.onmessage = event => {
        lastHeard = Date.now();
        if (typeof event.data !== "string") {
          term.write(new Uint8Array(event.data));
          return;
        }
        const message = JSON.parse(event.data);
        if (message.type === "exit") {
          state = "exited";
          report("exited");
        } else if (message.type === "error") {
          state = "failed";
          report("failed", message.message);
        }
      };
      socket.onclose = () => {
        if (ws !== socket) return;
        ws = undefined;
        if (state === "open") reconnect();
      };
    };

    const reconnect = () => {
      report("reconnecting");
      clearTimeout(retryTimer);
      retryTimer = setTimeout(connect, retryMs);
      retryMs = Math.min(retryMs * 2, MAX_RETRY_MS);
    };

    const heartbeat = () => {
      if (ws?.readyState !== WebSocket.OPEN) return;
      if (Date.now() - lastHeard > SILENT_MS) {
        const stale = ws;
        ws = undefined;
        stale.close();
        reconnect();
      } else {
        send('{"type":"ping"}');
      }
    };

    // As typed, untransformed: a paste or the extra keys.
    const untransformed = f => {
      pressing = true;
      try {
        f();
      } finally {
        pressing = false;
      }
    };
    const paste = text => untransformed(() => term.paste(text));

    // Ctrl+Shift+C and V copy and paste (Ctrl+C stays the shell's ^C);
    // where [menuKey] says so, Shift+F10 and the menu key open the context
    // menu instead of reaching the shell.
    const keyHandler = ev => {
      const plainCtrlShift = ev.ctrlKey && ev.shiftKey && !ev.altKey && !ev.metaKey;
      if (plainCtrlShift && ev.code === "KeyC") {
        if (ev.type === "keydown") {
          // Not Chrome's inspector, which has the same keys.
          ev.preventDefault();
          if (term.hasSelection()) options.copy?.(term.getSelection());
        }
        return false;
      }
      if (plainCtrlShift && ev.code === "KeyV") {
        // Without the Clipboard API (http) the browser's own paste (its
        // Ctrl+Shift+V) reaches xterm.js's paste handler.
        if (ev.type === "keydown" && navigator.clipboard?.readText) {
          ev.preventDefault();
          navigator.clipboard.readText().then(paste, () => {});
        }
        return false;
      }
      const menuKey = ev.key === "ContextMenu"
        || (ev.key === "F10" && ev.shiftKey && !ev.ctrlKey && !ev.altKey && !ev.metaKey);
      if (menuKey && options.menuKey?.()) {
        // Not every browser makes these keys a contextmenu: we do, on the
        // input that follows the cursor.
        if (ev.type === "keydown") {
          ev.preventDefault();
          term.textarea?.dispatchEvent(new MouseEvent("contextmenu", { bubbles: true, cancelable: true }));
        }
        return false;
      }
      return true;
    };

    const start = () => {
      if (state === "disposed") return;
      term = new Terminal({
        cursorBlink: true,
        fontFamily:
          '"JetBrains Mono", "Fira Code", Menlo, Consolas, "DejaVu Sans Mono", monospace',
        fontSize: 14,
        scrollback: 5000,
        theme: options.theme ?? {
          background: "#1e1e1e",
          foreground: "#d4d4d4",
          cursor: "#d4d4d4",
        },
      });
      fit = new FitAddon.FitAddon();
      term.loadAddon(fit);
      term.open(host);
      term.attachCustomKeyEventHandler(keyHandler);
      // For prigh's context menu (app/field_menu_listeners.ml), which finds
      // it on xterm.js's element.
      term.element.prighTerminal = {
        hasSelection: () => term.hasSelection(),
        selection: () => term.getSelection(),
        paste: text => {
          paste(text);
          term.focus();
        },
        selectAll: () => term.selectAll(),
        focus: () => term.focus(),
      };
      fit.fit();
      term.onData(data => {
        if (options.transform && !pressing) data = options.transform(data);
        if (state === "exited") {
          state = "open";
          connect();
        } else {
          send(encoder.encode(data));
        }
      });
      term.onBinary(data => send(Uint8Array.from(data, c => c.charCodeAt(0))));
      term.onResize(({ cols, rows }) =>
        send(JSON.stringify({ type: "resize", cols, rows })));
      observer = new ResizeObserver(() => {
        clearTimeout(resizeTimer);
        resizeTimer = setTimeout(() => fit.fit(), 50);
      });
      observer.observe(host);
      pingTimer = setInterval(heartbeat, PING_MS);
      connect();
      if (focusWhenOpen) term.focus();
    };

    whenConnected(host, start);

    return {
      focus: () => {
        if (term) term.focus();
        else focusWhenOpen = true;
      },
      // Through xterm.js, as a key typed: it scrolls to the prompt.
      press: data => {
        if (term) untransformed(() => term.input(data, true));
      },
      appCursor: () => term?.modes.applicationCursorKeysMode ?? false,
      setTheme: theme => {
        options.theme = theme;
        if (term) term.options.theme = theme;
      },
      dispose: () => {
        state = "disposed";
        clearTimeout(retryTimer);
        clearTimeout(resizeTimer);
        clearInterval(pingTimer);
        observer?.disconnect();
        const socket = ws;
        ws = undefined;
        socket?.close();
        term?.dispose();
      },
    };
  };

  window.prighTerminal = { mount };
})();
