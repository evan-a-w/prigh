// The browser end of a prigh terminal: xterm.js on a WebSocket to the
// backend's /terminal, whose protocol is described in
// backend/lib/terminals.mli. The Bonsai app mounts it through
// [window.prighTerminal.mount] (see tui/web-app/terminal_panel.ml), and so
// do prigh-web (tui/prigh-web/app/terminal_widget.ml) and pi-web
// (pi-web/src/components/terminal-panel.tsx).
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
   *   onStatus?: (status: "connecting" | "open" | "reconnecting" | "exited"
   *     | "failed", message?: string) => void,
   *   focus?: boolean,
   *   theme?: object,
   * }} [options] with [onStatus] the caller shows the connection's state
   *   (otherwise a line over the terminal does); [focus] (default true)
   *   focuses the terminal once it is open; [theme] is xterm.js's.
   * @returns {{ focus(): void, dispose(): void }}
   */
  const mount = (host, url, options = {}) => {
    let status;
    if (!options.onStatus) {
      status = document.createElement("div");
      status.className = "terminal-status";
      host.appendChild(status);
    }
    const report = (state, text, message) => {
      if (options.onStatus) options.onStatus(state, message);
      else {
        status.textContent = text;
        status.hidden = text === "";
      }
    };
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

    const send = data => {
      if (ws?.readyState === WebSocket.OPEN) ws.send(data);
    };

    const connect = () => {
      const target = new URL(url);
      target.searchParams.set("cols", String(term.cols));
      target.searchParams.set("rows", String(term.rows));
      const socket = new WebSocket(target);
      ws = socket;
      if (retryMs === 250) report("connecting", "");
      socket.binaryType = "arraybuffer";
      socket.onopen = () => {
        // The first message is a replay of the whole screen.
        term.reset();
        retryMs = 250;
        lastHeard = Date.now();
        report("open", "");
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
          report("exited", "shell exited — press a key for a new one");
        } else if (message.type === "error") {
          state = "failed";
          report("failed", `terminal unavailable: ${message.message}`, message.message);
        }
      };
      socket.onclose = () => {
        if (ws !== socket) return;
        ws = undefined;
        if (state === "open") reconnect();
      };
    };

    const reconnect = () => {
      report("reconnecting", "reconnecting…");
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
      fit.fit();
      term.onData(data => {
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
