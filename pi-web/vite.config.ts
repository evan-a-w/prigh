import { readFileSync } from "node:fs";
import { basename, resolve } from "node:path";
import preact from "@preact/preset-vite";
import { defineConfig, type Plugin } from "vite";

// The Bonsai web UI's terminal (tui/web-bin/terminal.js on xterm.js), shipped
// under xterm/ and loaded by src/components/terminal-panel.tsx when opened.
// $PRIGH_TERMINAL_ASSETS overrides the directory (the Nix build has no ../tui).
const terminalAssetsDir = process.env.PRIGH_TERMINAL_ASSETS ?? resolve(import.meta.dirname, "../tui/web-bin");
const terminalAssets = ["terminal.js", "vendor/xterm.js", "vendor/xterm.css", "vendor/addon-fit.js"];

function terminalAssetsPlugin(): Plugin {
	const files = new Map(terminalAssets.map((path) => [`xterm/${basename(path)}`, resolve(terminalAssetsDir, path)]));
	return {
		name: "prigh-terminal-assets",
		configureServer(server) {
			server.middlewares.use((req, res, next) => {
				const file = files.get((req.url ?? "").split("?")[0].replace(/^\//, ""));
				if (!file) return next();
				res.setHeader("Content-Type", file.endsWith(".css") ? "text/css" : "text/javascript");
				res.end(readFileSync(file));
			});
		},
		generateBundle() {
			for (const [fileName, file] of files) {
				this.emitFile({ type: "asset", fileName, source: readFileSync(file) });
			}
		},
	};
}

// `npm run dev` proxies the WebSockets to a running backend:
//   prigh serve -pi-web 127.0.0.1:7789 -faux
export default defineConfig({
	plugins: [preact(), terminalAssetsPlugin()],
	server: {
		proxy: {
			"/ws": { target: "http://127.0.0.1:7789", ws: true },
			"/terminal": { target: "http://127.0.0.1:7789", ws: true },
		},
	},
	build: {
		outDir: "dist",
		target: "es2022",
	},
});
