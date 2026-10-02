import preact from "@preact/preset-vite";
import { defineConfig } from "vite";

// `npm run dev` proxies the WebSocket to a running backend:
//   prigh serve -pi-web 127.0.0.1:7789 -faux
export default defineConfig({
	plugins: [preact()],
	server: {
		proxy: {
			"/ws": { target: "http://127.0.0.1:7789", ws: true },
		},
	},
	build: {
		outDir: "dist",
		target: "es2022",
	},
});
