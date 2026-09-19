import { existsSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { Client } from "./client.js";
import { App, type Terminal } from "./tui/app.js";

function findBackend(): string {
	const fromEnv = process.env.PRIGH_BACKEND;
	if (fromEnv) return fromEnv;
	const here = path.dirname(fileURLToPath(import.meta.url));
	const candidates = [
		path.resolve(here, "../../../backend/_build/default/bin/main.exe"),
		path.resolve(here, "../../backend/_build/default/bin/main.exe"),
	];
	for (const c of candidates) if (existsSync(c)) return c;
	return "prigh";
}

function usage(): never {
	console.error(`usage: prigh [options] [-- backend options]
  --backend PATH     backend executable (default: $PRIGH_BACKEND or the dune build)
  --faux             scripted provider, no API calls (backend -faux)
  --session PATH     resume a session file
  --model ID         model id
  --thinking LEVEL   off|on|low|high|max
  --cwd DIR          working directory`);
	process.exit(2);
}

function main(): void {
	const argv = process.argv.slice(2);
	let backend = findBackend();
	const backendArgs = ["serve"];
	for (let i = 0; i < argv.length; i++) {
		const arg = argv[i] as string;
		const next = (): string => {
			const v = argv[++i];
			if (v === undefined) usage();
			return v;
		};
		if (arg === "--backend") backend = next();
		else if (arg === "--faux") backendArgs.push("-faux");
		else if (arg === "--session") backendArgs.push("-session", next());
		else if (arg === "--model") backendArgs.push("-model", next());
		else if (arg === "--thinking") backendArgs.push("-thinking", next());
		else if (arg === "--cwd") backendArgs.push("-cwd", next());
		else if (arg === "--") backendArgs.push(...argv.slice(i + 1)), (i = argv.length);
		else usage();
	}
	const client = Client.spawnBackend(backend, backendArgs);
	const terminal: Terminal = {
		write: (text) => process.stdout.write(text),
		columns: () => process.stdout.columns || 80,
		onInput: (f) => {
			process.stdin.setEncoding("utf8");
			process.stdin.on("data", (data: string) => f(data));
		},
		onResize: (f) => process.stdout.on("resize", f),
		setRawMode: (enabled) => {
			if (process.stdin.isTTY) process.stdin.setRawMode(enabled);
			if (enabled) process.stdin.resume();
			else process.stdin.pause();
		},
	};
	const app = new App(client, terminal);
	void app.start();
	void app.exited.then(() => {
		setTimeout(() => {
			client.kill();
			process.exit(0);
		}, 500).unref();
		void client.exited.then(() => process.exit(0));
	});
}

main();
