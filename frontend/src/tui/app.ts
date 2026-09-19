import { spawn } from "node:child_process";
import type { Client } from "../client.js";
import { ansi } from "../markdown.js";
import type { AuthEvent, AuthStatus, Event, SessionSummary, State } from "../protocol.js";
import { COMMANDS, completeCommand, helpText, parseCommand } from "./commands.js";
import { Editor } from "./editor.js";
import { parseKeys, type Key } from "./keys.js";
import {
	renderAssistantText,
	renderStatus,
	renderThinking,
	renderToolResult,
	renderToolStart,
	renderUser,
	rowsFor,
	type Style,
} from "./render.js";

export interface Terminal {
	write(text: string): void;
	columns(): number;
	onInput(f: (data: string) => void): void;
	onResize(f: () => void): void;
	setRawMode(enabled: boolean): void;
}

const SPINNER = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];

interface PendingAuthPrompt {
	id: string;
	kind: "secret" | "manual_code" | "select";
	options: { id: string; label: string }[];
}

/** Best-effort, detached; failures are silent because the URL is printed too. */
export function openBrowser(url: string): void {
	const [command, args] =
		process.platform === "darwin" ? ["open", [url]] : process.platform === "win32" ? ["cmd", ["/c", "start", "", url]] : ["xdg-open", [url]];
	try {
		const child = spawn(command, args, { stdio: "ignore", detached: true });
		child.on("error", () => {});
		child.unref();
	} catch {
		// ignore
	}
}

export function formatAuthStatus(statuses: AuthStatus[]): string {
	const width = Math.max(...statuses.map((s) => s.provider.length));
	return statuses
		.map((s) => {
			const methods = s.methods.map((m) => `${m.method} (${m.label})`).join(", ");
			const state = s.configured ? `${ansi.green}logged in${ansi.reset} via ${s.configured.source}` : `${ansi.gray}not configured${ansi.reset}`;
			return `${s.provider.padEnd(width)}  ${state}  [${methods}]`;
		})
		.join("\n");
}

/**
 * The transcript lives in the terminal scrollback and only ever receives
 * complete lines. The bottom panel (partial streaming line, editor, status)
 * is erased and redrawn on every change.
 */
export class App {
	private readonly client: Client;
	private readonly terminal: Terminal;
	private readonly editor = new Editor();
	private state: State | null = null;
	private panelRows = 0; // rows currently occupied by the bottom panel
	private cursorRowFromTop = 0;
	private streamTail = ""; // incomplete last line of streaming assistant text
	private streamKind: "text" | "thinking" | null = null;
	private toolTail = ""; // last line of running tool output
	private spinnerIndex = 0;
	private spinnerTimer: NodeJS.Timeout | null = null;
	private sessions: SessionSummary[] = [];
	private authPrompt: PendingAuthPrompt | null = null;
	private pendingQuit = false;
	private ready = false;
	private inputBuffer: Key[] = [];
	private resolveExit: (() => void) | null = null;
	readonly exited: Promise<void>;

	constructor(client: Client, terminal: Terminal) {
		this.client = client;
		this.terminal = terminal;
		this.exited = new Promise((resolve) => {
			this.resolveExit = resolve;
		});
	}

	private get style(): Style {
		return { color: true, width: this.terminal.columns() };
	}

	async start(): Promise<void> {
		this.terminal.setRawMode(true);
		this.terminal.write("\x1b[?2004h"); // bracketed paste
		this.terminal.onInput((data) => {
			for (const key of parseKeys(data)) {
				if (this.ready) this.handleKey(key);
				else this.inputBuffer.push(key);
			}
		});
		this.terminal.onResize(() => this.redraw());
		this.client.subscribe((event) => this.handleEvent(event));
		this.client.onProtocolError = (message) => this.notice(`protocol error: ${message}`);
		void this.client.exited.then((code) => {
			this.notice(`backend exited (${String(code)})`);
			this.quit();
		});
		try {
			this.state = await this.client.getState();
			const messages = await this.client.getMessages();
			for (const message of messages) {
				if (message.role === "user") this.printBlock(renderUser(message.text, this.style));
				else if (message.role === "assistant") {
					for (const block of message.content) {
						if (block.type === "text" && block.text.trim()) this.printBlock(renderAssistantText(block.text, this.style));
						else if (block.type === "tool_call") this.printBlock(renderToolStart(block, this.style));
					}
				} else this.printBlock(renderToolResult(message, this.style));
			}
			this.notice(`session ${this.state.session_id} in ${this.state.cwd}. /help for commands, Esc aborts, Ctrl+C twice quits.`);
		} catch (e) {
			this.notice(`failed to connect: ${(e as Error).message}`);
		}
		this.ready = true;
		const buffered = this.inputBuffer;
		this.inputBuffer = [];
		for (const key of buffered) this.handleKey(key);
		this.redraw();
	}

	// ---- output -------------------------------------------------------

	private erasePanel(): void {
		if (this.panelRows === 0) return;
		if (this.cursorRowFromTop > 0) this.terminal.write(`\x1b[${this.cursorRowFromTop}A`);
		this.terminal.write("\r\x1b[J");
		this.panelRows = 0;
	}

	/** Writes complete lines to the scrollback. */
	private printBlock(text: string): void {
		this.erasePanel();
		this.terminal.write(text.endsWith("\n") ? text : `${text}\n`);
		this.redraw();
	}

	private notice(text: string): void {
		this.printBlock(`${ansi.yellow}${text}${ansi.reset}`);
	}

	private redraw(): void {
		this.erasePanel();
		const style = this.style;
		const width = style.width;
		const parts: string[] = [];
		if (this.streamTail) parts.push(this.streamKind === "thinking" ? renderThinking(this.streamTail, style) : this.streamTail);
		if (this.toolTail) parts.push(`${ansi.gray}  ${this.toolTail.slice(-(width - 2))}${ansi.reset}`);
		const separator = `${ansi.gray}${"─".repeat(Math.max(1, width))}${ansi.reset}`;
		parts.push(separator);
		const secret = this.authPrompt?.kind === "secret";
		const marker = this.authPrompt ? `${ansi.bold}${ansi.yellow}? ${ansi.reset}` : `${ansi.bold}${ansi.cyan}> ${ansi.reset}`;
		const lines = this.editor.getLines().map((line) => (secret ? "*".repeat(line.length) : line));
		const editorLines = lines.map((line, i) => `${i === 0 ? marker : "  "}${line}`);
		parts.push(...editorLines);
		const running = this.state?.running ?? false;
		const extra = this.authPrompt
			? "login: Enter answers, Esc cancels"
			: running
				? `${SPINNER[this.spinnerIndex % SPINNER.length]} working (Esc to abort; Enter steers)`
				: "";
		parts.push(renderStatus(this.state, extra, style));
		const panel = parts.join("\n");
		this.terminal.write(panel);
		this.panelRows = rowsFor(panel, width);
		// Position the cursor inside the editor.
		const pos = this.editor.position;
		const rowsBeforeEditor = parts.slice(0, parts.length - editorLines.length - 1).reduce((n, p) => n + rowsFor(p, width), 0);
		let editorRow = rowsBeforeEditor;
		for (let i = 0; i < pos.line; i++) editorRow += rowsFor(editorLines[i] ?? "", width);
		const col = 2 + pos.col;
		editorRow += Math.floor(col / width);
		const targetRow = editorRow;
		const rowsUp = this.panelRows - 1 - targetRow;
		if (rowsUp > 0) this.terminal.write(`\x1b[${rowsUp}A`);
		this.terminal.write(`\r${col % width > 0 ? `\x1b[${col % width}C` : ""}`);
		this.cursorRowFromTop = targetRow;
	}

	private setSpinner(on: boolean): void {
		if (on && !this.spinnerTimer) {
			this.spinnerTimer = setInterval(() => {
				this.spinnerIndex++;
				this.redraw();
			}, 100);
		} else if (!on && this.spinnerTimer) {
			clearInterval(this.spinnerTimer);
			this.spinnerTimer = null;
		}
	}

	// ---- events -------------------------------------------------------

	private flushStream(): void {
		if (this.streamTail) {
			const text = this.streamKind === "thinking" ? renderThinking(this.streamTail, this.style) : this.streamTail;
			this.streamTail = "";
			this.streamKind = null;
			this.printBlock(text);
		}
	}

	private appendStream(kind: "text" | "thinking", text: string): void {
		if (this.streamKind !== kind) this.flushStream();
		this.streamKind = kind;
		const combined = this.streamTail + text;
		const lastNewline = combined.lastIndexOf("\n");
		if (lastNewline === -1) {
			this.streamTail = combined;
			this.redraw();
			return;
		}
		const complete = combined.slice(0, lastNewline);
		this.streamTail = combined.slice(lastNewline + 1);
		this.printBlock(kind === "thinking" ? renderThinking(complete, this.style) : complete);
	}

	private handleEvent(event: Event): void {
		switch (event.event) {
			case "state":
				this.state = event.state;
				this.setSpinner(event.state.running);
				if (!event.state.running) {
					this.flushStream();
					this.toolTail = "";
				}
				this.redraw();
				break;
			case "message_start":
				if (event.message.role === "user") this.printBlock(renderUser(event.message.text, this.style));
				break;
			case "message_update": {
				const d = event.delta;
				if (d.type === "text_delta") this.appendStream("text", d.text);
				else if (d.type === "thinking_delta") this.appendStream("thinking", d.text);
				break;
			}
			case "message_end":
				this.flushStream();
				if (event.message.role === "assistant") {
					const r = event.message.stop_reason;
					if (r.type === "error") this.notice(`error: ${r.message}`);
					else if (r.type === "aborted") this.notice("[aborted]");
					else if (r.type === "length") this.notice("[output truncated by the model's length limit]");
				}
				break;
			case "tool_start":
				this.flushStream();
				this.toolTail = "";
				this.printBlock(renderToolStart(event.call, this.style));
				break;
			case "tool_output": {
				const lines = (this.toolTail + event.chunk).split("\n");
				this.toolTail = (lines[lines.length - 1] || lines[lines.length - 2]) ?? "";
				this.redraw();
				break;
			}
			case "tool_end":
				this.toolTail = "";
				this.printBlock(renderToolResult(event.result, this.style));
				break;
			case "compacted":
				this.notice("context compacted");
				break;
			case "notice":
				this.notice(event.text);
				break;
			case "auth":
				this.handleAuthEvent(event);
				break;
			case "agent_start":
			case "agent_end":
			case "turn_start":
			case "turn_end":
				break;
		}
	}

	private handleAuthEvent(event: AuthEvent): void {
		switch (event.kind) {
			case "auth_url":
				this.printBlock(`${ansi.bold}Open this URL to log in:${ansi.reset}\n  ${ansi.cyan}${event.url}${ansi.reset}\n${ansi.gray}${event.instructions}${ansi.reset}`);
				openBrowser(event.url);
				break;
			case "prompt": {
				const options = event.prompt === "select" ? event.options : [];
				this.authPrompt = { id: event.id, kind: event.prompt, options };
				let text = `${ansi.yellow}${event.message}${ansi.reset}`;
				if (event.prompt === "select") text += `\n${options.map((o, i) => `  ${i + 1}. ${o.label} (${o.id})`).join("\n")}`;
				if (event.prompt === "manual_code") text += `\n${ansi.gray}e.g. ${event.placeholder}?code=...${ansi.reset}`;
				this.printBlock(text);
				break;
			}
			case "prompt_cancelled":
				if (this.authPrompt?.id === event.id) {
					this.authPrompt = null;
					this.editor.clear();
					this.redraw();
				}
				break;
			case "progress":
				this.notice(event.message);
				break;
			case "done":
				this.authPrompt = null;
				this.notice(`logged in to ${event.provider} (${event.method})`);
				void this.switchToProvider(event.provider);
				break;
			case "failed":
				this.authPrompt = null;
				this.notice(`login to ${event.provider} failed: ${event.error}`);
				this.redraw();
				break;
			case "logged_out":
				this.notice(`logged out of ${event.provider}`);
				break;
		}
	}

	/** After a login, move to that provider unless it is already in use. */
	private async switchToProvider(provider: string): Promise<void> {
		if (this.state?.model.provider === provider) return;
		try {
			const models = await this.client.listModels();
			const model = models.find((m) => m.provider === provider);
			if (model) {
				await this.client.call("set_model", { model: model.key });
				this.notice(`model set to ${model.key}; /model to change`);
			}
		} catch (e) {
			this.notice((e as Error).message);
		}
	}

	private async submitAuthAnswer(text: string): Promise<void> {
		const prompt = this.authPrompt;
		if (!prompt) return;
		let value = text.trim();
		if (prompt.kind === "select") {
			const index = Number(value);
			const option = Number.isInteger(index) ? prompt.options[index - 1] : prompt.options.find((o) => o.id === value);
			if (!option) {
				this.notice(`choose 1-${prompt.options.length}`);
				return;
			}
			value = option.id;
		}
		this.authPrompt = null;
		try {
			await this.client.authRespond(prompt.id, value);
		} catch (e) {
			this.notice((e as Error).message);
		}
		this.redraw();
	}

	// ---- input --------------------------------------------------------

	private handleKey(key: Key): void {
		if (key.kind !== "ctrl" || key.letter !== "c") this.pendingQuit = false;
		switch (key.kind) {
			case "char":
				this.editor.insert(key.char);
				break;
			case "paste":
				this.editor.insert(key.text);
				break;
			case "enter":
				void this.submit();
				return;
			case "newline":
				this.editor.newline();
				break;
			case "backspace":
				this.editor.backspace();
				break;
			case "delete":
				this.editor.deleteForward();
				break;
			case "left":
				this.editor.left();
				break;
			case "right":
				this.editor.right();
				break;
			case "up":
				if (!this.editor.up()) this.editor.historyPrev();
				break;
			case "down":
				if (!this.editor.down()) this.editor.historyNext();
				break;
			case "home":
				this.editor.home();
				break;
			case "end":
				this.editor.end();
				break;
			case "tab": {
				const completed = completeCommand(this.editor.text);
				if (completed) this.editor.setText(completed);
				break;
			}
			case "escape":
				if (this.authPrompt) {
					this.authPrompt = null;
					this.editor.clear();
					void this.client.authCancel().catch(() => {});
				} else if (this.state?.running) void this.client.abort().catch(() => {});
				break;
			case "ctrl":
				this.handleCtrl(key.letter);
				return;
			case "unknown":
				break;
		}
		this.redraw();
	}

	private handleCtrl(letter: string): void {
		switch (letter) {
			case "c":
				if (this.editor.text !== "") {
					this.editor.clear();
					this.pendingQuit = false;
				} else if (this.pendingQuit) {
					this.quit();
					return;
				} else {
					this.pendingQuit = true;
					this.notice("press Ctrl+C again to quit");
				}
				break;
			case "d":
				this.quit();
				return;
			case "a":
				this.editor.home();
				break;
			case "e":
				this.editor.end();
				break;
			case "k":
				this.editor.killToEnd();
				break;
			case "u":
				this.editor.killLine();
				break;
			case "w":
				this.editor.killWord();
				break;
			case "l":
				this.terminal.write("\x1b[2J\x1b[H");
				this.panelRows = 0;
				break;
			case "j":
				this.editor.newline();
				break;
		}
		this.redraw();
	}

	private async submit(): Promise<void> {
		const text = this.editor.submit(this.authPrompt !== null);
		this.redraw();
		if (this.authPrompt) {
			await this.submitAuthAnswer(text);
			return;
		}
		if (text.trim() === "") return;
		const command = parseCommand(text);
		if (command) {
			await this.runCommand(command.name, command.args, command.rest);
			return;
		}
		try {
			if (this.state?.running) {
				await this.client.steer(text);
				this.notice("queued (will be delivered after the current turn)");
			} else await this.client.prompt(text);
		} catch (e) {
			this.notice((e as Error).message);
		}
	}

	private async runCommand(name: string, args: string[], rest: string): Promise<void> {
		try {
			switch (name) {
				case "help":
					this.printBlock(helpText());
					break;
				case "model": {
					const models = await this.client.listModels();
					if (args[0]) {
						await this.client.call("set_model", { model: args[0] });
					} else {
						this.printBlock(
							models
								.map((m) => `${m.key === this.state?.model.key ? "* " : "  "}${m.key.padEnd(34)} ${m.name}  ($${m.cost.input}/$${m.cost.output} per M)`)
								.join("\n"),
						);
					}
					break;
				}
				case "thinking":
					if (args[0]) await this.client.call("set_thinking", { thinking: args[0] });
					else this.printBlock(`thinking: ${this.state?.thinking ?? "?"} (levels: off, on, low, high, max)`);
					break;
				case "auth":
					this.printBlock(formatAuthStatus(await this.client.authStatus()));
					break;
				case "login": {
					if (!args[0]) {
						this.printBlock(`${formatAuthStatus(await this.client.authStatus())}\n\nusage: /login <provider> [api_key|oauth]`);
						break;
					}
					await this.client.login(args[0], args[1]);
					break;
				}
				case "logout":
					if (!args[0]) throw new Error("usage: /logout <provider>");
					await this.client.logout(args[0]);
					break;
				case "compact": {
					this.notice("compacting…");
					await this.client.call("compact");
					break;
				}
				case "new":
					await this.client.call("new_session");
					this.notice("new session");
					break;
				case "sessions": {
					this.sessions = await this.client.listSessions();
					const lines = this.sessions.map(
						(s, i) => `${String(i + 1).padStart(3)}. ${s.created_at.slice(0, 19)}  ${String(s.message_count).padStart(3)} msgs  ${(s.first_prompt ?? "").replace(/\n/g, " ").slice(0, 60)}`,
					);
					this.printBlock(lines.length ? lines.join("\n") : "no sessions");
					break;
				}
				case "switch": {
					const index = Number(args[0]);
					const path = Number.isInteger(index) && this.sessions[index - 1] ? this.sessions[index - 1]?.path : rest;
					if (!path) throw new Error("usage: /switch <number|path>");
					await this.client.call("switch_session", { path });
					const messages = await this.client.getMessages();
					this.terminal.write("\x1b[2J\x1b[H");
					this.panelRows = 0;
					for (const m of messages) {
						if (m.role === "user") this.printBlock(renderUser(m.text, this.style));
						else if (m.role === "assistant") {
							for (const b of m.content) {
								if (b.type === "text" && b.text.trim()) this.printBlock(renderAssistantText(b.text, this.style));
								else if (b.type === "tool_call") this.printBlock(renderToolStart(b, this.style));
							}
						} else this.printBlock(renderToolResult(m, this.style));
					}
					break;
				}
				case "fork":
					await this.client.call("fork");
					this.notice("forked session");
					break;
				case "abort":
					await this.client.abort();
					break;
				case "state":
					this.printBlock(JSON.stringify(this.state, null, 2));
					break;
				case "clear":
					this.terminal.write("\x1b[2J\x1b[H");
					this.panelRows = 0;
					this.redraw();
					break;
				case "quit":
					this.quit();
					break;
				default:
					this.notice(`unknown command /${name}; commands: ${COMMANDS.map((c) => `/${c.name}`).join(" ")}`);
			}
		} catch (e) {
			this.notice((e as Error).message);
		}
	}

	quit(): void {
		this.setSpinner(false);
		this.erasePanel();
		this.terminal.write("\x1b[?2004l");
		this.terminal.setRawMode(false);
		this.client.close();
		this.resolveExit?.();
	}
}
