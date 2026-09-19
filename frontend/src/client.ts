import { spawn, type ChildProcessByStdio } from "node:child_process";
import type { Readable, Writable } from "node:stream";
import {
	isAuthStatus,
	isMessage,
	isModel,
	isSessionSummary,
	isState,
	parseServerMessage,
	type AuthStatus,
	type Event,
	type JsonObject,
	type JsonValue,
	type Message,
	type Model,
	type SessionSummary,
	type State,
} from "./protocol.js";

export type EventListener = (event: Event) => void;

interface Pending {
	resolve: (value: JsonValue) => void;
	reject: (error: Error) => void;
}

/** Splits a byte stream into complete lines, keeping a partial tail. */
export class LineSplitter {
	private tail = "";

	push(chunk: string): string[] {
		const data = this.tail + chunk;
		const parts = data.split("\n");
		this.tail = parts.pop() ?? "";
		return parts.filter((line) => line.trim() !== "");
	}

	flush(): string[] {
		const rest = this.tail.trim();
		this.tail = "";
		return rest === "" ? [] : [rest];
	}
}

export interface Transport {
	stdin: Writable;
	stdout: Readable;
	onExit(f: (code: number | null) => void): void;
	kill(): void;
}

export class RpcError extends Error {}

/** JSON-lines RPC client over a transport (normally the spawned backend). */
export class Client {
	private readonly transport: Transport;
	private readonly pending = new Map<number, Pending>();
	private readonly listeners: EventListener[] = [];
	private readonly splitter = new LineSplitter();
	private nextId = 1;
	private closed = false;
	readonly exited: Promise<number | null>;
	onProtocolError: (message: string) => void = () => {};

	constructor(transport: Transport) {
		this.transport = transport;
		this.exited = new Promise((resolve) => transport.onExit((code) => resolve(code)));
		transport.stdout.setEncoding("utf8");
		transport.stdout.on("data", (chunk: string) => {
			for (const line of this.splitter.push(chunk)) this.handleLine(line);
		});
		transport.stdout.on("end", () => {
			for (const line of this.splitter.flush()) this.handleLine(line);
			this.closed = true;
			for (const [, p] of this.pending) p.reject(new RpcError("backend closed"));
			this.pending.clear();
		});
	}

	static spawnBackend(command: string, args: string[]): Client {
		const child: ChildProcessByStdio<Writable, Readable, null> = spawn(command, args, {
			stdio: ["pipe", "pipe", "inherit"],
		});
		return new Client({
			stdin: child.stdin,
			stdout: child.stdout,
			onExit: (f) => child.on("exit", (code) => f(code)),
			kill: () => child.kill(),
		});
	}

	private handleLine(line: string): void {
		const parsed = parseServerMessage(line);
		if (parsed.kind === "invalid") {
			this.onProtocolError(parsed.error);
			return;
		}
		const message = parsed.message;
		if (message.type === "event") {
			const { type: _type, ...event } = message;
			for (const listener of this.listeners) listener(event as Event);
			return;
		}
		const id = typeof message.id === "number" ? message.id : Number.NaN;
		const pending = this.pending.get(id);
		if (!pending) {
			this.onProtocolError(`response for unknown request id ${String(message.id)}`);
			return;
		}
		this.pending.delete(id);
		if (message.ok) pending.resolve(message.result);
		else pending.reject(new RpcError(message.error));
	}

	subscribe(listener: EventListener): () => void {
		this.listeners.push(listener);
		return () => {
			const i = this.listeners.indexOf(listener);
			if (i >= 0) this.listeners.splice(i, 1);
		};
	}

	call(method: string, params: JsonObject = {}): Promise<JsonValue> {
		if (this.closed) return Promise.reject(new RpcError("backend closed"));
		const id = this.nextId++;
		return new Promise((resolve, reject) => {
			this.pending.set(id, { resolve, reject });
			this.transport.stdin.write(`${JSON.stringify({ id, method, params })}\n`);
		});
	}

	close(): void {
		this.closed = true;
		this.transport.stdin.end();
	}

	kill(): void {
		this.transport.kill();
	}

	// Typed convenience wrappers.

	async getState(): Promise<State> {
		const result = await this.call("get_state");
		if (!isState(result)) throw new RpcError("malformed state");
		return result;
	}

	async getMessages(): Promise<Message[]> {
		const result = await this.call("get_messages");
		if (!Array.isArray(result)) throw new RpcError("malformed messages");
		return result.map((m) => {
			if (!isMessage(m)) throw new RpcError("malformed message");
			return m;
		});
	}

	async listModels(): Promise<Model[]> {
		const result = await this.call("list_models");
		if (!Array.isArray(result)) throw new RpcError("malformed models");
		return result.map((m) => {
			if (!isModel(m)) throw new RpcError("malformed model");
			return m;
		});
	}

	async listSessions(): Promise<SessionSummary[]> {
		const result = await this.call("list_sessions");
		if (!Array.isArray(result)) throw new RpcError("malformed sessions");
		return result.map((s) => {
			if (!isSessionSummary(s)) throw new RpcError("malformed session");
			return s;
		});
	}

	async authStatus(): Promise<AuthStatus[]> {
		const result = await this.call("auth_status");
		if (!Array.isArray(result)) throw new RpcError("malformed auth status");
		return result.map((s) => {
			if (!isAuthStatus(s)) throw new RpcError("malformed auth status");
			return s;
		});
	}

	login(provider: string, method?: string): Promise<JsonValue> {
		return this.call("login", method ? { provider, method } : { provider });
	}
	authRespond(id: string, value: string): Promise<JsonValue> {
		return this.call("auth_respond", { id, value });
	}
	authCancel(): Promise<JsonValue> {
		return this.call("auth_cancel");
	}
	logout(provider: string): Promise<JsonValue> {
		return this.call("logout", { provider });
	}

	prompt(text: string): Promise<JsonValue> {
		return this.call("prompt", { text });
	}
	steer(text: string): Promise<JsonValue> {
		return this.call("steer", { text });
	}
	followUp(text: string): Promise<JsonValue> {
		return this.call("follow_up", { text });
	}
	abort(): Promise<JsonValue> {
		return this.call("abort");
	}
}
