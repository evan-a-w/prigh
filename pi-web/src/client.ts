import type {
	AgentSessionEvent,
	RpcCommand,
	RpcExtensionUICancel,
	RpcExtensionUIRequest,
	RpcExtensionUIResponse,
	RpcResponse,
} from "./protocol.ts";

export interface RpcClientCallbacks {
	onEvent(event: AgentSessionEvent): void;
	onUiRequest(request: RpcExtensionUIRequest): void;
	/** A dialog was answered by another client, timed out, or was aborted. */
	onUiCancel(id: string): void;
	onConnectionChange(connected: boolean): void;
	/**
	 * The backend refused this connection (bad token, unknown session). The
	 * client stops reconnecting: the user has to fix the token or session.
	 */
	onHelloFailed(error: string): void;
	/** `/setusr` succeeded: the page should reconnect acting as `user`. */
	onSetUser(user: string): void;
}

const RESPONSE_TIMEOUT_MS = 60_000;
const INITIAL_RECONNECT_DELAY_MS = 1_000;
const MAX_RECONNECT_DELAY_MS = 10_000;

/**
 * WebSocket client for the pi RPC protocol. Commands are correlated to
 * responses via an `id` field; events stream unsolicited. The token and
 * session travel in the WebSocket URL's query string, which the backend
 * turns into its `hello`.
 */
export class RpcClient {
	private ws: WebSocket | undefined;
	private nextRequestId = 1;
	private generation = 0;
	private reconnectDelayMs = INITIAL_RECONNECT_DELAY_MS;
	private stopped = false;
	private helloFailed = false;
	private readonly pending = new Map<
		string,
		{ resolve: (response: RpcResponse) => void; reject: (error: Error) => void; timer: ReturnType<typeof setTimeout> }
	>();
	private idleWaiters: Array<() => void> = [];

	/** Re-read on every (re)connect, so the session in the address bar is the one rejoined. */
	private readonly url: () => string;
	private readonly callbacks: RpcClientCallbacks;

	constructor(url: () => string, callbacks: RpcClientCallbacks) {
		this.url = url;
		this.callbacks = callbacks;
	}

	start(): void {
		this.connect();
	}

	stop(): void {
		this.stopped = true;
		this.ws?.close();
	}

	/** Drops the current connection (even a refused one) and connects again with a fresh URL. */
	reconnect(): void {
		const previous = this.ws;
		this.generation++;
		this.stopped = false;
		if (previous) {
			previous.close();
			this.dropPending();
			this.callbacks.onConnectionChange(false);
		}
		this.reconnectDelayMs = INITIAL_RECONNECT_DELAY_MS;
		this.connect();
	}

	/** Resolves once no command awaits its response, or after `timeoutMs`. */
	whenIdle(timeoutMs: number): Promise<void> {
		if (this.pending.size === 0) return Promise.resolve();
		return new Promise((resolve) => {
			const done = () => {
				clearTimeout(timer);
				this.idleWaiters = this.idleWaiters.filter((waiter) => waiter !== done);
				resolve();
			};
			const timer = setTimeout(done, timeoutMs);
			this.idleWaiters.push(done);
		});
	}

	private notifyIfIdle(): void {
		if (this.pending.size === 0) for (const waiter of [...this.idleWaiters]) waiter();
	}

	private dropPending(): void {
		for (const [id, entry] of this.pending) {
			clearTimeout(entry.timer);
			this.pending.delete(id);
			entry.reject(new Error("Connection closed"));
		}
		this.notifyIfIdle();
	}

	get connected(): boolean {
		return this.ws?.readyState === WebSocket.OPEN;
	}

	command(command: RpcCommand): Promise<RpcResponse> {
		const ws = this.ws;
		if (!ws || ws.readyState !== WebSocket.OPEN) {
			return Promise.reject(new Error("Not connected"));
		}
		const id = `req-${this.nextRequestId++}`;
		return new Promise<RpcResponse>((resolve, reject) => {
			const timer = setTimeout(() => {
				this.pending.delete(id);
				this.notifyIfIdle();
				reject(new Error(`Command "${command.type}" timed out`));
			}, RESPONSE_TIMEOUT_MS);
			this.pending.set(id, { resolve, reject, timer });
			ws.send(JSON.stringify({ ...command, id }));
		});
	}

	sendUiResponse(response: RpcExtensionUIResponse): void {
		if (this.ws?.readyState === WebSocket.OPEN) {
			this.ws.send(JSON.stringify(response));
		}
	}

	private connect(): void {
		const generation = ++this.generation;
		const ws = new WebSocket(this.url());
		this.ws = ws;
		this.helloFailed = false;

		ws.onopen = () => {
			if (generation !== this.generation) {
				ws.close();
				return;
			}
			this.reconnectDelayMs = INITIAL_RECONNECT_DELAY_MS;
			this.callbacks.onConnectionChange(true);
		};
		ws.onmessage = (event) => this.handleMessage(event);
		ws.onerror = () => ws.close();
		ws.onclose = () => {
			if (generation !== this.generation) return;
			this.callbacks.onConnectionChange(false);
			this.dropPending();
			if (!this.stopped && !this.helloFailed) {
				setTimeout(() => this.connect(), this.reconnectDelayMs);
				this.reconnectDelayMs = Math.min(this.reconnectDelayMs * 2, MAX_RECONNECT_DELAY_MS);
			}
		};
	}

	private handleMessage(event: MessageEvent): void {
		let message: { type?: string; id?: string };
		try {
			message = JSON.parse(String(event.data)) as { type?: string; id?: string };
		} catch {
			return;
		}
		if (message.type === "response") {
			const response = message as unknown as RpcResponse;
			if (response.id) {
				const entry = this.pending.get(response.id);
				if (entry) {
					this.pending.delete(response.id);
					clearTimeout(entry.timer);
					entry.resolve(response);
					this.notifyIfIdle();
				}
			}
			return;
		}
		if (message.type === "extension_ui_request") {
			this.callbacks.onUiRequest(message as unknown as RpcExtensionUIRequest);
			return;
		}
		if (message.type === "extension_ui_cancel") {
			this.callbacks.onUiCancel((message as unknown as RpcExtensionUICancel).id);
			return;
		}
		if (message.type === "prigh_hello_failed") {
			this.helloFailed = true;
			this.callbacks.onHelloFailed((message as { error?: string }).error ?? "connection refused");
			return;
		}
		if (message.type === "prigh_set_user") {
			const user = (message as { user?: unknown }).user;
			if (typeof user === "string") this.callbacks.onSetUser(user);
			return;
		}
		this.callbacks.onEvent(message as unknown as AgentSessionEvent);
	}
}
