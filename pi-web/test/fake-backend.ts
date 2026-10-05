/**
 * A stand-in for the prigh backend's pi-web WebSocket (backend/lib/pi_rpc.ml)
 * for tests of state.ts: `FakeSocket` replaces the global WebSocket and
 * `FakeBackend` answers the commands the way Pi_rpc does, per session.
 */
import type { AgentMessage, SubagentEvent, SubagentTranscript } from "../src/protocol.ts";
import type { AsyncStatusSnapshotNode } from "../src/subagent-status.ts";

type Json = Record<string, unknown>;

export class FakeSocket {
	static readonly CONNECTING = 0;
	static readonly OPEN = 1;
	static readonly CLOSING = 2;
	static readonly CLOSED = 3;
	static backend: FakeBackend | undefined;

	readyState = FakeSocket.CONNECTING;
	onopen: (() => void) | null = null;
	onmessage: ((event: { data: string }) => void) | null = null;
	onclose: (() => void) | null = null;
	onerror: (() => void) | null = null;

	constructor(readonly url: string) {
		FakeSocket.backend?.connected(this);
	}

	send(data: string): void {
		FakeSocket.backend?.received(this, JSON.parse(data) as Json);
	}

	close(): void {
		if (this.readyState === FakeSocket.CLOSED) return;
		this.readyState = FakeSocket.CLOSED;
		setTimeout(() => this.onclose?.(), 0);
	}

	/** The server's side. */
	push(message: Json): void {
		if (this.readyState === FakeSocket.OPEN) this.onmessage?.({ data: JSON.stringify(message) });
	}
}

export interface FakeSession {
	id: string;
	messages: AgentMessage[];
	steering: string[];
	followUp: string[];
	/** Root nodes of the agents rail. */
	subagents: AsyncStatusSnapshotNode[];
	confirms: Array<{ id: string; title: string; message: string }>;
	transcripts: Record<string, SubagentTranscript>;
}

export function session(id: string, overrides: Partial<FakeSession> = {}): FakeSession {
	return {
		id,
		messages: [{ role: "user", content: `hello from ${id}`, timestamp: 0 }],
		steering: [],
		followUp: [],
		subagents: [],
		confirms: [],
		transcripts: {},
		...overrides,
	};
}

export class FakeBackend {
	readonly sessions = new Map<string, FakeSession>();
	/** Every socket's URL, in connection order. */
	readonly urls: string[] = [];
	/** Commands received ("type" plus anything interesting). */
	readonly log: string[] = [];
	socket: FakeSocket | undefined;
	current: string;
	/** Users that /setusr accepts and that the hello accepts as as_user. */
	allowedAsUsers = new Set<string>();

	constructor(sessions: FakeSession[]) {
		for (const s of sessions) this.sessions.set(s.id, s);
		this.current = sessions[0].id;
	}

	get session(): FakeSession {
		const s = this.sessions.get(this.current);
		if (!s) throw new Error(`no session ${this.current}`);
		return s;
	}

	connected(socket: FakeSocket): void {
		this.socket = socket;
		this.urls.push(socket.url);
		const params = new URL(socket.url).searchParams;
		const wanted = params.get("session");
		if (wanted && this.sessions.has(wanted)) this.current = wanted;
		setTimeout(() => {
			socket.readyState = FakeSocket.OPEN;
			socket.onopen?.();
			const asUser = params.get("as_user");
			if (asUser && !this.allowedAsUsers.has(asUser)) {
				socket.push({ type: "prigh_hello_failed", error: `unauthorised: cannot act as ${asUser}` });
				socket.close();
			}
		}, 0);
	}

	/** Drops the connection as a network failure would. */
	disconnect(): void {
		this.socket?.close();
	}

	push(message: Json): void {
		this.socket?.push(message);
	}

	pushSubagentEvent(agentId: string, event: SubagentEvent): void {
		this.push({ type: "prigh_subagent_event", agentId, event });
	}

	widget(): Json {
		const runs = this.session.subagents;
		return {
			type: "extension_ui_request",
			id: "widget-subagents",
			method: "setWidget",
			widgetKey: "subagents",
			widgetLines:
				runs.length === 0
					? null
					: [
							`PI_SUBAGENT_ASYNC_JSON:${JSON.stringify({
								generatedAt: 1,
								omitted: { runs: 0, children: 0, byteLimitExceeded: false },
								runs,
							})}`,
						],
		};
	}

	received(socket: FakeSocket, command: Json): void {
		const type = String(command.type);
		if (type === "extension_ui_response") {
			this.log.push(`${type} ${String(command.id)}`);
			return;
		}
		const detail = command.path ?? command.agentId ?? command.toolCallId ?? command.message;
		this.log.push(detail === undefined ? type : `${type} ${String(detail)}`);
		const respond = (data: unknown, error?: string) =>
			socket.push(
				error === undefined
					? { type: "response", id: command.id, command: type, success: true, data }
					: { type: "response", id: command.id, command: type, success: false, error },
			);
		setTimeout(() => {
			const s = this.session;
			switch (type) {
				case "get_state":
					if (s.steering.length > 0 || s.followUp.length > 0) {
						socket.push({ type: "queue_update", steering: s.steering, followUp: s.followUp });
					}
					for (const confirm of s.confirms) {
						socket.push({ type: "extension_ui_request", method: "confirm", ...confirm });
					}
					if (s.subagents.length > 0) socket.push(this.widget());
					respond({
						cwd: "/",
						sessionId: s.id,
						sessionFile: `/${s.id}.jsonl`,
						isStreaming: false,
						messageCount: 1,
					});
					break;
				case "get_messages":
					respond({ messages: s.messages });
					break;
				case "get_commands":
					respond({ commands: [] });
					break;
				case "list_sessions":
					respond({ sessions: [], current: s.id });
					break;
				case "switch_session":
				case "new_session": {
					const target = type === "new_session" ? `new-${this.sessions.size}` : String(command.path);
					if (!this.sessions.has(target)) this.sessions.set(target, session(target));
					this.current = target;
					respond({});
					break;
				}
				case "watch_subagent": {
					const key = command.agentId ?? command.toolCallId;
					if (key === undefined) {
						respond({});
						break;
					}
					const transcript =
						s.transcripts[String(key)] ?? Object.values(s.transcripts).find((t) => t.subagent.callId === key);
					if (transcript) respond(transcript);
					else respond(undefined, `unknown subagent "${String(key)}"`);
					break;
				}
				case "prompt": {
					const match = /^\/setusr (\S+)$/.exec(String(command.message));
					if (match) {
						if (!this.allowedAsUsers.has(match[1])) {
							respond(undefined, `${match[1]} is not a user you may act as`);
							break;
						}
						socket.push({ type: "prigh_set_user", user: match[1] });
					}
					respond({});
					break;
				}
				default:
					respond(undefined, `command "${type}" is not supported by the fake`);
			}
		}, 0);
	}
}

/** Lets the fake's timers and the app's promises run. */
export async function settle(rounds = 10): Promise<void> {
	for (let i = 0; i < rounds; i++) await new Promise((resolve) => setTimeout(resolve, 0));
}
