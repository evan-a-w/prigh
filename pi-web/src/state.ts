import { effect, signal } from "@preact/signals";
import { RpcClient } from "./client.ts";
import {
	connectionFor,
	forgetAsUser,
	forgetCredentials,
	loadAsUser,
	loadCredentials,
	saveAsUser,
	searchWithoutSession,
	terminalUrl,
} from "./connection.ts";
import type {
	AgentMessage,
	AgentSessionEvent,
	BashResult,
	ImageContent,
	Model,
	PrighSession,
	RpcExtensionUIRequest,
	RpcResponse,
	RpcSessionState,
	RpcSlashCommand,
	SessionStats,
	SubagentTranscript,
	ThinkingLevel,
} from "./protocol.ts";
import { type ProviderLogin, type ProviderLoginInput, reduce as reduceProviderLogin } from "./provider-login.ts";
import {
	ASYNC_STATUS_SNAPSHOT_WIDGET_PREFIX,
	type AsyncStatusSnapshot,
	countRunningNodes,
	parseAsyncStatusSnapshotWidgetLine,
} from "./subagent-status.ts";
import * as SubagentViews from "./subagent-view.ts";
import type { SubagentTarget, SubagentView } from "./subagent-view.ts";
import { applyToolEvent, rebuildToolStates, type ToolStates, upsertMessage } from "./transcript.ts";

export type { ToolDisplayState } from "./transcript.ts";

export interface Toast {
	id: number;
	message: string;
	kind: "info" | "warning" | "error";
}

export interface Widget {
	lines: string[];
	placement: "aboveEditor" | "belowEditor";
}

export const connected = signal(false);
/**
 * Set when the app shows the login form: the backend refused the connection
 * (see client.ts onHelloFailed), or "" after signing out.
 */
export const helloError = signal<string | undefined>(undefined);
export const sessionState = signal<RpcSessionState | undefined>(undefined);
export const messages = signal<AgentMessage[]>([]);
export const toolStates = signal<ToolStates>({});
export const stats = signal<SessionStats | undefined>(undefined);
export const slashCommands = signal<RpcSlashCommand[]>([]);
export const queue = signal<{ steering: readonly string[]; followUp: readonly string[] }>({
	steering: [],
	followUp: [],
});
export const workingMessage = signal<string | undefined>(undefined);
export const toasts = signal<Toast[]>([]);
export const dialogQueue = signal<RpcExtensionUIRequest[]>([]);
export const statusEntries = signal<Record<string, string>>({});
export const widgets = signal<Record<string, Widget>>({});
export const editorText = signal("");

/**
 * Latest subagent snapshot (see subagent-status.ts), extracted out of the
 * widget line carrying the PI_SUBAGENT_ASYNC_JSON: prefix (the prigh backend
 * publishes one per running subagent tool call). That line is never stored
 * in `widgets`.
 */
export const subagentSnapshot = signal<AsyncStatusSnapshot | undefined>(undefined);
let subagentSnapshotWidgetKey: string | undefined;

let nextToastId = 1;

export function pushToast(message: string, kind: Toast["kind"] = "info"): void {
	const id = nextToastId++;
	toasts.value = [...toasts.value, { id, message, kind }];
	setTimeout(() => {
		toasts.value = toasts.value.filter((toast) => toast.id !== id);
	}, 6_000);
}

// ============================================================================
// Client wiring
// ============================================================================

const initialConnection = connectionFor(location, localStorage);
if (initialConnection.cleanedSearch !== undefined) {
	history.replaceState(null, "", `${location.pathname}${initialConnection.cleanedSearch}${location.hash}`);
}

/** The user name this page signed in with ("" on servers without namespaces). */
export const currentUser = signal(loadCredentials(localStorage).user);
/** Whether there is anything to sign out of: a stored user name or password. */
export const signedIn = signal(Object.values(loadCredentials(localStorage)).some((value) => value !== ""));
/** The user this page acts as after `/setusr` ("" for the signed-in user). */
export const asUser = signal(loadAsUser(localStorage));

export const client = new RpcClient(() => connectionFor(location, localStorage).url, {
	onEvent: handleEvent,
	onUiRequest: handleUiRequest,
	onUiCancel: (id) => {
		feedProviderLogin({ kind: "ui_cancel", id });
		dialogQueue.value = dialogQueue.value.filter((queued) => queued.id !== id);
	},
	onConnectionChange: handleConnectionChange,
	onHelloFailed: handleHelloFailed,
	onSetUser: handleSetUser,
});

/** Sessions belong to a user: drops `?session=` from the address bar. */
function forgetSessionInUrl(): void {
	history.replaceState(null, "", `${location.pathname}${searchWithoutSession(location.search)}${location.hash}`);
}

/** Forgets the stored user name and password, disconnects and shows the login form. */
export function signOut(): void {
	forgetCredentials(localStorage);
	client.stop();
	currentUser.value = "";
	signedIn.value = false;
	asUser.value = "";
	providerLogin.value = undefined;
	dialogQueue.value = [];
	terminalOpen.value = false;
	forgetSessionInUrl();
	helloError.value = "";
}

/** `/setusr NAME` succeeded: reconnect acting as NAME (or as ourselves again). */
function handleSetUser(user: string): void {
	if (user === "" || user === currentUser.value) forgetAsUser(localStorage);
	else saveAsUser(localStorage, user);
	asUser.value = loadAsUser(localStorage);
	forgetSessionInUrl();
	pushToast(asUser.value ? `Acting as ${asUser.value}` : `Acting as ${currentUser.value || "yourself"} again`, "info");
	// The /setusr prompt's response follows on this connection.
	void client.whenIdle(2_000).then(() => client.reconnect());
}

/**
 * The backend refused the connection. Acting as another user, that is
 * probably no longer allowed: retry once as ourselves before asking to
 * sign in.
 */
function handleHelloFailed(error: string): void {
	const actingAs = asUser.value;
	if (actingAs) {
		forgetAsUser(localStorage);
		asUser.value = "";
		forgetSessionInUrl();
		pushToast(`Could not act as ${actingAs} (${error}); back to ${currentUser.value || "your own user"}`, "warning");
		client.reconnect();
		return;
	}
	helloError.value = error;
}

let syncing = false;
const eventBuffer: AgentSessionEvent[] = [];

function handleConnectionChange(isConnected: boolean): void {
	connected.value = isConnected;
	if (isConnected) {
		helloError.value = undefined;
		void sync();
	} else {
		// Dialogs are answered on the connection that asked; the next one
		// asks again for whatever is still pending.
		dialogQueue.value = [];
	}
}

export function dataAs<T>(response: RpcResponse, command: string): T | undefined {
	return response.success && response.command === command ? (response.data as T) : undefined;
}

/**
 * State the backend pushes for the current session only: get_state sends
 * it again (when not empty) for the session it answers for, so a re-sync
 * starts from nothing rather than from another session's.
 */
function resetSessionPushedState(): void {
	queue.value = { steering: [], followUp: [] };
	statusEntries.value = {};
	widgets.value = {};
	subagentSnapshot.value = undefined;
	subagentSnapshotWidgetKey = undefined;
}

export async function sync(): Promise<void> {
	syncing = true;
	const previousSessionId = sessionState.value?.sessionId;
	resetSessionPushedState();
	try {
		const [stateRes, messagesRes, commandsRes, statsRes] = await Promise.all([
			client.command({ type: "get_state" }),
			client.command({ type: "get_messages" }),
			client.command({ type: "get_commands" }),
			client.command({ type: "get_session_stats" }),
		]);
		const state = dataAs<RpcSessionState>(stateRes, "get_state");
		if (state) {
			sessionState.value = state;
			updateTitle(state.sessionName);
			rememberSession(state.sessionId);
			if (state.sessionId !== previousSessionId) sessionChanged();
		}
		const history = dataAs<{ messages: AgentMessage[] }>(messagesRes, "get_messages");
		if (history) {
			messages.value = history.messages;
			toolStates.value = rebuildToolStates(history.messages);
		}
		const commandList = dataAs<{ commands: RpcSlashCommand[] }>(commandsRes, "get_commands");
		if (commandList) {
			slashCommands.value = [
				...commandList.commands,
				{ name: "signout", description: "Sign out of this prigh server", source: "builtin" },
			];
		}
		const sessionStats = dataAs<SessionStats>(statsRes, "get_session_stats");
		if (sessionStats) {
			stats.value = sessionStats;
		}
		workingMessage.value = sessionState.value?.isStreaming ? "Working" : undefined;
		void refreshSessions();
		// Same session (a reconnect, a model change): the backend may have
		// forgotten what we watch.
		const view = subagentView.value;
		if (view && state && state.sessionId === previousSessionId) void watchSubagent(view.target);
	} catch (error) {
		// Lost the connection: the next one syncs again.
		if (client.connected)
			pushToast(`Failed to sync session state: ${error instanceof Error ? error.message : String(error)}`, "error");
	} finally {
		syncing = false;
		const buffered = eventBuffer.splice(0);
		for (const event of buffered) {
			applyEvent(event);
		}
	}
}

/** What only made sense in the previous session. */
function sessionChanged(): void {
	subagentView.value = undefined;
	commandResult.value = undefined;
}

function handleEvent(event: AgentSessionEvent): void {
	if (syncing) {
		eventBuffer.push(event);
		return;
	}
	applyEvent(event);
}

function updateTitle(sessionName: string | undefined): void {
	document.title = sessionName ? `prigh - ${sessionName}` : "prigh";
}

/** Keeps `?session=` in the address bar, so a reload (or a reconnect) rejoins this session. */
function rememberSession(sessionId: string): void {
	const params = new URLSearchParams(location.search);
	if (params.get("session") === sessionId) return;
	params.set("session", sessionId);
	history.replaceState(null, "", `${location.pathname}?${params.toString()}${location.hash}`);
}

// ============================================================================
// Event reduction
// ============================================================================

function refreshStats(): void {
	void client
		.command({ type: "get_session_stats" })
		.then((res) => {
			const sessionStats = dataAs<SessionStats>(res, "get_session_stats");
			if (sessionStats) {
				stats.value = sessionStats;
			}
		})
		.catch(() => {});
}

function applyEvent(event: AgentSessionEvent): void {
	switch (event.type) {
		case "message_start":
		case "message_end":
		case "message_update":
			if (!feedProviderLogin({ kind: "message", message: event.message })) {
				messages.value = upsertMessage(messages.value, event.message);
			}
			break;

		case "tool_execution_start":
		case "tool_execution_update":
		case "tool_execution_end":
			toolStates.value = applyToolEvent(toolStates.value, event);
			break;

		case "prigh_subagent_event":
			if (subagentView.value) {
				subagentView.value = SubagentViews.receive(subagentView.value, event.agentId, event.event);
			}
			break;

		case "agent_start":
			workingMessage.value = "Working";
			break;

		case "turn_end":
			refreshStats();
			break;

		case "agent_end":
			if (!event.willRetry) {
				refreshStats();
			}
			break;

		case "agent_settled":
			workingMessage.value = undefined;
			refreshStats();
			void refreshSessions();
			break;

		case "queue_update":
			queue.value = { steering: event.steering, followUp: event.followUp };
			break;

		case "compaction_start":
			workingMessage.value = "Compacting context";
			break;

		case "compaction_end":
			workingMessage.value = undefined;
			if (event.errorMessage) {
				pushToast(`Compaction failed: ${event.errorMessage}`, "error");
			} else if (!event.aborted) {
				if (event.result) {
					messages.value = [
						...messages.value,
						{
							role: "compactionSummary",
							summary: event.result.summary,
							tokensBefore: event.result.tokensBefore,
							timestamp: Date.now(),
						},
					];
				}
				pushToast("Context compacted", "info");
				refreshStats();
			}
			break;

		case "auto_retry_start":
			workingMessage.value = `Retrying (${event.attempt}/${event.maxAttempts})`;
			break;

		case "auto_retry_end":
			workingMessage.value = undefined;
			if (!event.success) {
				pushToast(event.finalError ?? "Retry failed", "error");
			}
			break;

		case "session_info_changed":
			if (sessionState.value) {
				sessionState.value = { ...sessionState.value, sessionName: event.name };
			}
			updateTitle(event.name);
			void refreshSessions();
			break;

		case "thinking_level_changed":
			if (sessionState.value) {
				sessionState.value = { ...sessionState.value, thinkingLevel: event.level };
			}
			break;

		case "extension_error":
			pushToast(`Extension error: ${event.error}`, "error");
			break;

		case "session_reloaded":
			// The backend moved this connection to another session, or changed
			// the model or cwd behind our back: re-sync everything.
			void sync();
			break;

		default:
			break;
	}
}

// ============================================================================
// Extension UI requests
// ============================================================================

/** The provider login (OAuth) dialog, see provider-login.ts. */
export const providerLogin = signal<ProviderLogin | undefined>(undefined);

function feedProviderLogin(input: ProviderLoginInput): boolean {
	const { login, consumed } = reduceProviderLogin(providerLogin.value, input);
	providerLogin.value = login;
	return consumed;
}

export function submitProviderLoginCode(code: string): void {
	const request = providerLogin.value?.request;
	if (!request) return;
	client.sendUiResponse({ type: "extension_ui_response", id: request.id, value: code });
	feedProviderLogin({ kind: "responded" });
}

export function closeProviderLogin(): void {
	const request = providerLogin.value?.request;
	if (request) client.sendUiResponse({ type: "extension_ui_response", id: request.id, cancelled: true });
	feedProviderLogin({ kind: "dismiss" });
}

function handleUiRequest(request: RpcExtensionUIRequest): void {
	if (feedProviderLogin({ kind: "ui_request", request })) return;
	switch (request.method) {
		case "select":
		case "confirm":
		case "input":
		case "editor":
			// The backend asks again for a confirmation still pending when we re-sync.
			dialogQueue.value = dialogQueue.value.some((queued) => queued.id === request.id)
				? dialogQueue.value.map((queued) => (queued.id === request.id ? request : queued))
				: [...dialogQueue.value, request];
			break;
		case "notify":
			pushToast(request.message, request.notifyType ?? "info");
			break;
		case "setStatus": {
			const next = { ...statusEntries.value };
			if (request.statusText) {
				next[request.statusKey] = request.statusText;
			} else {
				delete next[request.statusKey];
			}
			statusEntries.value = next;
			break;
		}
		case "setWidget": {
			const next = { ...widgets.value };
			if (request.widgetLines) {
				const displayLines: string[] = [];
				for (const line of request.widgetLines) {
					const snapshot = parseAsyncStatusSnapshotWidgetLine(line);
					if (snapshot) {
						subagentSnapshot.value = snapshot;
						subagentSnapshotWidgetKey = request.widgetKey;
						continue;
					}
					if (line.startsWith(ASYNC_STATUS_SNAPSHOT_WIDGET_PREFIX)) {
						continue;
					}
					displayLines.push(line);
				}
				if (displayLines.length > 0) {
					next[request.widgetKey] = {
						lines: displayLines,
						placement: request.widgetPlacement ?? "aboveEditor",
					};
				} else {
					delete next[request.widgetKey];
				}
			} else {
				delete next[request.widgetKey];
				if (subagentSnapshotWidgetKey === request.widgetKey) {
					subagentSnapshot.value = undefined;
					subagentSnapshotWidgetKey = undefined;
				}
			}
			widgets.value = next;
			break;
		}
		case "setTitle":
			document.title = request.title;
			break;
		case "set_editor_text":
			editorText.value = request.text;
			break;
	}
}

export function respondToDialog(
	request: RpcExtensionUIRequest,
	response: { value?: string; confirmed?: boolean; cancelled?: true },
): void {
	dialogQueue.value = dialogQueue.value.filter((queued) => queued.id !== request.id);
	if (response.cancelled) {
		client.sendUiResponse({ type: "extension_ui_response", id: request.id, cancelled: true });
	} else if (response.confirmed !== undefined) {
		client.sendUiResponse({ type: "extension_ui_response", id: request.id, confirmed: response.confirmed });
	} else if (response.value !== undefined) {
		client.sendUiResponse({ type: "extension_ui_response", id: request.id, value: response.value });
	} else {
		client.sendUiResponse({ type: "extension_ui_response", id: request.id, cancelled: true });
	}
}

// ============================================================================
// Outgoing actions
// ============================================================================

function reportFailure(response: RpcResponse, fallback: string): void {
	if (!response.success) {
		pushToast(response.error || fallback, "error");
	}
}

export async function sendPrompt(text: string, images: ImageContent[]): Promise<void> {
	const busy = sessionState.value?.isStreaming || workingMessage.value !== undefined;
	const response = await client.command({
		type: "prompt",
		message: text,
		...(images.length > 0 ? { images } : {}),
		...(busy ? { streamingBehavior: "steer" as const } : {}),
	});
	reportFailure(response, "Prompt rejected");
}

export async function sendAbort(): Promise<void> {
	const response = await client.command({ type: "abort" });
	reportFailure(response, "Abort failed");
}

/** Transient card shown at the bottom of the chat (e.g. /session output). */
export const commandResult = signal<{ title: string; markdown: string } | undefined>(undefined);
export const modelPickerOpen = signal(false);
export const forkPickerOpen = signal(false);

// ============================================================================
// Sessions sidebar
// ============================================================================

/**
 * Mobile off-canvas state for the left sidebar (hidden inline below 900px; see
 * .sidebar in style.css). Desktop ignores this - the sidebar is always inline
 * there.
 */
export const sidebarOpen = signal(false);

/** The backend's saved sessions, most recently updated first. */
export const sessions = signal<PrighSession[]>([]);

export async function refreshSessions(): Promise<void> {
	if (!client.connected) return;
	try {
		const response = await client.command({ type: "list_sessions" });
		const data = dataAs<{ sessions: PrighSession[] }>(response, "list_sessions");
		if (data) sessions.value = data.sessions;
	} catch {
		// Best-effort: the sidebar keeps its last-known list.
	}
}

export async function switchSession(path: string): Promise<void> {
	sidebarOpen.value = false;
	const response = await client.command({ type: "switch_session", path });
	if (!response.success) {
		reportFailure(response, "Failed to switch session");
		return;
	}
	await sync();
}

export async function newSession(): Promise<void> {
	sidebarOpen.value = false;
	const response = await client.command({ type: "new_session" });
	if (!response.success) {
		reportFailure(response, "Failed to start new session");
		return;
	}
	await sync();
}

// ============================================================================
// Agents rail (live subagent activity beside the chat, see agents-rail.tsx)
// ============================================================================

const AGENTS_RAIL_STORAGE_KEY = "prigh-pi-web:agents-rail-open";

function loadStoredAgentsRailOpen(): boolean | undefined {
	try {
		const raw = localStorage.getItem(AGENTS_RAIL_STORAGE_KEY);
		if (raw === "true") return true;
		if (raw === "false") return false;
		return undefined;
	} catch {
		return undefined;
	}
}

const storedAgentsRailOpen = loadStoredAgentsRailOpen();
export const agentsRailOpen = signal(storedAgentsRailOpen ?? false);
// Skip auto-open once the user has an explicit stored preference either way.
let agentsRailAutoOpened = storedAgentsRailOpen !== undefined;

export const terminalOpen = signal(false);

// ============================================================================
// Subagent view (a subagent's conversation in place of the chat, see subagent-panel.tsx)
// ============================================================================

export const subagentView = signal<SubagentView | undefined>(undefined);

async function watchSubagent(target: SubagentTarget): Promise<void> {
	let response: RpcResponse;
	try {
		response = await client.command({ type: "watch_subagent", ...target });
	} catch (error) {
		response = { type: "response", command: "watch_subagent", success: false, error: String(error) };
	}
	const view = subagentView.value;
	if (!view || view.target !== target) return;
	const transcript = dataAs<SubagentTranscript>(response, "watch_subagent");
	subagentView.value = transcript
		? SubagentViews.loaded(view, transcript)
		: SubagentViews.failed(view, response.success ? "no transcript" : response.error);
}

/** Shows a subagent's conversation (live while it runs) instead of the chat. */
export async function openSubagent(target: SubagentTarget): Promise<void> {
	if (SubagentViews.sameTarget(subagentView.value, target)) return;
	subagentView.value = SubagentViews.openView(target);
	if (window.matchMedia?.("(max-width: 900px)").matches) agentsRailOpen.value = false;
	await watchSubagent(target);
}

export function closeSubagent(): void {
	if (!subagentView.value) return;
	subagentView.value = undefined;
	if (client.connected) void client.command({ type: "watch_subagent" }).catch(() => {});
}

/** Where the terminal panel connects: the current session's shell on this backend. */
export function currentTerminalUrl(): string {
	return terminalUrl(connectionFor(location, localStorage).url, sessionState.value?.sessionId);
}

export function toggleAgentsRail(): void {
	agentsRailOpen.value = !agentsRailOpen.value;
	try {
		localStorage.setItem(AGENTS_RAIL_STORAGE_KEY, String(agentsRailOpen.value));
	} catch {
		// Best-effort; the toggle still works for the current page load.
	}
}

// Auto-reveal the rail the first time live subagent activity shows up.
effect(() => {
	if (agentsRailAutoOpened) return;
	if (countRunningNodes(subagentSnapshot.value) > 0) {
		agentsRailAutoOpened = true;
		agentsRailOpen.value = true;
	}
});

function formatTokenCount(count: number): string {
	if (count >= 1_000_000) return `${(count / 1_000_000).toFixed(1)}M`;
	if (count >= 1_000) return `${(count / 1_000).toFixed(1)}k`;
	return String(count);
}

/**
 * Run a shell command (! prefix in the editor). The backend records it as a
 * user message and streams that back, so nothing is mirrored locally.
 */
export async function sendBash(command: string): Promise<void> {
	if (command.trim() === "") return;
	const response = await client.command({ type: "bash", command });
	if (!response.success) {
		reportFailure(response, "Bash failed");
		return;
	}
	const result = dataAs<BashResult>(response, "bash");
	if (result && result.exitCode !== undefined && result.exitCode !== 0) {
		pushToast(`Command failed (exit code ${result.exitCode})`, "warning");
	}
	refreshStats();
}

async function setModelByQuery(query: string): Promise<void> {
	const response = await client.command({ type: "get_available_models" });
	const models = dataAs<{ models: Model[] }>(response, "get_available_models")?.models ?? [];
	const normalized = query.toLowerCase();
	const match =
		models.find((model) => `${model.provider}/${model.id}`.toLowerCase() === normalized) ??
		models.find((model) => model.id.toLowerCase() === normalized) ??
		models.find((model) => model.name.toLowerCase() === normalized) ??
		models.find(
			(model) =>
				model.id.toLowerCase().includes(normalized) ||
				model.name.toLowerCase().includes(normalized) ||
				`${model.provider}/${model.id}`.toLowerCase().includes(normalized),
		);
	if (!match) {
		pushToast(`No model matching "${query}"`, "error");
		return;
	}
	const setResponse = await client.command({ type: "set_model", provider: match.provider, modelId: match.id });
	if (!setResponse.success) {
		reportFailure(setResponse, "Failed to set model");
		return;
	}
	pushToast(`Model: ${match.name}`, "info");
	await sync();
}

async function showSessionInfo(): Promise<void> {
	const [statsResponse, stateResponse] = await Promise.all([
		client.command({ type: "get_session_stats" }),
		client.command({ type: "get_state" }),
	]);
	const sessionStats = dataAs<SessionStats>(statsResponse, "get_session_stats");
	const state = dataAs<RpcSessionState>(stateResponse, "get_state");
	if (!sessionStats || !state) {
		pushToast("Failed to load session info", "error");
		return;
	}
	const lines = [
		state.sessionName ? `**${state.sessionName}**` : undefined,
		`Session: \`${sessionStats.sessionId}\``,
		sessionStats.sessionFile ? `File: \`${sessionStats.sessionFile}\`` : undefined,
		`Directory: \`${state.cwd}\``,
		state.model ? `Model: ${state.model.name} · thinking ${state.thinkingLevel}` : undefined,
		`Messages: ${sessionStats.totalMessages} (${sessionStats.userMessages} user, ${sessionStats.assistantMessages} assistant, ${sessionStats.toolCalls} tool calls)`,
		`Tokens: ${formatTokenCount(sessionStats.tokens.total)} total (${formatTokenCount(sessionStats.tokens.input)} in, ${formatTokenCount(sessionStats.tokens.output)} out, ${formatTokenCount(sessionStats.tokens.cacheRead)} cache read)`,
		`Cost: $${sessionStats.cost.toFixed(4)}`,
		sessionStats.contextUsage?.percent !== null && sessionStats.contextUsage?.percent !== undefined
			? `Context: ${sessionStats.contextUsage.percent}% of ${formatTokenCount(sessionStats.contextUsage.contextWindow)}`
			: undefined,
	].filter((line): line is string => line !== undefined);
	commandResult.value = { title: "Session", markdown: lines.join("\n\n") };
}

async function forkFromEntry(entryId: string): Promise<void> {
	const response = await client.command({ type: "fork", entryId });
	if (!response.success) {
		reportFailure(response, "Fork failed");
		return;
	}
	const result = dataAs<{ text?: string; cancelled: boolean }>(response, "fork");
	if (result?.text) {
		// Like the TUI: the forked message text goes into the editor for editing
		editorText.value = result.text;
	}
	pushToast("Forked to new session", "info");
	await sync();
}

export async function selectForkEntry(entryId: string): Promise<void> {
	forkPickerOpen.value = false;
	await forkFromEntry(entryId);
}

export async function selectModel(provider: string, modelId: string): Promise<void> {
	modelPickerOpen.value = false;
	const response = await client.command({ type: "set_model", provider, modelId });
	if (!response.success) {
		reportFailure(response, "Failed to set model");
		return;
	}
	await sync();
}

/**
 * `/thinking <level>` sets it; bare `/thinking` cycles to the next level.
 * The available levels come from the backend per model.
 */
export async function setThinkingLevelCommand(args: string): Promise<void> {
	if (!args) {
		const response = await client.command({ type: "cycle_thinking_level" });
		if (!response.success) {
			reportFailure(response, "Failed to change thinking level");
			return;
		}
		const data = dataAs<{ level: ThinkingLevel } | null>(response, "cycle_thinking_level");
		if (!data) {
			pushToast("This model does not support thinking levels", "info");
			return;
		}
		if (sessionState.value) sessionState.value = { ...sessionState.value, thinkingLevel: data.level };
		pushToast(`Thinking level: ${data.level}`, "info");
		return;
	}
	const available = await client.command({ type: "get_available_thinking_levels" });
	const levels = dataAs<{ levels: ThinkingLevel[] }>(available, "get_available_thinking_levels")?.levels ?? [];
	const wanted = args.toLowerCase() as ThinkingLevel;
	if (!levels.includes(wanted)) {
		pushToast(`"${args}" is not available for this model (use ${levels.join(", ") || "off"})`, "error");
		return;
	}
	const response = await client.command({ type: "set_thinking_level", level: wanted });
	if (!response.success) {
		reportFailure(response, "Failed to set thinking level");
		return;
	}
	const data = dataAs<{ level: ThinkingLevel }>(response, "set_thinking_level");
	const level = data?.level ?? wanted;
	if (sessionState.value) sessionState.value = { ...sessionState.value, thinkingLevel: level };
	pushToast(`Thinking level: ${level}`, "info");
}

/**
 * Execute a builtin slash command (/compact, /new, /model, ...). Returns true
 * when the command was handled here; false when it should go through `prompt`
 * (the backend runs /login, /sessions, /host, ... itself).
 */
export async function executeBuiltinCommand(text: string): Promise<boolean> {
	const spaceIndex = text.indexOf(" ");
	const name = spaceIndex === -1 ? text.slice(1) : text.slice(1, spaceIndex);
	const args = spaceIndex === -1 ? "" : text.slice(spaceIndex + 1).trim();

	switch (name) {
		case "compact": {
			const response = await client.command({ type: "compact", ...(args ? { customInstructions: args } : {}) });
			reportFailure(response, "Compaction failed");
			return true;
		}
		case "new": {
			await newSession();
			return true;
		}
		case "name": {
			if (!args) {
				pushToast("Usage: /name <session name>", "error");
				return true;
			}
			const response = await client.command({ type: "set_session_name", name: args });
			reportFailure(response, "Failed to set session name");
			return true;
		}
		case "model": {
			if (args) {
				await setModelByQuery(args);
			} else {
				modelPickerOpen.value = true;
			}
			return true;
		}
		case "thinking":
		case "effort": {
			await setThinkingLevelCommand(args);
			return true;
		}
		case "session": {
			await showSessionInfo();
			return true;
		}
		case "export": {
			const response = await client.command({ type: "export_html", ...(args ? { outputPath: args } : {}) });
			if (!response.success) {
				reportFailure(response, "Export failed");
				return true;
			}
			const exported = dataAs<{ path: string }>(response, "export_html");
			pushToast(`Exported to ${exported?.path ?? "the sessions directory"} (on the backend)`, "info");
			return true;
		}
		case "copy": {
			const response = await client.command({ type: "get_last_assistant_text" });
			const result = dataAs<{ text: string | null }>(response, "get_last_assistant_text");
			if (!result?.text) {
				pushToast("No agent message to copy", "error");
				return true;
			}
			try {
				await navigator.clipboard.writeText(result.text);
				pushToast("Copied last agent message", "info");
			} catch {
				pushToast("Clipboard unavailable (requires https or localhost)", "error");
			}
			return true;
		}
		case "fork": {
			forkPickerOpen.value = true;
			return true;
		}
		case "clone": {
			const response = await client.command({ type: "clone" });
			if (!response.success) {
				reportFailure(response, "Clone failed");
				return true;
			}
			pushToast("Cloned session", "info");
			await sync();
			return true;
		}
		case "signout": {
			signOut();
			return true;
		}
		case "cd": {
			if (!args) {
				pushToast(`Working directory: ${sessionState.value?.cwd ?? "unknown"}`, "info");
				return true;
			}
			const response = await client.command({ type: "change_cwd", cwd: args });
			if (!response.success) {
				reportFailure(response, "Failed to change working directory");
				return true;
			}
			const changed = dataAs<{ cancelled: boolean; cwd: string }>(response, "change_cwd");
			pushToast(`Working directory: ${changed?.cwd ?? args}`, "info");
			await sync();
			return true;
		}
		default:
			return false;
	}
}
