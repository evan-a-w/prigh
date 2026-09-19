// Wire types mirroring backend/lib/rpc_json.ml, with runtime guards for
// everything that comes from the backend.

export type JsonValue = null | boolean | number | string | JsonValue[] | { [key: string]: JsonValue };
export type JsonObject = { [key: string]: JsonValue };

export interface Usage {
	input: number;
	output: number;
	cache_read: number;
}

export type StopReason =
	| { type: "end_turn" }
	| { type: "tool_use" }
	| { type: "length" }
	| { type: "aborted" }
	| { type: "error"; message: string };

export type Content =
	| { type: "text"; text: string }
	| { type: "thinking"; text: string }
	| { type: "tool_call"; id: string; name: string; arguments: string };

export interface UserMessage {
	role: "user";
	text: string;
}

export interface AssistantMessage {
	role: "assistant";
	content: Content[];
	stop_reason: StopReason;
	usage: Usage;
	model: string;
}

export interface ToolResultMessage {
	role: "tool_result";
	tool_call_id: string;
	tool_name: string;
	text: string;
	is_error: boolean;
}

export type Message = UserMessage | AssistantMessage | ToolResultMessage;

export type Delta =
	| { type: "text_delta"; text: string }
	| { type: "thinking_delta"; text: string }
	| { type: "thinking_signature" }
	| { type: "tool_call_start"; index: number; id: string; name: string }
	| { type: "tool_call_delta"; index: number; arguments: string };

export interface ToolCall {
	id: string;
	name: string;
	arguments: string;
}

export interface Model {
	id: string;
	provider: string;
	key: string;
	name: string;
	context_window: number;
	max_output: number;
	supports_thinking: boolean;
	cost: { input: number; output: number; cache_read: number };
}

export type Thinking = "off" | "on" | "low" | "high" | "max";

export interface State {
	session_id: string;
	session_path: string;
	cwd: string;
	model: Model;
	thinking: Thinking;
	running: boolean;
	message_count: number;
	usage: Usage;
	cost_usd: number;
	context_tokens: number;
}

export interface SessionSummary {
	id: string;
	path: string;
	cwd: string;
	created_at: string;
	first_prompt: string | null;
	message_count: number;
}

export type AuthMethod = "api_key" | "oauth";

export interface AuthStatus {
	provider: string;
	name: string;
	methods: { method: AuthMethod; label: string }[];
	configured: { method: AuthMethod; source: string } | null;
	expires_ms: number | null;
}

export type AuthPrompt =
	| { prompt: "secret"; message: string }
	| { prompt: "manual_code"; message: string; placeholder: string }
	| { prompt: "select"; message: string; options: { id: string; label: string }[] };

export type AuthEvent =
	| { kind: "auth_url"; url: string; instructions: string }
	| ({ kind: "prompt"; id: string } & AuthPrompt)
	| { kind: "prompt_cancelled"; id: string }
	| { kind: "progress"; message: string }
	| { kind: "done"; provider: string; method: AuthMethod }
	| { kind: "failed"; provider: string; error: string }
	| { kind: "logged_out"; provider: string };

export type Event =
	| { event: "agent_start" }
	| { event: "agent_end"; messages: Message[] }
	| { event: "turn_start" }
	| { event: "turn_end"; assistant: AssistantMessage; tool_results: ToolResultMessage[] }
	| { event: "message_start"; message: Message }
	| { event: "message_update"; partial: AssistantMessage; delta: Delta }
	| { event: "message_end"; message: Message }
	| { event: "tool_start"; call: ToolCall }
	| { event: "tool_output"; call_id: string; chunk: string }
	| { event: "tool_end"; call: ToolCall; result: ToolResultMessage }
	| { event: "state"; state: State }
	| { event: "compacted"; summary: string }
	| { event: "notice"; text: string }
	| ({ event: "auth" } & AuthEvent);

export type Response =
	| { type: "response"; id: JsonValue; ok: true; result: JsonValue }
	| { type: "response"; id: JsonValue; ok: false; error: string };

export type ServerMessage = Response | ({ type: "event" } & Event);

function isObject(v: unknown): v is JsonObject {
	return typeof v === "object" && v !== null && !Array.isArray(v);
}
const isString = (v: unknown): v is string => typeof v === "string";
const isNumber = (v: unknown): v is number => typeof v === "number";
const isBoolean = (v: unknown): v is boolean => typeof v === "boolean";

export function isUsage(v: unknown): v is Usage {
	return isObject(v) && isNumber(v.input) && isNumber(v.output) && isNumber(v.cache_read);
}

export function isStopReason(v: unknown): v is StopReason {
	if (!isObject(v) || !isString(v.type)) return false;
	switch (v.type) {
		case "end_turn":
		case "tool_use":
		case "length":
		case "aborted":
			return true;
		case "error":
			return isString(v.message);
		default:
			return false;
	}
}

export function isContent(v: unknown): v is Content {
	if (!isObject(v)) return false;
	switch (v.type) {
		case "text":
		case "thinking":
			return isString(v.text);
		case "tool_call":
			return isString(v.id) && isString(v.name) && isString(v.arguments);
		default:
			return false;
	}
}

export function isToolCall(v: unknown): v is ToolCall {
	return isObject(v) && isString(v.id) && isString(v.name) && isString(v.arguments);
}

export function isAssistantMessage(v: unknown): v is AssistantMessage {
	return (
		isObject(v) &&
		v.role === "assistant" &&
		Array.isArray(v.content) &&
		v.content.every(isContent) &&
		isStopReason(v.stop_reason) &&
		isUsage(v.usage) &&
		isString(v.model)
	);
}

export function isToolResultMessage(v: unknown): v is ToolResultMessage {
	return (
		isObject(v) &&
		v.role === "tool_result" &&
		isString(v.tool_call_id) &&
		isString(v.tool_name) &&
		isString(v.text) &&
		isBoolean(v.is_error)
	);
}

export function isMessage(v: unknown): v is Message {
	if (!isObject(v)) return false;
	if (v.role === "user") return isString(v.text);
	return isAssistantMessage(v) || isToolResultMessage(v);
}

export function isDelta(v: unknown): v is Delta {
	if (!isObject(v)) return false;
	switch (v.type) {
		case "text_delta":
		case "thinking_delta":
			return isString(v.text);
		case "thinking_signature":
			return true;
		case "tool_call_start":
			return isNumber(v.index) && isString(v.id) && isString(v.name);
		case "tool_call_delta":
			return isNumber(v.index) && isString(v.arguments);
		default:
			return false;
	}
}

export function isModel(v: unknown): v is Model {
	return (
		isObject(v) &&
		isString(v.id) &&
		isString(v.provider) &&
		isString(v.key) &&
		isString(v.name) &&
		isNumber(v.context_window) &&
		isNumber(v.max_output) &&
		isBoolean(v.supports_thinking) &&
		isObject(v.cost) &&
		isNumber(v.cost.input) &&
		isNumber(v.cost.output) &&
		isNumber(v.cost.cache_read)
	);
}

export const THINKING_LEVELS: readonly Thinking[] = ["off", "on", "low", "high", "max"];

export function isThinking(v: unknown): v is Thinking {
	return isString(v) && (THINKING_LEVELS as readonly string[]).includes(v);
}

export function isState(v: unknown): v is State {
	return (
		isObject(v) &&
		isString(v.session_id) &&
		isString(v.session_path) &&
		isString(v.cwd) &&
		isModel(v.model) &&
		isThinking(v.thinking) &&
		isBoolean(v.running) &&
		isNumber(v.message_count) &&
		isUsage(v.usage) &&
		isNumber(v.cost_usd) &&
		isNumber(v.context_tokens)
	);
}

export function isSessionSummary(v: unknown): v is SessionSummary {
	return (
		isObject(v) &&
		isString(v.id) &&
		isString(v.path) &&
		isString(v.cwd) &&
		isString(v.created_at) &&
		(v.first_prompt === null || isString(v.first_prompt)) &&
		isNumber(v.message_count)
	);
}

const isAuthMethod = (v: unknown): v is AuthMethod => v === "api_key" || v === "oauth";

export function isAuthStatus(v: unknown): v is AuthStatus {
	return (
		isObject(v) &&
		isString(v.provider) &&
		isString(v.name) &&
		Array.isArray(v.methods) &&
		v.methods.every((m) => isObject(m) && isAuthMethod(m.method) && isString(m.label)) &&
		(v.configured === null || (isObject(v.configured) && isAuthMethod(v.configured.method) && isString(v.configured.source))) &&
		(v.expires_ms === null || isNumber(v.expires_ms))
	);
}

function isAuthPrompt(v: JsonObject): boolean {
	switch (v.prompt) {
		case "secret":
			return isString(v.message);
		case "manual_code":
			return isString(v.message) && isString(v.placeholder);
		case "select":
			return isString(v.message) && Array.isArray(v.options) && v.options.every((o) => isObject(o) && isString(o.id) && isString(o.label));
		default:
			return false;
	}
}

export function isAuthEvent(v: unknown): v is AuthEvent {
	if (!isObject(v)) return false;
	switch (v.kind) {
		case "auth_url":
			return isString(v.url) && isString(v.instructions);
		case "prompt":
			return isString(v.id) && isAuthPrompt(v);
		case "prompt_cancelled":
			return isString(v.id);
		case "progress":
			return isString(v.message);
		case "done":
			return isString(v.provider) && isAuthMethod(v.method);
		case "failed":
			return isString(v.provider) && isString(v.error);
		case "logged_out":
			return isString(v.provider);
		default:
			return false;
	}
}

export function isEvent(v: unknown): v is Event {
	if (!isObject(v) || !isString(v.event)) return false;
	switch (v.event) {
		case "agent_start":
		case "turn_start":
			return true;
		case "agent_end":
			return Array.isArray(v.messages) && v.messages.every(isMessage);
		case "turn_end":
			return isAssistantMessage(v.assistant) && Array.isArray(v.tool_results) && v.tool_results.every(isToolResultMessage);
		case "message_start":
		case "message_end":
			return isMessage(v.message);
		case "message_update":
			return isAssistantMessage(v.partial) && isDelta(v.delta);
		case "tool_start":
			return isToolCall(v.call);
		case "tool_output":
			return isString(v.call_id) && isString(v.chunk);
		case "tool_end":
			return isToolCall(v.call) && isToolResultMessage(v.result);
		case "state":
			return isState(v.state);
		case "compacted":
			return isString(v.summary);
		case "notice":
			return isString(v.text);
		case "auth":
			return isAuthEvent(v);
		default:
			return false;
	}
}

export type ParseResult = { kind: "message"; message: ServerMessage } | { kind: "invalid"; error: string };

export function parseServerMessage(line: string): ParseResult {
	let value: unknown;
	try {
		value = JSON.parse(line);
	} catch (e) {
		return { kind: "invalid", error: `invalid JSON from backend: ${(e as Error).message}` };
	}
	if (!isObject(value)) return { kind: "invalid", error: "backend message is not an object" };
	if (value.type === "response") {
		if (value.ok === true)
			return { kind: "message", message: { type: "response", id: value.id ?? null, ok: true, result: value.result ?? null } };
		if (value.ok === false && isString(value.error))
			return { kind: "message", message: { type: "response", id: value.id ?? null, ok: false, error: value.error } };
		return { kind: "invalid", error: "malformed response" };
	}
	if (value.type === "event") {
		if (isEvent(value)) return { kind: "message", message: { type: "event", ...value } };
		return { kind: "invalid", error: `malformed event: ${String(value.event)}` };
	}
	return { kind: "invalid", error: `unknown message type: ${String(value.type)}` };
}
