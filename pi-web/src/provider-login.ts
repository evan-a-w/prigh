import type { AgentMessage, RpcExtensionUIRequest } from "./protocol.ts";

/**
 * An OAuth login to a model provider (`/login anthropic`), shown as one
 * dialog. The backend (pi_rpc.ml) sends the pieces separately: a custom
 * `login` chat message with the instructions and a markdown link to the
 * authorization URL, an `input` dialog with an `auth-` id asking for the code
 * (cancelled when the loopback redirect delivers it first), and `notify`
 * messages for progress, failure ("login to P failed: ...") and success
 * ("logged in to P ..."). `reduce` folds them into the dialog's state.
 */
export interface ProviderLogin {
	url: string;
	instructions: string;
	/** The pending code prompt, if the backend is asking for one. */
	request?: RpcExtensionUIRequest & { method: "input" };
	progress?: string;
	error?: string;
}

export type ProviderLoginInput =
	| { kind: "message"; message: AgentMessage }
	| { kind: "ui_request"; request: RpcExtensionUIRequest }
	| { kind: "ui_cancel"; id: string }
	| { kind: "responded" }
	| { kind: "dismiss" };

export interface Reduced {
	login: ProviderLogin | undefined;
	/** The input belongs to the login dialog: don't also put it in the chat, dialog queue or toasts. */
	consumed: boolean;
}

const LINK = /\[[^\]]*\]\((\S+?)\)\s*$/;

export function authorizationLink(message: AgentMessage): { url: string; instructions: string } | undefined {
	if (message.role !== "custom" || message.customType !== "login" || typeof message.content !== "string") {
		return undefined;
	}
	const match = LINK.exec(message.content);
	if (!match) return undefined;
	return { url: match[1], instructions: message.content.slice(0, match.index).trim() };
}

export const isAuthPrompt = (request: RpcExtensionUIRequest): request is RpcExtensionUIRequest & { method: "input" } =>
	request.method === "input" && request.id.startsWith("auth-");

export function reduce(login: ProviderLogin | undefined, input: ProviderLoginInput): Reduced {
	const unchanged = { login, consumed: false };
	switch (input.kind) {
		case "message": {
			const link = authorizationLink(input.message);
			return link ? { login: link, consumed: true } : unchanged;
		}
		case "ui_request": {
			const { request } = input;
			if (!login) return unchanged;
			if (isAuthPrompt(request)) {
				return { login: { ...login, request, error: undefined }, consumed: true };
			}
			if (request.method !== "notify") return unchanged;
			if (request.notifyType === "error") {
				return { login: { ...login, request: undefined, progress: undefined, error: request.message }, consumed: true };
			}
			if (/^logged in to /.test(request.message)) return { login: undefined, consumed: false };
			return { login: { ...login, progress: request.message }, consumed: true };
		}
		case "ui_cancel":
			if (login?.request?.id !== input.id) return unchanged;
			return { login: { ...login, request: undefined }, consumed: true };
		case "responded":
			if (!login) return unchanged;
			return { login: { ...login, request: undefined, error: undefined, progress: "Checking the code…" }, consumed: true };
		case "dismiss":
			return { login: undefined, consumed: true };
	}
}
