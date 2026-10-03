/**
 * Where the WebSocket goes and what it identifies as. Pure functions over a
 * page location and a storage, so they are unit-tested without a browser.
 *
 * - `?backend=ws://host:port/ws` points the page at a backend elsewhere
 *   (default: this page's origin, `/ws`).
 * - `?session=ID` joins a saved session; `?name=` names this frontend.
 * - `?token=SECRET` is remembered in localStorage and removed from the
 *   address bar; the connect form saves it the same way.
 */

export const TOKEN_STORAGE_KEY = "prigh-pi-web:token";

export interface PageLocation {
	protocol: string;
	host: string;
	pathname: string;
	search: string;
}

export interface Storage {
	getItem(key: string): string | null;
	setItem(key: string, value: string): void;
	removeItem(key: string): void;
}

export interface Connection {
	url: string;
	session: string | undefined;
	/** Query string to reload the page with after the token was stashed (no token in it). */
	cleanedSearch: string | undefined;
}

function defaultBackend(location: PageLocation): string {
	const protocol = location.protocol === "https:" ? "wss" : "ws";
	const base = location.pathname.endsWith("/") ? location.pathname : `${location.pathname}/`;
	return `${protocol}://${location.host}${base}ws`;
}

export function connectionFor(location: PageLocation, storage: Storage): Connection {
	const params = new URLSearchParams(location.search);
	let cleanedSearch: string | undefined;
	const queryToken = params.get("token");
	if (queryToken !== null) {
		storage.setItem(TOKEN_STORAGE_KEY, queryToken);
		params.delete("token");
		const rest = params.toString();
		cleanedSearch = rest ? `?${rest}` : "";
	}
	const backend = params.get("backend") ?? defaultBackend(location);
	const url = new URL(backend);
	const token = storage.getItem(TOKEN_STORAGE_KEY);
	if (token) url.searchParams.set("token", token);
	const session = params.get("session") ?? undefined;
	if (session) url.searchParams.set("session", session);
	const name = params.get("name");
	if (name) url.searchParams.set("name", name);
	return { url: url.toString(), session, cleanedSearch };
}

/**
 * The backend's shell WebSocket next to the RPC one at `wsUrl` (same token),
 * keyed by `session` so it starts in that session's directory.
 */
export function terminalUrl(wsUrl: string, session: string | undefined): string {
	const rpc = new URL(wsUrl);
	const url = new URL(wsUrl);
	url.search = "";
	url.pathname = url.pathname.replace(/\/ws\/?$/, "").replace(/\/$/, "") + "/terminal";
	const token = rpc.searchParams.get("token");
	if (token) url.searchParams.set("token", token);
	if (session) url.searchParams.set("session", session);
	return url.toString();
}

export function saveToken(storage: Storage, token: string): void {
	if (token) storage.setItem(TOKEN_STORAGE_KEY, token);
	else storage.removeItem(TOKEN_STORAGE_KEY);
}
