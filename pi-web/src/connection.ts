/**
 * Where the WebSocket goes and what it identifies as. Pure functions over a
 * page location and a storage, so they are unit-tested without a browser.
 *
 * - `?backend=ws://host:port/ws` points the page at a backend elsewhere
 *   (default: this page's origin, `/ws`).
 * - `?session=ID` joins a saved session; `?name=` names this frontend.
 * - `?user=NAME` and `?token=PASSWORD` are remembered in localStorage and
 *   removed from the address bar; the login form saves them the same way.
 *   The user name selects the server's namespace (empty: no namespaces).
 */

export const TOKEN_STORAGE_KEY = "prigh-pi-web:token";
export const USER_STORAGE_KEY = "prigh-pi-web:user";

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
	/** Query string to reload the page with after the credentials were stashed (none in it). */
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
	for (const [param, key] of [
		["user", USER_STORAGE_KEY],
		["token", TOKEN_STORAGE_KEY],
	] as const) {
		const value = params.get(param);
		if (value === null) continue;
		store(storage, key, value);
		params.delete(param);
		const rest = params.toString();
		cleanedSearch = rest ? `?${rest}` : "";
	}
	const backend = params.get("backend") ?? defaultBackend(location);
	const url = new URL(backend);
	const { user, password } = loadCredentials(storage);
	if (user) url.searchParams.set("user", user);
	if (password) url.searchParams.set("token", password);
	const session = params.get("session") ?? undefined;
	if (session) url.searchParams.set("session", session);
	const name = params.get("name");
	if (name) url.searchParams.set("name", name);
	return { url: url.toString(), session, cleanedSearch };
}

/**
 * The backend's shell WebSocket next to the RPC one at `wsUrl` (same user and token),
 * keyed by `session` so it starts in that session's directory.
 */
export function terminalUrl(wsUrl: string, session: string | undefined): string {
	const rpc = new URL(wsUrl);
	const url = new URL(wsUrl);
	url.search = "";
	url.pathname = url.pathname.replace(/\/ws\/?$/, "").replace(/\/$/, "") + "/terminal";
	for (const param of ["user", "token"]) {
		const value = rpc.searchParams.get(param);
		if (value) url.searchParams.set(param, value);
	}
	if (session) url.searchParams.set("session", session);
	return url.toString();
}

function store(storage: Storage, key: string, value: string): void {
	if (value) storage.setItem(key, value);
	else storage.removeItem(key);
}

export interface Credentials {
	user: string;
	password: string;
}

export function loadCredentials(storage: Storage): Credentials {
	return {
		user: storage.getItem(USER_STORAGE_KEY) ?? "",
		password: storage.getItem(TOKEN_STORAGE_KEY) ?? "",
	};
}

export function saveCredentials(storage: Storage, { user, password }: Credentials): void {
	store(storage, USER_STORAGE_KEY, user);
	store(storage, TOKEN_STORAGE_KEY, password);
}

export function forgetCredentials(storage: Storage): void {
	saveCredentials(storage, { user: "", password: "" });
}

/**
 * The query string to load after signing in as `user`, given the current
 * one: a `?session=` belongs to whoever was signed in before, so it only
 * survives when the user name is unchanged.
 */
export function searchAfterLogin(search: string, previousUser: string, user: string): string {
	if (previousUser === user) return search;
	return searchWithoutSession(search);
}

export function searchWithoutSession(search: string): string {
	const params = new URLSearchParams(search);
	params.delete("session");
	const rest = params.toString();
	return rest ? `?${rest}` : "";
}
