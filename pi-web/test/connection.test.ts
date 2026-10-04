import { describe, expect, it } from "vitest";
import {
	connectionFor,
	forgetCredentials,
	loadCredentials,
	saveCredentials,
	searchAfterLogin,
	searchWithoutSession,
	TOKEN_STORAGE_KEY,
	terminalUrl,
	USER_STORAGE_KEY,
} from "../src/connection.ts";
import { memoryStorage } from "./memory-storage.ts";

const page = (search: string, overrides: Partial<Parameters<typeof connectionFor>[0]> = {}) => ({
	protocol: "http:",
	host: "127.0.0.1:7789",
	pathname: "/",
	search,
	...overrides,
});

describe("connectionFor", () => {
	it("defaults to this origin's /ws with nothing else", () => {
		expect(connectionFor(page(""), memoryStorage())).toEqual({
			url: "ws://127.0.0.1:7789/ws",
			session: undefined,
			cleanedSearch: undefined,
		});
	});

	it("uses wss under https and keeps the base path", () => {
		const connection = connectionFor(page("", { protocol: "https:", host: "host", pathname: "/pi" }), memoryStorage());
		expect(connection.url).toBe("wss://host/pi/ws");
	});

	it("stashes ?token in storage and asks for the address bar to be cleaned", () => {
		const storage = memoryStorage();
		const connection = connectionFor(page("?token=sek%26ret&session=abc"), storage);
		expect(storage.data[TOKEN_STORAGE_KEY]).toBe("sek&ret");
		expect(connection.cleanedSearch).toBe("?session=abc");
		expect(connection.session).toBe("abc");
		expect(connection.url).toBe("ws://127.0.0.1:7789/ws?token=sek%26ret&session=abc");
	});

	it("stashes ?user and ?token together", () => {
		const storage = memoryStorage({ [USER_STORAGE_KEY]: "old" });
		const connection = connectionFor(page("?user=bob&token=pw&name=n"), storage);
		expect(storage.data).toEqual({ [USER_STORAGE_KEY]: "bob", [TOKEN_STORAGE_KEY]: "pw" });
		expect(connection.cleanedSearch).toBe("?name=n");
		expect(connection.url).toBe("ws://127.0.0.1:7789/ws?user=bob&token=pw&name=n");
	});

	it("sends the stored user name alone (no password)", () => {
		const connection = connectionFor(page(""), memoryStorage({ [USER_STORAGE_KEY]: "a b" }));
		expect(connection.url).toBe("ws://127.0.0.1:7789/ws?user=a+b");
	});

	it("sends the stored token, session and name", () => {
		const storage = memoryStorage({ [TOKEN_STORAGE_KEY]: "t" });
		const connection = connectionFor(page("?session=s1&name=laptop"), storage);
		expect(connection.url).toBe("ws://127.0.0.1:7789/ws?token=t&session=s1&name=laptop");
		expect(connection.cleanedSearch).toBeUndefined();
	});

	it("?backend= points elsewhere", () => {
		const connection = connectionFor(page("?backend=ws%3A%2F%2Fother%3A1%2Fws&session=x"), memoryStorage());
		expect(connection.url).toBe("ws://other:1/ws?session=x");
	});
});

describe("credentials", () => {
	it("stores the user name and password under separate keys; empty ones are removed", () => {
		const storage = memoryStorage();
		saveCredentials(storage, { user: "alice", password: "pw" });
		expect(storage.data).toEqual({ [USER_STORAGE_KEY]: "alice", [TOKEN_STORAGE_KEY]: "pw" });
		expect(loadCredentials(storage)).toEqual({ user: "alice", password: "pw" });
		saveCredentials(storage, { user: "", password: "pw2" });
		expect(storage.data).toEqual({ [TOKEN_STORAGE_KEY]: "pw2" });
		expect(loadCredentials(storage)).toEqual({ user: "", password: "pw2" });
	});

	it("signing out forgets both and leaves other keys alone", () => {
		const storage = memoryStorage({ [USER_STORAGE_KEY]: "a", [TOKEN_STORAGE_KEY]: "b", "prigh-pi-web:theme": "light" });
		forgetCredentials(storage);
		expect(storage.data).toEqual({ "prigh-pi-web:theme": "light" });
		expect(connectionFor(page(""), storage).url).toBe("ws://127.0.0.1:7789/ws");
	});

	it("a session in the address bar belongs to the previous user", () => {
		expect(searchAfterLogin("?session=s1&name=x", "alice", "alice")).toBe("?session=s1&name=x");
		expect(searchAfterLogin("?session=s1&name=x", "alice", "bob")).toBe("?name=x");
		expect(searchAfterLogin("?session=s1", "", "bob")).toBe("");
		expect(searchWithoutSession("?backend=ws%3A%2F%2Fh%2Fws&session=s")).toBe("?backend=ws%3A%2F%2Fh%2Fws");
	});
});

describe("terminalUrl", () => {
	it("is /terminal next to /ws, keeping only the token, keyed by the session", () => {
		expect(terminalUrl("ws://127.0.0.1:7789/ws?token=sek%26ret&session=old&name=laptop", "s2")).toBe(
			"ws://127.0.0.1:7789/terminal?token=sek%26ret&session=s2",
		);
	});

	it("keeps the user name", () => {
		expect(terminalUrl("ws://h/ws?user=alice&token=pw&session=old", "s2")).toBe(
			"ws://h/terminal?user=alice&token=pw&session=s2",
		);
	});

	it("keeps the scheme and base path, and works without token or session", () => {
		expect(terminalUrl("wss://host/pi/ws", undefined)).toBe("wss://host/pi/terminal");
		expect(terminalUrl("ws://other:1/", "x")).toBe("ws://other:1/terminal?session=x");
	});
});
