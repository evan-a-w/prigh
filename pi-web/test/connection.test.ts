import { describe, expect, it } from "vitest";
import { connectionFor, saveToken, type Storage, TOKEN_STORAGE_KEY } from "../src/connection.ts";

function memoryStorage(initial: Record<string, string> = {}): Storage & { data: Record<string, string> } {
	const data = { ...initial };
	return {
		data,
		getItem: (key) => data[key] ?? null,
		setItem: (key, value) => {
			data[key] = value;
		},
		removeItem: (key) => {
			delete data[key];
		},
	};
}

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

describe("saveToken", () => {
	it("stores a token and clears it when empty", () => {
		const storage = memoryStorage();
		saveToken(storage, "abc");
		expect(storage.data[TOKEN_STORAGE_KEY]).toBe("abc");
		saveToken(storage, "");
		expect(storage.data[TOKEN_STORAGE_KEY]).toBeUndefined();
	});
});
