// @vitest-environment happy-dom
import { render } from "preact";
import { act } from "preact/test-utils";
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { capsLockFromEvent } from "../src/caps-lock.ts";
import { LoginView } from "../src/components/login-view.tsx";
import { TOKEN_STORAGE_KEY, USER_STORAGE_KEY } from "../src/connection.ts";
import { memoryStorage } from "./memory-storage.ts";

let root: HTMLElement;
beforeEach(() => {
	root = document.createElement("div");
	document.body.appendChild(root);
});
afterEach(() => {
	render(null, root);
	root.remove();
});

const input = (name: string) => root.querySelector<HTMLInputElement>(`input[name=${name}]`) as HTMLInputElement;

function type(element: HTMLInputElement, value: string) {
	act(() => {
		element.value = value;
		element.dispatchEvent(new Event("input", { bubbles: true }));
	});
}

function key(element: HTMLInputElement, type: "keydown" | "keyup", capsLock: boolean) {
	const event = new KeyboardEvent(type, { key: "a", bubbles: true });
	Object.defineProperty(event, "getModifierState", { value: (k: string) => k === "CapsLock" && capsLock });
	act(() => {
		element.dispatchEvent(event);
	});
}

const view = () =>
	[...root.querySelectorAll("h1, p, label > span, input, button")]
		.map((e) =>
			e instanceof HTMLInputElement ? `[${e.name}:${e.type}=${JSON.stringify(e.value)}]` : (e.textContent ?? ""),
		)
		.join("\n");

function mount(error: string, storage: ReturnType<typeof memoryStorage>, search = "") {
	const logins: string[] = [];
	act(() => {
		render(<LoginView error={error} storage={storage} search={search} onLogin={(s) => logins.push(s)} />, root);
	});
	return logins;
}

describe("LoginView", () => {
	it("asks for a user name and a password, prefilling the remembered user name", () => {
		mount("unauthorised: bad user name or password", memoryStorage({ [USER_STORAGE_KEY]: "alice" }));
		expect(view()).toMatchInlineSnapshot(`
			"Sign in to prigh
			unauthorised: bad user name or password
			User name
			[user:text="alice"]
			Password
			[password:password=""]
			Sign in"
		`);
	});

	it("shows no error after signing out", () => {
		mount("", memoryStorage());
		expect(root.querySelector(".connect-error")).toBeNull();
	});

	it("stores both credentials and keeps the session for the same user", () => {
		const storage = memoryStorage({ [USER_STORAGE_KEY]: "alice" });
		const logins = mount("bad", storage, "?session=s1");
		type(input("password"), " pw ");
		act(() => {
			root.querySelector("form")?.dispatchEvent(new Event("submit", { bubbles: true, cancelable: true }));
		});
		expect(storage.data).toEqual({ [USER_STORAGE_KEY]: "alice", [TOKEN_STORAGE_KEY]: "pw" });
		expect(logins).toEqual(["?session=s1"]);
	});

	it("a different user drops the previous user's session; an empty user name is allowed", () => {
		const storage = memoryStorage({ [USER_STORAGE_KEY]: "alice", [TOKEN_STORAGE_KEY]: "old" });
		const logins = mount("bad", storage, "?session=s1&name=n");
		type(input("user"), "");
		type(input("password"), "tok");
		act(() => {
			root.querySelector("form")?.dispatchEvent(new Event("submit", { bubbles: true, cancelable: true }));
		});
		expect(storage.data).toEqual({ [TOKEN_STORAGE_KEY]: "tok" });
		expect(logins).toEqual(["?name=n"]);
	});

	it("warns about Caps Lock in the password field until it is off or the field loses focus", () => {
		mount("", memoryStorage());
		const password = input("password");
		const warning = () => root.querySelector(".caps-lock-warning")?.textContent ?? "(none)";
		const states: string[] = [];
		key(password, "keydown", false);
		states.push(`a: ${warning()}`);
		key(password, "keydown", true);
		states.push(`A: ${warning()}`);
		key(password, "keyup", false);
		states.push(`caps off: ${warning()}`);
		key(password, "keyup", true);
		act(() => {
			password.dispatchEvent(new FocusEvent("blur"));
		});
		states.push(`blur: ${warning()}`);
		key(input("user"), "keydown", true);
		states.push(`in the user field: ${warning()}`);
		expect(states).toEqual([
			"a: (none)",
			"A: Caps Lock is on",
			"caps off: (none)",
			"blur: (none)",
			"in the user field: (none)",
		]);
	});
});

describe("capsLockFromEvent", () => {
	it("reads the modifier state, unknown for events without one", () => {
		const keyEvent = new KeyboardEvent("keydown", { key: "A" });
		Object.defineProperty(keyEvent, "getModifierState", { value: (k: string) => k === "CapsLock" });
		expect(capsLockFromEvent(keyEvent)).toBe(true);
		expect(capsLockFromEvent(new KeyboardEvent("keydown", { key: "a" }))).toBe(false);
		expect(capsLockFromEvent(new FocusEvent("focus"))).toBeUndefined();
	});
});
