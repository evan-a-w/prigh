// @vitest-environment happy-dom
// @vitest-environment-options {"url": "http://127.0.0.1:7789/?session=A"}
import { render } from "preact";
import { act } from "preact/test-utils";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import type { ImageContent } from "../src/protocol.ts";
import { FakeBackend, FakeSocket, session, settle } from "./fake-backend.ts";

const shrunk: ImageContent = { type: "image", mimeType: "image/png", data: "iVBORw0KGgo=" };

// The canvas part is tested on its own (image-file.test.ts).
vi.mock("../src/image-file.ts", () => ({
	prepareImageFile: async () => ({ ok: true, image: shrunk }),
}));

let root: HTMLElement;
let backend: FakeBackend;
let state: typeof import("../src/state.ts");
let commands: Record<string, unknown>[];

beforeEach(async () => {
	vi.stubGlobal("WebSocket", FakeSocket);
	localStorage.clear();
	history.replaceState(null, "", "/?session=A");
	backend = new FakeBackend([session("A")]);
	commands = [];
	const received = backend.received.bind(backend);
	backend.received = (socket, command) => {
		commands.push(command as Record<string, unknown>);
		received(socket, command);
	};
	FakeSocket.backend = backend;
	vi.resetModules();
	state = await import("../src/state.ts");
	state.client.start();
	await settle();
	root = document.createElement("div");
	document.body.appendChild(root);
	const { App } = await import("../src/app.tsx");
	act(() => {
		render(<App />, root);
	});
	await act(async () => {
		await settle();
	});
});

afterEach(() => {
	render(null, root);
	root.remove();
	state.client.stop();
	vi.unstubAllGlobals();
});

describe("pasted images", () => {
	it("are sent with the prompt, also without any text", async () => {
		const textarea = root.querySelector("textarea") as HTMLTextAreaElement;
		const paste = new Event("paste", { bubbles: true, cancelable: true });
		Object.defineProperty(paste, "clipboardData", {
			value: { files: [new File(["x"], "shot.png", { type: "image/png" })] },
		});
		await act(async () => {
			textarea.dispatchEvent(paste);
			await settle();
		});
		expect(root.querySelectorAll(".pending-image").length).toBe(1);
		await act(async () => {
			textarea.dispatchEvent(new KeyboardEvent("keydown", { key: "Enter", bubbles: true }));
			await settle();
		});
		const prompt = commands.find((c) => c.type === "prompt");
		expect(prompt).toMatchObject({ type: "prompt", message: "", images: [shrunk] });
		expect(root.querySelectorAll(".pending-image").length).toBe(0);
	});
});
