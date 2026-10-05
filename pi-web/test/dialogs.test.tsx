// @vitest-environment happy-dom
import { render } from "preact";
import { act } from "preact/test-utils";
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { DialogHost } from "../src/components/dialogs.tsx";
import type { RpcExtensionUIRequest } from "../src/protocol.ts";
import { dialogQueue } from "../src/state.ts";

// What pi_rpc.ml sends for `/login custom`'s prompts.
const baseUrl: RpcExtensionUIRequest = {
	type: "extension_ui_request",
	id: "auth-p2",
	method: "input",
	title: "Base URL of aiproxy's API (the part before /chat/completions, usually ending in /v1)",
	placeholder: "http://localhost:3000/v1",
	prefill: "http://localhost:3000/v1",
};
const listFailed: RpcExtensionUIRequest = {
	type: "extension_ui_request",
	id: "auth-p5",
	method: "select",
	title:
		"Could not list aiproxy's models: connection refused\nCheck the base URL (it usually ends in /v1), the API key, and that the server is running.",
	options: ["Save anyway", "Change the settings", "Cancel (nothing is saved)"],
};

describe("dialogs", () => {
	let root: HTMLElement;
	beforeEach(() => {
		root = document.createElement("div");
		document.body.append(root);
	});
	afterEach(() => {
		render(null, root);
		root.remove();
		dialogQueue.value = [];
	});

	const show = (request: RpcExtensionUIRequest) => {
		act(() => {
			dialogQueue.value = [request];
			render(<DialogHost />, root);
		});
		return [...root.querySelectorAll(".dialog-title, input, button")]
			.map((e) =>
				e instanceof HTMLInputElement
					? `[input value=${e.value} placeholder=${e.placeholder}]`
					: e instanceof HTMLButtonElement
						? `[${e.textContent}]`
						: e.textContent,
			)
			.join("\n");
	};

	it("prefills an input dialog", () => {
		expect(show(baseUrl)).toMatchInlineSnapshot(`
			"Base URL of aiproxy's API (the part before /chat/completions, usually ending in /v1)
			[input value=http://localhost:3000/v1 placeholder=http://localhost:3000/v1]
			[Submit]
			[Cancel]"
		`);
		const { prefill: _, ...empty } = baseUrl as RpcExtensionUIRequest & { method: "input" };
		expect(show({ ...empty, id: "auth-p3" })).toContain("[input value= placeholder=http://localhost:3000/v1]");
	});

	it("keeps a multi-line title's lines", () => {
		expect(show(listFailed)).toMatchInlineSnapshot(`
			"Could not list aiproxy's models: connection refused
			Check the base URL (it usually ends in /v1), the API key, and that the server is running.
			[Save anyway]
			[Change the settings]
			[Cancel (nothing is saved)]
			[Cancel]"
		`);
	});
});
