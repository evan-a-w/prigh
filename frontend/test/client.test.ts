import assert from "node:assert/strict";
import { test } from "node:test";
import { PassThrough } from "node:stream";
import { Client, LineSplitter, RpcError, type Transport } from "../src/client.js";
import type { Event } from "../src/protocol.js";

test("LineSplitter handles partial chunks and blank lines", () => {
	const s = new LineSplitter();
	assert.deepEqual(s.push('{"a":1}\n{"b'), ['{"a":1}']);
	assert.deepEqual(s.push('":2}\n\n\n'), ['{"b":2}']);
	assert.deepEqual(s.push("tail"), []);
	assert.deepEqual(s.flush(), ["tail"]);
	assert.deepEqual(s.flush(), []);
});

interface Fake {
	client: Client;
	received: string[];
	send(line: string): void;
	end(): void;
}

function fakeBackend(): Fake {
	const stdin = new PassThrough();
	const stdout = new PassThrough();
	const received: string[] = [];
	stdin.setEncoding("utf8");
	stdin.on("data", (chunk: string) => {
		for (const line of chunk.split("\n")) if (line.trim()) received.push(line);
	});
	let onExit: (code: number | null) => void = () => {};
	const transport: Transport = {
		stdin,
		stdout,
		onExit: (f) => {
			onExit = f;
		},
		kill: () => onExit(-1),
	};
	return {
		client: new Client(transport),
		received,
		send: (line) => stdout.write(`${line}\n`),
		end: () => {
			stdout.end();
			onExit(0);
		},
	};
}

const tick = (): Promise<void> => new Promise((r) => setImmediate(r));

test("requests are correlated by id; errors reject; events dispatch", async () => {
	const fake = fakeBackend();
	const events: Event[] = [];
	fake.client.subscribe((e) => events.push(e));
	const p1 = fake.client.call("ping");
	const p2 = fake.client.call("prompt", { text: "hi" });
	await tick();
	assert.deepEqual(fake.received.map((l) => JSON.parse(l)), [
		{ id: 1, method: "ping", params: {} },
		{ id: 2, method: "prompt", params: { text: "hi" } },
	]);
	fake.send('{"type":"event","event":"notice","text":"n1"}');
	fake.send('{"type":"response","id":2,"ok":false,"error":"busy"}');
	fake.send('{"type":"response","id":1,"ok":true,"result":"pong"}');
	assert.equal(await p1, "pong");
	await assert.rejects(p2, (e: unknown) => e instanceof RpcError && e.message === "busy");
	assert.deepEqual(events, [{ event: "notice", text: "n1" }]);
});

test("protocol errors are reported, not thrown; close rejects pending", async () => {
	const fake = fakeBackend();
	const errors: string[] = [];
	fake.client.onProtocolError = (m) => errors.push(m);
	fake.send("garbage");
	fake.send('{"type":"response","id":99,"ok":true,"result":null}');
	await tick();
	assert.equal(errors.length, 2);
	const pending = fake.client.call("ping");
	fake.end();
	await assert.rejects(pending, /backend closed/);
	assert.equal(await fake.client.exited, 0);
	await assert.rejects(fake.client.call("ping"), /backend closed/);
});

test("typed wrappers validate results", async () => {
	const fake = fakeBackend();
	const p = fake.client.getState();
	await tick();
	fake.send('{"type":"response","id":1,"ok":true,"result":{"nope":1}}');
	await assert.rejects(p, /malformed state/);
	const m = fake.client.getMessages();
	await tick();
	fake.send('{"type":"response","id":2,"ok":true,"result":[{"role":"user","text":"x"}]}');
	assert.deepEqual(await m, [{ role: "user", text: "x" }]);
});
