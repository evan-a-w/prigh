import assert from "node:assert/strict";
import { test } from "node:test";
import { renderInline, renderMarkdown } from "../src/markdown.js";

test("plain output without color is unchanged", () => {
	const src = "# Title\n- item **bold** `code`\n```\nx = 1\n```\n> quote";
	assert.equal(renderMarkdown(src, { color: false, width: 80 }), "Title\n• item **bold** `code`\n```\nx = 1\n```\n│ quote");
});

test("inline styles with color", () => {
	assert.equal(renderInline("a `b` c", true), "a \x1b[36mb\x1b[0m c");
	assert.equal(renderInline("**bold**", true), "\x1b[1mbold\x1b[0m");
	assert.equal(renderInline("*it*", true), "\x1b[3mit\x1b[0m");
	assert.equal(renderInline("2 * 3 * 4", true), "2 * 3 * 4");
	assert.equal(renderInline("snake_case_name", true), "snake_case_name");
});

test("code fences are not inline-rendered", () => {
	const out = renderMarkdown("```\n**not bold**\n```", { color: true, width: 80 });
	assert.ok(out.includes("**not bold**"));
});
