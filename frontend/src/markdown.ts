// A small markdown-to-ANSI renderer: headings, fenced code, lists,
// blockquotes, inline code, bold, italic. Enough for assistant replies.

export const ansi = {
	reset: "\x1b[0m",
	bold: "\x1b[1m",
	dim: "\x1b[2m",
	italic: "\x1b[3m",
	underline: "\x1b[4m",
	red: "\x1b[31m",
	green: "\x1b[32m",
	yellow: "\x1b[33m",
	blue: "\x1b[34m",
	magenta: "\x1b[35m",
	cyan: "\x1b[36m",
	gray: "\x1b[90m",
};

export function renderInline(text: string, color: boolean): string {
	if (!color) return text;
	let out = "";
	let i = 0;
	while (i < text.length) {
		if (text[i] === "`") {
			const end = text.indexOf("`", i + 1);
			if (end > i) {
				out += `${ansi.cyan}${text.slice(i + 1, end)}${ansi.reset}`;
				i = end + 1;
				continue;
			}
		}
		if (text.startsWith("**", i)) {
			const end = text.indexOf("**", i + 2);
			if (end > i) {
				out += `${ansi.bold}${renderInline(text.slice(i + 2, end), color)}${ansi.reset}`;
				i = end + 2;
				continue;
			}
		}
		if ((text[i] === "*" || text[i] === "_") && text[i + 1] !== " ") {
			const marker = text[i] as string;
			const end = text.indexOf(marker, i + 1);
			if (end > i + 1 && (end + 1 >= text.length || !/\w/.test(text[end + 1] as string))) {
				out += `${ansi.italic}${text.slice(i + 1, end)}${ansi.reset}`;
				i = end + 1;
				continue;
			}
		}
		out += text[i];
		i++;
	}
	return out;
}

export function renderMarkdown(source: string, options: { color: boolean; width: number }): string {
	const { color } = options;
	const lines = source.split("\n");
	const out: string[] = [];
	let inCode = false;
	for (const line of lines) {
		if (line.trimStart().startsWith("```")) {
			inCode = !inCode;
			out.push(color ? `${ansi.gray}${line}${ansi.reset}` : line);
			continue;
		}
		if (inCode) {
			out.push(color ? `${ansi.yellow}${line}${ansi.reset}` : line);
			continue;
		}
		const heading = /^(#{1,6})\s+(.*)$/.exec(line);
		if (heading) {
			const text = heading[2] ?? "";
			out.push(color ? `${ansi.bold}${ansi.underline}${text}${ansi.reset}` : text);
			continue;
		}
		const bullet = /^(\s*)[-*+]\s+(.*)$/.exec(line);
		if (bullet) {
			out.push(`${bullet[1] ?? ""}${color ? ansi.blue : ""}•${color ? ansi.reset : ""} ${renderInline(bullet[2] ?? "", color)}`);
			continue;
		}
		const numbered = /^(\s*)(\d+)\.\s+(.*)$/.exec(line);
		if (numbered) {
			out.push(`${numbered[1] ?? ""}${numbered[2]}. ${renderInline(numbered[3] ?? "", color)}`);
			continue;
		}
		if (line.startsWith("> ")) {
			out.push(color ? `${ansi.gray}│ ${renderInline(line.slice(2), color)}${ansi.reset}` : `│ ${line.slice(2)}`);
			continue;
		}
		out.push(renderInline(line, color));
	}
	return out.join("\n");
}
