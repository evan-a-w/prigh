// Decodes raw terminal input bytes into key events.

export type Key =
	| { kind: "char"; char: string }
	| { kind: "enter" }
	| { kind: "newline" } // alt+enter / ctrl+j
	| { kind: "backspace" }
	| { kind: "delete" }
	| { kind: "tab" }
	| { kind: "escape" }
	| { kind: "up" }
	| { kind: "down" }
	| { kind: "left" }
	| { kind: "right" }
	| { kind: "home" }
	| { kind: "end" }
	| { kind: "ctrl"; letter: string }
	| { kind: "paste"; text: string }
	| { kind: "unknown"; raw: string };

const CSI: Record<string, Key> = {
	A: { kind: "up" },
	B: { kind: "down" },
	C: { kind: "right" },
	D: { kind: "left" },
	H: { kind: "home" },
	F: { kind: "end" },
	"1~": { kind: "home" },
	"4~": { kind: "end" },
	"3~": { kind: "delete" },
	"7~": { kind: "home" },
	"8~": { kind: "end" },
};

/** Parses one input chunk; multiple keys may arrive together. */
export function parseKeys(input: string): Key[] {
	const keys: Key[] = [];
	let i = 0;
	while (i < input.length) {
		const c = input[i] as string;
		if (c === "\x1b") {
			// Bracketed paste.
			if (input.startsWith("\x1b[200~", i)) {
				const end = input.indexOf("\x1b[201~", i);
				const text = end === -1 ? input.slice(i + 6) : input.slice(i + 6, end);
				keys.push({ kind: "paste", text });
				i = end === -1 ? input.length : end + 6;
				continue;
			}
			if (input[i + 1] === "[" || input[i + 1] === "O") {
				let j = i + 2;
				while (j < input.length && !/[A-Za-z~]/.test(input[j] as string)) j++;
				const seq = input.slice(i + 2, j + 1);
				const key = CSI[seq];
				keys.push(key ?? { kind: "unknown", raw: input.slice(i, j + 1) });
				i = j + 1;
				continue;
			}
			if (input[i + 1] === "\r") {
				keys.push({ kind: "newline" });
				i += 2;
				continue;
			}
			keys.push({ kind: "escape" });
			i++;
			continue;
		}
		if (c === "\r") keys.push({ kind: "enter" });
		else if (c === "\n") keys.push({ kind: "newline" });
		else if (c === "\x7f" || c === "\b") keys.push({ kind: "backspace" });
		else if (c === "\t") keys.push({ kind: "tab" });
		else if (c < " ") keys.push({ kind: "ctrl", letter: String.fromCharCode(c.charCodeAt(0) + 96) });
		else {
			// Group consecutive printable characters (e.g. from a fast paste without bracketing).
			let j = i;
			while (j < input.length && (input[j] as string) >= " " && input[j] !== "\x7f" && input[j] !== "\x1b") j++;
			const text = input.slice(i, j);
			if (text.length > 1) keys.push({ kind: "paste", text });
			else keys.push({ kind: "char", char: text });
			i = j;
			continue;
		}
		i++;
	}
	return keys;
}
