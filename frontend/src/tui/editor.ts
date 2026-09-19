// Multi-line text editor state with cursor movement; pure so it can be tested.

export interface Position {
	line: number;
	col: number;
}

export class Editor {
	private lines: string[] = [""];
	private cursor: Position = { line: 0, col: 0 };
	private history: string[] = [];
	private historyIndex = -1;
	private draft = "";

	get text(): string {
		return this.lines.join("\n");
	}

	get position(): Position {
		return { ...this.cursor };
	}

	get lineCount(): number {
		return this.lines.length;
	}

	getLines(): readonly string[] {
		return this.lines;
	}

	setText(text: string): void {
		this.lines = text.split("\n");
		const last = this.lines.length - 1;
		this.cursor = { line: last, col: (this.lines[last] ?? "").length };
	}

	clear(): void {
		this.setText("");
		this.historyIndex = -1;
	}

	private currentLine(): string {
		return this.lines[this.cursor.line] ?? "";
	}

	insert(text: string): void {
		const parts = text.replace(/\r\n?/g, "\n").split("\n");
		const line = this.currentLine();
		const before = line.slice(0, this.cursor.col);
		const after = line.slice(this.cursor.col);
		if (parts.length === 1) {
			this.lines[this.cursor.line] = before + parts[0] + after;
			this.cursor.col += (parts[0] ?? "").length;
			return;
		}
		const first = before + (parts[0] ?? "");
		const middle = parts.slice(1, -1);
		const lastPart = parts[parts.length - 1] ?? "";
		this.lines.splice(this.cursor.line, 1, first, ...middle, lastPart + after);
		this.cursor = { line: this.cursor.line + parts.length - 1, col: lastPart.length };
	}

	newline(): void {
		this.insert("\n");
	}

	backspace(): void {
		if (this.cursor.col > 0) {
			const line = this.currentLine();
			this.lines[this.cursor.line] = line.slice(0, this.cursor.col - 1) + line.slice(this.cursor.col);
			this.cursor.col--;
		} else if (this.cursor.line > 0) {
			const prev = this.lines[this.cursor.line - 1] ?? "";
			this.lines.splice(this.cursor.line - 1, 2, prev + this.currentLine());
			this.cursor = { line: this.cursor.line - 1, col: prev.length };
		}
	}

	deleteForward(): void {
		const line = this.currentLine();
		if (this.cursor.col < line.length) {
			this.lines[this.cursor.line] = line.slice(0, this.cursor.col) + line.slice(this.cursor.col + 1);
		} else if (this.cursor.line < this.lines.length - 1) {
			const next = this.lines[this.cursor.line + 1] ?? "";
			this.lines.splice(this.cursor.line, 2, line + next);
		}
	}

	left(): void {
		if (this.cursor.col > 0) this.cursor.col--;
		else if (this.cursor.line > 0) {
			this.cursor.line--;
			this.cursor.col = this.currentLine().length;
		}
	}

	right(): void {
		if (this.cursor.col < this.currentLine().length) this.cursor.col++;
		else if (this.cursor.line < this.lines.length - 1) {
			this.cursor.line++;
			this.cursor.col = 0;
		}
	}

	/** Returns false when already on the first line (caller may use history). */
	up(): boolean {
		if (this.cursor.line === 0) return false;
		this.cursor.line--;
		this.cursor.col = Math.min(this.cursor.col, this.currentLine().length);
		return true;
	}

	down(): boolean {
		if (this.cursor.line >= this.lines.length - 1) return false;
		this.cursor.line++;
		this.cursor.col = Math.min(this.cursor.col, this.currentLine().length);
		return true;
	}

	home(): void {
		this.cursor.col = 0;
	}

	end(): void {
		this.cursor.col = this.currentLine().length;
	}

	killToEnd(): void {
		this.lines[this.cursor.line] = this.currentLine().slice(0, this.cursor.col);
	}

	killLine(): void {
		this.lines[this.cursor.line] = "";
		this.cursor.col = 0;
	}

	killWord(): void {
		const line = this.currentLine();
		let i = this.cursor.col;
		while (i > 0 && line[i - 1] === " ") i--;
		while (i > 0 && line[i - 1] !== " ") i--;
		this.lines[this.cursor.line] = line.slice(0, i) + line.slice(this.cursor.col);
		this.cursor.col = i;
	}

	/** Records submitted text and resets the editor. */
	submit(): string {
		const text = this.text;
		if (text.trim() !== "" && this.history[this.history.length - 1] !== text) this.history.push(text);
		this.clear();
		return text;
	}

	historyPrev(): boolean {
		if (this.history.length === 0) return false;
		if (this.historyIndex === -1) {
			this.draft = this.text;
			this.historyIndex = this.history.length;
		}
		if (this.historyIndex === 0) return false;
		this.historyIndex--;
		this.setText(this.history[this.historyIndex] ?? "");
		return true;
	}

	historyNext(): boolean {
		if (this.historyIndex === -1) return false;
		this.historyIndex++;
		if (this.historyIndex >= this.history.length) {
			this.historyIndex = -1;
			this.setText(this.draft);
		} else {
			this.setText(this.history[this.historyIndex] ?? "");
		}
		return true;
	}
}
