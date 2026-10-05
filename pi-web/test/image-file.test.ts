// @vitest-environment happy-dom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { prepareImageFile } from "../src/image-file.ts";
import { MAX_BASE64_BYTES } from "../src/image-fit.ts";

/** What the stubbed browser decodes files to: size by URL; undefined fails to decode. */
let decodedSize: { width: number; height: number } | undefined;
/** Canvas calls, and the base64 size toDataURL produces per type. */
let canvasLog: string[];
let encodedBase64Bytes: (mimeType: string, width: number) => number;

class FakeImage {
	onload: (() => void) | null = null;
	onerror: (() => void) | null = null;
	naturalWidth = 0;
	naturalHeight = 0;
	set src(_url: string) {
		setTimeout(() => {
			if (!decodedSize) return this.onerror?.();
			this.naturalWidth = decodedSize.width;
			this.naturalHeight = decodedSize.height;
			this.onload?.();
		}, 0);
	}
}

beforeEach(() => {
	decodedSize = { width: 100, height: 50 };
	canvasLog = [];
	encodedBase64Bytes = () => 100;
	vi.stubGlobal("Image", FakeImage);
	vi.spyOn(HTMLCanvasElement.prototype, "getContext").mockImplementation(function (this: HTMLCanvasElement) {
		const canvas = this;
		return {
			set fillStyle(value: string) {
				canvasLog.push(`fill ${value}`);
			},
			fillRect: () => {},
			drawImage: (_img: unknown, x: number, y: number, w: number, h: number) =>
				canvasLog.push(`draw ${x},${y} ${w}x${h} on ${canvas.width}x${canvas.height}`),
		} as unknown as CanvasRenderingContext2D;
	} as never);
	vi.spyOn(HTMLCanvasElement.prototype, "toDataURL").mockImplementation(function (
		this: HTMLCanvasElement,
		type?: string,
		quality?: unknown,
	) {
		canvasLog.push(`toDataURL ${type} ${quality ?? "-"}`);
		return `data:${type};base64,${"C".repeat(encodedBase64Bytes(type ?? "", this.width))}`;
	});
});

afterEach(() => {
	vi.restoreAllMocks();
	vi.unstubAllGlobals();
});

const file = (bytes: number[], name: string, type: string) => new File([new Uint8Array(bytes)], name, { type });

describe("prepareImageFile", () => {
	it("passes a small supported image through byte for byte", async () => {
		const fitted = await prepareImageFile(file([137, 80, 78, 71, 1, 2, 3], "a.png", "image/png"));
		expect(fitted).toEqual({
			ok: true,
			image: { type: "image", mimeType: "image/png", data: btoa("\x89PNG\x01\x02\x03") },
		});
		expect(canvasLog).toEqual([]);
	});

	it("draws a large image smaller, on white for JPEG", async () => {
		decodedSize = { width: 4000, height: 3000 };
		encodedBase64Bytes = (mimeType, width) => (mimeType === "image/png" ? MAX_BASE64_BYTES + 1 : width * 100);
		const fitted = await prepareImageFile(file([1, 2, 3], "photo.webp", "image/webp"));
		expect(fitted.ok && `${fitted.image.mimeType} ${fitted.image.data.length}`).toBe("image/jpeg 200000");
		expect(canvasLog).toMatchInlineSnapshot(`
			[
			  "draw 0,0 2000x1500 on 2000x1500",
			  "toDataURL image/png -",
			  "fill #fff",
			  "draw 0,0 2000x1500 on 2000x1500",
			  "toDataURL image/jpeg 0.85",
			]
		`);
	});

	it("converts an unsupported type, and reports one the browser cannot read", async () => {
		const bmp = await prepareImageFile(file([66, 77], "a.bmp", "image/bmp"));
		expect(bmp.ok && bmp.image.mimeType).toBe("image/png");
		expect(canvasLog).toEqual(["draw 0,0 100x50 on 100x50", "toDataURL image/png -"]);

		decodedSize = undefined;
		expect(await prepareImageFile(file([73, 73], "scan.tiff", "image/tiff"))).toEqual({
			ok: false,
			error: "Cannot read scan.tiff (image/tiff): convert it to PNG or JPEG and attach it again",
		});
	});
});
