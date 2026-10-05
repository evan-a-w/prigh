import { describe, expect, it } from "vitest";
import {
	base64Length,
	type Encoding,
	encodings,
	fitImage,
	fitWithin,
	MAX_BASE64_BYTES,
	type Source,
} from "../src/image-fit.ts";
import type { ImageContent } from "../src/protocol.ts";

const show = (e: Encoding) => `${e.width}x${e.height} ${e.mimeType}${e.quality === undefined ? "" : ` q${e.quality}`}`;

const source = (mimeType: string, size: Source["size"], base64Bytes = 1000): Source => ({
	name: "shot.png",
	image: { type: "image", mimeType, data: "A".repeat(base64Bytes) },
	size,
});

/**
 * A stand-in for the canvas: base64 of `bytesPerPixel(encoding)` per pixel,
 * recording what was tried.
 */
function fakeEncoder(bytesPerPixel: (encoding: Encoding) => number) {
	const tried: string[] = [];
	const encode = async (encoding: Encoding): Promise<ImageContent> => {
		tried.push(show(encoding));
		const bytes = Math.round(encoding.width * encoding.height * bytesPerPixel(encoding));
		return { type: "image", mimeType: encoding.mimeType, data: "B".repeat(base64Length(bytes)) };
	};
	return { tried, encode };
}

const result = (fitted: Awaited<ReturnType<typeof fitImage>>) =>
	fitted.ok ? `${fitted.image.mimeType}, ${fitted.image.data.length} bytes of base64` : `error: ${fitted.error}`;

describe("sizing", () => {
	it("scales down to fit, keeping the aspect ratio, never up", () => {
		expect(fitWithin({ width: 4000, height: 3000 }, 2000)).toEqual({ width: 2000, height: 1500 });
		expect(fitWithin({ width: 1000, height: 5000 }, 2000)).toEqual({ width: 400, height: 2000 });
		expect(fitWithin({ width: 800, height: 600 }, 2000)).toEqual({ width: 800, height: 600 });
		expect(fitWithin({ width: 10000, height: 1 }, 768)).toEqual({ width: 768, height: 1 });
	});

	it("base64 is 4 bytes per 3", () => {
		expect([0, 1, 3, 4, 3_538_944].map(base64Length)).toEqual([0, 4, 4, 8, MAX_BASE64_BYTES]);
	});

	it("tries PNG then JPEG at decreasing quality, per size, largest first", () => {
		expect(encodings("image/png", { width: 4000, height: 3000 }).map(show)).toMatchInlineSnapshot(`
			[
			  "2000x1500 image/png",
			  "2000x1500 image/jpeg q0.85",
			  "2000x1500 image/jpeg q0.6",
			  "1500x1125 image/png",
			  "1500x1125 image/jpeg q0.85",
			  "1500x1125 image/jpeg q0.6",
			  "1000x750 image/png",
			  "1000x750 image/jpeg q0.85",
			  "1000x750 image/jpeg q0.6",
			  "768x576 image/png",
			  "768x576 image/jpeg q0.85",
			  "768x576 image/jpeg q0.6",
			]
		`);
	});

	it("photos start at JPEG; sizes above the image's own are tried once, at its size", () => {
		expect(encodings("image/jpeg", { width: 1200, height: 900 }).map(show)).toMatchInlineSnapshot(`
			[
			  "1200x900 image/jpeg q0.85",
			  "1200x900 image/jpeg q0.6",
			  "1000x750 image/jpeg q0.85",
			  "1000x750 image/jpeg q0.6",
			  "768x576 image/jpeg q0.85",
			  "768x576 image/jpeg q0.6",
			]
		`);
		expect(encodings("image/bmp", { width: 300, height: 200 }).map(show)).toMatchInlineSnapshot(`
			[
			  "300x200 image/png",
			  "300x200 image/jpeg q0.85",
			  "300x200 image/jpeg q0.6",
			]
		`);
	});
});

describe("fitImage", () => {
	it("leaves small supported images untouched", async () => {
		for (const mimeType of ["image/png", "image/jpeg", "image/gif", "image/webp"]) {
			const { tried, encode } = fakeEncoder(() => 3);
			const original = source(mimeType, { width: 2000, height: 2000 }, MAX_BASE64_BYTES);
			const fitted = await fitImage(original, encode);
			expect(fitted.ok && fitted.image).toBe(original.image);
			expect(tried).toEqual([]);
		}
	});

	it("sends a supported image the browser cannot decode as it is, for the backend to judge", async () => {
		const { tried, encode } = fakeEncoder(() => 3);
		const original = source("image/webp", undefined);
		const fitted = await fitImage(original, encode);
		expect(fitted.ok && fitted.image).toBe(original.image);
		expect(tried).toEqual([]);
	});

	it("downscales an image larger than 2000 px, as PNG when that is small enough", async () => {
		const { tried, encode } = fakeEncoder((e) => (e.mimeType === "image/png" ? 0.5 : 0.2));
		expect(result(await fitImage(source("image/png", { width: 2560, height: 1440 }), encode))).toBe(
			"image/png, 1500000 bytes of base64",
		);
		expect(tried).toEqual(["2000x1125 image/png"]);
	});

	it("falls back to JPEG and smaller sizes when the base64 is too large", async () => {
		const { tried, encode } = fakeEncoder((e) => (e.mimeType === "image/png" ? 3 : e.quality === 0.85 ? 2 : 1.5));
		expect(result(await fitImage(source("image/png", { width: 1800, height: 1800 }, 6_000_000), encode))).toBe(
			"image/jpeg, 4500000 bytes of base64",
		);
		expect(tried).toMatchInlineSnapshot(`
			[
			  "1800x1800 image/png",
			  "1800x1800 image/jpeg q0.85",
			  "1800x1800 image/jpeg q0.6",
			  "1500x1500 image/png",
			  "1500x1500 image/jpeg q0.85",
			  "1500x1500 image/jpeg q0.6",
			]
		`);
	});

	it("converts unsupported types even when they are small", async () => {
		for (const mimeType of ["image/bmp", "image/svg+xml", "image/tiff", ""]) {
			const { tried, encode } = fakeEncoder(() => 1);
			expect(result(await fitImage(source(mimeType, { width: 640, height: 480 }), encode))).toBe(
				"image/png, 409600 bytes of base64",
			);
			expect(tried).toEqual(["640x480 image/png"]);
		}
	});

	it("skips an encoding the browser does not produce (it falls back to another type)", async () => {
		const tried: string[] = [];
		const fitted = await fitImage(source("image/bmp", { width: 10, height: 10 }), async (e) => {
			tried.push(show(e));
			return e.mimeType === "image/png" ? { type: "image", mimeType: "image/bmp", data: "x" } : undefined;
		});
		expect(result(fitted)).toMatchInlineSnapshot(
			`"error: shot.png is too large to send even as a 10x10 JPEG: crop it and attach it again"`,
		);
		expect(tried).toEqual(["10x10 image/png", "10x10 image/jpeg q0.85", "10x10 image/jpeg q0.6"]);
	});

	it("says what to do when nothing is small enough or the image cannot be read", async () => {
		const { tried, encode } = fakeEncoder(() => 100);
		expect(result(await fitImage(source("image/png", { width: 9000, height: 3000 }, 9_000_000), encode)))
			.toMatchInlineSnapshot(`"error: shot.png is too large to send even as a 768x256 JPEG: crop it and attach it again"`);
		expect(tried).toHaveLength(12);
		expect(result(await fitImage(source("image/tiff", undefined), encode))).toMatchInlineSnapshot(
			`"error: Cannot read shot.png (image/tiff): convert it to PNG or JPEG and attach it again"`,
		);
		expect(result(await fitImage(source("", undefined), encode))).toMatchInlineSnapshot(
			`"error: Cannot read shot.png (unknown type): convert it to PNG or JPEG and attach it again"`,
		);
	});
});
