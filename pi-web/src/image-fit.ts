/**
 * Making a pasted image acceptable to the backend (backend/lib/image.ml):
 * PNG, JPEG, GIF or WebP, at most MAX_DIMENSION px each way and
 * MAX_BASE64_BYTES of base64. Pure: the encoder (a canvas in the browser,
 * image-file.ts) is passed in.
 */
import type { ImageContent } from "./protocol.ts";

export const MAX_DIMENSION = 2000;
/** The backend's Image.max_base64_bytes (Anthropic allows 5 MB per image). */
export const MAX_BASE64_BYTES = 4_718_592;
export const SUPPORTED_MIME_TYPES: readonly string[] = ["image/png", "image/jpeg", "image/gif", "image/webp"];

/** Longest sides to try, as the backend's own downscaling does. */
const SIDES = [MAX_DIMENSION, 1500, 1000, 768];
const JPEG_QUALITIES = [0.85, 0.6];

export interface Size {
	width: number;
	height: number;
}

export interface Encoding extends Size {
	mimeType: "image/png" | "image/jpeg";
	/** JPEG only. */
	quality?: number;
}

export function base64Length(bytes: number): number {
	return 4 * Math.ceil(bytes / 3);
}

export function isSupported(mimeType: string): boolean {
	return SUPPORTED_MIME_TYPES.includes(mimeType);
}

/** `size` scaled down (never up) to fit in `side` x `side`, keeping the aspect ratio. */
export function fitWithin(size: Size, side: number): Size {
	const scale = Math.min(1, side / size.width, side / size.height);
	return {
		width: Math.max(1, Math.round(size.width * scale)),
		height: Math.max(1, Math.round(size.height * scale)),
	};
}

/**
 * What to try, in order, until one fits: per size (largest first) PNG, which
 * keeps screenshots and diagrams sharp, then JPEG at decreasing quality.
 * JPEG sources skip PNG, which would only be larger.
 */
export function encodings(mimeType: string, size: Size): Encoding[] {
	const result: Encoding[] = [];
	let previous: Size | undefined;
	for (const side of SIDES) {
		const fitted = fitWithin(size, side);
		if (previous && previous.width === fitted.width && previous.height === fitted.height) continue;
		previous = fitted;
		if (mimeType !== "image/jpeg") result.push({ ...fitted, mimeType: "image/png" });
		for (const quality of JPEG_QUALITIES) result.push({ ...fitted, mimeType: "image/jpeg", quality });
	}
	return result;
}

export interface Source {
	/** For messages: the file name. */
	name: string;
	image: ImageContent;
	/** Undefined when the browser could not decode the image. */
	size: Size | undefined;
}

export type Fitted = { ok: true; image: ImageContent } | { ok: false; error: string };

function sendableAsIs({ image, size }: Source): boolean {
	return (
		isSupported(image.mimeType) &&
		image.data.length <= MAX_BASE64_BYTES &&
		(size === undefined || (size.width <= MAX_DIMENSION && size.height <= MAX_DIMENSION))
	);
}

/**
 * The source itself when the backend accepts it as it is (byte-identical),
 * otherwise the first of `encodings` whose base64 is small enough. An
 * undecodable image is sent as it is if it looks acceptable, for the backend
 * to judge.
 */
export async function fitImage(
	source: Source,
	encode: (encoding: Encoding) => Promise<ImageContent | undefined>,
): Promise<Fitted> {
	if (sendableAsIs(source)) return { ok: true, image: source.image };
	const { name, image, size } = source;
	if (!size) {
		return {
			ok: false,
			error: `Cannot read ${name} (${image.mimeType || "unknown type"}): convert it to PNG or JPEG and attach it again`,
		};
	}
	const attempts = encodings(image.mimeType, size);
	for (const encoding of attempts) {
		const encoded = await encode(encoding);
		if (encoded && isSupported(encoded.mimeType) && encoded.data.length <= MAX_BASE64_BYTES) {
			return { ok: true, image: encoded };
		}
	}
	const smallest = attempts[attempts.length - 1];
	return {
		ok: false,
		error:
			`${name} is too large to send even as a ${smallest.width}x${smallest.height} JPEG: ` +
			"crop it and attach it again",
	};
}
