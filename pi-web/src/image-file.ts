/** A pasted or dropped image file, made acceptable to the backend (image-fit.ts) with a canvas. */
import { type Encoding, type Fitted, fitImage } from "./image-fit.ts";
import type { ImageContent } from "./protocol.ts";

/** Images without an intrinsic size (some SVGs) are drawn at this size. */
const FALLBACK_SIDE = 1024;

function readBase64(file: File): Promise<string> {
	return new Promise((resolve, reject) => {
		const reader = new FileReader();
		reader.onload = () => {
			const dataUrl = String(reader.result);
			resolve(dataUrl.slice(dataUrl.indexOf(",") + 1));
		};
		reader.onerror = () => reject(reader.error);
		reader.readAsDataURL(file);
	});
}

function decode(url: string): Promise<HTMLImageElement | undefined> {
	return new Promise((resolve) => {
		const img = new Image();
		img.onload = () => resolve(img);
		img.onerror = () => resolve(undefined);
		img.src = url;
	});
}

function encodeWithCanvas(img: HTMLImageElement, encoding: Encoding): ImageContent | undefined {
	const canvas = document.createElement("canvas");
	canvas.width = encoding.width;
	canvas.height = encoding.height;
	const context = canvas.getContext("2d");
	if (!context) return undefined;
	if (encoding.mimeType === "image/jpeg") {
		context.fillStyle = "#fff";
		context.fillRect(0, 0, encoding.width, encoding.height);
	}
	context.imageSmoothingQuality = "high";
	context.drawImage(img, 0, 0, encoding.width, encoding.height);
	const match = /^data:([^;,]+);base64,(.*)$/.exec(canvas.toDataURL(encoding.mimeType, encoding.quality));
	return match ? { type: "image", mimeType: match[1], data: match[2] } : undefined;
}

export async function prepareImageFile(file: File): Promise<Fitted> {
	const data = await readBase64(file);
	const url = URL.createObjectURL(file);
	try {
		const img = await decode(url);
		const size = img && {
			width: img.naturalWidth || FALLBACK_SIDE,
			height: img.naturalHeight || FALLBACK_SIDE,
		};
		return await fitImage(
			{ name: file.name || "the image", image: { type: "image", data, mimeType: file.type }, size },
			async (encoding) => (img ? encodeWithCanvas(img, encoding) : undefined),
		);
	} finally {
		URL.revokeObjectURL(url);
	}
}
