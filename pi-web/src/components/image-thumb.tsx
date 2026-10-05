import { useState } from "preact/hooks";
import type { ImageContent } from "../protocol.ts";

export function imageSrc(image: ImageContent): string {
	return `data:${image.mimeType};base64,${image.data}`;
}

/** A thumbnail in the chat; a click shows it full size (up to the chat's width) and back. */
export function ImageThumb({ image, alt }: { image: ImageContent; alt: string }) {
	const [full, setFull] = useState(false);
	return (
		<img
			class={full ? "msg-image full" : "msg-image"}
			src={imageSrc(image)}
			alt={alt}
			title={full ? "Click to shrink" : "Click for full size"}
			onClick={() => setFull(!full)}
		/>
	);
}
