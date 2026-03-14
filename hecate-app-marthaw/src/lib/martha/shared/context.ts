import type { ChatMessage, StreamChunk, LLMModel } from '../types.js';

/** Base URL for the hecate-daemon custom protocol (LLM lives on daemon, not plugin) */
const DAEMON_BASE = 'hecate://localhost';

export interface ChatStream {
	onChunk: (handler: (chunk: StreamChunk) => void) => ChatStream;
	onDone: (handler: (chunk: StreamChunk) => void) => ChatStream;
	onError: (handler: (error: string) => void) => ChatStream;
	start: () => Promise<void>;
	cancel: () => void;
}

export interface StudioContext {
	stream: {
		chat: (model: string, messages: ChatMessage[]) => ChatStream;
	};
	transcribeAudio: (audioBlob: Blob, opts?: { provider?: string; language?: string }) => Promise<string>;
	speakText: (text: string, opts?: { voice?: string; provider?: string }) => Promise<void>;
}

/** Fetch available LLM models with full metadata from hecate-daemon. */
export async function fetchModels(): Promise<LLMModel[]> {
	try {
		const resp = await fetch(`${DAEMON_BASE}/api/llm/models`);
		if (!resp.ok) return [];
		const data = await resp.json();
		if (data.ok && Array.isArray(data.models)) {
			return data.models.map((m: Record<string, unknown>) => ({
				name: String(m.name ?? ''),
				context_length: Number(m.context_length ?? 0),
				family: String(m.family ?? ''),
				parameter_size: String(m.parameter_size ?? ''),
				format: String(m.format ?? 'api'),
				provider: String(m.provider ?? ''),
				quantization: m.quantization ? String(m.quantization) : undefined,
				size_bytes: m.size_bytes ? Number(m.size_bytes) : undefined
			}));
		}
		return [];
	} catch {
		return [];
	}
}

function createChatStream(model: string, messages: ChatMessage[]): ChatStream {
	let chunkHandler: ((chunk: StreamChunk) => void) | null = null;
	let doneHandler: ((chunk: StreamChunk) => void) | null = null;
	let errorHandler: ((error: string) => void) | null = null;
	let cancelled = false;

	const stream: ChatStream = {
		onChunk(handler) {
			chunkHandler = handler;
			return stream;
		},
		onDone(handler) {
			doneHandler = handler;
			return stream;
		},
		onError(handler) {
			errorHandler = handler;
			return stream;
		},
		async start() {
			if (cancelled) return;
			try {
				const resp = await fetch(`${DAEMON_BASE}/api/llm/chat`, {
					method: 'POST',
					headers: { 'Content-Type': 'application/json' },
					body: JSON.stringify({ model, messages })
				});
				if (cancelled) return;
				if (!resp.ok) {
					const text = await resp.text().catch(() => resp.statusText);
					if (errorHandler) errorHandler(text || 'LLM request failed');
					return;
				}
				const data = await resp.json();
				const response = data.response ?? data;
				if (chunkHandler) chunkHandler({ content: response.content ?? '' });
				if (doneHandler) doneHandler({ content: '', done: true });
			} catch (e: unknown) {
				if (cancelled) return;
				const err = e as { message?: string };
				if (errorHandler) errorHandler(err.message || 'LLM request failed');
			}
		},
		cancel() {
			cancelled = true;
		}
	};

	return stream;
}

async function transcribeAudio(
	audioBlob: Blob,
	opts?: { provider?: string; language?: string }
): Promise<string> {
	const form = new FormData();
	form.append('file', audioBlob, 'audio.webm');
	if (opts?.provider) form.append('provider', opts.provider);
	if (opts?.language) form.append('language', opts.language);
	const resp = await fetch(`${DAEMON_BASE}/api/llm/audio/transcribe`, {
		method: 'POST',
		body: form
	});
	if (!resp.ok) return '';
	const data = await resp.json();
	return data.text ?? '';
}

async function speakText(
	text: string,
	opts?: { voice?: string; provider?: string }
): Promise<void> {
	const resp = await fetch(`${DAEMON_BASE}/api/llm/audio/speak`, {
		method: 'POST',
		headers: { 'Content-Type': 'application/json' },
		body: JSON.stringify({
			text,
			voice: opts?.voice ?? 'tara',
			provider: opts?.provider
		})
	});
	if (!resp.ok) return;
	const audioBlob = await resp.blob();
	const url = URL.createObjectURL(audioBlob);
	const audio = new Audio(url);
	audio.onended = () => URL.revokeObjectURL(url);
	await audio.play();
}

export function createStudioContext(): StudioContext {
	return {
		stream: {
			chat: createChatStream
		},
		transcribeAudio,
		speakText
	};
}
