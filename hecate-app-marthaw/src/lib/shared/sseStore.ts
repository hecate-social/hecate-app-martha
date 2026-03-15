import { writable, get } from 'svelte/store';
export interface SSEEvent {
	type: string;
	data: Record<string, unknown>;
	receivedAt: number;
}

export type SSEStatus = 'disconnected' | 'connecting' | 'connected';

export const sseStatus = writable<SSEStatus>('disconnected');
export const lastSSEEvent = writable<SSEEvent | null>(null);

// Use relative URL — works in both Tauri (hecate://localhost) and dev (Vite proxy).
// The browser resolves relative paths against the current page origin.
const SSE_PATH = '/plugin/hecate-app-martha/api/events/stream';
const RECONNECT_DELAY_MS = 3000;

let eventSource: EventSource | null = null;
let reconnectTimer: ReturnType<typeof setTimeout> | null = null;
let listeners: Array<(event: SSEEvent) => void> = [];

export function onSSEEvent(handler: (event: SSEEvent) => void): () => void {
	listeners.push(handler);
	return () => {
		listeners = listeners.filter((h) => h !== handler);
	};
}

function notifyListeners(event: SSEEvent) {
	lastSSEEvent.set(event);
	for (const handler of listeners) {
		handler(event);
	}
}

export function connectSSE(): void {
	if (eventSource) return;

	sseStatus.set('connecting');

	try {
		eventSource = new EventSource(SSE_PATH);

		eventSource.onopen = () => {
			sseStatus.set('connected');
			if (reconnectTimer) {
				clearTimeout(reconnectTimer);
				reconnectTimer = null;
			}
		};

		eventSource.onerror = () => {
			sseStatus.set('disconnected');
			cleanupEventSource();
			scheduleReconnect();
		};

		// Default message handler (unnamed events)
		eventSource.onmessage = (e) => {
			handleRawEvent('message', e.data);
		};
	} catch {
		// EventSource might not work with hecate:// protocol.
		// Fall back to fetch-based SSE parsing.
		sseStatus.set('disconnected');
		connectSSEViaFetch();
	}
}

async function connectSSEViaFetch(): Promise<void> {
	sseStatus.set('connecting');
	try {
		const resp = await fetch(SSE_PATH);
		if (!resp.ok || !resp.body) {
			sseStatus.set('disconnected');
			scheduleReconnect();
			return;
		}

		sseStatus.set('connected');
		const reader = resp.body.getReader();
		const decoder = new TextDecoder();
		let buffer = '';

		while (true) {
			const { done, value } = await reader.read();
			if (done) break;

			buffer += decoder.decode(value, { stream: true });
			const parts = buffer.split('\n\n');
			buffer = parts.pop() ?? '';

			for (const part of parts) {
				if (!part.trim() || part.startsWith(':')) continue;
				parseSSEBlock(part);
			}
		}
	} catch {
		// Connection lost
	}

	sseStatus.set('disconnected');
	scheduleReconnect();
}

function parseSSEBlock(block: string): void {
	let eventType = 'message';
	let data = '';

	for (const line of block.split('\n')) {
		if (line.startsWith('event: ')) {
			eventType = line.slice(7);
		} else if (line.startsWith('data: ')) {
			data = line.slice(6);
		}
	}

	if (data) {
		handleRawEvent(eventType, data);
	}
}

function handleRawEvent(type: string, rawData: string): void {
	try {
		const data = JSON.parse(rawData) as Record<string, unknown>;
		notifyListeners({ type, data, receivedAt: Date.now() });
	} catch {
		// Non-JSON data, ignore
	}
}

function scheduleReconnect(): void {
	if (reconnectTimer) return;
	reconnectTimer = setTimeout(() => {
		reconnectTimer = null;
		connectSSE();
	}, RECONNECT_DELAY_MS);
}

function cleanupEventSource(): void {
	if (eventSource) {
		eventSource.close();
		eventSource = null;
	}
}

export function disconnectSSE(): void {
	if (reconnectTimer) {
		clearTimeout(reconnectTimer);
		reconnectTimer = null;
	}
	cleanupEventSource();
	sseStatus.set('disconnected');
}

/** Register a named event type listener on the EventSource (for typed SSE events). */
export function addEventTypeListener(eventType: string): void {
	if (!eventSource) return;
	eventSource.addEventListener(eventType, (e: MessageEvent) => {
		handleRawEvent(eventType, e.data);
	});
}
