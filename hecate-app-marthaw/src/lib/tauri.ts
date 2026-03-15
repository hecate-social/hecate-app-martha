// Tauri detection utility.
// When running inside Tauri (hecate-web), native capabilities are available.
// When running in a plain browser (daemon-served UI), we gracefully degrade.

declare global {
	interface Window {
		__TAURI_INTERNALS__?: unknown;
	}
}

export function isTauri(): boolean {
	return typeof window !== 'undefined' && '__TAURI_INTERNALS__' in window;
}
