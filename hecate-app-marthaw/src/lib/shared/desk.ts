/// Desk — mechanical frontend wrappers for screaming-desk-name API.
///
/// Backend desk = one module, one route, one handler.
/// Frontend desk = one typed function call with standardized error handling.
///
/// Three patterns cover every API call:
///   cmd()     — fire-and-forget POST (return ok/fail)
///   cmdThen() — POST then refetch aggregate state (return ok/fail)
///   qry()     — GET and return typed response

import type { Writable } from 'svelte/store';
import { getApi } from './api.js';

function errorMessage(e: unknown, desk: string): string {
	const msg = (e as { message?: string }).message;
	return msg || `${desk} failed`;
}

/** Fire-and-forget CMD desk. POST body, return ok/fail. */
export async function cmd(
	desk: string,
	body: unknown,
	error: Writable<string | null>
): Promise<boolean> {
	error.set(null);
	try {
		const api = getApi();
		await api.post(`/${desk}`, body);
		return true;
	} catch (e) {
		error.set(errorMessage(e, desk));
		return false;
	}
}

/** CMD desk that refetches aggregate state after success. */
export async function cmdThen(
	desk: string,
	body: unknown,
	error: Writable<string | null>,
	refetch: () => Promise<void>,
	loading?: Writable<boolean>
): Promise<boolean> {
	loading?.set(true);
	error.set(null);
	try {
		const api = getApi();
		await api.post(`/${desk}`, body);
		await refetch();
		return true;
	} catch (e) {
		error.set(errorMessage(e, desk));
		return false;
	} finally {
		loading?.set(false);
	}
}

/** CMD desk that returns the response data (for optimistic updates). */
export async function cmdGet<T>(
	desk: string,
	body: unknown,
	error: Writable<string | null>,
	loading?: Writable<boolean>
): Promise<T | null> {
	loading?.set(true);
	error.set(null);
	try {
		const api = getApi();
		return await api.post<T>(`/${desk}`, body);
	} catch (e) {
		error.set(errorMessage(e, desk));
		return null;
	} finally {
		loading?.set(false);
	}
}

/** QRY desk. GET and return typed response, or null on failure. */
export async function qry<T>(
	desk: string,
	error?: Writable<string | null>
): Promise<T | null> {
	try {
		const api = getApi();
		return await api.get<T>(`/${desk}`);
	} catch (e) {
		error?.set(errorMessage(e, desk));
		return null;
	}
}
