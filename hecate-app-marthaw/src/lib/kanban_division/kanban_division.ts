import { writable, derived } from 'svelte/store';
import { getApi } from '../shared/api.js';

// --- Types ---
export interface KanbanItem {
	item_id: string;
	division_id: string;
	title: string;
	description: string | null;
	item_type: 'cmd_desk' | 'prj_desk' | 'qry_desk';
	status_text: 'ready' | 'in_progress' | 'done';
	submitted_by: string | null;
	submitted_at: number | null;
	picked_by: string | null;
	picked_at: number | null;
	completed_at: number | null;
	return_reason: string | null;
}

export interface KanbanBoard {
	division_id: string;
	venture_id: string;
	context_name: string;
	status: number;
	status_label: string;
	initiated_at: number | null;
	initiated_by: string | null;
}

// --- State ---
export const kanbanBoard = writable<KanbanBoard | null>(null);
export const kanbanItems = writable<KanbanItem[]>([]);
export const kanbanError = writable<string | null>(null);
export const kanbanLoading = writable(false);

// --- Derived: items grouped by status ---
export const readyItems = derived(kanbanItems, ($items) =>
	$items.filter((i) => i.status_text === 'ready')
);
export const inProgressItems = derived(kanbanItems, ($items) =>
	$items.filter((i) => i.status_text === 'in_progress')
);
export const doneItems = derived(kanbanItems, ($items) =>
	$items.filter((i) => i.status_text === 'done')
);

export const itemCounts = derived(kanbanItems, ($items) => ({
	ready: $items.filter((i) => i.status_text === 'ready').length,
	in_progress: $items.filter((i) => i.status_text === 'in_progress').length,
	done: $items.filter((i) => i.status_text === 'done').length,
	total: $items.length
}));

// --- Actions ---
export async function fetchKanban(divisionId: string): Promise<void> {
	try {
		kanbanLoading.set(true);
		kanbanError.set(null);
		const api = getApi();
		const resp = await api.get<{ board: KanbanBoard; items: KanbanItem[] }>(
			`/kanbans/${divisionId}`
		);
		kanbanBoard.set(resp.board);
		kanbanItems.set(resp.items ?? []);
	} catch (e: unknown) {
		const err = e as { message?: string };
		kanbanError.set(err.message || 'Failed to fetch kanban board');
		kanbanBoard.set(null);
		kanbanItems.set([]);
	} finally {
		kanbanLoading.set(false);
	}
}

export async function submitItem(
	divisionId: string,
	data: { title: string; description?: string; item_type: string; submitted_by?: string }
): Promise<boolean> {
	try {
		kanbanError.set(null);
		const api = getApi();
		await api.post(`/kanbans/${divisionId}/items`, data);
		await fetchKanban(divisionId);
		return true;
	} catch (e: unknown) {
		const err = e as { message?: string };
		kanbanError.set(err.message || 'Failed to submit item');
		return false;
	}
}

export async function pickItem(
	divisionId: string,
	itemId: string,
	pickedBy: string = 'hecate-web'
): Promise<boolean> {
	try {
		kanbanError.set(null);
		const api = getApi();
		await api.post(`/kanbans/${divisionId}/items/${itemId}/pick`, {
			picked_by: pickedBy
		});
		await fetchKanban(divisionId);
		return true;
	} catch (e: unknown) {
		const err = e as { message?: string };
		kanbanError.set(err.message || 'Failed to pick item');
		return false;
	}
}

export async function completeItem(
	divisionId: string,
	itemId: string
): Promise<boolean> {
	try {
		kanbanError.set(null);
		const api = getApi();
		await api.post(`/kanbans/${divisionId}/items/${itemId}/complete`, {});
		await fetchKanban(divisionId);
		return true;
	} catch (e: unknown) {
		const err = e as { message?: string };
		kanbanError.set(err.message || 'Failed to complete item');
		return false;
	}
}

export async function returnItem(
	divisionId: string,
	itemId: string,
	reason: string
): Promise<boolean> {
	try {
		kanbanError.set(null);
		const api = getApi();
		await api.post(`/kanbans/${divisionId}/items/${itemId}/return`, {
			reason
		});
		await fetchKanban(divisionId);
		return true;
	} catch (e: unknown) {
		const err = e as { message?: string };
		kanbanError.set(err.message || 'Failed to return item');
		return false;
	}
}
