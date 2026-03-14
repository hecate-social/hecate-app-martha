import { writable, derived } from 'svelte/store';
import type { SSEEvent } from './sseStore.js';
import { onSSEEvent } from './sseStore.js';

export interface ActivityItem {
	id: string;
	type: string;
	summary: string;
	detail?: string;
	severity: 'info' | 'success' | 'warning' | 'error';
	timestamp: number;
}

const MAX_ITEMS = 50;
let nextId = 0;

export const activityItems = writable<ActivityItem[]>([]);
export const unreadCount = writable(0);

export const recentActivity = derived(activityItems, ($items) =>
	$items.slice(0, 10)
);

function eventToActivity(event: SSEEvent): ActivityItem | null {
	const { type, data, receivedAt } = event;

	// Venture events
	if (type === 'venture_initiated_v1') {
		return {
			id: `act-${nextId++}`,
			type,
			summary: `Venture "${data.name ?? 'unknown'}" initiated`,
			severity: 'success',
			timestamp: receivedAt
		};
	}
	if (type === 'venture_archived_v1') {
		return {
			id: `act-${nextId++}`,
			type,
			summary: `Venture archived`,
			severity: 'info',
			timestamp: receivedAt
		};
	}

	// Division events
	if (type === 'division_initiated_v1') {
		return {
			id: `act-${nextId++}`,
			type,
			summary: `Division "${data.context_name ?? 'unknown'}" initiated`,
			severity: 'success',
			timestamp: receivedAt
		};
	}

	// Phase transitions
	if (type.includes('_opened_v')) {
		const phase = extractPhase(type);
		return {
			id: `act-${nextId++}`,
			type,
			summary: `${phase} phase opened`,
			severity: 'info',
			timestamp: receivedAt
		};
	}
	if (type.includes('_shelved_v')) {
		const phase = extractPhase(type);
		return {
			id: `act-${nextId++}`,
			type,
			summary: `${phase} phase shelved`,
			detail: (data.reason as string) ?? undefined,
			severity: 'warning',
			timestamp: receivedAt
		};
	}
	if (type.includes('_resumed_v')) {
		const phase = extractPhase(type);
		return {
			id: `act-${nextId++}`,
			type,
			summary: `${phase} phase resumed`,
			severity: 'info',
			timestamp: receivedAt
		};
	}

	// Agent events
	if (type.includes('agent_') || type.includes('mentor_') || type.includes('coordinator_')) {
		return {
			id: `act-${nextId++}`,
			type,
			summary: humanizeEventType(type),
			severity: 'info',
			timestamp: receivedAt
		};
	}

	// Storming events
	if (type.startsWith('aggregate_designed_v') || type.startsWith('event_designed_v')) {
		return {
			id: `act-${nextId++}`,
			type,
			summary: humanizeEventType(type),
			detail: (data.name as string) ?? undefined,
			severity: 'info',
			timestamp: receivedAt
		};
	}

	// Kanban events
	if (type.startsWith('kanban_card_')) {
		return {
			id: `act-${nextId++}`,
			type,
			summary: humanizeEventType(type),
			severity: 'info',
			timestamp: receivedAt
		};
	}

	// Gate events
	if (type.includes('_gate_')) {
		const isPass = type.includes('passed');
		const isReject = type.includes('rejected');
		return {
			id: `act-${nextId++}`,
			type,
			summary: humanizeEventType(type),
			severity: isReject ? 'error' : isPass ? 'success' : 'warning',
			timestamp: receivedAt
		};
	}

	// Retry events
	if (type === 'retry_initiated_v1') {
		return {
			id: `act-${nextId++}`,
			type,
			summary: `Retry initiated for ${(data.agent_role as string) ?? 'agent'}`,
			severity: 'warning',
			timestamp: receivedAt
		};
	}
	if (type === 'retry_attempted_v1') {
		return {
			id: `act-${nextId++}`,
			type,
			summary: `Retry attempt #${data.attempt_number ?? '?'}`,
			severity: 'info',
			timestamp: receivedAt
		};
	}
	if (type === 'retry_exhausted_v1') {
		return {
			id: `act-${nextId++}`,
			type,
			summary: 'Retry attempts exhausted',
			severity: 'error',
			timestamp: receivedAt
		};
	}
	if (type === 'retry_succeeded_v1') {
		return {
			id: `act-${nextId++}`,
			type,
			summary: 'Retry succeeded',
			severity: 'success',
			timestamp: receivedAt
		};
	}

	// Cost events
	if (type === 'cost_budget_set_v1') {
		return {
			id: `act-${nextId++}`,
			type,
			summary: `Budget set: $${data.budget_usd ?? '?'}`,
			severity: 'info',
			timestamp: receivedAt
		};
	}
	if (type === 'spending_recorded_v1') {
		return {
			id: `act-${nextId++}`,
			type,
			summary: `Spent $${data.amount_usd ?? '?'} (${(data.model as string) ?? 'unknown'})`,
			severity: 'info',
			timestamp: receivedAt
		};
	}
	if (type === 'cost_budget_warning_v1') {
		return {
			id: `act-${nextId++}`,
			type,
			summary: `Budget warning: ${data.pct ?? '?'}% used`,
			severity: 'warning',
			timestamp: receivedAt
		};
	}
	if (type === 'cost_budget_breached_v1') {
		return {
			id: `act-${nextId++}`,
			type,
			summary: 'Budget breached!',
			severity: 'error',
			timestamp: receivedAt
		};
	}

	// Quality events
	if (type === 'insight_captured_v1' && data.insight_type === 'quality_warning') {
		return {
			id: `act-${nextId++}`,
			type,
			summary: `Quality warning from ${(data.source_agent as string) ?? 'agent'}`,
			severity: 'warning',
			timestamp: receivedAt
		};
	}

	// Catch-all for anything else
	return {
		id: `act-${nextId++}`,
		type,
		summary: humanizeEventType(type),
		severity: 'info',
		timestamp: receivedAt
	};
}

function extractPhase(eventType: string): string {
	if (eventType.includes('planning')) return 'Planning';
	if (eventType.includes('crafting')) return 'Crafting';
	if (eventType.includes('storming')) return 'Storming';
	if (eventType.includes('kanban')) return 'Kanban';
	return 'Phase';
}

function humanizeEventType(type: string): string {
	return type
		.replace(/_v\d+$/, '')
		.replace(/_/g, ' ')
		.replace(/\b\w/g, (c) => c.toUpperCase());
}

export function initActivityListener(): () => void {
	return onSSEEvent((event: SSEEvent) => {
		const item = eventToActivity(event);
		if (item) {
			activityItems.update((items) => {
				const next = [item, ...items];
				if (next.length > MAX_ITEMS) next.length = MAX_ITEMS;
				return next;
			});
			unreadCount.update((n) => n + 1);
		}
	});
}

export function markAllRead(): void {
	unreadCount.set(0);
}
