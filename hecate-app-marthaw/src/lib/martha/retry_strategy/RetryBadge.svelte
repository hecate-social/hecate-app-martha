<script lang="ts">
	import { getApi } from '$lib/martha/shared/api.js';

	let { ventureId, sessionId }: { ventureId: string; sessionId: string } = $props();

	interface RetryInfo {
		status: string;
		attempt_count: number;
		max_attempts: number;
	}

	let retry: RetryInfo | null = $state(null);

	$effect(() => {
		if (ventureId && sessionId) fetchRetry();
	});

	async function fetchRetry() {
		try {
			const api = getApi();
			const resp = await api.get<{ ok: boolean; retry: RetryInfo }>(
				`/retry-strategy/${ventureId}/${sessionId}`
			);
			if (resp?.ok && resp.retry) {
				retry = resp.retry;
			}
		} catch {
			retry = null;
		}
	}

	const statusColor = $derived.by(() => {
		if (!retry) return '';
		switch (retry.status) {
			case 'retrying':
				return 'text-amber-400';
			case 'exhausted':
				return 'text-health-err';
			case 'succeeded':
				return 'text-health-ok';
			default:
				return '';
		}
	});

	const dotClass = $derived.by(() => {
		if (!retry) return '';
		const base = 'w-1.5 h-1.5 rounded-full';
		switch (retry.status) {
			case 'retrying':
				return `${base} bg-amber-400 animate-pulse`;
			case 'exhausted':
				return `${base} bg-health-err`;
			case 'succeeded':
				return `${base} bg-health-ok`;
			default:
				return base;
		}
	});
</script>

{#if retry}
	<span class="inline-flex items-center gap-1 text-[9px] font-medium {statusColor}">
		<span class={dotClass}></span>
		{#if retry.status === 'retrying'}
			Retry {retry.attempt_count}/{retry.max_attempts}
		{:else if retry.status === 'exhausted'}
			Retry {retry.attempt_count}/{retry.max_attempts}
		{:else if retry.status === 'succeeded'}
			Retry OK
		{/if}
	</span>
{/if}
