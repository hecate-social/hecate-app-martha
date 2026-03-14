<script lang="ts">
	import { getApi } from '$lib/martha/shared/api.js';
	import { activeVenture } from '$lib/martha/guide_venture/guide_venture.js';

	let narrative = $state('');
	let loading = $state(false);

	function renderMarkdown(md: string): string {
		return md
			.replace(
				/^### (.*$)/gm,
				'<h4 class="text-xs font-semibold text-surface-200 mt-3 mb-1">$1</h4>'
			)
			.replace(
				/^## (.*$)/gm,
				'<h3 class="text-sm font-semibold text-surface-100 mt-4 mb-2">$1</h3>'
			)
			.replace(
				/^# (.*$)/gm,
				'<h3 class="text-sm font-bold text-hecate-300 mt-4 mb-2">$1</h3>'
			)
			.replace(/\*\*(.*?)\*\*/g, '<strong class="text-surface-200">$1</strong>')
			.replace(
				/^- (.*$)/gm,
				'<div class="flex gap-1.5 ml-1"><span class="text-hecate-500">\u2022</span><span>$1</span></div>'
			)
			.replace(/^---$/gm, '<hr class="border-surface-700/50 my-3">')
			.replace(/\n\n/g, '<div class="h-2"></div>');
	}

	async function fetchNarrative() {
		if (!$activeVenture) return;
		loading = true;
		try {
			const api = getApi();
			const resp = await api.get<{ venture_id: string; narrative: string }>(
				`/knowledge-graph/${$activeVenture.venture_id}/narrative`
			);
			if (resp && resp.narrative) {
				narrative = resp.narrative;
			} else {
				narrative = '';
			}
		} catch {
			narrative = '';
		} finally {
			loading = false;
		}
	}

	$effect(() => {
		if ($activeVenture?.venture_id) {
			fetchNarrative();
		}
	});
</script>

<div class="bg-surface-800/30 border border-surface-700/50 rounded-lg">
	<div class="px-4 py-2.5 border-b border-surface-700/50 flex items-center justify-between">
		<span class="text-xs font-semibold text-surface-300 uppercase tracking-wider"
			>Venture Progress</span
		>
		<button
			onclick={fetchNarrative}
			disabled={loading}
			class="text-[10px] text-surface-500 hover:text-hecate-400 transition-colors disabled:opacity-50"
		>
			refresh
		</button>
	</div>

	<div class="px-4 py-3 text-xs text-surface-300 space-y-2 max-h-[500px] overflow-y-auto">
		{#if loading}
			<p class="animate-pulse text-surface-500 text-xs">Loading narrative...</p>
		{:else if narrative}
			{@html renderMarkdown(narrative)}
		{:else}
			<p class="text-center text-surface-500 py-8 text-xs">
				No narrative available yet. Run some agents to build knowledge.
			</p>
		{/if}
	</div>
</div>
