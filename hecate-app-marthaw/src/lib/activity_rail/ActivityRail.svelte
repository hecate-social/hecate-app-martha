<script lang="ts">
	import {
		recentActivity,
		unreadCount,
		markAllRead,
		type ActivityItem
	} from '$lib/shared/activityStore.js';
	import { sseStatus } from '$lib/shared/sseStore.js';

	let expanded = $state(false);

	function severityColor(severity: ActivityItem['severity']): string {
		switch (severity) {
			case 'success': return 'text-health-ok';
			case 'warning': return 'text-amber-400';
			case 'error': return 'text-health-err';
			default: return 'text-surface-400';
		}
	}

	function severityDot(severity: ActivityItem['severity']): string {
		switch (severity) {
			case 'success': return 'bg-health-ok';
			case 'warning': return 'bg-amber-400';
			case 'error': return 'bg-health-err';
			default: return 'bg-surface-500';
		}
	}

	function timeAgo(ts: number): string {
		const diffSec = Math.floor((Date.now() - ts) / 1000);
		if (diffSec < 5) return 'now';
		if (diffSec < 60) return `${diffSec}s ago`;
		const diffMin = Math.floor(diffSec / 60);
		if (diffMin < 60) return `${diffMin}m ago`;
		const diffHr = Math.floor(diffMin / 60);
		return `${diffHr}h ago`;
	}

	function handleToggle() {
		expanded = !expanded;
		if (expanded) markAllRead();
	}
</script>

<div class="border-t border-surface-600 bg-surface-800/50 shrink-0">
	<!-- Collapsed ticker bar -->
	<button
		onclick={handleToggle}
		class="w-full flex items-center gap-2 px-4 py-1.5 text-[10px]
			hover:bg-surface-700/30 transition-colors"
	>
		<!-- SSE status dot -->
		<span
			class="inline-block w-1.5 h-1.5 rounded-full shrink-0
				{$sseStatus === 'connected'
				? 'bg-health-ok'
				: $sseStatus === 'connecting'
					? 'bg-amber-400 animate-pulse'
					: 'bg-surface-500'}"
			title="SSE: {$sseStatus}"
		></span>

		<!-- Latest event summary -->
		{#if $recentActivity.length > 0}
			{@const latest = $recentActivity[0]}
			<span class="{severityColor(latest.severity)} truncate">
				{latest.summary}
			</span>
			{#if latest.detail}
				<span class="text-surface-500 truncate">{latest.detail}</span>
			{/if}
			<span class="text-surface-500 shrink-0">{timeAgo(latest.timestamp)}</span>
		{:else}
			<span class="text-surface-500">No recent activity</span>
		{/if}

		<span class="flex-1"></span>

		<!-- Unread badge -->
		{#if $unreadCount > 0}
			<span class="px-1.5 py-0.5 rounded-full bg-hecate-600/30 text-hecate-300 text-[9px]">
				{$unreadCount}
			</span>
		{/if}

		<span class="text-surface-500 text-[8px]">{expanded ? '\u{25BC}' : '\u{25B2}'}</span>
	</button>

	<!-- Expanded event list -->
	{#if expanded}
		<div class="max-h-48 overflow-y-auto border-t border-surface-700/50">
			{#each $recentActivity as item (item.id)}
				<div class="flex items-start gap-2 px-4 py-1.5 hover:bg-surface-700/20
					transition-colors border-b border-surface-700/30 last:border-b-0">
					<span class="inline-block w-1.5 h-1.5 rounded-full mt-1 shrink-0
						{severityDot(item.severity)}"></span>
					<div class="min-w-0 flex-1">
						<span class="text-[10px] {severityColor(item.severity)}">{item.summary}</span>
						{#if item.detail}
							<span class="text-[9px] text-surface-500 ml-1">{item.detail}</span>
						{/if}
					</div>
					<span class="text-[9px] text-surface-500 shrink-0 tabular-nums">
						{timeAgo(item.timestamp)}
					</span>
				</div>
			{/each}

			{#if $recentActivity.length === 0}
				<div class="text-center py-4 text-[10px] text-surface-500">
					Activity will appear here as events stream in
				</div>
			{/if}
		</div>
	{/if}
</div>
