<script lang="ts">
	import { getApi } from '$lib/martha/shared/api.js';
	import { activeVenture } from '$lib/martha/guide_venture/guide_venture.js';

	interface KGEntity {
		entity_id?: string;
		name: string;
		type: string;
		description?: string;
		source_agent?: string;
		captured_at?: number;
		division_id?: string;
	}

	interface KGInsight {
		insight_id?: string;
		content: string;
		insight_type: string;
		source_agent?: string;
		source_session?: string;
		captured_at?: number;
		superseded?: boolean;
	}

	interface KGRelationship {
		from_entity: string;
		to_entity: string;
		rel_type: string;
		strength?: number;
	}

	interface KnowledgeGraph {
		entities: Record<string, KGEntity>;
		insights: KGInsight[];
		relationships: Record<string, KGRelationship>;
	}

	type TabKey = 'entities' | 'insights' | 'warnings';

	let activeTab: TabKey = $state('entities');
	let searchQuery = $state('');
	let graph: KnowledgeGraph | null = $state(null);
	let loading = $state(false);
	let error = $state<string | null>(null);
	let collapsedGroups = $state<Record<string, boolean>>({});

	// --- Derived data ---

	let entities = $derived.by((): KGEntity[] => {
		const g = graph;
		return g ? Object.values(g.entities) : [];
	});
	let relationships = $derived.by((): KGRelationship[] => {
		const g = graph;
		return g ? Object.values(g.relationships) : [];
	});

	let filteredEntities = $derived.by((): KGEntity[] => {
		if (!searchQuery.trim()) return entities;
		const q = searchQuery.toLowerCase();
		return entities.filter(
			(e: KGEntity) =>
				e.name.toLowerCase().includes(q) ||
				e.type.toLowerCase().includes(q) ||
				(e.description?.toLowerCase().includes(q) ?? false) ||
				(e.source_agent?.toLowerCase().includes(q) ?? false)
		);
	});

	let entityGroups = $derived.by((): [string, KGEntity[]][] => {
		const groups: Record<string, KGEntity[]> = {};
		for (const entity of filteredEntities) {
			const key = entity.type || 'unknown';
			(groups[key] ??= []).push(entity);
		}
		return Object.entries(groups).sort(([a], [b]) => a.localeCompare(b));
	});

	let activeInsights = $derived.by((): KGInsight[] => {
		const g = graph;
		if (!g) return [];
		return g.insights
			.filter((i: KGInsight) => !i.superseded)
			.sort((a: KGInsight, b: KGInsight) => (b.captured_at ?? 0) - (a.captured_at ?? 0));
	});

	let filteredInsights = $derived.by((): KGInsight[] => {
		if (!searchQuery.trim()) return activeInsights;
		const q = searchQuery.toLowerCase();
		return activeInsights.filter(
			(i: KGInsight) =>
				i.content.toLowerCase().includes(q) ||
				i.insight_type.toLowerCase().includes(q) ||
				(i.source_agent?.toLowerCase().includes(q) ?? false)
		);
	});

	let warnings = $derived(
		filteredInsights.filter((i: KGInsight) => i.insight_type === 'quality_warning')
	);

	let warningCount = $derived.by(() => {
		const g = graph;
		return g
			? g.insights.filter(
					(i: KGInsight) => !i.superseded && i.insight_type === 'quality_warning'
				).length
			: 0;
	});

	// --- Fetch ---

	async function fetchGraph(ventureId: string) {
		loading = true;
		error = null;
		try {
			const api = getApi();
			const resp = await api.get<{ ok: boolean; graph: KnowledgeGraph }>(
				`/knowledge-graph/${ventureId}`
			);
			graph = resp.graph;
		} catch (e: unknown) {
			const err = e as { message?: string };
			error = err.message || 'Failed to load knowledge graph';
			graph = null;
		} finally {
			loading = false;
		}
	}

	// --- React to venture changes ---

	$effect(() => {
		const v = $activeVenture;
		if (v?.venture_id) {
			fetchGraph(v.venture_id);
		} else {
			graph = null;
		}
	});

	// --- Helpers ---

	function toggleGroup(key: string) {
		collapsedGroups = { ...collapsedGroups, [key]: !collapsedGroups[key] };
	}

	function timeAgo(ts?: number): string {
		if (!ts) return '';
		const diff = Math.floor(Date.now() / 1000) - ts;
		if (diff < 60) return 'just now';
		if (diff < 3600) return `${Math.floor(diff / 60)}m ago`;
		if (diff < 86400) return `${Math.floor(diff / 3600)}h ago`;
		return `${Math.floor(diff / 86400)}d ago`;
	}

	function formatAgentName(agent?: string): string {
		if (!agent) return '';
		return agent.replace(/_/g, ' ').replace(/\b\w/g, (c) => c.toUpperCase());
	}

	function typeColor(type: string): string {
		switch (type) {
			case 'aggregate':
				return 'bg-indigo-900/40 text-indigo-300 border-indigo-700/40';
			case 'bounded_context':
				return 'bg-purple-900/40 text-purple-300 border-purple-700/40';
			case 'domain_event':
				return 'bg-sky-900/40 text-sky-300 border-sky-700/40';
			case 'command':
				return 'bg-emerald-900/40 text-emerald-300 border-emerald-700/40';
			case 'process_manager':
				return 'bg-amber-900/40 text-amber-300 border-amber-700/40';
			case 'read_model':
				return 'bg-teal-900/40 text-teal-300 border-teal-700/40';
			case 'external_system':
				return 'bg-rose-900/40 text-rose-300 border-rose-700/40';
			case 'policy':
				return 'bg-orange-900/40 text-orange-300 border-orange-700/40';
			default:
				return 'bg-surface-700 text-surface-300';
		}
	}

	function insightColor(type: string): string {
		switch (type) {
			case 'quality_warning':
				return 'bg-amber-900/30 text-amber-400 border-amber-700/50';
			case 'architectural_decision':
				return 'bg-indigo-900/30 text-indigo-300 border-indigo-700/50';
			case 'pattern_detected':
				return 'bg-sky-900/30 text-sky-300 border-sky-700/50';
			case 'constraint':
				return 'bg-rose-900/30 text-rose-300 border-rose-700/50';
			case 'recommendation':
				return 'bg-emerald-900/30 text-emerald-300 border-emerald-700/50';
			case 'observation':
				return 'bg-surface-700/60 text-surface-300 border-surface-600/50';
			default:
				return 'bg-surface-700 text-surface-300';
		}
	}

	function formatInsightType(type: string): string {
		return type.replace(/_/g, ' ');
	}

	function formatEntityType(type: string): string {
		return type.replace(/_/g, ' ');
	}

	const TABS: { key: TabKey; label: string }[] = [
		{ key: 'entities', label: 'Entities' },
		{ key: 'insights', label: 'Insights' },
		{ key: 'warnings', label: 'Warnings' }
	];
</script>

<div class="bg-surface-800/30 border border-surface-700/50 rounded-lg flex flex-col overflow-hidden">
	<!-- Header -->
	<div class="px-4 py-2.5 border-b border-surface-700/50">
		<div class="flex items-center justify-between mb-2">
			<h3 class="text-xs font-semibold text-surface-200 tracking-wide">Knowledge Graph</h3>
			{#if loading}
				<span class="text-[9px] text-hecate-400 animate-pulse">Loading...</span>
			{/if}
		</div>

		<!-- Stats bar -->
		{#if graph}
			<div class="flex items-center gap-3 text-[10px] text-surface-500 font-mono">
				<span>
					<span class="text-surface-400">{entities.length}</span> entities
				</span>
				<span class="text-surface-700">|</span>
				<span>
					<span class="text-surface-400">{activeInsights.length}</span> insights
				</span>
				<span class="text-surface-700">|</span>
				<span>
					<span class="text-surface-400">{relationships.length}</span> relationships
				</span>
				{#if warningCount > 0}
					<span class="text-surface-700">|</span>
					<span class="text-amber-400">
						<span class="font-semibold">{warningCount}</span> warnings
					</span>
				{/if}
			</div>
		{/if}
	</div>

	<!-- Tabs + Search -->
	<div class="px-4 py-2 border-b border-surface-700/50 flex items-center gap-3">
		<div class="flex items-center gap-1">
			{#each TABS as tab}
				<button
					onclick={() => (activeTab = tab.key)}
					class="text-[10px] uppercase tracking-wider px-2.5 py-1 transition-colors border-b
						{activeTab === tab.key
						? 'text-hecate-400 border-hecate-400'
						: 'text-surface-500 border-transparent hover:text-surface-300'}"
				>
					{tab.label}
					{#if tab.key === 'warnings' && warningCount > 0}
						<span
							class="ml-1 text-[8px] px-1 py-0.5 rounded-full bg-amber-500/20 text-amber-300"
						>
							{warningCount}
						</span>
					{/if}
				</button>
			{/each}
		</div>
		<div class="flex-1"></div>
		<input
			type="text"
			placeholder="Filter..."
			bind:value={searchQuery}
			class="bg-surface-800 border border-surface-600 text-xs text-surface-200 rounded px-2 py-1
				placeholder-surface-500 focus:outline-none focus:border-hecate-500/50 w-32 transition-colors"
		/>
	</div>

	<!-- Content -->
	<div class="max-h-[400px] overflow-y-auto">
		{#if error}
			<div class="px-4 py-6 text-center">
				<p class="text-xs text-health-err">{error}</p>
				{#if $activeVenture?.venture_id}
					<button
						onclick={() => fetchGraph($activeVenture!.venture_id)}
						class="mt-2 text-[10px] text-hecate-400 hover:text-hecate-300 transition-colors"
					>
						Retry
					</button>
				{/if}
			</div>
		{:else if !graph && !loading}
			<div class="text-center text-surface-500 py-6 text-xs">
				Select a venture to view its knowledge graph
			</div>
		{:else if loading && !graph}
			<div class="text-center text-surface-500 py-6 text-xs">
				Loading knowledge graph...
			</div>
		{:else if activeTab === 'entities'}
			<!-- Entities Tab -->
			{#if entityGroups.length === 0}
				<div class="text-center text-surface-500 py-6 text-xs">
					{searchQuery ? 'No entities match your filter' : 'No entities captured yet'}
				</div>
			{:else}
				{#each entityGroups as [type, groupEntities]}
					<div class="border-b border-surface-800/50 last:border-b-0">
						<!-- Group header -->
						<button
							onclick={() => toggleGroup(type)}
							class="w-full flex items-center gap-2 px-4 py-2 hover:bg-surface-700/20 transition-colors"
						>
							<span class="text-[8px] text-surface-500">
								{collapsedGroups[type] ? '\u{25B6}' : '\u{25BC}'}
							</span>
							<span
								class="text-[9px] px-1.5 py-0.5 rounded border {typeColor(type)} capitalize"
							>
								{formatEntityType(type)}
							</span>
							<span class="text-[10px] text-surface-500 ml-auto">
								{groupEntities.length}
							</span>
						</button>

						{#if !collapsedGroups[type]}
							<div class="pb-1">
								{#each groupEntities as entity}
									<div
										class="px-4 py-2 mx-2 mb-1 rounded bg-surface-800/30 hover:bg-surface-700/30 transition-colors"
									>
										<div class="flex items-start justify-between gap-2">
											<div class="flex-1 min-w-0">
												<span class="text-xs font-medium text-surface-100 block truncate">
													{entity.name}
												</span>
												{#if entity.description}
													<p class="text-[10px] text-surface-400 mt-0.5 line-clamp-2">
														{entity.description}
													</p>
												{/if}
											</div>
											{#if entity.captured_at}
												<span class="text-[9px] text-surface-600 shrink-0">
													{timeAgo(entity.captured_at)}
												</span>
											{/if}
										</div>
										<div class="flex items-center gap-2 mt-1.5">
											{#if entity.source_agent}
												<span
													class="text-[9px] px-1.5 py-0.5 rounded bg-hecate-900/20 text-hecate-400/80 border border-hecate-700/30"
												>
													{formatAgentName(entity.source_agent)}
												</span>
											{/if}
											{#if entity.division_id}
												<span class="text-[9px] text-surface-500 font-mono truncate">
													{entity.division_id}
												</span>
											{/if}
										</div>
									</div>
								{/each}
							</div>
						{/if}
					</div>
				{/each}
			{/if}
		{:else if activeTab === 'insights'}
			<!-- Insights Tab -->
			{#if filteredInsights.length === 0}
				<div class="text-center text-surface-500 py-6 text-xs">
					{searchQuery ? 'No insights match your filter' : 'No insights captured yet'}
				</div>
			{:else}
				<div class="divide-y divide-surface-800/50">
					{#each filteredInsights as insight}
						<div class="px-4 py-2.5 hover:bg-surface-700/20 transition-colors">
							<div class="flex items-start gap-2">
								<span
									class="text-[9px] px-1.5 py-0.5 rounded border shrink-0 mt-0.5 capitalize
										{insightColor(insight.insight_type)}"
								>
									{formatInsightType(insight.insight_type)}
								</span>
								<div class="flex-1 min-w-0">
									<p class="text-[11px] text-surface-200 leading-relaxed">
										{insight.content}
									</p>
									<div class="flex items-center gap-2 mt-1.5">
										{#if insight.source_agent}
											<span
												class="text-[9px] px-1.5 py-0.5 rounded bg-hecate-900/20 text-hecate-400/80 border border-hecate-700/30"
											>
												{formatAgentName(insight.source_agent)}
											</span>
										{/if}
										{#if insight.captured_at}
											<span class="text-[9px] text-surface-600">
												{timeAgo(insight.captured_at)}
											</span>
										{/if}
									</div>
								</div>
							</div>
						</div>
					{/each}
				</div>
			{/if}
		{:else if activeTab === 'warnings'}
			<!-- Warnings Tab -->
			{#if warnings.length === 0}
				<div class="text-center text-surface-500 py-6 text-xs">
					{searchQuery ? 'No warnings match your filter' : 'No quality warnings - looking good!'}
				</div>
			{:else}
				<div class="p-2 space-y-2">
					{#each warnings as warning}
						<div
							class="px-3 py-2.5 rounded border border-amber-700/50 bg-amber-900/10
								hover:bg-amber-900/20 transition-colors"
						>
							<div class="flex items-start gap-2">
								<span class="text-amber-400 text-xs mt-0.5 shrink-0">{'\u{26A0}'}</span>
								<div class="flex-1 min-w-0">
									<p class="text-[11px] text-amber-200/90 leading-relaxed">
										{warning.content}
									</p>
									<div class="flex items-center gap-2 mt-1.5">
										{#if warning.source_agent}
											<span
												class="text-[9px] px-1.5 py-0.5 rounded bg-amber-900/30 text-amber-400/80 border border-amber-700/30"
											>
												{formatAgentName(warning.source_agent)}
											</span>
										{/if}
										{#if warning.captured_at}
											<span class="text-[9px] text-amber-600">
												{timeAgo(warning.captured_at)}
											</span>
										{/if}
									</div>
								</div>
							</div>
						</div>
					{/each}
				</div>
			{/if}
		{/if}
	</div>
</div>
