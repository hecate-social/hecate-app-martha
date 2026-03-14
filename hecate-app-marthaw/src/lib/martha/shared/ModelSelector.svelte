<script lang="ts">
	import {
		availableModels,
		modelAffinity,
		formatContextLength,
		topRecommendations,
		type PhaseCode
	} from './aiStore.js';
	import type { LLMModel } from '../types.js';

	interface Props {
		currentModel: string | null;
		onSelect: (name: string) => void;
		showPhaseInfo?: boolean;
		phasePreference?: string | null;
		phaseAffinity?: 'code' | 'general' | 'creative';
		onPinModel?: (name: string) => void;
		onClearPin?: () => void;
		phaseName?: string;
	}

	let {
		currentModel,
		onSelect,
		showPhaseInfo = false,
		phasePreference = null,
		phaseAffinity = 'general',
		onPinModel,
		onClearPin,
		phaseName = ''
	}: Props = $props();

	let isOpen = $state(false);
	let searchQuery = $state('');
	let dropdownEl: HTMLDivElement | undefined = $state();
	let searchEl: HTMLInputElement | undefined = $state();
	let expandedProviders = $state(new Set<string>());

	const PROVIDER_ORDER = ['Anthropic', 'OpenAI', 'Google', 'Groq', 'Ollama', 'Other'];

	type ModelGroup = { provider: string; models: LLMModel[] };

	let groupedModels = $derived.by(() => {
		const models = $availableModels;
		const q = searchQuery.toLowerCase();
		const filtered = q
			? models.filter((m) =>
				m.name.toLowerCase().includes(q) ||
				m.provider.toLowerCase().includes(q) ||
				m.family.toLowerCase().includes(q))
			: models;

		const groups = new Map<string, LLMModel[]>();
		for (const m of filtered) {
			const provider = normalizeProviderName(m.provider);
			const list = groups.get(provider) ?? [];
			list.push(m);
			groups.set(provider, list);
		}

		const result: ModelGroup[] = [];
		const ordered = [...groups.keys()].sort((a, b) => {
			const ai = PROVIDER_ORDER.indexOf(a);
			const bi = PROVIDER_ORDER.indexOf(b);
			const aIdx = ai === -1 ? 999 : ai;
			const bIdx = bi === -1 ? 999 : bi;
			return aIdx - bIdx || a.localeCompare(b);
		});

		for (const provider of ordered) {
			const list = groups.get(provider) ?? [];
			// Sort within provider: larger context first, then alphabetical
			list.sort((a, b) => b.context_length - a.context_length || a.name.localeCompare(b.name));
			result.push({ provider, models: list });
		}
		return result;
	});

	// Smart recommendations based on current phase affinity
	let recommended = $derived(
		topRecommendations($availableModels, phaseAffinity as 'code' | 'general' | 'creative', 4)
	);

	let totalCount = $derived($availableModels.length);

	// Current model metadata
	let currentModelInfo = $derived(
		$availableModels.find((m) => m.name === currentModel) ?? null
	);

	function normalizeProviderName(provider: string): string {
		const lower = provider.toLowerCase();
		if (lower.includes('anthropic')) return 'Anthropic';
		if (lower.includes('openai')) return 'OpenAI';
		if (lower.includes('google') || lower.includes('gemini')) return 'Google';
		if (lower.includes('groq')) return 'Groq';
		if (lower.includes('ollama')) return 'Ollama';
		if (!provider) return 'Other';
		return provider.charAt(0).toUpperCase() + provider.slice(1);
	}

	function providerIcon(provider: string): string {
		switch (provider) {
			case 'Anthropic': return '\u{25C6}';
			case 'OpenAI': return '\u{25CB}';
			case 'Google': return '\u{25B3}';
			case 'Groq': return '\u{26A1}';
			case 'Ollama': return '\u{2302}';
			default: return '\u{25CF}';
		}
	}

	function modelBadges(m: LLMModel): Array<{ label: string; css: string }> {
		const badges: Array<{ label: string; css: string }> = [];
		const ctx = formatContextLength(m.context_length);
		if (ctx) badges.push({ label: ctx, css: 'text-surface-400 bg-surface-700' });
		if (m.format !== 'api') badges.push({ label: 'local', css: 'text-emerald-400 bg-emerald-500/10' });
		if (modelAffinity(m.name) === 'code') badges.push({ label: 'code', css: 'text-hecate-400 bg-hecate-500/10' });
		if (m.parameter_size && m.parameter_size !== '' && m.parameter_size !== 'unknown')
			badges.push({ label: m.parameter_size, css: 'text-surface-400 bg-surface-700' });
		return badges;
	}

	function handleSelect(name: string) {
		onSelect(name);
		isOpen = false;
		searchQuery = '';
		expandedProviders = new Set();
	}

	function toggleProvider(provider: string) {
		const next = new Set(expandedProviders);
		if (next.has(provider)) {
			next.delete(provider);
		} else {
			next.add(provider);
		}
		expandedProviders = next;
	}

	function handleClickOutside(e: MouseEvent) {
		if (dropdownEl && !dropdownEl.contains(e.target as Node)) {
			isOpen = false;
			searchQuery = '';
			expandedProviders = new Set();
		}
	}

	function shortName(name: string): string {
		if (name.length <= 24) return name;
		return name.slice(0, 22) + '\u2026';
	}

	function handleKeydown(e: KeyboardEvent) {
		if (e.key === 'Escape') {
			isOpen = false;
			searchQuery = '';
		}
	}

	$effect(() => {
		if (isOpen) {
			document.addEventListener('click', handleClickOutside, true);
			requestAnimationFrame(() => searchEl?.focus());
		} else {
			document.removeEventListener('click', handleClickOutside, true);
		}
		return () => document.removeEventListener('click', handleClickOutside, true);
	});
</script>

<div class="relative" bind:this={dropdownEl}>
	<!-- Trigger button -->
	<button
		onclick={() => (isOpen = !isOpen)}
		class="text-[10px] px-2 py-0.5 rounded bg-surface-700 text-surface-300
			hover:bg-surface-600 transition-colors truncate max-w-[220px] flex items-center gap-1"
		title={currentModel ? `${currentModel}${currentModelInfo ? ` (${currentModelInfo.provider}, ${formatContextLength(currentModelInfo.context_length)} ctx)` : ''}` : 'No model selected'}
	>
		{#if currentModel}
			<span class="truncate">{shortName(currentModel)}</span>
			{#if currentModelInfo}
				{@const ctx = formatContextLength(currentModelInfo.context_length)}
				{#if ctx}
					<span class="text-[8px] text-surface-500">{ctx}</span>
				{/if}
			{/if}
		{:else}
			<span class="text-surface-500">Select model</span>
		{/if}
		<span class="text-[8px] ml-0.5">{isOpen ? '\u{25B2}' : '\u{25BC}'}</span>
	</button>

	<!-- Dropdown -->
	{#if isOpen}
		<!-- svelte-ignore a11y_no_static_element_interactions -->
		<div
			onkeydown={handleKeydown}
			class="absolute top-full left-0 mt-1 w-96 max-h-[460px] overflow-hidden
				bg-surface-800 border border-surface-600 rounded-lg shadow-xl z-50
				flex flex-col"
		>
			<!-- Search input -->
			<div class="p-2 border-b border-surface-700">
				<input
					bind:this={searchEl}
					bind:value={searchQuery}
					placeholder="Search {totalCount} models..."
					class="w-full bg-surface-700 border border-surface-600 rounded px-2 py-1
						text-[11px] text-surface-100 placeholder-surface-500
						focus:outline-none focus:border-hecate-500"
				/>
			</div>

			<!-- Phase info -->
			{#if showPhaseInfo && phaseName}
				<div class="px-2 py-1.5 border-b border-surface-700 flex items-center justify-between">
					<span class="text-[9px] text-surface-400">
						Phase: <span class="text-surface-200">{phaseName}</span>
						{#if phaseAffinity === 'code'}
							<span class="text-hecate-400 ml-1">(code-optimized)</span>
						{:else if phaseAffinity === 'creative'}
							<span class="text-orange-400 ml-1">(creative)</span>
						{/if}
					</span>
					{#if phasePreference}
						<button
							onclick={() => onClearPin?.()}
							class="text-[9px] text-surface-500 hover:text-surface-300"
							title="Clear pinned model for this phase"
						>
							Unpin
						</button>
					{/if}
				</div>
			{/if}

			<!-- Model list -->
			<div class="overflow-y-auto flex-1">
				{#if groupedModels.length === 0}
					<div class="p-3 text-center text-[11px] text-surface-500">
						{totalCount === 0 ? 'No models available' : 'No matching models'}
					</div>
				{/if}

				<!-- Smart recommendations (always visible when not searching) -->
				{#if !searchQuery && recommended.length > 0}
					<div class="py-1 border-b border-surface-700">
						<div class="px-2 py-1 text-[9px] text-hecate-400 uppercase tracking-wider font-medium">
							Recommended{phaseAffinity !== 'general' ? ` for ${phaseAffinity}` : ''}
						</div>
						{#each recommended as { model: rec, tier }}
							{@const isSelected = rec.name === currentModel}
							{@const badges = modelBadges(rec)}
							<!-- svelte-ignore a11y_click_events_have_key_events -->
							<!-- svelte-ignore a11y_no_static_element_interactions -->
							<div
								onclick={() => handleSelect(rec.name)}
								class="w-full text-left px-2 py-1.5 text-[11px] flex items-center gap-1.5
									transition-colors cursor-pointer
									{isSelected
									? 'bg-hecate-600/20 text-hecate-300'
									: 'text-surface-200 hover:bg-surface-700'}"
							>
								<span class="text-[8px] text-surface-500 w-3 text-center shrink-0">{providerIcon(normalizeProviderName(rec.provider))}</span>
								<span class="truncate flex-1">{rec.name}</span>
								{#each badges as badge}
									<span class="text-[8px] px-1 py-0.5 rounded {badge.css} shrink-0">{badge.label}</span>
								{/each}
								<span class="text-[8px] px-1 py-0.5 rounded shrink-0
									{tier === 'flagship' ? 'text-amber-400 bg-amber-500/10' :
									 tier === 'balanced' ? 'text-blue-400 bg-blue-500/10' :
									 tier === 'fast' ? 'text-emerald-400 bg-emerald-500/10' :
									 'text-surface-400 bg-surface-700'}">{tier}</span>
								{#if isSelected}
									<span class="text-[9px] text-hecate-400 shrink-0">{'\u{2713}'}</span>
								{/if}
							</div>
						{/each}
					</div>
				{/if}

				<!-- Provider groups -->
				{#each groupedModels as group}
					{@const isExpanded = searchQuery !== '' || expandedProviders.has(group.provider)}
					<div class="py-0.5">
						<!-- Provider header -->
						<!-- svelte-ignore a11y_click_events_have_key_events -->
						<!-- svelte-ignore a11y_no_static_element_interactions -->
						<div
							onclick={() => !searchQuery && toggleProvider(group.provider)}
							class="px-2 py-1 text-[9px] uppercase tracking-wider font-medium flex items-center gap-1.5
								{searchQuery ? 'text-surface-500' : 'text-surface-500 hover:text-surface-300 cursor-pointer'}"
						>
							{#if !searchQuery}
								<span class="text-[8px] w-3 text-center">{isExpanded ? '\u{25BC}' : '\u{25B6}'}</span>
							{/if}
							<span class="text-[9px]">{providerIcon(group.provider)}</span>
							<span>{group.provider}</span>
							<span class="text-surface-600 font-normal">({group.models.length})</span>
						</div>

						<!-- Models -->
						{#if isExpanded}
							{#each group.models as m}
								{@const isSelected = m.name === currentModel}
								{@const isPinned = m.name === phasePreference}
								{@const badges = modelBadges(m)}
								<!-- svelte-ignore a11y_click_events_have_key_events -->
								<!-- svelte-ignore a11y_no_static_element_interactions -->
								<div
									onclick={() => handleSelect(m.name)}
									class="w-full text-left px-2 py-1.5 text-[11px] flex items-center gap-1.5
										transition-colors cursor-pointer
										{searchQuery ? 'pl-2' : 'pl-7'}
										{isSelected
										? 'bg-hecate-600/20 text-hecate-300'
										: 'text-surface-200 hover:bg-surface-700'}"
								>
									<span class="truncate flex-1">{m.name}</span>
									{#each badges as badge}
										<span class="text-[8px] px-1 py-0.5 rounded {badge.css} shrink-0">{badge.label}</span>
									{/each}
									{#if isPinned}
										<span class="text-[8px] text-hecate-400 shrink-0" title="Pinned for this phase">{'\u{1F4CC}'}</span>
									{/if}
									{#if isSelected}
										<span class="text-[9px] text-hecate-400 shrink-0">{'\u{2713}'}</span>
									{/if}
									{#if showPhaseInfo && onPinModel && !isPinned}
										<button
											onclick={(e: MouseEvent) => { e.stopPropagation(); onPinModel?.(m.name); }}
											class="text-[8px] text-surface-600 hover:text-hecate-400 shrink-0"
											title="Pin for {phaseName} phase"
										>
											pin
										</button>
									{/if}
								</div>
							{/each}
						{/if}
					</div>
				{/each}
			</div>
		</div>
	{/if}
</div>
