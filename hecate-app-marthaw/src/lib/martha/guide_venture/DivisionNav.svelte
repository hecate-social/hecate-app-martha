<script lang="ts">
	import { divisions, selectedDivisionId } from '$lib/martha/guide_venture/guide_venture.js';
	import { selectedPhase } from '$lib/martha/shared/phaseStore.js';
	import { PHASES } from '$lib/martha/shared/aiStore.js';
	import { phaseStatusLabel, phaseAvailableActions, type PhaseCode } from '$lib/martha/types.js';

	function selectDivision(divId: string) {
		selectedDivisionId.set(divId);
	}

	function selectPhase(divId: string, phase: PhaseCode) {
		selectedDivisionId.set(divId);
		selectedPhase.set(phase);
	}

	/** Derive visual state from available_actions + label presence.
	 *  No label parsing — only structural checks. */
	function phaseVisual(label: string, actions: string[]): { icon: string; css: string } {
		if (!label) return { icon: '\u{25CB}', css: 'text-surface-500' };
		if (actions.length === 0) return { icon: '\u{25CF}', css: 'text-health-ok' };
		if (actions.includes('resume')) return { icon: '\u{25CB}', css: 'text-health-warn' };
		if (actions.includes('shelve') || actions.includes('conclude') || actions.includes('archive'))
			return { icon: '\u{25D0}', css: 'text-hecate-400' };
		if (actions.includes('open')) return { icon: '\u{25CB}', css: 'text-surface-300' };
		return { icon: '\u{25CB}', css: 'text-surface-500' };
	}

	function labelCss(label: string, actions: string[]): string {
		if (!label) return 'text-surface-500';
		if (actions.length === 0) return 'text-health-ok';
		if (actions.includes('resume')) return 'text-health-warn';
		if (actions.includes('shelve') || actions.includes('conclude') || actions.includes('archive'))
			return 'text-hecate-400';
		return 'text-surface-300';
	}
</script>

<div
	class="w-48 border-r border-surface-600 bg-surface-800/30 overflow-y-auto shrink-0"
>
	<div class="p-3">
		<div class="text-[10px] text-surface-400 uppercase tracking-wider mb-2">
			Divisions
		</div>

		{#each $divisions as div}
			{@const isSelected = $selectedDivisionId === div.division_id}
			<div class="mb-2">
				<!-- Division name -->
				<button
					onclick={() => selectDivision(div.division_id)}
					class="w-full text-left px-2 py-1.5 rounded text-xs transition-colors
						{isSelected
						? 'bg-surface-700 text-surface-100'
						: 'text-surface-300 hover:bg-surface-700/50 hover:text-surface-100'}"
				>
					<span class="font-medium">{div.context_name}</span>
				</button>

				<!-- Phase indicators -->
				{#if isSelected}
					<div class="ml-2 mt-1 space-y-0.5">
						{#each PHASES as phase}
							{@const label = phaseStatusLabel(div, phase.code)}
							{@const actions = phaseAvailableActions(div, phase.code)}
							{@const { icon, css } = phaseVisual(label, actions)}
							<button
								onclick={() => selectPhase(div.division_id, phase.code)}
								class="w-full flex items-center gap-1.5 px-2 py-0.5 rounded text-[10px]
									transition-colors
									{$selectedPhase === phase.code
									? 'bg-surface-600/50 text-surface-100'
									: 'text-surface-400 hover:text-surface-300'}"
							>
								<span class={css}>{icon}</span>
								<span>{phase.shortName}</span>
								<span class="ml-auto text-[9px] {labelCss(label, actions)}">
									{label || 'Pending'}
								</span>
							</button>
						{/each}
					</div>
				{/if}
			</div>
		{/each}

		{#if $divisions.length === 0}
			<div class="text-[10px] text-surface-400 px-2 py-4 text-center">
				No divisions yet.
				<br />
				Start discovery to identify them.
			</div>
		{/if}
	</div>
</div>
