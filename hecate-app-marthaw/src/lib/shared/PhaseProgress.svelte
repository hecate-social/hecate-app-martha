<script lang="ts">
	import { selectedDivision } from '$lib/guide_venture/guide_venture.js';
	import { selectedPhase, openPhase, shelvePhase, resumePhase, concludePhase, isLoading } from '$lib/shared/phaseStore.js';
	import { PHASES, openAIAssist } from '$lib/shared/aiStore.js';
	import {
		phaseStatusLabel,
		phaseAvailableActions,
		type PhaseCode
	} from '$lib/types.js';

	let currentActions = $derived(
		$selectedDivision ? phaseAvailableActions($selectedDivision, $selectedPhase) : []
	);

	function setPhase(code: PhaseCode) {
		selectedPhase.set(code);
	}

	function phaseColor(code: PhaseCode, isActive: boolean): string {
		if (!isActive) return '';
		switch (code) {
			case 'storming':
				return 'border-es-event text-es-event';
			case 'planning':
				return 'border-phase-planning text-phase-planning';
			case 'kanban':
				return 'border-hecate-400 text-hecate-400';
			case 'crafting':
				return 'border-phase-crafting text-phase-crafting';
		}
	}

	/** Derive phase visual state from available_actions + label presence.
	 *  No label parsing — only structural checks. */
	function phaseVisual(label: string, actions: string[]): { icon: string; css: string } {
		if (!label) return { icon: '\u{25CB}', css: 'text-surface-500' };
		if (actions.length === 0) return { icon: '\u{2713}', css: 'text-health-ok' };
		if (actions.includes('resume')) return { icon: '\u{25D0}', css: 'text-health-warn' };
		if (actions.includes('shelve') || actions.includes('conclude') || actions.includes('archive'))
			return { icon: '\u{25CF}', css: 'text-hecate-400 animate-pulse' };
		if (actions.includes('open')) return { icon: '\u{25CB}', css: 'text-surface-300' };
		return { icon: '\u{25CB}', css: 'text-surface-500' };
	}

	/** Map action name to button style. */
	function actionStyle(action: string): string {
		switch (action) {
			case 'open':
				return 'bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30';
			case 'shelve':
				return 'text-surface-400 hover:text-health-warn hover:bg-surface-700';
			case 'conclude':
				return 'text-surface-400 hover:text-health-ok hover:bg-surface-700';
			case 'resume':
				return 'bg-health-warn/10 text-health-warn hover:bg-health-warn/20';
			case 'archive':
				return 'text-surface-400 hover:text-surface-200 hover:bg-surface-700';
			default:
				return 'text-surface-400 hover:bg-surface-700';
		}
	}

	function actionLabel(action: string): string {
		return action.charAt(0).toUpperCase() + action.slice(1);
	}

	async function handlePhaseAction(action: string) {
		if (!$selectedDivision) return;
		const divId = $selectedDivision.division_id;
		const phase = $selectedPhase;

		switch (action) {
			case 'open':
				await openPhase(divId, phase);
				break;
			case 'shelve':
				await shelvePhase(divId, phase);
				break;
			case 'resume':
				await resumePhase(divId, phase);
				break;
			case 'conclude':
				await concludePhase(divId, phase);
				break;
		}
	}
</script>

{#if $selectedDivision}
	<div class="border-b border-surface-600 bg-surface-800/30 px-4 py-2 shrink-0">
		<div class="flex items-center gap-1">
			<!-- Phase tabs -->
			{#each PHASES as phase, i}
				{@const label = phaseStatusLabel($selectedDivision, phase.code)}
				{@const actions = phaseAvailableActions($selectedDivision, phase.code)}
				{@const isActive = $selectedPhase === phase.code}
				{@const { icon, css } = phaseVisual(label, actions)}
				{@const isTerminal = label && actions.length === 0}

				{#if i > 0}
					<div
						class="w-4 h-px {isTerminal ? 'bg-health-ok/40' : 'bg-surface-600'}"
					></div>
				{/if}

				<button
					onclick={() => setPhase(phase.code)}
					class="flex items-center gap-1.5 px-3 py-1.5 rounded text-xs transition-all
						border
						{isActive
						? `bg-surface-700 border-current ${phaseColor(phase.code, true)}`
						: 'border-transparent text-surface-400 hover:text-surface-200 hover:bg-surface-700/50'}"
				>
					<span class="{css} text-[10px]">{icon}</span>
					<span>{phase.shortName}</span>
				</button>
			{/each}

			<div class="flex-1"></div>

			<!-- Phase info -->
			<span class="text-[10px] text-surface-400 mr-2">
				{PHASES.find((p) => p.code === $selectedPhase)?.name}
			</span>

			<!-- Phase lifecycle buttons — driven by backend available_actions -->
			{#if currentActions.length === 0}
				{@const label = phaseStatusLabel($selectedDivision, $selectedPhase)}
				{#if !label}
					<span class="text-[10px] text-surface-500 mr-1">Pending</span>
				{/if}
			{:else}
				{#each currentActions as action}
					<button
						onclick={() => handlePhaseAction(action)}
						disabled={$isLoading}
						class="text-[10px] px-2 py-0.5 rounded transition-colors disabled:opacity-50
							{actionStyle(action)}"
					>
						{actionLabel(action)}
					</button>
				{/each}
			{/if}

			<!-- AI Assist toggle -->
			<button
				onclick={() =>
					openAIAssist(
						`Help with ${PHASES.find((p) => p.code === $selectedPhase)?.name} phase for division "${$selectedDivision?.context_name}"`
					)}
				class="text-[10px] px-2 py-0.5 rounded text-hecate-400
					hover:bg-hecate-600/20 transition-colors ml-1"
				title="Open AI Assistant"
			>
				{'\u{2726}'} AI Assist
			</button>
		</div>
	</div>
{/if}
