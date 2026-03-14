<script lang="ts">
	import {
		divisions,
		selectedDivisionId
	} from '$lib/martha/guide_venture/guide_venture.js';
	import { selectedPhase } from '$lib/martha/shared/phaseStore.js';
	import { PHASES } from '$lib/martha/shared/aiStore.js';
	import {
		agentRoleStatuses,
		pendingGates,
		type RoleStatus,
		type AgentSession
	} from '$lib/martha/agent_orchestration/agent_orchestration.js';
	import { phaseStatusLabel, phaseAvailableActions, type PhaseCode } from '$lib/martha/types.js';

	let { onSelectAgent, onSelectGate }: {
		onSelectAgent?: (role: string) => void;
		onSelectGate?: (sessionId: string) => void;
	} = $props();

	let collapsedSections = $state<Record<string, boolean>>({});

	function toggleSection(key: string) {
		collapsedSections = { ...collapsedSections, [key]: !collapsedSections[key] };
	}

	function selectDivision(divId: string) {
		selectedDivisionId.set(divId);
	}

	function selectPhase(divId: string, phase: PhaseCode) {
		selectedDivisionId.set(divId);
		selectedPhase.set(phase);
	}

	function phaseVisual(label: string, actions: string[]): { icon: string; css: string } {
		if (!label) return { icon: '\u{25CB}', css: 'text-surface-500' };
		if (actions.length === 0) return { icon: '\u{25CF}', css: 'text-health-ok' };
		if (actions.includes('resume')) return { icon: '\u{25CB}', css: 'text-health-warn' };
		if (actions.includes('shelve') || actions.includes('conclude') || actions.includes('archive'))
			return { icon: '\u{25D0}', css: 'text-hecate-400' };
		if (actions.includes('open')) return { icon: '\u{25CB}', css: 'text-surface-300' };
		return { icon: '\u{25CB}', css: 'text-surface-500' };
	}

	function agentStatusIcon(status: string): { icon: string; css: string } {
		switch (status) {
			case 'running': return { icon: '\u{25CF}', css: 'text-hecate-400 animate-pulse' };
			case 'gate_pending': return { icon: '\u{25CF}', css: 'text-amber-400 animate-pulse' };
			case 'completed': return { icon: '\u{2713}', css: 'text-health-ok' };
			case 'failed': return { icon: '\u{2717}', css: 'text-health-err' };
			default: return { icon: '\u{25CB}', css: 'text-surface-500' };
		}
	}

	function agentDisplayName(role: string): string {
		return role
			.replace(/_/g, ' ')
			.replace(/\b\w/g, (c) => c.toUpperCase());
	}
</script>

<div class="w-52 border-r border-surface-600 bg-surface-800/30 overflow-y-auto shrink-0 flex flex-col">
	<!-- Division Map -->
	<div class="border-b border-surface-700/50">
		<button
			onclick={() => toggleSection('divisions')}
			class="w-full flex items-center gap-1.5 px-3 py-2 text-[10px] text-surface-400
				uppercase tracking-wider hover:text-surface-300 transition-colors"
		>
			<span class="text-[8px]">{collapsedSections['divisions'] ? '\u{25B6}' : '\u{25BC}'}</span>
			<span>Divisions</span>
			<span class="ml-auto text-surface-500">{$divisions.length}</span>
		</button>

		{#if !collapsedSections['divisions']}
			<div class="px-2 pb-2 space-y-1">
				{#each $divisions as div}
					{@const isSelected = $selectedDivisionId === div.division_id}
					<div>
						<button
							onclick={() => selectDivision(div.division_id)}
							class="w-full text-left px-2 py-1 rounded text-xs transition-colors
								{isSelected
								? 'bg-surface-700 text-surface-100'
								: 'text-surface-300 hover:bg-surface-700/50 hover:text-surface-100'}"
						>
							<span class="font-medium truncate block">{div.context_name}</span>
						</button>

						<!-- Compact phase dots (always visible) -->
						<div class="flex items-center gap-1 ml-2 mt-0.5">
							{#each PHASES as phase}
								{@const label = phaseStatusLabel(div, phase.code)}
								{@const actions = phaseAvailableActions(div, phase.code)}
								{@const { icon, css } = phaseVisual(label, actions)}
								<button
									onclick={() => selectPhase(div.division_id, phase.code)}
									class="text-[9px] {css} hover:opacity-80 transition-opacity"
									title="{phase.shortName}: {label || 'Pending'}"
								>
									{icon}
								</button>
							{/each}
						</div>
					</div>
				{/each}

				{#if $divisions.length === 0}
					<div class="text-[10px] text-surface-500 px-2 py-3 text-center">
						No divisions yet
					</div>
				{/if}
			</div>
		{/if}
	</div>

	<!-- Agent Roster -->
	<div class="border-b border-surface-700/50">
		<button
			onclick={() => toggleSection('agents')}
			class="w-full flex items-center gap-1.5 px-3 py-2 text-[10px] text-surface-400
				uppercase tracking-wider hover:text-surface-300 transition-colors"
		>
			<span class="text-[8px]">{collapsedSections['agents'] ? '\u{25B6}' : '\u{25BC}'}</span>
			<span>Agents</span>
			{#if $agentRoleStatuses.filter((r) => r.status === 'running').length > 0}
				<span class="ml-auto text-[9px] px-1.5 py-0.5 rounded-full bg-hecate-600/20 text-hecate-300">
					{$agentRoleStatuses.filter((r) => r.status === 'running').length} active
				</span>
			{/if}
		</button>

		{#if !collapsedSections['agents']}
			<div class="px-2 pb-2 space-y-0.5">
				{#each $agentRoleStatuses as rs}
					{@const { icon, css } = agentStatusIcon(rs.status)}
					<button
						onclick={() => onSelectAgent?.(rs.role)}
						class="w-full flex items-center gap-1.5 px-2 py-1 rounded text-[10px]
							text-surface-300 hover:bg-surface-700/50 transition-colors"
					>
						<span class="{css} text-[8px]">{icon}</span>
						<span class="truncate">{agentDisplayName(rs.role)}</span>
						{#if rs.active_session?.division_id}
							<span class="ml-auto text-[9px] text-surface-500 truncate max-w-[60px]">
								{rs.active_session.division_id}
							</span>
						{/if}
					</button>
				{/each}
			</div>
		{/if}
	</div>

	<!-- Gate Queue -->
	<div class="flex-1">
		<button
			onclick={() => toggleSection('gates')}
			class="w-full flex items-center gap-1.5 px-3 py-2 text-[10px] text-surface-400
				uppercase tracking-wider hover:text-surface-300 transition-colors"
		>
			<span class="text-[8px]">{collapsedSections['gates'] ? '\u{25B6}' : '\u{25BC}'}</span>
			<span>Gates</span>
			{#if $pendingGates.length > 0}
				<span class="ml-auto text-[9px] px-1.5 py-0.5 rounded-full bg-amber-500/20 text-amber-300 animate-pulse">
					{$pendingGates.length}
				</span>
			{/if}
		</button>

		{#if !collapsedSections['gates']}
			<div class="px-2 pb-2 space-y-1">
				{#each $pendingGates as gate}
					<button
						onclick={() => onSelectGate?.(gate.session_id)}
						class="w-full text-left p-2 rounded bg-amber-500/5 border border-amber-500/20
							hover:bg-amber-500/10 transition-colors"
					>
						<div class="flex items-center gap-1.5">
							<span class="text-[8px] text-amber-400 animate-pulse">{'\u{25CF}'}</span>
							<span class="text-[10px] font-medium text-amber-300 truncate">
								{agentDisplayName(gate.role)}
							</span>
						</div>
						{#if gate.division_id}
							<div class="text-[9px] text-surface-400 ml-3 mt-0.5 truncate">
								{gate.division_id}
							</div>
						{/if}
					</button>
				{/each}

				{#if $pendingGates.length === 0}
					<div class="text-[10px] text-surface-500 px-2 py-3 text-center">
						No pending gates
					</div>
				{/if}
			</div>
		{/if}
	</div>
</div>
