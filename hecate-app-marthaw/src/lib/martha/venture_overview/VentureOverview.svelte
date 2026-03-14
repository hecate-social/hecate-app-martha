<script lang="ts">
	import { divisions, selectedDivisionId } from '../guide_venture/guide_venture.js';
	import { PHASES } from '../shared/aiStore.js';
	import {
		agentRoleStatuses,
		ROLE_META,
		type RoleStatus
	} from '../agent_orchestration/agent_orchestration.js';
	import { phaseStatusLabel, type PhaseCode, type Division } from '../types.js';

	let { onSelectDivision }: {
		onSelectDivision?: (divId: string) => void;
	} = $props();

	// --- Division graph helpers ---

	interface PhaseProgress {
		code: PhaseCode;
		name: string;
		completed: number;
		total: number;
		percent: number;
	}

	function computePhaseProgress(divs: Division[]): PhaseProgress[] {
		return PHASES.map((phase) => {
			const total = divs.length;
			const completed = divs.filter((d) => {
				const label = phaseStatusLabel(d, phase.code);
				return label === 'concluded' || label === 'completed' || label === 'submitted';
			}).length;
			const inProgress = divs.filter((d) => {
				const label = phaseStatusLabel(d, phase.code);
				return label === 'open' || label === 'active' || label === 'initiated';
			}).length;
			const progressCount = completed + inProgress * 0.5;
			return {
				code: phase.code,
				name: phase.shortName,
				completed,
				total,
				percent: total > 0 ? Math.round((progressCount / total) * 100) : 0
			};
		});
	}

	function phaseDot(div: Division, phaseCode: PhaseCode): { css: string; label: string } {
		const status = phaseStatusLabel(div, phaseCode);
		if (!status || status === 'pending')
			return { css: 'bg-surface-600', label: 'Not started' };
		if (status === 'concluded' || status === 'completed' || status === 'submitted')
			return { css: 'bg-health-ok', label: 'Complete' };
		if (status === 'shelved')
			return { css: 'bg-amber-400', label: 'Shelved' };
		if (status === 'open' || status === 'active' || status === 'initiated')
			return { css: 'bg-hecate-400', label: 'In progress' };
		return { css: 'bg-surface-500', label: status };
	}

	function activeAgentForDivision(divId: string): RoleStatus | null {
		return $agentRoleStatuses.find(
			(r) => (r.status === 'running' || r.status === 'gate_pending') &&
				r.active_session?.division_id === divId
		) ?? null;
	}

	function activeAgents(): RoleStatus[] {
		return $agentRoleStatuses.filter(
			(r) => r.status === 'running' || r.status === 'gate_pending'
		);
	}

	const phaseProgress = $derived(computePhaseProgress($divisions));
	const liveAgents = $derived(activeAgents());
</script>

<div class="flex flex-col h-full overflow-hidden">
	<!-- Header -->
	<div class="border-b border-surface-600 bg-surface-800/50 px-5 py-3 shrink-0">
		<h2 class="text-sm font-semibold text-surface-100">Venture Overview</h2>
	</div>

	<!-- Content -->
	<div class="flex-1 overflow-y-auto p-5 space-y-6">
		<!-- Division Graph -->
		<div>
			<h3 class="text-[10px] text-surface-400 uppercase tracking-wider mb-3">
				Divisions
			</h3>

			{#if $divisions.length === 0}
				<div class="text-center py-8 text-surface-500 text-xs">
					No divisions identified yet
				</div>
			{:else}
				<div class="grid grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-3">
					{#each $divisions as div (div.division_id)}
						{@const agent = activeAgentForDivision(div.division_id)}
						<button
							onclick={() => {
								selectedDivisionId.set(div.division_id);
								onSelectDivision?.(div.division_id);
							}}
							class="group text-left p-3 rounded-lg border border-surface-600
								bg-surface-800/60 hover:border-hecate-500/50 hover:bg-hecate-600/5
								transition-all"
						>
							<!-- Division name -->
							<div class="text-xs font-medium text-surface-100 truncate mb-2">
								{div.context_name}
							</div>

							<!-- Phase dots -->
							<div class="flex items-center gap-1.5 mb-2">
								{#each PHASES as phase}
									{@const dot = phaseDot(div, phase.code)}
									<span
										class="w-2 h-2 rounded-full {dot.css}"
										title="{phase.shortName}: {dot.label}"
									></span>
								{/each}
							</div>

							<!-- Active agent -->
							{#if agent}
								<div class="flex items-center gap-1.5 text-[9px]">
									<span class="w-1.5 h-1.5 rounded-full {agent.status === 'gate_pending' ? 'bg-amber-400 animate-pulse' : 'bg-hecate-400 animate-pulse'}"></span>
									<span class="text-surface-300 truncate">
										{ROLE_META[agent.role]?.label ?? agent.role}
									</span>
								</div>
							{:else}
								<div class="text-[9px] text-surface-500">idle</div>
							{/if}
						</button>
					{/each}
				</div>
			{/if}
		</div>

		<!-- Phase Progress Bars -->
		{#if $divisions.length > 0}
			<div>
				<h3 class="text-[10px] text-surface-400 uppercase tracking-wider mb-3">
					Progress
				</h3>

				<div class="space-y-2.5">
					{#each phaseProgress as phase}
						<div class="flex items-center gap-3">
							<span class="text-[10px] text-surface-300 w-16 text-right shrink-0">
								{phase.name}
							</span>
							<div class="flex-1 h-2 bg-surface-700 rounded-full overflow-hidden">
								<div
									class="h-full rounded-full transition-all duration-500
										{phase.code === 'storming' ? 'bg-orange-400/70' :
										 phase.code === 'planning' ? 'bg-blue-400/70' :
										 phase.code === 'kanban' ? 'bg-emerald-400/70' :
										 'bg-purple-400/70'}"
									style="width: {phase.percent}%"
								></div>
							</div>
							<span class="text-[10px] text-surface-400 w-10 shrink-0 tabular-nums">
								{phase.percent}%
							</span>
						</div>
					{/each}
				</div>
			</div>
		{/if}

		<!-- Agent Activity -->
		<div>
			<h3 class="text-[10px] text-surface-400 uppercase tracking-wider mb-3">
				Agent Activity
			</h3>

			{#if liveAgents.length === 0}
				<div class="text-center py-6 text-surface-500 text-xs">
					No agents currently active
				</div>
			{:else}
				<div class="space-y-2">
					{#each liveAgents as rs}
						<div class="flex items-center gap-3 px-3 py-2 rounded-lg bg-surface-800/40 border border-surface-700/50">
							<span class="w-2 h-2 rounded-full shrink-0
								{rs.status === 'gate_pending' ? 'bg-amber-400 animate-pulse' : 'bg-hecate-400 animate-pulse'}"></span>
							<span class="text-xs text-surface-200 font-medium w-20 shrink-0">
								{ROLE_META[rs.role]?.label ?? rs.role}
							</span>
							<span class="text-[10px] text-surface-400 flex-1 truncate">
								{#if rs.status === 'gate_pending'}
									waiting for gate decision
								{:else if rs.active_session?.division_id}
									working on "{rs.active_session.division_id}"
								{:else}
									running
								{/if}
							</span>
							{#if rs.active_session}
								<span class="text-[9px] text-surface-500 tabular-nums shrink-0">
									{rs.active_session.tokens_in.toLocaleString()}+{rs.active_session.tokens_out.toLocaleString()} tok
								</span>
							{/if}
						</div>
					{/each}
				</div>
			{/if}

			<!-- Idle agents summary -->
			{#if $agentRoleStatuses.filter((r) => r.status === 'idle').length > 0 || $agentRoleStatuses.filter((r) => r.status === 'completed').length > 0}
				<div class="flex items-center gap-4 mt-3 text-[9px] text-surface-500">
					{#if $agentRoleStatuses.filter((r) => r.status === 'completed').length > 0}
						<span>{$agentRoleStatuses.filter((r) => r.status === 'completed').length} completed</span>
					{/if}
					{#if $agentRoleStatuses.filter((r) => r.status === 'idle').length > 0}
						<span>{$agentRoleStatuses.filter((r) => r.status === 'idle').length} idle</span>
					{/if}
				</div>
			{/if}
		</div>
	</div>
</div>
