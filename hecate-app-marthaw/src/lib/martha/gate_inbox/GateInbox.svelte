<script lang="ts">
	import {
		pendingGates,
		agentSessions,
		passGate,
		rejectGate,
		ROLE_META,
		type AgentSession,
		type AgentRole
	} from '../agent_orchestration/agent_orchestration.js';
	import { activeVenture } from '../guide_venture/guide_venture.js';
	import RetryBadge from '../retry_strategy/RetryBadge.svelte';

	let expandedGateId = $state<string | null>(null);
	let rejectingGateId = $state<string | null>(null);
	let rejectReason = $state('');

	// Decided gates: recently passed or rejected
	const decidedGates = $derived(
		$agentSessions
			.filter((s) => s.status === 'gate_passed' || s.status === 'gate_rejected')
			.sort((a, b) => (b.completed_at ?? 0) - (a.completed_at ?? 0))
			.slice(0, 10)
	);

	function gateTypeLabel(role: AgentRole): string {
		switch (role) {
			case 'visionary': return 'VISION GATE';
			case 'explorer': return 'BOUNDARY GATE';
			case 'stormer': return 'DESIGN GATE';
			case 'reviewer': return 'REVIEW GATE';
			case 'architect': return 'ARCHITECTURE GATE';
			case 'tester': return 'TEST GATE';
			default: return `${ROLE_META[role]?.label?.toUpperCase() ?? role.toUpperCase()} GATE`;
		}
	}

	function timeAgo(ts: number | null): string {
		if (!ts) return '';
		const diffSec = Math.floor((Date.now() - ts) / 1000);
		if (diffSec < 5) return 'just now';
		if (diffSec < 60) return `${diffSec}s ago`;
		const diffMin = Math.floor(diffSec / 60);
		if (diffMin < 60) return `${diffMin}m ago`;
		const diffHr = Math.floor(diffMin / 60);
		return `${diffHr}h ago`;
	}

	function gateSummary(session: AgentSession): string {
		if (session.gate_output) {
			const lines = session.gate_output.split('\n').filter((l) => l.trim());
			if (lines.length > 0) return lines[0].slice(0, 120);
		}
		if (session.output) {
			const lines = session.output.split('\n').filter((l) => l.trim());
			if (lines.length > 0) return lines[0].slice(0, 120);
		}
		return 'Awaiting review';
	}

	async function handlePass(session: AgentSession) {
		const ventureId = $activeVenture?.venture_id;
		if (!ventureId) return;
		await passGate(ventureId, session.role, session.session_id);
	}

	async function handleReject(session: AgentSession) {
		const ventureId = $activeVenture?.venture_id;
		if (!ventureId || !rejectReason.trim()) return;
		await rejectGate(ventureId, session.role, session.session_id, rejectReason.trim());
		rejectReason = '';
		rejectingGateId = null;
	}
</script>

<div class="flex flex-col h-full overflow-hidden">
	<!-- Header -->
	<div class="border-b border-surface-600 bg-surface-800/50 px-5 py-3 shrink-0">
		<div class="flex items-center gap-3">
			<h2 class="text-sm font-semibold text-surface-100">Gates</h2>
			{#if $pendingGates.length > 0}
				<span class="px-2 py-0.5 rounded-full bg-amber-500/20 text-amber-300 text-[10px] font-medium">
					{$pendingGates.length} pending
				</span>
			{/if}
		</div>
	</div>

	<!-- Content -->
	<div class="flex-1 overflow-y-auto p-5 space-y-6">
		<!-- Pending Section -->
		<div>
			<h3 class="text-[10px] text-surface-400 uppercase tracking-wider mb-3 flex items-center gap-2">
				<span class="w-1.5 h-1.5 rounded-full bg-amber-400 animate-pulse"></span>
				Pending
			</h3>

			{#if $pendingGates.length === 0}
				<div class="text-center py-8 text-surface-500 text-xs">
					No gates awaiting decision
				</div>
			{:else}
				<div class="space-y-3">
					{#each $pendingGates as gate (gate.session_id)}
						{@const isExpanded = expandedGateId === gate.session_id}
						{@const isRejecting = rejectingGateId === gate.session_id}
						<div class="rounded-lg border border-amber-500/20 bg-amber-500/5 overflow-hidden">
							<!-- Gate card header -->
							<button
								onclick={() => { expandedGateId = isExpanded ? null : gate.session_id; }}
								class="w-full text-left px-4 py-3 hover:bg-amber-500/10 transition-colors"
							>
								<div class="flex items-center gap-3">
									<span class="text-amber-400 text-[8px] animate-pulse">{'\u{25CF}'}</span>
									<span class="text-xs font-semibold text-amber-300">
										{gateTypeLabel(gate.role)}
									</span>
									{#if gate.division_id}
										<span class="text-[10px] text-surface-400">{'\u{00B7}'}</span>
										<span class="text-[10px] text-surface-300">{gate.division_id}</span>
									{/if}
									<span class="text-[10px] text-surface-400">{'\u{00B7}'}</span>
									<span class="text-[10px] text-surface-400">
										{ROLE_META[gate.role]?.label ?? gate.role}
									</span>
									<span class="text-[10px] text-surface-500 ml-auto">
										{timeAgo(gate.started_at)}
									</span>
									<span class="text-[8px] text-surface-500">
										{isExpanded ? '\u{25BC}' : '\u{25B6}'}
									</span>
								</div>
								<div class="text-[10px] text-surface-400 mt-1 ml-5 truncate">
									{gateSummary(gate)}
								</div>
							</button>

							<!-- Expanded content -->
							{#if isExpanded}
								<div class="border-t border-amber-500/10 px-4 py-3 space-y-3">
									<!-- Gate output -->
									{#if gate.gate_output || gate.output}
										<div class="rounded bg-surface-800 border border-surface-600 p-3 max-h-64 overflow-y-auto">
											<pre class="text-[10px] text-surface-200 whitespace-pre-wrap font-mono leading-relaxed">{gate.gate_output || gate.output}</pre>
										</div>
									{/if}

									<!-- Token usage -->
									<div class="flex items-center gap-4 text-[9px] text-surface-400">
										<span>Tokens: {gate.tokens_in.toLocaleString()} in / {gate.tokens_out.toLocaleString()} out</span>
										{#if gate.started_at}
											<span>Started: {new Date(gate.started_at).toLocaleTimeString()}</span>
										{/if}
									</div>

									<!-- Actions -->
									{#if isRejecting}
										<div class="space-y-2">
											<textarea
												bind:value={rejectReason}
												placeholder="Reason for rejecting..."
												rows="2"
												class="w-full bg-surface-700 border border-surface-600 rounded px-2.5 py-1.5
													text-xs text-surface-100 placeholder-surface-400
													focus:outline-none focus:border-health-err/50 resize-none"
											></textarea>
											<div class="flex gap-2">
												<button
													onclick={() => handleReject(gate)}
													disabled={!rejectReason.trim()}
													class="px-3 py-1.5 rounded text-xs transition-colors
														{rejectReason.trim()
														? 'bg-health-err/20 text-health-err hover:bg-health-err/30'
														: 'bg-surface-700 text-surface-500 cursor-not-allowed'}"
												>
													Confirm Reject
												</button>
												<button
													onclick={() => { rejectingGateId = null; }}
													class="px-3 py-1.5 rounded text-xs text-surface-400 hover:text-surface-100"
												>
													Cancel
												</button>
											</div>
										</div>
									{:else}
										<div class="flex gap-2">
											<button
												onclick={() => handlePass(gate)}
												class="px-4 py-1.5 rounded text-xs font-medium
													bg-health-ok/20 text-health-ok hover:bg-health-ok/30 transition-colors"
											>
												{'\u{2713}'} Pass Gate
											</button>
											<button
												onclick={() => { rejectingGateId = gate.session_id; }}
												class="px-4 py-1.5 rounded text-xs font-medium
													bg-health-err/20 text-health-err hover:bg-health-err/30 transition-colors"
											>
												{'\u{2715}'} Reject
											</button>
										</div>
									{/if}
								</div>
							{/if}
						</div>
					{/each}
				</div>
			{/if}
		</div>

		<!-- Decided Section -->
		{#if decidedGates.length > 0}
			<div>
				<h3 class="text-[10px] text-surface-400 uppercase tracking-wider mb-3">
					Decided (recent)
				</h3>

				<div class="space-y-2">
					{#each decidedGates as gate (gate.session_id)}
						{@const isPassed = gate.status === 'gate_passed'}
						<div class="rounded-lg border border-surface-600/50 bg-surface-800/30 px-4 py-3">
							<div class="flex items-center gap-3">
								<span class="text-[10px] {isPassed ? 'text-health-ok' : 'text-health-err'}">
									{isPassed ? '\u{2713}' : '\u{2715}'}
								</span>
								<span class="text-xs font-medium {isPassed ? 'text-health-ok/80' : 'text-health-err/80'}">
									{gateTypeLabel(gate.role)}
								</span>
								{#if gate.division_id}
									<span class="text-[10px] text-surface-500">{'\u{00B7}'}</span>
									<span class="text-[10px] text-surface-400">{gate.division_id}</span>
								{/if}
								<span class="text-[10px] text-surface-500">{'\u{00B7}'}</span>
								<span class="text-[10px] text-surface-500">
									{ROLE_META[gate.role]?.label ?? gate.role}
								</span>
								{#if !isPassed && $activeVenture}
									<RetryBadge ventureId={$activeVenture.venture_id} sessionId={gate.session_id} />
								{/if}
								<span class="text-[10px] text-surface-500 ml-auto">
									{timeAgo(gate.completed_at)}
								</span>
							</div>
							{#if gate.error}
								<div class="text-[10px] text-surface-400 mt-1 ml-5 truncate">
									{isPassed ? 'Passed with feedback' : 'Rejected'}: "{gate.error}"
								</div>
							{/if}
						</div>
					{/each}
				</div>
			</div>
		{/if}
	</div>
</div>
