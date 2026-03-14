<script lang="ts">
	import { getApi } from '$lib/martha/shared/api.js';
	import { activeVenture } from '$lib/martha/guide_venture/guide_venture.js';

	interface CostBudget {
		venture_id: string;
		budget_usd: number;
		spent_usd: number;
		warning_pct: number;
		status: string;
		pct_used?: number;
		model_breakdown?: Record<string, number>;
		initiated_at?: number;
	}

	let budget: CostBudget | null = $state(null);
	let showSetForm = $state(false);
	let newBudgetAmount = $state('10.00');
	let loading = $state(false);
	let pollTimer: ReturnType<typeof setInterval> | undefined = $state(undefined);

	let pctUsed = $derived.by(() => {
		const b = budget;
		return b ? (b.spent_usd / b.budget_usd) * 100 : 0;
	});
	let statusColor = $derived.by(() => {
		if (!budget) return 'surface';
		return pctUsed > 80 ? 'err' : pctUsed > 60 ? 'warn' : 'ok';
	});
	let barColor = $derived(
		statusColor === 'err'
			? 'bg-health-err'
			: statusColor === 'warn'
				? 'bg-amber-400'
				: 'bg-health-ok'
	);
	let breached = $derived.by(() => {
		const b = budget;
		return b ? b.status === 'breached' : false;
	});

	async function fetchBudget() {
		if (!$activeVenture) return;
		try {
			const api = getApi();
			const resp = await api.get<{ ok: boolean; budget: CostBudget }>(
				`/cost-budgets/${$activeVenture.venture_id}`
			);
			if (resp?.ok && resp.budget) {
				budget = resp.budget;
			} else {
				budget = null;
			}
		} catch {
			budget = null;
		}
	}

	async function handleSetBudget() {
		if (!$activeVenture) return;
		const amount = parseFloat(newBudgetAmount);
		if (isNaN(amount) || amount <= 0) return;
		loading = true;
		try {
			const api = getApi();
			await api.post(`/cost-budgets/${$activeVenture.venture_id}/set`, {
				budget_usd: amount,
				warning_pct: 0.8
			});
			await fetchBudget();
			showSetForm = false;
		} catch {
			// silently fail — user can retry
		} finally {
			loading = false;
		}
	}

	function formatUsd(n: number): string {
		return `$${n.toFixed(2)}`;
	}

	$effect(() => {
		if ($activeVenture) {
			fetchBudget();
			if (pollTimer) clearInterval(pollTimer);
			pollTimer = setInterval(fetchBudget, 15_000);
		} else {
			budget = null;
			if (pollTimer) clearInterval(pollTimer);
			pollTimer = undefined;
		}
		return () => {
			if (pollTimer) clearInterval(pollTimer);
		};
	});
</script>

{#if budget && $activeVenture}
	<!-- Inline budget indicator — lives in the tab bar -->
	<div class="flex items-center gap-2 relative {breached ? 'animate-pulse' : ''}">
		<span class="text-[10px] font-mono shrink-0
			{statusColor === 'err' ? 'text-health-err' :
			 statusColor === 'warn' ? 'text-amber-400' :
			 'text-surface-400'}">
			{formatUsd(budget.spent_usd)}/{formatUsd(budget.budget_usd)}
		</span>

		<div class="bg-surface-700 rounded-full h-1 w-16">
			<div
				class="h-full rounded-full transition-all duration-500 {barColor}"
				style="width: {Math.min(pctUsed, 100)}%"
			></div>
		</div>

		<button
			onclick={() => (showSetForm = !showSetForm)}
			class="text-[9px] text-surface-500 hover:text-hecate-400 transition-colors"
			title="Adjust budget"
		>
			{'\u{270E}'}
		</button>

		<!-- Set budget popover -->
		{#if showSetForm}
			<div class="absolute top-full right-0 mt-2 z-20 bg-surface-700 border border-surface-600
				rounded-lg shadow-lg p-3 flex items-center gap-2 min-w-[240px]">
				<label for="budget-amount" class="text-[10px] text-surface-400 shrink-0">USD</label>
				<input
					id="budget-amount"
					type="number"
					step="0.01"
					min="0.01"
					bind:value={newBudgetAmount}
					class="w-20 bg-surface-800 border border-surface-600 rounded px-2 py-1 text-xs
						font-mono text-surface-100 focus:outline-none focus:border-hecate-500"
				/>
				<button
					onclick={handleSetBudget}
					disabled={loading}
					class="px-2.5 py-1 rounded text-[11px] bg-hecate-600/20 text-hecate-300
						hover:bg-hecate-600/30 transition-colors disabled:opacity-50"
				>
					{loading ? '...' : 'Set'}
				</button>
				<button
					onclick={() => (showSetForm = false)}
					class="text-[11px] text-surface-500 hover:text-surface-200"
				>
					{'\u{2715}'}
				</button>
			</div>
		{/if}
	</div>
{/if}
