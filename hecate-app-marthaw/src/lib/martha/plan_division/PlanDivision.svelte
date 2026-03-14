<script lang="ts">
	import { selectedDivision, isLoading } from '$lib/martha/guide_venture/guide_venture.js';
	import { openAIAssist } from '$lib/martha/shared/aiStore.js';
	import TaskCard from '$lib/martha/shared/TaskCard.svelte';
</script>

<div class="p-4 space-y-6">
	<!-- Header -->
	<div>
		<h3 class="text-sm font-semibold text-surface-100">
			Planning
		</h3>
		<p class="text-[11px] text-surface-400 mt-0.5">
			Lifecycle management for
			<span class="text-surface-200">{$selectedDivision?.context_name}</span>
		</p>
	</div>

	<!-- Status overview -->
	<div class="rounded-lg border border-surface-600 bg-surface-800/50 p-4">
		<h4 class="text-xs font-semibold text-surface-100 mb-3">Division Lifecycle</h4>
		<p class="text-[10px] text-surface-400 leading-relaxed">
			Use the phase controls above to manage this division's planning lifecycle:
			<span class="text-surface-300">Open</span> to begin work,
			<span class="text-surface-300">Shelve</span> to pause,
			<span class="text-surface-300">Resume</span> to continue, or
			<span class="text-surface-300">Conclude</span> when planning is complete.
		</p>
		<p class="text-[10px] text-surface-400 mt-2 leading-relaxed">
			Content work (designing aggregates, events, desks) happens in the
			<span class="text-es-event">Storming</span> phase.
			Implementation items are tracked on the
			<span class="text-hecate-400">Kanban</span> board.
		</p>
	</div>

	<!-- Planning Tasks -->
	<div>
		<h4 class="text-xs font-semibold text-surface-100 mb-3">Planning Tasks</h4>
		<div class="grid grid-cols-1 md:grid-cols-2 gap-3">
			<TaskCard
				title="Desk Inventory"
				description="Create a complete inventory of desks needed for this division, organized by department (CMD, QRY, PRJ)"
				icon={'\u{25A3}'}
				aiContext={`Help me create a desk inventory for the "${$selectedDivision?.context_name}" division. Each desk is a vertical slice (command + event + handler). Organize by CMD (write), QRY (read), and PRJ (projection) departments.`}
			/>
			<TaskCard
				title="Dependency Mapping"
				description="Map dependencies between desks to determine implementation order"
				icon={'\u{21C4}'}
				aiContext={`Help me map dependencies between desks in the "${$selectedDivision?.context_name}" division. Which desks depend on which? What's the optimal implementation order?`}
			/>
			<TaskCard
				title="Sprint Sequencing"
				description="Prioritize and sequence desks into implementation sprints"
				icon={'\u{2630}'}
				aiContext={`Help me sequence the implementation of desks in the "${$selectedDivision?.context_name}" division into logical sprints. Consider dependencies, walking skeleton principles, and the "initiate + archive" first rule.`}
			/>
			<TaskCard
				title="API Design"
				description="Design REST API endpoints for each desk's capabilities"
				icon={'\u{2194}'}
				aiContext={`Help me design REST API endpoints for the "${$selectedDivision?.context_name}" division. Follow the pattern: POST /api/{resource}/{action} for commands, GET /api/{resource} for queries.`}
			/>
		</div>
	</div>
</div>
