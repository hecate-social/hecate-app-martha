<script lang="ts">
	import { selectedDivision } from '$lib/guide_venture/guide_venture.js';
	import {
		kanbanItems,
		kanbanBoard,
		kanbanError,
		kanbanLoading,
		readyItems,
		inProgressItems,
		doneItems,
		itemCounts,
		fetchKanban,
		submitItem,
		pickItem,
		completeItem,
		returnItem
	} from '$lib/kanban_division/kanban_division.js';
	import type { KanbanItem } from '$lib/kanban_division/kanban_division.js';
	import { openAIAssist } from '$lib/shared/aiStore.js';

	// Fetch kanban when division changes
	let lastDivId = $state<string | null>(null);
	$effect(() => {
		const div = $selectedDivision;
		if (div && div.division_id !== lastDivId) {
			lastDivId = div.division_id;
			fetchKanban(div.division_id);
		}
	});

	// --- Submit form state ---
	let showSubmitForm = $state(false);
	let itemTitle = $state('');
	let itemDesc = $state('');
	let itemType = $state<'cmd_desk' | 'prj_desk' | 'qry_desk'>('cmd_desk');

	// --- Return modal state ---
	let returningItem = $state<KanbanItem | null>(null);
	let returnReason = $state('');

	async function handleSubmit() {
		if (!$selectedDivision || !itemTitle.trim()) return;
		const ok = await submitItem($selectedDivision.division_id, {
			title: itemTitle.trim(),
			description: itemDesc.trim() || undefined,
			item_type: itemType,
			submitted_by: 'hecate-web'
		});
		if (ok) {
			itemTitle = '';
			itemDesc = '';
			showSubmitForm = false;
		}
	}

	async function handlePick(item: KanbanItem) {
		if (!$selectedDivision) return;
		await pickItem($selectedDivision.division_id, item.item_id);
	}

	async function handleComplete(item: KanbanItem) {
		if (!$selectedDivision) return;
		await completeItem($selectedDivision.division_id, item.item_id);
	}

	function startReturn(item: KanbanItem) {
		returningItem = item;
		returnReason = '';
	}

	async function handleReturn() {
		if (!$selectedDivision || !returningItem || !returnReason.trim()) return;
		const ok = await returnItem(
			$selectedDivision.division_id,
			returningItem.item_id,
			returnReason.trim()
		);
		if (ok) {
			returningItem = null;
			returnReason = '';
		}
	}

	function itemTypeLabel(type: string): string {
		switch (type) {
			case 'cmd_desk': return 'CMD';
			case 'prj_desk': return 'PRJ';
			case 'qry_desk': return 'QRY';
			default: return type;
		}
	}

	function itemTypeColor(type: string): string {
		switch (type) {
			case 'cmd_desk': return 'bg-es-command/20 text-es-command';
			case 'prj_desk': return 'bg-phase-crafting/20 text-phase-crafting';
			case 'qry_desk': return 'bg-hecate-600/20 text-hecate-400';
			default: return 'bg-surface-600 text-surface-300';
		}
	}

	function formatTime(ts: number | null): string {
		if (!ts) return '';
		return new Date(ts).toLocaleDateString(undefined, {
			month: 'short', day: 'numeric', hour: '2-digit', minute: '2-digit'
		});
	}
</script>

<div class="p-4 space-y-4">
	<!-- Header -->
	<div class="flex items-center justify-between">
		<div>
			<h3 class="text-sm font-semibold text-surface-100">Kanban</h3>
			<p class="text-[11px] text-surface-400 mt-0.5">
				Work items for
				<span class="text-surface-200">{$selectedDivision?.context_name}</span>
			</p>
		</div>

		<div class="flex items-center gap-2">
			<!-- Item counts -->
			{#if $kanbanItems.length > 0}
				<div class="flex items-center gap-3 text-[10px] text-surface-400 mr-2">
					<span>{$itemCounts.ready} ready</span>
					<span>{$itemCounts.in_progress} active</span>
					<span>{$itemCounts.done} done</span>
				</div>
			{/if}

			<button
				onclick={() => (showSubmitForm = !showSubmitForm)}
				class="text-[11px] px-3 py-1 rounded bg-hecate-600/20 text-hecate-300
					hover:bg-hecate-600/30 transition-colors"
			>
				+ Submit Item
			</button>
		</div>
	</div>

	<!-- Error banner -->
	{#if $kanbanError}
		<div class="text-[11px] text-health-err bg-health-err/10 rounded px-3 py-2">
			{$kanbanError}
		</div>
	{/if}

	<!-- Submit form -->
	{#if showSubmitForm}
		<div class="rounded-lg border border-hecate-600/30 bg-surface-800/80 p-4 space-y-3">
			<h4 class="text-xs font-medium text-hecate-300 uppercase tracking-wider">
				New Work Item
			</h4>
			<div class="grid grid-cols-[1fr_auto] gap-3">
				<div>
					<label for="item-title" class="text-[10px] text-surface-400 block mb-1">Title (desk name)</label>
					<input
						id="item-title"
						bind:value={itemTitle}
						placeholder="e.g., register_user"
						class="w-full bg-surface-700 border border-surface-600 rounded px-2.5 py-1.5
							text-xs text-surface-100 placeholder-surface-400
							focus:outline-none focus:border-hecate-500/50"
					/>
				</div>
				<div>
					<label for="item-type" class="text-[10px] text-surface-400 block mb-1">Department</label>
					<select
						id="item-type"
						bind:value={itemType}
						class="bg-surface-700 border border-surface-600 rounded px-2 py-1.5
							text-xs text-surface-100
							focus:outline-none focus:border-hecate-500/50 cursor-pointer"
					>
						<option value="cmd_desk">CMD</option>
						<option value="prj_desk">PRJ</option>
						<option value="qry_desk">QRY</option>
					</select>
				</div>
			</div>
			<div>
				<label for="item-desc" class="text-[10px] text-surface-400 block mb-1">Description (optional)</label>
				<input
					id="item-desc"
					bind:value={itemDesc}
					placeholder="Brief description of this desk"
					class="w-full bg-surface-700 border border-surface-600 rounded px-2.5 py-1.5
						text-xs text-surface-100 placeholder-surface-400
						focus:outline-none focus:border-hecate-500/50"
				/>
			</div>
			<div class="flex gap-2">
				<button
					onclick={handleSubmit}
					disabled={!itemTitle.trim()}
					class="px-3 py-1.5 rounded text-xs transition-colors
						{itemTitle.trim()
						? 'bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30'
						: 'bg-surface-700 text-surface-500 cursor-not-allowed'}"
				>
					Submit
				</button>
				<button
					onclick={() => (showSubmitForm = false)}
					class="px-3 py-1.5 rounded text-xs text-surface-400 hover:text-surface-100"
				>
					Cancel
				</button>
			</div>
		</div>
	{/if}

	<!-- Loading -->
	{#if $kanbanLoading}
		<div class="text-center py-8 text-surface-400 text-xs animate-pulse">
			Loading kanban board...
		</div>
	{:else if $kanbanItems.length === 0 && !showSubmitForm}
		<!-- Empty state -->
		<div class="text-center py-12 text-surface-500 text-xs border border-dashed border-surface-600 rounded-lg">
			<div class="text-2xl mb-3 text-surface-400">{'\u{2610}'}</div>
			<p class="mb-1">No work items yet.</p>
			<p class="text-[10px] text-surface-500">
				Submit items from storming output, or add them manually above.
			</p>
		</div>
	{:else}
		<!-- Three-column kanban board -->
		<div class="grid grid-cols-3 gap-3 min-h-[300px]">
			<!-- READY column -->
			<div class="rounded-lg border border-surface-600 bg-surface-800/30">
				<div class="px-3 py-2 border-b border-surface-600 flex items-center gap-2">
					<span class="w-2 h-2 rounded-full bg-hecate-400"></span>
					<span class="text-[11px] font-semibold text-surface-200">Ready</span>
					<span class="text-[10px] text-surface-500 ml-auto">{$readyItems.length}</span>
				</div>
				<div class="p-2 space-y-2">
					{#each $readyItems as item (item.item_id)}
						<div class="rounded border border-surface-600 bg-surface-800/60 p-2.5 group">
							<div class="flex items-start gap-2 mb-1.5">
								<span class="text-xs font-medium text-surface-100 flex-1 leading-tight">
									{item.title}
								</span>
								<span class="text-[9px] px-1.5 py-0.5 rounded {itemTypeColor(item.item_type)} shrink-0">
									{itemTypeLabel(item.item_type)}
								</span>
							</div>
							{#if item.description}
								<p class="text-[10px] text-surface-400 mb-2 leading-relaxed">{item.description}</p>
							{/if}
							{#if item.return_reason}
								<div class="text-[9px] text-health-warn bg-health-warn/10 rounded px-2 py-1 mb-2">
									Returned: {item.return_reason}
								</div>
							{/if}
							<div class="flex items-center justify-between">
								<span class="text-[9px] text-surface-500">{formatTime(item.submitted_at)}</span>
								<button
									onclick={() => handlePick(item)}
									class="text-[10px] px-2 py-0.5 rounded bg-hecate-600/15 text-hecate-300
										hover:bg-hecate-600/25 transition-colors
										opacity-0 group-hover:opacity-100"
								>
									Pick up
								</button>
							</div>
						</div>
					{/each}
				</div>
			</div>

			<!-- IN PROGRESS column -->
			<div class="rounded-lg border border-surface-600 bg-surface-800/30">
				<div class="px-3 py-2 border-b border-surface-600 flex items-center gap-2">
					<span class="w-2 h-2 rounded-full bg-phase-crafting"></span>
					<span class="text-[11px] font-semibold text-surface-200">In Progress</span>
					<span class="text-[10px] text-surface-500 ml-auto">{$inProgressItems.length}</span>
				</div>
				<div class="p-2 space-y-2">
					{#each $inProgressItems as item (item.item_id)}
						<div class="rounded border border-surface-600 bg-surface-800/60 p-2.5 group">
							<div class="flex items-start gap-2 mb-1.5">
								<span class="text-xs font-medium text-surface-100 flex-1 leading-tight">
									{item.title}
								</span>
								<span class="text-[9px] px-1.5 py-0.5 rounded {itemTypeColor(item.item_type)} shrink-0">
									{itemTypeLabel(item.item_type)}
								</span>
							</div>
							{#if item.description}
								<p class="text-[10px] text-surface-400 mb-2 leading-relaxed">{item.description}</p>
							{/if}
							{#if item.picked_by}
								<div class="text-[9px] text-surface-400 mb-2">
									Picked by {item.picked_by}
								</div>
							{/if}
							<div class="flex items-center gap-1 justify-end
								opacity-0 group-hover:opacity-100 transition-opacity">
								<button
									onclick={() => startReturn(item)}
									class="text-[10px] px-2 py-0.5 rounded text-health-warn
										hover:bg-health-warn/10 transition-colors"
								>
									Return
								</button>
								<button
									onclick={() => handleComplete(item)}
									class="text-[10px] px-2 py-0.5 rounded bg-health-ok/15 text-health-ok
										hover:bg-health-ok/25 transition-colors"
								>
									Complete
								</button>
							</div>
						</div>
					{/each}
				</div>
			</div>

			<!-- DONE column -->
			<div class="rounded-lg border border-surface-600 bg-surface-800/30">
				<div class="px-3 py-2 border-b border-surface-600 flex items-center gap-2">
					<span class="w-2 h-2 rounded-full bg-health-ok"></span>
					<span class="text-[11px] font-semibold text-surface-200">Done</span>
					<span class="text-[10px] text-surface-500 ml-auto">{$doneItems.length}</span>
				</div>
				<div class="p-2 space-y-2">
					{#each $doneItems as item (item.item_id)}
						<div class="rounded border border-surface-600/50 bg-surface-800/30 p-2.5 opacity-70">
							<div class="flex items-start gap-2 mb-1">
								<span class="text-[10px] text-health-ok">{'\u{2713}'}</span>
								<span class="text-xs text-surface-300 flex-1 leading-tight">
									{item.title}
								</span>
								<span class="text-[9px] px-1.5 py-0.5 rounded {itemTypeColor(item.item_type)} shrink-0">
									{itemTypeLabel(item.item_type)}
								</span>
							</div>
							<div class="text-[9px] text-surface-500 ml-4">
								{formatTime(item.completed_at)}
							</div>
						</div>
					{/each}
				</div>
			</div>
		</div>
	{/if}

	<!-- Return modal -->
	{#if returningItem}
		<div class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center"
			role="dialog" aria-modal="true">
			<div class="bg-surface-800 border border-surface-600 rounded-xl p-5 w-96 space-y-3">
				<h4 class="text-sm font-semibold text-surface-100">Return Item</h4>
				<p class="text-[11px] text-surface-400">
					Returning <span class="text-surface-200 font-medium">{returningItem.title}</span> to the Ready column.
				</p>
				<div>
					<label for="return-reason" class="text-[10px] text-surface-400 block mb-1">Reason</label>
					<textarea
						id="return-reason"
						bind:value={returnReason}
						placeholder="Why is this item being returned?"
						rows="3"
						class="w-full bg-surface-700 border border-surface-600 rounded px-2.5 py-1.5
							text-xs text-surface-100 placeholder-surface-400
							focus:outline-none focus:border-health-warn/50 resize-none"
					></textarea>
				</div>
				<div class="flex gap-2 justify-end">
					<button
						onclick={() => (returningItem = null)}
						class="px-3 py-1.5 rounded text-xs text-surface-400 hover:text-surface-100"
					>
						Cancel
					</button>
					<button
						onclick={handleReturn}
						disabled={!returnReason.trim()}
						class="px-3 py-1.5 rounded text-xs transition-colors
							{returnReason.trim()
							? 'bg-health-warn/20 text-health-warn hover:bg-health-warn/30'
							: 'bg-surface-700 text-surface-500 cursor-not-allowed'}"
					>
						Return Item
					</button>
				</div>
			</div>
		</div>
	{/if}
</div>
