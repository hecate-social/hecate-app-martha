<script lang="ts">
	import { invoke } from '@tauri-apps/api/core';
	import type { StudioContext } from '$lib/martha/shared/context.js';
	import { selectedAudioDeviceId, setAudioDevice } from '$lib/martha/shared/aiStore.js';

	let {
		onTranscript,
		ctx
	}: { onTranscript: (text: string) => void; ctx: StudioContext } = $props();

	let recording = $state(false);
	let processing = $state(false);
	let showDevicePicker = $state(false);
	let audioDevices = $state<{ id: string; name: string }[]>([]);
	let audioLevel = $state(0);
	let levelPollTimer: ReturnType<typeof setInterval> | null = null;

	async function loadDevices() {
		try {
			audioDevices = await invoke('list_audio_devices');
		} catch (e) {
			console.error('Failed to list audio devices:', e);
			audioDevices = [];
		}
	}

	function toggleDevicePicker() {
		if (!showDevicePicker) {
			loadDevices();
		}
		showDevicePicker = !showDevicePicker;
	}

	async function selectDevice(deviceId: string | null) {
		setAudioDevice(deviceId);
		try {
			await invoke('select_audio_device', { deviceId: deviceId ?? null });
		} catch (e) {
			console.error('Failed to select audio device:', e);
		}
		showDevicePicker = false;
	}

	function selectedDeviceLabel(): string {
		const id = $selectedAudioDeviceId;
		if (!id) return 'Default';
		const dev = audioDevices.find((d) => d.id === id);
		return dev?.name || 'Selected device';
	}

	function startLevelPolling() {
		stopLevelPolling();
		levelPollTimer = setInterval(async () => {
			try {
				audioLevel = await invoke('get_audio_level');
			} catch {
				audioLevel = 0;
			}
		}, 80);
	}

	function stopLevelPolling() {
		if (levelPollTimer) {
			clearInterval(levelPollTimer);
			levelPollTimer = null;
		}
		audioLevel = 0;
	}

	async function startRecording() {
		if (recording || processing) return;
		try {
			await invoke('start_audio_recording');
			recording = true;
			startLevelPolling();
		} catch (e) {
			console.error('Failed to start recording:', e);
		}
	}

	async function toggleRecording() {
		if (recording) {
			await stopRecording();
			return;
		}
		await startRecording();
	}

	function handleKeyDown(e: KeyboardEvent) {
		if (e.altKey && e.code === 'KeyV' && !e.repeat) {
			e.preventDefault();
			startRecording();
		}
	}

	function handleKeyUp(e: KeyboardEvent) {
		if (e.code === 'KeyV' && recording) {
			e.preventDefault();
			stopRecording();
		}
	}

	async function stopRecording() {
		recording = false;
		stopLevelPolling();
		processing = true;

		try {
			const base64Wav: string = await invoke('stop_audio_recording');
			// Convert base64 to Blob for the STT API
			const binaryStr = atob(base64Wav);
			const bytes = new Uint8Array(binaryStr.length);
			for (let i = 0; i < binaryStr.length; i++) {
				bytes[i] = binaryStr.charCodeAt(i);
			}
			const blob = new Blob([bytes], { type: 'audio/wav' });
			const text = await ctx.transcribeAudio(blob);
			if (text) onTranscript(text);
		} catch (e) {
			console.error('Transcription failed:', e);
		}

		processing = false;
	}
</script>

<svelte:window onkeydown={handleKeyDown} onkeyup={handleKeyUp} />

<div class="relative self-end flex items-center gap-1">
	<!-- Level meter (visible during recording) -->
	{#if recording}
		<div class="flex items-center gap-px h-5">
			{#each [0.08, 0.2, 0.35, 0.5, 0.7] as threshold}
				<div
					class="w-[3px] rounded-full transition-all duration-75"
					style="height: {audioLevel > threshold ? 6 + (audioLevel - threshold) * 14 : 3}px;
						background-color: {audioLevel > threshold
							? audioLevel > 0.7 ? 'rgb(239, 68, 68)' : 'rgb(var(--color-hecate-400))'
							: 'rgb(var(--color-surface-500))'};
						opacity: {audioLevel > threshold ? 0.6 + audioLevel * 0.4 : 0.3};"
				></div>
			{/each}
		</div>
	{/if}

	<!-- Mic button -->
	<button
		onclick={toggleRecording}
		oncontextmenu={(e) => {
			e.preventDefault();
			toggleDevicePicker();
		}}
		disabled={processing}
		class="px-2 rounded text-[11px] transition-colors
			{processing
			? 'bg-surface-600 text-surface-400 cursor-wait'
			: recording
				? 'bg-health-err/80 text-surface-50 hover:bg-health-err'
				: 'bg-surface-600 text-surface-300 hover:bg-surface-500 hover:text-surface-100'}"
		title={recording
			? 'Stop recording'
			: processing
				? 'Transcribing...'
				: `Voice input (${selectedDeviceLabel()}) \u2014 Alt+V hold to talk \u2014 right-click to change device`}
	>
		{#if processing}
			<svg class="w-3.5 h-3.5 animate-spin" viewBox="0 0 24 24" fill="none">
				<circle
					cx="12"
					cy="12"
					r="10"
					stroke="currentColor"
					stroke-width="2"
					opacity="0.3"
				/>
				<path
					d="M12 2a10 10 0 0 1 10 10"
					stroke="currentColor"
					stroke-width="2"
					stroke-linecap="round"
				/>
			</svg>
		{:else if recording}
			<div class="w-3.5 h-3.5 flex items-center justify-center">
				<div class="w-2 h-2 rounded-sm bg-current"></div>
			</div>
		{:else}
			<svg class="w-3.5 h-3.5" viewBox="0 0 24 24" fill="currentColor">
				<path
					d="M12 14c1.66 0 3-1.34 3-3V5c0-1.66-1.34-3-3-3S9 3.34 9 5v6c0 1.66 1.34 3 3 3z"
				/>
				<path
					d="M17 11c0 2.76-2.24 5-5 5s-5-2.24-5-5H5c0 3.53 2.61 6.43 6 6.92V21h2v-3.08c3.39-.49 6-3.39 6-6.92h-2z"
				/>
			</svg>
		{/if}
	</button>

	<!-- Device picker dropdown -->
	{#if showDevicePicker}
		<!-- Backdrop to close -->
		<!-- svelte-ignore a11y_no_static_element_interactions -->
		<div class="fixed inset-0 z-40" onclick={() => (showDevicePicker = false)}></div>

		<div
			class="absolute bottom-full right-0 mb-1 z-50 w-56 bg-surface-700 border border-surface-500 rounded-lg shadow-xl overflow-hidden"
		>
			<div
				class="px-3 py-1.5 text-[9px] font-semibold text-surface-400 uppercase border-b border-surface-600"
			>
				Audio Input Device
			</div>
			<div class="max-h-48 overflow-y-auto">
				<!-- Default option -->
				<button
					onclick={() => selectDevice(null)}
					class="w-full text-left px-3 py-1.5 text-[11px] transition-colors
						{$selectedAudioDeviceId === null
						? 'text-hecate-400 bg-hecate-600/10'
						: 'text-surface-200 hover:bg-surface-600'}"
				>
					System Default
				</button>
				{#each audioDevices as device}
					<button
						onclick={() => selectDevice(device.id)}
						class="w-full text-left px-3 py-1.5 text-[11px] transition-colors truncate
							{$selectedAudioDeviceId === device.id
							? 'text-hecate-400 bg-hecate-600/10'
							: 'text-surface-200 hover:bg-surface-600'}"
						title={device.name}
					>
						{device.name}
					</button>
				{/each}
				{#if audioDevices.length === 0}
					<div class="px-3 py-2 text-[10px] text-surface-400 italic">
						No audio devices found
					</div>
				{/if}
			</div>
		</div>
	{/if}
</div>
