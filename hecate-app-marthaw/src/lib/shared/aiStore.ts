import { writable, derived, get } from 'svelte/store';
import type { PhaseCode } from '../types.js';
import type { LLMModel } from '../types.js';

// Re-export PhaseCode for consumers that imported from here
export type { PhaseCode };

export interface PhaseInfo {
	code: PhaseCode;
	name: string;
	shortName: string;
	description: string;
	role: string;
	color: string;
}

// --- State ---
export const showAIAssist = writable(false);
export const aiAssistContext = writable<string>('');
export const aiAssistAgent = writable<string | null>(null);
export const aiModelOverride = writable<string | null>(null);
export const availableModels = writable<LLMModel[]>([]);

// --- Smart Model Selection ---

/** Tier ranking: higher = better for the task. */
type ModelTier = 'flagship' | 'balanced' | 'fast' | 'local';

interface ScoredModel {
	model: LLMModel;
	tier: ModelTier;
	score: number;
}

/** Classify a model into a tier based on its metadata. */
function classifyTier(m: LLMModel): ModelTier {
	const name = m.name.toLowerCase();
	const ctx = m.context_length;

	// Local models (Ollama)
	if (m.format !== 'api') return 'local';

	// Flagship: large context, top-tier names
	if (/claude-opus|claude-sonnet-4|o4-mini|o3|gpt-4o$/.test(name)) return 'flagship';
	if (/gemini-2\.5-pro/.test(name)) return 'flagship';

	// Balanced: good general-purpose
	if (/claude-haiku|claude-sonnet|gpt-4o-mini|gemini-2\.5-flash|gemini-2\.0-flash/.test(name)) return 'balanced';
	if (/llama-3\.3|llama-3\.1/.test(name) && ctx >= 32000) return 'balanced';

	// Fast: small/cheap models
	if (/flash|mini|nano|small|lite|fast/.test(name)) return 'fast';
	if (ctx > 0 && ctx <= 8192) return 'fast';

	return 'balanced';
}

/** Score a model for a given task affinity. Higher = better match. */
function scoreModel(m: LLMModel, affinity: 'code' | 'general' | 'creative'): number {
	const tier = classifyTier(m);
	let score = 0;

	// Base tier score
	switch (tier) {
		case 'flagship': score += 100; break;
		case 'balanced': score += 70; break;
		case 'fast': score += 40; break;
		case 'local': score += 20; break;
	}

	// Context window bonus (prefer larger context for creative/general)
	if (m.context_length >= 100000) score += 15;
	else if (m.context_length >= 32000) score += 10;
	else if (m.context_length >= 8000) score += 5;

	// Code affinity bonus
	const name = m.name.toLowerCase();
	if (affinity === 'code') {
		if (/code|coder|codestral|starcoder|deepseek-coder|wizard-?coder/.test(name)) score += 25;
		if (/claude|gpt-4o/.test(name)) score += 10; // good at code too
	}

	// Creative affinity bonus (vision, storming)
	if (affinity === 'creative') {
		if (/claude-opus|claude-sonnet-4|o3|gpt-4o$/.test(name)) score += 20;
		if (/gemini-2\.5-pro/.test(name)) score += 15;
	}

	// Provider reliability bonus (API > local for consistency)
	if (m.format === 'api') score += 5;

	return score;
}

/** Get the best model for a given task. */
export function recommendModel(
	models: LLMModel[],
	affinity: 'code' | 'general' | 'creative' = 'general'
): LLMModel | null {
	if (models.length === 0) return null;
	const scored = models
		.map((m) => ({ model: m, tier: classifyTier(m), score: scoreModel(m, affinity) }))
		.sort((a, b) => b.score - a.score);
	return scored[0]?.model ?? null;
}

/** Get top N recommended models for a task, grouped by tier. */
export function topRecommendations(
	models: LLMModel[],
	affinity: 'code' | 'general' | 'creative' = 'general',
	limit = 5
): ScoredModel[] {
	return models
		.map((m) => ({ model: m, tier: classifyTier(m), score: scoreModel(m, affinity) }))
		.sort((a, b) => b.score - a.score)
		.slice(0, limit);
}

// --- Derived ---
export const aiModel = derived(
	[aiModelOverride, availableModels],
	([$override, $models]) => {
		if ($override) return $override;
		// Smart default: pick best balanced model
		const best = recommendModel($models, 'general');
		return best?.name ?? null;
	}
);

// --- Phase Model Preferences ---
const PHASE_MODEL_PREFS_KEY = 'hecate-app-martha-phase-models';

type PhaseModelPrefs = Record<PhaseCode, string | null>;

function loadPhaseModelPrefs(): PhaseModelPrefs {
	try {
		const raw = localStorage.getItem(PHASE_MODEL_PREFS_KEY);
		if (raw) {
			const parsed = JSON.parse(raw);
			return { storming: null, planning: null, kanban: null, crafting: null, ...parsed };
		}
	} catch {
		// ignore
	}
	return { storming: null, planning: null, kanban: null, crafting: null };
}

function savePhaseModelPrefs(prefs: PhaseModelPrefs): void {
	try {
		localStorage.setItem(PHASE_MODEL_PREFS_KEY, JSON.stringify(prefs));
	} catch {
		// ignore
	}
}

export const phaseModelPrefs = writable<PhaseModelPrefs>(loadPhaseModelPrefs());

// --- Model Metadata Helpers ---
const CODE_PATTERNS = [
	/code/i, /coder/i, /codestral/i, /starcoder/i,
	/codellama/i, /wizard-?coder/i, /deepseek-coder/i
];

export function modelAffinity(name: string): 'code' | 'general' {
	if (CODE_PATTERNS.some((p) => p.test(name))) return 'code';
	return 'general';
}

export function phaseAffinity(phase: PhaseCode): 'code' | 'general' | 'creative' {
	if (phase === 'crafting') return 'code';
	if (phase === 'storming') return 'creative';
	return 'general';
}

/** Format context length for display (e.g., "200k", "8k") */
export function formatContextLength(ctx: number): string {
	if (ctx <= 0) return '';
	if (ctx >= 1000000) return `${Math.round(ctx / 1000000)}M`;
	if (ctx >= 1000) return `${Math.round(ctx / 1000)}k`;
	return `${ctx}`;
}

/** Get a model by name from the store. */
export function getModelByName(name: string): LLMModel | undefined {
	return get(availableModels).find((m) => m.name === name);
}

// --- Actions ---
export function openAIAssist(context: string, agentId?: string): void {
	aiAssistAgent.set(agentId ?? null);
	aiAssistContext.set(context);
	showAIAssist.set(true);
}

export function closeAIAssist(): void {
	showAIAssist.set(false);
	aiAssistAgent.set(null);
}

export function setAIModelOverride(model: string): void {
	aiModelOverride.set(model);
}

export function setPhaseModel(phase: PhaseCode, model: string | null): void {
	phaseModelPrefs.update((prefs) => {
		const updated = { ...prefs, [phase]: model };
		savePhaseModelPrefs(updated);
		return updated;
	});
}

export function parseAgentEvents(text: string): string[] {
	return text
		.split('\n')
		.map((line) => line.replace(/^[\s\-*\u2022\d.]+/, '').trim())
		.filter((line) => line.length > 0 && line.length < 80 && !line.includes(':'))
		.map((line) => line.replace(/["`]/g, ''));
}

// --- Phase Metadata ---
export const PHASES: PhaseInfo[] = [
	{
		code: 'storming',
		name: 'Storming',
		shortName: 'Storming',
		description: 'Design aggregates, events, desks, and dependencies',
		role: 'storming',
		color: 'phase-storming'
	},
	{
		code: 'planning',
		name: 'Planning',
		shortName: 'Planning',
		description: 'Lifecycle management for the division',
		role: 'planning',
		color: 'phase-planning'
	},
	{
		code: 'kanban',
		name: 'Kanban',
		shortName: 'Kanban',
		description: 'Work items board for desk crafting',
		role: 'kanban',
		color: 'phase-kanban'
	},
	{
		code: 'crafting',
		name: 'Crafting',
		shortName: 'Crafting',
		description: 'Generate code, run tests, deliver releases',
		role: 'crafting',
		color: 'phase-crafting'
	}
];
