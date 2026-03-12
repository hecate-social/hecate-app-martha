/** Plugin API interface provided by hecate-web host */
export interface PluginApi {
	get: <T>(path: string) => Promise<T>;
	post: <T>(path: string, body: unknown) => Promise<T>;
	del: <T>(path: string) => Promise<T>;
}

/** Health check response from marthad */
export interface HealthData {
	ok: boolean;
	app: string;
	version: string;
	node: string;
}

/** Venture lifecycle types */
export interface Venture {
	venture_id: string;
	name: string;
	vision: string;
	brief?: string;
	status: number;
	phase: string;
	status_label?: string;
	initiated_at?: number;
	repos?: string[];
	created_at: string;
	updated_at: string;
}

export interface Division {
	division_id: string;
	venture_id: string;
	context_name: string;
	name: string;
	description: string;
	status: number;
	phase: string;
	storming_status: number;
	storming_status_label: string;
	planning_status: number;
	planning_status_label: string;
	kanban_status: number;
	kanban_status_label: string;
	crafting_status: number;
	crafting_status_label: string;
	storming_available_actions: string[];
	planning_available_actions: string[];
	kanban_available_actions: string[];
	crafting_available_actions: string[];
	created_at: string;
	updated_at: string;
}

/** Storm session types */
export interface StormSession {
	id: string;
	venture_id: string;
	phase: string;
	status: string;
	started_at: string;
}

export interface EventSticky {
	id: string;
	storm_id: string;
	text: string;
	author: string;
	cluster_id: string | null;
	stack_id: string | null;
	position: number;
}

export interface EventCluster {
	id: string;
	storm_id: string;
	name: string;
	status: string;
}

export interface FactArrow {
	id: string;
	storm_id: string;
	from_cluster_id: string;
	to_cluster_id: string;
}

/** Division ALC types */
export interface DesignedAggregate {
	id: string;
	division_id: string;
	name: string;
	description: string;
}

export interface DesignedEvent {
	id: string;
	division_id: string;
	aggregate_id: string;
	name: string;
	description: string;
}

export interface PlannedDesk {
	id: string;
	division_id: string;
	name: string;
	description: string;
}

export interface PlannedDependency {
	id: string;
	division_id: string;
	name: string;
	source: string;
	target: string;
}

/** API response wrapper */
export interface ApiResponse<T> {
	ok: boolean;
	data?: T;
	error?: string;
}

export interface PaginatedResponse<T> {
	ok: boolean;
	data: T[];
	total: number;
	page: number;
	per_page: number;
}

/** Martha error codes */
export type MarthaErrorCode =
	| 'venture_not_found'
	| 'venture_already_active'
	| 'venture_archived'
	| 'division_not_found'
	| 'division_archived'
	| 'invalid_phase_transition'
	| 'phase_already_active'
	| 'phase_not_active'
	| 'storm_not_found'
	| 'storm_not_active'
	| 'storm_already_active'
	| 'sticky_not_found'
	| 'cluster_not_found'
	| 'cluster_dissolved'
	| 'invalid_vision'
	| 'vision_not_submitted'
	| 'duplicate_name'
	| 'missing_required_field'
	| 'invalid_aggregate_design'
	| 'invalid_event_design'
	| 'generation_failed'
	| 'test_execution_failed'
	| 'deployment_failed'
	| 'health_check_failed';

/** Venture lifecycle status bit flags (must match venture_lifecycle_status.hrl) */
export const VL_INITIATED = 1;
export const VL_VISION_REFINED = 2;
export const VL_SUBMITTED = 4;
export const VL_DISCOVERING = 8;
export const VL_DISCOVERY_PAUSED = 16;
export const VL_DISCOVERY_COMPLETED = 32;
export const VL_ARCHIVED = 64;
export const VL_STORMING = 128;
export const VL_STORM_SHELVED = 256;
export const VL_PREPARING = 512;
export const VL_PREPARATION_DONE = 1024;

export type PhaseCode = 'storming' | 'planning' | 'kanban' | 'crafting';

/** Bitwise flag check — used for venture-level status grouping. */
export function hasFlag(status: number, flag: number): boolean {
	return (status & flag) !== 0;
}

/** Get the pre-computed status label for a phase (from backend projection). */
export function phaseStatusLabel(division: Division, phase: PhaseCode): string {
	switch (phase) {
		case 'storming':
			return division.storming_status_label ?? '';
		case 'planning':
			return division.planning_status_label ?? '';
		case 'kanban':
			return division.kanban_status_label ?? '';
		case 'crafting':
			return division.crafting_status_label ?? '';
	}
}

/** Get the available lifecycle actions for a phase (from backend projection). */
export function phaseAvailableActions(division: Division, phase: PhaseCode): string[] {
	switch (phase) {
		case 'storming':
			return division.storming_available_actions ?? [];
		case 'planning':
			return division.planning_available_actions ?? [];
		case 'kanban':
			return division.kanban_available_actions ?? [];
		case 'crafting':
			return division.crafting_available_actions ?? [];
	}
}

/** Design-level desk card types */
export type ExecutionMode = 'human' | 'agent' | 'both' | 'pair';

export interface DeskCardPolicy {
	id: string;
	text: string;
}

export interface DeskCardEvent {
	id: string;
	text: string;
}

export interface DeskCard {
	id: string;
	name: string;
	aggregate?: string;
	execution: ExecutionMode;
	policies: DeskCardPolicy[];
	events: DeskCardEvent[];
}

/** LLM chat types */
export interface ChatMessage {
	role: 'system' | 'user' | 'assistant';
	content: string;
}

export interface StreamChunk {
	content?: string;
	done?: boolean;
}

export function humanizeError(code: MarthaErrorCode): string {
	const messages: Record<MarthaErrorCode, string> = {
		venture_not_found: 'Venture not found',
		venture_already_active: 'A venture is already active',
		venture_archived: 'This venture has been archived',
		division_not_found: 'Division not found',
		division_archived: 'This division has been archived',
		invalid_phase_transition: 'Invalid phase transition',
		phase_already_active: 'This phase is already active',
		phase_not_active: 'This phase is not active',
		storm_not_found: 'Storm session not found',
		storm_not_active: 'Storm session is not active',
		storm_already_active: 'A storm session is already active',
		sticky_not_found: 'Sticky note not found',
		cluster_not_found: 'Event cluster not found',
		cluster_dissolved: 'This cluster has been dissolved',
		invalid_vision: 'Vision statement is invalid',
		vision_not_submitted: 'Vision has not been submitted yet',
		duplicate_name: 'A record with this name already exists',
		missing_required_field: 'A required field is missing',
		invalid_aggregate_design: 'Aggregate design is invalid',
		invalid_event_design: 'Event design is invalid',
		generation_failed: 'Code generation failed',
		test_execution_failed: 'Test execution failed',
		deployment_failed: 'Deployment failed',
		health_check_failed: 'Health check failed'
	};
	return messages[code] ?? code;
}
