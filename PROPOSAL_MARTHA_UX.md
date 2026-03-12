# Martha Studio UX Proposal

*A spectacular interface for AI-assisted software venture development*

---

## The Vision

Martha is unlike anything that exists. It's not an IDE, not a project manager, not a chatbot. It's a **venture studio** — a place where you and AI agents collaboratively shepherd a software idea from napkin sketch to deployed, tested, running code.

The UX should feel like a **mission control room for your venture**. You're the commander. The agents are your crew. The UI should make you feel powerful, informed, and in control — never lost.

---

## Current State

The current UI is functional but flat:
- Linear phase navigation (storming → planning → kanban → crafting)
- Agents hidden behind a toggle button
- No real-time feedback when agents are working
- No visual relationship between divisions
- No sense of the venture as a living, breathing system

---

## Design Principles

| Principle | What It Means |
|-----------|---------------|
| **Mission Control** | You see the whole venture at a glance — what's happening, what's blocked, what's done |
| **Agents Are Visible** | When an agent is working, you see it. Thinking, writing, waiting for you. Not hidden. |
| **Progressive Depth** | Overview first, drill down on demand. Never overwhelm, always accessible. |
| **Spatial Memory** | Things have consistent places. Divisions are always in the same spot. Phases flow left to right. |
| **Live System** | The UI feels alive — status dots pulse, progress bars move, agents announce what they're doing |

---

## Layout: The Three Zones

```
┌──────────────────────────────────────────────────────────────────┐
│ VENTURE BAR                                                        │
│ ◆ my-saas-app  ·  Discovery  ·  3 divisions  ·  2 agents active  │
├──────────┬───────────────────────────────────────────────────────┤
│          │                                                         │
│  NERVE   │                    WORKSPACE                            │
│  CENTER  │                                                         │
│          │  (context-sensitive main area)                           │
│          │                                                         │
│          │                                                         │
│          │                                                         │
│          │                                                         │
│          │                                                         │
├──────────┴───────────────────────────────────────────────────────┤
│ ACTIVITY RAIL                                                      │
│ ◉ Stormer finished design for "billing"  ·  2m ago                │
│ ◉ Reviewer flagged 1 CRITICAL in "auth"  ·  5m ago                │
└──────────────────────────────────────────────────────────────────┘
```

### Zone 1: Venture Bar (top)

Persistent header. Shows:
- Venture name + overall status
- Quick stats: division count, active agents, gate decisions pending
- Model selector (which LLM)
- Breadcrumb: Venture > Division > Phase (clickable navigation)

### Zone 2: Nerve Center (left panel, ~200px)

The **always-visible navigation** that replaces the current division sidebar. Three stacked sections:

**a) Division Map**
A compact visual showing all divisions as connected nodes. Each node shows:
- Name (truncated)
- Phase indicator (4 tiny dots: storming/planning/kanban/crafting, colored by status)
- Active agent avatar (if any agent is working on this division)

Clicking a division opens it in the workspace. The currently selected division is highlighted.

**b) Agent Roster**
Shows all 12 agent roles as small tiles. Each tile shows:
- Role icon/initial (V for Visionary, S for Stormer, etc.)
- Status: idle / working / waiting-for-gate / completed
- If working: which division they're on

Clicking an agent opens its session detail in the workspace.

**c) Gate Queue**
Shows pending human decisions (HITL gates):
- "Stormer awaits DESIGN GATE for billing" [Pass] [Reject]
- "Reviewer awaits REVIEW GATE for auth" [Pass] [Reject]

These are **action items for you**. They should feel urgent — amber/pulsing when new.

### Zone 3: Workspace (main area)

Context-sensitive. Changes based on what you clicked:

| Context | Workspace Shows |
|---------|----------------|
| No venture | Venture browser (current, but refined) |
| Venture selected, no division | **Venture Overview** (new!) |
| Division selected | Phase workspace (storming/planning/kanban/crafting) |
| Agent clicked | Agent session detail (conversation + output) |
| Gate clicked | Gate review view with agent output + approve/reject |

### Zone 4: Activity Rail (bottom, ~40px, collapsible)

A ticker of recent events across the venture. Like a news feed:
- Agent completions, gate decisions, phase transitions
- Errors and warnings
- Clicking an event navigates to the relevant context

---

## The Five Key Screens

### Screen 1: Venture Overview (NEW — the killer screen)

When you select a venture but no specific division, you see the **big picture**.

```
┌─────────────────────────────────────────────────────────┐
│                    VENTURE OVERVIEW                       │
│                                                           │
│  ┌─────────┐    ┌─────────┐    ┌─────────┐              │
│  │  auth   │───→│ billing │───→│ notify  │              │
│  │ ●●●○    │    │ ●●○○    │    │ ●○○○    │              │
│  │ Stormer │    │ Archit. │    │  idle   │              │
│  └─────────┘    └─────────┘    └─────────┘              │
│       │                                                   │
│       └────────→┌─────────┐                              │
│                 │  users  │                              │
│                 │ ●●●●    │                              │
│                 │ Testing │                              │
│                 └─────────┘                              │
│                                                           │
│  ─── Progress ──────────────────────────────────────     │
│  Storming   ████████████████████░░░░  80%                │
│  Planning   ████████████░░░░░░░░░░░░  50%                │
│  Kanban     ██████░░░░░░░░░░░░░░░░░░  25%                │
│  Crafting   ███░░░░░░░░░░░░░░░░░░░░░  12%                │
│                                                           │
│  ─── Agent Activity ────────────────────────────────     │
│  ◉ Stormer    working on "billing"  ████░░ 67%           │
│  ◉ Architect  idle (waiting for design gate)             │
│  ◉ Reviewer   reviewing "auth"  ████████░ 90%            │
│                                                           │
└─────────────────────────────────────────────────────────┘
```

**Division Graph**: Divisions as nodes with dependency arrows between them (from the `CONSUMES/PUBLISHES` data the explorer/stormer produce). Each node shows:
- Name
- 4 phase dots (filled = complete, half = in progress, empty = not started)
- Current agent working on it (if any)

**Phase Progress Bars**: Aggregate progress across all divisions. "How far along is this venture overall?"

**Agent Activity**: Live view of what each agent is doing right now.

This screen is what you see 80% of the time. It's your **command center**.

### Screen 2: Division Phase Workspace (ENHANCED)

The current phase views (storming, planning, kanban, crafting) but with key improvements:

**a) Phase Rail** (horizontal, replaces PhaseProgress)

```
  STORMING ──────── PLANNING ──────── KANBAN ──────── CRAFTING
  ● Complete        ● Open            ○ Not started   ○ Not started
  [3 aggs, 8 evts]  [shelve|submit]   []              []
```

Each phase shows:
- Status dot + label
- Key metrics (aggregate count, desk count, card count, module count)
- Available actions as small buttons
- Click to switch phase

**b) Agent Presence Indicator**

When an agent is actively working on this division's current phase, show a small card at the top:

```
┌─ Agent Active ──────────────────────────────────┐
│ ◉ Stormer is working...  "Identifying commands  │
│   for the payment aggregate"     [View Session]  │
└─────────────────────────────────────────────────┘
```

This replaces the hidden "Agents" toggle. The agent is always visible when working.

**c) AI Assist Stays** — the right-panel AI chat is already good. Keep it.

### Screen 3: Agent Session View (ENHANCED)

When you click an agent (from the roster or activity rail):

```
┌─────────────────────────────────────────────────────────┐
│  STORMER SESSION  ·  Division: billing  ·  T1           │
│  Status: GATE_PENDING  ·  12 turns  ·  4.2k tokens     │
├─────────────────────────────────────────────────────────┤
│                                                           │
│  [System] You are The Stormer. You facilitate...         │
│                                                           │
│  [Assistant] I'll begin the EventStorm for billing.      │
│  ## Aggregates                                            │
│  ### invoice_aggregate                                    │
│  - Stream: invoice-{id}                                  │
│  - Desks:                                                │
│    - initiate_invoice/ → invoice_initiated_v1            │
│    ...                                                    │
│                                                           │
│  [Notation Output]                                       │
│  AGG invoice_aggregate invoice-{id}                      │
│  DESK initiate_invoice -> invoice_initiated_v1           │
│  PM on_payment_received_generate_invoice -> invoice      │
│                                                           │
├─────────────────────────────────────────────────────────┤
│  ┌── DESIGN GATE ──────────────────────────────────┐    │
│  │ The Stormer has completed the EventStorm.         │    │
│  │ Review the output above and decide:               │    │
│  │                                                    │    │
│  │  [✓ Pass Gate]    [✗ Reject with Feedback]        │    │
│  └──────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────┘
```

Key features:
- **Rendered notation**: Martha notation blocks (`AGG`, `DESK`, `PM`, `FLAGS`) rendered as structured cards, not raw text
- **Gate decision inline**: When a gate is pending, the decision UI appears right there
- **Token/cost tracking**: Visible per session
- **Conversation flow**: System → Assistant → (optional human input) → Assistant → Gate

### Screen 4: Big Picture Storm (ENHANCED)

The EventStorming canvas. Currently exists. Enhancements:

**a) Spatial Canvas** (the long-term dream)

Instead of a list, make it a 2D canvas where:
- Event stickies are orange rectangles you can drag
- Command stickies are blue
- Aggregate swimlanes are yellow
- Arrows connect related events across aggregates
- Clusters are outlined groups

This is the classic EventStorming on a virtual wall. Agents place stickies, humans can rearrange them.

**b) Near-term**: Keep the current structured output but add:
- Collapsible cluster sections
- Visual arrows between dependencies
- "Run Stormer" button per-cluster for AI refinement

### Screen 5: Gate Review (NEW — dedicated view)

When a gate needs human decision, it deserves its own focused view:

```
┌─────────────────────────────────────────────────────────┐
│  DESIGN GATE  ·  Division: billing                       │
│                                                           │
│  Agent: Stormer  ·  Completed 2 minutes ago              │
│                                                           │
│  ── Output Summary ─────────────────────────────────     │
│                                                           │
│  3 Aggregates    8 Events    5 Desks    2 PMs            │
│                                                           │
│  invoice_aggregate                                       │
│    initiate_invoice → invoice_initiated_v1               │
│    submit_invoice   → invoice_submitted_v1               │
│    pay_invoice      → invoice_paid_v1                    │
│                                                           │
│  payment_aggregate                                       │
│    process_payment  → payment_processed_v1               │
│    refund_payment   → payment_refunded_v1                │
│                                                           │
│  PM: on_invoice_submitted_process_payment                │
│  PM: on_payment_refunded_credit_invoice                  │
│                                                           │
│  ── Reviewer Notes ─────────────────────────────────     │
│  ⚠ MAJOR: invoice_paid_v1 missing amount field           │
│  ✓ Walking skeleton present for all aggregates           │
│  ✓ No CRUD events detected                               │
│                                                           │
│  ┌──────────────────────────────────────────────────┐    │
│  │  [✓ Pass Gate]    [✗ Reject]    [💬 Comment]     │    │
│  │                                                    │    │
│  │  Optional feedback:                                │    │
│  │  ┌──────────────────────────────────────────────┐ │    │
│  │  │ Add a "cancel_invoice" desk with reason...   │ │    │
│  │  └──────────────────────────────────────────────┘ │    │
│  └──────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────┘
```

This is where the **human-in-the-loop** happens. It should feel decisive and important.

---

## Screen 6: Gate Inbox (NEW — your decision queue)

Gates are the moments where the human matters most. With 5 divisions and 12 agent roles, gates pile up. You need a dedicated **inbox** — not just a sidebar section.

```
┌─────────────────────────────────────────────────────────────────┐
│  GATES                                              3 pending    │
├─────────────────────────────────────────────────────────────────┤
│                                                                   │
│  ┌── PENDING ──────────────────────────────────────────────────┐ │
│  │                                                              │ │
│  │  ⏳ DESIGN GATE  ·  billing  ·  Stormer  ·  2 min ago      │ │
│  │     3 aggregates, 8 events, 5 desks, 2 PMs                  │ │
│  │     Reviewer: 0 critical, 1 major                            │ │
│  │     [Review]                                                  │ │
│  │                                                              │ │
│  │  ⏳ REVIEW GATE  ·  auth  ·  Reviewer  ·  8 min ago        │ │
│  │     1 CRITICAL finding, 2 minor                              │ │
│  │     [Review]                                                  │ │
│  │                                                              │ │
│  │  ⏳ BOUNDARY GATE  ·  venture  ·  Explorer  ·  15 min ago   │ │
│  │     5 divisions identified, 3 dependencies                   │ │
│  │     [Review]                                                  │ │
│  │                                                              │ │
│  └──────────────────────────────────────────────────────────────┘ │
│                                                                   │
│  ┌── DECIDED (recent) ─────────────────────────────────────────┐ │
│  │                                                              │ │
│  │  ✓ VISION GATE  ·  venture  ·  Visionary  ·  1h ago        │ │
│  │     Passed with feedback: "Add multi-tenancy to scope"       │ │
│  │                                                              │ │
│  │  ✗ DESIGN GATE  ·  notify  ·  Stormer  ·  2h ago           │ │
│  │     Rejected: "Missing error handling aggregates"            │ │
│  │                                                              │ │
│  └──────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────┘
```

**Why a dedicated screen?**
- Gates accumulate across divisions — you need to see all of them at once
- Each gate has different context (stormer output vs reviewer findings vs explorer boundaries)
- Your feedback on rejected gates feeds back to the agent for retry
- The decision history gives you an audit trail: when did you approve this, what feedback did you give?

**The Gate Inbox is reachable from:**
- The Nerve Center gate queue section (click "View All")
- Keyboard shortcut `g`
- The Venture Bar badge: "3 gates pending"
- The Activity Rail when a gate event appears

**Badge behavior:** The Venture Bar shows a persistent badge when gates are pending. Like unread emails — you always know there are decisions waiting.

---

## Agent Relay: The Spectacle

This is the signature visual that makes Martha unlike anything else.

### The Concept

Agents don't work in isolation. The stormer's EventStorm becomes the architect's input. The architect's design becomes the coder's blueprint. The reviewer examines everyone's output. The mentor observes everything and whispers corrections.

**The user should SEE this.**

### Agent Relay View (workspace mode)

When you click "Agent Relay" from the Venture Overview, you see the full agent pipeline as a **live relay race**:

```
┌─────────────────────────────────────────────────────────────────┐
│  AGENT RELAY  ·  Division: billing                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                   │
│  ┌──────────┐   handoff    ┌──────────┐   handoff               │
│  │VISIONARY │─────────────→│ EXPLORER │─────────────→           │
│  │ ✓ Done   │  "venture    │ ✓ Done   │  "5 divs    │          │
│  │ T1       │   vision"    │ T1       │   found"     │          │
│  └──────────┘              └──────────┘              │          │
│                                  │                    │          │
│                                  │ spawns per-div     │          │
│                             ┌────┴─────┐              │          │
│                             ▼          ▼              ▼          │
│  ┌──────────┐         ┌──────────┐  ┌──────────┐  ┌──────────┐ │
│  │ STORMER  │────────→│ARCHITECT │  │ STORMER  │  │ STORMER  │ │
│  │ ◉ Active │ handoff │ ⏳ Wait  │  │ ✓ Done   │  │ ○ Idle   │ │
│  │ billing  │ >>>>>>> │ billing  │  │ auth     │  │ notify   │ │
│  │ T1→T2    │         │ T2       │  │ T1→T2    │  │          │ │
│  └──────────┘         └──────────┘  └──────────┘  └──────────┘ │
│       │                    │              │                       │
│       │                    │              ▼                       │
│       │                    │         ┌──────────┐                │
│       │                    │         │ ERL_CODER│                │
│       │                    │         │ ◉ Active │                │
│       │                    └────────→│ auth     │                │
│       │                   (design)   │ T2       │                │
│       │                              └──────────┘                │
│       │                                   │                      │
│       │                                   ▼                      │
│       │                              ┌──────────┐                │
│       │          observes all        │ REVIEWER │                │
│       │         ┌───────────────────→│ ○ Idle   │                │
│       │         │                    │ T1       │                │
│       │         │                    └──────────┘                │
│       │         │                                                │
│       │    ┌──────────┐                                          │
│       └───→│ MENTOR   │  (observes everything, whispers)         │
│            │ ◉ Watch  │                                          │
│            │ T3/T1    │                                          │
│            └──────────┘                                          │
│                                                                   │
│  ─── Live Feed ──────────────────────────────────────────────   │
│  14:32  Stormer → Architect [billing]: "3 aggs, 8 events,       │
│         5 desks. Ready for design gate."                         │
│  14:30  Mentor → Stormer [billing]: "Consider separating        │
│         invoice from payment — different lifecycles."            │
│  14:28  Explorer → Stormer [auth]: handoff complete              │
│  14:25  Visionary → Explorer: "Vision gate passed"               │
│                                                                   │
└─────────────────────────────────────────────────────────────────┘
```

### What You See

**Agent Nodes** — each agent as a card showing:
- Role name + icon
- Status: `○ Idle` / `◉ Active` / `✓ Done` / `⏳ Waiting` / `✗ Failed`
- Current division assignment
- Tier (T1/T2/T3)

**Handoff Arrows** — animated connections showing data flow between agents:
- Solid line = handoff complete (data transferred)
- Animated dashes `>>>>>>>` = handoff in progress
- Dotted line = waiting for upstream

**The Live Feed** — at the bottom, a chronological log of agent-to-agent messages:
- Handoffs: "Stormer → Architect: 3 aggs, 8 events, ready for design gate"
- Mentor whispers: "Mentor → Stormer: consider separating invoice from payment"
- Gate decisions: "Human → Stormer: design gate PASSED"
- Errors: "Erlang Coder: compilation failed on invoice_aggregate.erl"

### The Animated Handoff

When a handoff happens (e.g., stormer finishes, architect receives the output), the arrow between them **animates**:

1. Stormer node transitions from `◉ Active` to `✓ Done` (green pulse)
2. A "data packet" visual slides along the arrow from Stormer → Architect
3. The packet briefly shows a summary: "3 aggs, 8 evts"
4. Architect node transitions from `⏳ Waiting` to `◉ Active` (blue pulse)
5. If a gate is involved, the packet pauses at a gate icon on the arrow

This is the **money shot**. Watching data flow between agents in real-time is what makes Martha feel like a living system, not a batch job.

### Mentor's Observation Lines

The Mentor is special — it observes all agents. Its connections are shown as thin, subtle dotted lines radiating outward. When the Mentor sends a whisper (suggestion), the relevant line pulses briefly and the whisper appears in the Live Feed.

### Click Behavior

- Click an **agent node** → opens that agent's session detail
- Click a **handoff arrow** → shows the data that was passed (the notation output)
- Click a **gate icon** on an arrow → opens the gate review for that handoff

### Layout Modes

The relay can render in two modes:

**Pipeline mode** (default): Left-to-right flow showing the sequential pipeline:
```
Visionary → Explorer → Stormer → Architect → Coders → Tester → Reviewer
                                                          ↕
                                                       Mentor (observes)
```

**Division mode**: Groups agents by which division they're working on:
```
billing:  Stormer ◉ → Architect ⏳
auth:     Erlang Coder ◉ → Tester ○
notify:   (idle)
```

Toggle between modes with a tab in the header.

### What Powers This (Backend)

The handoff data already flows through the system — it's the PM chain:
- `on_visionary_completed_escalate_vision_gate` → gate → `on_vision_gate_passed_initiate_explorer`
- `on_stormer_completed_escalate_design_gate` → gate → architect initiation

We just need the SSE stream to carry these events to the frontend. Each `on_*` PM firing = a handoff event the UI can render.

The `on_*` directories we just refactored literally ARE the relay points. The filesystem screams the same thing the UI visualizes.

---

## Real-Time: The Missing Piece

The current architecture has NO real-time updates (polling only). For the UX to feel alive, we need **Server-Sent Events (SSE)**.

### Proposal: SSE Event Stream

Add one SSE endpoint:

```
GET /api/ventures/:venture_id/stream
```

Streams all events for the venture:
- Agent status changes (started, completed, gate_pending)
- Division phase transitions
- Kanban card moves
- Gate decisions
- Errors

The frontend subscribes once and updates all stores reactively. No polling.

**Backend**: A simple Cowboy SSE handler that subscribes to pg groups and forwards events as JSON.

**Impact**: This single addition transforms the entire UX from "refresh to see changes" to "watch it happen live."

---

## Micro-Interactions That Delight

| Interaction | Effect |
|------------|--------|
| Agent starts working | Its avatar pulses gently in the Nerve Center |
| Gate becomes pending | Amber glow on the gate queue item, subtle bounce |
| Phase completes | The phase dot fills with a smooth animation |
| Division created | Node appears in the graph with a fade-in |
| Error occurs | Red flash on the activity rail, auto-expands |
| Venture archived | Nodes gracefully fade to gray |
| All phases complete | Confetti (just kidding). Green checkmark cascade. |

---

## Keyboard Shortcuts

| Key | Action |
|-----|--------|
| `1-4` | Switch phase (storming/planning/kanban/crafting) |
| `d` | Division list focus |
| `a` | Toggle agent roster |
| `g` | Jump to next pending gate |
| `?` | Open AI assist |
| `Esc` | Back to venture overview |
| `/` | Search |

---

## Color Language

| Color | Meaning |
|-------|---------|
| Hecate purple | Brand, primary actions |
| Orange | Storming phase, events |
| Blue | Planning phase, commands |
| Green | Kanban phase, success |
| Amber | Crafting phase, warnings, pending gates |
| Red | Errors, critical findings |
| Gray | Archived, inactive |

Each phase has its own color. When you're in a phase, the accent color subtly shifts. This gives spatial memory — "I'm in the blue zone" = planning.

---

## Implementation Priority

### Phase 1: Foundation (backend SSE + layout restructure)
1. **SSE endpoint** for real-time venture events
2. **Venture Bar** (persistent header with breadcrumb + gate badge)
3. **Nerve Center** (left panel: division list + agent roster + gate queue)
4. **Activity Rail** (bottom ticker)

### Phase 2: Gate Inbox + Venture Overview
5. **Gate Inbox** (dedicated gate decision screen with pending/decided sections)
6. **Division graph** (nodes + dependency arrows)
7. **Aggregate progress bars**
8. **Live agent activity section**

### Phase 3: Agent Relay
9. **Agent Relay view** (pipeline visualization with agent nodes + handoff arrows)
10. **Live Feed** (chronological agent-to-agent message log)
11. **Animated handoffs** (data packets sliding along arrows)
12. **Pipeline/Division toggle** modes

### Phase 4: Agent Presence + Session Enhancement
13. **Agent presence indicator** on phase workspace
14. **Enhanced session view** with rendered notation
15. **Dedicated gate review** (from Gate Inbox → full review screen)

### Phase 5: Polish
16. **Phase color theming**
17. **Keyboard shortcuts**
18. **Micro-interactions and animations**
19. **Mentor observation lines** (subtle dotted connections)
20. **EventStorming canvas** (2D spatial view — ambitious)

---

## What Makes This Spectacular

1. **You never wonder "what's happening?"** — agents are visible, progress is visible, decisions needed are visible
2. **You never get lost** — the Nerve Center is always there, the breadcrumb always tells you where you are
3. **You feel powerful** — the Gate Inbox puts all decisions in one place, you're the commander
4. **It feels alive** — SSE makes everything real-time, dots pulse, progress moves
5. **The Agent Relay is the signature** — watching data flow between agents with animated handoffs is unlike anything that exists. The `on_*` PM directories in the backend literally map 1:1 to the relay arrows in the UI. Architecture made visible.
6. **It's dense but clear** — power-user friendly, no wasted space, every pixel earns its place
