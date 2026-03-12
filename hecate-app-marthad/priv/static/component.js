typeof window < "u" && ((window.__svelte ??= {}).v ??= /* @__PURE__ */ new Set()).add("5");
const yo = 1, wo = 2, Gn = 4, ko = 8, $o = 16, Co = 2, So = 4, Eo = 8, Ao = 1, Do = 2, Ua = "[", ra = "[!", za = "]", Jr = {}, It = /* @__PURE__ */ Symbol(), Po = "http://www.w3.org/1999/xhtml", wa = !1;
var Ya = Array.isArray, To = Array.prototype.indexOf, cs = Array.prototype.includes, sa = Array.from, Ks = Object.keys, Rs = Object.defineProperty, os = Object.getOwnPropertyDescriptor, Wn = Object.getOwnPropertyDescriptors, Ro = Object.prototype, Mo = Array.prototype, Ka = Object.getPrototypeOf, kn = Object.isExtensible;
const Er = () => {
};
function Io(e) {
  return e();
}
function Js(e) {
  for (var t = 0; t < e.length; t++)
    e[t]();
}
function Un() {
  var e, t, r = new Promise((a, o) => {
    e = a, t = o;
  });
  return { promise: r, resolve: e, reject: t };
}
function ka(e, t) {
  if (Array.isArray(e))
    return e;
  if (!(Symbol.iterator in e))
    return Array.from(e);
  const r = [];
  for (const a of e)
    if (r.push(a), r.length === t) break;
  return r;
}
const Lt = 2, Qs = 4, Ls = 8, zn = 1 << 24, Rr = 16, ur = 32, qr = 64, Ja = 128, rr = 512, Tt = 1024, Nt = 2048, dr = 4096, Qt = 8192, Ar = 16384, aa = 32768, ds = 65536, $n = 1 << 17, Yn = 1 << 18, ts = 1 << 19, Kn = 1 << 20, Cr = 1 << 25, Qr = 32768, $a = 1 << 21, Qa = 1 << 22, Nr = 1 << 23, Or = /* @__PURE__ */ Symbol("$state"), Lo = /* @__PURE__ */ Symbol("legacy props"), No = /* @__PURE__ */ Symbol(""), is = new class extends Error {
  name = "StaleReactionError";
  message = "The reaction that called `getAbortSignal()` was re-run or destroyed";
}(), na = 3, rs = 8;
function Jn(e) {
  throw new Error("https://svelte.dev/e/lifecycle_outside_component");
}
function Oo() {
  throw new Error("https://svelte.dev/e/async_derived_orphan");
}
function Fo(e, t, r) {
  throw new Error("https://svelte.dev/e/each_key_duplicate");
}
function jo(e) {
  throw new Error("https://svelte.dev/e/effect_in_teardown");
}
function Vo() {
  throw new Error("https://svelte.dev/e/effect_in_unowned_derived");
}
function Bo(e) {
  throw new Error("https://svelte.dev/e/effect_orphan");
}
function qo() {
  throw new Error("https://svelte.dev/e/effect_update_depth_exceeded");
}
function Ho() {
  throw new Error("https://svelte.dev/e/hydration_failed");
}
function Go() {
  throw new Error("https://svelte.dev/e/state_descriptors_fixed");
}
function Wo() {
  throw new Error("https://svelte.dev/e/state_prototype_fixed");
}
function Uo() {
  throw new Error("https://svelte.dev/e/state_unsafe_mutation");
}
function zo() {
  throw new Error("https://svelte.dev/e/svelte_boundary_reset_onerror");
}
function Ns(e) {
  console.warn("https://svelte.dev/e/hydration_mismatch");
}
function Yo() {
  console.warn("https://svelte.dev/e/select_multiple_invalid_value");
}
function Ko() {
  console.warn("https://svelte.dev/e/svelte_boundary_reset_noop");
}
let it = !1;
function Sr(e) {
  it = e;
}
let lt;
function Vt(e) {
  if (e === null)
    throw Ns(), Jr;
  return lt = e;
}
function us() {
  return Vt(/* @__PURE__ */ vr(lt));
}
function n(e) {
  if (it) {
    if (/* @__PURE__ */ vr(lt) !== null)
      throw Ns(), Jr;
    lt = e;
  }
}
function Et(e = 1) {
  if (it) {
    for (var t = e, r = lt; t--; )
      r = /** @type {TemplateNode} */
      /* @__PURE__ */ vr(r);
    lt = r;
  }
}
function Xs(e = !0) {
  for (var t = 0, r = lt; ; ) {
    if (r.nodeType === rs) {
      var a = (
        /** @type {Comment} */
        r.data
      );
      if (a === za) {
        if (t === 0) return r;
        t -= 1;
      } else (a === Ua || a === ra || // "[1", "[2", etc. for if blocks
      a[0] === "[" && !isNaN(Number(a.slice(1)))) && (t += 1);
    }
    var o = (
      /** @type {TemplateNode} */
      /* @__PURE__ */ vr(r)
    );
    e && r.remove(), r = o;
  }
}
function Qn(e) {
  if (!e || e.nodeType !== rs)
    throw Ns(), Jr;
  return (
    /** @type {Comment} */
    e.data
  );
}
function Xn(e) {
  return e === this.v;
}
function Zn(e, t) {
  return e != e ? t == t : e !== t || e !== null && typeof e == "object" || typeof e == "function";
}
function ei(e) {
  return !Zn(e, this.v);
}
let gs = !1, Jo = !1;
function Qo() {
  gs = !0;
}
let mt = null;
function vs(e) {
  mt = e;
}
function Ct(e, t = !1, r) {
  mt = {
    p: mt,
    i: !1,
    c: null,
    e: null,
    s: e,
    x: null,
    l: gs && !t ? { s: null, u: null, $: [] } : null
  };
}
function St(e) {
  var t = (
    /** @type {ComponentContext} */
    mt
  ), r = t.e;
  if (r !== null) {
    t.e = null;
    for (var a of r)
      mi(a);
  }
  return e !== void 0 && (t.x = e), t.i = !0, mt = t.p, e ?? /** @type {T} */
  {};
}
function Os() {
  return !gs || mt !== null && mt.l === null;
}
let Gr = [];
function ti() {
  var e = Gr;
  Gr = [], Js(e);
}
function gr(e) {
  if (Gr.length === 0 && !Ss) {
    var t = Gr;
    queueMicrotask(() => {
      t === Gr && ti();
    });
  }
  Gr.push(e);
}
function Xo() {
  for (; Gr.length > 0; )
    ti();
}
function ri(e) {
  var t = ct;
  if (t === null)
    return ot.f |= Nr, e;
  if ((t.f & aa) === 0) {
    if ((t.f & Ja) === 0)
      throw e;
    t.b.error(e);
  } else
    fs(e, t);
}
function fs(e, t) {
  for (; t !== null; ) {
    if ((t.f & Ja) !== 0)
      try {
        t.b.error(e);
        return;
      } catch (r) {
        e = r;
      }
    t = t.parent;
  }
  throw e;
}
const Zo = -7169;
function yt(e, t) {
  e.f = e.f & Zo | t;
}
function Xa(e) {
  (e.f & rr) !== 0 || e.deps === null ? yt(e, Tt) : yt(e, dr);
}
function si(e) {
  if (e !== null)
    for (const t of e)
      (t.f & Lt) === 0 || (t.f & Qr) === 0 || (t.f ^= Qr, si(
        /** @type {Derived} */
        t.deps
      ));
}
function ai(e, t, r) {
  (e.f & Nt) !== 0 ? t.add(e) : (e.f & dr) !== 0 && r.add(e), si(e.deps), yt(e, Tt);
}
const qs = /* @__PURE__ */ new Set();
let dt = null, Zs = null, or = null, Gt = [], ia = null, Ca = !1, Ss = !1;
class Dr {
  committed = !1;
  /**
   * The current values of any sources that are updated in this batch
   * They keys of this map are identical to `this.#previous`
   * @type {Map<Source, any>}
   */
  current = /* @__PURE__ */ new Map();
  /**
   * The values of any sources that are updated in this batch _before_ those updates took place.
   * They keys of this map are identical to `this.#current`
   * @type {Map<Source, any>}
   */
  previous = /* @__PURE__ */ new Map();
  /**
   * When the batch is committed (and the DOM is updated), we need to remove old branches
   * and append new ones by calling the functions added inside (if/each/key/etc) blocks
   * @type {Set<() => void>}
   */
  #e = /* @__PURE__ */ new Set();
  /**
   * If a fork is discarded, we need to destroy any effects that are no longer needed
   * @type {Set<(batch: Batch) => void>}
   */
  #t = /* @__PURE__ */ new Set();
  /**
   * The number of async effects that are currently in flight
   */
  #r = 0;
  /**
   * The number of async effects that are currently in flight, _not_ inside a pending boundary
   */
  #i = 0;
  /**
   * A deferred that resolves when the batch is committed, used with `settled()`
   * TODO replace with Promise.withResolvers once supported widely enough
   * @type {{ promise: Promise<void>, resolve: (value?: any) => void, reject: (reason: unknown) => void } | null}
   */
  #o = null;
  /**
   * Deferred effects (which run after async work has completed) that are DIRTY
   * @type {Set<Effect>}
   */
  #a = /* @__PURE__ */ new Set();
  /**
   * Deferred effects that are MAYBE_DIRTY
   * @type {Set<Effect>}
   */
  #s = /* @__PURE__ */ new Set();
  /**
   * A map of branches that still exist, but will be destroyed when this batch
   * is committed — we skip over these during `process`.
   * The value contains child effects that were dirty/maybe_dirty before being reset,
   * so they can be rescheduled if the branch survives.
   * @type {Map<Effect, { d: Effect[], m: Effect[] }>}
   */
  #n = /* @__PURE__ */ new Map();
  is_fork = !1;
  #l = !1;
  is_deferred() {
    return this.is_fork || this.#i > 0;
  }
  /**
   * Add an effect to the #skipped_branches map and reset its children
   * @param {Effect} effect
   */
  skip_effect(t) {
    this.#n.has(t) || this.#n.set(t, { d: [], m: [] });
  }
  /**
   * Remove an effect from the #skipped_branches map and reschedule
   * any tracked dirty/maybe_dirty child effects
   * @param {Effect} effect
   */
  unskip_effect(t) {
    var r = this.#n.get(t);
    if (r) {
      this.#n.delete(t);
      for (var a of r.d)
        yt(a, Nt), lr(a);
      for (a of r.m)
        yt(a, dr), lr(a);
    }
  }
  /**
   *
   * @param {Effect[]} root_effects
   */
  process(t) {
    Gt = [], this.apply();
    var r = [], a = [];
    for (const o of t)
      this.#c(o, r, a);
    if (this.is_deferred()) {
      this.#u(a), this.#u(r);
      for (const [o, c] of this.#n)
        li(o, c);
    } else {
      for (const o of this.#e) o();
      this.#e.clear(), this.#r === 0 && this.#d(), Zs = this, dt = null, Cn(a), Cn(r), Zs = null, this.#o?.resolve();
    }
    or = null;
  }
  /**
   * Traverse the effect tree, executing effects or stashing
   * them for later execution as appropriate
   * @param {Effect} root
   * @param {Effect[]} effects
   * @param {Effect[]} render_effects
   */
  #c(t, r, a) {
    t.f ^= Tt;
    for (var o = t.first, c = null; o !== null; ) {
      var d = o.f, u = (d & (ur | qr)) !== 0, v = u && (d & Tt) !== 0, h = v || (d & Qt) !== 0 || this.#n.has(o);
      if (!h && o.fn !== null) {
        u ? o.f ^= Tt : c !== null && (d & (Qs | Ls | zn)) !== 0 ? c.b.defer_effect(o) : (d & Qs) !== 0 ? r.push(o) : Vs(o) && ((d & Rr) !== 0 && this.#s.add(o), Ms(o));
        var w = o.first;
        if (w !== null) {
          o = w;
          continue;
        }
      }
      var g = o.parent;
      for (o = o.next; o === null && g !== null; )
        g === c && (c = null), o = g.next, g = g.parent;
    }
  }
  /**
   * @param {Effect[]} effects
   */
  #u(t) {
    for (var r = 0; r < t.length; r += 1)
      ai(t[r], this.#a, this.#s);
  }
  /**
   * Associate a change to a given source with the current
   * batch, noting its previous and current values
   * @param {Source} source
   * @param {any} value
   */
  capture(t, r) {
    r !== It && !this.previous.has(t) && this.previous.set(t, r), (t.f & Nr) === 0 && (this.current.set(t, t.v), or?.set(t, t.v));
  }
  activate() {
    dt = this, this.apply();
  }
  deactivate() {
    dt === this && (dt = null, or = null);
  }
  flush() {
    if (this.activate(), Gt.length > 0) {
      if (ni(), dt !== null && dt !== this)
        return;
    } else this.#r === 0 && this.process([]);
    this.deactivate();
  }
  discard() {
    for (const t of this.#t) t(this);
    this.#t.clear();
  }
  #d() {
    if (qs.size > 1) {
      this.previous.clear();
      var t = or, r = !0;
      for (const o of qs) {
        if (o === this) {
          r = !1;
          continue;
        }
        const c = [];
        for (const [u, v] of this.current) {
          if (o.current.has(u))
            if (r && v !== o.current.get(u))
              o.current.set(u, v);
            else
              continue;
          c.push(u);
        }
        if (c.length === 0)
          continue;
        const d = [...o.current.keys()].filter((u) => !this.current.has(u));
        if (d.length > 0) {
          var a = Gt;
          Gt = [];
          const u = /* @__PURE__ */ new Set(), v = /* @__PURE__ */ new Map();
          for (const h of c)
            ii(h, d, u, v);
          if (Gt.length > 0) {
            dt = o, o.apply();
            for (const h of Gt)
              o.#c(h, [], []);
            o.deactivate();
          }
          Gt = a;
        }
      }
      dt = null, or = t;
    }
    this.committed = !0, qs.delete(this);
  }
  /**
   *
   * @param {boolean} blocking
   */
  increment(t) {
    this.#r += 1, t && (this.#i += 1);
  }
  /**
   *
   * @param {boolean} blocking
   */
  decrement(t) {
    this.#r -= 1, t && (this.#i -= 1), !this.#l && (this.#l = !0, gr(() => {
      this.#l = !1, this.is_deferred() ? Gt.length > 0 && this.flush() : this.revive();
    }));
  }
  revive() {
    for (const t of this.#a)
      this.#s.delete(t), yt(t, Nt), lr(t);
    for (const t of this.#s)
      yt(t, dr), lr(t);
    this.flush();
  }
  /** @param {() => void} fn */
  oncommit(t) {
    this.#e.add(t);
  }
  /** @param {(batch: Batch) => void} fn */
  ondiscard(t) {
    this.#t.add(t);
  }
  settled() {
    return (this.#o ??= Un()).promise;
  }
  static ensure() {
    if (dt === null) {
      const t = dt = new Dr();
      qs.add(dt), Ss || gr(() => {
        dt === t && t.flush();
      });
    }
    return dt;
  }
  apply() {
  }
}
function vt(e) {
  var t = Ss;
  Ss = !0;
  try {
    for (var r; ; ) {
      if (Xo(), Gt.length === 0 && (dt?.flush(), Gt.length === 0))
        return ia = null, /** @type {T} */
        r;
      ni();
    }
  } finally {
    Ss = t;
  }
}
function ni() {
  Ca = !0;
  var e = null;
  try {
    for (var t = 0; Gt.length > 0; ) {
      var r = Dr.ensure();
      if (t++ > 1e3) {
        var a, o;
        el();
      }
      r.process(Gt), Fr.clear();
    }
  } finally {
    Gt = [], Ca = !1, ia = null;
  }
}
function el() {
  try {
    qo();
  } catch (e) {
    fs(e, ia);
  }
}
let kr = null;
function Cn(e) {
  var t = e.length;
  if (t !== 0) {
    for (var r = 0; r < t; ) {
      var a = e[r++];
      if ((a.f & (Ar | Qt)) === 0 && Vs(a) && (kr = /* @__PURE__ */ new Set(), Ms(a), a.deps === null && a.first === null && a.nodes === null && (a.teardown === null && a.ac === null ? $i(a) : a.fn = null), kr?.size > 0)) {
        Fr.clear();
        for (const o of kr) {
          if ((o.f & (Ar | Qt)) !== 0) continue;
          const c = [o];
          let d = o.parent;
          for (; d !== null; )
            kr.has(d) && (kr.delete(d), c.push(d)), d = d.parent;
          for (let u = c.length - 1; u >= 0; u--) {
            const v = c[u];
            (v.f & (Ar | Qt)) === 0 && Ms(v);
          }
        }
        kr.clear();
      }
    }
    kr = null;
  }
}
function ii(e, t, r, a) {
  if (!r.has(e) && (r.add(e), e.reactions !== null))
    for (const o of e.reactions) {
      const c = o.f;
      (c & Lt) !== 0 ? ii(
        /** @type {Derived} */
        o,
        t,
        r,
        a
      ) : (c & (Qa | Rr)) !== 0 && (c & Nt) === 0 && oi(o, t, a) && (yt(o, Nt), lr(
        /** @type {Effect} */
        o
      ));
    }
}
function oi(e, t, r) {
  const a = r.get(e);
  if (a !== void 0) return a;
  if (e.deps !== null)
    for (const o of e.deps) {
      if (cs.call(t, o))
        return !0;
      if ((o.f & Lt) !== 0 && oi(
        /** @type {Derived} */
        o,
        t,
        r
      ))
        return r.set(
          /** @type {Derived} */
          o,
          !0
        ), !0;
    }
  return r.set(e, !1), !1;
}
function lr(e) {
  for (var t = ia = e; t.parent !== null; ) {
    t = t.parent;
    var r = t.f;
    if (Ca && t === ct && (r & Rr) !== 0 && (r & Yn) === 0)
      return;
    if ((r & (qr | ur)) !== 0) {
      if ((r & Tt) === 0) return;
      t.f ^= Tt;
    }
  }
  Gt.push(t);
}
function li(e, t) {
  if (!((e.f & ur) !== 0 && (e.f & Tt) !== 0)) {
    (e.f & Nt) !== 0 ? t.d.push(e) : (e.f & dr) !== 0 && t.m.push(e), yt(e, Tt);
    for (var r = e.first; r !== null; )
      li(r, t), r = r.next;
  }
}
function tl(e) {
  let t = 0, r = Xr(0), a;
  return () => {
    sn() && (s(r), ca(() => (t === 0 && (a = ss(() => e(() => Es(r)))), t += 1, () => {
      gr(() => {
        t -= 1, t === 0 && (a?.(), a = void 0, Es(r));
      });
    })));
  };
}
var rl = ds | ts | Ja;
function sl(e, t, r) {
  new al(e, t, r);
}
class al {
  /** @type {Boundary | null} */
  parent;
  is_pending = !1;
  /** @type {TemplateNode} */
  #e;
  /** @type {TemplateNode | null} */
  #t = it ? lt : null;
  /** @type {BoundaryProps} */
  #r;
  /** @type {((anchor: Node) => void)} */
  #i;
  /** @type {Effect} */
  #o;
  /** @type {Effect | null} */
  #a = null;
  /** @type {Effect | null} */
  #s = null;
  /** @type {Effect | null} */
  #n = null;
  /** @type {DocumentFragment | null} */
  #l = null;
  /** @type {TemplateNode | null} */
  #c = null;
  #u = 0;
  #d = 0;
  #p = !1;
  #f = !1;
  /** @type {Set<Effect>} */
  #x = /* @__PURE__ */ new Set();
  /** @type {Set<Effect>} */
  #h = /* @__PURE__ */ new Set();
  /**
   * A source containing the number of pending async deriveds/expressions.
   * Only created if `$effect.pending()` is used inside the boundary,
   * otherwise updating the source results in needless `Batch.ensure()`
   * calls followed by no-op flushes
   * @type {Source<number> | null}
   */
  #v = null;
  #y = tl(() => (this.#v = Xr(this.#u), () => {
    this.#v = null;
  }));
  /**
   * @param {TemplateNode} node
   * @param {BoundaryProps} props
   * @param {((anchor: Node) => void)} children
   */
  constructor(t, r, a) {
    this.#e = t, this.#r = r, this.#i = a, this.parent = /** @type {Effect} */
    ct.b, this.is_pending = !!this.#r.pending, this.#o = nn(() => {
      if (ct.b = this, it) {
        const c = this.#t;
        us(), /** @type {Comment} */
        c.nodeType === rs && /** @type {Comment} */
        c.data === ra ? this.#k() : (this.#w(), this.#d === 0 && (this.is_pending = !1));
      } else {
        var o = this.#b();
        try {
          this.#a = er(() => a(o));
        } catch (c) {
          this.error(c);
        }
        this.#d > 0 ? this.#g() : this.is_pending = !1;
      }
      return () => {
        this.#c?.remove();
      };
    }, rl), it && (this.#e = lt);
  }
  #w() {
    try {
      this.#a = er(() => this.#i(this.#e));
    } catch (t) {
      this.error(t);
    }
  }
  #k() {
    const t = this.#r.pending;
    t && (this.#s = er(() => t(this.#e)), gr(() => {
      var r = this.#b();
      this.#a = this.#_(() => (Dr.ensure(), er(() => this.#i(r)))), this.#d > 0 ? this.#g() : (zr(
        /** @type {Effect} */
        this.#s,
        () => {
          this.#s = null;
        }
      ), this.is_pending = !1);
    }));
  }
  #b() {
    var t = this.#e;
    return this.is_pending && (this.#c = sr(), this.#e.before(this.#c), t = this.#c), t;
  }
  /**
   * Defer an effect inside a pending boundary until the boundary resolves
   * @param {Effect} effect
   */
  defer_effect(t) {
    ai(t, this.#x, this.#h);
  }
  /**
   * Returns `false` if the effect exists inside a boundary whose pending snippet is shown
   * @returns {boolean}
   */
  is_rendered() {
    return !this.is_pending && (!this.parent || this.parent.is_rendered());
  }
  has_pending_snippet() {
    return !!this.#r.pending;
  }
  /**
   * @param {() => Effect | null} fn
   */
  #_(t) {
    var r = ct, a = ot, o = mt;
    mr(this.#o), nr(this.#o), vs(this.#o.ctx);
    try {
      return t();
    } catch (c) {
      return ri(c), null;
    } finally {
      mr(r), nr(a), vs(o);
    }
  }
  #g() {
    const t = (
      /** @type {(anchor: Node) => void} */
      this.#r.pending
    );
    this.#a !== null && (this.#l = document.createDocumentFragment(), this.#l.append(
      /** @type {TemplateNode} */
      this.#c
    ), Ei(this.#a, this.#l)), this.#s === null && (this.#s = er(() => t(this.#e)));
  }
  /**
   * Updates the pending count associated with the currently visible pending snippet,
   * if any, such that we can replace the snippet with content once work is done
   * @param {1 | -1} d
   */
  #m(t) {
    if (!this.has_pending_snippet()) {
      this.parent && this.parent.#m(t);
      return;
    }
    if (this.#d += t, this.#d === 0) {
      this.is_pending = !1;
      for (const r of this.#x)
        yt(r, Nt), lr(r);
      for (const r of this.#h)
        yt(r, dr), lr(r);
      this.#x.clear(), this.#h.clear(), this.#s && zr(this.#s, () => {
        this.#s = null;
      }), this.#l && (this.#e.before(this.#l), this.#l = null);
    }
  }
  /**
   * Update the source that powers `$effect.pending()` inside this boundary,
   * and controls when the current `pending` snippet (if any) is removed.
   * Do not call from inside the class
   * @param {1 | -1} d
   */
  update_pending_count(t) {
    this.#m(t), this.#u += t, !(!this.#v || this.#p) && (this.#p = !0, gr(() => {
      this.#p = !1, this.#v && ps(this.#v, this.#u);
    }));
  }
  get_effect_pending() {
    return this.#y(), s(
      /** @type {Source<number>} */
      this.#v
    );
  }
  /** @param {unknown} error */
  error(t) {
    var r = this.#r.onerror;
    let a = this.#r.failed;
    if (this.#f || !r && !a)
      throw t;
    this.#a && (Bt(this.#a), this.#a = null), this.#s && (Bt(this.#s), this.#s = null), this.#n && (Bt(this.#n), this.#n = null), it && (Vt(
      /** @type {TemplateNode} */
      this.#t
    ), Et(), Vt(Xs()));
    var o = !1, c = !1;
    const d = () => {
      if (o) {
        Ko();
        return;
      }
      o = !0, c && zo(), Dr.ensure(), this.#u = 0, this.#n !== null && zr(this.#n, () => {
        this.#n = null;
      }), this.is_pending = this.has_pending_snippet(), this.#a = this.#_(() => (this.#f = !1, er(() => this.#i(this.#e)))), this.#d > 0 ? this.#g() : this.is_pending = !1;
    };
    gr(() => {
      try {
        c = !0, r?.(t, d), c = !1;
      } catch (u) {
        fs(u, this.#o && this.#o.parent);
      }
      a && (this.#n = this.#_(() => {
        Dr.ensure(), this.#f = !0;
        try {
          return er(() => {
            a(
              this.#e,
              () => t,
              () => d
            );
          });
        } catch (u) {
          return fs(
            u,
            /** @type {Effect} */
            this.#o.parent
          ), null;
        } finally {
          this.#f = !1;
        }
      }));
    });
  }
}
function nl(e, t, r, a) {
  const o = Os() ? Fs : _r;
  var c = e.filter((b) => !b.settled);
  if (r.length === 0 && c.length === 0) {
    a(t.map(o));
    return;
  }
  var d = dt, u = (
    /** @type {Effect} */
    ct
  ), v = il(), h = c.length === 1 ? c[0].promise : c.length > 1 ? Promise.all(c.map((b) => b.promise)) : null;
  function w(b) {
    v();
    try {
      a(b);
    } catch (T) {
      (u.f & Ar) === 0 && fs(T, u);
    }
    d?.deactivate(), Sa();
  }
  if (r.length === 0) {
    h.then(() => w(t.map(o)));
    return;
  }
  function g() {
    v(), Promise.all(r.map((b) => /* @__PURE__ */ ol(b))).then((b) => w([...t.map(o), ...b])).catch((b) => fs(b, u));
  }
  h ? h.then(g) : g();
}
function il() {
  var e = ct, t = ot, r = mt, a = dt;
  return function(c = !0) {
    mr(e), nr(t), vs(r), c && a?.activate();
  };
}
function Sa() {
  mr(null), nr(null), vs(null);
}
// @__NO_SIDE_EFFECTS__
function Fs(e) {
  var t = Lt | Nt, r = ot !== null && (ot.f & Lt) !== 0 ? (
    /** @type {Derived} */
    ot
  ) : null;
  return ct !== null && (ct.f |= ts), {
    ctx: mt,
    deps: null,
    effects: null,
    equals: Xn,
    f: t,
    fn: e,
    reactions: null,
    rv: 0,
    v: (
      /** @type {V} */
      It
    ),
    wv: 0,
    parent: r ?? ct,
    ac: null
  };
}
// @__NO_SIDE_EFFECTS__
function ol(e, t, r) {
  let a = (
    /** @type {Effect | null} */
    ct
  );
  a === null && Oo();
  var o = (
    /** @type {Boundary} */
    a.b
  ), c = (
    /** @type {Promise<V>} */
    /** @type {unknown} */
    void 0
  ), d = Xr(
    /** @type {V} */
    It
  ), u = !ot, v = /* @__PURE__ */ new Map();
  return xl(() => {
    var h = Un();
    c = h.promise;
    try {
      Promise.resolve(e()).then(h.resolve, h.reject).then(() => {
        w === dt && w.committed && w.deactivate(), Sa();
      });
    } catch (T) {
      h.reject(T), Sa();
    }
    var w = (
      /** @type {Batch} */
      dt
    );
    if (u) {
      var g = o.is_rendered();
      o.update_pending_count(1), w.increment(g), v.get(w)?.reject(is), v.delete(w), v.set(w, h);
    }
    const b = (T, S = void 0) => {
      if (w.activate(), S)
        S !== is && (d.f |= Nr, ps(d, S));
      else {
        (d.f & Nr) !== 0 && (d.f ^= Nr), ps(d, T);
        for (const [W, $] of v) {
          if (v.delete(W), W === w) break;
          $.reject(is);
        }
      }
      u && (o.update_pending_count(-1), w.decrement(g));
    };
    h.promise.then(b, (T) => b(null, T || "unknown"));
  }), la(() => {
    for (const h of v.values())
      h.reject(is);
  }), new Promise((h) => {
    function w(g) {
      function b() {
        g === c ? h(d) : w(c);
      }
      g.then(b, b);
    }
    w(c);
  });
}
// @__NO_SIDE_EFFECTS__
function Te(e) {
  const t = /* @__PURE__ */ Fs(e);
  return Ai(t), t;
}
// @__NO_SIDE_EFFECTS__
function _r(e) {
  const t = /* @__PURE__ */ Fs(e);
  return t.equals = ei, t;
}
function ci(e) {
  var t = e.effects;
  if (t !== null) {
    e.effects = null;
    for (var r = 0; r < t.length; r += 1)
      Bt(
        /** @type {Effect} */
        t[r]
      );
  }
}
function ll(e) {
  for (var t = e.parent; t !== null; ) {
    if ((t.f & Lt) === 0)
      return (t.f & Ar) === 0 ? (
        /** @type {Effect} */
        t
      ) : null;
    t = t.parent;
  }
  return null;
}
function Za(e) {
  var t, r = ct;
  mr(ll(e));
  try {
    e.f &= ~Qr, ci(e), t = Ri(e);
  } finally {
    mr(r);
  }
  return t;
}
function di(e) {
  var t = Za(e);
  if (!e.equals(t) && (e.wv = Pi(), (!dt?.is_fork || e.deps === null) && (e.v = t, e.deps === null))) {
    yt(e, Tt);
    return;
  }
  Vr || (or !== null ? (sn() || dt?.is_fork) && or.set(e, t) : Xa(e));
}
let Ea = /* @__PURE__ */ new Set();
const Fr = /* @__PURE__ */ new Map();
let ui = !1;
function Xr(e, t) {
  var r = {
    f: 0,
    // TODO ideally we could skip this altogether, but it causes type errors
    v: e,
    reactions: null,
    equals: Xn,
    rv: 0,
    wv: 0
  };
  return r;
}
// @__NO_SIDE_EFFECTS__
function se(e, t) {
  const r = Xr(e);
  return Ai(r), r;
}
// @__NO_SIDE_EFFECTS__
function en(e, t = !1, r = !0) {
  const a = Xr(e);
  return t || (a.equals = ei), gs && r && mt !== null && mt.l !== null && (mt.l.s ??= []).push(a), a;
}
function x(e, t, r = !1) {
  ot !== null && // since we are untracking the function inside `$inspect.with` we need to add this check
  // to ensure we error if state is set inside an inspect effect
  (!cr || (ot.f & $n) !== 0) && Os() && (ot.f & (Lt | Rr | Qa | $n)) !== 0 && (ar === null || !cs.call(ar, e)) && Uo();
  let a = r ? Ut(t) : t;
  return ps(e, a);
}
function ps(e, t) {
  if (!e.equals(t)) {
    var r = e.v;
    Vr ? Fr.set(e, t) : Fr.set(e, r), e.v = t;
    var a = Dr.ensure();
    if (a.capture(e, r), (e.f & Lt) !== 0) {
      const o = (
        /** @type {Derived} */
        e
      );
      (e.f & Nt) !== 0 && Za(o), Xa(o);
    }
    e.wv = Pi(), vi(e, Nt), Os() && ct !== null && (ct.f & Tt) !== 0 && (ct.f & (ur | qr)) === 0 && (Zt === null ? _l([e]) : Zt.push(e)), !a.is_fork && Ea.size > 0 && !ui && cl();
  }
  return t;
}
function cl() {
  ui = !1;
  for (const e of Ea)
    (e.f & Tt) !== 0 && yt(e, dr), Vs(e) && Ms(e);
  Ea.clear();
}
function Es(e) {
  x(e, e.v + 1);
}
function vi(e, t) {
  var r = e.reactions;
  if (r !== null)
    for (var a = Os(), o = r.length, c = 0; c < o; c++) {
      var d = r[c], u = d.f;
      if (!(!a && d === ct)) {
        var v = (u & Nt) === 0;
        if (v && yt(d, t), (u & Lt) !== 0) {
          var h = (
            /** @type {Derived} */
            d
          );
          or?.delete(h), (u & Qr) === 0 && (u & rr && (d.f |= Qr), vi(h, dr));
        } else v && ((u & Rr) !== 0 && kr !== null && kr.add(
          /** @type {Effect} */
          d
        ), lr(
          /** @type {Effect} */
          d
        ));
      }
    }
}
function Ut(e) {
  if (typeof e != "object" || e === null || Or in e)
    return e;
  const t = Ka(e);
  if (t !== Ro && t !== Mo)
    return e;
  var r = /* @__PURE__ */ new Map(), a = Ya(e), o = /* @__PURE__ */ se(0), c = Yr, d = (u) => {
    if (Yr === c)
      return u();
    var v = ot, h = Yr;
    nr(null), Pn(c);
    var w = u();
    return nr(v), Pn(h), w;
  };
  return a && r.set("length", /* @__PURE__ */ se(
    /** @type {any[]} */
    e.length
  )), new Proxy(
    /** @type {any} */
    e,
    {
      defineProperty(u, v, h) {
        (!("value" in h) || h.configurable === !1 || h.enumerable === !1 || h.writable === !1) && Go();
        var w = r.get(v);
        return w === void 0 ? d(() => {
          var g = /* @__PURE__ */ se(h.value);
          return r.set(v, g), g;
        }) : x(w, h.value, !0), !0;
      },
      deleteProperty(u, v) {
        var h = r.get(v);
        if (h === void 0) {
          if (v in u) {
            const w = d(() => /* @__PURE__ */ se(It));
            r.set(v, w), Es(o);
          }
        } else
          x(h, It), Es(o);
        return !0;
      },
      get(u, v, h) {
        if (v === Or)
          return e;
        var w = r.get(v), g = v in u;
        if (w === void 0 && (!g || os(u, v)?.writable) && (w = d(() => {
          var T = Ut(g ? u[v] : It), S = /* @__PURE__ */ se(T);
          return S;
        }), r.set(v, w)), w !== void 0) {
          var b = s(w);
          return b === It ? void 0 : b;
        }
        return Reflect.get(u, v, h);
      },
      getOwnPropertyDescriptor(u, v) {
        var h = Reflect.getOwnPropertyDescriptor(u, v);
        if (h && "value" in h) {
          var w = r.get(v);
          w && (h.value = s(w));
        } else if (h === void 0) {
          var g = r.get(v), b = g?.v;
          if (g !== void 0 && b !== It)
            return {
              enumerable: !0,
              configurable: !0,
              value: b,
              writable: !0
            };
        }
        return h;
      },
      has(u, v) {
        if (v === Or)
          return !0;
        var h = r.get(v), w = h !== void 0 && h.v !== It || Reflect.has(u, v);
        if (h !== void 0 || ct !== null && (!w || os(u, v)?.writable)) {
          h === void 0 && (h = d(() => {
            var b = w ? Ut(u[v]) : It, T = /* @__PURE__ */ se(b);
            return T;
          }), r.set(v, h));
          var g = s(h);
          if (g === It)
            return !1;
        }
        return w;
      },
      set(u, v, h, w) {
        var g = r.get(v), b = v in u;
        if (a && v === "length")
          for (var T = h; T < /** @type {Source<number>} */
          g.v; T += 1) {
            var S = r.get(T + "");
            S !== void 0 ? x(S, It) : T in u && (S = d(() => /* @__PURE__ */ se(It)), r.set(T + "", S));
          }
        if (g === void 0)
          (!b || os(u, v)?.writable) && (g = d(() => /* @__PURE__ */ se(void 0)), x(g, Ut(h)), r.set(v, g));
        else {
          b = g.v !== It;
          var W = d(() => Ut(h));
          x(g, W);
        }
        var $ = Reflect.getOwnPropertyDescriptor(u, v);
        if ($?.set && $.set.call(w, h), !b) {
          if (a && typeof v == "string") {
            var R = (
              /** @type {Source<number>} */
              r.get("length")
            ), K = Number(v);
            Number.isInteger(K) && K >= R.v && x(R, K + 1);
          }
          Es(o);
        }
        return !0;
      },
      ownKeys(u) {
        s(o);
        var v = Reflect.ownKeys(u).filter((g) => {
          var b = r.get(g);
          return b === void 0 || b.v !== It;
        });
        for (var [h, w] of r)
          w.v !== It && !(h in u) && v.push(h);
        return v;
      },
      setPrototypeOf() {
        Wo();
      }
    }
  );
}
function Sn(e) {
  try {
    if (e !== null && typeof e == "object" && Or in e)
      return e[Or];
  } catch {
  }
  return e;
}
function dl(e, t) {
  return Object.is(Sn(e), Sn(t));
}
var En, fi, pi, xi;
function Aa() {
  if (En === void 0) {
    En = window, fi = /Firefox/.test(navigator.userAgent);
    var e = Element.prototype, t = Node.prototype, r = Text.prototype;
    pi = os(t, "firstChild").get, xi = os(t, "nextSibling").get, kn(e) && (e.__click = void 0, e.__className = void 0, e.__attributes = null, e.__style = void 0, e.__e = void 0), kn(r) && (r.__t = void 0);
  }
}
function sr(e = "") {
  return document.createTextNode(e);
}
// @__NO_SIDE_EFFECTS__
function tr(e) {
  return (
    /** @type {TemplateNode | null} */
    pi.call(e)
  );
}
// @__NO_SIDE_EFFECTS__
function vr(e) {
  return (
    /** @type {TemplateNode | null} */
    xi.call(e)
  );
}
function i(e, t) {
  if (!it)
    return /* @__PURE__ */ tr(e);
  var r = /* @__PURE__ */ tr(lt);
  if (r === null)
    r = lt.appendChild(sr());
  else if (t && r.nodeType !== na) {
    var a = sr();
    return r?.before(a), Vt(a), a;
  }
  return t && rn(
    /** @type {Text} */
    r
  ), Vt(r), r;
}
function ut(e, t = !1) {
  if (!it) {
    var r = /* @__PURE__ */ tr(e);
    return r instanceof Comment && r.data === "" ? /* @__PURE__ */ vr(r) : r;
  }
  if (t) {
    if (lt?.nodeType !== na) {
      var a = sr();
      return lt?.before(a), Vt(a), a;
    }
    rn(
      /** @type {Text} */
      lt
    );
  }
  return lt;
}
function l(e, t = 1, r = !1) {
  let a = it ? lt : e;
  for (var o; t--; )
    o = a, a = /** @type {TemplateNode} */
    /* @__PURE__ */ vr(a);
  if (!it)
    return a;
  if (r) {
    if (a?.nodeType !== na) {
      var c = sr();
      return a === null ? o?.after(c) : a.before(c), Vt(c), c;
    }
    rn(
      /** @type {Text} */
      a
    );
  }
  return Vt(a), a;
}
function tn(e) {
  e.textContent = "";
}
function hi() {
  return !1;
}
function rn(e) {
  if (
    /** @type {string} */
    e.nodeValue.length < 65536
  )
    return;
  let t = e.nextSibling;
  for (; t !== null && t.nodeType === na; )
    t.remove(), e.nodeValue += /** @type {string} */
    t.nodeValue, t = e.nextSibling;
}
function js(e) {
  it && /* @__PURE__ */ tr(e) !== null && tn(e);
}
let An = !1;
function _i() {
  An || (An = !0, document.addEventListener(
    "reset",
    (e) => {
      Promise.resolve().then(() => {
        if (!e.defaultPrevented)
          for (
            const t of
            /**@type {HTMLFormElement} */
            e.target.elements
          )
            t.__on_r?.();
      });
    },
    // In the capture phase to guarantee we get noticed of it (no possibility of stopPropagation)
    { capture: !0 }
  ));
}
function oa(e) {
  var t = ot, r = ct;
  nr(null), mr(null);
  try {
    return e();
  } finally {
    nr(t), mr(r);
  }
}
function gi(e, t, r, a = r) {
  e.addEventListener(t, () => oa(r));
  const o = e.__on_r;
  o ? e.__on_r = () => {
    o(), a(!0);
  } : e.__on_r = () => a(!0), _i();
}
function bi(e) {
  ct === null && (ot === null && Bo(), Vo()), Vr && jo();
}
function ul(e, t) {
  var r = t.last;
  r === null ? t.last = t.first = e : (r.next = e, e.prev = r, t.last = e);
}
function fr(e, t, r) {
  var a = ct;
  a !== null && (a.f & Qt) !== 0 && (e |= Qt);
  var o = {
    ctx: mt,
    deps: null,
    nodes: null,
    f: e | Nt | rr,
    first: null,
    fn: t,
    last: null,
    next: null,
    parent: a,
    b: a && a.b,
    prev: null,
    teardown: null,
    wv: 0,
    ac: null
  };
  if (r)
    try {
      Ms(o), o.f |= aa;
    } catch (u) {
      throw Bt(o), u;
    }
  else t !== null && lr(o);
  var c = o;
  if (r && c.deps === null && c.teardown === null && c.nodes === null && c.first === c.last && // either `null`, or a singular child
  (c.f & ts) === 0 && (c = c.first, (e & Rr) !== 0 && (e & ds) !== 0 && c !== null && (c.f |= ds)), c !== null && (c.parent = a, a !== null && ul(c, a), ot !== null && (ot.f & Lt) !== 0 && (e & qr) === 0)) {
    var d = (
      /** @type {Derived} */
      ot
    );
    (d.effects ??= []).push(c);
  }
  return o;
}
function sn() {
  return ot !== null && !cr;
}
function la(e) {
  const t = fr(Ls, null, !1);
  return yt(t, Tt), t.teardown = e, t;
}
function Pt(e) {
  bi();
  var t = (
    /** @type {Effect} */
    ct.f
  ), r = !ot && (t & ur) !== 0 && (t & aa) === 0;
  if (r) {
    var a = (
      /** @type {ComponentContext} */
      mt
    );
    (a.e ??= []).push(e);
  } else
    return mi(e);
}
function mi(e) {
  return fr(Qs | Kn, e, !1);
}
function vl(e) {
  return bi(), fr(Ls | Kn, e, !0);
}
function fl(e) {
  Dr.ensure();
  const t = fr(qr | ts, e, !0);
  return () => {
    Bt(t);
  };
}
function pl(e) {
  Dr.ensure();
  const t = fr(qr | ts, e, !0);
  return (r = {}) => new Promise((a) => {
    r.outro ? zr(t, () => {
      Bt(t), a(void 0);
    }) : (Bt(t), a(void 0));
  });
}
function an(e) {
  return fr(Qs, e, !1);
}
function xl(e) {
  return fr(Qa | ts, e, !0);
}
function ca(e, t = 0) {
  return fr(Ls | t, e, !0);
}
function C(e, t = [], r = [], a = []) {
  nl(a, t, r, (o) => {
    fr(Ls, () => e(...o.map(s)), !0);
  });
}
function nn(e, t = 0) {
  var r = fr(Rr | t, e, !0);
  return r;
}
function er(e) {
  return fr(ur | ts, e, !0);
}
function yi(e) {
  var t = e.teardown;
  if (t !== null) {
    const r = Vr, a = ot;
    Dn(!0), nr(null);
    try {
      t.call(null);
    } finally {
      Dn(r), nr(a);
    }
  }
}
function wi(e, t = !1) {
  var r = e.first;
  for (e.first = e.last = null; r !== null; ) {
    const o = r.ac;
    o !== null && oa(() => {
      o.abort(is);
    });
    var a = r.next;
    (r.f & qr) !== 0 ? r.parent = null : Bt(r, t), r = a;
  }
}
function hl(e) {
  for (var t = e.first; t !== null; ) {
    var r = t.next;
    (t.f & ur) === 0 && Bt(t), t = r;
  }
}
function Bt(e, t = !0) {
  var r = !1;
  (t || (e.f & Yn) !== 0) && e.nodes !== null && e.nodes.end !== null && (ki(
    e.nodes.start,
    /** @type {TemplateNode} */
    e.nodes.end
  ), r = !0), wi(e, t && !r), ea(e, 0), yt(e, Ar);
  var a = e.nodes && e.nodes.t;
  if (a !== null)
    for (const c of a)
      c.stop();
  yi(e);
  var o = e.parent;
  o !== null && o.first !== null && $i(e), e.next = e.prev = e.teardown = e.ctx = e.deps = e.fn = e.nodes = e.ac = null;
}
function ki(e, t) {
  for (; e !== null; ) {
    var r = e === t ? null : /* @__PURE__ */ vr(e);
    e.remove(), e = r;
  }
}
function $i(e) {
  var t = e.parent, r = e.prev, a = e.next;
  r !== null && (r.next = a), a !== null && (a.prev = r), t !== null && (t.first === e && (t.first = a), t.last === e && (t.last = r));
}
function zr(e, t, r = !0) {
  var a = [];
  Ci(e, a, !0);
  var o = () => {
    r && Bt(e), t && t();
  }, c = a.length;
  if (c > 0) {
    var d = () => --c || o();
    for (var u of a)
      u.out(d);
  } else
    o();
}
function Ci(e, t, r) {
  if ((e.f & Qt) === 0) {
    e.f ^= Qt;
    var a = e.nodes && e.nodes.t;
    if (a !== null)
      for (const u of a)
        (u.is_global || r) && t.push(u);
    for (var o = e.first; o !== null; ) {
      var c = o.next, d = (o.f & ds) !== 0 || // If this is a branch effect without a block effect parent,
      // it means the parent block effect was pruned. In that case,
      // transparency information was transferred to the branch effect.
      (o.f & ur) !== 0 && (e.f & Rr) !== 0;
      Ci(o, t, d ? r : !1), o = c;
    }
  }
}
function on(e) {
  Si(e, !0);
}
function Si(e, t) {
  if ((e.f & Qt) !== 0) {
    e.f ^= Qt, (e.f & Tt) === 0 && (yt(e, Nt), lr(e));
    for (var r = e.first; r !== null; ) {
      var a = r.next, o = (r.f & ds) !== 0 || (r.f & ur) !== 0;
      Si(r, o ? t : !1), r = a;
    }
    var c = e.nodes && e.nodes.t;
    if (c !== null)
      for (const d of c)
        (d.is_global || t) && d.in();
  }
}
function Ei(e, t) {
  if (e.nodes)
    for (var r = e.nodes.start, a = e.nodes.end; r !== null; ) {
      var o = r === a ? null : /* @__PURE__ */ vr(r);
      t.append(r), r = o;
    }
}
let Hs = !1, Vr = !1;
function Dn(e) {
  Vr = e;
}
let ot = null, cr = !1;
function nr(e) {
  ot = e;
}
let ct = null;
function mr(e) {
  ct = e;
}
let ar = null;
function Ai(e) {
  ot !== null && (ar === null ? ar = [e] : ar.push(e));
}
let Wt = null, Kt = 0, Zt = null;
function _l(e) {
  Zt = e;
}
let Di = 1, Wr = 0, Yr = Wr;
function Pn(e) {
  Yr = e;
}
function Pi() {
  return ++Di;
}
function Vs(e) {
  var t = e.f;
  if ((t & Nt) !== 0)
    return !0;
  if (t & Lt && (e.f &= ~Qr), (t & dr) !== 0) {
    for (var r = (
      /** @type {Value[]} */
      e.deps
    ), a = r.length, o = 0; o < a; o++) {
      var c = r[o];
      if (Vs(
        /** @type {Derived} */
        c
      ) && di(
        /** @type {Derived} */
        c
      ), c.wv > e.wv)
        return !0;
    }
    (t & rr) !== 0 && // During time traveling we don't want to reset the status so that
    // traversal of the graph in the other batches still happens
    or === null && yt(e, Tt);
  }
  return !1;
}
function Ti(e, t, r = !0) {
  var a = e.reactions;
  if (a !== null && !(ar !== null && cs.call(ar, e)))
    for (var o = 0; o < a.length; o++) {
      var c = a[o];
      (c.f & Lt) !== 0 ? Ti(
        /** @type {Derived} */
        c,
        t,
        !1
      ) : t === c && (r ? yt(c, Nt) : (c.f & Tt) !== 0 && yt(c, dr), lr(
        /** @type {Effect} */
        c
      ));
    }
}
function Ri(e) {
  var t = Wt, r = Kt, a = Zt, o = ot, c = ar, d = mt, u = cr, v = Yr, h = e.f;
  Wt = /** @type {null | Value[]} */
  null, Kt = 0, Zt = null, ot = (h & (ur | qr)) === 0 ? e : null, ar = null, vs(e.ctx), cr = !1, Yr = ++Wr, e.ac !== null && (oa(() => {
    e.ac.abort(is);
  }), e.ac = null);
  try {
    e.f |= $a;
    var w = (
      /** @type {Function} */
      e.fn
    ), g = w(), b = e.deps, T = dt?.is_fork;
    if (Wt !== null) {
      var S;
      if (T || ea(e, Kt), b !== null && Kt > 0)
        for (b.length = Kt + Wt.length, S = 0; S < Wt.length; S++)
          b[Kt + S] = Wt[S];
      else
        e.deps = b = Wt;
      if (sn() && (e.f & rr) !== 0)
        for (S = Kt; S < b.length; S++)
          (b[S].reactions ??= []).push(e);
    } else !T && b !== null && Kt < b.length && (ea(e, Kt), b.length = Kt);
    if (Os() && Zt !== null && !cr && b !== null && (e.f & (Lt | dr | Nt)) === 0)
      for (S = 0; S < /** @type {Source[]} */
      Zt.length; S++)
        Ti(
          Zt[S],
          /** @type {Effect} */
          e
        );
    if (o !== null && o !== e) {
      if (Wr++, o.deps !== null)
        for (let W = 0; W < r; W += 1)
          o.deps[W].rv = Wr;
      if (t !== null)
        for (const W of t)
          W.rv = Wr;
      Zt !== null && (a === null ? a = Zt : a.push(.../** @type {Source[]} */
      Zt));
    }
    return (e.f & Nr) !== 0 && (e.f ^= Nr), g;
  } catch (W) {
    return ri(W);
  } finally {
    e.f ^= $a, Wt = t, Kt = r, Zt = a, ot = o, ar = c, vs(d), cr = u, Yr = v;
  }
}
function gl(e, t) {
  let r = t.reactions;
  if (r !== null) {
    var a = To.call(r, e);
    if (a !== -1) {
      var o = r.length - 1;
      o === 0 ? r = t.reactions = null : (r[a] = r[o], r.pop());
    }
  }
  if (r === null && (t.f & Lt) !== 0 && // Destroying a child effect while updating a parent effect can cause a dependency to appear
  // to be unused, when in fact it is used by the currently-updating parent. Checking `new_deps`
  // allows us to skip the expensive work of disconnecting and immediately reconnecting it
  (Wt === null || !cs.call(Wt, t))) {
    var c = (
      /** @type {Derived} */
      t
    );
    (c.f & rr) !== 0 && (c.f ^= rr, c.f &= ~Qr), Xa(c), ci(c), ea(c, 0);
  }
}
function ea(e, t) {
  var r = e.deps;
  if (r !== null)
    for (var a = t; a < r.length; a++)
      gl(e, r[a]);
}
function Ms(e) {
  var t = e.f;
  if ((t & Ar) === 0) {
    yt(e, Tt);
    var r = ct, a = Hs;
    ct = e, Hs = !0;
    try {
      (t & (Rr | zn)) !== 0 ? hl(e) : wi(e), yi(e);
      var o = Ri(e);
      e.teardown = typeof o == "function" ? o : null, e.wv = Di;
      var c;
      wa && Jo && (e.f & Nt) !== 0 && e.deps;
    } finally {
      Hs = a, ct = r;
    }
  }
}
async function ln() {
  await Promise.resolve(), vt();
}
function s(e) {
  var t = e.f, r = (t & Lt) !== 0;
  if (ot !== null && !cr) {
    var a = ct !== null && (ct.f & Ar) !== 0;
    if (!a && (ar === null || !cs.call(ar, e))) {
      var o = ot.deps;
      if ((ot.f & $a) !== 0)
        e.rv < Wr && (e.rv = Wr, Wt === null && o !== null && o[Kt] === e ? Kt++ : Wt === null ? Wt = [e] : Wt.push(e));
      else {
        (ot.deps ??= []).push(e);
        var c = e.reactions;
        c === null ? e.reactions = [ot] : cs.call(c, ot) || c.push(ot);
      }
    }
  }
  if (Vr && Fr.has(e))
    return Fr.get(e);
  if (r) {
    var d = (
      /** @type {Derived} */
      e
    );
    if (Vr) {
      var u = d.v;
      return ((d.f & Tt) === 0 && d.reactions !== null || Ii(d)) && (u = Za(d)), Fr.set(d, u), u;
    }
    var v = (d.f & rr) === 0 && !cr && ot !== null && (Hs || (ot.f & rr) !== 0), h = d.deps === null;
    Vs(d) && (v && (d.f |= rr), di(d)), v && !h && Mi(d);
  }
  if (or?.has(e))
    return or.get(e);
  if ((e.f & Nr) !== 0)
    throw e.v;
  return e.v;
}
function Mi(e) {
  if (e.deps !== null) {
    e.f |= rr;
    for (const t of e.deps)
      (t.reactions ??= []).push(e), (t.f & Lt) !== 0 && (t.f & rr) === 0 && Mi(
        /** @type {Derived} */
        t
      );
  }
}
function Ii(e) {
  if (e.v === It) return !0;
  if (e.deps === null) return !1;
  for (const t of e.deps)
    if (Fr.has(t) || (t.f & Lt) !== 0 && Ii(
      /** @type {Derived} */
      t
    ))
      return !0;
  return !1;
}
function ss(e) {
  var t = cr;
  try {
    return cr = !0, e();
  } finally {
    cr = t;
  }
}
function bl(e) {
  if (!(typeof e != "object" || !e || e instanceof EventTarget)) {
    if (Or in e)
      Da(e);
    else if (!Array.isArray(e))
      for (let t in e) {
        const r = e[t];
        typeof r == "object" && r && Or in r && Da(r);
      }
  }
}
function Da(e, t = /* @__PURE__ */ new Set()) {
  if (typeof e == "object" && e !== null && // We don't want to traverse DOM elements
  !(e instanceof EventTarget) && !t.has(e)) {
    t.add(e), e instanceof Date && e.getTime();
    for (let a in e)
      try {
        Da(e[a], t);
      } catch {
      }
    const r = Ka(e);
    if (r !== Object.prototype && r !== Array.prototype && r !== Map.prototype && r !== Set.prototype && r !== Date.prototype) {
      const a = Wn(r);
      for (let o in a) {
        const c = a[o].get;
        if (c)
          try {
            c.call(e);
          } catch {
          }
      }
    }
  }
}
const Li = /* @__PURE__ */ new Set(), Pa = /* @__PURE__ */ new Set();
function ml(e, t, r, a = {}) {
  function o(c) {
    if (a.capture || $s.call(t, c), !c.cancelBubble)
      return oa(() => r?.call(this, c));
  }
  return e.startsWith("pointer") || e.startsWith("touch") || e === "wheel" ? gr(() => {
    t.addEventListener(e, o, a);
  }) : t.addEventListener(e, o, a), o;
}
function Mt(e, t, r, a, o) {
  var c = { capture: a, passive: o }, d = ml(e, t, r, c);
  (t === document.body || // @ts-ignore
  t === window || // @ts-ignore
  t === document || // Firefox has quirky behavior, it can happen that we still get "canplay" events when the element is already removed
  t instanceof HTMLMediaElement) && la(() => {
    t.removeEventListener(e, d, c);
  });
}
function Ot(e) {
  for (var t = 0; t < e.length; t++)
    Li.add(e[t]);
  for (var r of Pa)
    r(e);
}
let Tn = null;
function $s(e) {
  var t = this, r = (
    /** @type {Node} */
    t.ownerDocument
  ), a = e.type, o = e.composedPath?.() || [], c = (
    /** @type {null | Element} */
    o[0] || e.target
  );
  Tn = e;
  var d = 0, u = Tn === e && e.__root;
  if (u) {
    var v = o.indexOf(u);
    if (v !== -1 && (t === document || t === /** @type {any} */
    window)) {
      e.__root = t;
      return;
    }
    var h = o.indexOf(t);
    if (h === -1)
      return;
    v <= h && (d = v);
  }
  if (c = /** @type {Element} */
  o[d] || e.target, c !== t) {
    Rs(e, "currentTarget", {
      configurable: !0,
      get() {
        return c || r;
      }
    });
    var w = ot, g = ct;
    nr(null), mr(null);
    try {
      for (var b, T = []; c !== null; ) {
        var S = c.assignedSlot || c.parentNode || /** @type {any} */
        c.host || null;
        try {
          var W = c["__" + a];
          W != null && (!/** @type {any} */
          c.disabled || // DOM could've been updated already by the time this is reached, so we check this as well
          // -> the target could not have been disabled because it emits the event in the first place
          e.target === c) && W.call(c, e);
        } catch ($) {
          b ? T.push($) : b = $;
        }
        if (e.cancelBubble || S === t || S === null)
          break;
        c = S;
      }
      if (b) {
        for (let $ of T)
          queueMicrotask(() => {
            throw $;
          });
        throw b;
      }
    } finally {
      e.__root = t, delete e.currentTarget, nr(w), mr(g);
    }
  }
}
function Ni(e) {
  var t = document.createElement("template");
  return t.innerHTML = e.replaceAll("<!>", "<!---->"), t.content;
}
function jr(e, t) {
  var r = (
    /** @type {Effect} */
    ct
  );
  r.nodes === null && (r.nodes = { start: e, end: t, a: null, t: null });
}
// @__NO_SIDE_EFFECTS__
function p(e, t) {
  var r = (t & Ao) !== 0, a = (t & Do) !== 0, o, c = !e.startsWith("<!>");
  return () => {
    if (it)
      return jr(lt, null), lt;
    o === void 0 && (o = Ni(c ? e : "<!>" + e), r || (o = /** @type {TemplateNode} */
    /* @__PURE__ */ tr(o)));
    var d = (
      /** @type {TemplateNode} */
      a || fi ? document.importNode(o, !0) : o.cloneNode(!0)
    );
    if (r) {
      var u = (
        /** @type {TemplateNode} */
        /* @__PURE__ */ tr(d)
      ), v = (
        /** @type {TemplateNode} */
        d.lastChild
      );
      jr(u, v);
    } else
      jr(d, d);
    return d;
  };
}
function br() {
  if (it)
    return jr(lt, null), lt;
  var e = document.createDocumentFragment(), t = document.createComment(""), r = sr();
  return e.append(t, r), jr(t, r), e;
}
function f(e, t) {
  if (it) {
    var r = (
      /** @type {Effect & { nodes: EffectNodes }} */
      ct
    );
    ((r.f & aa) === 0 || r.nodes.end === null) && (r.nodes.end = lt), us();
    return;
  }
  e !== null && e.before(
    /** @type {Node} */
    t
  );
}
const yl = ["touchstart", "touchmove"];
function wl(e) {
  return yl.includes(e);
}
function _(e, t) {
  var r = t == null ? "" : typeof t == "object" ? t + "" : t;
  r !== (e.__t ??= e.nodeValue) && (e.__t = r, e.nodeValue = r + "");
}
function Oi(e, t) {
  return Fi(e, t);
}
function kl(e, t) {
  Aa(), t.intro = t.intro ?? !1;
  const r = t.target, a = it, o = lt;
  try {
    for (var c = /* @__PURE__ */ tr(r); c && (c.nodeType !== rs || /** @type {Comment} */
    c.data !== Ua); )
      c = /* @__PURE__ */ vr(c);
    if (!c)
      throw Jr;
    Sr(!0), Vt(
      /** @type {Comment} */
      c
    );
    const d = Fi(e, { ...t, anchor: c });
    return Sr(!1), /**  @type {Exports} */
    d;
  } catch (d) {
    if (d instanceof Error && d.message.split(`
`).some((u) => u.startsWith("https://svelte.dev/e/")))
      throw d;
    return d !== Jr && console.warn("Failed to hydrate: ", d), t.recover === !1 && Ho(), Aa(), tn(r), Sr(!1), Oi(e, t);
  } finally {
    Sr(a), Vt(o);
  }
}
const as = /* @__PURE__ */ new Map();
function Fi(e, { target: t, anchor: r, props: a = {}, events: o, context: c, intro: d = !0 }) {
  Aa();
  var u = /* @__PURE__ */ new Set(), v = (g) => {
    for (var b = 0; b < g.length; b++) {
      var T = g[b];
      if (!u.has(T)) {
        u.add(T);
        var S = wl(T);
        t.addEventListener(T, $s, { passive: S });
        var W = as.get(T);
        W === void 0 ? (document.addEventListener(T, $s, { passive: S }), as.set(T, 1)) : as.set(T, W + 1);
      }
    }
  };
  v(sa(Li)), Pa.add(v);
  var h = void 0, w = pl(() => {
    var g = r ?? t.appendChild(sr());
    return sl(
      /** @type {TemplateNode} */
      g,
      {
        pending: () => {
        }
      },
      (b) => {
        Ct({});
        var T = (
          /** @type {ComponentContext} */
          mt
        );
        if (c && (T.c = c), o && (a.$$events = o), it && jr(
          /** @type {TemplateNode} */
          b,
          null
        ), h = e(b, a) || {}, it && (ct.nodes.end = lt, lt === null || lt.nodeType !== rs || /** @type {Comment} */
        lt.data !== za))
          throw Ns(), Jr;
        St();
      }
    ), () => {
      for (var b of u) {
        t.removeEventListener(b, $s);
        var T = (
          /** @type {number} */
          as.get(b)
        );
        --T === 0 ? (document.removeEventListener(b, $s), as.delete(b)) : as.set(b, T);
      }
      Pa.delete(v), g !== r && g.parentNode?.removeChild(g);
    };
  });
  return Ta.set(h, w), h;
}
let Ta = /* @__PURE__ */ new WeakMap();
function $l(e, t) {
  const r = Ta.get(e);
  return r ? (Ta.delete(e), r(t)) : Promise.resolve();
}
class Cl {
  /** @type {TemplateNode} */
  anchor;
  /** @type {Map<Batch, Key>} */
  #e = /* @__PURE__ */ new Map();
  /**
   * Map of keys to effects that are currently rendered in the DOM.
   * These effects are visible and actively part of the document tree.
   * Example:
   * ```
   * {#if condition}
   * 	foo
   * {:else}
   * 	bar
   * {/if}
   * ```
   * Can result in the entries `true->Effect` and `false->Effect`
   * @type {Map<Key, Effect>}
   */
  #t = /* @__PURE__ */ new Map();
  /**
   * Similar to #onscreen with respect to the keys, but contains branches that are not yet
   * in the DOM, because their insertion is deferred.
   * @type {Map<Key, Branch>}
   */
  #r = /* @__PURE__ */ new Map();
  /**
   * Keys of effects that are currently outroing
   * @type {Set<Key>}
   */
  #i = /* @__PURE__ */ new Set();
  /**
   * Whether to pause (i.e. outro) on change, or destroy immediately.
   * This is necessary for `<svelte:element>`
   */
  #o = !0;
  /**
   * @param {TemplateNode} anchor
   * @param {boolean} transition
   */
  constructor(t, r = !0) {
    this.anchor = t, this.#o = r;
  }
  #a = () => {
    var t = (
      /** @type {Batch} */
      dt
    );
    if (this.#e.has(t)) {
      var r = (
        /** @type {Key} */
        this.#e.get(t)
      ), a = this.#t.get(r);
      if (a)
        on(a), this.#i.delete(r);
      else {
        var o = this.#r.get(r);
        o && (this.#t.set(r, o.effect), this.#r.delete(r), o.fragment.lastChild.remove(), this.anchor.before(o.fragment), a = o.effect);
      }
      for (const [c, d] of this.#e) {
        if (this.#e.delete(c), c === t)
          break;
        const u = this.#r.get(d);
        u && (Bt(u.effect), this.#r.delete(d));
      }
      for (const [c, d] of this.#t) {
        if (c === r || this.#i.has(c)) continue;
        const u = () => {
          if (Array.from(this.#e.values()).includes(c)) {
            var h = document.createDocumentFragment();
            Ei(d, h), h.append(sr()), this.#r.set(c, { effect: d, fragment: h });
          } else
            Bt(d);
          this.#i.delete(c), this.#t.delete(c);
        };
        this.#o || !a ? (this.#i.add(c), zr(d, u, !1)) : u();
      }
    }
  };
  /**
   * @param {Batch} batch
   */
  #s = (t) => {
    this.#e.delete(t);
    const r = Array.from(this.#e.values());
    for (const [a, o] of this.#r)
      r.includes(a) || (Bt(o.effect), this.#r.delete(a));
  };
  /**
   *
   * @param {any} key
   * @param {null | ((target: TemplateNode) => void)} fn
   */
  ensure(t, r) {
    var a = (
      /** @type {Batch} */
      dt
    ), o = hi();
    if (r && !this.#t.has(t) && !this.#r.has(t))
      if (o) {
        var c = document.createDocumentFragment(), d = sr();
        c.append(d), this.#r.set(t, {
          effect: er(() => r(d)),
          fragment: c
        });
      } else
        this.#t.set(
          t,
          er(() => r(this.anchor))
        );
    if (this.#e.set(a, t), o) {
      for (const [u, v] of this.#t)
        u === t ? a.unskip_effect(v) : a.skip_effect(v);
      for (const [u, v] of this.#r)
        u === t ? a.unskip_effect(v.effect) : a.skip_effect(v.effect);
      a.oncommit(this.#a), a.ondiscard(this.#s);
    } else
      it && (this.anchor = lt), this.#a();
  }
}
function ji(e) {
  mt === null && Jn(), gs && mt.l !== null ? El(mt).m.push(e) : Pt(() => {
    const t = ss(e);
    if (typeof t == "function") return (
      /** @type {() => void} */
      t
    );
  });
}
function Sl(e) {
  mt === null && Jn(), ji(() => () => ss(e));
}
function El(e) {
  var t = (
    /** @type {ComponentContextLegacy} */
    e.l
  );
  return t.u ??= { a: [], b: [], m: [] };
}
function P(e, t, r = !1) {
  it && us();
  var a = new Cl(e), o = r ? ds : 0;
  function c(d, u) {
    if (it) {
      const w = Qn(e);
      var v;
      if (w === Ua ? v = 0 : w === ra ? v = !1 : v = parseInt(w.substring(1)), d !== v) {
        var h = Xs();
        Vt(h), a.anchor = h, Sr(!1), a.ensure(d, u), Sr(!0);
        return;
      }
    }
    a.ensure(d, u);
  }
  nn(() => {
    var d = !1;
    t((u, v = 0) => {
      d = !0, c(v, u);
    }), d || c(!1, null);
  }, o);
}
function pt(e, t) {
  return t;
}
function Al(e, t, r) {
  for (var a = [], o = t.length, c, d = t.length, u = 0; u < o; u++) {
    let g = t[u];
    zr(
      g,
      () => {
        if (c) {
          if (c.pending.delete(g), c.done.add(g), c.pending.size === 0) {
            var b = (
              /** @type {Set<EachOutroGroup>} */
              e.outrogroups
            );
            Ra(sa(c.done)), b.delete(c), b.size === 0 && (e.outrogroups = null);
          }
        } else
          d -= 1;
      },
      !1
    );
  }
  if (d === 0) {
    var v = a.length === 0 && r !== null;
    if (v) {
      var h = (
        /** @type {Element} */
        r
      ), w = (
        /** @type {Element} */
        h.parentNode
      );
      tn(w), w.append(h), e.items.clear();
    }
    Ra(t, !v);
  } else
    c = {
      pending: new Set(t),
      done: /* @__PURE__ */ new Set()
    }, (e.outrogroups ??= /* @__PURE__ */ new Set()).add(c);
}
function Ra(e, t = !0) {
  for (var r = 0; r < e.length; r++)
    Bt(e[r], t);
}
var Rn;
function He(e, t, r, a, o, c = null) {
  var d = e, u = /* @__PURE__ */ new Map(), v = (t & Gn) !== 0;
  if (v) {
    var h = (
      /** @type {Element} */
      e
    );
    d = it ? Vt(/* @__PURE__ */ tr(h)) : h.appendChild(sr());
  }
  it && us();
  var w = null, g = /* @__PURE__ */ _r(() => {
    var R = r();
    return Ya(R) ? R : R == null ? [] : sa(R);
  }), b, T = !0;
  function S() {
    $.fallback = w, Dl($, b, d, t, a), w !== null && (b.length === 0 ? (w.f & Cr) === 0 ? on(w) : (w.f ^= Cr, Cs(w, null, d)) : zr(w, () => {
      w = null;
    }));
  }
  var W = nn(() => {
    b = /** @type {V[]} */
    s(g);
    var R = b.length;
    let K = !1;
    if (it) {
      var ge = Qn(d) === ra;
      ge !== (R === 0) && (d = Xs(), Vt(d), Sr(!1), K = !0);
    }
    for (var ie = /* @__PURE__ */ new Set(), ae = (
      /** @type {Batch} */
      dt
    ), Z = hi(), De = 0; De < R; De += 1) {
      it && lt.nodeType === rs && /** @type {Comment} */
      lt.data === za && (d = /** @type {Comment} */
      lt, K = !0, Sr(!1));
      var Ve = b[De], Ge = a(Ve, De), ve = T ? null : u.get(Ge);
      ve ? (ve.v && ps(ve.v, Ve), ve.i && ps(ve.i, De), Z && ae.unskip_effect(ve.e)) : (ve = Pl(
        u,
        T ? d : Rn ??= sr(),
        Ve,
        Ge,
        De,
        o,
        t,
        r
      ), T || (ve.e.f |= Cr), u.set(Ge, ve)), ie.add(Ge);
    }
    if (R === 0 && c && !w && (T ? w = er(() => c(d)) : (w = er(() => c(Rn ??= sr())), w.f |= Cr)), R > ie.size && Fo(), it && R > 0 && Vt(Xs()), !T)
      if (Z) {
        for (const [N, U] of u)
          ie.has(N) || ae.skip_effect(U.e);
        ae.oncommit(S), ae.ondiscard(() => {
        });
      } else
        S();
    K && Sr(!0), s(g);
  }), $ = { effect: W, items: u, outrogroups: null, fallback: w };
  T = !1, it && (d = lt);
}
function ws(e) {
  for (; e !== null && (e.f & ur) === 0; )
    e = e.next;
  return e;
}
function Dl(e, t, r, a, o) {
  var c = (a & ko) !== 0, d = t.length, u = e.items, v = ws(e.effect.first), h, w = null, g, b = [], T = [], S, W, $, R;
  if (c)
    for (R = 0; R < d; R += 1)
      S = t[R], W = o(S, R), $ = /** @type {EachItem} */
      u.get(W).e, ($.f & Cr) === 0 && ($.nodes?.a?.measure(), (g ??= /* @__PURE__ */ new Set()).add($));
  for (R = 0; R < d; R += 1) {
    if (S = t[R], W = o(S, R), $ = /** @type {EachItem} */
    u.get(W).e, e.outrogroups !== null)
      for (const ve of e.outrogroups)
        ve.pending.delete($), ve.done.delete($);
    if (($.f & Cr) !== 0)
      if ($.f ^= Cr, $ === v)
        Cs($, null, r);
      else {
        var K = w ? w.next : v;
        $ === e.effect.last && (e.effect.last = $.prev), $.prev && ($.prev.next = $.next), $.next && ($.next.prev = $.prev), Ir(e, w, $), Ir(e, $, K), Cs($, K, r), w = $, b = [], T = [], v = ws(w.next);
        continue;
      }
    if (($.f & Qt) !== 0 && (on($), c && ($.nodes?.a?.unfix(), (g ??= /* @__PURE__ */ new Set()).delete($))), $ !== v) {
      if (h !== void 0 && h.has($)) {
        if (b.length < T.length) {
          var ge = T[0], ie;
          w = ge.prev;
          var ae = b[0], Z = b[b.length - 1];
          for (ie = 0; ie < b.length; ie += 1)
            Cs(b[ie], ge, r);
          for (ie = 0; ie < T.length; ie += 1)
            h.delete(T[ie]);
          Ir(e, ae.prev, Z.next), Ir(e, w, ae), Ir(e, Z, ge), v = ge, w = Z, R -= 1, b = [], T = [];
        } else
          h.delete($), Cs($, v, r), Ir(e, $.prev, $.next), Ir(e, $, w === null ? e.effect.first : w.next), Ir(e, w, $), w = $;
        continue;
      }
      for (b = [], T = []; v !== null && v !== $; )
        (h ??= /* @__PURE__ */ new Set()).add(v), T.push(v), v = ws(v.next);
      if (v === null)
        continue;
    }
    ($.f & Cr) === 0 && b.push($), w = $, v = ws($.next);
  }
  if (e.outrogroups !== null) {
    for (const ve of e.outrogroups)
      ve.pending.size === 0 && (Ra(sa(ve.done)), e.outrogroups?.delete(ve));
    e.outrogroups.size === 0 && (e.outrogroups = null);
  }
  if (v !== null || h !== void 0) {
    var De = [];
    if (h !== void 0)
      for ($ of h)
        ($.f & Qt) === 0 && De.push($);
    for (; v !== null; )
      (v.f & Qt) === 0 && v !== e.fallback && De.push(v), v = ws(v.next);
    var Ve = De.length;
    if (Ve > 0) {
      var Ge = (a & Gn) !== 0 && d === 0 ? r : null;
      if (c) {
        for (R = 0; R < Ve; R += 1)
          De[R].nodes?.a?.measure();
        for (R = 0; R < Ve; R += 1)
          De[R].nodes?.a?.fix();
      }
      Al(e, De, Ge);
    }
  }
  c && gr(() => {
    if (g !== void 0)
      for ($ of g)
        $.nodes?.a?.apply();
  });
}
function Pl(e, t, r, a, o, c, d, u) {
  var v = (d & yo) !== 0 ? (d & $o) === 0 ? /* @__PURE__ */ en(r, !1, !1) : Xr(r) : null, h = (d & wo) !== 0 ? Xr(o) : null;
  return {
    v,
    i: h,
    e: er(() => (c(t, v ?? r, h ?? o, u), () => {
      e.delete(a);
    }))
  };
}
function Cs(e, t, r) {
  if (e.nodes)
    for (var a = e.nodes.start, o = e.nodes.end, c = t && (t.f & Cr) === 0 ? (
      /** @type {EffectNodes} */
      t.nodes.start
    ) : r; a !== null; ) {
      var d = (
        /** @type {TemplateNode} */
        /* @__PURE__ */ vr(a)
      );
      if (c.before(a), a === o)
        return;
      a = d;
    }
}
function Ir(e, t, r) {
  t === null ? e.effect.first = r : t.next = r, r === null ? e.effect.last = t : r.prev = t;
}
function Tl(e, t, r = !1, a = !1, o = !1) {
  var c = e, d = "";
  C(() => {
    var u = (
      /** @type {Effect} */
      ct
    );
    if (d === (d = t() ?? "")) {
      it && us();
      return;
    }
    if (u.nodes !== null && (ki(
      u.nodes.start,
      /** @type {TemplateNode} */
      u.nodes.end
    ), u.nodes = null), d !== "") {
      if (it) {
        lt.data;
        for (var v = us(), h = v; v !== null && (v.nodeType !== rs || /** @type {Comment} */
        v.data !== ""); )
          h = v, v = /* @__PURE__ */ vr(v);
        if (v === null)
          throw Ns(), Jr;
        jr(lt, h), c = Vt(v);
        return;
      }
      var w = d + "";
      r ? w = `<svg>${w}</svg>` : a && (w = `<math>${w}</math>`);
      var g = Ni(w);
      if ((r || a) && (g = /** @type {Element} */
      /* @__PURE__ */ tr(g)), jr(
        /** @type {TemplateNode} */
        /* @__PURE__ */ tr(g),
        /** @type {TemplateNode} */
        g.lastChild
      ), r || a)
        for (; /* @__PURE__ */ tr(g); )
          c.before(
            /** @type {TemplateNode} */
            /* @__PURE__ */ tr(g)
          );
      else
        c.before(g);
    }
  });
}
function Vi(e, t) {
  an(() => {
    var r = e.getRootNode(), a = (
      /** @type {ShadowRoot} */
      r.host ? (
        /** @type {ShadowRoot} */
        r
      ) : (
        /** @type {Document} */
        r.head ?? /** @type {Document} */
        r.ownerDocument.head
      )
    );
    if (!a.querySelector("#" + t.hash)) {
      const o = document.createElement("style");
      o.id = t.hash, o.textContent = t.code, a.appendChild(o);
    }
  });
}
function Bi(e) {
  var t, r, a = "";
  if (typeof e == "string" || typeof e == "number") a += e;
  else if (typeof e == "object") if (Array.isArray(e)) {
    var o = e.length;
    for (t = 0; t < o; t++) e[t] && (r = Bi(e[t])) && (a && (a += " "), a += r);
  } else for (r in e) e[r] && (a && (a += " "), a += r);
  return a;
}
function Rl() {
  for (var e, t, r = 0, a = "", o = arguments.length; r < o; r++) (e = arguments[r]) && (t = Bi(e)) && (a && (a += " "), a += t);
  return a;
}
function Ml(e) {
  return typeof e == "object" ? Rl(e) : e ?? "";
}
function Il(e, t, r) {
  var a = e == null ? "" : "" + e;
  return t && (a = a ? a + " " + t : t), a === "" ? null : a;
}
function Ll(e, t) {
  return e == null ? null : String(e);
}
function Re(e, t, r, a, o, c) {
  var d = e.__className;
  if (it || d !== r || d === void 0) {
    var u = Il(r, a);
    (!it || u !== e.getAttribute("class")) && (u == null ? e.removeAttribute("class") : e.className = u), e.__className = r;
  }
  return c;
}
function hr(e, t, r, a) {
  var o = e.__style;
  if (it || o !== t) {
    var c = Ll(t);
    (!it || c !== e.getAttribute("style")) && (c == null ? e.removeAttribute("style") : e.style.cssText = c), e.__style = t;
  }
  return a;
}
function qi(e, t, r = !1) {
  if (e.multiple) {
    if (t == null)
      return;
    if (!Ya(t))
      return Yo();
    for (var a of e.options)
      a.selected = t.includes(As(a));
    return;
  }
  for (a of e.options) {
    var o = As(a);
    if (dl(o, t)) {
      a.selected = !0;
      return;
    }
  }
  (!r || t !== void 0) && (e.selectedIndex = -1);
}
function Nl(e) {
  var t = new MutationObserver(() => {
    qi(e, e.__value);
  });
  t.observe(e, {
    // Listen to option element changes
    childList: !0,
    subtree: !0,
    // because of <optgroup>
    // Listen to option element value attribute changes
    // (doesn't get notified of select value changes,
    // because that property is not reflected as an attribute)
    attributes: !0,
    attributeFilter: ["value"]
  }), la(() => {
    t.disconnect();
  });
}
function Is(e, t, r = t) {
  var a = /* @__PURE__ */ new WeakSet(), o = !0;
  gi(e, "change", (c) => {
    var d = c ? "[selected]" : ":checked", u;
    if (e.multiple)
      u = [].map.call(e.querySelectorAll(d), As);
    else {
      var v = e.querySelector(d) ?? // will fall back to first non-disabled option if no option is selected
      e.querySelector("option:not([disabled])");
      u = v && As(v);
    }
    r(u), dt !== null && a.add(dt);
  }), an(() => {
    var c = t();
    if (e === document.activeElement) {
      var d = (
        /** @type {Batch} */
        Zs ?? dt
      );
      if (a.has(d))
        return;
    }
    if (qi(e, c, o), o && c === void 0) {
      var u = e.querySelector(":checked");
      u !== null && (c = As(u), r(c));
    }
    e.__value = c, o = !1;
  }), Nl(e);
}
function As(e) {
  return "__value" in e ? e.__value : e.value;
}
const Ol = /* @__PURE__ */ Symbol("is custom element"), Fl = /* @__PURE__ */ Symbol("is html");
function bt(e) {
  if (it) {
    var t = !1, r = () => {
      if (!t) {
        if (t = !0, e.hasAttribute("value")) {
          var a = e.value;
          zt(e, "value", null), e.value = a;
        }
        if (e.hasAttribute("checked")) {
          var o = e.checked;
          zt(e, "checked", null), e.checked = o;
        }
      }
    };
    e.__on_r = r, gr(r), _i();
  }
}
function zt(e, t, r, a) {
  var o = jl(e);
  it && (o[t] = e.getAttribute(t), t === "src" || t === "srcset" || t === "href" && e.nodeName === "LINK") || o[t] !== (o[t] = r) && (t === "loading" && (e[No] = r), r == null ? e.removeAttribute(t) : typeof r != "string" && Vl(e).includes(t) ? e[t] = r : e.setAttribute(t, r));
}
function jl(e) {
  return (
    /** @type {Record<string | symbol, unknown>} **/
    // @ts-expect-error
    e.__attributes ??= {
      [Ol]: e.nodeName.includes("-"),
      [Fl]: e.namespaceURI === Po
    }
  );
}
var Mn = /* @__PURE__ */ new Map();
function Vl(e) {
  var t = e.getAttribute("is") || e.nodeName, r = Mn.get(t);
  if (r) return r;
  Mn.set(t, r = []);
  for (var a, o = e, c = Element.prototype; c !== o; ) {
    a = Wn(o);
    for (var d in a)
      a[d].set && r.push(d);
    o = Ka(o);
  }
  return r;
}
function xt(e, t, r = t) {
  var a = /* @__PURE__ */ new WeakSet();
  gi(e, "input", async (o) => {
    var c = o ? e.defaultValue : e.value;
    if (c = ma(e) ? ya(c) : c, r(c), dt !== null && a.add(dt), await ln(), c !== (c = t())) {
      var d = e.selectionStart, u = e.selectionEnd, v = e.value.length;
      if (e.value = c ?? "", u !== null) {
        var h = e.value.length;
        d === u && u === v && h > v ? (e.selectionStart = h, e.selectionEnd = h) : (e.selectionStart = d, e.selectionEnd = Math.min(u, h));
      }
    }
  }), // If we are hydrating and the value has since changed,
  // then use the updated value from the input instead.
  (it && e.defaultValue !== e.value || // If defaultValue is set, then value == defaultValue
  // TODO Svelte 6: remove input.value check and set to empty string?
  ss(t) == null && e.value) && (r(ma(e) ? ya(e.value) : e.value), dt !== null && a.add(dt)), ca(() => {
    var o = t();
    if (e === document.activeElement) {
      var c = (
        /** @type {Batch} */
        Zs ?? dt
      );
      if (a.has(c))
        return;
    }
    ma(e) && o === ya(e.value) || e.type === "date" && !o && !e.value || o !== e.value && (e.value = o ?? "");
  });
}
function ma(e) {
  var t = e.type;
  return t === "number" || t === "range";
}
function ya(e) {
  return e === "" ? null : +e;
}
function In(e, t) {
  return e === t || e?.[Or] === t;
}
function ls(e = {}, t, r, a) {
  return an(() => {
    var o, c;
    return ca(() => {
      o = c, c = [], ss(() => {
        e !== r(...c) && (t(e, ...c), o && In(r(...o), e) && t(null, ...o));
      });
    }), () => {
      gr(() => {
        c && In(r(...c), e) && t(null, ...c);
      });
    };
  }), e;
}
function Hi(e = !1) {
  const t = (
    /** @type {ComponentContextLegacy} */
    mt
  ), r = t.l.u;
  if (!r) return;
  let a = () => bl(t.s);
  if (e) {
    let o = 0, c = (
      /** @type {Record<string, any>} */
      {}
    );
    const d = /* @__PURE__ */ Fs(() => {
      let u = !1;
      const v = t.s;
      for (const h in v)
        v[h] !== c[h] && (c[h] = v[h], u = !0);
      return u && o++, o;
    });
    a = () => s(d);
  }
  r.b.length && vl(() => {
    Ln(t, a), Js(r.b);
  }), Pt(() => {
    const o = ss(() => r.m.map(Io));
    return () => {
      for (const c of o)
        typeof c == "function" && c();
    };
  }), r.a.length && Pt(() => {
    Ln(t, a), Js(r.a);
  });
}
function Ln(e, t) {
  if (e.l.s)
    for (const r of e.l.s) s(r);
  t();
}
function cn(e, t, r) {
  if (e == null)
    return t(void 0), r && r(void 0), Er;
  const a = ss(
    () => e.subscribe(
      t,
      // @ts-expect-error
      r
    )
  );
  return a.unsubscribe ? () => a.unsubscribe() : a;
}
const ns = [];
function Bl(e, t) {
  return {
    subscribe: Ze(e, t).subscribe
  };
}
function Ze(e, t = Er) {
  let r = null;
  const a = /* @__PURE__ */ new Set();
  function o(u) {
    if (Zn(e, u) && (e = u, r)) {
      const v = !ns.length;
      for (const h of a)
        h[1](), ns.push(h, e);
      if (v) {
        for (let h = 0; h < ns.length; h += 2)
          ns[h][0](ns[h + 1]);
        ns.length = 0;
      }
    }
  }
  function c(u) {
    o(u(
      /** @type {T} */
      e
    ));
  }
  function d(u, v = Er) {
    const h = [u, v];
    return a.add(h), a.size === 1 && (r = t(o, c) || Er), u(
      /** @type {T} */
      e
    ), () => {
      a.delete(h), a.size === 0 && r && (r(), r = null);
    };
  }
  return { set: o, update: c, subscribe: d };
}
function Rt(e, t, r) {
  const a = !Array.isArray(e), o = a ? [e] : e;
  if (!o.every(Boolean))
    throw new Error("derived() expects stores as input, got a falsy value");
  const c = t.length < 2;
  return Bl(r, (d, u) => {
    let v = !1;
    const h = [];
    let w = 0, g = Er;
    const b = () => {
      if (w)
        return;
      g();
      const S = t(a ? h[0] : h, d, u);
      c ? d(S) : g = typeof S == "function" ? S : Er;
    }, T = o.map(
      (S, W) => cn(
        S,
        ($) => {
          h[W] = $, w &= ~(1 << W), v && b();
        },
        () => {
          w |= 1 << W;
        }
      )
    );
    return v = !0, b(), function() {
      Js(T), g(), v = !1;
    };
  });
}
function Jt(e) {
  let t;
  return cn(e, (r) => t = r)(), t;
}
let Ma = /* @__PURE__ */ Symbol();
function $e(e, t, r) {
  const a = r[t] ??= {
    store: null,
    source: /* @__PURE__ */ en(void 0),
    unsubscribe: Er
  };
  if (a.store !== e && !(Ma in r))
    if (a.unsubscribe(), a.store = e ?? null, e == null)
      a.source.v = void 0, a.unsubscribe = Er;
    else {
      var o = !0;
      a.unsubscribe = cn(e, (c) => {
        o ? a.source.v = c : x(a.source, c);
      }), o = !1;
    }
  return e && Ma in r ? Jt(e) : s(a.source);
}
function qt() {
  const e = {};
  function t() {
    la(() => {
      for (var r in e)
        e[r].unsubscribe();
      Rs(e, Ma, {
        enumerable: !1,
        value: !0
      });
    });
  }
  return [e, t];
}
function ft(e, t, r, a) {
  var o = !gs || (r & Co) !== 0, c = (r & Eo) !== 0, d = (
    /** @type {V} */
    a
  ), u = !0, v = () => (u && (u = !1, d = /** @type {V} */
  a), d), h;
  h = /** @type {V} */
  e[t], h === void 0 && a !== void 0 && (h = v());
  var w;
  if (o ? w = () => {
    var S = (
      /** @type {V} */
      e[t]
    );
    return S === void 0 ? v() : (u = !0, S);
  } : w = () => {
    var S = (
      /** @type {V} */
      e[t]
    );
    return S !== void 0 && (d = /** @type {V} */
    void 0), S === void 0 ? d : S;
  }, o && (r & So) === 0)
    return w;
  var g = !1, b = /* @__PURE__ */ Fs(() => (g = !1, w())), T = (
    /** @type {Effect} */
    ct
  );
  return (
    /** @type {() => V} */
    (function(S, W) {
      if (arguments.length > 0) {
        const $ = W ? s(b) : o && c ? Ut(S) : S;
        return x(b, $), g = !0, d !== void 0 && (d = $), S;
      }
      return Vr && g || (T.f & Ar) !== 0 ? b.v : s(b);
    })
  );
}
function ql(e) {
  return new Hl(e);
}
class Hl {
  /** @type {any} */
  #e;
  /** @type {Record<string, any>} */
  #t;
  /**
   * @param {ComponentConstructorOptions & {
   *  component: any;
   * }} options
   */
  constructor(t) {
    var r = /* @__PURE__ */ new Map(), a = (c, d) => {
      var u = /* @__PURE__ */ en(d, !1, !1);
      return r.set(c, u), u;
    };
    const o = new Proxy(
      { ...t.props || {}, $$events: {} },
      {
        get(c, d) {
          return s(r.get(d) ?? a(d, Reflect.get(c, d)));
        },
        has(c, d) {
          return d === Lo ? !0 : (s(r.get(d) ?? a(d, Reflect.get(c, d))), Reflect.has(c, d));
        },
        set(c, d, u) {
          return x(r.get(d) ?? a(d, u), u), Reflect.set(c, d, u);
        }
      }
    );
    this.#t = (t.hydrate ? kl : Oi)(t.component, {
      target: t.target,
      anchor: t.anchor,
      props: o,
      context: t.context,
      intro: t.intro ?? !1,
      recover: t.recover
    }), (!t?.props?.$$host || t.sync === !1) && vt(), this.#e = o.$$events;
    for (const c of Object.keys(this.#t))
      c === "$set" || c === "$destroy" || c === "$on" || Rs(this, c, {
        get() {
          return this.#t[c];
        },
        /** @param {any} value */
        set(d) {
          this.#t[c] = d;
        },
        enumerable: !0
      });
    this.#t.$set = /** @param {Record<string, any>} next */
    (c) => {
      Object.assign(o, c);
    }, this.#t.$destroy = () => {
      $l(this.#t);
    };
  }
  /** @param {Record<string, any>} props */
  $set(t) {
    this.#t.$set(t);
  }
  /**
   * @param {string} event
   * @param {(...args: any[]) => any} callback
   * @returns {any}
   */
  $on(t, r) {
    this.#e[t] = this.#e[t] || [];
    const a = (...o) => r.call(this, ...o);
    return this.#e[t].push(a), () => {
      this.#e[t] = this.#e[t].filter(
        /** @param {any} fn */
        (o) => o !== a
      );
    };
  }
  $destroy() {
    this.#t.$destroy();
  }
}
let Gi;
typeof HTMLElement == "function" && (Gi = class extends HTMLElement {
  /** The Svelte component constructor */
  $$ctor;
  /** Slots */
  $$s;
  /** @type {any} The Svelte component instance */
  $$c;
  /** Whether or not the custom element is connected */
  $$cn = !1;
  /** @type {Record<string, any>} Component props data */
  $$d = {};
  /** `true` if currently in the process of reflecting component props back to attributes */
  $$r = !1;
  /** @type {Record<string, CustomElementPropDefinition>} Props definition (name, reflected, type etc) */
  $$p_d = {};
  /** @type {Record<string, EventListenerOrEventListenerObject[]>} Event listeners */
  $$l = {};
  /** @type {Map<EventListenerOrEventListenerObject, Function>} Event listener unsubscribe functions */
  $$l_u = /* @__PURE__ */ new Map();
  /** @type {any} The managed render effect for reflecting attributes */
  $$me;
  /** @type {ShadowRoot | null} The ShadowRoot of the custom element */
  $$shadowRoot = null;
  /**
   * @param {*} $$componentCtor
   * @param {*} $$slots
   * @param {ShadowRootInit | undefined} shadow_root_init
   */
  constructor(e, t, r) {
    super(), this.$$ctor = e, this.$$s = t, r && (this.$$shadowRoot = this.attachShadow(r));
  }
  /**
   * @param {string} type
   * @param {EventListenerOrEventListenerObject} listener
   * @param {boolean | AddEventListenerOptions} [options]
   */
  addEventListener(e, t, r) {
    if (this.$$l[e] = this.$$l[e] || [], this.$$l[e].push(t), this.$$c) {
      const a = this.$$c.$on(e, t);
      this.$$l_u.set(t, a);
    }
    super.addEventListener(e, t, r);
  }
  /**
   * @param {string} type
   * @param {EventListenerOrEventListenerObject} listener
   * @param {boolean | AddEventListenerOptions} [options]
   */
  removeEventListener(e, t, r) {
    if (super.removeEventListener(e, t, r), this.$$c) {
      const a = this.$$l_u.get(t);
      a && (a(), this.$$l_u.delete(t));
    }
  }
  async connectedCallback() {
    if (this.$$cn = !0, !this.$$c) {
      let e = function(a) {
        return (o) => {
          const c = document.createElement("slot");
          a !== "default" && (c.name = a), f(o, c);
        };
      };
      if (await Promise.resolve(), !this.$$cn || this.$$c)
        return;
      const t = {}, r = Gl(this);
      for (const a of this.$$s)
        a in r && (a === "default" && !this.$$d.children ? (this.$$d.children = e(a), t.default = !0) : t[a] = e(a));
      for (const a of this.attributes) {
        const o = this.$$g_p(a.name);
        o in this.$$d || (this.$$d[o] = Gs(o, a.value, this.$$p_d, "toProp"));
      }
      for (const a in this.$$p_d)
        !(a in this.$$d) && this[a] !== void 0 && (this.$$d[a] = this[a], delete this[a]);
      this.$$c = ql({
        component: this.$$ctor,
        target: this.$$shadowRoot || this,
        props: {
          ...this.$$d,
          $$slots: t,
          $$host: this
        }
      }), this.$$me = fl(() => {
        ca(() => {
          this.$$r = !0;
          for (const a of Ks(this.$$c)) {
            if (!this.$$p_d[a]?.reflect) continue;
            this.$$d[a] = this.$$c[a];
            const o = Gs(
              a,
              this.$$d[a],
              this.$$p_d,
              "toAttribute"
            );
            o == null ? this.removeAttribute(this.$$p_d[a].attribute || a) : this.setAttribute(this.$$p_d[a].attribute || a, o);
          }
          this.$$r = !1;
        });
      });
      for (const a in this.$$l)
        for (const o of this.$$l[a]) {
          const c = this.$$c.$on(a, o);
          this.$$l_u.set(o, c);
        }
      this.$$l = {};
    }
  }
  // We don't need this when working within Svelte code, but for compatibility of people using this outside of Svelte
  // and setting attributes through setAttribute etc, this is helpful
  /**
   * @param {string} attr
   * @param {string} _oldValue
   * @param {string} newValue
   */
  attributeChangedCallback(e, t, r) {
    this.$$r || (e = this.$$g_p(e), this.$$d[e] = Gs(e, r, this.$$p_d, "toProp"), this.$$c?.$set({ [e]: this.$$d[e] }));
  }
  disconnectedCallback() {
    this.$$cn = !1, Promise.resolve().then(() => {
      !this.$$cn && this.$$c && (this.$$c.$destroy(), this.$$me(), this.$$c = void 0);
    });
  }
  /**
   * @param {string} attribute_name
   */
  $$g_p(e) {
    return Ks(this.$$p_d).find(
      (t) => this.$$p_d[t].attribute === e || !this.$$p_d[t].attribute && t.toLowerCase() === e
    ) || e;
  }
});
function Gs(e, t, r, a) {
  const o = r[e]?.type;
  if (t = o === "Boolean" && typeof t != "boolean" ? t != null : t, !a || !r[e])
    return t;
  if (a === "toAttribute")
    switch (o) {
      case "Object":
      case "Array":
        return t == null ? null : JSON.stringify(t);
      case "Boolean":
        return t ? "" : null;
      case "Number":
        return t ?? null;
      default:
        return t;
    }
  else
    switch (o) {
      case "Object":
      case "Array":
        return t && JSON.parse(t);
      case "Boolean":
        return t;
      // conversion already handled above
      case "Number":
        return t != null ? +t : t;
      default:
        return t;
    }
}
function Gl(e) {
  const t = {};
  return e.childNodes.forEach((r) => {
    t[
      /** @type {Element} node */
      r.slot || "default"
    ] = !0;
  }), t;
}
function Dt(e, t, r, a, o, c) {
  let d = class extends Gi {
    constructor() {
      super(e, r, o), this.$$p_d = t;
    }
    static get observedAttributes() {
      return Ks(t).map(
        (u) => (t[u].attribute || u).toLowerCase()
      );
    }
  };
  return Ks(t).forEach((u) => {
    Rs(d.prototype, u, {
      get() {
        return this.$$c && u in this.$$c ? this.$$c[u] : this.$$d[u];
      },
      set(v) {
        v = Gs(u, v, t), this.$$d[u] = v;
        var h = this.$$c;
        if (h) {
          var w = os(h, u)?.get;
          w ? h[u] = v : h.$set({ [u]: v });
        }
      }
    });
  }), a.forEach((u) => {
    Rs(d.prototype, u, {
      get() {
        return this.$$c?.[u];
      }
    });
  }), e.element = /** @type {any} */
  d, d;
}
const Wi = 8, Ui = 16, Ds = 64;
function Lr(e, t) {
  return (e & t) !== 0;
}
function Ia(e, t) {
  switch (t) {
    case "storming":
      return e.storming_status_label ?? "";
    case "planning":
      return e.planning_status_label ?? "";
    case "kanban":
      return e.kanban_status_label ?? "";
    case "crafting":
      return e.crafting_status_label ?? "";
  }
}
function La(e, t) {
  switch (t) {
    case "storming":
      return e.storming_available_actions ?? [];
    case "planning":
      return e.planning_available_actions ?? [];
    case "kanban":
      return e.kanban_available_actions ?? [];
    case "crafting":
      return e.crafting_available_actions ?? [];
  }
}
let Na;
function Wl(e) {
  Na = e;
}
function Ue() {
  if (!Na)
    throw new Error("Martha API not initialized. Call setApi() first.");
  return Na;
}
const zi = "hecate://localhost";
async function Ul() {
  try {
    const e = await fetch(`${zi}/api/llm/models`);
    if (!e.ok) return [];
    const t = await e.json();
    return t.ok && Array.isArray(t.models) ? t.models.map((r) => r.name) : [];
  } catch {
    return [];
  }
}
function zl(e, t) {
  let r = null, a = null, o = null, c = !1;
  const d = {
    onChunk(u) {
      return r = u, d;
    },
    onDone(u) {
      return a = u, d;
    },
    onError(u) {
      return o = u, d;
    },
    async start() {
      if (!c)
        try {
          const u = await fetch(`${zi}/api/llm/chat`, {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ model: e, messages: t })
          });
          if (c) return;
          if (!u.ok) {
            const h = await u.text().catch(() => u.statusText);
            o && o(h || "LLM request failed");
            return;
          }
          const v = await u.json();
          r && r({ content: v.content }), a && a({ content: "", done: !0 });
        } catch (u) {
          if (c) return;
          o && o(u.message || "LLM request failed");
        }
    },
    cancel() {
      c = !0;
    }
  };
  return d;
}
function Yi() {
  return {
    stream: {
      chat: zl
    }
  };
}
const dn = Ze(!1), Ki = Ze(""), un = Ze(null), Ji = Ze(null), vn = Ze([]), fn = Rt(
  [Ji, vn],
  ([e, t]) => e ?? t[0] ?? null
), Qi = "hecate-app-martha-phase-models";
function Yl() {
  try {
    const e = localStorage.getItem(Qi);
    if (e)
      return { storming: null, planning: null, kanban: null, crafting: null, ...JSON.parse(e) };
  } catch {
  }
  return { storming: null, planning: null, kanban: null, crafting: null };
}
function Kl(e) {
  try {
    localStorage.setItem(Qi, JSON.stringify(e));
  } catch {
  }
}
const Xi = Ze(Yl()), Jl = [
  /code/i,
  /coder/i,
  /codestral/i,
  /starcoder/i,
  /codellama/i,
  /wizard-?coder/i,
  /deepseek-coder/i
];
function Nn(e) {
  return Jl.some((t) => t.test(e)) ? "code" : "general";
}
function Ql(e) {
  return e === "crafting" ? "code" : "general";
}
function $r(e, t) {
  un.set(t ?? null), Ki.set(e), dn.set(!0);
}
function Xl() {
  dn.set(!1), un.set(null);
}
function pn(e) {
  Ji.set(e);
}
function On(e, t) {
  Xi.update((r) => {
    const a = { ...r, [e]: t };
    return Kl(a), a;
  });
}
function Zl(e) {
  return e.split(`
`).map((t) => t.replace(/^[\s\-*\u2022\d.]+/, "").trim()).filter((t) => t.length > 0 && t.length < 80 && !t.includes(":")).map((t) => t.replace(/["`]/g, ""));
}
const Kr = [
  {
    code: "storming",
    name: "Storming",
    shortName: "Storming",
    description: "Design aggregates, events, desks, and dependencies",
    role: "storming",
    color: "phase-storming"
  },
  {
    code: "planning",
    name: "Planning",
    shortName: "Planning",
    description: "Lifecycle management for the division",
    role: "planning",
    color: "phase-planning"
  },
  {
    code: "kanban",
    name: "Kanban",
    shortName: "Kanban",
    description: "Work items board for desk crafting",
    role: "kanban",
    color: "phase-kanban"
  },
  {
    code: "crafting",
    name: "Crafting",
    shortName: "Crafting",
    description: "Generate code, run tests, deliver releases",
    role: "crafting",
    color: "phase-crafting"
  }
], xs = Ze([]), $t = Ze(null), Zr = Ze([]), Ps = Ze(null), wt = Ze(!1), pr = Ze(null), Hr = Rt(
  [Zr, Ps],
  ([e, t]) => e.find((r) => r.division_id === t) ?? null
), da = Rt(
  $t,
  (e) => e ? Lr(e.status, Ds) ? "archived" : Lr(e.status, Ui) ? "discovery_paused" : Lr(e.status, Wi) ? "discovering" : e.phase || "initiated" : "none"
);
function Ts(e) {
  $t.set(e);
}
function Oa() {
  $t.set(null);
}
async function ua() {
  try {
    const t = await Ue().get("/ventures");
    xs.set(t.ventures);
  } catch {
    xs.set([]);
  }
}
async function yr() {
  try {
    const t = await Ue().get("/venture");
    $t.set(t.venture);
  } catch {
    $t.set(null);
  }
}
async function bs(e) {
  try {
    const r = await Ue().get(
      `/ventures/${e}/divisions`
    );
    Zr.set(r.divisions);
  } catch {
    Zr.set([]);
  }
}
async function Zi(e, t) {
  try {
    wt.set(!0);
    const a = await Ue().post("/ventures/initiate", { name: e, brief: t, initiated_by: "hecate-web" }), o = {
      venture_id: a.venture_id,
      name: a.name,
      vision: "",
      brief: a.brief || "",
      status: a.status,
      status_label: a.status_label,
      phase: "initiated",
      initiated_at: a.initiated_at,
      created_at: String(a.initiated_at),
      updated_at: String(a.initiated_at)
    };
    return xs.update((c) => [...c, o]), $t.set(o), !0;
  } catch (r) {
    const a = r;
    return pr.set(a.message || "Failed to initiate venture"), !1;
  } finally {
    wt.set(!1);
  }
}
async function eo(e, t, r, a, o) {
  try {
    return wt.set(!0), await Ue().post(`/ventures/${e}/repo`, {
      repo_url: t,
      vision: r || void 0,
      name: a || void 0,
      brief: o || void 0
    }), await yr(), !0;
  } catch (c) {
    const d = c;
    return pr.set(d.message || "Failed to scaffold venture repo"), !1;
  } finally {
    wt.set(!1);
  }
}
async function xn(e) {
  try {
    return wt.set(!0), await Ue().post(`/ventures/${e}/discovery/start`, {}), await yr(), !0;
  } catch (t) {
    const r = t;
    return pr.set(r.message || "Failed to start discovery"), !1;
  } finally {
    wt.set(!1);
  }
}
async function to(e, t, r) {
  try {
    return wt.set(!0), await Ue().post(`/ventures/${e}/discovery/identify`, {
      context_name: t,
      description: r || null,
      identified_by: "hecate-web"
    }), await bs(e), !0;
  } catch (a) {
    const o = a;
    return pr.set(o.message || "Failed to identify division"), !1;
  } finally {
    wt.set(!1);
  }
}
async function ro(e, t) {
  try {
    return wt.set(!0), await Ue().post(`/ventures/${e}/discovery/pause`, {
      reason: t || null
    }), await yr(), !0;
  } catch (r) {
    const a = r;
    return pr.set(a.message || "Failed to pause discovery"), !1;
  } finally {
    wt.set(!1);
  }
}
async function so(e) {
  try {
    return wt.set(!0), await Ue().post(`/ventures/${e}/discovery/resume`, {}), await yr(), !0;
  } catch (t) {
    const r = t;
    return pr.set(r.message || "Failed to resume discovery"), !1;
  } finally {
    wt.set(!1);
  }
}
async function ao(e) {
  try {
    return wt.set(!0), await Ue().post(`/ventures/${e}/discovery/complete`, {}), await yr(), !0;
  } catch (t) {
    const r = t;
    return pr.set(r.message || "Failed to complete discovery"), !1;
  } finally {
    wt.set(!1);
  }
}
const ec = /* @__PURE__ */ Object.freeze(/* @__PURE__ */ Object.defineProperty({
  __proto__: null,
  activeVenture: $t,
  clearActiveVenture: Oa,
  completeDiscovery: ao,
  divisions: Zr,
  fetchActiveVenture: yr,
  fetchDivisions: bs,
  fetchVentures: ua,
  identifyDivision: to,
  initiateVenture: Zi,
  isLoading: wt,
  pauseDiscovery: ro,
  resumeDiscovery: so,
  scaffoldVentureRepo: eo,
  selectVenture: Ts,
  selectedDivision: Hr,
  selectedDivisionId: Ps,
  startDiscovery: xn,
  ventureError: pr,
  ventureStep: da,
  ventures: xs
}, Symbol.toStringTag, { value: "Module" })), hs = Ze("storming"), va = Ze(null), Pr = Ze(!1);
function fa(e) {
  switch (e) {
    case "storming":
      return "stormings";
    case "planning":
      return "plannings";
    case "kanban":
      return "kanbans";
    case "crafting":
      return "craftings";
  }
}
async function tc(e, t) {
  try {
    Pr.set(!0), await Ue().post(`/${fa(t)}/${e}/open`, {});
    const a = Jt($t);
    return a && await bs(a.venture_id), !0;
  } catch (r) {
    const a = r;
    return va.set(a.message || `Failed to open ${t}`), !1;
  } finally {
    Pr.set(!1);
  }
}
async function rc(e, t, r) {
  try {
    Pr.set(!0), await Ue().post(`/${fa(t)}/${e}/shelve`, {
      reason: r || null
    });
    const o = Jt($t);
    return o && await bs(o.venture_id), !0;
  } catch (a) {
    const o = a;
    return va.set(o.message || `Failed to shelve ${t}`), !1;
  } finally {
    Pr.set(!1);
  }
}
async function sc(e, t) {
  try {
    Pr.set(!0), await Ue().post(`/${fa(t)}/${e}/resume`, {});
    const a = Jt($t);
    return a && await bs(a.venture_id), !0;
  } catch (r) {
    const a = r;
    return va.set(a.message || `Failed to resume ${t}`), !1;
  } finally {
    Pr.set(!1);
  }
}
async function ac(e, t) {
  try {
    Pr.set(!0), await Ue().post(`/${fa(t)}/${e}/conclude`, {});
    const a = Jt($t);
    return a && await bs(a.venture_id), !0;
  } catch (r) {
    const a = r;
    return va.set(a.message || `Failed to conclude ${t}`), !1;
  } finally {
    Pr.set(!1);
  }
}
const es = Ze("ready"), ms = Ze([]), pa = Ze([]), hn = Ze([]), ta = Ze(600), _n = Ze([]), Fa = Ze(!1), Ft = Ze(null), ja = Ze(!1);
let Ur = null;
const nc = Rt(
  ms,
  (e) => e.filter((t) => !t.cluster_id)
), ic = Rt(
  ms,
  (e) => {
    const t = /* @__PURE__ */ new Map();
    for (const r of e)
      if (r.stack_id) {
        const a = t.get(r.stack_id) || [];
        a.push(r), t.set(r.stack_id, a);
      }
    return t;
  }
), oc = Rt(
  ms,
  (e) => e.length
);
async function Ht(e) {
  try {
    const a = (await Ue().get(
      `/ventures/${e}/storm/state`
    )).storm;
    es.set(a.phase), ms.set(a.stickies), pa.set(a.clusters), hn.set(a.arrows);
  } catch {
    es.set("ready");
  }
}
async function Fn(e, t = 0, r = 50) {
  try {
    const o = await Ue().get(
      `/ventures/${e}/events?offset=${t}&limit=${r}`
    );
    return _n.set(o.events), { events: o.events, count: o.count };
  } catch {
    return { events: [], count: 0 };
  }
}
async function lc(e) {
  try {
    return ja.set(!0), await Ue().post(`/ventures/${e}/storm/start`, {}), es.set("storm"), ta.set(600), Ur = setInterval(() => {
      ta.update((r) => r <= 1 ? (Ur && (clearInterval(Ur), Ur = null), 0) : r - 1);
    }, 1e3), !0;
  } catch (t) {
    const r = t;
    return Ft.set(r.message || "Failed to start storm"), !1;
  } finally {
    ja.set(!1);
  }
}
async function Ws(e, t, r = "user") {
  try {
    return await Ue().post(`/ventures/${e}/storm/sticky`, { text: t, author: r }), await Ht(e), !0;
  } catch (a) {
    const o = a;
    return Ft.set(o.message || "Failed to post sticky"), !1;
  }
}
async function cc(e, t) {
  try {
    return await Ue().post(`/ventures/${e}/storm/sticky/${t}/pull`, {}), await Ht(e), !0;
  } catch (r) {
    const a = r;
    return Ft.set(a.message || "Failed to pull sticky"), !1;
  }
}
async function jn(e, t, r) {
  try {
    return await Ue().post(`/ventures/${e}/storm/sticky/${t}/stack`, {
      target_sticky_id: r
    }), await Ht(e), !0;
  } catch (a) {
    const o = a;
    return Ft.set(o.message || "Failed to stack sticky"), !1;
  }
}
async function dc(e, t) {
  try {
    return await Ue().post(`/ventures/${e}/storm/sticky/${t}/unstack`, {}), await Ht(e), !0;
  } catch (r) {
    const a = r;
    return Ft.set(a.message || "Failed to unstack sticky"), !1;
  }
}
async function uc(e, t, r) {
  try {
    return await Ue().post(`/ventures/${e}/storm/stack/${t}/groom`, {
      canonical_sticky_id: r
    }), await Ht(e), !0;
  } catch (a) {
    const o = a;
    return Ft.set(o.message || "Failed to groom stack"), !1;
  }
}
async function Vn(e, t, r) {
  try {
    return await Ue().post(`/ventures/${e}/storm/sticky/${t}/cluster`, {
      target_cluster_id: r
    }), await Ht(e), !0;
  } catch (a) {
    const o = a;
    return Ft.set(o.message || "Failed to cluster sticky"), !1;
  }
}
async function vc(e, t) {
  try {
    return await Ue().post(`/ventures/${e}/storm/sticky/${t}/uncluster`, {}), await Ht(e), !0;
  } catch (r) {
    const a = r;
    return Ft.set(a.message || "Failed to uncluster sticky"), !1;
  }
}
async function fc(e, t) {
  try {
    return await Ue().post(`/ventures/${e}/storm/cluster/${t}/dissolve`, {}), await Ht(e), !0;
  } catch (r) {
    const a = r;
    return Ft.set(a.message || "Failed to dissolve cluster"), !1;
  }
}
async function pc(e, t, r) {
  try {
    return await Ue().post(`/ventures/${e}/storm/cluster/${t}/name`, { name: r }), await Ht(e), !0;
  } catch (a) {
    const o = a;
    return Ft.set(o.message || "Failed to name cluster"), !1;
  }
}
async function xc(e, t, r, a) {
  try {
    return await Ue().post(`/ventures/${e}/storm/fact`, {
      from_cluster: t,
      to_cluster: r,
      fact_name: a
    }), await Ht(e), !0;
  } catch (o) {
    const c = o;
    return Ft.set(c.message || "Failed to draw fact arrow"), !1;
  }
}
async function hc(e, t) {
  try {
    return await Ue().post(`/ventures/${e}/storm/fact/${t}/erase`, {}), await Ht(e), !0;
  } catch (r) {
    const a = r;
    return Ft.set(a.message || "Failed to erase fact arrow"), !1;
  }
}
async function _c(e, t) {
  try {
    return await Ue().post(`/ventures/${e}/storm/cluster/${t}/promote`, {}), await Ht(e), !0;
  } catch (r) {
    const a = r;
    return Ft.set(a.message || "Failed to promote cluster"), !1;
  }
}
async function ks(e, t) {
  try {
    return await Ue().post(`/ventures/${e}/storm/phase/advance`, {
      target_phase: t
    }), await Ht(e), !0;
  } catch (r) {
    const a = r;
    return Ft.set(a.message || "Failed to advance phase"), !1;
  }
}
async function gc(e) {
  try {
    return await Ue().post(`/ventures/${e}/storm/shelve`, {}), es.set("shelved"), !0;
  } catch (t) {
    const r = t;
    return Ft.set(r.message || "Failed to shelve storm"), !1;
  }
}
async function bc(e) {
  try {
    return await Ue().post(`/ventures/${e}/storm/resume`, {}), await Ht(e), !0;
  } catch (t) {
    const r = t;
    return Ft.set(r.message || "Failed to resume storm"), !1;
  }
}
async function mc(e) {
  const t = Jt(pa);
  let r = !0;
  for (const a of t) {
    if (a.status !== "active" || !a.name?.trim()) continue;
    await _c(e, a.cluster_id) || (r = !1);
  }
  if (r) {
    const { fetchDivisions: a } = await Promise.resolve().then(() => ec);
    await a(e);
  }
  return r;
}
function yc() {
  Ur && (clearInterval(Ur), Ur = null), es.set("ready"), ms.set([]), pa.set([]), hn.set([]), _n.set([]), ta.set(600);
}
const Bn = Ze(!1), wc = Ze(null), kc = Ze(null);
async function $c(e, t) {
  try {
    Bn.set(!0);
    const a = await Ue().post(
      `/ventures/${e}/vision/refine`,
      { vision: t }
    );
    return wc.set(a.refined), await yr(), !0;
  } catch (r) {
    const a = r;
    return kc.set(a.message || "Failed to refine vision"), !1;
  } finally {
    Bn.set(!1);
  }
}
var Cc = /* @__PURE__ */ p('<div class="text-[10px] text-surface-400 truncate mt-0.5"> </div>'), Sc = /* @__PURE__ */ p('<button><div class="font-medium"> </div> <!></button>'), Ec = /* @__PURE__ */ p(`<div class="absolute top-full left-0 mt-1 z-20 min-w-[220px]
						bg-surface-700 border border-surface-600 rounded-lg shadow-lg overflow-hidden"><!> <button class="w-full text-left px-3 py-2 text-xs text-hecate-400
							hover:bg-hecate-600/20 transition-colors border-t border-surface-600">+ New Venture</button></div>`), Ac = /* @__PURE__ */ p('<span class="text-[11px] text-surface-400 truncate max-w-[300px]"> </span>'), Dc = /* @__PURE__ */ p('<span class="text-[10px] text-surface-400"> </span>'), Pc = /* @__PURE__ */ p('<span class="text-[10px] text-surface-400 italic">Oracle active</span>'), Tc = /* @__PURE__ */ p(`<button class="text-[11px] px-2.5 py-1 rounded bg-hecate-600/20 text-hecate-300
					hover:bg-hecate-600/30 transition-colors disabled:opacity-50">Start Discovery</button>`), Rc = /* @__PURE__ */ p(`<button class="text-[11px] px-2 py-1 rounded text-surface-400
						hover:text-health-ok hover:bg-surface-700 transition-colors disabled:opacity-50">Complete Discovery</button>`), Mc = /* @__PURE__ */ p(
  `<button class="text-[11px] px-2.5 py-1 rounded bg-hecate-600/20 text-hecate-300
					hover:bg-hecate-600/30 transition-colors disabled:opacity-50">+ Identify Division</button> <button class="text-[11px] px-2 py-1 rounded text-surface-400
					hover:text-health-warn hover:bg-surface-700 transition-colors disabled:opacity-50">Pause</button> <!>`,
  1
), Ic = /* @__PURE__ */ p(`<button class="text-[11px] px-2.5 py-1 rounded bg-health-warn/10 text-health-warn
					hover:bg-health-warn/20 transition-colors disabled:opacity-50">Resume Discovery</button>`), Lc = /* @__PURE__ */ p('<div class="mt-2 text-[11px] text-health-err bg-health-err/10 rounded px-3 py-1.5"> </div>'), Nc = /* @__PURE__ */ p(`<div class="mt-3 flex gap-2 items-end"><div class="flex-1"><label for="refine-brief" class="text-[10px] text-surface-400 block mb-1">Vision Brief</label> <textarea id="refine-brief" placeholder="Describe what this venture aims to achieve..." class="w-full bg-surface-700 border border-surface-600 rounded px-3 py-2 text-xs
						text-surface-100 placeholder-surface-400 resize-none
						focus:outline-none focus:border-hecate-500"></textarea></div> <button class="px-3 py-2 rounded text-xs bg-hecate-600 text-surface-50
					hover:bg-hecate-500 transition-colors disabled:opacity-50 disabled:cursor-not-allowed">Refine</button> <button class="px-3 py-2 rounded text-xs text-surface-400 hover:text-surface-100 transition-colors">Cancel</button></div>`), Oc = /* @__PURE__ */ p(`<div class="mt-3 flex gap-2 items-end"><div class="flex-1"><label for="div-name" class="text-[10px] text-surface-400 block mb-1">Context Name</label> <input id="div-name" placeholder="e.g., authentication, billing, notifications" class="w-full bg-surface-700 border border-surface-600 rounded px-3 py-1.5 text-xs
						text-surface-100 placeholder-surface-400
						focus:outline-none focus:border-hecate-500"/></div> <div class="flex-1"><label for="div-desc" class="text-[10px] text-surface-400 block mb-1">Description (optional)</label> <input id="div-desc" placeholder="Brief description of this bounded context" class="w-full bg-surface-700 border border-surface-600 rounded px-3 py-1.5 text-xs
						text-surface-100 placeholder-surface-400
						focus:outline-none focus:border-hecate-500"/></div> <button class="px-3 py-1.5 rounded text-xs bg-hecate-600 text-surface-50
					hover:bg-hecate-500 transition-colors disabled:opacity-50 disabled:cursor-not-allowed">Identify</button> <button class="px-3 py-1.5 rounded text-xs text-surface-400 hover:text-surface-100 transition-colors">Cancel</button></div>`), Fc = /* @__PURE__ */ p(`<div class="border-b border-surface-600 bg-surface-800/50 px-4 py-3 shrink-0"><div class="flex items-center gap-3"><button class="flex items-center gap-1 text-xs text-surface-400 hover:text-hecate-300
				transition-colors shrink-0 -ml-1 px-1.5 py-1 rounded hover:bg-surface-700"><span class="text-sm"></span> <span>Ventures</span></button> <span class="text-surface-600 text-xs">|</span> <div class="relative flex items-center gap-2"><span class="text-hecate-400 text-lg"></span> <button class="flex items-center gap-1.5 text-sm font-semibold text-surface-100
					hover:text-hecate-300 transition-colors"> <span class="text-[9px] text-surface-400"></span></button> <!></div> <span> </span> <!> <div class="flex-1"></div> <!> <!></div> <!> <!> <!></div>`);
function no(e, t) {
  Ct(t, !0);
  const r = () => $e($t, "$activeVenture", v), a = () => $e(xs, "$ventures", v), o = () => $e(da, "$ventureStep", v), c = () => $e(Zr, "$divisions", v), d = () => $e(wt, "$isLoading", v), u = () => $e(pr, "$ventureError", v), [v, h] = qt();
  let w = /* @__PURE__ */ se(!1), g = /* @__PURE__ */ se(!1), b = /* @__PURE__ */ se(!1), T = /* @__PURE__ */ se(""), S = /* @__PURE__ */ se(""), W = /* @__PURE__ */ se("");
  async function $() {
    if (!r() || !s(T).trim()) return;
    await $c(r().venture_id, s(T).trim()) && (x(w, !1), x(T, ""));
  }
  async function R() {
    r() && await xn(r().venture_id);
  }
  async function K() {
    if (!r() || !s(S).trim()) return;
    await to(r().venture_id, s(S).trim(), s(W).trim() || void 0) && (x(g, !1), x(S, ""), x(W, ""));
  }
  function ge(A) {
    switch (A) {
      case "discovering":
        return "bg-hecate-600/20 text-hecate-300 border-hecate-600/40";
      case "discovery_completed":
        return "bg-health-ok/10 text-health-ok border-health-ok/30";
      case "discovery_paused":
        return "bg-health-warn/10 text-health-warn border-health-warn/30";
      default:
        return "bg-surface-700 text-surface-300 border-surface-600";
    }
  }
  var ie = Fc(), ae = i(ie), Z = i(ae);
  Z.__click = () => Oa();
  var De = i(Z);
  De.textContent = "←", Et(2), n(Z);
  var Ve = l(Z, 4), Ge = i(Ve);
  Ge.textContent = "◆";
  var ve = l(Ge, 2);
  ve.__click = () => x(b, !s(b));
  var N = i(ve), U = l(N);
  U.textContent = "▾", n(ve);
  var pe = l(ve, 2);
  {
    var Ne = (A) => {
      var E = Ec(), te = i(E);
      He(te, 1, () => a().filter((Ie) => !(Ie.status & Ds)), pt, (Ie, j) => {
        var B = Sc();
        B.__click = () => {
          Ts(s(j)), x(b, !1);
        };
        var xe = i(B), je = i(xe, !0);
        n(xe);
        var Me = l(xe, 2);
        {
          var et = (qe) => {
            var Ke = Cc(), We = i(Ke, !0);
            n(Ke), C(() => _(We, s(j).brief)), f(qe, Ke);
          };
          P(Me, (qe) => {
            s(j).brief && qe(et);
          });
        }
        n(B), C(() => {
          Re(B, 1, `w-full text-left px-3 py-2 text-xs transition-colors
								${s(j).venture_id === r()?.venture_id ? "bg-hecate-600/20 text-hecate-300" : "text-surface-200 hover:bg-surface-600"}`), _(je, s(j).name);
        }), f(Ie, B);
      });
      var ke = l(te, 2);
      ke.__click = () => {
        Oa(), x(b, !1);
      }, n(E), f(A, E);
    };
    P(pe, (A) => {
      s(b) && A(Ne);
    });
  }
  n(Ve);
  var ne = l(Ve, 2), k = i(ne, !0);
  n(ne);
  var F = l(ne, 2);
  {
    var Fe = (A) => {
      var E = Ac(), te = i(E, !0);
      n(E), C(() => _(te, r().brief)), f(A, E);
    };
    P(F, (A) => {
      r()?.brief && A(Fe);
    });
  }
  var ze = l(F, 4);
  {
    var Xe = (A) => {
      var E = Dc(), te = i(E);
      n(E), C(() => _(te, `${c().length ?? ""} division${c().length !== 1 ? "s" : ""}`)), f(A, E);
    };
    P(ze, (A) => {
      c().length > 0 && A(Xe);
    });
  }
  var Ee = l(ze, 2);
  {
    var Ce = (A) => {
      var E = Pc();
      f(A, E);
    }, me = (A) => {
      var E = Tc();
      E.__click = R, C(() => E.disabled = d()), f(A, E);
    }, fe = (A) => {
      var E = Mc(), te = ut(E);
      te.__click = () => x(g, !s(g));
      var ke = l(te, 2);
      ke.__click = () => r() && ro(r().venture_id);
      var Ie = l(ke, 2);
      {
        var j = (B) => {
          var xe = Rc();
          xe.__click = () => r() && ao(r().venture_id), C(() => xe.disabled = d()), f(B, xe);
        };
        P(Ie, (B) => {
          c().length > 0 && B(j);
        });
      }
      C(() => {
        te.disabled = d(), ke.disabled = d();
      }), f(A, E);
    }, de = (A) => {
      var E = Ic();
      E.__click = () => r() && so(r().venture_id), C(() => E.disabled = d()), f(A, E);
    };
    P(Ee, (A) => {
      o() === "initiated" || o() === "vision_refined" ? A(Ce) : o() === "vision_submitted" ? A(me, 1) : o() === "discovering" ? A(fe, 2) : o() === "discovery_paused" && A(de, 3);
    });
  }
  n(ae);
  var H = l(ae, 2);
  {
    var M = (A) => {
      var E = Lc(), te = i(E, !0);
      n(E), C(() => _(te, u())), f(A, E);
    };
    P(H, (A) => {
      u() && A(M);
    });
  }
  var I = l(H, 2);
  {
    var z = (A) => {
      var E = Nc(), te = i(E), ke = l(i(te), 2);
      js(ke), zt(ke, "rows", 2), n(te);
      var Ie = l(te, 2);
      Ie.__click = $;
      var j = l(Ie, 2);
      j.__click = () => x(w, !1), n(E), C((B) => Ie.disabled = B, [() => !s(T).trim() || d()]), xt(ke, () => s(T), (B) => x(T, B)), f(A, E);
    };
    P(I, (A) => {
      s(w) && A(z);
    });
  }
  var Oe = l(I, 2);
  {
    var Ye = (A) => {
      var E = Oc(), te = i(E), ke = l(i(te), 2);
      bt(ke), n(te);
      var Ie = l(te, 2), j = l(i(Ie), 2);
      bt(j), n(Ie);
      var B = l(Ie, 2);
      B.__click = K;
      var xe = l(B, 2);
      xe.__click = () => x(g, !1), n(E), C((je) => B.disabled = je, [() => !s(S).trim() || d()]), xt(ke, () => s(S), (je) => x(S, je)), xt(j, () => s(W), (je) => x(W, je)), f(A, E);
    };
    P(Oe, (A) => {
      s(g) && A(Ye);
    });
  }
  n(ie), C(
    (A) => {
      _(N, `${r()?.name ?? "Venture" ?? ""} `), Re(ne, 1, `text-[10px] px-2 py-0.5 rounded-full border ${A ?? ""}`), _(k, r()?.status_label ?? "New");
    },
    [() => ge(o())]
  ), f(e, ie), St(), h();
}
Ot(["click"]);
Dt(no, {}, [], [], { mode: "open" });
var jc = /* @__PURE__ */ p('<p class="text-xs text-surface-300 mt-1.5 max-w-md mx-auto"> </p>'), Vc = /* @__PURE__ */ p("<span></span>"), Bc = /* @__PURE__ */ p('<div class="flex items-center gap-1"><div class="flex flex-col items-center gap-0.5 px-2"><span> </span> <span> </span></div> <!></div>'), qc = /* @__PURE__ */ p('<div class="rounded-lg border border-surface-600 bg-surface-800 p-3 col-span-2"><div class="text-[9px] text-surface-400 uppercase tracking-wider mb-1">Repository</div> <div class="text-xs text-surface-200 font-mono"> </div></div>'), Hc = /* @__PURE__ */ p('<div class="rounded-lg border border-hecate-600/30 bg-hecate-600/5 p-5 text-center"><div class="text-xs text-surface-200 mb-3">Your venture repo has been scaffolded. The next step is <strong class="text-hecate-300">Big Picture Event Storming</strong> </div> <button> </button></div>'), Gc = /* @__PURE__ */ p(`<div class="rounded-lg border border-surface-600 bg-surface-800 p-5 text-center"><div class="text-xs text-surface-200 mb-2">Discovery is complete. Identify divisions (bounded contexts)
						from the events you discovered.</div> <div class="text-[10px] text-surface-400">Use the header controls to identify divisions.</div></div>`), Wc = /* @__PURE__ */ p('<div class="rounded-lg border border-surface-600 bg-surface-800 p-5 text-center"><div class="text-xs text-surface-200">Continue from the header controls to advance through the lifecycle.</div></div>'), Uc = /* @__PURE__ */ p('<div class="text-center"><div class="text-3xl mb-3 text-hecate-400"></div> <h2 class="text-lg font-semibold text-surface-100"> </h2> <!></div> <div class="flex items-center justify-center gap-1 py-4"></div> <div class="grid grid-cols-2 gap-3"><div class="rounded-lg border border-surface-600 bg-surface-800 p-3"><div class="text-[9px] text-surface-400 uppercase tracking-wider mb-1">Status</div> <div class="text-xs text-surface-100"> </div></div> <div class="rounded-lg border border-surface-600 bg-surface-800 p-3"><div class="text-[9px] text-surface-400 uppercase tracking-wider mb-1">Initiated</div> <div class="text-xs text-surface-100"> </div></div> <!></div> <!>', 1), zc = /* @__PURE__ */ p('<div class="flex flex-col h-full overflow-y-auto"><div class="max-w-2xl mx-auto w-full p-8 space-y-6"><!></div></div>');
function Us(e, t) {
  Ct(t, !0);
  const r = () => $e($t, "$activeVenture", c), a = () => $e(da, "$ventureStep", c), o = () => $e(wt, "$isLoading", c), [c, d] = qt();
  let u = ft(t, "nextAction", 7);
  function v(K) {
    return K ? new Date(K * 1e3).toLocaleDateString("en-US", {
      year: "numeric",
      month: "short",
      day: "numeric",
      hour: "2-digit",
      minute: "2-digit"
    }) : "";
  }
  async function h() {
    if (!r()) return;
    await xn(r().venture_id) && (await yr(), await ua());
  }
  const w = [
    { key: "vision", label: "Vision", icon: "◇" },
    { key: "discovery", label: "Discovery", icon: "○" },
    { key: "design", label: "Design", icon: "△" },
    { key: "plan", label: "Plan", icon: "□" },
    { key: "implement", label: "Implement", icon: "⚙" },
    { key: "deploy", label: "Deploy", icon: "▲" },
    { key: "monitor", label: "Monitor", icon: "◉" },
    { key: "rescue", label: "Rescue", icon: "↺" }
  ];
  let g = /* @__PURE__ */ Te(() => {
    const K = a();
    return K === "initiated" || K === "vision_refined" || K === "vision_submitted" ? 0 : K === "discovering" || K === "discovery_paused" || K === "discovery_completed" ? 1 : 0;
  });
  var b = {
    get nextAction() {
      return u();
    },
    set nextAction(K) {
      u(K), vt();
    }
  }, T = zc(), S = i(T), W = i(S);
  {
    var $ = (K) => {
      var ge = Uc(), ie = ut(ge), ae = i(ie);
      ae.textContent = "◆";
      var Z = l(ae, 2), De = i(Z, !0);
      n(Z);
      var Ve = l(Z, 2);
      {
        var Ge = (fe) => {
          var de = jc(), H = i(de, !0);
          n(de), C(() => _(H, r().brief)), f(fe, de);
        };
        P(Ve, (fe) => {
          r().brief && fe(Ge);
        });
      }
      n(ie);
      var ve = l(ie, 2);
      He(ve, 21, () => w, pt, (fe, de, H) => {
        const M = /* @__PURE__ */ Te(() => H < s(g)), I = /* @__PURE__ */ Te(() => H === s(g)), z = /* @__PURE__ */ Te(() => H === s(g) + 1);
        var Oe = Bc(), Ye = i(Oe), A = i(Ye), E = i(A, !0);
        n(A);
        var te = l(A, 2), ke = i(te, !0);
        n(te), n(Ye);
        var Ie = l(Ye, 2);
        {
          var j = (B) => {
            var xe = Vc();
            xe.textContent = "→", C(() => Re(xe, 1, `text-[10px]
									${s(M) ? "text-health-ok/40" : "text-surface-700"}`)), f(B, xe);
          };
          P(Ie, (B) => {
            H < w.length - 1 && B(j);
          });
        }
        n(Oe), C(() => {
          zt(Ye, "title", s(de).label), Re(A, 1, `text-sm transition-colors
									${s(M) ? "text-health-ok" : s(I) ? "text-hecate-400" : "text-surface-600"}`), _(E, s(M) ? "✓" : s(de).icon), Re(te, 1, `text-[9px] transition-colors
									${s(M) ? "text-health-ok/70" : s(I) ? "text-hecate-300" : s(z) ? "text-surface-400" : "text-surface-600"}`), _(ke, s(de).label);
        }), f(fe, Oe);
      }), n(ve);
      var N = l(ve, 2), U = i(N), pe = l(i(U), 2), Ne = i(pe, !0);
      n(pe), n(U);
      var ne = l(U, 2), k = l(i(ne), 2), F = i(k, !0);
      n(k), n(ne);
      var Fe = l(ne, 2);
      {
        var ze = (fe) => {
          var de = qc(), H = l(i(de), 2), M = i(H, !0);
          n(H), n(de), C(() => _(M, r().repos[0])), f(fe, de);
        };
        P(Fe, (fe) => {
          r().repos && r().repos.length > 0 && fe(ze);
        });
      }
      n(N);
      var Xe = l(N, 2);
      {
        var Ee = (fe) => {
          var de = Hc(), H = i(de), M = l(i(H), 2);
          M.nodeValue = " — discover the domain events that define your system.", n(H);
          var I = l(H, 2);
          I.__click = h;
          var z = i(I, !0);
          n(I), n(de), C(() => {
            I.disabled = o(), Re(I, 1, `px-5 py-2.5 rounded-lg text-sm font-medium transition-colors
							${o() ? "bg-surface-600 text-surface-400 cursor-not-allowed" : "bg-hecate-600 text-surface-50 hover:bg-hecate-500"}`), _(z, o() ? "Starting..." : "Start Discovery");
          }), f(fe, de);
        }, Ce = (fe) => {
          var de = Gc();
          f(fe, de);
        }, me = (fe) => {
          var de = Wc();
          f(fe, de);
        };
        P(Xe, (fe) => {
          u() === "discovery" && a() === "vision_submitted" ? fe(Ee) : u() === "identify" ? fe(Ce, 1) : fe(me, !1);
        });
      }
      C(
        (fe) => {
          _(De, r().name), _(Ne, r().status_label), _(F, fe);
        },
        [() => v(r().initiated_at ?? 0)]
      ), f(K, ge);
    };
    P(W, (K) => {
      r() && K($);
    });
  }
  n(S), n(T), f(e, T);
  var R = St(b);
  return d(), R;
}
Ot(["click"]);
Dt(Us, { nextAction: {} }, [], [], { mode: "open" });
Qo();
var Yc = /* @__PURE__ */ p("<button><span> </span> <span> </span> <span> </span></button>"), Kc = /* @__PURE__ */ p('<div class="ml-2 mt-1 space-y-0.5"></div>'), Jc = /* @__PURE__ */ p('<div class="mb-2"><button><span class="font-medium"> </span></button> <!></div>'), Qc = /* @__PURE__ */ p('<div class="text-[10px] text-surface-400 px-2 py-4 text-center">No divisions yet. <br/> Start discovery to identify them.</div>'), Xc = /* @__PURE__ */ p('<div class="w-48 border-r border-surface-600 bg-surface-800/30 overflow-y-auto shrink-0"><div class="p-3"><div class="text-[10px] text-surface-400 uppercase tracking-wider mb-2">Divisions</div> <!> <!></div></div>');
function io(e, t) {
  Ct(t, !1);
  const r = () => $e(Zr, "$divisions", c), a = () => $e(Ps, "$selectedDivisionId", c), o = () => $e(hs, "$selectedPhase", c), [c, d] = qt();
  function u($) {
    Ps.set($);
  }
  function v($, R) {
    Ps.set($), hs.set(R);
  }
  function h($, R) {
    return $ ? R.length === 0 ? { icon: "●", css: "text-health-ok" } : R.includes("resume") ? { icon: "○", css: "text-health-warn" } : R.includes("shelve") || R.includes("conclude") || R.includes("archive") ? { icon: "◐", css: "text-hecate-400" } : R.includes("open") ? { icon: "○", css: "text-surface-300" } : { icon: "○", css: "text-surface-500" } : { icon: "○", css: "text-surface-500" };
  }
  function w($, R) {
    return $ ? R.length === 0 ? "text-health-ok" : R.includes("resume") ? "text-health-warn" : R.includes("shelve") || R.includes("conclude") || R.includes("archive") ? "text-hecate-400" : "text-surface-300" : "text-surface-500";
  }
  Hi();
  var g = Xc(), b = i(g), T = l(i(b), 2);
  He(T, 1, r, pt, ($, R) => {
    const K = /* @__PURE__ */ _r(() => a() === s(R).division_id);
    var ge = Jc(), ie = i(ge);
    ie.__click = () => u(s(R).division_id);
    var ae = i(ie), Z = i(ae, !0);
    n(ae), n(ie);
    var De = l(ie, 2);
    {
      var Ve = (Ge) => {
        var ve = Kc();
        He(ve, 5, () => Kr, pt, (N, U) => {
          const pe = /* @__PURE__ */ _r(() => Ia(s(R), s(U).code)), Ne = /* @__PURE__ */ _r(() => La(s(R), s(U).code)), ne = /* @__PURE__ */ _r(() => {
            const { icon: me, css: fe } = h(s(pe), s(Ne));
            return { icon: me, css: fe };
          });
          var k = Yc();
          k.__click = () => v(s(R).division_id, s(U).code);
          var F = i(k), Fe = i(F, !0);
          n(F);
          var ze = l(F, 2), Xe = i(ze, !0);
          n(ze);
          var Ee = l(ze, 2), Ce = i(Ee, !0);
          n(Ee), n(k), C(
            (me) => {
              Re(k, 1, `w-full flex items-center gap-1.5 px-2 py-0.5 rounded text-[10px]
									transition-colors
									${o() === s(U).code ? "bg-surface-600/50 text-surface-100" : "text-surface-400 hover:text-surface-300"}`), Re(F, 1, Ml(s(ne).css)), _(Fe, s(ne).icon), _(Xe, s(U).shortName), Re(Ee, 1, `ml-auto text-[9px] ${me ?? ""}`), _(Ce, s(pe) || "Pending");
            },
            [() => w(s(pe), s(Ne))]
          ), f(N, k);
        }), n(ve), f(Ge, ve);
      };
      P(De, (Ge) => {
        s(K) && Ge(Ve);
      });
    }
    n(ge), C(() => {
      Re(ie, 1, `w-full text-left px-2 py-1.5 rounded text-xs transition-colors
						${s(K) ? "bg-surface-700 text-surface-100" : "text-surface-300 hover:bg-surface-700/50 hover:text-surface-100"}`), _(Z, s(R).context_name);
    }), f($, ge);
  });
  var S = l(T, 2);
  {
    var W = ($) => {
      var R = Qc();
      f($, R);
    };
    P(S, ($) => {
      r().length === 0 && $(W);
    });
  }
  n(b), n(g), f(e, g), St(), d();
}
Ot(["click"]);
Dt(io, {}, [], [], { mode: "open" });
const oo = Ze(
  "You are Martha, an AI assistant specializing in software architecture and domain-driven design."
), Zc = `You are The Oracle, a vision architect. You interview the user about their venture and build a vision document.

RULES:
1. Ask ONE question per response. Keep it short (2-3 sentences + question).
2. After EVERY response, include a vision draft inside a \`\`\`markdown code fence.
3. Cover 5 topics: Problem, Users, Capabilities, Constraints, Success Criteria.

Be warm but direct. Push for specifics when answers are vague.`, ed = "Be concise and practical. Suggest specific, actionable items. When suggesting domain elements, use snake_case naming. When suggesting events, use the format: {subject}_{verb_past}_v{N}.", td = [
  {
    id: "oracle",
    name: "The Oracle",
    role: "Domain Expert",
    icon: "◇",
    description: "Rapid-fires domain events from the vision document",
    prompt: "You are The Oracle, a domain expert participating in a Big Picture Event Storming session. Your job is to rapidly identify domain events. Output ONLY event names in past tense business language. One per line. Be fast, be prolific."
  },
  {
    id: "architect",
    name: "The Architect",
    role: "Boundary Spotter",
    icon: "△",
    description: "Identifies natural context boundaries between event clusters",
    prompt: "You are The Architect, a DDD strategist. Given the events on the board, identify BOUNDED CONTEXT BOUNDARIES. Name the candidate contexts (divisions) and list which events belong to each."
  },
  {
    id: "advocate",
    name: "The Advocate",
    role: "Devil's Advocate",
    icon: "★",
    description: "Challenges context boundaries and finds missing events",
    prompt: "You are The Advocate. Identify MISSING events and challenge proposed boundaries. Be specific and constructive."
  },
  {
    id: "scribe",
    name: "The Scribe",
    role: "Integration Mapper",
    icon: "□",
    description: "Maps how contexts communicate via integration facts",
    prompt: 'You are The Scribe. Map INTEGRATION FACTS between contexts. Use: "Context A publishes fact_name -> Context B".'
  }
], rd = [
  {
    id: "oracle",
    name: "The Oracle",
    role: "Domain Expert",
    icon: "◇",
    description: "Identifies domain events and business processes",
    prompt: "You are The Oracle for Design-Level Event Storming. Identify business events in past tense snake_case_v1 format with one-line rationale each."
  },
  {
    id: "architect",
    name: "The Architect",
    role: "Technical Lead",
    icon: "△",
    description: "Identifies aggregates and structural patterns",
    prompt: "You are The Architect for Design-Level Event Storming. Identify aggregate boundaries and explain which events cluster around them."
  },
  {
    id: "advocate",
    name: "The Advocate",
    role: "Devil's Advocate",
    icon: "★",
    description: "Questions assumptions and identifies edge cases",
    prompt: "You are The Advocate. Find problems, edge cases, and hotspots. Challenge every assumption."
  },
  {
    id: "scribe",
    name: "The Scribe",
    role: "Process Analyst",
    icon: "□",
    description: "Organizes discoveries and identifies read models",
    prompt: "You are The Scribe. Identify read models and policies. Focus on queryable information and domain rules."
  }
], sd = Ze(td), ad = Ze(rd), nd = Ze(Zc), id = Ze(ed);
function od(e, t) {
  return e.replace(/\{\{(\w+)\}\}/g, (r, a) => t[a] ?? `{{${a}}}`);
}
var ld = /* @__PURE__ */ p('<span class="text-[8px] text-hecate-400"></span>'), cd = /* @__PURE__ */ p('<span class="truncate"> </span> <!>', 1), dd = /* @__PURE__ */ p('<span class="text-surface-500">Select model</span>'), ud = /* @__PURE__ */ p('<span class="text-hecate-400 ml-1">(code-optimized)</span>'), vd = /* @__PURE__ */ p('<button class="text-[9px] text-surface-500 hover:text-surface-300" title="Clear pinned model for this phase">Unpin</button>'), fd = /* @__PURE__ */ p('<div class="px-2 py-1.5 border-b border-surface-700 flex items-center justify-between"><span class="text-[9px] text-surface-400">Phase: <span class="text-surface-200"> </span> <!></span> <!></div>'), pd = /* @__PURE__ */ p('<div class="p-3 text-center text-[11px] text-surface-500"> </div>'), xd = /* @__PURE__ */ p('<span class="text-[8px] text-hecate-400 shrink-0" title="Code model"></span>'), hd = /* @__PURE__ */ p('<span class="text-[8px] text-hecate-400 shrink-0" title="Pinned for this phase"></span>'), _d = /* @__PURE__ */ p('<span class="text-[9px] text-hecate-400 shrink-0"></span>'), gd = /* @__PURE__ */ p('<button class="text-[8px] text-surface-600 hover:text-hecate-400 shrink-0">pin</button>'), bd = /* @__PURE__ */ p('<div><span class="truncate flex-1"> </span> <!> <!> <!> <!></div>'), md = /* @__PURE__ */ p('<div class="py-1"><div class="px-2 py-1 text-[9px] text-surface-500 uppercase tracking-wider font-medium"> </div> <!></div>'), yd = /* @__PURE__ */ p(`<div class="absolute top-full left-0 mt-1 w-72 max-h-80 overflow-hidden
				bg-surface-800 border border-surface-600 rounded-lg shadow-xl z-50
				flex flex-col"><div class="p-2 border-b border-surface-700"><input placeholder="Search models..." class="w-full bg-surface-700 border border-surface-600 rounded px-2 py-1
						text-[11px] text-surface-100 placeholder-surface-500
						focus:outline-none focus:border-hecate-500"/></div> <!> <div class="overflow-y-auto flex-1"><!> <!></div></div>`), wd = /* @__PURE__ */ p(`<div class="relative"><button class="text-[10px] px-2 py-0.5 rounded bg-surface-700 text-surface-300
			hover:bg-surface-600 transition-colors truncate max-w-[180px] flex items-center gap-1"><!> <span class="text-[8px] ml-0.5"> </span></button> <!></div>`);
function xa(e, t) {
  Ct(t, !0);
  const r = () => $e(vn, "$availableModels", a), [a, o] = qt();
  let c = ft(t, "currentModel", 7), d = ft(t, "onSelect", 7), u = ft(t, "showPhaseInfo", 7, !1), v = ft(t, "phasePreference", 7, null), h = ft(t, "phaseAffinity", 7, "general"), w = ft(t, "onPinModel", 7), g = ft(t, "onClearPin", 7), b = ft(t, "phaseName", 7, ""), T = /* @__PURE__ */ se(!1), S = /* @__PURE__ */ se(""), W = /* @__PURE__ */ se(void 0), $ = /* @__PURE__ */ Te(() => {
    const k = r(), F = s(S).toLowerCase(), Fe = F ? k.filter((Ee) => Ee.toLowerCase().includes(F)) : k, ze = /* @__PURE__ */ new Map();
    for (const Ee of Fe) {
      const Ce = R(Ee), me = ze.get(Ce) ?? [];
      me.push(Ee), ze.set(Ce, me);
    }
    const Xe = [];
    for (const [Ee, Ce] of ze)
      Xe.push({ provider: Ee, models: Ce });
    return Xe;
  });
  function R(k) {
    return k.startsWith("claude") || k.startsWith("anthropic") ? "Anthropic" : k.startsWith("gemini") || k.startsWith("gemma") ? "Google" : k.startsWith("llama") || k.startsWith("meta-llama") ? "Meta" : k.startsWith("qwen") ? "Alibaba" : k.startsWith("groq/") ? "Groq" : k.startsWith("openai/") || k.startsWith("gpt") ? "OpenAI" : k.includes("/") ? k.split("/")[0] : "Other";
  }
  function K(k) {
    d()(k), x(T, !1), x(S, "");
  }
  function ge(k) {
    s(W) && !s(W).contains(k.target) && (x(T, !1), x(S, ""));
  }
  function ie(k) {
    return k.length <= 24 ? k : k.slice(0, 22) + "…";
  }
  Pt(() => (s(T) ? document.addEventListener("click", ge, !0) : document.removeEventListener("click", ge, !0), () => document.removeEventListener("click", ge, !0)));
  var ae = {
    get currentModel() {
      return c();
    },
    set currentModel(k) {
      c(k), vt();
    },
    get onSelect() {
      return d();
    },
    set onSelect(k) {
      d(k), vt();
    },
    get showPhaseInfo() {
      return u();
    },
    set showPhaseInfo(k = !1) {
      u(k), vt();
    },
    get phasePreference() {
      return v();
    },
    set phasePreference(k = null) {
      v(k), vt();
    },
    get phaseAffinity() {
      return h();
    },
    set phaseAffinity(k = "general") {
      h(k), vt();
    },
    get onPinModel() {
      return w();
    },
    set onPinModel(k) {
      w(k), vt();
    },
    get onClearPin() {
      return g();
    },
    set onClearPin(k) {
      g(k), vt();
    },
    get phaseName() {
      return b();
    },
    set phaseName(k = "") {
      b(k), vt();
    }
  }, Z = wd(), De = i(Z);
  De.__click = () => x(T, !s(T));
  var Ve = i(De);
  {
    var Ge = (k) => {
      var F = cd(), Fe = ut(F), ze = i(Fe, !0);
      n(Fe);
      var Xe = l(Fe, 2);
      {
        var Ee = (me) => {
          var fe = ld();
          fe.textContent = "•", f(me, fe);
        }, Ce = /* @__PURE__ */ Te(() => Nn(c()) === "code");
        P(Xe, (me) => {
          s(Ce) && me(Ee);
        });
      }
      C((me) => _(ze, me), [() => ie(c())]), f(k, F);
    }, ve = (k) => {
      var F = dd();
      f(k, F);
    };
    P(Ve, (k) => {
      c() ? k(Ge) : k(ve, !1);
    });
  }
  var N = l(Ve, 2), U = i(N, !0);
  n(N), n(De);
  var pe = l(De, 2);
  {
    var Ne = (k) => {
      var F = yd(), Fe = i(F), ze = i(Fe);
      bt(ze), n(Fe);
      var Xe = l(Fe, 2);
      {
        var Ee = (H) => {
          var M = fd(), I = i(M), z = l(i(I)), Oe = i(z, !0);
          n(z);
          var Ye = l(z, 2);
          {
            var A = (ke) => {
              var Ie = ud();
              f(ke, Ie);
            };
            P(Ye, (ke) => {
              h() === "code" && ke(A);
            });
          }
          n(I);
          var E = l(I, 2);
          {
            var te = (ke) => {
              var Ie = vd();
              Ie.__click = () => g()?.(), f(ke, Ie);
            };
            P(E, (ke) => {
              v() && ke(te);
            });
          }
          n(M), C(() => _(Oe, b())), f(H, M);
        };
        P(Xe, (H) => {
          u() && b() && H(Ee);
        });
      }
      var Ce = l(Xe, 2), me = i(Ce);
      {
        var fe = (H) => {
          var M = pd(), I = i(M, !0);
          n(M), C(() => _(I, r().length === 0 ? "No models available" : "No matching models")), f(H, M);
        };
        P(me, (H) => {
          s($).length === 0 && H(fe);
        });
      }
      var de = l(me, 2);
      He(de, 17, () => s($), pt, (H, M) => {
        var I = md(), z = i(I), Oe = i(z, !0);
        n(z);
        var Ye = l(z, 2);
        He(Ye, 17, () => s(M).models, pt, (A, E) => {
          const te = /* @__PURE__ */ Te(() => s(E) === c()), ke = /* @__PURE__ */ Te(() => s(E) === v());
          var Ie = bd();
          Ie.__click = () => K(s(E));
          var j = i(Ie), B = i(j, !0);
          n(j);
          var xe = l(j, 2);
          {
            var je = (Je) => {
              var L = xd();
              L.textContent = "• code", f(Je, L);
            }, Me = /* @__PURE__ */ Te(() => Nn(s(E)) === "code");
            P(xe, (Je) => {
              s(Me) && Je(je);
            });
          }
          var et = l(xe, 2);
          {
            var qe = (Je) => {
              var L = hd();
              L.textContent = "📌", f(Je, L);
            };
            P(et, (Je) => {
              s(ke) && Je(qe);
            });
          }
          var Ke = l(et, 2);
          {
            var We = (Je) => {
              var L = _d();
              L.textContent = "✓", f(Je, L);
            };
            P(Ke, (Je) => {
              s(te) && Je(We);
            });
          }
          var tt = l(Ke, 2);
          {
            var st = (Je) => {
              var L = gd();
              L.__click = (D) => {
                D.stopPropagation(), w()?.(s(E));
              }, C(() => zt(L, "title", `Pin for ${b() ?? ""} phase`)), f(Je, L);
            };
            P(tt, (Je) => {
              u() && w() && !s(ke) && Je(st);
            });
          }
          n(Ie), C(() => {
            Re(Ie, 1, `w-full text-left px-2 py-1.5 text-[11px] flex items-center gap-1.5
									transition-colors cursor-pointer
									${s(te) ? "bg-hecate-600/20 text-hecate-300" : "text-surface-200 hover:bg-surface-700"}`), _(B, s(E));
          }), f(A, Ie);
        }), n(I), C(() => _(Oe, s(M).provider)), f(H, I);
      }), n(Ce), n(F), xt(ze, () => s(S), (H) => x(S, H)), f(k, F);
    };
    P(pe, (k) => {
      s(T) && k(Ne);
    });
  }
  n(Z), ls(Z, (k) => x(W, k), () => s(W)), C(() => {
    zt(De, "title", c() ?? "No model selected"), _(U, s(T) ? "▲" : "▼");
  }), f(e, Z);
  var ne = St(ae);
  return o(), ne;
}
Ot(["click"]);
Dt(
  xa,
  {
    currentModel: {},
    onSelect: {},
    showPhaseInfo: {},
    phasePreference: {},
    phaseAffinity: {},
    onPinModel: {},
    onClearPin: {},
    phaseName: {}
  },
  [],
  [],
  { mode: "open" }
);
var kd = /* @__PURE__ */ p(`<div class="flex justify-end"><div class="max-w-[85%] rounded-lg px-3 py-2 text-[11px]
							bg-hecate-600/20 text-surface-100 border border-hecate-600/20"><div class="whitespace-pre-wrap break-words"> </div></div></div>`), $d = /* @__PURE__ */ p(`<details class="mb-1.5 group"><summary class="text-[10px] text-surface-400 cursor-pointer hover:text-surface-300
											select-none flex items-center gap-1"><span class="text-[9px] transition-transform group-open:rotate-90"></span> Reasoning</summary> <div class="mt-1 pl-2 border-l-2 border-surface-600 text-[10px] text-surface-400
											whitespace-pre-wrap break-words max-h-32 overflow-y-auto"> </div></details>`), Cd = /* @__PURE__ */ p('<div class="whitespace-pre-wrap break-words"> </div>'), Sd = /* @__PURE__ */ p('<div class="flex justify-start"><div></div></div>'), Ed = /* @__PURE__ */ p(`<details class="group"><summary class="text-[10px] text-surface-500 cursor-pointer hover:text-surface-400
										select-none flex items-center gap-1"><span class="text-[9px] transition-transform group-open:rotate-90"></span> Show reasoning</summary> <div class="mt-1 pl-2 border-l-2 border-surface-600 text-[10px] text-surface-400
										whitespace-pre-wrap break-words max-h-32 overflow-y-auto"> <span class="inline-block w-1 h-3 bg-accent-400/50 animate-pulse ml-0.5"></span></div></details>`), Ad = /* @__PURE__ */ p('<div class="flex items-center gap-2 text-surface-400 mb-1"><span class="flex gap-1"><span class="w-1.5 h-1.5 rounded-full bg-accent-500/60 animate-bounce" style="animation-delay: 0ms"></span> <span class="w-1.5 h-1.5 rounded-full bg-accent-500/60 animate-bounce" style="animation-delay: 150ms"></span> <span class="w-1.5 h-1.5 rounded-full bg-accent-500/60 animate-bounce" style="animation-delay: 300ms"></span></span> <span class="text-[10px] text-accent-400/70">Reasoning...</span></div> <!>', 1), Dd = /* @__PURE__ */ p(`<details class="mb-1.5 group"><summary class="text-[10px] text-surface-400 cursor-pointer hover:text-surface-300
										select-none flex items-center gap-1"><span class="text-[9px] transition-transform group-open:rotate-90"></span> Reasoning</summary> <div class="mt-1 pl-2 border-l-2 border-surface-600 text-[10px] text-surface-400
										whitespace-pre-wrap break-words max-h-32 overflow-y-auto"> </div></details>`), Pd = /* @__PURE__ */ p('<!> <div class="whitespace-pre-wrap break-words"> <span class="inline-block w-1.5 h-3 bg-hecate-400 animate-pulse ml-0.5"></span></div>', 1), Td = /* @__PURE__ */ p('<div class="flex items-center gap-1.5 text-surface-400"><span class="animate-bounce" style="animation-delay: 0ms">.</span> <span class="animate-bounce" style="animation-delay: 150ms">.</span> <span class="animate-bounce" style="animation-delay: 300ms">.</span></div>'), Rd = /* @__PURE__ */ p(`<div class="flex justify-start"><div class="max-w-[85%] rounded-lg px-3 py-2 text-[11px]
						bg-surface-700 text-surface-200 border border-surface-600"><!></div></div>`), Md = /* @__PURE__ */ p('<div class="flex items-center justify-center h-full"><div class="text-center text-surface-400"><div class="text-2xl mb-2"></div> <div class="text-[11px]">The Oracle is preparing...</div></div></div>'), Id = /* @__PURE__ */ p('<span class="text-[10px] text-health-ok"></span>'), Ld = /* @__PURE__ */ p('<span class="text-[10px] text-accent-400"></span>'), Nd = /* @__PURE__ */ p('<span class="text-[10px] text-surface-400"></span>'), Od = /* @__PURE__ */ p('<span class="text-[10px] text-surface-400">Waiting for Oracle...</span>'), Fd = /* @__PURE__ */ p('<div class="mt-4 p-2 rounded bg-surface-700 border border-surface-600"><div class="text-[9px] text-surface-400 uppercase tracking-wider mb-1">Brief</div> <div class="text-[11px] text-surface-200"> </div></div>'), jd = /* @__PURE__ */ p('<div class="prose prose-sm prose-invert"><!></div> <!>', 1), Vd = /* @__PURE__ */ p(`<div class="flex items-center justify-center h-full"><div class="text-center text-surface-400 max-w-[220px]"><div class="text-2xl mb-2"></div> <div class="text-[11px]">Your vision will take shape here as the Oracle
							gathers context about your venture.</div></div></div>`), Bd = /* @__PURE__ */ p('<div class="text-[10px] text-health-err bg-health-err/10 rounded px-2 py-1"> </div>'), qd = /* @__PURE__ */ p(`<div class="space-y-2"><div><label for="repo-path" class="text-[10px] text-surface-400 block mb-1">Repository Path</label> <input id="repo-path" placeholder="~/ventures/my-venture" class="w-full bg-surface-700 border border-surface-600 rounded px-3 py-1.5
								text-[11px] text-surface-100 placeholder-surface-400
								focus:outline-none focus:border-hecate-500"/></div> <!> <button> </button></div>`), Hd = /* @__PURE__ */ p('<div class="text-center text-[10px] text-surface-400 py-2"></div>'), Gd = /* @__PURE__ */ p('<div class="text-center text-[10px] text-surface-400 py-2">The Oracle will guide you through defining your venture</div>'), Wd = /* @__PURE__ */ p(`<div class="flex h-full overflow-hidden"><div class="flex flex-col overflow-hidden"><div class="flex items-center gap-2 px-4 py-2.5 border-b border-surface-600 shrink-0"><span class="text-hecate-400"></span> <span class="text-xs font-semibold text-surface-100">The Oracle</span> <span class="text-[10px] text-surface-400">Vision Architect</span> <div class="flex-1"></div> <!></div> <div class="flex-1 overflow-y-auto p-4 space-y-3"><!> <!> <!></div> <div class="border-t border-surface-600 p-3 shrink-0"><div class="flex gap-2"><textarea class="flex-1 bg-surface-700 border border-surface-600 rounded-lg px-3 py-2
						text-[11px] text-surface-100 placeholder-surface-400 resize-none
						focus:outline-none focus:border-hecate-500
						disabled:opacity-50 disabled:cursor-not-allowed"></textarea> <button>Send</button></div></div></div>  <div></div> <div class="flex flex-col overflow-hidden flex-1"><div class="flex items-center gap-2 px-4 py-2.5 border-b border-surface-600 shrink-0"><span class="text-surface-400 text-xs"></span> <span class="text-xs font-semibold text-surface-100">Vision Preview</span> <div class="flex-1"></div> <!></div> <div class="flex-1 overflow-y-auto p-4"><!></div> <div class="border-t border-surface-600 p-3 shrink-0"><!></div></div></div>`);
function lo(e, t) {
  Ct(t, !0);
  const r = () => $e($t, "$activeVenture", c), a = () => $e(fn, "$aiModel", c), o = () => $e(wt, "$isLoading", c), [c, d] = qt(), u = Yi();
  let v = /* @__PURE__ */ se(Ut([])), h = /* @__PURE__ */ se(""), w = /* @__PURE__ */ se(!1), g = /* @__PURE__ */ se(""), b = /* @__PURE__ */ se(void 0), T = /* @__PURE__ */ se(!1), S = /* @__PURE__ */ se(""), W = /* @__PURE__ */ se(""), $ = /* @__PURE__ */ se(null), R = /* @__PURE__ */ se(null), K = /* @__PURE__ */ se(65), ge = /* @__PURE__ */ se(!1), ie = /* @__PURE__ */ se(void 0);
  function ae(m) {
    let y = m.replace(/```markdown\n[\s\S]*?```/g, "◇ Vision updated ↗");
    return y = y.replace(/```markdown\n[\s\S]*$/, "◇ Synthesizing vision... ↗"), y;
  }
  function Z(m) {
    const y = ae(m), le = [];
    let O = y;
    for (; O.length > 0; ) {
      const re = O.indexOf("<think>");
      if (re === -1) {
        O.trim() && le.push({ type: "text", content: O });
        break;
      }
      if (re > 0) {
        const J = O.slice(0, re);
        J.trim() && le.push({ type: "text", content: J });
      }
      const he = O.indexOf("</think>", re);
      if (he === -1) {
        const J = O.slice(re + 7);
        J.trim() && le.push({ type: "think", content: J });
        break;
      }
      const we = O.slice(re + 7, he);
      we.trim() && le.push({ type: "think", content: we }), O = O.slice(he + 8);
    }
    return le.length > 0 ? le : [{ type: "text", content: y }];
  }
  function De(m) {
    return m.includes("<think>") && !m.includes("</think>");
  }
  function Ve(m) {
    const y = ae(m);
    return y.includes("</think>") ? (y.split("</think>").pop() || "").trim() : y.includes("<think>") ? "" : y;
  }
  function Ge(m) {
    const y = ae(m), le = y.indexOf("<think>");
    if (le === -1) return "";
    const O = y.indexOf("</think>");
    return O === -1 ? y.slice(le + 7) : y.slice(le + 7, O);
  }
  let ve = /* @__PURE__ */ Te(() => {
    for (let m = s(v).length - 1; m >= 0; m--)
      if (s(v)[m].role === "assistant") {
        const y = s(v)[m].content.match(/```markdown\n([\s\S]*?)```/);
        if (y) return y[1].trim();
      }
    if (s(g)) {
      const m = s(g).match(/```markdown\n([\s\S]*?)```/);
      if (m) return m[1].trim();
      const y = s(g).match(/```markdown\n([\s\S]*)$/);
      if (y) return y[1].trim();
    }
    return null;
  }), N = /* @__PURE__ */ Te(() => s(ve) !== null && !s(ve).includes("(Not yet explored)") && !s(ve).includes("*(Hypothetical)*")), U = /* @__PURE__ */ Te(() => {
    if (!s(ve)) return null;
    const m = s(ve).match(/<!--\s*brief:\s*(.*?)\s*-->/);
    return m ? m[1].trim() : null;
  }), pe = /* @__PURE__ */ se(null);
  Pt(() => {
    const m = r(), y = m?.venture_id ?? null;
    if (y !== s(pe) && (x(v, [], !0), x(g, ""), x(w, !1), x(S, ""), x(W, ""), x(pe, y, !0)), m && !s(W)) {
      const le = "~/ventures", O = m.name.toLowerCase().replace(/[^a-z0-9-]/g, "-");
      x(W, `${le}/${O}`);
    }
  }), Pt(() => {
    const m = a();
    s(R) !== null && s(R) !== m && (s($) && (s($).cancel(), x($, null)), x(v, [], !0), x(g, ""), x(w, !1)), x(R, m, !0);
  }), Pt(() => {
    const m = r();
    if (m && s(v).length === 0 && !s(w)) {
      const y = `I just initiated a new venture called "${m.name}". ${m.brief ? `Here's what I know so far: ${m.brief}` : "I need help defining the vision for this venture."}`;
      ne(y);
    }
  });
  function Ne() {
    const m = [], y = Jt(oo);
    y && m.push(y);
    const le = Jt(nd);
    if (m.push(od(le, { venture_name: r()?.name ?? "Unnamed" })), r()) {
      let O = `The venture is called "${r().name}"`;
      r().brief && (O += `. Initial brief: ${r().brief}`), m.push(O);
    }
    return m.join(`

---

`);
  }
  async function ne(m) {
    const y = a();
    if (!y || !m.trim() || s(w)) return;
    const le = { role: "user", content: m.trim() };
    x(v, [...s(v), le], !0), x(h, "");
    const O = [], re = Ne();
    re && O.push({ role: "system", content: re }), O.push(...s(v)), x(w, !0), x(g, "");
    let he = "";
    const we = u.stream.chat(y, O);
    x($, we, !0), we.onChunk((J) => {
      J.content && (he += J.content, x(g, he, !0));
    }).onDone(async (J) => {
      J.content && (he += J.content);
      const V = {
        role: "assistant",
        content: he || "(empty response)"
      };
      x(v, [...s(v), V], !0), x(g, ""), x(w, !1), x($, null);
    }).onError((J) => {
      const V = { role: "assistant", content: `Error: ${J}` };
      x(v, [...s(v), V], !0), x(g, ""), x(w, !1), x($, null);
    });
    try {
      await we.start();
    } catch (J) {
      const V = { role: "assistant", content: `Error: ${String(J)}` };
      x(v, [...s(v), V], !0), x(w, !1);
    }
  }
  async function k() {
    if (!r() || !s(ve) || !s(W).trim()) return;
    x(T, !0), x(S, ""), await eo(r().venture_id, s(W).trim(), s(ve), r().name, s(U) ?? void 0) ? (await yr(), await ua()) : x(S, Jt(pr) || "Failed to scaffold venture repo", !0), x(T, !1);
  }
  let F = /* @__PURE__ */ se(void 0);
  function Fe(m) {
    m.key === "Enter" && !m.shiftKey && (m.preventDefault(), ne(s(h)), s(F) && (s(F).style.height = "auto"));
  }
  function ze(m) {
    const y = m.target;
    y.style.height = "auto", y.style.height = Math.min(y.scrollHeight, 150) + "px";
  }
  function Xe(m) {
    x(ge, !0), m.preventDefault();
  }
  function Ee(m) {
    if (!s(ge) || !s(ie)) return;
    const y = s(ie).getBoundingClientRect(), O = (m.clientX - y.left) / y.width * 100;
    x(K, Math.max(30, Math.min(80, O)), !0);
  }
  function Ce() {
    x(ge, !1);
  }
  Pt(() => {
    s(v), s(g), ln().then(() => {
      s(b) && (s(b).scrollTop = s(b).scrollHeight);
    });
  });
  function me(m) {
    return m.replace(/<!--.*?-->/gs, "").replace(/^### (.*$)/gm, '<h3 class="text-xs font-semibold text-surface-100 mt-3 mb-1">$1</h3>').replace(/^## (.*$)/gm, '<h2 class="text-sm font-semibold text-hecate-300 mt-4 mb-1.5">$1</h2>').replace(/^# (.*$)/gm, '<h1 class="text-base font-bold text-surface-100 mb-2">$1</h1>').replace(/^(\d+)\.\s+(.*$)/gm, '<div class="text-[11px] text-surface-200 ml-3 mb-1"><span class="text-surface-400 mr-1.5">$1.</span>$2</div>').replace(/^\- (.*$)/gm, '<div class="text-[11px] text-surface-200 ml-3 mb-1"><span class="text-surface-400 mr-1.5">&bull;</span>$1</div>').replace(/\*\*(.*?)\*\*/g, '<strong class="text-surface-100">$1</strong>').replace(/\*(.*?)\*/g, '<em class="text-surface-300">$1</em>').replace(/\n\n/g, "<br/><br/>").trim();
  }
  var fe = Wd();
  fe.__mousemove = Ee, fe.__mouseup = Ce;
  var de = i(fe), H = i(de), M = i(H);
  M.textContent = "◇";
  var I = l(M, 8);
  xa(I, {
    get currentModel() {
      return a();
    },
    onSelect: (m) => pn(m)
  }), n(H);
  var z = l(H, 2), Oe = i(z);
  He(Oe, 17, () => s(v), pt, (m, y) => {
    var le = br(), O = ut(le);
    {
      var re = (we) => {
        var J = kd(), V = i(J), Y = i(V), ye = i(Y, !0);
        n(Y), n(V), n(J), C(() => _(ye, s(y).content)), f(we, J);
      }, he = (we) => {
        var J = Sd(), V = i(J);
        He(V, 21, () => Z(s(y).content), pt, (Y, ye) => {
          var _e = br(), ce = ut(_e);
          {
            var Q = (q) => {
              var ue = $d(), be = i(ue), Pe = i(be);
              Pe.textContent = "▶", Et(), n(be);
              var Be = l(be, 2), Qe = i(Be, !0);
              n(Be), n(ue), C((rt) => _(Qe, rt), [() => s(ye).content.trim()]), f(q, ue);
            }, X = (q) => {
              var ue = Cd(), be = i(ue, !0);
              n(ue), C((Pe) => _(be, Pe), [() => s(ye).content.trim()]), f(q, ue);
            };
            P(ce, (q) => {
              s(ye).type === "think" ? q(Q) : q(X, !1);
            });
          }
          f(Y, _e);
        }), n(V), n(J), C(
          (Y) => Re(V, 1, `max-w-[85%] rounded-lg px-3 py-2 text-[11px]
							bg-surface-700 text-surface-200 border border-surface-600
							${Y ?? ""}`),
          [
            () => s(y).content.startsWith("Error:") ? "border-health-err/30 text-health-err" : ""
          ]
        ), f(we, J);
      };
      P(O, (we) => {
        s(y).role === "user" ? we(re) : s(y).role === "assistant" && we(he, 1);
      });
    }
    f(m, le);
  });
  var Ye = l(Oe, 2);
  {
    var A = (m) => {
      var y = Rd(), le = i(y), O = i(le);
      {
        var re = (V) => {
          var Y = Ad(), ye = l(ut(Y), 2);
          {
            var _e = (Q) => {
              var X = Ed(), q = i(X), ue = i(q);
              ue.textContent = "▶", Et(), n(q);
              var be = l(q, 2), Pe = i(be, !0);
              Et(), n(be), n(X), C((Be) => _(Pe, Be), [
                () => Ge(s(g)).trim()
              ]), f(Q, X);
            }, ce = /* @__PURE__ */ Te(() => Ge(s(g)).trim());
            P(ye, (Q) => {
              s(ce) && Q(_e);
            });
          }
          f(V, Y);
        }, he = /* @__PURE__ */ Te(() => s(g) && De(s(g))), we = (V) => {
          var Y = Pd(), ye = ut(Y);
          {
            var _e = (q) => {
              var ue = Dd(), be = i(ue), Pe = i(be);
              Pe.textContent = "▶", Et(), n(be);
              var Be = l(be, 2), Qe = i(Be, !0);
              n(Be), n(ue), C((rt) => _(Qe, rt), [
                () => Ge(s(g)).trim()
              ]), f(q, ue);
            }, ce = /* @__PURE__ */ Te(() => Ge(s(g)).trim());
            P(ye, (q) => {
              s(ce) && q(_e);
            });
          }
          var Q = l(ye, 2), X = i(Q, !0);
          Et(), n(Q), C((q) => _(X, q), [() => Ve(s(g))]), f(V, Y);
        }, J = (V) => {
          var Y = Td();
          f(V, Y);
        };
        P(O, (V) => {
          s(he) ? V(re) : s(g) ? V(we, 1) : V(J, !1);
        });
      }
      n(le), n(y), f(m, y);
    };
    P(Ye, (m) => {
      s(w) && m(A);
    });
  }
  var E = l(Ye, 2);
  {
    var te = (m) => {
      var y = Md(), le = i(y), O = i(le);
      O.textContent = "◇", Et(2), n(le), n(y), f(m, y);
    };
    P(E, (m) => {
      s(v).length === 0 && !s(w) && m(te);
    });
  }
  n(z), ls(z, (m) => x(b, m), () => s(b));
  var ke = l(z, 2), Ie = i(ke), j = i(Ie);
  js(j), j.__keydown = Fe, j.__input = ze, zt(j, "rows", 1), ls(j, (m) => x(F, m), () => s(F));
  var B = l(j, 2);
  B.__click = () => ne(s(h)), n(Ie), n(ke), n(de);
  var xe = l(de, 2);
  xe.__mousedown = Xe;
  var je = l(xe, 2), Me = i(je), et = i(Me);
  et.textContent = "📄";
  var qe = l(et, 6);
  {
    var Ke = (m) => {
      var y = Id();
      y.textContent = "● Complete", f(m, y);
    }, We = (m) => {
      var y = Ld();
      y.textContent = "◐ Drafting...", f(m, y);
    }, tt = (m) => {
      var y = Nd();
      y.textContent = "◐ Listening...", f(m, y);
    }, st = (m) => {
      var y = Od();
      f(m, y);
    };
    P(qe, (m) => {
      s(N) ? m(Ke) : s(ve) ? m(We, 1) : s(w) ? m(tt, 2) : m(st, !1);
    });
  }
  n(Me);
  var Je = l(Me, 2), L = i(Je);
  {
    var D = (m) => {
      var y = jd(), le = ut(y), O = i(le);
      Tl(O, () => me(s(ve))), n(le);
      var re = l(le, 2);
      {
        var he = (we) => {
          var J = Fd(), V = l(i(J), 2), Y = i(V, !0);
          n(V), n(J), C(() => _(Y, s(U))), f(we, J);
        };
        P(re, (we) => {
          s(U) && we(he);
        });
      }
      f(m, y);
    }, oe = (m) => {
      var y = Vd(), le = i(y), O = i(le);
      O.textContent = "📄", Et(2), n(le), n(y), f(m, y);
    };
    P(L, (m) => {
      s(ve) ? m(D) : m(oe, !1);
    });
  }
  n(Je);
  var Se = l(Je, 2), G = i(Se);
  {
    var ee = (m) => {
      var y = qd(), le = i(y), O = l(i(le), 2);
      bt(O), n(le);
      var re = l(le, 2);
      {
        var he = (V) => {
          var Y = Bd(), ye = i(Y, !0);
          n(Y), C(() => _(ye, s(S))), f(V, Y);
        };
        P(re, (V) => {
          s(S) && V(he);
        });
      }
      var we = l(re, 2);
      we.__click = k;
      var J = i(we, !0);
      n(we), n(y), C(
        (V, Y) => {
          we.disabled = V, Re(we, 1, `w-full px-3 py-2 rounded-lg text-xs font-medium transition-colors
							${Y ?? ""}`), _(J, s(T) ? "Scaffolding..." : "Scaffold Venture");
        },
        [
          () => s(T) || o() || !s(W).trim(),
          () => s(T) || o() || !s(W).trim() ? "bg-surface-600 text-surface-400 cursor-not-allowed" : "bg-hecate-600 text-surface-50 hover:bg-hecate-500"
        ]
      ), xt(O, () => s(W), (V) => x(W, V)), f(m, y);
    }, Ae = (m) => {
      var y = Hd();
      y.textContent = "Vision is taking shape — keep exploring with the Oracle", f(m, y);
    }, Le = (m) => {
      var y = Gd();
      f(m, y);
    };
    P(G, (m) => {
      s(N) ? m(ee) : s(ve) ? m(Ae, 1) : m(Le, !1);
    });
  }
  n(Se), n(je), n(fe), ls(fe, (m) => x(ie, m), () => s(ie)), C(
    (m, y) => {
      hr(de, `width: ${s(K) ?? ""}%`), zt(j, "placeholder", s(w) ? "Oracle is thinking..." : "Describe your venture..."), j.disabled = s(w) || !a(), B.disabled = m, Re(B, 1, `px-3 rounded-lg text-[11px] transition-colors self-end
						${y ?? ""}`), Re(xe, 1, `w-1 cursor-col-resize shrink-0 transition-colors
			${s(ge) ? "bg-hecate-500" : "bg-surface-600 hover:bg-surface-500"}`);
    },
    [
      () => s(w) || !s(h).trim() || !a(),
      () => s(w) || !s(h).trim() || !a() ? "bg-surface-600 text-surface-400 cursor-not-allowed" : "bg-hecate-600 text-surface-50 hover:bg-hecate-500"
    ]
  ), Mt("mouseleave", fe, Ce), xt(j, () => s(h), (m) => x(h, m)), f(e, fe), St(), d();
}
Ot([
  "mousemove",
  "mouseup",
  "keydown",
  "input",
  "click",
  "mousedown"
]);
Dt(lo, {}, [], [], { mode: "open" });
var Ud = /* @__PURE__ */ p("<div></div>"), zd = /* @__PURE__ */ p('<!> <div><span class="svelte-gwxd3p"> </span> <span class="svelte-gwxd3p"> </span></div>', 1), Yd = /* @__PURE__ */ p("<span> </span>"), Kd = /* @__PURE__ */ p("<span> </span>"), Jd = /* @__PURE__ */ p(
  `<button title="Toggle event stream viewer">Stream</button> <button class="text-[9px] px-2 py-0.5 rounded ml-1
						text-surface-400 hover:text-health-warn hover:bg-surface-700 transition-colors svelte-gwxd3p" title="Shelve storm">Shelve</button>`,
  1
), Qd = /* @__PURE__ */ p(`<button class="flex items-center gap-1 text-[10px] px-2 py-1 rounded
										text-surface-400 hover:text-hecate-300
										hover:bg-hecate-600/10 transition-colors svelte-gwxd3p"><span class="svelte-gwxd3p"> </span> <span class="svelte-gwxd3p"> </span></button>`), Xd = /* @__PURE__ */ p(`<div class="flex items-center justify-center h-full svelte-gwxd3p"><div class="text-center max-w-lg mx-4 svelte-gwxd3p"><div class="text-4xl mb-4 text-es-event svelte-gwxd3p"></div> <h2 class="text-lg font-semibold text-surface-100 mb-3 svelte-gwxd3p">Big Picture Event Storming</h2> <p class="text-xs text-surface-400 leading-relaxed mb-6 svelte-gwxd3p">Discover the domain landscape by storming events onto the board.
						Start with a 10-minute high octane phase where everyone
						(including AI agents) throws domain events as fast as possible. <br class="svelte-gwxd3p"/><br class="svelte-gwxd3p"/> Volume over quality. The thick stacks reveal what matters.
						Natural clusters become your divisions (bounded contexts).</p> <div class="flex flex-col items-center gap-4 svelte-gwxd3p"><button class="px-6 py-3 rounded-lg text-sm font-medium
								bg-es-event text-surface-50 hover:bg-es-event/90
								transition-colors shadow-lg shadow-es-event/20 svelte-gwxd3p"></button> <div class="flex gap-2 svelte-gwxd3p"></div></div></div></div>`), Zd = /* @__PURE__ */ p(`<div class="group relative px-3 py-2 rounded text-xs
									bg-es-event/15 border border-es-event/30 text-surface-100
									hover:border-es-event/50 transition-all duration-200
									storm-sticky svelte-gwxd3p"><span class="svelte-gwxd3p"> </span> <span class="text-[8px] text-es-event/60 ml-1.5 svelte-gwxd3p"> </span> <button class="absolute -top-1 -right-1 w-4 h-4 rounded-full
										bg-surface-700 border border-surface-600
										text-surface-400 hover:text-health-err
										text-[8px] flex items-center justify-center
										opacity-0 group-hover:opacity-100 transition-opacity svelte-gwxd3p"></button></div>`), eu = /* @__PURE__ */ p(`<div class="group relative px-3 py-2 rounded text-xs
									border-2 border-dashed border-es-event/40 text-surface-300
									opacity-50 hover:opacity-80 transition-all duration-200
									storm-sticky ghost-sticky svelte-gwxd3p"><span class="italic svelte-gwxd3p"> </span> <span class="text-[8px] text-es-event/40 ml-1.5 svelte-gwxd3p">oracle</span> <div class="absolute -top-1 -right-1 flex gap-0.5
									opacity-0 group-hover:opacity-100 transition-opacity svelte-gwxd3p"><button class="w-4 h-4 rounded-full bg-health-ok/20 border border-health-ok/40
											text-health-ok text-[8px] flex items-center justify-center
											hover:bg-health-ok/30 svelte-gwxd3p" title="Accept"></button> <button class="w-4 h-4 rounded-full bg-surface-700 border border-surface-600
											text-surface-400 hover:text-health-err
											text-[8px] flex items-center justify-center svelte-gwxd3p" title="Dismiss"></button></div></div>`), tu = /* @__PURE__ */ p('<div class="text-surface-500 text-xs italic svelte-gwxd3p">Start throwing events! Type below or ask an AI agent...</div>'), ru = /* @__PURE__ */ p(`<button class="flex items-center gap-1 text-[9px] px-1.5 py-0.5 rounded
										text-surface-400 hover:text-hecate-300
										hover:bg-hecate-600/10 transition-colors svelte-gwxd3p"><span class="svelte-gwxd3p"> </span> <span class="svelte-gwxd3p"> </span></button>`), su = /* @__PURE__ */ p(`<div class="flex flex-col h-full svelte-gwxd3p"><div class="flex-1 overflow-y-auto p-4 svelte-gwxd3p"><div class="flex flex-wrap gap-2 content-start storm-board svelte-gwxd3p"><!> <!> <!></div></div> <div class="border-t border-surface-600 p-3 shrink-0 svelte-gwxd3p"><div class="flex gap-2 mb-2 svelte-gwxd3p"><input placeholder="Type a domain event (past tense)... e.g., order_placed" class="flex-1 bg-surface-700 border border-es-event/30 rounded px-3 py-2
								text-xs text-surface-100 placeholder-surface-400
								focus:outline-none focus:border-es-event svelte-gwxd3p"/> <button>Add</button></div> <div class="flex items-center justify-between svelte-gwxd3p"><div class="flex gap-1.5 svelte-gwxd3p"></div> <button class="text-[10px] px-3 py-1 rounded
								bg-surface-700 text-surface-300
								hover:text-surface-100 hover:bg-surface-600 transition-colors svelte-gwxd3p"></button></div></div></div>`), au = /* @__PURE__ */ p('<span class="text-[8px] px-1 rounded bg-es-event/20 text-es-event svelte-gwxd3p"> </span>'), nu = /* @__PURE__ */ p(`<div draggable="true" class="group flex items-center gap-1.5 px-2 py-1.5 rounded text-[11px]
											bg-es-event/15 border border-es-event/30 text-surface-100
											cursor-grab active:cursor-grabbing hover:border-es-event/50 svelte-gwxd3p"><span class="flex-1 truncate svelte-gwxd3p"> </span> <!></div>`), iu = /* @__PURE__ */ p(`<div class="group flex items-center gap-1 px-2 py-1 rounded text-[10px]
														bg-es-event/10 text-surface-200 svelte-gwxd3p"><span class="flex-1 truncate svelte-gwxd3p"> </span> <button class="text-[8px] text-surface-500 hover:text-surface-300
															opacity-0 group-hover:opacity-100 svelte-gwxd3p" title="Unstack"></button></div>`), ou = /* @__PURE__ */ p('<div><div class="flex items-center gap-2 mb-2 svelte-gwxd3p"><span class="text-[10px] font-bold text-es-event svelte-gwxd3p"> </span> <span class="text-[9px] text-surface-500 font-mono svelte-gwxd3p"> </span></div> <div class="space-y-1 svelte-gwxd3p"></div></div>'), lu = /* @__PURE__ */ p(`<div class="col-span-2 text-center py-8 text-surface-500 text-xs
											border border-dashed border-surface-600 rounded-lg svelte-gwxd3p">Drag stickies onto each other to create stacks.</div>`), cu = /* @__PURE__ */ p(`<button class="flex items-center gap-1 text-[9px] px-1.5 py-0.5 rounded
										text-surface-400 hover:text-hecate-300
										hover:bg-hecate-600/10 transition-colors svelte-gwxd3p"><span class="svelte-gwxd3p"> </span> <span class="svelte-gwxd3p"> </span></button>`), du = /* @__PURE__ */ p(`<div class="flex flex-col h-full svelte-gwxd3p"><div class="flex-1 overflow-y-auto p-4 svelte-gwxd3p"><p class="text-xs text-surface-400 mb-3 svelte-gwxd3p">Drag duplicate or related stickies onto each other to form stacks.
						Thick stacks reveal what matters most.</p> <div class="flex gap-4 svelte-gwxd3p"><div class="w-64 shrink-0 svelte-gwxd3p"><h3 class="text-[10px] font-semibold text-surface-300 mb-2 uppercase tracking-wider svelte-gwxd3p"> </h3> <div class="space-y-1 min-h-[200px] rounded-lg border border-dashed border-surface-600 p-2 svelte-gwxd3p"></div></div> <div class="flex-1 svelte-gwxd3p"><h3 class="text-[10px] font-semibold text-surface-300 mb-2 uppercase tracking-wider svelte-gwxd3p"> </h3> <div class="grid grid-cols-2 gap-3 svelte-gwxd3p"><!> <!></div></div></div></div> <div class="border-t border-surface-600 p-3 shrink-0 svelte-gwxd3p"><div class="flex items-center justify-between svelte-gwxd3p"><div class="flex gap-1.5 svelte-gwxd3p"></div> <button class="text-[10px] px-3 py-1 rounded transition-colors
								bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30 svelte-gwxd3p"></button></div></div></div>`), uu = /* @__PURE__ */ p('<button><span></span> <span class="flex-1 svelte-gwxd3p"> </span> <span class="text-[8px] text-surface-400 svelte-gwxd3p"> </span></button>'), vu = /* @__PURE__ */ p('<div class="rounded-lg border border-surface-600 bg-surface-800 p-4 svelte-gwxd3p"><div class="flex items-center gap-2 mb-3 svelte-gwxd3p"><span class="text-xs font-semibold text-surface-200 svelte-gwxd3p"> </span> <div class="flex-1 svelte-gwxd3p"></div> <button></button></div> <div class="space-y-1.5 svelte-gwxd3p"></div></div>'), fu = /* @__PURE__ */ p('<div class="space-y-4 mb-6 svelte-gwxd3p"></div>'), pu = /* @__PURE__ */ p(`<div class="text-center py-8 text-surface-500 text-xs
									border border-dashed border-surface-600 rounded-lg mb-6 svelte-gwxd3p">No stacks to groom. All stickies are unique.</div>`), xu = /* @__PURE__ */ p('<span class="text-[8px] text-es-event ml-1 svelte-gwxd3p"> </span>'), hu = /* @__PURE__ */ p(`<span class="text-[10px] px-2 py-1 rounded
												bg-es-event/10 text-surface-200 svelte-gwxd3p"> <!></span>`), _u = /* @__PURE__ */ p('<div class="svelte-gwxd3p"><h3 class="text-[10px] font-semibold text-surface-300 mb-2 uppercase tracking-wider svelte-gwxd3p"> </h3> <div class="flex flex-wrap gap-1.5 svelte-gwxd3p"></div></div>'), gu = /* @__PURE__ */ p(`<div class="flex flex-col h-full svelte-gwxd3p"><div class="flex-1 overflow-y-auto p-4 svelte-gwxd3p"><div class="max-w-2xl mx-auto svelte-gwxd3p"><p class="text-xs text-surface-400 mb-4 svelte-gwxd3p">For each stack, select the best representative sticky. The winner
							gets the stack's weight (vote count). Other stickies are absorbed.</p> <!> <!></div></div> <div class="border-t border-surface-600 p-3 shrink-0 svelte-gwxd3p"><div class="flex items-center justify-end svelte-gwxd3p"><button class="text-[10px] px-3 py-1 rounded transition-colors
								bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30 svelte-gwxd3p"></button></div></div></div>`), bu = /* @__PURE__ */ p('<span class="text-[8px] px-1 rounded bg-es-event/20 text-es-event svelte-gwxd3p"> </span>'), mu = /* @__PURE__ */ p(`<div draggable="true" class="group flex items-center gap-1.5 px-2 py-1.5 rounded text-[11px]
											bg-es-event/15 border border-es-event/30 text-surface-100
											cursor-grab active:cursor-grabbing hover:border-es-event/50 svelte-gwxd3p"><span class="flex-1 truncate svelte-gwxd3p"> </span> <!></div>`), yu = /* @__PURE__ */ p('<div class="text-[10px] text-surface-500 text-center py-4 italic svelte-gwxd3p">All events clustered</div>'), wu = /* @__PURE__ */ p('<span class="text-[8px] text-es-event/60 svelte-gwxd3p"> </span>'), ku = /* @__PURE__ */ p(`<div draggable="true" class="group flex items-center gap-1 px-2 py-1 rounded text-[10px]
														bg-es-event/10 text-surface-200
														cursor-grab active:cursor-grabbing svelte-gwxd3p"><span class="flex-1 truncate svelte-gwxd3p"> </span> <!> <button class="text-[8px] text-surface-500 hover:text-surface-300
															opacity-0 group-hover:opacity-100 svelte-gwxd3p" title="Remove from cluster"></button></div>`), $u = /* @__PURE__ */ p('<div><div class="flex items-center gap-2 mb-2 svelte-gwxd3p"><div class="w-3 h-3 rounded-sm shrink-0 svelte-gwxd3p"></div> <span class="flex-1 text-xs font-semibold text-surface-100 truncate svelte-gwxd3p"> </span> <span class="text-[9px] text-surface-400 svelte-gwxd3p"> </span> <button class="text-[9px] text-surface-500 hover:text-health-err transition-colors svelte-gwxd3p" title="Dissolve cluster"></button></div> <div class="space-y-1 svelte-gwxd3p"></div></div>'), Cu = /* @__PURE__ */ p(`<div class="col-span-2 text-center py-8 text-surface-500 text-xs
											border border-dashed border-surface-600 rounded-lg svelte-gwxd3p">Drag stickies onto each other to create clusters.</div>`), Su = /* @__PURE__ */ p(`<button class="flex items-center gap-1 text-[9px] px-1.5 py-0.5 rounded
										text-surface-400 hover:text-hecate-300
										hover:bg-hecate-600/10 transition-colors svelte-gwxd3p"><span class="svelte-gwxd3p"> </span> <span class="svelte-gwxd3p"> </span></button>`), Eu = /* @__PURE__ */ p(`<div class="flex flex-col h-full svelte-gwxd3p"><div class="flex-1 overflow-y-auto p-4 svelte-gwxd3p"><p class="text-xs text-surface-400 mb-3 svelte-gwxd3p">Drag related stickies onto each other to form clusters.
						Clusters become candidate divisions (bounded contexts).</p> <div class="flex gap-4 svelte-gwxd3p"><div class="w-64 shrink-0 svelte-gwxd3p"><h3 class="text-[10px] font-semibold text-surface-300 mb-2 uppercase tracking-wider svelte-gwxd3p"> </h3> <div class="space-y-1 min-h-[200px] rounded-lg border border-dashed border-surface-600 p-2 svelte-gwxd3p"><!> <!></div></div> <div class="flex-1 svelte-gwxd3p"><h3 class="text-[10px] font-semibold text-surface-300 mb-2 uppercase tracking-wider svelte-gwxd3p"> </h3> <div class="grid grid-cols-2 gap-3 svelte-gwxd3p"><!> <!></div></div></div></div> <div class="border-t border-surface-600 p-3 shrink-0 svelte-gwxd3p"><div class="flex items-center justify-between svelte-gwxd3p"><div class="flex gap-1.5 svelte-gwxd3p"></div> <button></button></div></div></div>`), Au = /* @__PURE__ */ p(`<input class="flex-1 bg-surface-700 border border-surface-500 rounded px-3 py-1.5
													text-sm text-surface-100 focus:outline-none focus:border-hecate-500 svelte-gwxd3p" placeholder="division_name (snake_case)"/>`), Du = /* @__PURE__ */ p('<button title="Click to name"> </button>'), Pu = /* @__PURE__ */ p('<span class="text-es-event/50 svelte-gwxd3p"> </span>'), Tu = /* @__PURE__ */ p(`<span class="text-[9px] px-1.5 py-0.5 rounded
													bg-es-event/10 text-es-event/80 svelte-gwxd3p"> <!></span>`), Ru = /* @__PURE__ */ p('<div class="rounded-lg border bg-surface-800 p-4 svelte-gwxd3p"><div class="flex items-center gap-3 mb-2 svelte-gwxd3p"><div class="w-4 h-4 rounded svelte-gwxd3p"></div> <!> <span class="text-[10px] text-surface-400 svelte-gwxd3p"> </span></div> <div class="flex flex-wrap gap-1.5 ml-7 svelte-gwxd3p"></div></div>'), Mu = /* @__PURE__ */ p(`<div class="flex flex-col h-full svelte-gwxd3p"><div class="flex-1 overflow-y-auto p-4 svelte-gwxd3p"><div class="max-w-2xl mx-auto svelte-gwxd3p"><p class="text-xs text-surface-400 mb-4 svelte-gwxd3p">Name each cluster as a bounded context (division). These become
							the divisions in your venture. Use snake_case naming.</p> <div class="space-y-3 svelte-gwxd3p"></div></div></div> <div class="border-t border-surface-600 p-3 shrink-0 svelte-gwxd3p"><div class="flex items-center justify-end svelte-gwxd3p"><button class="text-[10px] px-3 py-1 rounded
								bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30 transition-colors svelte-gwxd3p"></button></div></div></div>`), Iu = /* @__PURE__ */ p('<div class="px-4 py-2 rounded-lg border-2 text-xs font-semibold text-surface-100 svelte-gwxd3p"> <span class="text-[9px] text-surface-400 ml-1 svelte-gwxd3p"> </span></div>'), Lu = /* @__PURE__ */ p(`<div class="flex items-center gap-2 px-3 py-1.5 rounded
												bg-surface-800 border border-surface-600 text-xs svelte-gwxd3p"><span class="px-1.5 py-0.5 rounded text-[10px] font-medium svelte-gwxd3p"> </span> <span class="text-surface-400 svelte-gwxd3p"></span> <span class="text-es-event font-mono text-[10px] svelte-gwxd3p"> </span> <span class="text-surface-400 svelte-gwxd3p"></span> <span class="px-1.5 py-0.5 rounded text-[10px] font-medium svelte-gwxd3p"> </span> <div class="flex-1 svelte-gwxd3p"></div> <button class="text-surface-500 hover:text-health-err text-[9px] transition-colors svelte-gwxd3p"></button></div>`), Nu = /* @__PURE__ */ p('<div class="space-y-1.5 mb-4 svelte-gwxd3p"></div>'), Ou = /* @__PURE__ */ p('<option class="svelte-gwxd3p"> </option>'), Fu = /* @__PURE__ */ p('<option class="svelte-gwxd3p"> </option>'), ju = /* @__PURE__ */ p(`<div class="rounded-lg border border-surface-600 bg-surface-800 p-4 svelte-gwxd3p"><h4 class="text-[10px] font-semibold text-surface-300 uppercase tracking-wider mb-3 svelte-gwxd3p">Add Integration Fact</h4> <div class="flex items-end gap-2 svelte-gwxd3p"><div class="flex-1 svelte-gwxd3p"><label class="text-[9px] text-surface-400 block mb-1 svelte-gwxd3p">From (publishes)</label> <select class="w-full bg-surface-700 border border-surface-600 rounded px-2 py-1.5
												text-[10px] text-surface-100 focus:outline-none focus:border-hecate-500 svelte-gwxd3p"><option class="svelte-gwxd3p">Select...</option><!></select></div> <div class="flex-1 svelte-gwxd3p"><label class="text-[9px] text-surface-400 block mb-1 svelte-gwxd3p">Fact name</label> <input placeholder="e.g., order_confirmed" class="w-full bg-surface-700 border border-surface-600 rounded px-2 py-1.5
												text-[10px] text-surface-100 placeholder-surface-400
												focus:outline-none focus:border-hecate-500 svelte-gwxd3p"/></div> <div class="flex-1 svelte-gwxd3p"><label class="text-[9px] text-surface-400 block mb-1 svelte-gwxd3p">To (consumes)</label> <select class="w-full bg-surface-700 border border-surface-600 rounded px-2 py-1.5
												text-[10px] text-surface-100 focus:outline-none focus:border-hecate-500 svelte-gwxd3p"><option class="svelte-gwxd3p">Select...</option><!></select></div> <button>Add</button></div></div>`), Vu = /* @__PURE__ */ p(`<button class="flex items-center gap-1 text-[9px] px-1.5 py-0.5 rounded
										text-surface-400 hover:text-hecate-300
										hover:bg-hecate-600/10 transition-colors svelte-gwxd3p"><span class="svelte-gwxd3p"> </span> <span class="svelte-gwxd3p"> </span></button>`), Bu = /* @__PURE__ */ p(`<div class="flex flex-col h-full svelte-gwxd3p"><div class="flex-1 overflow-y-auto p-4 svelte-gwxd3p"><div class="max-w-3xl mx-auto svelte-gwxd3p"><p class="text-xs text-surface-400 mb-4 svelte-gwxd3p">Map how divisions communicate. Each arrow represents an
							integration fact that flows from one context to another.
							This is your Context Map.</p> <div class="mb-6 svelte-gwxd3p"><div class="flex flex-wrap gap-3 justify-center mb-4 svelte-gwxd3p"></div> <!></div> <!></div></div> <div class="border-t border-surface-600 p-3 shrink-0 svelte-gwxd3p"><div class="flex items-center justify-between svelte-gwxd3p"><div class="flex gap-2 svelte-gwxd3p"></div> <button> </button></div></div></div>`), qu = /* @__PURE__ */ p(`<div class="flex items-center justify-center h-full svelte-gwxd3p"><div class="text-center max-w-md mx-4 svelte-gwxd3p"><div class="text-4xl mb-4 text-health-ok svelte-gwxd3p"></div> <h2 class="text-lg font-semibold text-surface-100 mb-2 svelte-gwxd3p">Context Map Complete</h2> <p class="text-xs text-surface-400 mb-4 svelte-gwxd3p"> </p> <p class="text-xs text-surface-400 mb-6 svelte-gwxd3p">Select a division from the sidebar to begin Design-Level
						Event Storming in its DnA phase.</p> <button class="text-[10px] px-3 py-1 rounded
							text-surface-400 hover:text-surface-200 hover:bg-surface-700 transition-colors svelte-gwxd3p">Reset Board</button></div></div>`), Hu = /* @__PURE__ */ p(`<div class="flex items-center justify-center h-full svelte-gwxd3p"><div class="text-center max-w-md mx-4 svelte-gwxd3p"><div class="text-4xl mb-4 text-health-warn svelte-gwxd3p"></div> <h2 class="text-lg font-semibold text-surface-100 mb-2 svelte-gwxd3p">Storm Shelved</h2> <p class="text-xs text-surface-400 mb-6 svelte-gwxd3p">This storm session has been shelved. You can resume it at any time
						to continue where you left off.</p> <button class="px-6 py-3 rounded-lg text-sm font-medium
							bg-hecate-600 text-surface-50 hover:bg-hecate-500
							transition-colors svelte-gwxd3p">Resume Storm</button></div></div>`), Gu = /* @__PURE__ */ p('<div class="flex flex-col h-full svelte-gwxd3p"><div class="border-b border-surface-600 bg-surface-800/50 px-4 py-2 shrink-0 svelte-gwxd3p"><div class="flex items-center gap-1 svelte-gwxd3p"><span class="text-xs text-surface-400 mr-2 svelte-gwxd3p">Big Picture</span> <!> <div class="flex-1 svelte-gwxd3p"></div> <!> <!> <!></div></div> <div><!></div></div>');
const Wu = {
  hash: "svelte-gwxd3p",
  code: `
	/* Storm phase: stickies animate to aligned grid on phase change */.storm-board .storm-sticky {transition:transform 0.5s ease, opacity 0.3s ease;}

	/* Ghost stickies: translucent dashed border, subtle float */.ghost-sticky {
		animation: svelte-gwxd3p-ghost-float 3s ease-in-out infinite;}

	@keyframes svelte-gwxd3p-ghost-float {
		0%, 100% { transform: translateY(0px); }
		50% { transform: translateY(-2px); }
	}

	/* Scale utility for event count pulse */.scale-110 {transform:scale(1.15);}`
};
function Va(e, t) {
  Ct(t, !0), Vi(e, Wu);
  const r = () => $e(sd, "$bigPictureAgents", S), a = () => $e(oc, "$bigPictureEventCount", S), o = () => $e(es, "$bigPicturePhase", S), c = () => $e($t, "$activeVenture", S), d = () => $e(ms, "$bigPictureEvents", S), u = () => $e(pa, "$eventClusters", S), v = () => $e(hn, "$factArrows", S), h = () => $e(ta, "$highOctaneRemaining", S), w = () => $e(Fa, "$showEventStream", S), g = () => $e(ic, "$stickyStacks", S), b = () => $e(nc, "$unclusteredEvents", S), T = () => $e(ja, "$isLoading", S), [S, W] = qt();
  let $ = /* @__PURE__ */ se(""), R = /* @__PURE__ */ se(null), K = /* @__PURE__ */ se(""), ge = /* @__PURE__ */ se(null), ie = /* @__PURE__ */ se(null), ae = /* @__PURE__ */ se(""), Z = /* @__PURE__ */ se(null), De = /* @__PURE__ */ se(Ut({})), Ve = /* @__PURE__ */ se(Ut(/* @__PURE__ */ new Map()));
  function Ge(L) {
    return s(Ve).has(L) || s(Ve).set(L, {
      rotate: (Math.random() - 0.5) * 6,
      // -3 to +3 degrees
      dx: (Math.random() - 0.5) * 4,
      // -2 to +2 px
      dy: (Math.random() - 0.5) * 4
    }), s(Ve).get(L);
  }
  let ve = /* @__PURE__ */ se(Ut([]));
  async function N(L) {
    await Ws(F(), L.text, "oracle"), x(ve, s(ve).filter((D) => D.id !== L.id), !0);
  }
  function U(L) {
    x(ve, s(ve).filter((D) => D.id !== L.id), !0);
  }
  let pe = /* @__PURE__ */ se(!1), Ne = /* @__PURE__ */ se(0);
  Pt(() => {
    const L = a();
    L > s(Ne) && s(Ne) > 0 && (x(pe, !0), setTimeout(() => x(pe, !1), 300)), x(Ne, L, !0);
  });
  let ne = /* @__PURE__ */ se(!1), k = /* @__PURE__ */ se("");
  Pt(() => {
    const L = o();
    L !== s(k) && s(k) !== "" && (x(ne, !0), setTimeout(() => x(ne, !1), 300)), x(k, L, !0);
  });
  function F() {
    return c()?.venture_id ?? "";
  }
  function Fe(L) {
    const D = Math.floor(L / 60), oe = L % 60;
    return `${D}:${oe.toString().padStart(2, "0")}`;
  }
  async function ze(L) {
    L.key === "Enter" && !L.shiftKey && s($).trim() && (L.preventDefault(), await Ws(F(), s($)), x($, ""));
  }
  async function Xe(L, D) {
    L.key === "Enter" && s(K).trim() ? (await pc(F(), D, s(K).trim()), x(R, null), x(K, "")) : L.key === "Escape" && x(R, null);
  }
  function Ee(L) {
    x(R, L.cluster_id, !0), x(K, L.name ?? "", !0);
  }
  async function Ce() {
    s(ge) && s(ie) && s(ge) !== s(ie) && s(ae).trim() && (await xc(F(), s(ge), s(ie), s(ae).trim()), x(ae, ""));
  }
  async function me() {
    await mc(F());
  }
  function fe(L) {
    return d().filter((D) => D.cluster_id === L);
  }
  let de = /* @__PURE__ */ Te(() => d().filter((L) => !L.stack_id));
  function H(L) {
    const D = c(), oe = d(), Se = u(), G = v();
    let ee = L + `

---

`;
    if (D && (ee += `Venture: "${D.name}"`, D.brief && (ee += ` — ${D.brief}`), ee += `

`), oe.length > 0 && (ee += `Events on the board:
`, ee += oe.map((Ae) => `- ${Ae.text}${Ae.weight > 1 ? ` (x${Ae.weight})` : ""}`).join(`
`), ee += `

`), Se.length > 0) {
      ee += `Current clusters (candidate divisions):
`;
      for (const Ae of Se) {
        const Le = oe.filter((m) => m.cluster_id === Ae.cluster_id);
        ee += `- ${Ae.name ?? "(unnamed)"}: ${Le.map((m) => m.text).join(", ") || "(empty)"}
`;
      }
      ee += `
`;
    }
    if (G.length > 0) {
      ee += `Integration fact arrows:
`;
      for (const Ae of G) {
        const Le = Se.find((y) => y.cluster_id === Ae.from_cluster)?.name ?? "?", m = Se.find((y) => y.cluster_id === Ae.to_cluster)?.name ?? "?";
        ee += `- ${Le} → ${Ae.fact_name} → ${m}
`;
      }
    }
    return ee;
  }
  const M = [
    { phase: "storm", label: "Storm", icon: "⚡" },
    { phase: "stack", label: "Stack", icon: "≡" },
    { phase: "groom", label: "Groom", icon: "✂" },
    { phase: "cluster", label: "Cluster", icon: "⭐" },
    { phase: "name", label: "Name", icon: "⬡" },
    { phase: "map", label: "Map", icon: "→" },
    { phase: "promoted", label: "Done", icon: "✓" }
  ];
  Pt(() => {
    const L = c();
    L && Ht(L.venture_id);
  });
  var I = Gu(), z = i(I), Oe = i(z), Ye = l(i(Oe), 2);
  He(Ye, 17, () => M, pt, (L, D, oe) => {
    const Se = /* @__PURE__ */ Te(() => o() === s(D).phase), G = /* @__PURE__ */ Te(() => M.findIndex((he) => he.phase === o()) > oe);
    var ee = zd(), Ae = ut(ee);
    {
      var Le = (he) => {
        var we = Ud();
        C(() => Re(we, 1, `w-6 h-px ${s(G) ? "bg-hecate-400/60" : "bg-surface-600"}`, "svelte-gwxd3p")), f(he, we);
      };
      P(Ae, (he) => {
        oe > 0 && he(Le);
      });
    }
    var m = l(Ae, 2), y = i(m), le = i(y, !0);
    n(y);
    var O = l(y, 2), re = i(O, !0);
    n(O), n(m), C(() => {
      Re(
        m,
        1,
        `flex items-center gap-1 px-2 py-1 rounded text-[10px]
						${s(Se) ? "bg-surface-700 border border-hecate-500/40 text-hecate-300" : s(G) ? "text-hecate-400/60" : "text-surface-500"}`,
        "svelte-gwxd3p"
      ), _(le, s(D).icon), _(re, s(D).label);
    }), f(L, ee);
  });
  var A = l(Ye, 4);
  {
    var E = (L) => {
      var D = Yd(), oe = i(D);
      n(D), C(() => {
        Re(
          D,
          1,
          `text-[10px] transition-all duration-300
						${s(pe) ? "scale-110 text-es-event font-bold" : "text-surface-400"}`,
          "svelte-gwxd3p"
        ), _(oe, `${a() ?? ""} events`);
      }), f(L, D);
    };
    P(A, (L) => {
      o() !== "ready" && o() !== "promoted" && o() !== "shelved" && L(E);
    });
  }
  var te = l(A, 2);
  {
    var ke = (L) => {
      var D = Kd(), oe = i(D, !0);
      n(D), C(
        (Se) => {
          Re(
            D,
            1,
            `text-sm font-bold tabular-nums ml-2
						${h() <= 60 ? "text-health-err animate-pulse" : h() <= 180 ? "text-health-warn" : "text-es-event"}`,
            "svelte-gwxd3p"
          ), _(oe, Se);
        },
        [() => Fe(h())]
      ), f(L, D);
    };
    P(te, (L) => {
      o() === "storm" && L(ke);
    });
  }
  var Ie = l(te, 2);
  {
    var j = (L) => {
      var D = Jd(), oe = ut(D);
      oe.__click = () => Fa.update((G) => !G);
      var Se = l(oe, 2);
      Se.__click = () => gc(F()), C(() => Re(
        oe,
        1,
        `text-[9px] px-2 py-0.5 rounded ml-1
						${w() ? "text-hecate-300 bg-hecate-600/20" : "text-surface-400 hover:text-surface-200 hover:bg-surface-700"} transition-colors`,
        "svelte-gwxd3p"
      )), f(L, D);
    };
    P(Ie, (L) => {
      o() !== "ready" && o() !== "promoted" && o() !== "shelved" && L(j);
    });
  }
  n(Oe), n(z);
  var B = l(z, 2), xe = i(B);
  {
    var je = (L) => {
      var D = Xd(), oe = i(D), Se = i(oe);
      Se.textContent = "⚡";
      var G = l(Se, 6), ee = i(G);
      ee.__click = () => lc(F()), ee.textContent = "⚡ Start High Octane (10 min)";
      var Ae = l(ee, 2);
      He(Ae, 5, r, pt, (Le, m) => {
        var y = Qd();
        y.__click = () => $r(H(s(m).prompt), s(m).id);
        var le = i(y), O = i(le, !0);
        n(le);
        var re = l(le, 2), he = i(re, !0);
        n(re), n(y), C(() => {
          zt(y, "title", s(m).description), _(O, s(m).icon), _(he, s(m).name);
        }), f(Le, y);
      }), n(Ae), n(G), n(oe), n(D), f(L, D);
    }, Me = (L) => {
      var D = su(), oe = i(D), Se = i(oe), G = i(Se);
      He(G, 1, d, (J) => J.sticky_id, (J, V) => {
        const Y = /* @__PURE__ */ Te(() => Ge(s(V).sticky_id));
        var ye = Zd(), _e = i(ye), ce = i(_e, !0);
        n(_e);
        var Q = l(_e, 2), X = i(Q, !0);
        n(Q);
        var q = l(Q, 2);
        q.__click = () => cc(F(), s(V).sticky_id), q.textContent = "✕", n(ye), C(() => {
          hr(ye, `transform: rotate(${s(Y).rotate ?? ""}deg) translate(${s(Y).dx ?? ""}px, ${s(Y).dy ?? ""}px)`), _(ce, s(V).text), _(X, s(V).author === "user" ? "" : s(V).author);
        }), f(J, ye);
      });
      var ee = l(G, 2);
      He(ee, 17, () => s(ve), (J) => J.id, (J, V) => {
        var Y = eu();
        hr(Y, `transform: rotate(${(Math.random() - 0.5) * 4}deg)`);
        var ye = i(Y), _e = i(ye, !0);
        n(ye);
        var ce = l(ye, 4), Q = i(ce);
        Q.__click = () => N(s(V)), Q.textContent = "✓";
        var X = l(Q, 2);
        X.__click = () => U(s(V)), X.textContent = "✕", n(ce), n(Y), C(() => _(_e, s(V).text)), f(J, Y);
      });
      var Ae = l(ee, 2);
      {
        var Le = (J) => {
          var V = tu();
          f(J, V);
        };
        P(Ae, (J) => {
          d().length === 0 && s(ve).length === 0 && J(Le);
        });
      }
      n(Se), n(oe);
      var m = l(oe, 2), y = i(m), le = i(y);
      bt(le), le.__keydown = ze;
      var O = l(le, 2);
      O.__click = async () => {
        s($).trim() && (await Ws(F(), s($)), x($, ""));
      }, n(y);
      var re = l(y, 2), he = i(re);
      He(he, 5, r, pt, (J, V) => {
        var Y = ru();
        Y.__click = () => $r(H(s(V).prompt), s(V).id);
        var ye = i(Y), _e = i(ye, !0);
        n(ye);
        var ce = l(ye, 2), Q = i(ce, !0);
        n(ce), n(Y), C(() => {
          zt(Y, "title", s(V).description), _(_e, s(V).icon), _(Q, s(V).role);
        }), f(J, Y);
      }), n(he);
      var we = l(he, 2);
      we.__click = () => ks(F(), "stack"), we.textContent = "End Storm → Stack", n(re), n(m), n(D), C(
        (J, V) => {
          O.disabled = J, Re(
            O,
            1,
            `px-3 py-2 rounded text-xs transition-colors
								${V ?? ""}`,
            "svelte-gwxd3p"
          );
        },
        [
          () => !s($).trim(),
          () => s($).trim() ? "bg-es-event text-surface-50 hover:bg-es-event/80" : "bg-surface-600 text-surface-400 cursor-not-allowed"
        ]
      ), xt(le, () => s($), (J) => x($, J)), f(L, D);
    }, et = (L) => {
      var D = du(), oe = i(D), Se = l(i(oe), 2), G = i(Se), ee = i(G), Ae = i(ee);
      n(ee);
      var Le = l(ee, 2);
      He(Le, 21, () => s(de), (_e) => _e.sticky_id, (_e, ce) => {
        var Q = nu(), X = i(Q), q = i(X, !0);
        n(X);
        var ue = l(X, 2);
        {
          var be = (Pe) => {
            var Be = au(), Qe = i(Be);
            n(Be), C(() => _(Qe, `x${s(ce).weight ?? ""}`)), f(Pe, Be);
          };
          P(ue, (Pe) => {
            s(ce).weight > 1 && Pe(be);
          });
        }
        n(Q), C(() => _(q, s(ce).text)), Mt("dragstart", Q, () => x(Z, s(ce).sticky_id, !0)), Mt("dragend", Q, () => x(Z, null)), Mt("dragover", Q, (Pe) => Pe.preventDefault()), Mt("drop", Q, () => {
          s(Z) && s(Z) !== s(ce).sticky_id && (jn(F(), s(Z), s(ce).sticky_id), x(Z, null));
        }), f(_e, Q);
      }), n(Le), n(G);
      var m = l(G, 2), y = i(m), le = i(y);
      n(y);
      var O = l(y, 2), re = i(O);
      He(re, 1, () => [...g().entries()], ([_e, ce]) => _e, (_e, ce) => {
        var Q = /* @__PURE__ */ Te(() => ka(s(ce), 2));
        let X = () => s(Q)[0], q = () => s(Q)[1];
        var ue = ou(), be = i(ue), Pe = i(be), Be = i(Pe);
        n(Pe);
        var Qe = l(Pe, 2), rt = i(Qe, !0);
        n(Qe), n(be);
        var at = l(be, 2);
        He(at, 21, q, (nt) => nt.sticky_id, (nt, gt) => {
          var ht = iu(), Yt = i(ht), ir = i(Yt, !0);
          n(Yt);
          var kt = l(Yt, 2);
          kt.__click = () => dc(F(), s(gt).sticky_id), kt.textContent = "↩", n(ht), C(() => _(ir, s(gt).text)), f(nt, ht);
        }), n(at), n(ue), C(
          (nt) => {
            Re(
              ue,
              1,
              `rounded-lg border-2 p-3 min-h-[80px] transition-colors
											${s(Z) ? "border-dashed border-hecate-500/50 bg-hecate-600/5" : "border-surface-600 bg-surface-800"}`,
              "svelte-gwxd3p"
            ), _(Be, `${q().length ?? ""}x`), _(rt, nt);
          },
          [() => X().slice(0, 8)]
        ), Mt("dragover", ue, (nt) => nt.preventDefault()), Mt("drop", ue, () => {
          s(Z) && q().length > 0 && (jn(F(), s(Z), q()[0].sticky_id), x(Z, null));
        }), f(_e, ue);
      });
      var he = l(re, 2);
      {
        var we = (_e) => {
          var ce = lu();
          f(_e, ce);
        };
        P(he, (_e) => {
          g().size === 0 && _e(we);
        });
      }
      n(O), n(m), n(Se), n(oe);
      var J = l(oe, 2), V = i(J), Y = i(V);
      He(Y, 5, () => r().slice(0, 2), pt, (_e, ce) => {
        var Q = cu();
        Q.__click = () => $r(H(s(ce).prompt), s(ce).id);
        var X = i(Q), q = i(X, !0);
        n(X);
        var ue = l(X, 2), be = i(ue);
        n(ue), n(Q), C(() => {
          _(q, s(ce).icon), _(be, `Ask ${s(ce).name ?? ""}`);
        }), f(_e, Q);
      }), n(Y);
      var ye = l(Y, 2);
      ye.__click = () => ks(F(), "groom"), ye.textContent = "Groom Stacks →", n(V), n(J), n(D), C(() => {
        _(Ae, `Stickies (${s(de).length ?? ""})`), _(le, `Stacks (${g().size ?? ""})`);
      }), f(L, D);
    }, qe = (L) => {
      var D = gu(), oe = i(D), Se = i(oe), G = l(i(Se), 2);
      {
        var ee = (re) => {
          var he = fu();
          He(he, 5, () => [...g().entries()], ([we, J]) => we, (we, J) => {
            var V = /* @__PURE__ */ Te(() => ka(s(J), 2));
            let Y = () => s(V)[0], ye = () => s(V)[1];
            const _e = /* @__PURE__ */ Te(() => s(De)[Y()]);
            var ce = vu(), Q = i(ce), X = i(Q), q = i(X);
            n(X);
            var ue = l(X, 4);
            ue.__click = () => {
              s(_e) && uc(F(), Y(), s(_e));
            }, ue.textContent = "Groom ✂", n(Q);
            var be = l(Q, 2);
            He(be, 21, ye, (Pe) => Pe.sticky_id, (Pe, Be) => {
              var Qe = uu();
              Qe.__click = () => x(De, { ...s(De), [Y()]: s(Be).sticky_id }, !0);
              var rt = i(Qe), at = l(rt, 2), nt = i(at, !0);
              n(at);
              var gt = l(at, 2), ht = i(gt, !0);
              n(gt), n(Qe), C(() => {
                Re(
                  Qe,
                  1,
                  `w-full text-left flex items-center gap-2 px-3 py-2 rounded text-[11px]
														transition-colors
														${s(_e) === s(Be).sticky_id ? "bg-hecate-600/20 border border-hecate-500/40 text-hecate-200" : "bg-surface-700/50 border border-transparent text-surface-200 hover:border-surface-500"}`,
                  "svelte-gwxd3p"
                ), Re(
                  rt,
                  1,
                  `w-3 h-3 rounded-full border-2 shrink-0
															${s(_e) === s(Be).sticky_id ? "border-hecate-400 bg-hecate-400" : "border-surface-500"}`,
                  "svelte-gwxd3p"
                ), _(nt, s(Be).text), _(ht, s(Be).author === "user" ? "" : s(Be).author);
              }), f(Pe, Qe);
            }), n(be), n(ce), C(() => {
              _(q, `Stack (${ye().length ?? ""} stickies)`), ue.disabled = !s(_e), Re(
                ue,
                1,
                `text-[10px] px-2 py-1 rounded transition-colors
													${s(_e) ? "bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30" : "bg-surface-700 text-surface-500 cursor-not-allowed"}`,
                "svelte-gwxd3p"
              );
            }), f(we, ce);
          }), n(he), f(re, he);
        }, Ae = (re) => {
          var he = pu();
          f(re, he);
        };
        P(G, (re) => {
          g().size > 0 ? re(ee) : re(Ae, !1);
        });
      }
      var Le = l(G, 2);
      {
        var m = (re) => {
          var he = _u(), we = i(he), J = i(we);
          n(we);
          var V = l(we, 2);
          He(V, 21, () => s(de), (Y) => Y.sticky_id, (Y, ye) => {
            var _e = hu(), ce = i(_e), Q = l(ce);
            {
              var X = (q) => {
                var ue = xu(), be = i(ue);
                n(ue), C(() => _(be, `x${s(ye).weight ?? ""}`)), f(q, ue);
              };
              P(Q, (q) => {
                s(ye).weight > 1 && q(X);
              });
            }
            n(_e), C(() => _(ce, `${s(ye).text ?? ""} `)), f(Y, _e);
          }), n(V), n(he), C(() => _(J, `Standalone Stickies (${s(de).length ?? ""})`)), f(re, he);
        };
        P(Le, (re) => {
          s(de).length > 0 && re(m);
        });
      }
      n(Se), n(oe);
      var y = l(oe, 2), le = i(y), O = i(le);
      O.__click = () => ks(F(), "cluster"), O.textContent = "Cluster Events →", n(le), n(y), n(D), f(L, D);
    }, Ke = (L) => {
      var D = Eu(), oe = i(D), Se = l(i(oe), 2), G = i(Se), ee = i(G), Ae = i(ee);
      n(ee);
      var Le = l(ee, 2), m = i(Le);
      He(m, 1, b, (X) => X.sticky_id, (X, q) => {
        var ue = mu(), be = i(ue), Pe = i(be, !0);
        n(be);
        var Be = l(be, 2);
        {
          var Qe = (rt) => {
            var at = bu(), nt = i(at);
            n(at), C(() => _(nt, `x${s(q).weight ?? ""}`)), f(rt, at);
          };
          P(Be, (rt) => {
            s(q).weight > 1 && rt(Qe);
          });
        }
        n(ue), C(() => _(Pe, s(q).text)), Mt("dragstart", ue, () => x(Z, s(q).sticky_id, !0)), Mt("dragend", ue, () => x(Z, null)), Mt("dragover", ue, (rt) => rt.preventDefault()), Mt("drop", ue, () => {
          s(Z) && s(Z) !== s(q).sticky_id && (Vn(F(), s(Z), s(q).sticky_id), x(Z, null));
        }), f(X, ue);
      });
      var y = l(m, 2);
      {
        var le = (X) => {
          var q = yu();
          f(X, q);
        };
        P(y, (X) => {
          b().length === 0 && X(le);
        });
      }
      n(Le), n(G);
      var O = l(G, 2), re = i(O), he = i(re);
      n(re);
      var we = l(re, 2), J = i(we);
      He(J, 1, u, (X) => X.cluster_id, (X, q) => {
        const ue = /* @__PURE__ */ Te(() => fe(s(q).cluster_id));
        var be = $u(), Pe = i(be), Be = i(Pe), Qe = l(Be, 2), rt = i(Qe, !0);
        n(Qe);
        var at = l(Qe, 2), nt = i(at, !0);
        n(at);
        var gt = l(at, 2);
        gt.__click = () => fc(F(), s(q).cluster_id), gt.textContent = "✕", n(Pe);
        var ht = l(Pe, 2);
        He(ht, 21, () => s(ue), (Yt) => Yt.sticky_id, (Yt, ir) => {
          var kt = ku(), jt = i(kt), wr = i(jt, !0);
          n(jt);
          var yn = l(jt, 2);
          {
            var bo = (ga) => {
              var ba = wu(), mo = i(ba);
              n(ba), C(() => _(mo, `x${s(ir).weight ?? ""}`)), f(ga, ba);
            };
            P(yn, (ga) => {
              s(ir).weight > 1 && ga(bo);
            });
          }
          var wn = l(yn, 2);
          wn.__click = () => vc(F(), s(ir).sticky_id), wn.textContent = "↩", n(kt), C(() => _(wr, s(ir).text)), Mt("dragstart", kt, () => x(Z, s(ir).sticky_id, !0)), Mt("dragend", kt, () => x(Z, null)), f(Yt, kt);
        }), n(ht), n(be), C(() => {
          Re(
            be,
            1,
            `rounded-lg border-2 p-3 min-h-[120px] transition-colors
											${s(Z) ? "border-dashed border-hecate-500/50 bg-hecate-600/5" : "border-surface-600 bg-surface-800"}`,
            "svelte-gwxd3p"
          ), hr(be, `border-color: ${s(Z) ? "" : s(q).color + "40"}`), hr(Be, `background-color: ${s(q).color ?? ""}`), _(rt, s(q).name ?? "Unnamed"), _(nt, s(ue).length);
        }), Mt("dragover", be, (Yt) => Yt.preventDefault()), Mt("drop", be, () => {
          s(Z) && s(ue).length > 0 && (Vn(F(), s(Z), s(ue)[0].sticky_id), x(Z, null));
        }), f(X, be);
      });
      var V = l(J, 2);
      {
        var Y = (X) => {
          var q = Cu();
          f(X, q);
        };
        P(V, (X) => {
          u().length === 0 && X(Y);
        });
      }
      n(we), n(O), n(Se), n(oe);
      var ye = l(oe, 2), _e = i(ye), ce = i(_e);
      He(ce, 5, () => r().slice(0, 2), pt, (X, q) => {
        var ue = Su();
        ue.__click = () => $r(H(s(q).prompt), s(q).id);
        var be = i(ue), Pe = i(be, !0);
        n(be);
        var Be = l(be, 2), Qe = i(Be);
        n(Be), n(ue), C(() => {
          _(Pe, s(q).icon), _(Qe, `Ask ${s(q).name ?? ""}`);
        }), f(X, ue);
      }), n(ce);
      var Q = l(ce, 2);
      Q.__click = () => ks(F(), "name"), Q.textContent = "Name Divisions →", n(_e), n(ye), n(D), C(() => {
        _(Ae, `Unclustered (${b().length ?? ""})`), _(he, `Clusters (${u().length ?? ""})`), Q.disabled = u().length === 0, Re(
          Q,
          1,
          `text-[10px] px-3 py-1 rounded transition-colors
								${u().length === 0 ? "bg-surface-700 text-surface-500 cursor-not-allowed" : "bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30"}`,
          "svelte-gwxd3p"
        );
      }), f(L, D);
    }, We = (L) => {
      var D = Mu(), oe = i(D), Se = i(oe), G = l(i(Se), 2);
      He(G, 5, u, (m) => m.cluster_id, (m, y) => {
        const le = /* @__PURE__ */ Te(() => fe(s(y).cluster_id));
        var O = Ru(), re = i(O), he = i(re), we = l(he, 2);
        {
          var J = (ce) => {
            var Q = Au();
            bt(Q), Q.__keydown = (X) => Xe(X, s(y).cluster_id), Mt("blur", Q, () => x(R, null)), xt(Q, () => s(K), (X) => x(K, X)), f(ce, Q);
          }, V = (ce) => {
            var Q = Du();
            Q.__click = () => Ee(s(y));
            var X = i(Q, !0);
            n(Q), C(() => {
              Re(
                Q,
                1,
                `flex-1 text-left text-sm font-semibold transition-colors
													${s(y).name ? "text-surface-100 hover:text-hecate-300" : "text-surface-400 italic hover:text-hecate-300"}`,
                "svelte-gwxd3p"
              ), _(X, s(y).name ?? "Click to name...");
            }), f(ce, Q);
          };
          P(we, (ce) => {
            s(R) === s(y).cluster_id ? ce(J) : ce(V, !1);
          });
        }
        var Y = l(we, 2), ye = i(Y);
        n(Y), n(re);
        var _e = l(re, 2);
        He(_e, 21, () => s(le), (ce) => ce.sticky_id, (ce, Q) => {
          var X = Tu(), q = i(X), ue = l(q);
          {
            var be = (Pe) => {
              var Be = Pu(), Qe = i(Be);
              n(Be), C(() => _(Qe, `x${s(Q).weight ?? ""}`)), f(Pe, Be);
            };
            P(ue, (Pe) => {
              s(Q).weight > 1 && Pe(be);
            });
          }
          n(X), C(() => _(q, `${s(Q).text ?? ""} `)), f(ce, X);
        }), n(_e), n(O), C(() => {
          hr(O, `border-color: ${s(y).color ?? ""}40`), hr(he, `background-color: ${s(y).color ?? ""}`), _(ye, `${s(le).length ?? ""} events`);
        }), f(m, O);
      }), n(G), n(Se), n(oe);
      var ee = l(oe, 2), Ae = i(ee), Le = i(Ae);
      Le.__click = () => ks(F(), "map"), Le.textContent = "Map Integration Facts →", n(Ae), n(ee), n(D), f(L, D);
    }, tt = (L) => {
      var D = Bu(), oe = i(D), Se = i(oe), G = l(i(Se), 2), ee = i(G);
      He(ee, 5, u, (J) => J.cluster_id, (J, V) => {
        var Y = Iu(), ye = i(Y), _e = l(ye), ce = i(_e);
        n(_e), n(Y), C(
          (Q) => {
            hr(Y, `border-color: ${s(V).color ?? ""}; background-color: ${s(V).color ?? ""}15`), _(ye, `${s(V).name ?? "Unnamed" ?? ""} `), _(ce, `(${Q ?? ""})`);
          },
          [() => fe(s(V).cluster_id).length]
        ), f(J, Y);
      }), n(ee);
      var Ae = l(ee, 2);
      {
        var Le = (J) => {
          var V = Nu();
          He(V, 5, v, (Y) => Y.arrow_id, (Y, ye) => {
            const _e = /* @__PURE__ */ Te(() => u().find((nt) => nt.cluster_id === s(ye).from_cluster)), ce = /* @__PURE__ */ Te(() => u().find((nt) => nt.cluster_id === s(ye).to_cluster));
            var Q = Lu(), X = i(Q), q = i(X, !0);
            n(X);
            var ue = l(X, 2);
            ue.textContent = "→";
            var be = l(ue, 2), Pe = i(be, !0);
            n(be);
            var Be = l(be, 2);
            Be.textContent = "→";
            var Qe = l(Be, 2), rt = i(Qe, !0);
            n(Qe);
            var at = l(Qe, 4);
            at.__click = () => hc(F(), s(ye).arrow_id), at.textContent = "✕", n(Q), C(() => {
              hr(X, `color: ${s(_e)?.color ?? "#888" ?? ""}; background-color: ${s(_e)?.color ?? "#888" ?? ""}15`), _(q, s(_e)?.name ?? "?"), _(Pe, s(ye).fact_name), hr(Qe, `color: ${s(ce)?.color ?? "#888" ?? ""}; background-color: ${s(ce)?.color ?? "#888" ?? ""}15`), _(rt, s(ce)?.name ?? "?");
            }), f(Y, Q);
          }), n(V), f(J, V);
        };
        P(Ae, (J) => {
          v().length > 0 && J(Le);
        });
      }
      n(G);
      var m = l(G, 2);
      {
        var y = (J) => {
          var V = ju(), Y = l(i(V), 2), ye = i(Y), _e = l(i(ye), 2), ce = i(_e);
          ce.value = (ce.__value = null) ?? "";
          var Q = l(ce);
          He(Q, 1, u, pt, (rt, at) => {
            var nt = Ou(), gt = i(nt, !0);
            n(nt);
            var ht = {};
            C(() => {
              _(gt, s(at).name ?? "Unnamed"), ht !== (ht = s(at).cluster_id) && (nt.value = (nt.__value = s(at).cluster_id) ?? "");
            }), f(rt, nt);
          }), n(_e), n(ye);
          var X = l(ye, 2), q = l(i(X), 2);
          bt(q), n(X);
          var ue = l(X, 2), be = l(i(ue), 2), Pe = i(be);
          Pe.value = (Pe.__value = null) ?? "";
          var Be = l(Pe);
          He(Be, 1, u, pt, (rt, at) => {
            var nt = Fu(), gt = i(nt, !0);
            n(nt);
            var ht = {};
            C(() => {
              _(gt, s(at).name ?? "Unnamed"), ht !== (ht = s(at).cluster_id) && (nt.value = (nt.__value = s(at).cluster_id) ?? "");
            }), f(rt, nt);
          }), n(be), n(ue);
          var Qe = l(ue, 2);
          Qe.__click = Ce, n(Y), n(V), C(
            (rt, at) => {
              Qe.disabled = rt, Re(
                Qe,
                1,
                `px-3 py-1.5 rounded text-[10px] transition-colors shrink-0
											${at ?? ""}`,
                "svelte-gwxd3p"
              );
            },
            [
              () => !s(ge) || !s(ie) || s(ge) === s(ie) || !s(ae).trim(),
              () => s(ge) && s(ie) && s(ge) !== s(ie) && s(ae).trim() ? "bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30" : "bg-surface-700 text-surface-500 cursor-not-allowed"
            ]
          ), Is(_e, () => s(ge), (rt) => x(ge, rt)), xt(q, () => s(ae), (rt) => x(ae, rt)), Is(be, () => s(ie), (rt) => x(ie, rt)), f(J, V);
        };
        P(m, (J) => {
          u().length >= 2 && J(y);
        });
      }
      n(Se), n(oe);
      var le = l(oe, 2), O = i(le), re = i(O);
      He(re, 5, () => r().slice(2), pt, (J, V) => {
        var Y = Vu();
        Y.__click = () => $r(H(s(V).prompt), s(V).id);
        var ye = i(Y), _e = i(ye, !0);
        n(ye);
        var ce = l(ye, 2), Q = i(ce);
        n(ce), n(Y), C(() => {
          _(_e, s(V).icon), _(Q, `Ask ${s(V).name ?? ""}`);
        }), f(J, Y);
      }), n(re);
      var he = l(re, 2);
      he.__click = me;
      var we = i(he, !0);
      n(he), n(O), n(le), n(D), C(() => {
        he.disabled = T(), Re(
          he,
          1,
          `text-[10px] px-4 py-1.5 rounded font-medium transition-colors
								${T() ? "bg-surface-700 text-surface-500 cursor-not-allowed" : "bg-hecate-600 text-surface-50 hover:bg-hecate-500"}`,
          "svelte-gwxd3p"
        ), _(we, T() ? "Promoting..." : "Promote to Divisions");
      }), f(L, D);
    }, st = (L) => {
      var D = qu(), oe = i(D), Se = i(oe);
      Se.textContent = "✓";
      var G = l(Se, 4), ee = i(G);
      n(G);
      var Ae = l(G, 4);
      Ae.__click = function(...Le) {
        yc?.apply(this, Le);
      }, n(oe), n(D), C(() => _(ee, `${u().length ?? ""} divisions identified from
						${a() ?? ""} domain events, with
						${v().length ?? ""} integration fact${v().length !== 1 ? "s" : ""} mapped.`)), f(L, D);
    }, Je = (L) => {
      var D = Hu(), oe = i(D), Se = i(oe);
      Se.textContent = "⏸";
      var G = l(Se, 6);
      G.__click = () => bc(F()), n(oe), n(D), f(L, D);
    };
    P(xe, (L) => {
      o() === "ready" ? L(je) : o() === "storm" ? L(Me, 1) : o() === "stack" ? L(et, 2) : o() === "groom" ? L(qe, 3) : o() === "cluster" ? L(Ke, 4) : o() === "name" ? L(We, 5) : o() === "map" ? L(tt, 6) : o() === "promoted" ? L(st, 7) : o() === "shelved" && L(Je, 8);
    });
  }
  n(B), n(I), C(() => Re(
    B,
    1,
    `flex-1 overflow-y-auto transition-opacity duration-150
		${s(ne) ? "opacity-0" : "opacity-100"}`,
    "svelte-gwxd3p"
  )), f(e, I), St(), W();
}
Ot(["click", "keydown"]);
Dt(Va, {}, [], [], { mode: "open" });
const xr = Ze([]), gn = Ze(null), Uu = Rt(xr, (e) => {
  const t = /* @__PURE__ */ new Set();
  for (const r of e)
    r.aggregate && t.add(r.aggregate);
  return Array.from(t).sort();
}), zu = Rt(xr, (e) => {
  const t = /* @__PURE__ */ new Map(), r = [];
  for (const a of e)
    if (a.aggregate) {
      const o = t.get(a.aggregate) || [];
      o.push(a), t.set(a.aggregate, o);
    } else
      r.push(a);
  return { grouped: t, ungrouped: r };
});
function Yu(e, t, r = "human") {
  const a = crypto.randomUUID(), o = {
    id: a,
    name: e.trim(),
    aggregate: t?.trim() || void 0,
    execution: r,
    policies: [],
    events: []
  };
  return xr.update((c) => [...c, o]), a;
}
function Ku(e) {
  xr.update((t) => t.filter((r) => r.id !== e));
}
function Ju(e, t) {
  xr.update(
    (r) => r.map((a) => a.id === e ? { ...a, ...t } : a)
  );
}
function Qu(e, t) {
  xr.update(
    (r) => r.map((a) => a.id === e ? { ...a, execution: t } : a)
  );
}
function Xu(e, t) {
  const r = { id: crypto.randomUUID(), text: t.trim() };
  xr.update(
    (a) => a.map(
      (o) => o.id === e ? { ...o, policies: [...o.policies, r] } : o
    )
  );
}
function Zu(e, t) {
  xr.update(
    (r) => r.map(
      (a) => a.id === e ? { ...a, policies: a.policies.filter((o) => o.id !== t) } : a
    )
  );
}
function ev(e, t) {
  const r = { id: crypto.randomUUID(), text: t.trim() };
  xr.update(
    (a) => a.map(
      (o) => o.id === e ? { ...o, events: [...o.events, r] } : o
    )
  );
}
function tv(e, t) {
  xr.update(
    (r) => r.map(
      (a) => a.id === e ? { ...a, events: a.events.filter((o) => o.id !== t) } : a
    )
  );
}
async function rv(e, t) {
  try {
    return await Ue().post(`/stormings/${e}/design-aggregate`, t), !0;
  } catch (r) {
    const a = r;
    return gn.set(a.message || "Failed to design aggregate"), !1;
  }
}
async function sv(e, t) {
  try {
    return await Ue().post(`/stormings/${e}/design-event`, t), !0;
  } catch (r) {
    const a = r;
    return gn.set(a.message || "Failed to design event"), !1;
  }
}
async function qn(e, t) {
  try {
    return await Ue().post(`/stormings/${e}/plan-desk`, t), !0;
  } catch (r) {
    const a = r;
    return gn.set(a.message || "Failed to plan desk"), !1;
  }
}
var av = /* @__PURE__ */ p(`<button class="text-[10px] px-2.5 py-1 rounded bg-hecate-600/20 text-hecate-300
					hover:bg-hecate-600/30 transition-colors disabled:opacity-50 disabled:cursor-not-allowed"> </button>`), nv = /* @__PURE__ */ p(`<button class="text-[10px] px-2 py-1 rounded text-surface-400
					hover:text-hecate-300 hover:bg-hecate-600/10 transition-colors" title="Get AI assistance"></button>`), iv = /* @__PURE__ */ p('<div><div class="flex items-start gap-2"><span class="text-hecate-400 text-sm mt-0.5"> </span> <div class="flex-1 min-w-0"><div class="flex items-center gap-2"><h3 class="text-xs font-semibold text-surface-100"> </h3> <span> </span></div> <p class="text-[11px] text-surface-400 mt-1"> </p></div></div> <div class="flex items-center gap-2 mt-1"><!> <!></div></div>');
function _t(e, t) {
  Ct(t, !0);
  let r = ft(t, "title", 7), a = ft(t, "description", 7), o = ft(t, "icon", 7, "■"), c = ft(t, "status", 7, "pending"), d = ft(t, "aiContext", 7), u = ft(t, "onaction", 7), v = ft(t, "actionLabel", 7, "Execute"), h = ft(t, "disabled", 7, !1), w = /* @__PURE__ */ Te(() => b(c()));
  function g(ne) {
    switch (ne) {
      case "active":
        return "border-hecate-600/40";
      case "done":
        return "border-health-ok/30";
      default:
        return "border-surface-600";
    }
  }
  function b(ne) {
    switch (ne) {
      case "active":
        return { text: "Active", cls: "bg-hecate-600/20 text-hecate-300" };
      case "done":
        return { text: "Done", cls: "bg-health-ok/10 text-health-ok" };
      default:
        return { text: "Pending", cls: "bg-surface-700 text-surface-400" };
    }
  }
  var T = {
    get title() {
      return r();
    },
    set title(ne) {
      r(ne), vt();
    },
    get description() {
      return a();
    },
    set description(ne) {
      a(ne), vt();
    },
    get icon() {
      return o();
    },
    set icon(ne = "■") {
      o(ne), vt();
    },
    get status() {
      return c();
    },
    set status(ne = "pending") {
      c(ne), vt();
    },
    get aiContext() {
      return d();
    },
    set aiContext(ne) {
      d(ne), vt();
    },
    get onaction() {
      return u();
    },
    set onaction(ne) {
      u(ne), vt();
    },
    get actionLabel() {
      return v();
    },
    set actionLabel(ne = "Execute") {
      v(ne), vt();
    },
    get disabled() {
      return h();
    },
    set disabled(ne = !1) {
      h(ne), vt();
    }
  }, S = iv(), W = i(S), $ = i(W), R = i($, !0);
  n($);
  var K = l($, 2), ge = i(K), ie = i(ge), ae = i(ie, !0);
  n(ie);
  var Z = l(ie, 2), De = i(Z, !0);
  n(Z), n(ge);
  var Ve = l(ge, 2), Ge = i(Ve, !0);
  n(Ve), n(K), n(W);
  var ve = l(W, 2), N = i(ve);
  {
    var U = (ne) => {
      var k = av();
      k.__click = function(...Fe) {
        u()?.apply(this, Fe);
      };
      var F = i(k, !0);
      n(k), C(() => {
        k.disabled = h(), _(F, v());
      }), f(ne, k);
    };
    P(N, (ne) => {
      u() && ne(U);
    });
  }
  var pe = l(N, 2);
  {
    var Ne = (ne) => {
      var k = nv();
      k.__click = () => $r(d()), k.textContent = "✦ AI", f(ne, k);
    };
    P(pe, (ne) => {
      d() && ne(Ne);
    });
  }
  return n(ve), n(S), C(
    (ne) => {
      Re(S, 1, `rounded-lg bg-surface-800 border ${ne ?? ""} p-4 flex flex-col gap-2 transition-colors hover:border-surface-500`), _(R, o()), _(ae, r()), Re(Z, 1, `text-[9px] px-1.5 py-0.5 rounded ${s(w).cls ?? ""}`), _(De, s(w).text), _(Ge, a());
    },
    [() => g(c())]
  ), f(e, S), St(T);
}
Ot(["click"]);
Dt(
  _t,
  {
    title: {},
    description: {},
    icon: {},
    status: {},
    aiContext: {},
    onaction: {},
    actionLabel: {},
    disabled: {}
  },
  [],
  [],
  { mode: "open" }
);
var ov = /* @__PURE__ */ p(`<div class="group/policy flex items-center gap-1 px-2 py-1 rounded-l rounded-r-sm
						bg-es-policy/15 border border-es-policy/30 text-[9px] text-surface-200
						max-w-[160px]"><span class="truncate flex-1"> </span> <button class="text-[7px] text-surface-500 hover:text-health-err
							opacity-0 group-hover/policy:opacity-100 transition-opacity shrink-0"></button></div>`), lv = /* @__PURE__ */ p(`<input class="flex-1 bg-surface-700 border border-es-command/30 rounded px-2 py-0.5
							text-xs font-semibold text-surface-100
							focus:outline-none focus:border-es-command"/>`), cv = /* @__PURE__ */ p(`<button class="flex-1 text-left text-xs font-semibold text-surface-100
							hover:text-es-command transition-colors" title="Double-click to rename"> </button>`), dv = /* @__PURE__ */ p('<span class="text-[9px] text-es-aggregate/70"> </span>'), uv = /* @__PURE__ */ p(`<div class="group/event flex items-center gap-1 px-2 py-1 rounded-r rounded-l-sm
						bg-es-event/15 border border-es-event/30 text-[9px] text-surface-200
						max-w-[200px]"><span class="truncate flex-1"> </span> <button class="text-[7px] text-surface-500 hover:text-health-err
							opacity-0 group-hover/event:opacity-100 transition-opacity shrink-0"></button></div>`), vv = /* @__PURE__ */ p(`<div class="flex items-stretch gap-0 group/card"><div class="flex flex-col items-end gap-1 -mr-2 z-10 pt-1 min-w-[100px]"><!> <input placeholder="+ policy" class="w-24 bg-transparent border border-dashed border-es-policy/20 rounded
					px-1.5 py-0.5 text-[8px] text-surface-400 placeholder-surface-500
					focus:outline-none focus:border-es-policy/40
					opacity-0 group-hover/card:opacity-100 transition-opacity"/></div> <div class="relative flex-1 rounded-lg border-2 border-es-command/40 bg-es-command/10
				px-4 py-3 min-h-[72px] z-20"><div class="flex items-center gap-2 mb-1"><button> </button> <!> <div class="flex items-center gap-1 opacity-0 group-hover/card:opacity-100 transition-opacity"><button class="text-[8px] px-1.5 py-0.5 rounded text-health-ok
							hover:bg-health-ok/10 transition-colors" title="Promote to daemon"></button> <button class="text-[8px] px-1 py-0.5 rounded text-surface-500
							hover:text-health-err hover:bg-health-err/10 transition-colors" title="Remove desk"></button></div></div> <!></div> <div class="flex flex-col items-start gap-1 -ml-2 z-10 pt-1 min-w-[100px]"><!> <input placeholder="+ event" class="w-32 bg-transparent border border-dashed border-es-event/20 rounded
					px-1.5 py-0.5 text-[8px] text-surface-400 placeholder-surface-500
					focus:outline-none focus:border-es-event/40
					opacity-0 group-hover/card:opacity-100 transition-opacity"/></div></div>`), fv = /* @__PURE__ */ p("<option></option>"), pv = /* @__PURE__ */ p('<div class="space-y-2"><div class="flex items-center gap-2"><div class="w-3 h-3 rounded-sm bg-es-aggregate/40"></div> <span class="text-[10px] font-semibold text-es-aggregate uppercase tracking-wider"> </span> <div class="flex-1 h-px bg-es-aggregate/20"></div> <span class="text-[9px] text-surface-400"> </span></div> <div class="space-y-3 ml-5"></div></div>'), xv = /* @__PURE__ */ p('<div class="flex items-center gap-2"><span class="text-[10px] font-semibold text-surface-400 uppercase tracking-wider">No Aggregate</span> <div class="flex-1 h-px bg-surface-600"></div></div>'), hv = /* @__PURE__ */ p("<!> <div></div>", 1), _v = /* @__PURE__ */ p("<!> <!>", 1), gv = /* @__PURE__ */ p(`<div class="text-center py-8 text-surface-500 text-xs border border-dashed border-surface-600 rounded-lg">No desk cards yet. Add your first command desk above,
				or ask an AI agent for suggestions.</div>`), bv = /* @__PURE__ */ p(`<button class="rounded-lg border border-surface-600 bg-surface-800/50
							p-3 text-left transition-all hover:border-hecate-500/40
							hover:bg-surface-700/50 group"><div class="flex items-center gap-2 mb-1.5"><span class="text-hecate-400 group-hover:text-hecate-300 transition-colors"> </span> <span class="text-[11px] font-semibold text-surface-100"> </span></div> <div class="text-[10px] text-surface-400 mb-1"> </div> <div class="text-[9px] text-surface-500"> </div></button>`), mv = /* @__PURE__ */ p(
  `<div class="rounded-lg border border-es-command/20 bg-es-command/5 p-3"><div class="flex items-end gap-2"><div class="flex-1"><label class="text-[9px] text-surface-400 block mb-1">Desk Name (command)</label> <input placeholder="e.g., register_user, process_order" class="w-full bg-surface-700 border border-surface-600 rounded px-2.5 py-1.5
							text-xs text-surface-100 placeholder-surface-400
							focus:outline-none focus:border-es-command/50"/></div> <div class="w-40"><label class="text-[9px] text-surface-400 block mb-1">Aggregate</label> <input placeholder="e.g., user, order" list="existing-aggregates" class="w-full bg-surface-700 border border-surface-600 rounded px-2.5 py-1.5
							text-xs text-surface-100 placeholder-surface-400
							focus:outline-none focus:border-surface-500"/> <datalist id="existing-aggregates"></datalist></div> <div class="w-24"><label class="text-[9px] text-surface-400 block mb-1">Execution</label> <select class="w-full bg-surface-700 border border-surface-600 rounded px-2 py-1.5
							text-xs text-surface-100
							focus:outline-none focus:border-surface-500"><option>Human</option><option>Agent</option><option>Both</option></select></div> <button>+ Desk</button></div></div> <!> <div class="rounded-lg border border-hecate-600/20 bg-hecate-950/20 p-4"><div class="flex items-center gap-2 mb-3"><span class="text-hecate-400"></span> <h4 class="text-xs font-semibold text-surface-100">AI Domain Experts</h4> <span class="text-[10px] text-surface-400">Ask a virtual agent to analyze the domain and suggest desk cards</span></div> <div class="grid grid-cols-2 md:grid-cols-4 gap-2"></div></div> <div><h4 class="text-xs font-semibold text-surface-100 mb-3">Design Tasks</h4> <div class="grid grid-cols-1 md:grid-cols-2 gap-3"><!> <!> <!> <!></div></div>`,
  1
), yv = /* @__PURE__ */ p(`<div class="flex gap-2 items-end mb-4 p-3 rounded bg-surface-700/30"><div class="flex-1"><label for="desk-name" class="text-[10px] text-surface-400 block mb-1">Desk Name</label> <input id="desk-name" placeholder="e.g., register_user" class="w-full bg-surface-700 border border-surface-600 rounded
								px-2.5 py-1.5 text-xs text-surface-100 placeholder-surface-400
								focus:outline-none focus:border-hecate-500/50"/></div> <div class="flex-1"><label for="desk-desc" class="text-[10px] text-surface-400 block mb-1">Description</label> <input id="desk-desc" placeholder="Brief purpose of this desk" class="w-full bg-surface-700 border border-surface-600 rounded
								px-2.5 py-1.5 text-xs text-surface-100 placeholder-surface-400
								focus:outline-none focus:border-hecate-500/50"/></div> <div><label for="desk-dept" class="text-[10px] text-surface-400 block mb-1">Dept</label> <select id="desk-dept" class="bg-surface-700 border border-surface-600 rounded
								px-2 py-1.5 text-xs text-surface-100
								focus:outline-none focus:border-hecate-500/50 cursor-pointer"><option>CMD</option><option>QRY</option><option>PRJ</option></select></div> <button class="px-3 py-1.5 rounded text-xs bg-hecate-600/20 text-hecate-300
							hover:bg-hecate-600/30 transition-colors disabled:opacity-50">Plan</button> <button class="px-3 py-1.5 rounded text-xs text-surface-400 hover:text-surface-100">Cancel</button></div>`), wv = /* @__PURE__ */ p(
  `<div class="rounded-lg border border-surface-600 bg-surface-800/50 p-4"><div class="flex items-center justify-between mb-3"><h4 class="text-xs font-semibold text-surface-100">Desk Inventory</h4> <button class="text-[10px] px-2 py-0.5 rounded bg-hecate-600/10 text-hecate-300
						hover:bg-hecate-600/20 transition-colors">+ Plan Desk</button></div> <!> <p class="text-[10px] text-surface-400">Desks are individual capabilities within a department. Each desk owns a
				vertical slice: command + event + handler + projection.</p></div> <div><h4 class="text-xs font-semibold text-surface-100 mb-3">Planning Tasks</h4> <div class="grid grid-cols-1 md:grid-cols-2 gap-3"><!> <!> <!> <!></div></div>`,
  1
), kv = /* @__PURE__ */ p('<div class="p-4 space-y-6"><div class="flex items-center justify-between"><div><h3 class="text-sm font-semibold text-surface-100">Storming</h3> <p class="text-[11px] text-surface-400 mt-0.5">Design aggregates, events, desks, and dependencies for <span class="text-surface-200"> </span></p></div> <div class="flex items-center gap-1 bg-surface-700/50 rounded-lg p-0.5"><button>Event Storm</button> <button>Desk Inventory</button></div></div> <!></div>');
function co(e, t) {
  Ct(t, !0);
  const r = () => $e(Hr, "$selectedDivision", v), a = () => $e(xr, "$deskCards", v), o = () => $e(Uu, "$deskAggregates", v), c = () => $e(zu, "$deskCardsByAggregate", v), d = () => $e(ad, "$designLevelAgents", v), u = () => $e(wt, "$isLoading", v), [v, h] = qt(), w = (A, E = Er) => {
    var te = vv(), ke = i(te), Ie = i(ke);
    He(Ie, 17, () => E().policies, (G) => G.id, (G, ee) => {
      var Ae = ov(), Le = i(Ae), m = i(Le, !0);
      n(Le);
      var y = l(Le, 2);
      y.__click = () => Zu(E().id, s(ee).id), y.textContent = "✕", n(Ae), C(() => _(m, s(ee).text)), f(G, Ae);
    });
    var j = l(Ie, 2);
    bt(j), j.__keydown = (G) => Ge(G, E().id), n(ke);
    var B = l(ke, 2), xe = i(B), je = i(xe);
    je.__click = () => pe(E());
    var Me = i(je, !0);
    n(je);
    var et = l(je, 2);
    {
      var qe = (G) => {
        var ee = lv();
        bt(ee), ee.__keydown = (Ae) => {
          Ae.key === "Enter" && U(E().id), Ae.key === "Escape" && x($, null);
        }, Mt("blur", ee, () => U(E().id)), xt(ee, () => s(R), (Ae) => x(R, Ae)), f(G, ee);
      }, Ke = (G) => {
        var ee = cv();
        ee.__dblclick = () => N(E());
        var Ae = i(ee, !0);
        n(ee), C(() => _(Ae, E().name)), f(G, ee);
      };
      P(et, (G) => {
        s($) === E().id ? G(qe) : G(Ke, !1);
      });
    }
    var We = l(et, 2), tt = i(We);
    tt.__click = () => F(E()), tt.textContent = "↑ promote";
    var st = l(tt, 2);
    st.__click = () => Ku(E().id), st.textContent = "✕", n(We), n(xe);
    var Je = l(xe, 2);
    {
      var L = (G) => {
        var ee = dv(), Ae = i(ee);
        n(ee), C(() => _(Ae, `■ ${E().aggregate ?? ""}`)), f(G, ee);
      };
      P(Je, (G) => {
        E().aggregate && G(L);
      });
    }
    n(B);
    var D = l(B, 2), oe = i(D);
    He(oe, 17, () => E().events, (G) => G.id, (G, ee) => {
      var Ae = uv(), Le = i(Ae), m = i(Le, !0);
      n(Le);
      var y = l(Le, 2);
      y.__click = () => tv(E().id, s(ee).id), y.textContent = "✕", n(Ae), C(() => _(m, s(ee).text)), f(G, Ae);
    });
    var Se = l(oe, 2);
    bt(Se), Se.__keydown = (G) => ve(G, E().id), n(D), n(te), C(
      (G, ee, Ae) => {
        Re(je, 1, `text-sm ${G ?? ""}
						hover:scale-110 transition-transform`), zt(je, "title", `${ee ?? ""} — click to cycle`), _(Me, Ae);
      },
      [
        () => k(E().execution),
        () => ne(E().execution),
        () => Ne(E().execution)
      ]
    ), xt(j, () => s(S)[E().id], (G) => s(S)[E().id] = G), xt(Se, () => s(W)[E().id], (G) => s(W)[E().id] = G), f(A, te);
  };
  let g = /* @__PURE__ */ se(""), b = /* @__PURE__ */ se(""), T = /* @__PURE__ */ se("human"), S = /* @__PURE__ */ se(Ut({})), W = /* @__PURE__ */ se(Ut({})), $ = /* @__PURE__ */ se(null), R = /* @__PURE__ */ se(""), K = /* @__PURE__ */ se(!1), ge = /* @__PURE__ */ se(""), ie = /* @__PURE__ */ se(""), ae = /* @__PURE__ */ se("cmd"), Z = /* @__PURE__ */ se("design");
  function De() {
    s(g).trim() && (Yu(s(g), s(b) || void 0, s(T)), x(g, ""), x(b, ""), x(T, "human"));
  }
  function Ve(A) {
    A.key === "Enter" && !A.shiftKey && s(g).trim() && (A.preventDefault(), De());
  }
  function Ge(A, E) {
    A.key === "Enter" && s(S)[E]?.trim() && (A.preventDefault(), Xu(E, s(S)[E]), s(S)[E] = "");
  }
  function ve(A, E) {
    A.key === "Enter" && s(W)[E]?.trim() && (A.preventDefault(), ev(E, s(W)[E]), s(W)[E] = "");
  }
  function N(A) {
    x($, A.id, !0), x(R, A.name, !0);
  }
  function U(A) {
    s(R).trim() && Ju(A, { name: s(R).trim() }), x($, null);
  }
  function pe(A) {
    const E = ["human", "agent", "both"], te = E.indexOf(A.execution);
    Qu(A.id, E[(te + 1) % E.length]);
  }
  function Ne(A) {
    switch (A) {
      case "human":
        return "𝗨";
      case "agent":
        return "⚙";
      case "both":
      case "pair":
        return "✦";
    }
  }
  function ne(A) {
    switch (A) {
      case "human":
        return "Interactive (human)";
      case "agent":
        return "Automated (AI agent)";
      case "both":
      case "pair":
        return "Assisted (human + AI)";
    }
  }
  function k(A) {
    switch (A) {
      case "human":
        return "text-es-command";
      case "agent":
        return "text-hecate-400";
      case "both":
      case "pair":
        return "text-phase-crafting";
    }
  }
  async function F(A) {
    if (!r()) return;
    const E = r().division_id;
    await qn(E, {
      desk_name: A.name,
      description: [
        A.execution === "agent" ? "AI-automated" : A.execution === "both" ? "Human+AI assisted" : "Interactive",
        A.policies.length > 0 ? `Policies: ${A.policies.map((te) => te.text).join(", ")}` : "",
        A.events.length > 0 ? `Emits: ${A.events.map((te) => te.text).join(", ")}` : ""
      ].filter(Boolean).join(". "),
      department: "CMD"
    });
    for (const te of A.events)
      await sv(E, {
        event_name: te.text,
        aggregate_type: A.aggregate || A.name
      });
    A.aggregate && await rv(E, { aggregate_name: A.aggregate });
  }
  async function Fe() {
    if (!r() || !s(ge).trim()) return;
    await qn(r().division_id, {
      desk_name: s(ge).trim(),
      description: s(ie).trim() || void 0,
      department: s(ae)
    }) && (x(ge, ""), x(ie, ""), x(K, !1));
  }
  function ze(A) {
    const E = r()?.context_name ?? "this division", te = a(), ke = te.map((xe) => xe.name).join(", "), Ie = te.flatMap((xe) => xe.events.map((je) => je.text)).join(", "), j = te.flatMap((xe) => xe.policies.map((je) => je.text)).join(", ");
    let B = `We are doing Design-Level Event Storming for the "${E}" division.

`;
    return B += `Our board uses command-centric desk cards:
`, B += `- Each card = a desk (command/slice)
`, B += `- Left side: policies (grey) = filter/guard conditions
`, B += `- Right side: events (orange) = what the desk emits
`, B += `- Cards can be human (interactive), agent (AI), or both

`, ke && (B += `Desks so far: ${ke}
`), Ie && (B += `Events so far: ${Ie}
`), j && (B += `Policies so far: ${j}
`), B += `
${A.prompt}

Please analyze and suggest items for the board.`, B;
  }
  var Xe = kv(), Ee = i(Xe), Ce = i(Ee), me = l(i(Ce), 2), fe = l(i(me)), de = i(fe, !0);
  n(fe), n(me), n(Ce);
  var H = l(Ce, 2), M = i(H);
  M.__click = () => x(Z, "design");
  var I = l(M, 2);
  I.__click = () => x(Z, "plan"), n(H), n(Ee);
  var z = l(Ee, 2);
  {
    var Oe = (A) => {
      var E = mv(), te = ut(E), ke = i(te), Ie = i(ke), j = l(i(Ie), 2);
      bt(j), j.__keydown = Ve, n(Ie);
      var B = l(Ie, 2), xe = l(i(B), 2);
      bt(xe);
      var je = l(xe, 2);
      He(je, 5, o, pt, (O, re) => {
        var he = fv(), we = {};
        C(() => {
          we !== (we = s(re)) && (he.value = (he.__value = s(re)) ?? "");
        }), f(O, he);
      }), n(je), n(B);
      var Me = l(B, 2), et = l(i(Me), 2), qe = i(et);
      qe.value = qe.__value = "human";
      var Ke = l(qe);
      Ke.value = Ke.__value = "agent";
      var We = l(Ke);
      We.value = We.__value = "both", n(et), n(Me);
      var tt = l(Me, 2);
      tt.__click = De, n(ke), n(te);
      var st = l(te, 2);
      {
        var Je = (O) => {
          const re = /* @__PURE__ */ Te(() => {
            const { grouped: Y, ungrouped: ye } = c();
            return { grouped: Y, ungrouped: ye };
          });
          var he = _v(), we = ut(he);
          He(we, 17, () => [...s(re).grouped.entries()], pt, (Y, ye) => {
            var _e = /* @__PURE__ */ Te(() => ka(s(ye), 2));
            let ce = () => s(_e)[0], Q = () => s(_e)[1];
            var X = pv(), q = i(X), ue = l(i(q), 2), be = i(ue, !0);
            n(ue);
            var Pe = l(ue, 4), Be = i(Pe);
            n(Pe), n(q);
            var Qe = l(q, 2);
            He(Qe, 21, Q, (rt) => rt.id, (rt, at) => {
              w(rt, () => s(at));
            }), n(Qe), n(X), C(() => {
              _(be, ce()), _(Be, `${Q().length ?? ""} desk${Q().length !== 1 ? "s" : ""}`);
            }), f(Y, X);
          });
          var J = l(we, 2);
          {
            var V = (Y) => {
              var ye = hv(), _e = ut(ye);
              {
                var ce = (X) => {
                  var q = xv();
                  f(X, q);
                };
                P(_e, (X) => {
                  s(re).grouped.size > 0 && X(ce);
                });
              }
              var Q = l(_e, 2);
              He(Q, 21, () => s(re).ungrouped, (X) => X.id, (X, q) => {
                w(X, () => s(q));
              }), n(Q), C(() => Re(Q, 1, `space-y-3 ${s(re).grouped.size > 0 ? "ml-5" : ""}`)), f(Y, ye);
            };
            P(J, (Y) => {
              s(re).ungrouped.length > 0 && Y(V);
            });
          }
          f(O, he);
        }, L = (O) => {
          var re = gv();
          f(O, re);
        };
        P(st, (O) => {
          a().length > 0 ? O(Je) : O(L, !1);
        });
      }
      var D = l(st, 2), oe = i(D), Se = i(oe);
      Se.textContent = "✦", Et(4), n(oe);
      var G = l(oe, 2);
      He(G, 5, d, pt, (O, re) => {
        var he = bv();
        he.__click = () => $r(ze(s(re)));
        var we = i(he), J = i(we), V = i(J, !0);
        n(J);
        var Y = l(J, 2), ye = i(Y, !0);
        n(Y), n(we);
        var _e = l(we, 2), ce = i(_e, !0);
        n(_e);
        var Q = l(_e, 2), X = i(Q, !0);
        n(Q), n(he), C(() => {
          _(V, s(re).icon), _(ye, s(re).name), _(ce, s(re).role), _(X, s(re).description);
        }), f(O, he);
      }), n(G), n(D);
      var ee = l(D, 2), Ae = l(i(ee), 2), Le = i(Ae);
      {
        let O = /* @__PURE__ */ Te(() => `Help me design aggregates for the "${r()?.context_name}" division. What are the natural consistency boundaries? What entities accumulate history over time?`);
        _t(Le, {
          title: "Design Aggregates",
          description: "Identify aggregate boundaries, define stream patterns and status flags",
          icon: "■",
          get aiContext() {
            return s(O);
          }
        });
      }
      var m = l(Le, 2);
      {
        let O = /* @__PURE__ */ Te(() => `Help me define status bit flags for aggregates in the "${r()?.context_name}" division. Each aggregate needs lifecycle states as bit flags (powers of 2).`);
        _t(m, {
          title: "Define Status Flags",
          description: "Design bit flag status fields for each aggregate lifecycle",
          icon: "⚑",
          get aiContext() {
            return s(O);
          }
        });
      }
      var y = l(m, 2);
      {
        let O = /* @__PURE__ */ Te(() => `Help me identify read models for the "${r()?.context_name}" division. What queries will users run? What data views are needed?`);
        _t(y, {
          title: "Map Read Models",
          description: "Identify what queries users will run and what data they need",
          icon: "▶",
          get aiContext() {
            return s(O);
          }
        });
      }
      var le = l(y, 2);
      {
        let O = /* @__PURE__ */ Te(() => `Help me create a domain glossary for the "${r()?.context_name}" division. Define key terms, bounded context boundaries, and ubiquitous language.`);
        _t(le, {
          title: "Domain Glossary",
          description: "Document ubiquitous language and bounded context definitions",
          icon: "✎",
          get aiContext() {
            return s(O);
          }
        });
      }
      n(Ae), n(ee), C(
        (O, re) => {
          tt.disabled = O, Re(tt, 1, `px-3 py-1.5 rounded text-xs transition-colors shrink-0
						${re ?? ""}`);
        },
        [
          () => !s(g).trim(),
          () => s(g).trim() ? "bg-es-command/20 text-es-command hover:bg-es-command/30" : "bg-surface-700 text-surface-500 cursor-not-allowed"
        ]
      ), xt(j, () => s(g), (O) => x(g, O)), xt(xe, () => s(b), (O) => x(b, O)), Is(et, () => s(T), (O) => x(T, O)), f(A, E);
    }, Ye = (A) => {
      var E = wv(), te = ut(E), ke = i(te), Ie = l(i(ke), 2);
      Ie.__click = () => x(K, !s(K)), n(ke);
      var j = l(ke, 2);
      {
        var B = (We) => {
          var tt = yv(), st = i(tt), Je = l(i(st), 2);
          bt(Je), n(st);
          var L = l(st, 2), D = l(i(L), 2);
          bt(D), n(L);
          var oe = l(L, 2), Se = l(i(oe), 2), G = i(Se);
          G.value = G.__value = "cmd";
          var ee = l(G);
          ee.value = ee.__value = "qry";
          var Ae = l(ee);
          Ae.value = Ae.__value = "prj", n(Se), n(oe);
          var Le = l(oe, 2);
          Le.__click = Fe;
          var m = l(Le, 2);
          m.__click = () => x(K, !1), n(tt), C((y) => Le.disabled = y, [() => !s(ge).trim() || u()]), xt(Je, () => s(ge), (y) => x(ge, y)), xt(D, () => s(ie), (y) => x(ie, y)), Is(Se, () => s(ae), (y) => x(ae, y)), f(We, tt);
        };
        P(j, (We) => {
          s(K) && We(B);
        });
      }
      Et(2), n(te);
      var xe = l(te, 2), je = l(i(xe), 2), Me = i(je);
      {
        let We = /* @__PURE__ */ Te(() => `Help me create a desk inventory for the "${r()?.context_name}" division. Each desk is a vertical slice (command + event + handler). Organize by CMD (write), QRY (read), and PRJ (projection) departments.`);
        _t(Me, {
          title: "Desk Inventory",
          description: "Create a complete inventory of desks needed for this division, organized by department (CMD, QRY, PRJ)",
          icon: "▣",
          get aiContext() {
            return s(We);
          }
        });
      }
      var et = l(Me, 2);
      {
        let We = /* @__PURE__ */ Te(() => `Help me map dependencies between desks in the "${r()?.context_name}" division. Which desks depend on which? What's the optimal implementation order?`);
        _t(et, {
          title: "Dependency Mapping",
          description: "Map dependencies between desks to determine implementation order",
          icon: "⇄",
          get aiContext() {
            return s(We);
          }
        });
      }
      var qe = l(et, 2);
      {
        let We = /* @__PURE__ */ Te(() => `Help me sequence the implementation of desks in the "${r()?.context_name}" division into logical sprints. Consider dependencies, walking skeleton principles, and the "initiate + archive" first rule.`);
        _t(qe, {
          title: "Sprint Sequencing",
          description: "Prioritize and sequence desks into implementation sprints",
          icon: "☰",
          get aiContext() {
            return s(We);
          }
        });
      }
      var Ke = l(qe, 2);
      {
        let We = /* @__PURE__ */ Te(() => `Help me design REST API endpoints for the "${r()?.context_name}" division. Follow the pattern: POST /api/{resource}/{action} for commands, GET /api/{resource} for queries.`);
        _t(Ke, {
          title: "API Design",
          description: "Design REST API endpoints for each desk's capabilities",
          icon: "↔",
          get aiContext() {
            return s(We);
          }
        });
      }
      n(je), n(xe), f(A, E);
    };
    P(z, (A) => {
      s(Z) === "design" ? A(Oe) : A(Ye, !1);
    });
  }
  n(Xe), C(() => {
    _(de, r()?.context_name), Re(M, 1, `px-3 py-1 rounded text-[11px] transition-colors
					${s(Z) === "design" ? "bg-surface-600 text-surface-100" : "text-surface-400 hover:text-surface-200"}`), Re(I, 1, `px-3 py-1 rounded text-[11px] transition-colors
					${s(Z) === "plan" ? "bg-surface-600 text-surface-100" : "text-surface-400 hover:text-surface-200"}`);
  }), f(e, Xe), St(), h();
}
Ot(["click", "keydown", "dblclick"]);
Dt(co, {}, [], [], { mode: "open" });
var $v = /* @__PURE__ */ p(`<div class="p-4 space-y-6"><div><h3 class="text-sm font-semibold text-surface-100">Planning</h3> <p class="text-[11px] text-surface-400 mt-0.5">Lifecycle management for <span class="text-surface-200"> </span></p></div> <div class="rounded-lg border border-surface-600 bg-surface-800/50 p-4"><h4 class="text-xs font-semibold text-surface-100 mb-3">Division Lifecycle</h4> <p class="text-[10px] text-surface-400 leading-relaxed">Use the phase controls above to manage this division's planning lifecycle: <span class="text-surface-300">Open</span> to begin work, <span class="text-surface-300">Shelve</span> to pause, <span class="text-surface-300">Resume</span> to continue, or <span class="text-surface-300">Conclude</span> when planning is complete.</p> <p class="text-[10px] text-surface-400 mt-2 leading-relaxed">Content work (designing aggregates, events, desks) happens in the <span class="text-es-event">Storming</span> phase.
			Implementation items are tracked on the <span class="text-hecate-400">Kanban</span> board.</p></div> <div><h4 class="text-xs font-semibold text-surface-100 mb-3">Planning Tasks</h4> <div class="grid grid-cols-1 md:grid-cols-2 gap-3"><!> <!> <!> <!></div></div></div>`);
function uo(e, t) {
  Ct(t, !1);
  const r = () => $e(Hr, "$selectedDivision", a), [a, o] = qt();
  Hi();
  var c = $v(), d = i(c), u = l(i(d), 2), v = l(i(u)), h = i(v, !0);
  n(v), n(u), n(d);
  var w = l(d, 4), g = l(i(w), 2), b = i(g);
  {
    let $ = /* @__PURE__ */ _r(() => `Help me create a desk inventory for the "${r()?.context_name}" division. Each desk is a vertical slice (command + event + handler). Organize by CMD (write), QRY (read), and PRJ (projection) departments.`);
    _t(b, {
      title: "Desk Inventory",
      description: "Create a complete inventory of desks needed for this division, organized by department (CMD, QRY, PRJ)",
      icon: "▣",
      get aiContext() {
        return s($);
      }
    });
  }
  var T = l(b, 2);
  {
    let $ = /* @__PURE__ */ _r(() => `Help me map dependencies between desks in the "${r()?.context_name}" division. Which desks depend on which? What's the optimal implementation order?`);
    _t(T, {
      title: "Dependency Mapping",
      description: "Map dependencies between desks to determine implementation order",
      icon: "⇄",
      get aiContext() {
        return s($);
      }
    });
  }
  var S = l(T, 2);
  {
    let $ = /* @__PURE__ */ _r(() => `Help me sequence the implementation of desks in the "${r()?.context_name}" division into logical sprints. Consider dependencies, walking skeleton principles, and the "initiate + archive" first rule.`);
    _t(S, {
      title: "Sprint Sequencing",
      description: "Prioritize and sequence desks into implementation sprints",
      icon: "☰",
      get aiContext() {
        return s($);
      }
    });
  }
  var W = l(S, 2);
  {
    let $ = /* @__PURE__ */ _r(() => `Help me design REST API endpoints for the "${r()?.context_name}" division. Follow the pattern: POST /api/{resource}/{action} for commands, GET /api/{resource} for queries.`);
    _t(W, {
      title: "API Design",
      description: "Design REST API endpoints for each desk's capabilities",
      icon: "↔",
      get aiContext() {
        return s($);
      }
    });
  }
  n(g), n(w), n(c), C(() => _(h, r()?.context_name)), f(e, c), St(), o();
}
Dt(uo, {}, [], [], { mode: "open" });
const bn = 2, ys = 4, ha = 8, _a = 16, Hn = Ze(null), Tr = Ze([]), At = Ze(null), Ba = Ze(!1), Cv = Rt(
  Tr,
  (e) => e.filter(
    (t) => (t.status & ha) === 0 && (t.status & _a) === 0 && (t.status & bn) === 0 && (t.status & ys) === 0
  )
), Sv = Rt(
  Tr,
  (e) => e.filter(
    (t) => (t.status & bn) !== 0 && (t.status & ha) === 0 && (t.status & _a) === 0 && (t.status & ys) === 0
  )
), Ev = Rt(
  Tr,
  (e) => e.filter((t) => (t.status & ys) !== 0)
), Av = Rt(
  Tr,
  (e) => e.filter((t) => (t.status & ha) !== 0 && (t.status & ys) === 0)
), Dv = Rt(
  Tr,
  (e) => e.filter((t) => (t.status & _a) !== 0 && (t.status & ys) === 0)
), Pv = Rt(Tr, (e) => {
  let t = 0, r = 0, a = 0, o = 0, c = 0;
  for (const d of e)
    d.status & ys ? a++ : d.status & ha ? o++ : d.status & _a ? c++ : d.status & bn ? r++ : t++;
  return { posted: t, picked: r, finished: a, parked: o, blocked: c, total: e.length };
});
async function Mr(e) {
  try {
    Ba.set(!0), At.set(null);
    const r = await Ue().get(
      `/kanbans/${e}`
    );
    Hn.set(r.board), Tr.set(r.cards ?? []);
  } catch (t) {
    const r = t;
    At.set(r.message || "Failed to fetch kanban board"), Hn.set(null), Tr.set([]);
  } finally {
    Ba.set(!1);
  }
}
async function Tv(e, t) {
  try {
    return At.set(null), await Ue().post(`/kanbans/${e}/cards`, t), await Mr(e), !0;
  } catch (r) {
    const a = r;
    return At.set(a.message || "Failed to post card"), !1;
  }
}
async function Rv(e, t, r = "hecate-web") {
  try {
    return At.set(null), await Ue().post(`/kanbans/${e}/cards/${t}/pick`, {
      picked_by: r
    }), await Mr(e), !0;
  } catch (a) {
    const o = a;
    return At.set(o.message || "Failed to pick card"), !1;
  }
}
async function Mv(e, t) {
  try {
    return At.set(null), await Ue().post(`/kanbans/${e}/cards/${t}/finish`, {}), await Mr(e), !0;
  } catch (r) {
    const a = r;
    return At.set(a.message || "Failed to finish card"), !1;
  }
}
async function Iv(e, t, r) {
  try {
    return At.set(null), await Ue().post(`/kanbans/${e}/cards/${t}/unpick`, { reason: r }), await Mr(e), !0;
  } catch (a) {
    const o = a;
    return At.set(o.message || "Failed to unpick card"), !1;
  }
}
async function Lv(e, t, r, a = "hecate-web") {
  try {
    return At.set(null), await Ue().post(`/kanbans/${e}/cards/${t}/park`, {
      reason: r,
      parked_by: a
    }), await Mr(e), !0;
  } catch (o) {
    const c = o;
    return At.set(c.message || "Failed to park card"), !1;
  }
}
async function Nv(e, t) {
  try {
    return At.set(null), await Ue().post(`/kanbans/${e}/cards/${t}/unpark`, {}), await Mr(e), !0;
  } catch (r) {
    const a = r;
    return At.set(a.message || "Failed to unpark card"), !1;
  }
}
async function Ov(e, t, r, a = "hecate-web") {
  try {
    return At.set(null), await Ue().post(`/kanbans/${e}/cards/${t}/block`, {
      reason: r,
      blocked_by: a
    }), await Mr(e), !0;
  } catch (o) {
    const c = o;
    return At.set(c.message || "Failed to block card"), !1;
  }
}
async function Fv(e, t) {
  try {
    return At.set(null), await Ue().post(`/kanbans/${e}/cards/${t}/unblock`, {}), await Mr(e), !0;
  } catch (r) {
    const a = r;
    return At.set(a.message || "Failed to unblock card"), !1;
  }
}
var jv = /* @__PURE__ */ p('<span class="text-health-warn"> </span>'), Vv = /* @__PURE__ */ p('<span class="text-health-err"> </span>'), Bv = /* @__PURE__ */ p('<div class="flex items-center gap-3 text-[10px] text-surface-400 mr-2"><span> </span> <span> </span> <span> </span> <!> <!></div>'), qv = /* @__PURE__ */ p('<div class="text-[11px] text-health-err bg-health-err/10 rounded px-3 py-2"> </div>'), Hv = /* @__PURE__ */ p(`<div class="rounded-lg border border-hecate-600/30 bg-surface-800/80 p-4 space-y-3"><h4 class="text-xs font-medium text-hecate-300 uppercase tracking-wider">New Work Card</h4> <div class="grid grid-cols-[1fr_auto] gap-3"><div><label for="card-title" class="text-[10px] text-surface-400 block mb-1">Title (desk name)</label> <input id="card-title" placeholder="e.g., register_user" class="w-full bg-surface-700 border border-surface-600 rounded px-2.5 py-1.5
							text-xs text-surface-100 placeholder-surface-400
							focus:outline-none focus:border-hecate-500/50"/></div> <div><label for="card-type" class="text-[10px] text-surface-400 block mb-1">Department</label> <select id="card-type" class="bg-surface-700 border border-surface-600 rounded px-2 py-1.5
							text-xs text-surface-100
							focus:outline-none focus:border-hecate-500/50 cursor-pointer"><option>CMD</option><option>PRJ</option><option>QRY</option></select></div></div> <div><label for="card-desc" class="text-[10px] text-surface-400 block mb-1">Description (optional)</label> <input id="card-desc" placeholder="Brief description of this desk" class="w-full bg-surface-700 border border-surface-600 rounded px-2.5 py-1.5
						text-xs text-surface-100 placeholder-surface-400
						focus:outline-none focus:border-hecate-500/50"/></div> <div class="flex gap-2"><button>Post</button> <button class="px-3 py-1.5 rounded text-xs text-surface-400 hover:text-surface-100">Cancel</button></div></div>`), Gv = /* @__PURE__ */ p('<div class="text-center py-8 text-surface-400 text-xs animate-pulse">Loading kanban board...</div>'), Wv = /* @__PURE__ */ p('<div class="text-center py-12 text-surface-500 text-xs border border-dashed border-surface-600 rounded-lg"><div class="text-2xl mb-3 text-surface-400"></div> <p class="mb-1">No work cards yet.</p> <p class="text-[10px] text-surface-500">Post cards from storming output, or add them manually above.</p></div>'), Uv = /* @__PURE__ */ p('<p class="text-[10px] text-surface-400 mb-2 leading-relaxed"> </p>'), zv = /* @__PURE__ */ p(`<div class="rounded border border-surface-600 bg-surface-800/60 p-2.5 group"><div class="flex items-start gap-2 mb-1.5"><span class="text-xs font-medium text-surface-100 flex-1 leading-tight"> </span> <span> </span></div> <!> <div class="flex items-center justify-between"><span class="text-[9px] text-surface-500"> </span> <div class="flex items-center gap-1
									opacity-0 group-hover:opacity-100 transition-opacity"><button class="text-[10px] px-2 py-0.5 rounded bg-hecate-600/15 text-hecate-300
											hover:bg-hecate-600/25 transition-colors">Pick</button> <button class="text-[10px] px-1.5 py-0.5 rounded text-surface-400
											hover:text-health-warn hover:bg-health-warn/10 transition-colors" title="Park card"></button> <button class="text-[10px] px-1.5 py-0.5 rounded text-surface-400
											hover:text-health-err hover:bg-health-err/10 transition-colors" title="Block card"></button></div></div></div>`), Yv = /* @__PURE__ */ p('<p class="text-[10px] text-surface-400 mb-2 leading-relaxed"> </p>'), Kv = /* @__PURE__ */ p('<div class="text-[9px] text-surface-400 mb-2"> </div>'), Jv = /* @__PURE__ */ p(`<div class="rounded border border-surface-600 bg-surface-800/60 p-2.5 group"><div class="flex items-start gap-2 mb-1.5"><span class="text-xs font-medium text-surface-100 flex-1 leading-tight"> </span> <span> </span></div> <!> <!> <div class="flex items-center gap-1 justify-end
								opacity-0 group-hover:opacity-100 transition-opacity"><button class="text-[10px] px-2 py-0.5 rounded text-health-warn
										hover:bg-health-warn/10 transition-colors">Unpick</button> <button class="text-[10px] px-1.5 py-0.5 rounded text-surface-400
										hover:text-health-warn hover:bg-health-warn/10 transition-colors" title="Park card"></button> <button class="text-[10px] px-1.5 py-0.5 rounded text-surface-400
										hover:text-health-err hover:bg-health-err/10 transition-colors" title="Block card"></button> <button class="text-[10px] px-2 py-0.5 rounded bg-health-ok/15 text-health-ok
										hover:bg-health-ok/25 transition-colors">Finish</button></div></div>`), Qv = /* @__PURE__ */ p('<div class="rounded border border-surface-600/50 bg-surface-800/30 p-2.5 opacity-70"><div class="flex items-start gap-2 mb-1"><span class="text-[10px] text-health-ok"></span> <span class="text-xs text-surface-300 flex-1 leading-tight"> </span> <span> </span></div> <div class="text-[9px] text-surface-500 ml-4"> </div></div>'), Xv = /* @__PURE__ */ p('<p class="text-[10px] text-health-warn/80 mb-2 italic leading-relaxed"> </p>'), Zv = /* @__PURE__ */ p(`<div class="rounded border border-health-warn/20 bg-surface-800/60 p-2.5 group"><div class="flex items-start gap-2 mb-1"><span class="text-xs font-medium text-surface-200 flex-1 leading-tight"> </span> <span> </span></div> <!> <div class="flex items-center justify-between"><span class="text-[9px] text-surface-500"> </span> <button class="text-[10px] px-2 py-0.5 rounded text-health-warn
											hover:bg-health-warn/15 transition-colors
											opacity-0 group-hover:opacity-100">Unpark</button></div></div>`), ef = /* @__PURE__ */ p('<p class="text-[10px] text-health-err/80 mb-2 italic leading-relaxed"> </p>'), tf = /* @__PURE__ */ p(`<div class="rounded border border-health-err/20 bg-surface-800/60 p-2.5 group"><div class="flex items-start gap-2 mb-1"><span class="text-xs font-medium text-surface-200 flex-1 leading-tight"> </span> <span> </span></div> <!> <div class="flex items-center justify-between"><span class="text-[9px] text-surface-500"> </span> <button class="text-[10px] px-2 py-0.5 rounded text-health-err
											hover:bg-health-err/15 transition-colors
											opacity-0 group-hover:opacity-100">Unblock</button></div></div>`), rf = /* @__PURE__ */ p('<div class="grid grid-cols-2 gap-3"><div class="rounded-lg border border-health-warn/30 bg-health-warn/5"><div class="px-3 py-2 border-b border-health-warn/20 flex items-center gap-2"><span class="text-[11px]"></span> <span class="text-[11px] font-semibold text-health-warn">Parked</span> <span class="text-[10px] text-surface-500 ml-auto"> </span></div> <div class="p-2 space-y-2"></div></div> <div class="rounded-lg border border-health-err/30 bg-health-err/5"><div class="px-3 py-2 border-b border-health-err/20 flex items-center gap-2"><span class="text-[11px]"></span> <span class="text-[11px] font-semibold text-health-err">Blocked</span> <span class="text-[10px] text-surface-500 ml-auto"> </span></div> <div class="p-2 space-y-2"></div></div></div>'), sf = /* @__PURE__ */ p('<div class="grid grid-cols-3 gap-3 min-h-[300px]"><div class="rounded-lg border border-surface-600 bg-surface-800/30"><div class="px-3 py-2 border-b border-surface-600 flex items-center gap-2"><span class="w-2 h-2 rounded-full bg-hecate-400"></span> <span class="text-[11px] font-semibold text-surface-200">Posted</span> <span class="text-[10px] text-surface-500 ml-auto"> </span></div> <div class="p-2 space-y-2"></div></div> <div class="rounded-lg border border-surface-600 bg-surface-800/30"><div class="px-3 py-2 border-b border-surface-600 flex items-center gap-2"><span class="w-2 h-2 rounded-full bg-phase-crafting"></span> <span class="text-[11px] font-semibold text-surface-200">Picked</span> <span class="text-[10px] text-surface-500 ml-auto"> </span></div> <div class="p-2 space-y-2"></div></div> <div class="rounded-lg border border-surface-600 bg-surface-800/30"><div class="px-3 py-2 border-b border-surface-600 flex items-center gap-2"><span class="w-2 h-2 rounded-full bg-health-ok"></span> <span class="text-[11px] font-semibold text-surface-200">Finished</span> <span class="text-[10px] text-surface-500 ml-auto"> </span></div> <div class="p-2 space-y-2"></div></div></div> <!>', 1), af = /* @__PURE__ */ p('<div class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center" role="dialog" aria-modal="true"><div class="bg-surface-800 border border-surface-600 rounded-xl p-5 w-96 space-y-3"><h4 class="text-sm font-semibold text-surface-100"> </h4> <p class="text-[11px] text-surface-400"> <span class="text-surface-200 font-medium"> </span></p> <div><label for="modal-reason" class="text-[10px] text-surface-400 block mb-1">Reason</label> <textarea id="modal-reason" rows="3"></textarea></div> <div class="flex gap-2 justify-end"><button class="px-3 py-1.5 rounded text-xs text-surface-400 hover:text-surface-100">Cancel</button> <button> </button></div></div></div>'), nf = /* @__PURE__ */ p(`<div class="p-4 space-y-4"><div class="flex items-center justify-between"><div><h3 class="text-sm font-semibold text-surface-100">Kanban</h3> <p class="text-[11px] text-surface-400 mt-0.5">Work cards for <span class="text-surface-200"> </span></p></div> <div class="flex items-center gap-2"><!> <button class="text-[11px] px-3 py-1 rounded bg-hecate-600/20 text-hecate-300
					hover:bg-hecate-600/30 transition-colors">+ Post Card</button></div></div> <!> <!> <!> <!></div>`);
function vo(e, t) {
  Ct(t, !0);
  const r = () => $e(Hr, "$selectedDivision", b), a = () => $e(Tr, "$kanbanCards", b), o = () => $e(Pv, "$cardCounts", b), c = () => $e(At, "$kanbanError", b), d = () => $e(Ba, "$kanbanLoading", b), u = () => $e(Cv, "$postedCards", b), v = () => $e(Sv, "$pickedCards", b), h = () => $e(Ev, "$finishedCards", b), w = () => $e(Av, "$parkedCards", b), g = () => $e(Dv, "$blockedCards", b), [b, T] = qt();
  let S = /* @__PURE__ */ se(null);
  Pt(() => {
    const j = r();
    j && j.division_id !== s(S) && (x(S, j.division_id, !0), Mr(j.division_id));
  });
  let W = /* @__PURE__ */ se(!1), $ = /* @__PURE__ */ se(""), R = /* @__PURE__ */ se(""), K = /* @__PURE__ */ se("cmd_desk"), ge = /* @__PURE__ */ se(null), ie = /* @__PURE__ */ se("unpick"), ae = /* @__PURE__ */ se("");
  async function Z() {
    if (!r() || !s($).trim()) return;
    await Tv(r().division_id, {
      title: s($).trim(),
      description: s(R).trim() || void 0,
      card_type: s(K),
      posted_by: "hecate-web"
    }) && (x($, ""), x(R, ""), x(W, !1));
  }
  async function De(j) {
    r() && await Rv(r().division_id, j.card_id);
  }
  async function Ve(j) {
    r() && await Mv(r().division_id, j.card_id);
  }
  function Ge(j, B) {
    x(ge, j, !0), x(ie, B, !0), x(ae, "");
  }
  async function ve() {
    if (!r() || !s(ge) || !s(ae).trim()) return;
    const j = r().division_id, B = s(ge).card_id;
    let xe = !1;
    s(ie) === "unpick" ? xe = await Iv(j, B, s(ae).trim()) : s(ie) === "park" ? xe = await Lv(j, B, s(ae).trim()) : s(ie) === "block" && (xe = await Ov(j, B, s(ae).trim())), xe && (x(ge, null), x(ae, ""));
  }
  async function N(j) {
    r() && await Nv(r().division_id, j.card_id);
  }
  async function U(j) {
    r() && await Fv(r().division_id, j.card_id);
  }
  function pe(j) {
    switch (j) {
      case "cmd_desk":
        return "CMD";
      case "prj_desk":
        return "PRJ";
      case "qry_desk":
        return "QRY";
      default:
        return j;
    }
  }
  function Ne(j) {
    switch (j) {
      case "cmd_desk":
        return "bg-es-command/20 text-es-command";
      case "prj_desk":
        return "bg-phase-crafting/20 text-phase-crafting";
      case "qry_desk":
        return "bg-hecate-600/20 text-hecate-400";
      default:
        return "bg-surface-600 text-surface-300";
    }
  }
  function ne(j) {
    return j ? new Date(j).toLocaleDateString(void 0, {
      month: "short",
      day: "numeric",
      hour: "2-digit",
      minute: "2-digit"
    }) : "";
  }
  const k = {
    unpick: { title: "Unpick Card", verb: "Unpick", color: "health-warn" },
    park: { title: "Park Card", verb: "Park", color: "health-warn" },
    block: { title: "Block Card", verb: "Block", color: "health-err" }
  };
  var F = nf(), Fe = i(F), ze = i(Fe), Xe = l(i(ze), 2), Ee = l(i(Xe)), Ce = i(Ee, !0);
  n(Ee), n(Xe), n(ze);
  var me = l(ze, 2), fe = i(me);
  {
    var de = (j) => {
      var B = Bv(), xe = i(B), je = i(xe);
      n(xe);
      var Me = l(xe, 2), et = i(Me);
      n(Me);
      var qe = l(Me, 2), Ke = i(qe);
      n(qe);
      var We = l(qe, 2);
      {
        var tt = (L) => {
          var D = jv(), oe = i(D);
          n(D), C(() => _(oe, `${o().parked ?? ""} parked`)), f(L, D);
        };
        P(We, (L) => {
          o().parked > 0 && L(tt);
        });
      }
      var st = l(We, 2);
      {
        var Je = (L) => {
          var D = Vv(), oe = i(D);
          n(D), C(() => _(oe, `${o().blocked ?? ""} blocked`)), f(L, D);
        };
        P(st, (L) => {
          o().blocked > 0 && L(Je);
        });
      }
      n(B), C(() => {
        _(je, `${o().posted ?? ""} posted`), _(et, `${o().picked ?? ""} picked`), _(Ke, `${o().finished ?? ""} done`);
      }), f(j, B);
    };
    P(fe, (j) => {
      a().length > 0 && j(de);
    });
  }
  var H = l(fe, 2);
  H.__click = () => x(W, !s(W)), n(me), n(Fe);
  var M = l(Fe, 2);
  {
    var I = (j) => {
      var B = qv(), xe = i(B, !0);
      n(B), C(() => _(xe, c())), f(j, B);
    };
    P(M, (j) => {
      c() && j(I);
    });
  }
  var z = l(M, 2);
  {
    var Oe = (j) => {
      var B = Hv(), xe = l(i(B), 2), je = i(xe), Me = l(i(je), 2);
      bt(Me), n(je);
      var et = l(je, 2), qe = l(i(et), 2), Ke = i(qe);
      Ke.value = Ke.__value = "cmd_desk";
      var We = l(Ke);
      We.value = We.__value = "prj_desk";
      var tt = l(We);
      tt.value = tt.__value = "qry_desk", n(qe), n(et), n(xe);
      var st = l(xe, 2), Je = l(i(st), 2);
      bt(Je), n(st);
      var L = l(st, 2), D = i(L);
      D.__click = Z;
      var oe = l(D, 2);
      oe.__click = () => x(W, !1), n(L), n(B), C(
        (Se, G) => {
          D.disabled = Se, Re(D, 1, `px-3 py-1.5 rounded text-xs transition-colors
						${G ?? ""}`);
        },
        [
          () => !s($).trim(),
          () => s($).trim() ? "bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30" : "bg-surface-700 text-surface-500 cursor-not-allowed"
        ]
      ), xt(Me, () => s($), (Se) => x($, Se)), Is(qe, () => s(K), (Se) => x(K, Se)), xt(Je, () => s(R), (Se) => x(R, Se)), f(j, B);
    };
    P(z, (j) => {
      s(W) && j(Oe);
    });
  }
  var Ye = l(z, 2);
  {
    var A = (j) => {
      var B = Gv();
      f(j, B);
    }, E = (j) => {
      var B = Wv(), xe = i(B);
      xe.textContent = "☐", Et(4), n(B), f(j, B);
    }, te = (j) => {
      var B = sf(), xe = ut(B), je = i(xe), Me = i(je), et = l(i(Me), 4), qe = i(et, !0);
      n(et), n(Me);
      var Ke = l(Me, 2);
      He(Ke, 5, u, (m) => m.card_id, (m, y) => {
        var le = zv(), O = i(le), re = i(O), he = i(re, !0);
        n(re);
        var we = l(re, 2), J = i(we, !0);
        n(we), n(O);
        var V = l(O, 2);
        {
          var Y = (be) => {
            var Pe = Uv(), Be = i(Pe, !0);
            n(Pe), C(() => _(Be, s(y).description)), f(be, Pe);
          };
          P(V, (be) => {
            s(y).description && be(Y);
          });
        }
        var ye = l(V, 2), _e = i(ye), ce = i(_e, !0);
        n(_e);
        var Q = l(_e, 2), X = i(Q);
        X.__click = () => De(s(y));
        var q = l(X, 2);
        q.__click = () => Ge(s(y), "park"), q.textContent = "⏸";
        var ue = l(q, 2);
        ue.__click = () => Ge(s(y), "block"), ue.textContent = "⛔", n(Q), n(ye), n(le), C(
          (be, Pe, Be) => {
            _(he, s(y).title), Re(we, 1, `text-[9px] px-1.5 py-0.5 rounded ${be ?? ""} shrink-0`), _(J, Pe), _(ce, Be);
          },
          [
            () => Ne(s(y).card_type),
            () => pe(s(y).card_type),
            () => ne(s(y).posted_at)
          ]
        ), f(m, le);
      }), n(Ke), n(je);
      var We = l(je, 2), tt = i(We), st = l(i(tt), 4), Je = i(st, !0);
      n(st), n(tt);
      var L = l(tt, 2);
      He(L, 5, v, (m) => m.card_id, (m, y) => {
        var le = Jv(), O = i(le), re = i(O), he = i(re, !0);
        n(re);
        var we = l(re, 2), J = i(we, !0);
        n(we), n(O);
        var V = l(O, 2);
        {
          var Y = (be) => {
            var Pe = Yv(), Be = i(Pe, !0);
            n(Pe), C(() => _(Be, s(y).description)), f(be, Pe);
          };
          P(V, (be) => {
            s(y).description && be(Y);
          });
        }
        var ye = l(V, 2);
        {
          var _e = (be) => {
            var Pe = Kv(), Be = i(Pe);
            n(Pe), C(() => _(Be, `Picked by ${s(y).picked_by ?? ""}`)), f(be, Pe);
          };
          P(ye, (be) => {
            s(y).picked_by && be(_e);
          });
        }
        var ce = l(ye, 2), Q = i(ce);
        Q.__click = () => Ge(s(y), "unpick");
        var X = l(Q, 2);
        X.__click = () => Ge(s(y), "park"), X.textContent = "⏸";
        var q = l(X, 2);
        q.__click = () => Ge(s(y), "block"), q.textContent = "⛔";
        var ue = l(q, 2);
        ue.__click = () => Ve(s(y)), n(ce), n(le), C(
          (be, Pe) => {
            _(he, s(y).title), Re(we, 1, `text-[9px] px-1.5 py-0.5 rounded ${be ?? ""} shrink-0`), _(J, Pe);
          },
          [
            () => Ne(s(y).card_type),
            () => pe(s(y).card_type)
          ]
        ), f(m, le);
      }), n(L), n(We);
      var D = l(We, 2), oe = i(D), Se = l(i(oe), 4), G = i(Se, !0);
      n(Se), n(oe);
      var ee = l(oe, 2);
      He(ee, 5, h, (m) => m.card_id, (m, y) => {
        var le = Qv(), O = i(le), re = i(O);
        re.textContent = "✓";
        var he = l(re, 2), we = i(he, !0);
        n(he);
        var J = l(he, 2), V = i(J, !0);
        n(J), n(O);
        var Y = l(O, 2), ye = i(Y, !0);
        n(Y), n(le), C(
          (_e, ce, Q) => {
            _(we, s(y).title), Re(J, 1, `text-[9px] px-1.5 py-0.5 rounded ${_e ?? ""} shrink-0`), _(V, ce), _(ye, Q);
          },
          [
            () => Ne(s(y).card_type),
            () => pe(s(y).card_type),
            () => ne(s(y).finished_at)
          ]
        ), f(m, le);
      }), n(ee), n(D), n(xe);
      var Ae = l(xe, 2);
      {
        var Le = (m) => {
          var y = rf(), le = i(y), O = i(le), re = i(O);
          re.textContent = "⏸";
          var he = l(re, 4), we = i(he, !0);
          n(he), n(O);
          var J = l(O, 2);
          He(J, 5, w, (X) => X.card_id, (X, q) => {
            var ue = Zv(), be = i(ue), Pe = i(be), Be = i(Pe, !0);
            n(Pe);
            var Qe = l(Pe, 2), rt = i(Qe, !0);
            n(Qe), n(be);
            var at = l(be, 2);
            {
              var nt = (kt) => {
                var jt = Xv(), wr = i(jt, !0);
                n(jt), C(() => _(wr, s(q).park_reason)), f(kt, jt);
              };
              P(at, (kt) => {
                s(q).park_reason && kt(nt);
              });
            }
            var gt = l(at, 2), ht = i(gt), Yt = i(ht);
            n(ht);
            var ir = l(ht, 2);
            ir.__click = () => N(s(q)), n(gt), n(ue), C(
              (kt, jt, wr) => {
                _(Be, s(q).title), Re(Qe, 1, `text-[9px] px-1.5 py-0.5 rounded ${kt ?? ""} shrink-0`), _(rt, jt), _(Yt, `${s(q).parked_by ? `by ${s(q).parked_by}` : ""}
										${wr ?? ""}`);
              },
              [
                () => Ne(s(q).card_type),
                () => pe(s(q).card_type),
                () => ne(s(q).parked_at)
              ]
            ), f(X, ue);
          }), n(J), n(le);
          var V = l(le, 2), Y = i(V), ye = i(Y);
          ye.textContent = "⛔";
          var _e = l(ye, 4), ce = i(_e, !0);
          n(_e), n(Y);
          var Q = l(Y, 2);
          He(Q, 5, g, (X) => X.card_id, (X, q) => {
            var ue = tf(), be = i(ue), Pe = i(be), Be = i(Pe, !0);
            n(Pe);
            var Qe = l(Pe, 2), rt = i(Qe, !0);
            n(Qe), n(be);
            var at = l(be, 2);
            {
              var nt = (kt) => {
                var jt = ef(), wr = i(jt, !0);
                n(jt), C(() => _(wr, s(q).block_reason)), f(kt, jt);
              };
              P(at, (kt) => {
                s(q).block_reason && kt(nt);
              });
            }
            var gt = l(at, 2), ht = i(gt), Yt = i(ht);
            n(ht);
            var ir = l(ht, 2);
            ir.__click = () => U(s(q)), n(gt), n(ue), C(
              (kt, jt, wr) => {
                _(Be, s(q).title), Re(Qe, 1, `text-[9px] px-1.5 py-0.5 rounded ${kt ?? ""} shrink-0`), _(rt, jt), _(Yt, `${s(q).blocked_by ? `by ${s(q).blocked_by}` : ""}
										${wr ?? ""}`);
              },
              [
                () => Ne(s(q).card_type),
                () => pe(s(q).card_type),
                () => ne(s(q).blocked_at)
              ]
            ), f(X, ue);
          }), n(Q), n(V), n(y), C(() => {
            _(we, w().length), _(ce, g().length);
          }), f(m, y);
        };
        P(Ae, (m) => {
          (w().length > 0 || g().length > 0) && m(Le);
        });
      }
      C(() => {
        _(qe, u().length), _(Je, v().length), _(G, h().length);
      }), f(j, B);
    };
    P(Ye, (j) => {
      d() ? j(A) : a().length === 0 && !s(W) ? j(E, 1) : j(te, !1);
    });
  }
  var ke = l(Ye, 2);
  {
    var Ie = (j) => {
      const B = /* @__PURE__ */ Te(() => k[s(ie)]);
      var xe = af(), je = i(xe), Me = i(je), et = i(Me, !0);
      n(Me);
      var qe = l(Me, 2), Ke = i(qe), We = l(Ke), tt = i(We, !0);
      n(We), n(qe);
      var st = l(qe, 2), Je = l(i(st), 2);
      js(Je), n(st);
      var L = l(st, 2), D = i(L);
      D.__click = () => x(ge, null);
      var oe = l(D, 2);
      oe.__click = ve;
      var Se = i(oe, !0);
      n(oe), n(L), n(je), n(xe), C(
        (G, ee) => {
          _(et, s(B).title), _(Ke, `${s(B).verb ?? ""}ing `), _(tt, s(ge).title), zt(Je, "placeholder", `Why is this card being ${s(ie) === "unpick" ? "unpicked" : s(ie) === "park" ? "parked" : "blocked"}?`), Re(Je, 1, `w-full bg-surface-700 border border-surface-600 rounded px-2.5 py-1.5
							text-xs text-surface-100 placeholder-surface-400
							focus:outline-none focus:border-${s(B).color ?? ""}/50 resize-none`), oe.disabled = G, Re(oe, 1, `px-3 py-1.5 rounded text-xs transition-colors
							${ee ?? ""}`), _(Se, s(B).verb);
        },
        [
          () => !s(ae).trim(),
          () => s(ae).trim() ? `bg-${s(B).color}/20 text-${s(B).color} hover:bg-${s(B).color}/30` : "bg-surface-700 text-surface-500 cursor-not-allowed"
        ]
      ), xt(Je, () => s(ae), (G) => x(ae, G)), f(j, xe);
    };
    P(ke, (j) => {
      s(ge) && j(Ie);
    });
  }
  n(F), C(() => _(Ce, r()?.context_name)), f(e, F), St(), T();
}
Ot(["click"]);
Dt(vo, {}, [], [], { mode: "open" });
const fo = Ze(null);
async function of(e, t) {
  try {
    return await Ue().post(`/craftings/${e}/generate-module`, t), !0;
  } catch (r) {
    const a = r;
    return fo.set(a.message || "Failed to generate module"), !1;
  }
}
async function lf(e, t) {
  try {
    return await Ue().post(`/craftings/${e}/deliver-release`, { version: t }), !0;
  } catch (r) {
    const a = r;
    return fo.set(a.message || "Failed to deliver release"), !1;
  }
}
var cf = /* @__PURE__ */ p(`<div class="flex gap-2 items-end mb-4 p-3 rounded bg-surface-700/30"><div class="flex-1"><label for="mod-name" class="text-[10px] text-surface-400 block mb-1">Module Name</label> <input id="mod-name" placeholder="e.g., register_user_v1" class="w-full bg-surface-700 border border-surface-600 rounded
								px-2.5 py-1.5 text-xs text-surface-100 placeholder-surface-400
								focus:outline-none focus:border-hecate-500/50"/></div> <div class="flex-1"><label for="mod-template" class="text-[10px] text-surface-400 block mb-1">Template (optional)</label> <input id="mod-template" placeholder="e.g., command, event, handler" class="w-full bg-surface-700 border border-surface-600 rounded
								px-2.5 py-1.5 text-xs text-surface-100 placeholder-surface-400
								focus:outline-none focus:border-hecate-500/50"/></div> <button class="px-3 py-1.5 rounded text-xs bg-hecate-600/20 text-hecate-300
							hover:bg-hecate-600/30 transition-colors disabled:opacity-50">Generate</button> <button class="px-3 py-1.5 rounded text-xs text-surface-400 hover:text-surface-100">Cancel</button></div>`), df = /* @__PURE__ */ p(
  `<div class="rounded-lg border border-surface-600 bg-surface-800/50 p-4"><div class="flex items-center justify-between mb-3"><h4 class="text-xs font-semibold text-surface-100">Code Generation</h4> <button class="text-[10px] px-2 py-0.5 rounded bg-hecate-600/10 text-hecate-300
						hover:bg-hecate-600/20 transition-colors">+ Generate Module</button></div> <!> <p class="text-[10px] text-surface-400">Generate Erlang modules from templates based on planned desks and design
				artifacts.</p></div> <div><h4 class="text-xs font-semibold text-surface-100 mb-3">Implementation Tasks</h4> <div class="grid grid-cols-1 md:grid-cols-2 gap-3"><!> <!> <!> <!> <!> <!></div></div>`,
  1
), uf = /* @__PURE__ */ p(`<div class="flex gap-2 items-end mb-4 p-3 rounded bg-surface-700/30"><div class="flex-1"><label for="rel-version" class="text-[10px] text-surface-400 block mb-1">Version</label> <input id="rel-version" placeholder="e.g., 0.1.0" class="w-full bg-surface-700 border border-surface-600 rounded
								px-2.5 py-1.5 text-xs text-surface-100 placeholder-surface-400
								focus:outline-none focus:border-hecate-500/50"/></div> <button class="px-3 py-1.5 rounded text-xs bg-hecate-600/20 text-hecate-300
							hover:bg-hecate-600/30 transition-colors disabled:opacity-50">Deliver</button> <button class="px-3 py-1.5 rounded text-xs text-surface-400 hover:text-surface-100">Cancel</button></div>`), vf = /* @__PURE__ */ p(
  `<div class="rounded-lg border border-surface-600 bg-surface-800/50 p-4"><div class="flex items-center justify-between mb-3"><h4 class="text-xs font-semibold text-surface-100">Releases</h4> <button class="text-[10px] px-2 py-0.5 rounded bg-hecate-600/10 text-hecate-300
						hover:bg-hecate-600/20 transition-colors">+ Deliver Release</button></div> <!> <p class="text-[10px] text-surface-400">Deliver through GitOps: version bump, git tag, CI/CD builds and deploys.</p></div> <div><h4 class="text-xs font-semibold text-surface-100 mb-3">Delivery Tasks</h4> <div class="grid grid-cols-1 md:grid-cols-2 gap-3"><!> <!> <!> <!></div></div>`,
  1
), ff = /* @__PURE__ */ p('<div class="p-4 space-y-6"><div class="flex items-center justify-between"><div><h3 class="text-sm font-semibold text-surface-100">Crafting</h3> <p class="text-[11px] text-surface-400 mt-0.5">Generate code, run tests, and deliver releases for <span class="text-surface-200"> </span></p></div> <div class="flex items-center gap-1 bg-surface-700/50 rounded-lg p-0.5"><button>Implementation</button> <button>Delivery</button></div></div> <!></div>');
function po(e, t) {
  Ct(t, !0);
  const r = () => $e(Hr, "$selectedDivision", o), a = () => $e(wt, "$isLoading", o), [o, c] = qt();
  let d = /* @__PURE__ */ se("implement"), u = /* @__PURE__ */ se(!1), v = /* @__PURE__ */ se(""), h = /* @__PURE__ */ se(""), w = /* @__PURE__ */ se(!1), g = /* @__PURE__ */ se("");
  async function b() {
    if (!r() || !s(v).trim()) return;
    await of(r().division_id, {
      module_name: s(v).trim(),
      template: s(h).trim() || void 0
    }) && (x(v, ""), x(h, ""), x(u, !1));
  }
  async function T() {
    if (!r() || !s(g).trim()) return;
    await lf(r().division_id, s(g).trim()) && (x(g, ""), x(w, !1));
  }
  var S = ff(), W = i(S), $ = i(W), R = l(i($), 2), K = l(i(R)), ge = i(K, !0);
  n(K), n(R), n($);
  var ie = l($, 2), ae = i(ie);
  ae.__click = () => x(d, "implement");
  var Z = l(ae, 2);
  Z.__click = () => x(d, "deliver"), n(ie), n(W);
  var De = l(W, 2);
  {
    var Ve = (ve) => {
      var N = df(), U = ut(N), pe = i(U), Ne = l(i(pe), 2);
      Ne.__click = () => x(u, !s(u)), n(pe);
      var ne = l(pe, 2);
      {
        var k = (de) => {
          var H = cf(), M = i(H), I = l(i(M), 2);
          bt(I), n(M);
          var z = l(M, 2), Oe = l(i(z), 2);
          bt(Oe), n(z);
          var Ye = l(z, 2);
          Ye.__click = b;
          var A = l(Ye, 2);
          A.__click = () => x(u, !1), n(H), C((E) => Ye.disabled = E, [() => !s(v).trim() || a()]), xt(I, () => s(v), (E) => x(v, E)), xt(Oe, () => s(h), (E) => x(h, E)), f(de, H);
        };
        P(ne, (de) => {
          s(u) && de(k);
        });
      }
      Et(2), n(U);
      var F = l(U, 2), Fe = l(i(F), 2), ze = i(Fe);
      {
        let de = /* @__PURE__ */ Te(() => `Help me implement the walking skeleton for the "${r()?.context_name}" division. We need initiate_{aggregate} and archive_{aggregate} desks first. Generate the Erlang module structure for each.`);
        _t(ze, {
          title: "Walking Skeleton",
          description: "Generate initiate + archive desks first, establishing the aggregate lifecycle foundation",
          icon: "⚲",
          get aiContext() {
            return s(de);
          }
        });
      }
      var Xe = l(ze, 2);
      {
        let de = /* @__PURE__ */ Te(() => `Help me generate Erlang command modules for the "${r()?.context_name}" division. Each command needs: module, record, to_map/1, from_map/1. Use the evoq command pattern.`);
        _t(Xe, {
          title: "Generate Commands",
          description: "Create command modules from the desk inventory with proper versioning",
          icon: "▶",
          get aiContext() {
            return s(de);
          }
        });
      }
      var Ee = l(Xe, 2);
      {
        let de = /* @__PURE__ */ Te(() => `Help me generate Erlang event modules for the "${r()?.context_name}" division. Each event needs: module, record, to_map/1, from_map/1. Follow the event naming convention: {subject}_{verb_past}_v{N}.`);
        _t(Ee, {
          title: "Generate Events",
          description: "Create event modules matching the designed domain events",
          icon: "◆",
          get aiContext() {
            return s(de);
          }
        });
      }
      var Ce = l(Ee, 2);
      {
        let de = /* @__PURE__ */ Te(() => `Help me write EUnit tests for the "${r()?.context_name}" division. Cover aggregate behavior (execute + apply), handler dispatch, and projection state updates.`);
        _t(Ce, {
          title: "Write Tests",
          description: "Generate EUnit test modules for aggregates, handlers, and projections",
          icon: "✓",
          get aiContext() {
            return s(de);
          }
        });
      }
      var me = l(Ce, 2);
      {
        let de = /* @__PURE__ */ Te(() => `Help me analyze test results for the "${r()?.context_name}" division. What patterns should I look for? How do I ensure adequate coverage of the aggregate lifecycle?`);
        _t(me, {
          title: "Run Test Suite",
          description: "Execute all tests and review results for quality gates",
          icon: "▷",
          get aiContext() {
            return s(de);
          }
        });
      }
      var fe = l(me, 2);
      {
        let de = /* @__PURE__ */ Te(() => `Help me define acceptance criteria for the "${r()?.context_name}" division. What must be true before we can say this division is implemented correctly?`);
        _t(fe, {
          title: "Acceptance Criteria",
          description: "Validate that implementation meets the design specifications",
          icon: "☑",
          get aiContext() {
            return s(de);
          }
        });
      }
      n(Fe), n(F), f(ve, N);
    }, Ge = (ve) => {
      var N = vf(), U = ut(N), pe = i(U), Ne = l(i(pe), 2);
      Ne.__click = () => x(w, !s(w)), n(pe);
      var ne = l(pe, 2);
      {
        var k = (me) => {
          var fe = uf(), de = i(fe), H = l(i(de), 2);
          bt(H), n(de);
          var M = l(de, 2);
          M.__click = T;
          var I = l(M, 2);
          I.__click = () => x(w, !1), n(fe), C((z) => M.disabled = z, [() => !s(g).trim() || a()]), xt(H, () => s(g), (z) => x(g, z)), f(me, fe);
        };
        P(ne, (me) => {
          s(w) && me(k);
        });
      }
      Et(2), n(U);
      var F = l(U, 2), Fe = l(i(F), 2), ze = i(Fe);
      {
        let me = /* @__PURE__ */ Te(() => `Help me prepare a release for the "${r()?.context_name}" division. Walk me through the GitOps flow: version bump, CHANGELOG update, git tag, and CI/CD pipeline.`);
        _t(ze, {
          title: "Release Management",
          description: "Prepare release: version bump, changelog, git tag, push for CI/CD",
          icon: "↑",
          get aiContext() {
            return s(me);
          }
        });
      }
      var Xe = l(ze, 2);
      {
        let me = /* @__PURE__ */ Te(() => `Help me plan a staged rollout for the "${r()?.context_name}" division. How should we structure canary deployments with health gates on the beam cluster?`);
        _t(Xe, {
          title: "Staged Rollout",
          description: "Plan a staged rollout with canary deployment and health gates",
          icon: "▤",
          get aiContext() {
            return s(me);
          }
        });
      }
      var Ee = l(Xe, 2);
      {
        let me = /* @__PURE__ */ Te(() => `Help me set up health monitoring for the "${r()?.context_name}" division. What health checks should we configure? What SLA thresholds make sense?`);
        _t(Ee, {
          title: "Health Monitoring",
          description: "Configure health checks and SLA thresholds",
          icon: "♥",
          get aiContext() {
            return s(me);
          }
        });
      }
      var Ce = l(Ee, 2);
      {
        let me = /* @__PURE__ */ Te(() => `Help me set up observability for the "${r()?.context_name}" division. What should we log? What metrics should we track? How do we set up distributed tracing on the beam cluster?`);
        _t(Ce, {
          title: "Observability",
          description: "Set up logging, metrics, and tracing for production visibility",
          icon: "◎",
          get aiContext() {
            return s(me);
          }
        });
      }
      n(Fe), n(F), f(ve, N);
    };
    P(De, (ve) => {
      s(d) === "implement" ? ve(Ve) : ve(Ge, !1);
    });
  }
  n(S), C(() => {
    _(ge, r()?.context_name), Re(ae, 1, `px-3 py-1 rounded text-[11px] transition-colors
					${s(d) === "implement" ? "bg-surface-600 text-surface-100" : "text-surface-400 hover:text-surface-200"}`), Re(Z, 1, `px-3 py-1 rounded text-[11px] transition-colors
					${s(d) === "deliver" ? "bg-surface-600 text-surface-100" : "text-surface-400 hover:text-surface-200"}`);
  }), f(e, S), St(), c();
}
Ot(["click"]);
Dt(po, {}, [], [], { mode: "open" });
const Br = {
  visionary: { label: "Visionary", tier: "creative", icon: "◇" },
  explorer: { label: "Explorer", tier: "creative", icon: "🔍" },
  stormer: { label: "Stormer", tier: "creative", icon: "⚡" },
  reviewer: { label: "Reviewer", tier: "creative", icon: "🔎" },
  architect: { label: "Architect", tier: "mechanical", icon: "△" },
  erlang_coder: { label: "Erl Coder", tier: "mechanical", icon: "⚙" },
  svelte_coder: { label: "Svlt Coder", tier: "mechanical", icon: "⬡" },
  sql_coder: { label: "SQL Coder", tier: "mechanical", icon: "▣" },
  tester: { label: "Tester", tier: "mechanical", icon: "✓" },
  delivery: { label: "Delivery", tier: "mechanical", icon: "→" },
  coordinator: { label: "Coordinator", tier: "always_on", icon: "💬" },
  mentor: { label: "Mentor", tier: "always_on", icon: "💬" }
}, pf = [
  "visionary",
  "explorer",
  "stormer",
  "reviewer",
  "architect",
  "erlang_coder",
  "svelte_coder",
  "sql_coder",
  "tester",
  "delivery",
  "coordinator",
  "mentor"
], xf = Ze([]), Bs = Ze([]), Xt = Ze(null), qa = Ze(!1), zs = Ze(null), Ha = Ze([]), mn = Ze([]), hf = Rt(
  Bs,
  (e) => e.filter((t) => Br[t.role]?.tier === "creative")
), _f = Rt(
  Bs,
  (e) => e.filter((t) => Br[t.role]?.tier === "mechanical")
), gf = Rt(
  Bs,
  (e) => e.filter((t) => Br[t.role]?.tier === "always_on")
);
Rt(mn, (e) => e.length > 0);
async function _s(e) {
  try {
    qa.set(!0), Xt.set(null);
    const r = await Ue().get(
      `/ventures/${e}/agents/sessions`
    );
    xf.set(r.sessions ?? []), Cf(r.sessions ?? []), Sf(r.sessions ?? []);
  } catch (t) {
    const r = t;
    Xt.set(r.message || "Failed to fetch agent sessions");
  } finally {
    qa.set(!1);
  }
}
async function bf(e, t) {
  try {
    Xt.set(null);
    const a = await Ue().get(
      `/ventures/${e}/agents/sessions/${t}`
    );
    zs.set(a.session);
  } catch (r) {
    const a = r;
    Xt.set(a.message || "Failed to fetch session detail");
  }
}
async function mf(e, t) {
  try {
    const a = await Ue().get(
      `/ventures/${e}/agents/sessions/${t}/turns`
    );
    Ha.set(a.turns ?? []);
  } catch {
    Ha.set([]);
  }
}
async function yf(e, t, r) {
  try {
    Xt.set(null);
    const a = Ue(), o = {};
    return await a.post(`/ventures/${e}/agents/${t}/initiate`, o), await _s(e), !0;
  } catch (a) {
    const o = a;
    return Xt.set(o.message || `Failed to initiate ${t}`), !1;
  }
}
async function wf(e, t, r) {
  try {
    return Xt.set(null), await Ue().post(`/ventures/${e}/agents/${t}/gate/pass`, { session_id: r }), await _s(e), !0;
  } catch (a) {
    const o = a;
    return Xt.set(o.message || "Failed to pass gate"), !1;
  }
}
async function kf(e, t, r, a) {
  try {
    return Xt.set(null), await Ue().post(`/ventures/${e}/agents/${t}/gate/reject`, {
      session_id: r,
      reason: a
    }), await _s(e), !0;
  } catch (o) {
    const c = o;
    return Xt.set(c.message || "Failed to reject gate"), !1;
  }
}
async function $f(e, t) {
  try {
    return Xt.set(null), await Ue().post(`/ventures/${e}/agents/sessions/${t}/archive`, {}), await _s(e), !0;
  } catch (r) {
    const a = r;
    return Xt.set(a.message || "Failed to archive session"), !1;
  }
}
function Cf(e) {
  const t = pf.map((r) => {
    const a = e.filter((v) => v.role === r), o = a.find((v) => v.status === "running" || v.status === "gate_pending"), c = a.filter((v) => v.status === "completed").sort((v, h) => (h.completed_at ?? 0) - (v.completed_at ?? 0))[0], d = a.find((v) => v.status === "failed");
    let u = "idle";
    return o?.status === "gate_pending" ? u = "gate_pending" : o?.status === "running" ? u = "running" : d && !c ? u = "failed" : c && (u = "completed"), {
      role: r,
      label: Br[r].label,
      tier: Br[r].tier,
      status: u,
      active_session: o ?? c ?? d ?? null,
      session_count: a.length
    };
  });
  Bs.set(t);
}
function Sf(e) {
  mn.set(e.filter((t) => t.status === "gate_pending"));
}
var Ef = /* @__PURE__ */ p('<div class="svelte-1ug3tqa"> </div>'), Af = /* @__PURE__ */ p('<div class="svelte-1ug3tqa"> </div>'), Df = /* @__PURE__ */ p('<div class="text-[9px] text-surface-400 space-y-0.5 svelte-1ug3tqa"><!> <!></div>'), Pf = /* @__PURE__ */ p('<div class="text-[9px] text-surface-500 italic svelte-1ug3tqa">No sessions</div>'), Tf = /* @__PURE__ */ p(`<span role="button" tabindex="0" class="absolute top-1.5 right-1.5 text-[8px] px-1.5 py-0.5 rounded cursor-pointer
				bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30
				opacity-0 group-hover:opacity-100 transition-opacity svelte-1ug3tqa">Run</span>`), Rf = /* @__PURE__ */ p('<button><div class="flex items-center gap-2 mb-1.5 svelte-1ug3tqa"><span></span> <span class="text-xs font-semibold text-surface-100 flex-1 truncate svelte-1ug3tqa"> </span> <span class="text-[9px] text-surface-500 svelte-1ug3tqa"> </span></div> <!> <!></button>');
const Mf = {
  hash: "svelte-1ug3tqa",
  code: `
	@keyframes svelte-1ug3tqa-pulse-subtle {
		0%, 100% { opacity: 1; }
		50% { opacity: 0.85; }
	}.animate-pulse-subtle {
		animation: svelte-1ug3tqa-pulse-subtle 2s ease-in-out infinite;}`
};
function Ys(e, t) {
  Ct(t, !0), Vi(e, Mf);
  let r = ft(t, "roleStatus", 7), a = ft(t, "onSelect", 7), o = ft(t, "onInitiate", 7);
  const c = /* @__PURE__ */ Te(() => Br[r().role]), d = {
    idle: { dot: "bg-surface-500", label: "Idle" },
    running: { dot: "bg-hecate-400 animate-pulse", label: "Active" },
    completed: { dot: "bg-health-ok", label: "Done" },
    failed: { dot: "bg-health-err", label: "Failed" },
    gate_pending: { dot: "bg-health-warn animate-pulse", label: "Gate" }
  }, u = /* @__PURE__ */ Te(() => d[r().status]);
  var v = {
    get roleStatus() {
      return r();
    },
    set roleStatus(ae) {
      r(ae), vt();
    },
    get onSelect() {
      return a();
    },
    set onSelect(ae) {
      a(ae), vt();
    },
    get onInitiate() {
      return o();
    },
    set onInitiate(ae) {
      o(ae), vt();
    }
  }, h = Rf();
  h.__click = () => a()(r().role);
  var w = i(h), g = i(w), b = l(g, 2), T = i(b, !0);
  n(b);
  var S = l(b, 2), W = i(S, !0);
  n(S), n(w);
  var $ = l(w, 2);
  {
    var R = (ae) => {
      const Z = /* @__PURE__ */ Te(() => r().active_session);
      var De = Df(), Ve = i(De);
      {
        var Ge = (U) => {
          var pe = Ef(), Ne = i(pe);
          n(pe), C((ne, k) => _(Ne, `${ne ?? ""} in / ${k ?? ""} out`), [
            () => s(Z).tokens_in.toLocaleString(),
            () => s(Z).tokens_out.toLocaleString()
          ]), f(U, pe);
        };
        P(Ve, (U) => {
          (s(Z).tokens_in > 0 || s(Z).tokens_out > 0) && U(Ge);
        });
      }
      var ve = l(Ve, 2);
      {
        var N = (U) => {
          var pe = Af(), Ne = i(pe);
          n(pe), C(() => _(Ne, `${r().session_count ?? ""} sessions`)), f(U, pe);
        };
        P(ve, (U) => {
          r().session_count > 1 && U(N);
        });
      }
      n(De), f(ae, De);
    }, K = (ae) => {
      var Z = Pf();
      f(ae, Z);
    };
    P($, (ae) => {
      r().active_session ? ae(R) : ae(K, !1);
    });
  }
  var ge = l($, 2);
  {
    var ie = (ae) => {
      var Z = Tf();
      Z.__click = (De) => {
        De.stopPropagation(), o()(r().role);
      }, Z.__keydown = (De) => {
        De.key === "Enter" && (De.stopPropagation(), o()(r().role));
      }, f(ae, Z);
    };
    P(ge, (ae) => {
      r().status === "idle" && ae(ie);
    });
  }
  return n(h), C(() => {
    Re(
      h,
      1,
      `group relative text-left p-3 rounded-lg border transition-all
		${r().status === "gate_pending" ? "border-health-warn/50 bg-health-warn/5 shadow-sm shadow-health-warn/10 animate-pulse-subtle" : r().status === "running" ? "border-hecate-500/40 bg-hecate-600/5" : "border-surface-600 bg-surface-800/60 hover:border-surface-500"}`,
      "svelte-1ug3tqa"
    ), Re(g, 1, `w-2 h-2 rounded-full ${s(u).dot ?? ""} shrink-0`, "svelte-1ug3tqa"), _(T, s(c).label), _(W, s(u).label);
  }), f(e, h), St(v);
}
Ot(["click", "keydown"]);
Dt(Ys, { roleStatus: {}, onSelect: {}, onInitiate: {} }, [], [], { mode: "open" });
var If = /* @__PURE__ */ p('<span class="text-[9px] text-surface-400 ml-auto"> </span>'), Lf = /* @__PURE__ */ p('<div class="rounded bg-surface-800 border border-surface-600 p-3 max-h-64 overflow-y-auto"><pre class="text-[10px] text-surface-200 whitespace-pre-wrap font-mono leading-relaxed"> </pre></div>'), Nf = /* @__PURE__ */ p('<div class="rounded bg-surface-800 border border-surface-600 p-3 max-h-64 overflow-y-auto"><pre class="text-[10px] text-surface-200 whitespace-pre-wrap font-mono leading-relaxed"> </pre></div>'), Of = /* @__PURE__ */ p("<span> </span>"), Ff = /* @__PURE__ */ p(`<div class="space-y-2"><textarea placeholder="Reason for rejecting..." rows="2" class="w-full bg-surface-700 border border-surface-600 rounded px-2.5 py-1.5
					text-xs text-surface-100 placeholder-surface-400
					focus:outline-none focus:border-health-err/50 resize-none"></textarea> <div class="flex gap-2"><button>Confirm Reject</button> <button class="px-3 py-1.5 rounded text-xs text-surface-400 hover:text-surface-100">Cancel</button></div></div>`), jf = /* @__PURE__ */ p(`<div class="flex gap-2"><button class="px-4 py-1.5 rounded text-xs font-medium
					bg-health-ok/20 text-health-ok hover:bg-health-ok/30 transition-colors"></button> <button class="px-4 py-1.5 rounded text-xs font-medium
					bg-health-err/20 text-health-err hover:bg-health-err/30 transition-colors"></button></div>`), Vf = /* @__PURE__ */ p('<div class="rounded-lg border-2 border-health-warn/40 bg-health-warn/5 p-4 space-y-3"><div class="flex items-center gap-2"><span class="w-2 h-2 rounded-full bg-health-warn animate-pulse"></span> <h4 class="text-xs font-semibold text-health-warn uppercase tracking-wider"> </h4> <!></div> <!> <div class="flex items-center gap-4 text-[9px] text-surface-400"><span> </span> <!></div> <!></div>');
function xo(e, t) {
  Ct(t, !0);
  let r = ft(t, "session", 7), a = ft(t, "onPass", 7), o = ft(t, "onReject", 7), c = /* @__PURE__ */ se(""), d = /* @__PURE__ */ se(!1);
  const u = /* @__PURE__ */ Te(() => Br[r().role]);
  function v() {
    s(c).trim() && (o()(s(c).trim()), x(c, ""), x(d, !1));
  }
  var h = {
    get session() {
      return r();
    },
    set session(N) {
      r(N), vt();
    },
    get onPass() {
      return a();
    },
    set onPass(N) {
      a(N), vt();
    },
    get onReject() {
      return o();
    },
    set onReject(N) {
      o(N), vt();
    }
  }, w = Vf(), g = i(w), b = l(i(g), 2), T = i(b);
  n(b);
  var S = l(b, 2);
  {
    var W = (N) => {
      var U = If(), pe = i(U, !0);
      n(U), C(() => _(pe, r().division_id)), f(N, U);
    };
    P(S, (N) => {
      r().division_id && N(W);
    });
  }
  n(g);
  var $ = l(g, 2);
  {
    var R = (N) => {
      var U = Lf(), pe = i(U), Ne = i(pe, !0);
      n(pe), n(U), C(() => _(Ne, r().gate_output)), f(N, U);
    }, K = (N) => {
      var U = Nf(), pe = i(U), Ne = i(pe, !0);
      n(pe), n(U), C(() => _(Ne, r().output)), f(N, U);
    };
    P($, (N) => {
      r().gate_output ? N(R) : r().output && N(K, 1);
    });
  }
  var ge = l($, 2), ie = i(ge), ae = i(ie);
  n(ie);
  var Z = l(ie, 2);
  {
    var De = (N) => {
      var U = Of(), pe = i(U);
      n(U), C((Ne) => _(pe, `Started: ${Ne ?? ""}`), [() => new Date(r().started_at).toLocaleTimeString()]), f(N, U);
    };
    P(Z, (N) => {
      r().started_at && N(De);
    });
  }
  n(ge);
  var Ve = l(ge, 2);
  {
    var Ge = (N) => {
      var U = Ff(), pe = i(U);
      js(pe);
      var Ne = l(pe, 2), ne = i(Ne);
      ne.__click = v;
      var k = l(ne, 2);
      k.__click = () => x(d, !1), n(Ne), n(U), C(
        (F, Fe) => {
          ne.disabled = F, Re(ne, 1, `px-3 py-1.5 rounded text-xs transition-colors
						${Fe ?? ""}`);
        },
        [
          () => !s(c).trim(),
          () => s(c).trim() ? "bg-health-err/20 text-health-err hover:bg-health-err/30" : "bg-surface-700 text-surface-500 cursor-not-allowed"
        ]
      ), xt(pe, () => s(c), (F) => x(c, F)), f(N, U);
    }, ve = (N) => {
      var U = jf(), pe = i(U);
      pe.__click = function(...ne) {
        a()?.apply(this, ne);
      }, pe.textContent = "✓ Pass Gate";
      var Ne = l(pe, 2);
      Ne.__click = () => x(d, !0), Ne.textContent = "✕ Reject Gate", n(U), f(N, U);
    };
    P(Ve, (N) => {
      s(d) ? N(Ge) : N(ve, !1);
    });
  }
  return n(w), C(
    (N, U) => {
      _(T, `Gate Review: ${s(u).label ?? ""}`), _(ae, `Tokens: ${N ?? ""} in / ${U ?? ""} out`);
    },
    [
      () => r().tokens_in.toLocaleString(),
      () => r().tokens_out.toLocaleString()
    ]
  ), f(e, w), St(h);
}
Ot(["click"]);
Dt(xo, { session: {}, onPass: {}, onReject: {} }, [], [], { mode: "open" });
var Bf = /* @__PURE__ */ p(`<button class="text-[10px] px-2 py-0.5 rounded text-surface-400
					hover:text-surface-200 hover:bg-surface-700 transition-colors">Archive</button>`), qf = /* @__PURE__ */ p('<div class="px-4 py-3 border-b border-surface-600 shrink-0"><h4 class="text-[10px] font-semibold text-surface-300 uppercase tracking-wider mb-2">Output</h4> <div class="rounded bg-surface-800 border border-surface-600 p-3 max-h-48 overflow-y-auto"><pre class="text-[10px] text-surface-200 whitespace-pre-wrap font-mono leading-relaxed"> </pre></div></div>'), Hf = /* @__PURE__ */ p('<div class="px-4 py-3 border-b border-surface-600 shrink-0"><div class="text-[10px] text-health-err bg-health-err/10 rounded px-3 py-2"> </div></div>'), Gf = /* @__PURE__ */ p('<div><div class="flex items-center gap-2 mb-1"><span> </span> <span class="text-[8px] text-surface-500"> </span></div> <pre class="text-[10px] text-surface-200 whitespace-pre-wrap font-mono leading-relaxed"> </pre></div>'), Wf = /* @__PURE__ */ p('<div class="flex-1 overflow-y-auto px-4 py-3"><h4 class="text-[10px] font-semibold text-surface-300 uppercase tracking-wider mb-2"> </h4> <div class="space-y-2"></div></div>'), Uf = /* @__PURE__ */ p('<div class="flex-1 flex items-center justify-center text-surface-500 text-xs">No conversation turns recorded</div>'), zf = /* @__PURE__ */ p('<div class="flex flex-col h-full"><div class="px-4 py-3 border-b border-surface-600 flex items-center gap-3 shrink-0"><button class="text-surface-400 hover:text-surface-100 transition-colors text-sm"></button> <span class="text-sm"> </span> <h3 class="text-sm font-semibold text-surface-100"> </h3> <span> </span> <div class="flex-1"></div> <!></div> <div class="px-4 py-3 border-b border-surface-600 shrink-0"><div class="grid grid-cols-4 gap-3 text-[10px]"><div><span class="text-surface-500 block">Started</span> <span class="text-surface-200"> </span></div> <div><span class="text-surface-500 block">Completed</span> <span class="text-surface-200"> </span></div> <div><span class="text-surface-500 block">Tokens In</span> <span class="text-surface-200"> </span></div> <div><span class="text-surface-500 block">Tokens Out</span> <span class="text-surface-200"> </span></div></div></div> <!> <!> <!></div>');
function ho(e, t) {
  Ct(t, !0);
  let r = ft(t, "session", 7), a = ft(t, "turns", 7), o = ft(t, "onClose", 7), c = ft(t, "onArchive", 7);
  const d = /* @__PURE__ */ Te(() => Br[r().role]);
  function u(H) {
    return H ? new Date(H).toLocaleString(void 0, {
      month: "short",
      day: "numeric",
      hour: "2-digit",
      minute: "2-digit",
      second: "2-digit"
    }) : "-";
  }
  const v = {
    pending: "text-surface-400",
    running: "text-hecate-300",
    completed: "text-health-ok",
    failed: "text-health-err",
    gate_pending: "text-health-warn",
    gate_passed: "text-health-ok",
    gate_rejected: "text-health-err",
    archived: "text-surface-500"
  };
  var h = {
    get session() {
      return r();
    },
    set session(H) {
      r(H), vt();
    },
    get turns() {
      return a();
    },
    set turns(H) {
      a(H), vt();
    },
    get onClose() {
      return o();
    },
    set onClose(H) {
      o(H), vt();
    },
    get onArchive() {
      return c();
    },
    set onArchive(H) {
      c(H), vt();
    }
  }, w = zf(), g = i(w), b = i(g);
  b.__click = function(...H) {
    o()?.apply(this, H);
  }, b.textContent = "←";
  var T = l(b, 2), S = i(T, !0);
  n(T);
  var W = l(T, 2), $ = i(W);
  n(W);
  var R = l(W, 2), K = i(R, !0);
  n(R);
  var ge = l(R, 4);
  {
    var ie = (H) => {
      var M = Bf();
      M.__click = function(...I) {
        c()?.apply(this, I);
      }, f(H, M);
    };
    P(ge, (H) => {
      r().status !== "archived" && r().status !== "running" && H(ie);
    });
  }
  n(g);
  var ae = l(g, 2), Z = i(ae), De = i(Z), Ve = l(i(De), 2), Ge = i(Ve, !0);
  n(Ve), n(De);
  var ve = l(De, 2), N = l(i(ve), 2), U = i(N, !0);
  n(N), n(ve);
  var pe = l(ve, 2), Ne = l(i(pe), 2), ne = i(Ne, !0);
  n(Ne), n(pe);
  var k = l(pe, 2), F = l(i(k), 2), Fe = i(F, !0);
  n(F), n(k), n(Z), n(ae);
  var ze = l(ae, 2);
  {
    var Xe = (H) => {
      var M = qf(), I = l(i(M), 2), z = i(I), Oe = i(z, !0);
      n(z), n(I), n(M), C(() => _(Oe, r().output)), f(H, M);
    };
    P(ze, (H) => {
      r().output && H(Xe);
    });
  }
  var Ee = l(ze, 2);
  {
    var Ce = (H) => {
      var M = Hf(), I = i(M), z = i(I, !0);
      n(I), n(M), C(() => _(z, r().error)), f(H, M);
    };
    P(Ee, (H) => {
      r().error && H(Ce);
    });
  }
  var me = l(Ee, 2);
  {
    var fe = (H) => {
      var M = Wf(), I = i(M), z = i(I);
      n(I);
      var Oe = l(I, 2);
      He(Oe, 21, a, (Ye) => Ye.turn_id, (Ye, A) => {
        var E = Gf(), te = i(E), ke = i(te), Ie = i(ke, !0);
        n(ke);
        var j = l(ke, 2), B = i(j, !0);
        n(j), n(te);
        var xe = l(te, 2), je = i(xe, !0);
        n(xe), n(E), C(
          (Me) => {
            Re(E, 1, `rounded p-2.5
						${s(A).role === "assistant" ? "bg-hecate-600/10 border border-hecate-600/20" : s(A).role === "user" ? "bg-surface-700/50 border border-surface-600" : "bg-surface-800 border border-surface-600/50"}`), Re(ke, 1, `text-[9px] font-semibold uppercase tracking-wider
								${s(A).role === "assistant" ? "text-hecate-300" : "text-surface-400"}`), _(Ie, s(A).role), _(B, Me), _(je, s(A).content);
          },
          [() => new Date(s(A).timestamp).toLocaleTimeString()]
        ), f(Ye, E);
      }), n(Oe), n(M), C(() => _(z, `Conversation (${a().length ?? ""} turns)`)), f(H, M);
    }, de = (H) => {
      var M = Uf();
      f(H, M);
    };
    P(me, (H) => {
      a().length > 0 ? H(fe) : H(de, !1);
    });
  }
  return n(w), C(
    (H, M, I, z) => {
      _(S, s(d).icon), _($, `${s(d).label ?? ""} Session`), Re(R, 1, `text-[10px] ${v[r().status] ?? "text-surface-400" ?? ""}`), _(K, r().status), _(Ge, H), _(U, M), _(ne, I), _(Fe, z);
    },
    [
      () => u(r().started_at),
      () => u(r().completed_at),
      () => r().tokens_in.toLocaleString(),
      () => r().tokens_out.toLocaleString()
    ]
  ), f(e, w), St(h);
}
Ot(["click"]);
Dt(ho, { session: {}, turns: {}, onClose: {}, onArchive: {} }, [], [], { mode: "open" });
var Yf = /* @__PURE__ */ p('<span class="text-[10px] text-surface-400 animate-pulse">Refreshing...</span>'), Kf = /* @__PURE__ */ p('<div class="text-[11px] text-health-err bg-health-err/10 rounded px-3 py-2"> </div>'), Jf = /* @__PURE__ */ p('<div class="space-y-3"></div>'), Qf = /* @__PURE__ */ p('<div class="p-4 space-y-4 overflow-y-auto h-full"><div class="flex items-center justify-between"><div><h3 class="text-sm font-semibold text-surface-100">Agent Pipeline</h3> <p class="text-[11px] text-surface-400 mt-0.5">12 roles across the venture lifecycle</p></div> <!></div> <!> <!> <div><h4 class="text-[10px] font-semibold text-surface-400 uppercase tracking-wider mb-2">Tier 1 — Creative</h4> <div class="grid grid-cols-4 gap-2"></div></div> <div class="flex items-center gap-2 px-2"><div class="flex-1 h-px bg-surface-600"></div> <span class="text-[9px] text-surface-500">gate</span> <span class="text-surface-500"></span> <div class="flex-1 h-px bg-surface-600"></div></div> <div><h4 class="text-[10px] font-semibold text-surface-400 uppercase tracking-wider mb-2">Tier 2 — Mechanical</h4> <div class="grid grid-cols-4 gap-2"></div></div> <div><h4 class="text-[10px] font-semibold text-surface-400 uppercase tracking-wider mb-2">Always-On</h4> <div class="grid grid-cols-4 gap-2"></div></div></div>');
function _o(e, t) {
  Ct(t, !0);
  const r = () => $e($t, "$activeVenture", b), a = () => $e(Bs, "$agentRoleStatuses", b), o = () => $e(zs, "$selectedSession", b), c = () => $e(Ha, "$sessionTurns", b), d = () => $e(qa, "$agentLoading", b), u = () => $e(Xt, "$agentError", b), v = () => $e(mn, "$pendingGates", b), h = () => $e(hf, "$creativeRoles", b), w = () => $e(_f, "$mechanicalRoles", b), g = () => $e(gf, "$alwaysOnRoles", b), [b, T] = qt();
  let S = /* @__PURE__ */ se(null);
  Pt(() => {
    const N = r();
    N && N.venture_id !== s(S) && (x(S, N.venture_id, !0), _s(N.venture_id));
  });
  let W;
  Pt(() => {
    const N = r();
    if (N)
      return W = setInterval(() => _s(N.venture_id), 1e4), () => {
        W && clearInterval(W);
      };
  });
  let $ = /* @__PURE__ */ se(!1);
  function R() {
    return r()?.venture_id ?? "";
  }
  async function K(N) {
    const U = a().find((pe) => pe.role === N);
    U?.active_session && (await bf(R(), U.active_session.session_id), (N === "coordinator" || N === "mentor") && await mf(R(), U.active_session.session_id), x($, !0));
  }
  async function ge(N) {
    await yf(R(), N);
  }
  async function ie(N, U) {
    await wf(R(), U, N);
  }
  async function ae(N, U, pe) {
    await kf(R(), U, N, pe);
  }
  async function Z() {
    const N = o();
    N && (await $f(R(), N.session_id), x($, !1), zs.set(null));
  }
  var De = br(), Ve = ut(De);
  {
    var Ge = (N) => {
      ho(N, {
        get session() {
          return o();
        },
        get turns() {
          return c();
        },
        onClose: () => {
          x($, !1), zs.set(null);
        },
        onArchive: Z
      });
    }, ve = (N) => {
      var U = Qf(), pe = i(U), Ne = l(i(pe), 2);
      {
        var ne = (I) => {
          var z = Yf();
          f(I, z);
        };
        P(Ne, (I) => {
          d() && I(ne);
        });
      }
      n(pe);
      var k = l(pe, 2);
      {
        var F = (I) => {
          var z = Kf(), Oe = i(z, !0);
          n(z), C(() => _(Oe, u())), f(I, z);
        };
        P(k, (I) => {
          u() && I(F);
        });
      }
      var Fe = l(k, 2);
      {
        var ze = (I) => {
          var z = Jf();
          He(z, 5, v, (Oe) => Oe.session_id, (Oe, Ye) => {
            xo(Oe, {
              get session() {
                return s(Ye);
              },
              onPass: () => ie(s(Ye).session_id, s(Ye).role),
              onReject: (A) => ae(s(Ye).session_id, s(Ye).role, A)
            });
          }), n(z), f(I, z);
        };
        P(Fe, (I) => {
          v().length > 0 && I(ze);
        });
      }
      var Xe = l(Fe, 2), Ee = l(i(Xe), 2);
      He(Ee, 5, h, (I) => I.role, (I, z) => {
        Ys(I, {
          get roleStatus() {
            return s(z);
          },
          onSelect: K,
          onInitiate: ge
        });
      }), n(Ee), n(Xe);
      var Ce = l(Xe, 2), me = l(i(Ce), 4);
      me.textContent = "↓", Et(2), n(Ce);
      var fe = l(Ce, 2), de = l(i(fe), 2);
      He(de, 5, w, (I) => I.role, (I, z) => {
        Ys(I, {
          get roleStatus() {
            return s(z);
          },
          onSelect: K,
          onInitiate: ge
        });
      }), n(de), n(fe);
      var H = l(fe, 2), M = l(i(H), 2);
      He(M, 5, g, (I) => I.role, (I, z) => {
        Ys(I, {
          get roleStatus() {
            return s(z);
          },
          onSelect: K,
          onInitiate: ge
        });
      }), n(M), n(H), n(U), f(N, U);
    };
    P(Ve, (N) => {
      s($) && o() ? N(Ge) : N(ve, !1);
    });
  }
  f(e, De), St(), T();
}
Dt(_o, {}, [], [], { mode: "open" });
var Xf = /* @__PURE__ */ p("<div></div>"), Zf = /* @__PURE__ */ p("<!> <button><span> </span> <span> </span></button>", 1), ep = /* @__PURE__ */ p('<span class="text-[10px] text-surface-500 mr-1">Pending</span>'), tp = /* @__PURE__ */ p("<button> </button>"), rp = /* @__PURE__ */ p(`<div class="border-b border-surface-600 bg-surface-800/30 px-4 py-2 shrink-0"><div class="flex items-center gap-1"><!> <div class="flex-1"></div> <span class="text-[10px] text-surface-400 mr-2"> </span> <!> <button class="text-[10px] px-2 py-0.5 rounded text-hecate-400
					hover:bg-hecate-600/20 transition-colors ml-1" title="Open AI Assistant"></button></div></div>`);
function go(e, t) {
  Ct(t, !0);
  const r = () => $e(Hr, "$selectedDivision", c), a = () => $e(hs, "$selectedPhase", c), o = () => $e(Pr, "$isLoading", c), [c, d] = qt();
  let u = /* @__PURE__ */ Te(() => r() ? La(r(), a()) : []);
  function v(R) {
    hs.set(R);
  }
  function h(R, K) {
    switch (R) {
      case "storming":
        return "border-es-event text-es-event";
      case "planning":
        return "border-phase-planning text-phase-planning";
      case "kanban":
        return "border-hecate-400 text-hecate-400";
      case "crafting":
        return "border-phase-crafting text-phase-crafting";
    }
  }
  function w(R, K) {
    return R ? K.length === 0 ? { icon: "✓", css: "text-health-ok" } : K.includes("resume") ? { icon: "◐", css: "text-health-warn" } : K.includes("shelve") || K.includes("conclude") || K.includes("archive") ? { icon: "●", css: "text-hecate-400 animate-pulse" } : K.includes("open") ? { icon: "○", css: "text-surface-300" } : { icon: "○", css: "text-surface-500" } : { icon: "○", css: "text-surface-500" };
  }
  function g(R) {
    switch (R) {
      case "open":
        return "bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30";
      case "shelve":
        return "text-surface-400 hover:text-health-warn hover:bg-surface-700";
      case "conclude":
        return "text-surface-400 hover:text-health-ok hover:bg-surface-700";
      case "resume":
        return "bg-health-warn/10 text-health-warn hover:bg-health-warn/20";
      case "archive":
        return "text-surface-400 hover:text-surface-200 hover:bg-surface-700";
      default:
        return "text-surface-400 hover:bg-surface-700";
    }
  }
  function b(R) {
    return R.charAt(0).toUpperCase() + R.slice(1);
  }
  async function T(R) {
    if (!r()) return;
    const K = r().division_id, ge = a();
    switch (R) {
      case "open":
        await tc(K, ge);
        break;
      case "shelve":
        await rc(K, ge);
        break;
      case "resume":
        await sc(K, ge);
        break;
      case "conclude":
        await ac(K, ge);
        break;
    }
  }
  var S = br(), W = ut(S);
  {
    var $ = (R) => {
      var K = rp(), ge = i(K), ie = i(ge);
      He(ie, 17, () => Kr, pt, (N, U, pe) => {
        const Ne = /* @__PURE__ */ Te(() => Ia(r(), s(U).code)), ne = /* @__PURE__ */ Te(() => La(r(), s(U).code)), k = /* @__PURE__ */ Te(() => a() === s(U).code), F = /* @__PURE__ */ Te(() => {
          const { icon: M, css: I } = w(s(Ne), s(ne));
          return { icon: M, css: I };
        }), Fe = /* @__PURE__ */ Te(() => s(Ne) && s(ne).length === 0);
        var ze = Zf(), Xe = ut(ze);
        {
          var Ee = (M) => {
            var I = Xf();
            C(() => Re(I, 1, `w-4 h-px ${s(Fe) ? "bg-health-ok/40" : "bg-surface-600"}`)), f(M, I);
          };
          P(Xe, (M) => {
            pe > 0 && M(Ee);
          });
        }
        var Ce = l(Xe, 2);
        Ce.__click = () => v(s(U).code);
        var me = i(Ce), fe = i(me, !0);
        n(me);
        var de = l(me, 2), H = i(de, !0);
        n(de), n(Ce), C(
          (M) => {
            Re(Ce, 1, `flex items-center gap-1.5 px-3 py-1.5 rounded text-xs transition-all
						border
						${M ?? ""}`), Re(me, 1, `${s(F).css ?? ""} text-[10px]`), _(fe, s(F).icon), _(H, s(U).shortName);
          },
          [
            () => s(k) ? `bg-surface-700 border-current ${h(s(U).code)}` : "border-transparent text-surface-400 hover:text-surface-200 hover:bg-surface-700/50"
          ]
        ), f(N, ze);
      });
      var ae = l(ie, 4), Z = i(ae, !0);
      n(ae);
      var De = l(ae, 2);
      {
        var Ve = (N) => {
          const U = /* @__PURE__ */ Te(() => Ia(r(), a()));
          var pe = br(), Ne = ut(pe);
          {
            var ne = (k) => {
              var F = ep();
              f(k, F);
            };
            P(Ne, (k) => {
              s(U) || k(ne);
            });
          }
          f(N, pe);
        }, Ge = (N) => {
          var U = br(), pe = ut(U);
          He(pe, 17, () => s(u), pt, (Ne, ne) => {
            var k = tp();
            k.__click = () => T(s(ne));
            var F = i(k, !0);
            n(k), C(
              (Fe, ze) => {
                k.disabled = o(), Re(k, 1, `text-[10px] px-2 py-0.5 rounded transition-colors disabled:opacity-50
							${Fe ?? ""}`), _(F, ze);
              },
              [
                () => g(s(ne)),
                () => b(s(ne))
              ]
            ), f(Ne, k);
          }), f(N, U);
        };
        P(De, (N) => {
          s(u).length === 0 ? N(Ve) : N(Ge, !1);
        });
      }
      var ve = l(De, 2);
      ve.__click = () => $r(`Help with ${Kr.find((N) => N.code === a())?.name} phase for division "${r()?.context_name}"`), ve.textContent = "✦ AI Assist", n(ge), n(K), C((N) => _(Z, N), [() => Kr.find((N) => N.code === a())?.name]), f(R, K);
    };
    P(W, (R) => {
      r() && R($);
    });
  }
  f(e, S), St(), d();
}
Ot(["click"]);
Dt(go, {}, [], [], { mode: "open" });
var sp = /* @__PURE__ */ p('<span class="text-[9px] text-surface-500"> </span>'), ap = /* @__PURE__ */ p('<div class="flex items-center justify-center h-full"><div class="text-center text-surface-400"><div class="text-sm mb-2 animate-pulse">...</div> <div class="text-[10px]">Loading events</div></div></div>'), np = /* @__PURE__ */ p('<div class="flex items-center justify-center h-full"><div class="text-center text-surface-500 text-xs">Select a venture to view its event stream.</div></div>'), ip = /* @__PURE__ */ p('<div class="flex items-center justify-center h-full"><div class="text-center text-surface-500"><div class="text-lg mb-2"></div> <div class="text-xs">No events recorded yet.</div> <div class="text-[10px] mt-1">Events will appear here as the venture progresses.</div></div></div>'), op = /* @__PURE__ */ p('<span class="text-[9px] px-1 py-0.5 rounded bg-surface-700 text-surface-400 shrink-0"> </span>'), lp = /* @__PURE__ */ p('<span class="text-[9px] text-surface-500 shrink-0 tabular-nums"> </span>'), cp = /* @__PURE__ */ p(`<div class="px-4 pb-3 pt-0 ml-5"><pre class="text-[10px] text-surface-300 bg-surface-800 border border-surface-600
									rounded p-3 overflow-x-auto whitespace-pre-wrap break-words
									font-mono leading-relaxed"> </pre></div>`), dp = /* @__PURE__ */ p(`<div class="group"><button class="w-full text-left px-4 py-2 flex items-start gap-2
								hover:bg-surface-700/30 transition-colors"><span class="text-[9px] text-surface-500 mt-0.5 shrink-0 w-3"> </span> <span> </span> <!> <!></button> <!></div>`), up = /* @__PURE__ */ p('<div class="p-3 border-t border-surface-700/50"><button> </button></div>'), vp = /* @__PURE__ */ p('<div class="divide-y divide-surface-700/50"></div> <!>', 1), fp = /* @__PURE__ */ p('<div class="flex flex-col h-full"><div class="border-b border-surface-600 bg-surface-800/50 px-4 py-2 shrink-0"><div class="flex items-center gap-2"><span class="text-xs text-surface-400">Event Stream</span> <!> <div class="flex-1"></div> <button title="Refresh events"> </button></div></div> <div class="flex-1 overflow-y-auto"><!></div></div>');
function Ga(e, t) {
  Ct(t, !0);
  const r = () => $e(_n, "$ventureRawEvents", o), a = () => $e($t, "$activeVenture", o), [o, c] = qt(), d = 50;
  let u = /* @__PURE__ */ se(!1), v = /* @__PURE__ */ se(0), h = /* @__PURE__ */ se(0), w = /* @__PURE__ */ se(Ut(/* @__PURE__ */ new Set())), g = /* @__PURE__ */ Te(() => s(h) + d < s(v)), b = /* @__PURE__ */ Te(r);
  async function T(k, F = !0) {
    x(u, !0), F && (x(h, 0), x(w, /* @__PURE__ */ new Set(), !0));
    try {
      const Fe = await Fn(k, s(h), d);
      x(v, Fe.count, !0);
    } finally {
      x(u, !1);
    }
  }
  async function S() {
    const k = a();
    if (!(!k || s(u))) {
      x(h, s(h) + d), x(u, !0);
      try {
        const F = await Fn(k.venture_id, s(h), d);
        x(v, F.count, !0);
      } finally {
        x(u, !1);
      }
    }
  }
  function W(k) {
    const F = new Set(s(w));
    F.has(k) ? F.delete(k) : F.add(k), x(w, F, !0);
  }
  function $(k) {
    return k.startsWith("venture_") || k.startsWith("big_picture_storm_") ? "text-hecate-400" : k.startsWith("event_sticky_") ? "text-es-event" : k.startsWith("event_stack_") || k.startsWith("event_cluster_") ? "text-success-400" : k.startsWith("fact_arrow_") ? "text-sky-400" : k.startsWith("storm_phase_") ? "text-accent-400" : "text-surface-400";
  }
  function R(k) {
    if (!k) return "";
    const F = typeof k == "string" ? Number(k) || new Date(k).getTime() : k;
    if (isNaN(F)) return "";
    const Fe = new Date(F), Xe = Date.now() - F, Ee = Math.floor(Xe / 1e3);
    if (Ee < 60) return `${Ee}s ago`;
    const Ce = Math.floor(Ee / 60);
    if (Ce < 60) return `${Ce}m ago`;
    const me = Math.floor(Ce / 60);
    if (me < 24) return `${me}h ago`;
    const fe = Math.floor(me / 24);
    return fe < 7 ? `${fe}d ago` : Fe.toLocaleDateString("en-US", {
      month: "short",
      day: "numeric",
      hour: "2-digit",
      minute: "2-digit"
    });
  }
  function K(k) {
    try {
      return JSON.stringify(k, null, 2);
    } catch {
      return String(k);
    }
  }
  Pt(() => {
    const k = a();
    k && T(k.venture_id);
  });
  var ge = fp(), ie = i(ge), ae = i(ie), Z = l(i(ae), 2);
  {
    var De = (k) => {
      var F = sp(), Fe = i(F);
      n(F), C(() => _(Fe, `${s(b).length ?? ""}${s(v) > s(b).length ? ` / ${s(v)}` : ""} events`)), f(k, F);
    };
    P(Z, (k) => {
      s(b).length > 0 && k(De);
    });
  }
  var Ve = l(Z, 4);
  Ve.__click = () => {
    const k = a();
    k && T(k.venture_id);
  };
  var Ge = i(Ve, !0);
  n(Ve), n(ae), n(ie);
  var ve = l(ie, 2), N = i(ve);
  {
    var U = (k) => {
      var F = ap();
      f(k, F);
    }, pe = (k) => {
      var F = np();
      f(k, F);
    }, Ne = (k) => {
      var F = ip(), Fe = i(F), ze = i(Fe);
      ze.textContent = "○", Et(4), n(Fe), n(F), f(k, F);
    }, ne = (k) => {
      var F = vp(), Fe = ut(F);
      He(Fe, 21, () => s(b), pt, (Ee, Ce, me) => {
        const fe = /* @__PURE__ */ Te(() => s(w).has(me)), de = /* @__PURE__ */ Te(() => $(s(Ce).event_type));
        var H = dp(), M = i(H);
        M.__click = () => W(me);
        var I = i(M), z = i(I, !0);
        n(I);
        var Oe = l(I, 2), Ye = i(Oe, !0);
        n(Oe);
        var A = l(Oe, 2);
        {
          var E = (B) => {
            var xe = op(), je = i(xe);
            n(xe), C(() => _(je, `v${s(Ce).version ?? ""}`)), f(B, xe);
          };
          P(A, (B) => {
            s(Ce).version !== void 0 && B(E);
          });
        }
        var te = l(A, 2);
        {
          var ke = (B) => {
            var xe = lp(), je = i(xe, !0);
            n(xe), C((Me) => _(je, Me), [() => R(s(Ce).timestamp)]), f(B, xe);
          };
          P(te, (B) => {
            s(Ce).timestamp && B(ke);
          });
        }
        n(M);
        var Ie = l(M, 2);
        {
          var j = (B) => {
            var xe = cp(), je = i(xe), Me = i(je, !0);
            n(je), n(xe), C((et) => _(Me, et), [() => K(s(Ce).data)]), f(B, xe);
          };
          P(Ie, (B) => {
            s(fe) && B(j);
          });
        }
        n(H), C(() => {
          _(z, s(fe) ? "▾" : "▸"), Re(Oe, 1, `text-[11px] font-mono ${s(de) ?? ""} flex-1 min-w-0 truncate`), _(Ye, s(Ce).event_type);
        }), f(Ee, H);
      }), n(Fe);
      var ze = l(Fe, 2);
      {
        var Xe = (Ee) => {
          var Ce = up(), me = i(Ce);
          me.__click = S;
          var fe = i(me, !0);
          n(me), n(Ce), C(() => {
            me.disabled = s(u), Re(me, 1, `w-full text-[10px] py-1.5 rounded transition-colors
							${s(u) ? "bg-surface-700 text-surface-500 cursor-not-allowed" : "bg-surface-700 text-surface-300 hover:text-surface-100 hover:bg-surface-600"}`), _(fe, s(u) ? "Loading..." : `Load More (${s(v) - s(b).length} remaining)`);
          }), f(Ee, Ce);
        };
        P(ze, (Ee) => {
          s(g) && Ee(Xe);
        });
      }
      f(k, F);
    };
    P(N, (k) => {
      s(u) && s(b).length === 0 ? k(U) : a() ? s(b).length === 0 ? k(Ne, 2) : k(ne, !1) : k(pe, 1);
    });
  }
  n(ve), n(ge), C(() => {
    Ve.disabled = s(u) || !a(), Re(Ve, 1, `text-[10px] px-2 py-0.5 rounded transition-colors
					${s(u) || !a() ? "text-surface-500 cursor-not-allowed" : "text-surface-400 hover:text-surface-200 hover:bg-surface-700"}`), _(Ge, s(u) ? "Loading..." : "Refresh");
  }), f(e, ge), St(), c();
}
Ot(["click"]);
Dt(Ga, {}, [], [], { mode: "open" });
var pp = /* @__PURE__ */ p(`<div class="flex justify-end"><div class="max-w-[85%] rounded-lg px-3 py-2 text-[11px]
						bg-hecate-600/20 text-surface-100 border border-hecate-600/20"> </div></div>`), xp = /* @__PURE__ */ p('<div class="flex justify-start"><div><div class="whitespace-pre-wrap break-words"> </div></div></div>'), hp = /* @__PURE__ */ p('<div class="whitespace-pre-wrap break-words"> <span class="inline-block w-1.5 h-3 bg-hecate-400 animate-pulse ml-0.5"></span></div>'), _p = /* @__PURE__ */ p('<div class="flex items-center gap-1.5 text-surface-400"><span class="animate-bounce" style="animation-delay: 0ms">.</span> <span class="animate-bounce" style="animation-delay: 150ms">.</span> <span class="animate-bounce" style="animation-delay: 300ms">.</span></div>'), gp = /* @__PURE__ */ p(`<div class="flex justify-start"><div class="max-w-[85%] rounded-lg px-3 py-2 text-[11px]
					bg-surface-700 text-surface-200 border border-surface-600"><!></div></div>`), bp = /* @__PURE__ */ p('<span class="text-[9px] text-hecate-400 ml-1">(code-optimized)</span>'), mp = /* @__PURE__ */ p('<span class="text-hecate-400"> </span> <!>', 1), yp = /* @__PURE__ */ p('<span class="text-health-warn">No model available</span>'), wp = /* @__PURE__ */ p('<div class="flex items-center justify-center h-full"><div class="text-center text-surface-400"><div class="text-xl mb-2"></div> <div class="text-[11px]">AI Assistant ready <br/> <!></div></div></div>'), kp = /* @__PURE__ */ p(`<div class="w-[380px] border-l border-surface-600 bg-surface-800 flex flex-col shrink-0 overflow-hidden"><div class="flex items-center gap-2 px-3 py-2 border-b border-surface-600 shrink-0"><span class="text-hecate-400"></span> <span class="text-xs font-semibold text-surface-100">AI</span> <!> <div class="flex-1"></div> <span class="text-[9px] text-surface-400"> </span> <button class="text-surface-400 hover:text-surface-100 transition-colors px-1" title="Close AI Assistant"></button></div> <div class="flex-1 overflow-y-auto p-3 space-y-3"><!> <!> <!></div> <div class="border-t border-surface-600 p-2 shrink-0"><div class="flex gap-1.5"><textarea class="flex-1 bg-surface-700 border border-surface-600 rounded px-2.5 py-1.5
					text-[11px] text-surface-100 placeholder-surface-400 resize-none
					focus:outline-none focus:border-hecate-500
					disabled:opacity-50 disabled:cursor-not-allowed"></textarea> <button>Send</button></div></div></div>`);
function Wa(e, t) {
  Ct(t, !0);
  const r = () => $e(hs, "$selectedPhase", v), a = () => $e(Xi, "$phaseModelPrefs", v), o = () => $e(fn, "$aiModel", v), c = () => $e(Ki, "$aiAssistContext", v), d = () => $e($t, "$activeVenture", v), u = () => $e(Hr, "$selectedDivision", v), [v, h] = qt(), w = Yi();
  let g = /* @__PURE__ */ se(Ut([])), b = /* @__PURE__ */ se(""), T = /* @__PURE__ */ se(!1), S = /* @__PURE__ */ se(""), W = /* @__PURE__ */ se(void 0), $ = /* @__PURE__ */ se(null), R = /* @__PURE__ */ se(null), K = /* @__PURE__ */ Te(() => Ql(r())), ge = /* @__PURE__ */ Te(() => a()[r()]);
  Pt(() => {
    const M = o();
    s(R) !== null && s(R) !== M && (s($) && (s($).cancel(), x($, null)), x(g, [], !0), x(S, ""), x(T, !1)), x(R, M, !0);
  }), Pt(() => {
    const M = c();
    M && s(g).length === 0 && ae(M);
  });
  function ie() {
    const M = [], I = Jt(oo);
    I && M.push(I);
    const z = Kr.find((Oe) => Oe.code === r());
    if (z && M.push(`You are currently assisting with the ${z.name} phase. ${z.description}.`), d()) {
      let Oe = `Venture: "${d().name}"`;
      d().brief && (Oe += ` — ${d().brief}`), M.push(Oe);
    }
    return u() && M.push(`Division: "${u().context_name}" (bounded context)`), M.push(Jt(id)), M.join(`

---

`);
  }
  async function ae(M) {
    const I = o();
    if (!I || !M.trim() || s(T)) return;
    const z = { role: "user", content: M.trim() };
    x(g, [...s(g), z], !0), x(b, "");
    const Oe = [], Ye = ie();
    Ye && Oe.push({ role: "system", content: Ye }), Oe.push(...s(g)), x(T, !0), x(S, "");
    let A = "";
    const E = w.stream.chat(I, Oe);
    x($, E, !0), E.onChunk((te) => {
      te.content && (A += te.content, x(S, A, !0));
    }).onDone(async (te) => {
      x($, null), te.content && (A += te.content);
      const ke = {
        role: "assistant",
        content: A || "(empty response)"
      };
      if (x(g, [...s(g), ke], !0), x(S, ""), x(T, !1), Jt(un) === "oracle" && A) {
        const j = Jt($t)?.venture_id;
        if (j) {
          const B = Zl(A);
          for (const xe of B)
            await Ws(j, xe, "oracle");
        }
      }
    }).onError((te) => {
      x($, null);
      const ke = { role: "assistant", content: `Error: ${te}` };
      x(g, [...s(g), ke], !0), x(S, ""), x(T, !1);
    });
    try {
      await E.start();
    } catch (te) {
      const ke = { role: "assistant", content: `Error: ${String(te)}` };
      x(g, [...s(g), ke], !0), x(T, !1);
    }
  }
  let Z = /* @__PURE__ */ se(void 0);
  function De(M) {
    M.key === "Enter" && !M.shiftKey && (M.preventDefault(), ae(s(b)), s(Z) && (s(Z).style.height = "auto"));
  }
  function Ve(M) {
    const I = M.target;
    I.style.height = "auto", I.style.height = Math.min(I.scrollHeight, 120) + "px";
  }
  function Ge() {
    Xl(), x(g, [], !0), x(S, "");
  }
  Pt(() => {
    s(g), s(S), ln().then(() => {
      s(W) && (s(W).scrollTop = s(W).scrollHeight);
    });
  });
  var ve = kp(), N = i(ve), U = i(N);
  U.textContent = "✦";
  var pe = l(U, 4);
  {
    let M = /* @__PURE__ */ Te(() => Kr.find((I) => I.code === r())?.shortName ?? "");
    xa(pe, {
      get currentModel() {
        return o();
      },
      onSelect: (I) => pn(I),
      showPhaseInfo: !0,
      get phasePreference() {
        return s(ge);
      },
      get phaseAffinity() {
        return s(K);
      },
      onPinModel: (I) => On(r(), I),
      onClearPin: () => On(r(), null),
      get phaseName() {
        return s(M);
      }
    });
  }
  var Ne = l(pe, 4), ne = i(Ne, !0);
  n(Ne);
  var k = l(Ne, 2);
  k.__click = Ge, k.textContent = "✕", n(N);
  var F = l(N, 2), Fe = i(F);
  He(Fe, 17, () => s(g), pt, (M, I) => {
    var z = br(), Oe = ut(z);
    {
      var Ye = (E) => {
        var te = pp(), ke = i(te), Ie = i(ke, !0);
        n(ke), n(te), C(() => _(Ie, s(I).content)), f(E, te);
      }, A = (E) => {
        var te = xp(), ke = i(te), Ie = i(ke), j = i(Ie, !0);
        n(Ie), n(ke), n(te), C(
          (B) => {
            Re(ke, 1, `max-w-[85%] rounded-lg px-3 py-2 text-[11px]
						bg-surface-700 text-surface-200 border border-surface-600
						${B ?? ""}`), _(j, s(I).content);
          },
          [
            () => s(I).content.startsWith("Error:") ? "border-health-err/30 text-health-err" : ""
          ]
        ), f(E, te);
      };
      P(Oe, (E) => {
        s(I).role === "user" ? E(Ye) : s(I).role === "assistant" && E(A, 1);
      });
    }
    f(M, z);
  });
  var ze = l(Fe, 2);
  {
    var Xe = (M) => {
      var I = gp(), z = i(I), Oe = i(z);
      {
        var Ye = (E) => {
          var te = hp(), ke = i(te, !0);
          Et(), n(te), C(() => _(ke, s(S))), f(E, te);
        }, A = (E) => {
          var te = _p();
          f(E, te);
        };
        P(Oe, (E) => {
          s(S) ? E(Ye) : E(A, !1);
        });
      }
      n(z), n(I), f(M, I);
    };
    P(ze, (M) => {
      s(T) && M(Xe);
    });
  }
  var Ee = l(ze, 2);
  {
    var Ce = (M) => {
      var I = wp(), z = i(I), Oe = i(z);
      Oe.textContent = "✦";
      var Ye = l(Oe, 2), A = l(i(Ye), 3);
      {
        var E = (ke) => {
          var Ie = mp(), j = ut(Ie), B = i(j, !0);
          n(j);
          var xe = l(j, 2);
          {
            var je = (Me) => {
              var et = bp();
              f(Me, et);
            };
            P(xe, (Me) => {
              s(K) === "code" && Me(je);
            });
          }
          C(() => _(B, o())), f(ke, Ie);
        }, te = (ke) => {
          var Ie = yp();
          f(ke, Ie);
        };
        P(A, (ke) => {
          o() ? ke(E) : ke(te, !1);
        });
      }
      n(Ye), n(z), n(I), f(M, I);
    };
    P(Ee, (M) => {
      s(g).length === 0 && !s(T) && M(Ce);
    });
  }
  n(F), ls(F, (M) => x(W, M), () => s(W));
  var me = l(F, 2), fe = i(me), de = i(fe);
  js(de), de.__keydown = De, de.__input = Ve, zt(de, "rows", 1), ls(de, (M) => x(Z, M), () => s(Z));
  var H = l(de, 2);
  H.__click = () => ae(s(b)), n(fe), n(me), n(ve), C(
    (M, I, z) => {
      _(ne, M), zt(de, "placeholder", s(T) ? "Waiting..." : "Ask about this phase..."), de.disabled = s(T) || !o(), H.disabled = I, Re(H, 1, `px-2.5 rounded text-[11px] transition-colors self-end
					${z ?? ""}`);
    },
    [
      () => Kr.find((M) => M.code === r())?.shortName ?? "",
      () => s(T) || !s(b).trim() || !o(),
      () => s(T) || !s(b).trim() || !o() ? "bg-surface-600 text-surface-400 cursor-not-allowed" : "bg-hecate-600 text-surface-50 hover:bg-hecate-500"
    ]
  ), xt(de, () => s(b), (M) => x(b, M)), f(e, ve), St(), h();
}
Ot(["click", "keydown", "input"]);
Dt(Wa, {}, [], [], { mode: "open" });
var $p = /* @__PURE__ */ p('<div class="flex items-center justify-center h-full"><div class="text-center text-surface-400"><div class="text-2xl mb-2 animate-pulse"></div> <div class="text-sm">Loading venture...</div></div></div>'), Cp = /* @__PURE__ */ p('<div class="text-[11px] text-health-err bg-health-err/10 rounded px-3 py-2"> </div>'), Sp = /* @__PURE__ */ p(`<div class="rounded-xl border border-hecate-600/30 bg-surface-800/80 p-5 space-y-4"><h3 class="text-xs font-medium text-hecate-300 uppercase tracking-wider">New Venture</h3> <div class="grid grid-cols-[1fr_2fr] gap-4"><div><label for="venture-name" class="text-[11px] text-surface-300 block mb-1.5">Name</label> <input id="venture-name" placeholder="e.g., my-saas-app" class="w-full bg-surface-700 border border-surface-600 rounded-lg
										px-3 py-2 text-sm text-surface-100 placeholder-surface-500
										focus:outline-none focus:border-hecate-500"/></div> <div><label for="venture-brief" class="text-[11px] text-surface-300 block mb-1.5">Brief (optional)</label> <input id="venture-brief" placeholder="What does this venture aim to achieve?" class="w-full bg-surface-700 border border-surface-600 rounded-lg
										px-3 py-2 text-sm text-surface-100 placeholder-surface-500
										focus:outline-none focus:border-hecate-500"/></div></div> <!> <div class="flex gap-3"><button> </button> <button class="px-4 py-2 rounded-lg text-xs text-hecate-400 border border-hecate-600/30
									hover:bg-hecate-600/10 transition-colors"></button></div></div>`), Ep = /* @__PURE__ */ p(`<div class="flex flex-col items-center justify-center py-20 text-center"><div class="text-4xl mb-4 text-hecate-400"></div> <h2 class="text-lg font-semibold text-surface-100 mb-2">No Ventures Yet</h2> <p class="text-xs text-surface-400 leading-relaxed max-w-sm mb-6">A venture is the umbrella for your software endeavor. It houses
							divisions (bounded contexts) and guides them through the development
							lifecycle.</p> <button class="px-5 py-2.5 rounded-lg text-sm font-medium bg-hecate-600 text-surface-50
								hover:bg-hecate-500 transition-colors">+ Create Your First Venture</button></div>`), Ap = /* @__PURE__ */ p('<div class="text-[11px] text-surface-400 truncate mt-1.5 ml-5"> </div>'), Dp = /* @__PURE__ */ p(`<button class="group text-left p-4 rounded-xl bg-surface-800/80 border border-surface-600
												hover:border-hecate-500 hover:bg-hecate-600/5 transition-all"><div class="flex items-center gap-2"><span class="text-hecate-400 group-hover:text-hecate-300"></span> <span class="font-medium text-sm text-surface-100 truncate"> </span> <span class="text-[10px] px-1.5 py-0.5 rounded-full bg-surface-700 text-surface-300 border border-surface-600 shrink-0"> </span></div> <!></button>`), Pp = /* @__PURE__ */ p('<div><h3 class="text-[11px] text-surface-400 uppercase tracking-wider mb-3">Recently Updated</h3> <div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-3"></div></div>'), Tp = /* @__PURE__ */ p('<div class="text-[11px] text-surface-500 truncate mt-1.5 ml-5"> </div>'), Rp = /* @__PURE__ */ p(`<button class="group text-left p-4 rounded-xl bg-surface-800/40 border border-surface-700
													hover:border-surface-500 transition-all opacity-60 hover:opacity-80"><div class="flex items-center gap-2"><span class="text-surface-500"></span> <span class="font-medium text-sm text-surface-300 truncate"> </span> <span class="text-[10px] px-1.5 py-0.5 rounded-full bg-surface-700 text-surface-400 border border-surface-600 shrink-0">Archived</span></div> <!></button>`), Mp = /* @__PURE__ */ p('<div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-3"></div>'), Ip = /* @__PURE__ */ p(`<div><button class="flex items-center gap-2 text-[11px] text-surface-500 uppercase tracking-wider
										hover:text-surface-300 transition-colors mb-3"><span class="text-[9px]"> </span> <span class="text-surface-600"> </span></button> <!></div>`), Lp = /* @__PURE__ */ p('<div class="text-[11px] text-surface-400 truncate mt-1.5 ml-5"> </div>'), Np = /* @__PURE__ */ p(`<button class="group text-left p-4 rounded-xl bg-surface-800/80 border border-surface-600
												hover:border-hecate-500 hover:bg-hecate-600/5 transition-all"><div class="flex items-center gap-2"><span class="text-hecate-400 group-hover:text-hecate-300"></span> <span class="font-medium text-sm text-surface-100 truncate"> </span> <span class="text-[10px] px-1.5 py-0.5 rounded-full bg-surface-700 text-surface-300 border border-surface-600 shrink-0"> </span></div> <!></button>`), Op = /* @__PURE__ */ p('<div><h3 class="text-[11px] text-surface-400 uppercase tracking-wider mb-3"> </h3> <div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-3"></div></div>'), Fp = /* @__PURE__ */ p('<div class="text-center py-12 text-surface-400 text-sm"> </div>'), jp = /* @__PURE__ */ p("<!>  <!> <!>", 1), Vp = /* @__PURE__ */ p('<div class="absolute top-0 right-0 bottom-0 z-10"><!></div>'), Bp = /* @__PURE__ */ p(
  `<div class="flex flex-col h-full overflow-hidden"><div class="border-b border-surface-600 bg-surface-800/50 px-4 py-3 shrink-0"><div class="flex items-center gap-3"><span class="text-hecate-400 text-lg"></span> <h1 class="text-sm font-semibold text-surface-100">Ventures</h1> <div class="flex items-center gap-1.5 text-[10px]"><span></span> <span class="text-surface-500"> </span></div> <!> <div class="flex-1"></div> <input placeholder="Search ventures..." class="w-48 bg-surface-700 border border-surface-600 rounded-lg
							px-3 py-1.5 text-xs text-surface-100 placeholder-surface-500
							focus:outline-none focus:border-hecate-500"/> <button> </button></div></div> <div class="flex-1 overflow-y-auto p-4 space-y-6"><!> <!></div></div> <!>`,
  1
), qp = /* @__PURE__ */ p('<!> <div class="flex-1 overflow-y-auto"><!></div>', 1), Hp = /* @__PURE__ */ p('<div class="w-80 border-l border-surface-600 overflow-hidden shrink-0"><!></div>'), Gp = /* @__PURE__ */ p('<div class="flex h-full"><div class="flex-1 overflow-hidden"><!></div> <!></div>'), Wp = /* @__PURE__ */ p('<div class="w-80 border-l border-surface-600 overflow-hidden shrink-0"><!></div>'), Up = /* @__PURE__ */ p('<div class="flex h-full"><div class="flex-1 overflow-hidden"><!></div> <!></div>'), zp = /* @__PURE__ */ p('<div class="flex items-center justify-center h-full text-surface-400 text-sm">Select a division from the sidebar</div>'), Yp = /* @__PURE__ */ p('<!> <div class="absolute top-2 right-2 flex items-center gap-2 text-[10px] z-10"><button title="Agent Pipeline">Agents</button> <span class="flex items-center gap-1.5"><span></span> <span class="text-surface-500"> </span></span></div> <div class="flex flex-1 overflow-hidden relative"><!> <div class="flex-1 flex flex-col overflow-hidden"><!></div> <!></div>', 1), Kp = /* @__PURE__ */ p('<div class="flex flex-col h-full"><!></div>');
function Jp(e, t) {
  Ct(t, !0);
  const r = () => $e(wt, "$isLoading", S), a = () => $e($t, "$activeVenture", S), o = () => $e(fn, "$aiModel", S), c = () => $e(pr, "$ventureError", S), d = () => $e(xs, "$ventures", S), u = () => $e(dn, "$showAIAssist", S), v = () => $e(Zr, "$divisions", S), h = () => $e(Hr, "$selectedDivision", S), w = () => $e(hs, "$selectedPhase", S), g = () => $e(da, "$ventureStep", S), b = () => $e(es, "$bigPicturePhase", S), T = () => $e(Fa, "$showEventStream", S), [S, W] = qt();
  let $ = ft(t, "api", 7), R = /* @__PURE__ */ se(null), K = /* @__PURE__ */ se("connecting"), ge, ie = /* @__PURE__ */ se(""), ae = /* @__PURE__ */ se(""), Z = /* @__PURE__ */ se(""), De = /* @__PURE__ */ se(!1), Ve = /* @__PURE__ */ se(!1), Ge = /* @__PURE__ */ se(!1);
  function ve(Ee, Ce) {
    let me = Ee;
    if (Ce.trim()) {
      const z = Ce.toLowerCase();
      me = Ee.filter((Oe) => Oe.name.toLowerCase().includes(z) || Oe.brief && Oe.brief.toLowerCase().includes(z));
    }
    const fe = [], de = [], H = [], M = [];
    for (const z of me)
      Lr(z.status, Ds) ? M.push(z) : Lr(z.status, Wi) || Lr(z.status, Ui) ? de.push(z) : z.phase === "initiated" || z.phase === "vision_refined" || z.phase === "vision_submitted" ? fe.push(z) : z.phase === "discovery_completed" || z.phase === "designing" || z.phase === "planning" || z.phase === "crafting" || z.phase === "deploying" ? H.push(z) : fe.push(z);
    const I = [];
    return fe.length > 0 && I.push({ label: "Setup", ventures: fe }), de.length > 0 && I.push({ label: "Discovery", ventures: de }), H.length > 0 && I.push({ label: "Building", ventures: H }), M.length > 0 && I.push({ label: "Archived", ventures: M }), I;
  }
  function N(Ee) {
    return Ee.filter((Ce) => !Lr(Ce.status, Ds)).sort((Ce, me) => (me.updated_at ?? "").localeCompare(Ce.updated_at ?? "")).slice(0, 5);
  }
  async function U() {
    try {
      x(R, await $().get("/health"), !0), x(K, "connected");
    } catch {
      x(R, null), x(K, "disconnected");
    }
  }
  ji(async () => {
    Wl($()), U(), ge = setInterval(U, 5e3), yr(), ua();
    const Ee = await Ul();
    vn.set(Ee);
  }), Sl(() => {
    ge && clearInterval(ge);
  });
  async function pe() {
    if (!s(ie).trim()) return;
    await Zi(s(ie).trim(), s(ae).trim() || "") && (x(ie, ""), x(ae, ""), x(De, !1));
  }
  var Ne = {
    get api() {
      return $();
    },
    set api(Ee) {
      $(Ee), vt();
    }
  }, ne = Kp(), k = i(ne);
  {
    var F = (Ee) => {
      var Ce = $p(), me = i(Ce), fe = i(me);
      fe.textContent = "◆", Et(2), n(me), n(Ce), f(Ee, Ce);
    }, Fe = (Ee) => {
      var Ce = Bp(), me = ut(Ce), fe = i(me), de = i(fe), H = i(de);
      H.textContent = "◆";
      var M = l(H, 4), I = i(M), z = l(I, 2), Oe = i(z, !0);
      n(z), n(M);
      var Ye = l(M, 2);
      xa(Ye, {
        get currentModel() {
          return o();
        },
        onSelect: (qe) => pn(qe)
      });
      var A = l(Ye, 4);
      bt(A);
      var E = l(A, 2);
      E.__click = () => x(De, !s(De));
      var te = i(E, !0);
      n(E), n(de), n(fe);
      var ke = l(fe, 2), Ie = i(ke);
      {
        var j = (qe) => {
          var Ke = Sp(), We = l(i(Ke), 2), tt = i(We), st = l(i(tt), 2);
          bt(st), n(tt);
          var Je = l(tt, 2), L = l(i(Je), 2);
          bt(L), n(Je), n(We);
          var D = l(We, 2);
          {
            var oe = (Le) => {
              var m = Cp(), y = i(m, !0);
              n(m), C(() => _(y, c())), f(Le, m);
            };
            P(D, (Le) => {
              c() && Le(oe);
            });
          }
          var Se = l(D, 2), G = i(Se);
          G.__click = pe;
          var ee = i(G, !0);
          n(G);
          var Ae = l(G, 2);
          Ae.__click = () => $r("Help me define a new venture. What should I consider? Ask me about the problem domain, target users, and core functionality."), Ae.textContent = "✦ AI Help", n(Se), n(Ke), C(
            (Le, m) => {
              G.disabled = Le, Re(G, 1, `px-4 py-2 rounded-lg text-xs font-medium transition-colors
									${m ?? ""}`), _(ee, r() ? "Initiating..." : "Initiate Venture");
            },
            [
              () => !s(ie).trim() || r(),
              () => !s(ie).trim() || r() ? "bg-surface-600 text-surface-400 cursor-not-allowed" : "bg-hecate-600 text-surface-50 hover:bg-hecate-500"
            ]
          ), xt(st, () => s(ie), (Le) => x(ie, Le)), xt(L, () => s(ae), (Le) => x(ae, Le)), f(qe, Ke);
        };
        P(Ie, (qe) => {
          s(De) && qe(j);
        });
      }
      var B = l(Ie, 2);
      {
        var xe = (qe) => {
          var Ke = Ep(), We = i(Ke);
          We.textContent = "◆";
          var tt = l(We, 6);
          tt.__click = () => x(De, !0), n(Ke), f(qe, Ke);
        }, je = (qe) => {
          const Ke = /* @__PURE__ */ Te(() => ve(d(), s(Z)));
          var We = jp(), tt = ut(We);
          {
            var st = (G) => {
              const ee = /* @__PURE__ */ Te(() => N(d()));
              var Ae = br(), Le = ut(Ae);
              {
                var m = (y) => {
                  var le = Pp(), O = l(i(le), 2);
                  He(O, 21, () => s(ee), pt, (re, he) => {
                    var we = Dp();
                    we.__click = () => Ts(s(he));
                    var J = i(we), V = i(J);
                    V.textContent = "◆";
                    var Y = l(V, 2), ye = i(Y, !0);
                    n(Y);
                    var _e = l(Y, 2), ce = i(_e, !0);
                    n(_e), n(J);
                    var Q = l(J, 2);
                    {
                      var X = (q) => {
                        var ue = Ap(), be = i(ue, !0);
                        n(ue), C(() => _(be, s(he).brief)), f(q, ue);
                      };
                      P(Q, (q) => {
                        s(he).brief && q(X);
                      });
                    }
                    n(we), C(() => {
                      _(ye, s(he).name), _(ce, s(he).status_label ?? s(he).phase);
                    }), f(re, we);
                  }), n(O), n(le), f(y, le);
                };
                P(Le, (y) => {
                  s(ee).length > 0 && y(m);
                });
              }
              f(G, Ae);
            }, Je = /* @__PURE__ */ Te(() => !s(Z).trim() && d().filter((G) => !Lr(G.status, Ds)).length > 3);
            P(tt, (G) => {
              s(Je) && G(st);
            });
          }
          var L = l(tt, 2);
          He(L, 17, () => s(Ke), pt, (G, ee) => {
            var Ae = br(), Le = ut(Ae);
            {
              var m = (le) => {
                var O = Ip(), re = i(O);
                re.__click = () => x(Ve, !s(Ve));
                var he = i(re), we = i(he, !0);
                n(he);
                var J = l(he), V = l(J), Y = i(V);
                n(V), n(re);
                var ye = l(re, 2);
                {
                  var _e = (ce) => {
                    var Q = Mp();
                    He(Q, 21, () => s(ee).ventures, pt, (X, q) => {
                      var ue = Rp();
                      ue.__click = () => Ts(s(q));
                      var be = i(ue), Pe = i(be);
                      Pe.textContent = "◆";
                      var Be = l(Pe, 2), Qe = i(Be, !0);
                      n(Be), Et(2), n(be);
                      var rt = l(be, 2);
                      {
                        var at = (nt) => {
                          var gt = Tp(), ht = i(gt, !0);
                          n(gt), C(() => _(ht, s(q).brief)), f(nt, gt);
                        };
                        P(rt, (nt) => {
                          s(q).brief && nt(at);
                        });
                      }
                      n(ue), C(() => _(Qe, s(q).name)), f(X, ue);
                    }), n(Q), f(ce, Q);
                  };
                  P(ye, (ce) => {
                    s(Ve) && ce(_e);
                  });
                }
                n(O), C(() => {
                  _(we, s(Ve) ? "▼" : "▶"), _(J, ` ${s(ee).label ?? ""} `), _(Y, `(${s(ee).ventures.length ?? ""})`);
                }), f(le, O);
              }, y = (le) => {
                var O = Op(), re = i(O), he = i(re, !0);
                n(re);
                var we = l(re, 2);
                He(we, 21, () => s(ee).ventures, pt, (J, V) => {
                  var Y = Np();
                  Y.__click = () => Ts(s(V));
                  var ye = i(Y), _e = i(ye);
                  _e.textContent = "◆";
                  var ce = l(_e, 2), Q = i(ce, !0);
                  n(ce);
                  var X = l(ce, 2), q = i(X, !0);
                  n(X), n(ye);
                  var ue = l(ye, 2);
                  {
                    var be = (Pe) => {
                      var Be = Lp(), Qe = i(Be, !0);
                      n(Be), C(() => _(Qe, s(V).brief)), f(Pe, Be);
                    };
                    P(ue, (Pe) => {
                      s(V).brief && Pe(be);
                    });
                  }
                  n(Y), C(() => {
                    _(Q, s(V).name), _(q, s(V).status_label ?? s(V).phase);
                  }), f(J, Y);
                }), n(we), n(O), C(() => _(he, s(ee).label)), f(le, O);
              };
              P(Le, (le) => {
                s(ee).label === "Archived" ? le(m) : le(y, !1);
              });
            }
            f(G, Ae);
          });
          var D = l(L, 2);
          {
            var oe = (G) => {
              var ee = Fp(), Ae = i(ee);
              n(ee), C(() => _(Ae, `No ventures matching "${s(Z) ?? ""}"`)), f(G, ee);
            }, Se = /* @__PURE__ */ Te(() => s(Ke).length === 0 && s(Z).trim());
            P(D, (G) => {
              s(Se) && G(oe);
            });
          }
          f(qe, We);
        };
        P(B, (qe) => {
          d().length === 0 && !s(De) ? qe(xe) : qe(je, !1);
        });
      }
      n(ke), n(me);
      var Me = l(me, 2);
      {
        var et = (qe) => {
          var Ke = Vp(), We = i(Ke);
          Wa(We, {}), n(Ke), f(qe, Ke);
        };
        P(Me, (qe) => {
          u() && qe(et);
        });
      }
      C(() => {
        Re(I, 1, `inline-block w-1.5 h-1.5 rounded-full ${s(K) === "connected" ? "bg-success-400" : s(K) === "connecting" ? "bg-yellow-400 animate-pulse" : "bg-danger-400"}`), _(Oe, s(K) === "connected" ? `v${s(R)?.version ?? "?"}` : s(K)), Re(E, 1, `px-3 py-1.5 rounded-lg text-xs font-medium transition-colors
							${s(De) ? "bg-surface-600 text-surface-300" : "bg-hecate-600 text-surface-50 hover:bg-hecate-500"}`), _(te, s(De) ? "Cancel" : "+ New Venture");
      }), xt(A, () => s(Z), (qe) => x(Z, qe)), f(Ee, Ce);
    }, ze = (Ee) => {
      var Ce = Yp(), me = ut(Ce);
      no(me, {});
      var fe = l(me, 2), de = i(fe);
      de.__click = () => x(Ge, !s(Ge));
      var H = l(de, 2), M = i(H), I = l(M, 2), z = i(I, !0);
      n(I), n(H), n(fe);
      var Oe = l(fe, 2), Ye = i(Oe);
      {
        var A = (Me) => {
          io(Me, {});
        };
        P(Ye, (Me) => {
          v().length > 0 && Me(A);
        });
      }
      var E = l(Ye, 2), te = i(E);
      {
        var ke = (Me) => {
          _o(Me, {});
        }, Ie = (Me) => {
          var et = qp(), qe = ut(et);
          go(qe, {});
          var Ke = l(qe, 2), We = i(Ke);
          {
            var tt = (D) => {
              co(D, {});
            }, st = (D) => {
              uo(D, {});
            }, Je = (D) => {
              vo(D, {});
            }, L = (D) => {
              po(D, {});
            };
            P(We, (D) => {
              w() === "storming" ? D(tt) : w() === "planning" ? D(st, 1) : w() === "kanban" ? D(Je, 2) : w() === "crafting" && D(L, 3);
            });
          }
          n(Ke), f(Me, et);
        }, j = (Me) => {
          var et = br(), qe = ut(et);
          {
            var Ke = (D) => {
              var oe = Gp(), Se = i(oe), G = i(Se);
              Va(G, {}), n(Se);
              var ee = l(Se, 2);
              {
                var Ae = (Le) => {
                  var m = Hp(), y = i(m);
                  Ga(y, {}), n(m), f(Le, m);
                };
                P(ee, (Le) => {
                  T() && Le(Ae);
                });
              }
              n(oe), f(D, oe);
            }, We = (D) => {
              lo(D, {});
            }, tt = (D) => {
              Us(D, { nextAction: "discovery" });
            }, st = (D) => {
              var oe = Up(), Se = i(oe), G = i(Se);
              Va(G, {}), n(Se);
              var ee = l(Se, 2);
              {
                var Ae = (Le) => {
                  var m = Wp(), y = i(m);
                  Ga(y, {}), n(m), f(Le, m);
                };
                P(ee, (Le) => {
                  T() && Le(Ae);
                });
              }
              n(oe), f(D, oe);
            }, Je = (D) => {
              Us(D, { nextAction: "identify" });
            }, L = (D) => {
              Us(D, { nextAction: "discovery" });
            };
            P(qe, (D) => {
              g() === "discovering" || b() !== "ready" ? D(Ke) : g() === "initiated" || g() === "vision_refined" ? D(We, 1) : g() === "vision_submitted" ? D(tt, 2) : g() === "discovery_paused" ? D(st, 3) : g() === "discovery_completed" ? D(Je, 4) : D(L, !1);
            });
          }
          f(Me, et);
        }, B = (Me) => {
          var et = zp();
          f(Me, et);
        };
        P(te, (Me) => {
          s(Ge) ? Me(ke) : h() ? Me(Ie, 1) : v().length === 0 ? Me(j, 2) : Me(B, !1);
        });
      }
      n(E);
      var xe = l(E, 2);
      {
        var je = (Me) => {
          Wa(Me, {});
        };
        P(xe, (Me) => {
          u() && Me(je);
        });
      }
      n(Oe), C(() => {
        Re(de, 1, `px-2 py-0.5 rounded transition-colors
					${s(Ge) ? "bg-hecate-600/20 text-hecate-300" : "text-surface-400 hover:text-surface-200 hover:bg-surface-700"}`), Re(M, 1, `inline-block w-1.5 h-1.5 rounded-full ${s(K) === "connected" ? "bg-success-400" : s(K) === "connecting" ? "bg-yellow-400 animate-pulse" : "bg-danger-400"}`), _(z, s(K) === "connected" ? `v${s(R)?.version ?? "?"}` : s(K));
      }), f(Ee, Ce);
    };
    P(k, (Ee) => {
      r() && !a() ? Ee(F) : a() ? Ee(ze, !1) : Ee(Fe, 1);
    });
  }
  n(ne), f(e, ne);
  var Xe = St(Ne);
  return W(), Xe;
}
Ot(["click"]);
customElements.define("martha-studio", Dt(Jp, { api: {} }, [], []));
export {
  Jp as default
};
