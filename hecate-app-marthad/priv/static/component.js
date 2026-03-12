typeof window < "u" && ((window.__svelte ??= {}).v ??= /* @__PURE__ */ new Set()).add("5");
const Lo = 1, Oo = 2, ei = 4, Fo = 8, jo = 16, Bo = 2, Vo = 4, Go = 8, qo = 1, Ho = 2, tn = "[", la = "[!", rn = "]", es = {}, Lt = /* @__PURE__ */ Symbol(), Wo = "http://www.w3.org/1999/xhtml", Pa = !1;
var sn = Array.isArray, Uo = Array.prototype.indexOf, fs = Array.prototype.includes, da = Array.from, ra = Object.keys, Os = Object.defineProperty, vs = Object.getOwnPropertyDescriptor, ti = Object.getOwnPropertyDescriptors, zo = Object.prototype, Yo = Array.prototype, an = Object.getPrototypeOf, Rn = Object.isExtensible;
const Ar = () => {
};
function Ko(e) {
  return e();
}
function sa(e) {
  for (var t = 0; t < e.length; t++)
    e[t]();
}
function ri() {
  var e, t, r = new Promise((n, c) => {
    e = n, t = c;
  });
  return { promise: r, resolve: e, reject: t };
}
function Ta(e, t) {
  if (Array.isArray(e))
    return e;
  if (!(Symbol.iterator in e))
    return Array.from(e);
  const r = [];
  for (const n of e)
    if (r.push(n), r.length === t) break;
  return r;
}
const Ft = 2, aa = 4, Vs = 8, si = 1 << 24, Ir = 16, fr = 32, Wr = 64, nn = 128, sr = 512, Rt = 1024, jt = 2048, vr = 4096, Xt = 8192, Dr = 16384, ua = 32768, ps = 65536, Mn = 1 << 17, ai = 1 << 18, ns = 1 << 19, ni = 1 << 20, Sr = 1 << 25, ts = 32768, Ra = 1 << 21, on = 1 << 22, jr = 1 << 23, Br = /* @__PURE__ */ Symbol("$state"), Jo = /* @__PURE__ */ Symbol("legacy props"), Qo = /* @__PURE__ */ Symbol(""), ds = new class extends Error {
  name = "StaleReactionError";
  message = "The reaction that called `getAbortSignal()` was re-run or destroyed";
}(), va = 3, is = 8;
function ii(e) {
  throw new Error("https://svelte.dev/e/lifecycle_outside_component");
}
function Xo() {
  throw new Error("https://svelte.dev/e/async_derived_orphan");
}
function Zo(e, t, r) {
  throw new Error("https://svelte.dev/e/each_key_duplicate");
}
function ec(e) {
  throw new Error("https://svelte.dev/e/effect_in_teardown");
}
function tc() {
  throw new Error("https://svelte.dev/e/effect_in_unowned_derived");
}
function rc(e) {
  throw new Error("https://svelte.dev/e/effect_orphan");
}
function sc() {
  throw new Error("https://svelte.dev/e/effect_update_depth_exceeded");
}
function ac() {
  throw new Error("https://svelte.dev/e/hydration_failed");
}
function nc() {
  throw new Error("https://svelte.dev/e/state_descriptors_fixed");
}
function ic() {
  throw new Error("https://svelte.dev/e/state_prototype_fixed");
}
function oc() {
  throw new Error("https://svelte.dev/e/state_unsafe_mutation");
}
function cc() {
  throw new Error("https://svelte.dev/e/svelte_boundary_reset_onerror");
}
function Gs(e) {
  console.warn("https://svelte.dev/e/hydration_mismatch");
}
function lc() {
  console.warn("https://svelte.dev/e/select_multiple_invalid_value");
}
function dc() {
  console.warn("https://svelte.dev/e/svelte_boundary_reset_noop");
}
let it = !1;
function Er(e) {
  it = e;
}
let lt;
function Wt(e) {
  if (e === null)
    throw Gs(), es;
  return lt = e;
}
function xs() {
  return Wt(/* @__PURE__ */ pr(lt));
}
function a(e) {
  if (it) {
    if (/* @__PURE__ */ pr(lt) !== null)
      throw Gs(), es;
    lt = e;
  }
}
function At(e = 1) {
  if (it) {
    for (var t = e, r = lt; t--; )
      r = /** @type {TemplateNode} */
      /* @__PURE__ */ pr(r);
    lt = r;
  }
}
function na(e = !0) {
  for (var t = 0, r = lt; ; ) {
    if (r.nodeType === is) {
      var n = (
        /** @type {Comment} */
        r.data
      );
      if (n === rn) {
        if (t === 0) return r;
        t -= 1;
      } else (n === tn || n === la || // "[1", "[2", etc. for if blocks
      n[0] === "[" && !isNaN(Number(n.slice(1)))) && (t += 1);
    }
    var c = (
      /** @type {TemplateNode} */
      /* @__PURE__ */ pr(r)
    );
    e && r.remove(), r = c;
  }
}
function oi(e) {
  if (!e || e.nodeType !== is)
    throw Gs(), es;
  return (
    /** @type {Comment} */
    e.data
  );
}
function ci(e) {
  return e === this.v;
}
function li(e, t) {
  return e != e ? t == t : e !== t || e !== null && typeof e == "object" || typeof e == "function";
}
function di(e) {
  return !li(e, this.v);
}
let ys = !1, uc = !1;
function vc() {
  ys = !0;
}
let mt = null;
function hs(e) {
  mt = e;
}
function kt(e, t = !1, r) {
  mt = {
    p: mt,
    i: !1,
    c: null,
    e: null,
    s: e,
    x: null,
    l: ys && !t ? { s: null, u: null, $: [] } : null
  };
}
function $t(e) {
  var t = (
    /** @type {ComponentContext} */
    mt
  ), r = t.e;
  if (r !== null) {
    t.e = null;
    for (var n of r)
      Pi(n);
  }
  return e !== void 0 && (t.x = e), t.i = !0, mt = t.p, e ?? /** @type {T} */
  {};
}
function qs() {
  return !ys || mt !== null && mt.l === null;
}
let zr = [];
function ui() {
  var e = zr;
  zr = [], sa(e);
}
function mr(e) {
  if (zr.length === 0 && !Ts) {
    var t = zr;
    queueMicrotask(() => {
      t === zr && ui();
    });
  }
  zr.push(e);
}
function fc() {
  for (; zr.length > 0; )
    ui();
}
function vi(e) {
  var t = dt;
  if (t === null)
    return ot.f |= jr, e;
  if ((t.f & ua) === 0) {
    if ((t.f & nn) === 0)
      throw e;
    t.b.error(e);
  } else
    _s(e, t);
}
function _s(e, t) {
  for (; t !== null; ) {
    if ((t.f & nn) !== 0)
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
const pc = -7169;
function yt(e, t) {
  e.f = e.f & pc | t;
}
function cn(e) {
  (e.f & sr) !== 0 || e.deps === null ? yt(e, Rt) : yt(e, vr);
}
function fi(e) {
  if (e !== null)
    for (const t of e)
      (t.f & Ft) === 0 || (t.f & ts) === 0 || (t.f ^= ts, fi(
        /** @type {Derived} */
        t.deps
      ));
}
function pi(e, t, r) {
  (e.f & jt) !== 0 ? t.add(e) : (e.f & vr) !== 0 && r.add(e), fi(e.deps), yt(e, Rt);
}
const Ys = /* @__PURE__ */ new Set();
let ut = null, ia = null, cr = null, Yt = [], fa = null, Ma = !1, Ts = !1;
class Pr {
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
  #c = !1;
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
      for (var n of r.d)
        yt(n, jt), lr(n);
      for (n of r.m)
        yt(n, vr), lr(n);
    }
  }
  /**
   *
   * @param {Effect[]} root_effects
   */
  process(t) {
    Yt = [], this.apply();
    var r = [], n = [];
    for (const c of t)
      this.#l(c, r, n);
    if (this.is_deferred()) {
      this.#u(n), this.#u(r);
      for (const [c, l] of this.#n)
        gi(c, l);
    } else {
      for (const c of this.#e) c();
      this.#e.clear(), this.#r === 0 && this.#d(), ia = this, ut = null, In(n), In(r), ia = null, this.#o?.resolve();
    }
    cr = null;
  }
  /**
   * Traverse the effect tree, executing effects or stashing
   * them for later execution as appropriate
   * @param {Effect} root
   * @param {Effect[]} effects
   * @param {Effect[]} render_effects
   */
  #l(t, r, n) {
    t.f ^= Rt;
    for (var c = t.first, l = null; c !== null; ) {
      var u = c.f, d = (u & (fr | Wr)) !== 0, f = d && (u & Rt) !== 0, g = f || (u & Xt) !== 0 || this.#n.has(c);
      if (!g && c.fn !== null) {
        d ? c.f ^= Rt : l !== null && (u & (aa | Vs | si)) !== 0 ? l.b.defer_effect(c) : (u & aa) !== 0 ? r.push(c) : Us(c) && ((u & Ir) !== 0 && this.#s.add(c), Fs(c));
        var m = c.first;
        if (m !== null) {
          c = m;
          continue;
        }
      }
      var k = c.parent;
      for (c = c.next; c === null && k !== null; )
        k === l && (l = null), c = k.next, k = k.parent;
    }
  }
  /**
   * @param {Effect[]} effects
   */
  #u(t) {
    for (var r = 0; r < t.length; r += 1)
      pi(t[r], this.#a, this.#s);
  }
  /**
   * Associate a change to a given source with the current
   * batch, noting its previous and current values
   * @param {Source} source
   * @param {any} value
   */
  capture(t, r) {
    r !== Lt && !this.previous.has(t) && this.previous.set(t, r), (t.f & jr) === 0 && (this.current.set(t, t.v), cr?.set(t, t.v));
  }
  activate() {
    ut = this, this.apply();
  }
  deactivate() {
    ut === this && (ut = null, cr = null);
  }
  flush() {
    if (this.activate(), Yt.length > 0) {
      if (xi(), ut !== null && ut !== this)
        return;
    } else this.#r === 0 && this.process([]);
    this.deactivate();
  }
  discard() {
    for (const t of this.#t) t(this);
    this.#t.clear();
  }
  #d() {
    if (Ys.size > 1) {
      this.previous.clear();
      var t = cr, r = !0;
      for (const c of Ys) {
        if (c === this) {
          r = !1;
          continue;
        }
        const l = [];
        for (const [d, f] of this.current) {
          if (c.current.has(d))
            if (r && f !== c.current.get(d))
              c.current.set(d, f);
            else
              continue;
          l.push(d);
        }
        if (l.length === 0)
          continue;
        const u = [...c.current.keys()].filter((d) => !this.current.has(d));
        if (u.length > 0) {
          var n = Yt;
          Yt = [];
          const d = /* @__PURE__ */ new Set(), f = /* @__PURE__ */ new Map();
          for (const g of l)
            hi(g, u, d, f);
          if (Yt.length > 0) {
            ut = c, c.apply();
            for (const g of Yt)
              c.#l(g, [], []);
            c.deactivate();
          }
          Yt = n;
        }
      }
      ut = null, cr = t;
    }
    this.committed = !0, Ys.delete(this);
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
    this.#r -= 1, t && (this.#i -= 1), !this.#c && (this.#c = !0, mr(() => {
      this.#c = !1, this.is_deferred() ? Yt.length > 0 && this.flush() : this.revive();
    }));
  }
  revive() {
    for (const t of this.#a)
      this.#s.delete(t), yt(t, jt), lr(t);
    for (const t of this.#s)
      yt(t, vr), lr(t);
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
    return (this.#o ??= ri()).promise;
  }
  static ensure() {
    if (ut === null) {
      const t = ut = new Pr();
      Ys.add(ut), Ts || mr(() => {
        ut === t && t.flush();
      });
    }
    return ut;
  }
  apply() {
  }
}
function vt(e) {
  var t = Ts;
  Ts = !0;
  try {
    for (var r; ; ) {
      if (fc(), Yt.length === 0 && (ut?.flush(), Yt.length === 0))
        return fa = null, /** @type {T} */
        r;
      xi();
    }
  } finally {
    Ts = t;
  }
}
function xi() {
  Ma = !0;
  var e = null;
  try {
    for (var t = 0; Yt.length > 0; ) {
      var r = Pr.ensure();
      if (t++ > 1e3) {
        var n, c;
        xc();
      }
      r.process(Yt), Vr.clear();
    }
  } finally {
    Yt = [], Ma = !1, fa = null;
  }
}
function xc() {
  try {
    sc();
  } catch (e) {
    _s(e, fa);
  }
}
let $r = null;
function In(e) {
  var t = e.length;
  if (t !== 0) {
    for (var r = 0; r < t; ) {
      var n = e[r++];
      if ((n.f & (Dr | Xt)) === 0 && Us(n) && ($r = /* @__PURE__ */ new Set(), Fs(n), n.deps === null && n.first === null && n.nodes === null && (n.teardown === null && n.ac === null ? Ii(n) : n.fn = null), $r?.size > 0)) {
        Vr.clear();
        for (const c of $r) {
          if ((c.f & (Dr | Xt)) !== 0) continue;
          const l = [c];
          let u = c.parent;
          for (; u !== null; )
            $r.has(u) && ($r.delete(u), l.push(u)), u = u.parent;
          for (let d = l.length - 1; d >= 0; d--) {
            const f = l[d];
            (f.f & (Dr | Xt)) === 0 && Fs(f);
          }
        }
        $r.clear();
      }
    }
    $r = null;
  }
}
function hi(e, t, r, n) {
  if (!r.has(e) && (r.add(e), e.reactions !== null))
    for (const c of e.reactions) {
      const l = c.f;
      (l & Ft) !== 0 ? hi(
        /** @type {Derived} */
        c,
        t,
        r,
        n
      ) : (l & (on | Ir)) !== 0 && (l & jt) === 0 && _i(c, t, n) && (yt(c, jt), lr(
        /** @type {Effect} */
        c
      ));
    }
}
function _i(e, t, r) {
  const n = r.get(e);
  if (n !== void 0) return n;
  if (e.deps !== null)
    for (const c of e.deps) {
      if (fs.call(t, c))
        return !0;
      if ((c.f & Ft) !== 0 && _i(
        /** @type {Derived} */
        c,
        t,
        r
      ))
        return r.set(
          /** @type {Derived} */
          c,
          !0
        ), !0;
    }
  return r.set(e, !1), !1;
}
function lr(e) {
  for (var t = fa = e; t.parent !== null; ) {
    t = t.parent;
    var r = t.f;
    if (Ma && t === dt && (r & Ir) !== 0 && (r & ai) === 0)
      return;
    if ((r & (Wr | fr)) !== 0) {
      if ((r & Rt) === 0) return;
      t.f ^= Rt;
    }
  }
  Yt.push(t);
}
function gi(e, t) {
  if (!((e.f & fr) !== 0 && (e.f & Rt) !== 0)) {
    (e.f & jt) !== 0 ? t.d.push(e) : (e.f & vr) !== 0 && t.m.push(e), yt(e, Rt);
    for (var r = e.first; r !== null; )
      gi(r, t), r = r.next;
  }
}
function hc(e) {
  let t = 0, r = rs(0), n;
  return () => {
    fn() && (s(r), ha(() => (t === 0 && (n = os(() => e(() => Rs(r)))), t += 1, () => {
      mr(() => {
        t -= 1, t === 0 && (n?.(), n = void 0, Rs(r));
      });
    })));
  };
}
var _c = ps | ns | nn;
function gc(e, t, r) {
  new bc(e, t, r);
}
class bc {
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
  #c = null;
  /** @type {TemplateNode | null} */
  #l = null;
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
  #y = hc(() => (this.#v = rs(this.#u), () => {
    this.#v = null;
  }));
  /**
   * @param {TemplateNode} node
   * @param {BoundaryProps} props
   * @param {((anchor: Node) => void)} children
   */
  constructor(t, r, n) {
    this.#e = t, this.#r = r, this.#i = n, this.parent = /** @type {Effect} */
    dt.b, this.is_pending = !!this.#r.pending, this.#o = xn(() => {
      if (dt.b = this, it) {
        const l = this.#t;
        xs(), /** @type {Comment} */
        l.nodeType === is && /** @type {Comment} */
        l.data === la ? this.#k() : (this.#w(), this.#d === 0 && (this.is_pending = !1));
      } else {
        var c = this.#b();
        try {
          this.#a = tr(() => n(c));
        } catch (l) {
          this.error(l);
        }
        this.#d > 0 ? this.#g() : this.is_pending = !1;
      }
      return () => {
        this.#l?.remove();
      };
    }, _c), it && (this.#e = lt);
  }
  #w() {
    try {
      this.#a = tr(() => this.#i(this.#e));
    } catch (t) {
      this.error(t);
    }
  }
  #k() {
    const t = this.#r.pending;
    t && (this.#s = tr(() => t(this.#e)), mr(() => {
      var r = this.#b();
      this.#a = this.#_(() => (Pr.ensure(), tr(() => this.#i(r)))), this.#d > 0 ? this.#g() : (Jr(
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
    return this.is_pending && (this.#l = ar(), this.#e.before(this.#l), t = this.#l), t;
  }
  /**
   * Defer an effect inside a pending boundary until the boundary resolves
   * @param {Effect} effect
   */
  defer_effect(t) {
    pi(t, this.#x, this.#h);
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
    var r = dt, n = ot, c = mt;
    yr(this.#o), ir(this.#o), hs(this.#o.ctx);
    try {
      return t();
    } catch (l) {
      return vi(l), null;
    } finally {
      yr(r), ir(n), hs(c);
    }
  }
  #g() {
    const t = (
      /** @type {(anchor: Node) => void} */
      this.#r.pending
    );
    this.#a !== null && (this.#c = document.createDocumentFragment(), this.#c.append(
      /** @type {TemplateNode} */
      this.#l
    ), Oi(this.#a, this.#c)), this.#s === null && (this.#s = tr(() => t(this.#e)));
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
        yt(r, jt), lr(r);
      for (const r of this.#h)
        yt(r, vr), lr(r);
      this.#x.clear(), this.#h.clear(), this.#s && Jr(this.#s, () => {
        this.#s = null;
      }), this.#c && (this.#e.before(this.#c), this.#c = null);
    }
  }
  /**
   * Update the source that powers `$effect.pending()` inside this boundary,
   * and controls when the current `pending` snippet (if any) is removed.
   * Do not call from inside the class
   * @param {1 | -1} d
   */
  update_pending_count(t) {
    this.#m(t), this.#u += t, !(!this.#v || this.#p) && (this.#p = !0, mr(() => {
      this.#p = !1, this.#v && gs(this.#v, this.#u);
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
    let n = this.#r.failed;
    if (this.#f || !r && !n)
      throw t;
    this.#a && (Ut(this.#a), this.#a = null), this.#s && (Ut(this.#s), this.#s = null), this.#n && (Ut(this.#n), this.#n = null), it && (Wt(
      /** @type {TemplateNode} */
      this.#t
    ), At(), Wt(na()));
    var c = !1, l = !1;
    const u = () => {
      if (c) {
        dc();
        return;
      }
      c = !0, l && cc(), Pr.ensure(), this.#u = 0, this.#n !== null && Jr(this.#n, () => {
        this.#n = null;
      }), this.is_pending = this.has_pending_snippet(), this.#a = this.#_(() => (this.#f = !1, tr(() => this.#i(this.#e)))), this.#d > 0 ? this.#g() : this.is_pending = !1;
    };
    mr(() => {
      try {
        l = !0, r?.(t, u), l = !1;
      } catch (d) {
        _s(d, this.#o && this.#o.parent);
      }
      n && (this.#n = this.#_(() => {
        Pr.ensure(), this.#f = !0;
        try {
          return tr(() => {
            n(
              this.#e,
              () => t,
              () => u
            );
          });
        } catch (d) {
          return _s(
            d,
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
function mc(e, t, r, n) {
  const c = qs() ? Hs : us;
  var l = e.filter((y) => !y.settled);
  if (r.length === 0 && l.length === 0) {
    n(t.map(c));
    return;
  }
  var u = ut, d = (
    /** @type {Effect} */
    dt
  ), f = yc(), g = l.length === 1 ? l[0].promise : l.length > 1 ? Promise.all(l.map((y) => y.promise)) : null;
  function m(y) {
    f();
    try {
      n(y);
    } catch (P) {
      (d.f & Dr) === 0 && _s(P, d);
    }
    u?.deactivate(), Ia();
  }
  if (r.length === 0) {
    g.then(() => m(t.map(c)));
    return;
  }
  function k() {
    f(), Promise.all(r.map((y) => /* @__PURE__ */ wc(y))).then((y) => m([...t.map(c), ...y])).catch((y) => _s(y, d));
  }
  g ? g.then(k) : k();
}
function yc() {
  var e = dt, t = ot, r = mt, n = ut;
  return function(l = !0) {
    yr(e), ir(t), hs(r), l && n?.activate();
  };
}
function Ia() {
  yr(null), ir(null), hs(null);
}
// @__NO_SIDE_EFFECTS__
function Hs(e) {
  var t = Ft | jt, r = ot !== null && (ot.f & Ft) !== 0 ? (
    /** @type {Derived} */
    ot
  ) : null;
  return dt !== null && (dt.f |= ns), {
    ctx: mt,
    deps: null,
    effects: null,
    equals: ci,
    f: t,
    fn: e,
    reactions: null,
    rv: 0,
    v: (
      /** @type {V} */
      Lt
    ),
    wv: 0,
    parent: r ?? dt,
    ac: null
  };
}
// @__NO_SIDE_EFFECTS__
function wc(e, t, r) {
  let n = (
    /** @type {Effect | null} */
    dt
  );
  n === null && Xo();
  var c = (
    /** @type {Boundary} */
    n.b
  ), l = (
    /** @type {Promise<V>} */
    /** @type {unknown} */
    void 0
  ), u = rs(
    /** @type {V} */
    Lt
  ), d = !ot, f = /* @__PURE__ */ new Map();
  return Pc(() => {
    var g = ri();
    l = g.promise;
    try {
      Promise.resolve(e()).then(g.resolve, g.reject).then(() => {
        m === ut && m.committed && m.deactivate(), Ia();
      });
    } catch (P) {
      g.reject(P), Ia();
    }
    var m = (
      /** @type {Batch} */
      ut
    );
    if (d) {
      var k = c.is_rendered();
      c.update_pending_count(1), m.increment(k), f.get(m)?.reject(ds), f.delete(m), f.set(m, g);
    }
    const y = (P, E = void 0) => {
      if (m.activate(), E)
        E !== ds && (u.f |= jr, gs(u, E));
      else {
        (u.f & jr) !== 0 && (u.f ^= jr), gs(u, P);
        for (const [Q, S] of f) {
          if (f.delete(Q), Q === m) break;
          S.reject(ds);
        }
      }
      d && (c.update_pending_count(-1), m.decrement(k));
    };
    g.promise.then(y, (P) => y(null, P || "unknown"));
  }), xa(() => {
    for (const g of f.values())
      g.reject(ds);
  }), new Promise((g) => {
    function m(k) {
      function y() {
        k === l ? g(u) : m(l);
      }
      k.then(y, y);
    }
    m(l);
  });
}
// @__NO_SIDE_EFFECTS__
function Ee(e) {
  const t = /* @__PURE__ */ Hs(e);
  return Fi(t), t;
}
// @__NO_SIDE_EFFECTS__
function us(e) {
  const t = /* @__PURE__ */ Hs(e);
  return t.equals = di, t;
}
function bi(e) {
  var t = e.effects;
  if (t !== null) {
    e.effects = null;
    for (var r = 0; r < t.length; r += 1)
      Ut(
        /** @type {Effect} */
        t[r]
      );
  }
}
function kc(e) {
  for (var t = e.parent; t !== null; ) {
    if ((t.f & Ft) === 0)
      return (t.f & Dr) === 0 ? (
        /** @type {Effect} */
        t
      ) : null;
    t = t.parent;
  }
  return null;
}
function ln(e) {
  var t, r = dt;
  yr(kc(e));
  try {
    e.f &= ~ts, bi(e), t = Gi(e);
  } finally {
    yr(r);
  }
  return t;
}
function mi(e) {
  var t = ln(e);
  if (!e.equals(t) && (e.wv = Bi(), (!ut?.is_fork || e.deps === null) && (e.v = t, e.deps === null))) {
    yt(e, Rt);
    return;
  }
  qr || (cr !== null ? (fn() || ut?.is_fork) && cr.set(e, t) : cn(e));
}
let Na = /* @__PURE__ */ new Set();
const Vr = /* @__PURE__ */ new Map();
let yi = !1;
function rs(e, t) {
  var r = {
    f: 0,
    // TODO ideally we could skip this altogether, but it causes type errors
    v: e,
    reactions: null,
    equals: ci,
    rv: 0,
    wv: 0
  };
  return r;
}
// @__NO_SIDE_EFFECTS__
function oe(e, t) {
  const r = rs(e);
  return Fi(r), r;
}
// @__NO_SIDE_EFFECTS__
function dn(e, t = !1, r = !0) {
  const n = rs(e);
  return t || (n.equals = di), ys && r && mt !== null && mt.l !== null && (mt.l.s ??= []).push(n), n;
}
function x(e, t, r = !1) {
  ot !== null && // since we are untracking the function inside `$inspect.with` we need to add this check
  // to ensure we error if state is set inside an inspect effect
  (!dr || (ot.f & Mn) !== 0) && qs() && (ot.f & (Ft | Ir | on | Mn)) !== 0 && (nr === null || !fs.call(nr, e)) && oc();
  let n = r ? Bt(t) : t;
  return gs(e, n);
}
function gs(e, t) {
  if (!e.equals(t)) {
    var r = e.v;
    qr ? Vr.set(e, t) : Vr.set(e, r), e.v = t;
    var n = Pr.ensure();
    if (n.capture(e, r), (e.f & Ft) !== 0) {
      const c = (
        /** @type {Derived} */
        e
      );
      (e.f & jt) !== 0 && ln(c), cn(c);
    }
    e.wv = Bi(), wi(e, jt), qs() && dt !== null && (dt.f & Rt) !== 0 && (dt.f & (fr | Wr)) === 0 && (er === null ? Rc([e]) : er.push(e)), !n.is_fork && Na.size > 0 && !yi && $c();
  }
  return t;
}
function $c() {
  yi = !1;
  for (const e of Na)
    (e.f & Rt) !== 0 && yt(e, vr), Us(e) && Fs(e);
  Na.clear();
}
function Rs(e) {
  x(e, e.v + 1);
}
function wi(e, t) {
  var r = e.reactions;
  if (r !== null)
    for (var n = qs(), c = r.length, l = 0; l < c; l++) {
      var u = r[l], d = u.f;
      if (!(!n && u === dt)) {
        var f = (d & jt) === 0;
        if (f && yt(u, t), (d & Ft) !== 0) {
          var g = (
            /** @type {Derived} */
            u
          );
          cr?.delete(g), (d & ts) === 0 && (d & sr && (u.f |= ts), wi(g, vr));
        } else f && ((d & Ir) !== 0 && $r !== null && $r.add(
          /** @type {Effect} */
          u
        ), lr(
          /** @type {Effect} */
          u
        ));
      }
    }
}
function Bt(e) {
  if (typeof e != "object" || e === null || Br in e)
    return e;
  const t = an(e);
  if (t !== zo && t !== Yo)
    return e;
  var r = /* @__PURE__ */ new Map(), n = sn(e), c = /* @__PURE__ */ oe(0), l = Qr, u = (d) => {
    if (Qr === l)
      return d();
    var f = ot, g = Qr;
    ir(null), jn(l);
    var m = d();
    return ir(f), jn(g), m;
  };
  return n && r.set("length", /* @__PURE__ */ oe(
    /** @type {any[]} */
    e.length
  )), new Proxy(
    /** @type {any} */
    e,
    {
      defineProperty(d, f, g) {
        (!("value" in g) || g.configurable === !1 || g.enumerable === !1 || g.writable === !1) && nc();
        var m = r.get(f);
        return m === void 0 ? u(() => {
          var k = /* @__PURE__ */ oe(g.value);
          return r.set(f, k), k;
        }) : x(m, g.value, !0), !0;
      },
      deleteProperty(d, f) {
        var g = r.get(f);
        if (g === void 0) {
          if (f in d) {
            const m = u(() => /* @__PURE__ */ oe(Lt));
            r.set(f, m), Rs(c);
          }
        } else
          x(g, Lt), Rs(c);
        return !0;
      },
      get(d, f, g) {
        if (f === Br)
          return e;
        var m = r.get(f), k = f in d;
        if (m === void 0 && (!k || vs(d, f)?.writable) && (m = u(() => {
          var P = Bt(k ? d[f] : Lt), E = /* @__PURE__ */ oe(P);
          return E;
        }), r.set(f, m)), m !== void 0) {
          var y = s(m);
          return y === Lt ? void 0 : y;
        }
        return Reflect.get(d, f, g);
      },
      getOwnPropertyDescriptor(d, f) {
        var g = Reflect.getOwnPropertyDescriptor(d, f);
        if (g && "value" in g) {
          var m = r.get(f);
          m && (g.value = s(m));
        } else if (g === void 0) {
          var k = r.get(f), y = k?.v;
          if (k !== void 0 && y !== Lt)
            return {
              enumerable: !0,
              configurable: !0,
              value: y,
              writable: !0
            };
        }
        return g;
      },
      has(d, f) {
        if (f === Br)
          return !0;
        var g = r.get(f), m = g !== void 0 && g.v !== Lt || Reflect.has(d, f);
        if (g !== void 0 || dt !== null && (!m || vs(d, f)?.writable)) {
          g === void 0 && (g = u(() => {
            var y = m ? Bt(d[f]) : Lt, P = /* @__PURE__ */ oe(y);
            return P;
          }), r.set(f, g));
          var k = s(g);
          if (k === Lt)
            return !1;
        }
        return m;
      },
      set(d, f, g, m) {
        var k = r.get(f), y = f in d;
        if (n && f === "length")
          for (var P = g; P < /** @type {Source<number>} */
          k.v; P += 1) {
            var E = r.get(P + "");
            E !== void 0 ? x(E, Lt) : P in d && (E = u(() => /* @__PURE__ */ oe(Lt)), r.set(P + "", E));
          }
        if (k === void 0)
          (!y || vs(d, f)?.writable) && (k = u(() => /* @__PURE__ */ oe(void 0)), x(k, Bt(g)), r.set(f, k));
        else {
          y = k.v !== Lt;
          var Q = u(() => Bt(g));
          x(k, Q);
        }
        var S = Reflect.getOwnPropertyDescriptor(d, f);
        if (S?.set && S.set.call(m, g), !y) {
          if (n && typeof f == "string") {
            var U = (
              /** @type {Source<number>} */
              r.get("length")
            ), ae = Number(f);
            Number.isInteger(ae) && ae >= U.v && x(U, ae + 1);
          }
          Rs(c);
        }
        return !0;
      },
      ownKeys(d) {
        s(c);
        var f = Reflect.ownKeys(d).filter((k) => {
          var y = r.get(k);
          return y === void 0 || y.v !== Lt;
        });
        for (var [g, m] of r)
          m.v !== Lt && !(g in d) && f.push(g);
        return f;
      },
      setPrototypeOf() {
        ic();
      }
    }
  );
}
function Nn(e) {
  try {
    if (e !== null && typeof e == "object" && Br in e)
      return e[Br];
  } catch {
  }
  return e;
}
function Cc(e, t) {
  return Object.is(Nn(e), Nn(t));
}
var Ln, ki, $i, Ci;
function La() {
  if (Ln === void 0) {
    Ln = window, ki = /Firefox/.test(navigator.userAgent);
    var e = Element.prototype, t = Node.prototype, r = Text.prototype;
    $i = vs(t, "firstChild").get, Ci = vs(t, "nextSibling").get, Rn(e) && (e.__click = void 0, e.__className = void 0, e.__attributes = null, e.__style = void 0, e.__e = void 0), Rn(r) && (r.__t = void 0);
  }
}
function ar(e = "") {
  return document.createTextNode(e);
}
// @__NO_SIDE_EFFECTS__
function rr(e) {
  return (
    /** @type {TemplateNode | null} */
    $i.call(e)
  );
}
// @__NO_SIDE_EFFECTS__
function pr(e) {
  return (
    /** @type {TemplateNode | null} */
    Ci.call(e)
  );
}
function i(e, t) {
  if (!it)
    return /* @__PURE__ */ rr(e);
  var r = /* @__PURE__ */ rr(lt);
  if (r === null)
    r = lt.appendChild(ar());
  else if (t && r.nodeType !== va) {
    var n = ar();
    return r?.before(n), Wt(n), n;
  }
  return t && vn(
    /** @type {Text} */
    r
  ), Wt(r), r;
}
function ct(e, t = !1) {
  if (!it) {
    var r = /* @__PURE__ */ rr(e);
    return r instanceof Comment && r.data === "" ? /* @__PURE__ */ pr(r) : r;
  }
  if (t) {
    if (lt?.nodeType !== va) {
      var n = ar();
      return lt?.before(n), Wt(n), n;
    }
    vn(
      /** @type {Text} */
      lt
    );
  }
  return lt;
}
function o(e, t = 1, r = !1) {
  let n = it ? lt : e;
  for (var c; t--; )
    c = n, n = /** @type {TemplateNode} */
    /* @__PURE__ */ pr(n);
  if (!it)
    return n;
  if (r) {
    if (n?.nodeType !== va) {
      var l = ar();
      return n === null ? c?.after(l) : n.before(l), Wt(l), l;
    }
    vn(
      /** @type {Text} */
      n
    );
  }
  return Wt(n), n;
}
function un(e) {
  e.textContent = "";
}
function Si() {
  return !1;
}
function vn(e) {
  if (
    /** @type {string} */
    e.nodeValue.length < 65536
  )
    return;
  let t = e.nextSibling;
  for (; t !== null && t.nodeType === va; )
    t.remove(), e.nodeValue += /** @type {string} */
    t.nodeValue, t = e.nextSibling;
}
function Ws(e) {
  it && /* @__PURE__ */ rr(e) !== null && un(e);
}
let On = !1;
function Ei() {
  On || (On = !0, document.addEventListener(
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
function pa(e) {
  var t = ot, r = dt;
  ir(null), yr(null);
  try {
    return e();
  } finally {
    ir(t), yr(r);
  }
}
function Ai(e, t, r, n = r) {
  e.addEventListener(t, () => pa(r));
  const c = e.__on_r;
  c ? e.__on_r = () => {
    c(), n(!0);
  } : e.__on_r = () => n(!0), Ei();
}
function Di(e) {
  dt === null && (ot === null && rc(), tc()), qr && ec();
}
function Sc(e, t) {
  var r = t.last;
  r === null ? t.last = t.first = e : (r.next = e, e.prev = r, t.last = e);
}
function xr(e, t, r) {
  var n = dt;
  n !== null && (n.f & Xt) !== 0 && (e |= Xt);
  var c = {
    ctx: mt,
    deps: null,
    nodes: null,
    f: e | jt | sr,
    first: null,
    fn: t,
    last: null,
    next: null,
    parent: n,
    b: n && n.b,
    prev: null,
    teardown: null,
    wv: 0,
    ac: null
  };
  if (r)
    try {
      Fs(c), c.f |= ua;
    } catch (d) {
      throw Ut(c), d;
    }
  else t !== null && lr(c);
  var l = c;
  if (r && l.deps === null && l.teardown === null && l.nodes === null && l.first === l.last && // either `null`, or a singular child
  (l.f & ns) === 0 && (l = l.first, (e & Ir) !== 0 && (e & ps) !== 0 && l !== null && (l.f |= ps)), l !== null && (l.parent = n, n !== null && Sc(l, n), ot !== null && (ot.f & Ft) !== 0 && (e & Wr) === 0)) {
    var u = (
      /** @type {Derived} */
      ot
    );
    (u.effects ??= []).push(l);
  }
  return c;
}
function fn() {
  return ot !== null && !dr;
}
function xa(e) {
  const t = xr(Vs, null, !1);
  return yt(t, Rt), t.teardown = e, t;
}
function Tt(e) {
  Di();
  var t = (
    /** @type {Effect} */
    dt.f
  ), r = !ot && (t & fr) !== 0 && (t & ua) === 0;
  if (r) {
    var n = (
      /** @type {ComponentContext} */
      mt
    );
    (n.e ??= []).push(e);
  } else
    return Pi(e);
}
function Pi(e) {
  return xr(aa | ni, e, !1);
}
function Ec(e) {
  return Di(), xr(Vs | ni, e, !0);
}
function Ac(e) {
  Pr.ensure();
  const t = xr(Wr | ns, e, !0);
  return () => {
    Ut(t);
  };
}
function Dc(e) {
  Pr.ensure();
  const t = xr(Wr | ns, e, !0);
  return (r = {}) => new Promise((n) => {
    r.outro ? Jr(t, () => {
      Ut(t), n(void 0);
    }) : (Ut(t), n(void 0));
  });
}
function pn(e) {
  return xr(aa, e, !1);
}
function Pc(e) {
  return xr(on | ns, e, !0);
}
function ha(e, t = 0) {
  return xr(Vs | t, e, !0);
}
function $(e, t = [], r = [], n = []) {
  mc(n, t, r, (c) => {
    xr(Vs, () => e(...c.map(s)), !0);
  });
}
function xn(e, t = 0) {
  var r = xr(Ir | t, e, !0);
  return r;
}
function tr(e) {
  return xr(fr | ns, e, !0);
}
function Ti(e) {
  var t = e.teardown;
  if (t !== null) {
    const r = qr, n = ot;
    Fn(!0), ir(null);
    try {
      t.call(null);
    } finally {
      Fn(r), ir(n);
    }
  }
}
function Ri(e, t = !1) {
  var r = e.first;
  for (e.first = e.last = null; r !== null; ) {
    const c = r.ac;
    c !== null && pa(() => {
      c.abort(ds);
    });
    var n = r.next;
    (r.f & Wr) !== 0 ? r.parent = null : Ut(r, t), r = n;
  }
}
function Tc(e) {
  for (var t = e.first; t !== null; ) {
    var r = t.next;
    (t.f & fr) === 0 && Ut(t), t = r;
  }
}
function Ut(e, t = !0) {
  var r = !1;
  (t || (e.f & ai) !== 0) && e.nodes !== null && e.nodes.end !== null && (Mi(
    e.nodes.start,
    /** @type {TemplateNode} */
    e.nodes.end
  ), r = !0), Ri(e, t && !r), oa(e, 0), yt(e, Dr);
  var n = e.nodes && e.nodes.t;
  if (n !== null)
    for (const l of n)
      l.stop();
  Ti(e);
  var c = e.parent;
  c !== null && c.first !== null && Ii(e), e.next = e.prev = e.teardown = e.ctx = e.deps = e.fn = e.nodes = e.ac = null;
}
function Mi(e, t) {
  for (; e !== null; ) {
    var r = e === t ? null : /* @__PURE__ */ pr(e);
    e.remove(), e = r;
  }
}
function Ii(e) {
  var t = e.parent, r = e.prev, n = e.next;
  r !== null && (r.next = n), n !== null && (n.prev = r), t !== null && (t.first === e && (t.first = n), t.last === e && (t.last = r));
}
function Jr(e, t, r = !0) {
  var n = [];
  Ni(e, n, !0);
  var c = () => {
    r && Ut(e), t && t();
  }, l = n.length;
  if (l > 0) {
    var u = () => --l || c();
    for (var d of n)
      d.out(u);
  } else
    c();
}
function Ni(e, t, r) {
  if ((e.f & Xt) === 0) {
    e.f ^= Xt;
    var n = e.nodes && e.nodes.t;
    if (n !== null)
      for (const d of n)
        (d.is_global || r) && t.push(d);
    for (var c = e.first; c !== null; ) {
      var l = c.next, u = (c.f & ps) !== 0 || // If this is a branch effect without a block effect parent,
      // it means the parent block effect was pruned. In that case,
      // transparency information was transferred to the branch effect.
      (c.f & fr) !== 0 && (e.f & Ir) !== 0;
      Ni(c, t, u ? r : !1), c = l;
    }
  }
}
function hn(e) {
  Li(e, !0);
}
function Li(e, t) {
  if ((e.f & Xt) !== 0) {
    e.f ^= Xt, (e.f & Rt) === 0 && (yt(e, jt), lr(e));
    for (var r = e.first; r !== null; ) {
      var n = r.next, c = (r.f & ps) !== 0 || (r.f & fr) !== 0;
      Li(r, c ? t : !1), r = n;
    }
    var l = e.nodes && e.nodes.t;
    if (l !== null)
      for (const u of l)
        (u.is_global || t) && u.in();
  }
}
function Oi(e, t) {
  if (e.nodes)
    for (var r = e.nodes.start, n = e.nodes.end; r !== null; ) {
      var c = r === n ? null : /* @__PURE__ */ pr(r);
      t.append(r), r = c;
    }
}
let Ks = !1, qr = !1;
function Fn(e) {
  qr = e;
}
let ot = null, dr = !1;
function ir(e) {
  ot = e;
}
let dt = null;
function yr(e) {
  dt = e;
}
let nr = null;
function Fi(e) {
  ot !== null && (nr === null ? nr = [e] : nr.push(e));
}
let Kt = null, Jt = 0, er = null;
function Rc(e) {
  er = e;
}
let ji = 1, Yr = 0, Qr = Yr;
function jn(e) {
  Qr = e;
}
function Bi() {
  return ++ji;
}
function Us(e) {
  var t = e.f;
  if ((t & jt) !== 0)
    return !0;
  if (t & Ft && (e.f &= ~ts), (t & vr) !== 0) {
    for (var r = (
      /** @type {Value[]} */
      e.deps
    ), n = r.length, c = 0; c < n; c++) {
      var l = r[c];
      if (Us(
        /** @type {Derived} */
        l
      ) && mi(
        /** @type {Derived} */
        l
      ), l.wv > e.wv)
        return !0;
    }
    (t & sr) !== 0 && // During time traveling we don't want to reset the status so that
    // traversal of the graph in the other batches still happens
    cr === null && yt(e, Rt);
  }
  return !1;
}
function Vi(e, t, r = !0) {
  var n = e.reactions;
  if (n !== null && !(nr !== null && fs.call(nr, e)))
    for (var c = 0; c < n.length; c++) {
      var l = n[c];
      (l.f & Ft) !== 0 ? Vi(
        /** @type {Derived} */
        l,
        t,
        !1
      ) : t === l && (r ? yt(l, jt) : (l.f & Rt) !== 0 && yt(l, vr), lr(
        /** @type {Effect} */
        l
      ));
    }
}
function Gi(e) {
  var t = Kt, r = Jt, n = er, c = ot, l = nr, u = mt, d = dr, f = Qr, g = e.f;
  Kt = /** @type {null | Value[]} */
  null, Jt = 0, er = null, ot = (g & (fr | Wr)) === 0 ? e : null, nr = null, hs(e.ctx), dr = !1, Qr = ++Yr, e.ac !== null && (pa(() => {
    e.ac.abort(ds);
  }), e.ac = null);
  try {
    e.f |= Ra;
    var m = (
      /** @type {Function} */
      e.fn
    ), k = m(), y = e.deps, P = ut?.is_fork;
    if (Kt !== null) {
      var E;
      if (P || oa(e, Jt), y !== null && Jt > 0)
        for (y.length = Jt + Kt.length, E = 0; E < Kt.length; E++)
          y[Jt + E] = Kt[E];
      else
        e.deps = y = Kt;
      if (fn() && (e.f & sr) !== 0)
        for (E = Jt; E < y.length; E++)
          (y[E].reactions ??= []).push(e);
    } else !P && y !== null && Jt < y.length && (oa(e, Jt), y.length = Jt);
    if (qs() && er !== null && !dr && y !== null && (e.f & (Ft | vr | jt)) === 0)
      for (E = 0; E < /** @type {Source[]} */
      er.length; E++)
        Vi(
          er[E],
          /** @type {Effect} */
          e
        );
    if (c !== null && c !== e) {
      if (Yr++, c.deps !== null)
        for (let Q = 0; Q < r; Q += 1)
          c.deps[Q].rv = Yr;
      if (t !== null)
        for (const Q of t)
          Q.rv = Yr;
      er !== null && (n === null ? n = er : n.push(.../** @type {Source[]} */
      er));
    }
    return (e.f & jr) !== 0 && (e.f ^= jr), k;
  } catch (Q) {
    return vi(Q);
  } finally {
    e.f ^= Ra, Kt = t, Jt = r, er = n, ot = c, nr = l, hs(u), dr = d, Qr = f;
  }
}
function Mc(e, t) {
  let r = t.reactions;
  if (r !== null) {
    var n = Uo.call(r, e);
    if (n !== -1) {
      var c = r.length - 1;
      c === 0 ? r = t.reactions = null : (r[n] = r[c], r.pop());
    }
  }
  if (r === null && (t.f & Ft) !== 0 && // Destroying a child effect while updating a parent effect can cause a dependency to appear
  // to be unused, when in fact it is used by the currently-updating parent. Checking `new_deps`
  // allows us to skip the expensive work of disconnecting and immediately reconnecting it
  (Kt === null || !fs.call(Kt, t))) {
    var l = (
      /** @type {Derived} */
      t
    );
    (l.f & sr) !== 0 && (l.f ^= sr, l.f &= ~ts), cn(l), bi(l), oa(l, 0);
  }
}
function oa(e, t) {
  var r = e.deps;
  if (r !== null)
    for (var n = t; n < r.length; n++)
      Mc(e, r[n]);
}
function Fs(e) {
  var t = e.f;
  if ((t & Dr) === 0) {
    yt(e, Rt);
    var r = dt, n = Ks;
    dt = e, Ks = !0;
    try {
      (t & (Ir | si)) !== 0 ? Tc(e) : Ri(e), Ti(e);
      var c = Gi(e);
      e.teardown = typeof c == "function" ? c : null, e.wv = ji;
      var l;
      Pa && uc && (e.f & jt) !== 0 && e.deps;
    } finally {
      Ks = n, dt = r;
    }
  }
}
async function _n() {
  await Promise.resolve(), vt();
}
function s(e) {
  var t = e.f, r = (t & Ft) !== 0;
  if (ot !== null && !dr) {
    var n = dt !== null && (dt.f & Dr) !== 0;
    if (!n && (nr === null || !fs.call(nr, e))) {
      var c = ot.deps;
      if ((ot.f & Ra) !== 0)
        e.rv < Yr && (e.rv = Yr, Kt === null && c !== null && c[Jt] === e ? Jt++ : Kt === null ? Kt = [e] : Kt.push(e));
      else {
        (ot.deps ??= []).push(e);
        var l = e.reactions;
        l === null ? e.reactions = [ot] : fs.call(l, ot) || l.push(ot);
      }
    }
  }
  if (qr && Vr.has(e))
    return Vr.get(e);
  if (r) {
    var u = (
      /** @type {Derived} */
      e
    );
    if (qr) {
      var d = u.v;
      return ((u.f & Rt) === 0 && u.reactions !== null || Hi(u)) && (d = ln(u)), Vr.set(u, d), d;
    }
    var f = (u.f & sr) === 0 && !dr && ot !== null && (Ks || (ot.f & sr) !== 0), g = u.deps === null;
    Us(u) && (f && (u.f |= sr), mi(u)), f && !g && qi(u);
  }
  if (cr?.has(e))
    return cr.get(e);
  if ((e.f & jr) !== 0)
    throw e.v;
  return e.v;
}
function qi(e) {
  if (e.deps !== null) {
    e.f |= sr;
    for (const t of e.deps)
      (t.reactions ??= []).push(e), (t.f & Ft) !== 0 && (t.f & sr) === 0 && qi(
        /** @type {Derived} */
        t
      );
  }
}
function Hi(e) {
  if (e.v === Lt) return !0;
  if (e.deps === null) return !1;
  for (const t of e.deps)
    if (Vr.has(t) || (t.f & Ft) !== 0 && Hi(
      /** @type {Derived} */
      t
    ))
      return !0;
  return !1;
}
function os(e) {
  var t = dr;
  try {
    return dr = !0, e();
  } finally {
    dr = t;
  }
}
function Ic(e) {
  if (!(typeof e != "object" || !e || e instanceof EventTarget)) {
    if (Br in e)
      Oa(e);
    else if (!Array.isArray(e))
      for (let t in e) {
        const r = e[t];
        typeof r == "object" && r && Br in r && Oa(r);
      }
  }
}
function Oa(e, t = /* @__PURE__ */ new Set()) {
  if (typeof e == "object" && e !== null && // We don't want to traverse DOM elements
  !(e instanceof EventTarget) && !t.has(e)) {
    t.add(e), e instanceof Date && e.getTime();
    for (let n in e)
      try {
        Oa(e[n], t);
      } catch {
      }
    const r = an(e);
    if (r !== Object.prototype && r !== Array.prototype && r !== Map.prototype && r !== Set.prototype && r !== Date.prototype) {
      const n = ti(r);
      for (let c in n) {
        const l = n[c].get;
        if (l)
          try {
            l.call(e);
          } catch {
          }
      }
    }
  }
}
const Wi = /* @__PURE__ */ new Set(), Fa = /* @__PURE__ */ new Set();
function Nc(e, t, r, n = {}) {
  function c(l) {
    if (n.capture || Ds.call(t, l), !l.cancelBubble)
      return pa(() => r?.call(this, l));
  }
  return e.startsWith("pointer") || e.startsWith("touch") || e === "wheel" ? mr(() => {
    t.addEventListener(e, c, n);
  }) : t.addEventListener(e, c, n), c;
}
function Nt(e, t, r, n, c) {
  var l = { capture: n, passive: c }, u = Nc(e, t, r, l);
  (t === document.body || // @ts-ignore
  t === window || // @ts-ignore
  t === document || // Firefox has quirky behavior, it can happen that we still get "canplay" events when the element is already removed
  t instanceof HTMLMediaElement) && xa(() => {
    t.removeEventListener(e, u, l);
  });
}
function Mt(e) {
  for (var t = 0; t < e.length; t++)
    Wi.add(e[t]);
  for (var r of Fa)
    r(e);
}
let Bn = null;
function Ds(e) {
  var t = this, r = (
    /** @type {Node} */
    t.ownerDocument
  ), n = e.type, c = e.composedPath?.() || [], l = (
    /** @type {null | Element} */
    c[0] || e.target
  );
  Bn = e;
  var u = 0, d = Bn === e && e.__root;
  if (d) {
    var f = c.indexOf(d);
    if (f !== -1 && (t === document || t === /** @type {any} */
    window)) {
      e.__root = t;
      return;
    }
    var g = c.indexOf(t);
    if (g === -1)
      return;
    f <= g && (u = f);
  }
  if (l = /** @type {Element} */
  c[u] || e.target, l !== t) {
    Os(e, "currentTarget", {
      configurable: !0,
      get() {
        return l || r;
      }
    });
    var m = ot, k = dt;
    ir(null), yr(null);
    try {
      for (var y, P = []; l !== null; ) {
        var E = l.assignedSlot || l.parentNode || /** @type {any} */
        l.host || null;
        try {
          var Q = l["__" + n];
          Q != null && (!/** @type {any} */
          l.disabled || // DOM could've been updated already by the time this is reached, so we check this as well
          // -> the target could not have been disabled because it emits the event in the first place
          e.target === l) && Q.call(l, e);
        } catch (S) {
          y ? P.push(S) : y = S;
        }
        if (e.cancelBubble || E === t || E === null)
          break;
        l = E;
      }
      if (y) {
        for (let S of P)
          queueMicrotask(() => {
            throw S;
          });
        throw y;
      }
    } finally {
      e.__root = t, delete e.currentTarget, ir(m), yr(k);
    }
  }
}
function Ui(e) {
  var t = document.createElement("template");
  return t.innerHTML = e.replaceAll("<!>", "<!---->"), t.content;
}
function Gr(e, t) {
  var r = (
    /** @type {Effect} */
    dt
  );
  r.nodes === null && (r.nodes = { start: e, end: t, a: null, t: null });
}
// @__NO_SIDE_EFFECTS__
function p(e, t) {
  var r = (t & qo) !== 0, n = (t & Ho) !== 0, c, l = !e.startsWith("<!>");
  return () => {
    if (it)
      return Gr(lt, null), lt;
    c === void 0 && (c = Ui(l ? e : "<!>" + e), r || (c = /** @type {TemplateNode} */
    /* @__PURE__ */ rr(c)));
    var u = (
      /** @type {TemplateNode} */
      n || ki ? document.importNode(c, !0) : c.cloneNode(!0)
    );
    if (r) {
      var d = (
        /** @type {TemplateNode} */
        /* @__PURE__ */ rr(u)
      ), f = (
        /** @type {TemplateNode} */
        u.lastChild
      );
      Gr(d, f);
    } else
      Gr(u, u);
    return u;
  };
}
function ur() {
  if (it)
    return Gr(lt, null), lt;
  var e = document.createDocumentFragment(), t = document.createComment(""), r = ar();
  return e.append(t, r), Gr(t, r), e;
}
function v(e, t) {
  if (it) {
    var r = (
      /** @type {Effect & { nodes: EffectNodes }} */
      dt
    );
    ((r.f & ua) === 0 || r.nodes.end === null) && (r.nodes.end = lt), xs();
    return;
  }
  e !== null && e.before(
    /** @type {Node} */
    t
  );
}
const Lc = ["touchstart", "touchmove"];
function Oc(e) {
  return Lc.includes(e);
}
function h(e, t) {
  var r = t == null ? "" : typeof t == "object" ? t + "" : t;
  r !== (e.__t ??= e.nodeValue) && (e.__t = r, e.nodeValue = r + "");
}
function zi(e, t) {
  return Yi(e, t);
}
function Fc(e, t) {
  La(), t.intro = t.intro ?? !1;
  const r = t.target, n = it, c = lt;
  try {
    for (var l = /* @__PURE__ */ rr(r); l && (l.nodeType !== is || /** @type {Comment} */
    l.data !== tn); )
      l = /* @__PURE__ */ pr(l);
    if (!l)
      throw es;
    Er(!0), Wt(
      /** @type {Comment} */
      l
    );
    const u = Yi(e, { ...t, anchor: l });
    return Er(!1), /**  @type {Exports} */
    u;
  } catch (u) {
    if (u instanceof Error && u.message.split(`
`).some((d) => d.startsWith("https://svelte.dev/e/")))
      throw u;
    return u !== es && console.warn("Failed to hydrate: ", u), t.recover === !1 && ac(), La(), un(r), Er(!1), zi(e, t);
  } finally {
    Er(n), Wt(c);
  }
}
const cs = /* @__PURE__ */ new Map();
function Yi(e, { target: t, anchor: r, props: n = {}, events: c, context: l, intro: u = !0 }) {
  La();
  var d = /* @__PURE__ */ new Set(), f = (k) => {
    for (var y = 0; y < k.length; y++) {
      var P = k[y];
      if (!d.has(P)) {
        d.add(P);
        var E = Oc(P);
        t.addEventListener(P, Ds, { passive: E });
        var Q = cs.get(P);
        Q === void 0 ? (document.addEventListener(P, Ds, { passive: E }), cs.set(P, 1)) : cs.set(P, Q + 1);
      }
    }
  };
  f(da(Wi)), Fa.add(f);
  var g = void 0, m = Dc(() => {
    var k = r ?? t.appendChild(ar());
    return gc(
      /** @type {TemplateNode} */
      k,
      {
        pending: () => {
        }
      },
      (y) => {
        kt({});
        var P = (
          /** @type {ComponentContext} */
          mt
        );
        if (l && (P.c = l), c && (n.$$events = c), it && Gr(
          /** @type {TemplateNode} */
          y,
          null
        ), g = e(y, n) || {}, it && (dt.nodes.end = lt, lt === null || lt.nodeType !== is || /** @type {Comment} */
        lt.data !== rn))
          throw Gs(), es;
        $t();
      }
    ), () => {
      for (var y of d) {
        t.removeEventListener(y, Ds);
        var P = (
          /** @type {number} */
          cs.get(y)
        );
        --P === 0 ? (document.removeEventListener(y, Ds), cs.delete(y)) : cs.set(y, P);
      }
      Fa.delete(f), k !== r && k.parentNode?.removeChild(k);
    };
  });
  return ja.set(g, m), g;
}
let ja = /* @__PURE__ */ new WeakMap();
function jc(e, t) {
  const r = ja.get(e);
  return r ? (ja.delete(e), r(t)) : Promise.resolve();
}
class Bc {
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
      ut
    );
    if (this.#e.has(t)) {
      var r = (
        /** @type {Key} */
        this.#e.get(t)
      ), n = this.#t.get(r);
      if (n)
        hn(n), this.#i.delete(r);
      else {
        var c = this.#r.get(r);
        c && (this.#t.set(r, c.effect), this.#r.delete(r), c.fragment.lastChild.remove(), this.anchor.before(c.fragment), n = c.effect);
      }
      for (const [l, u] of this.#e) {
        if (this.#e.delete(l), l === t)
          break;
        const d = this.#r.get(u);
        d && (Ut(d.effect), this.#r.delete(u));
      }
      for (const [l, u] of this.#t) {
        if (l === r || this.#i.has(l)) continue;
        const d = () => {
          if (Array.from(this.#e.values()).includes(l)) {
            var g = document.createDocumentFragment();
            Oi(u, g), g.append(ar()), this.#r.set(l, { effect: u, fragment: g });
          } else
            Ut(u);
          this.#i.delete(l), this.#t.delete(l);
        };
        this.#o || !n ? (this.#i.add(l), Jr(u, d, !1)) : d();
      }
    }
  };
  /**
   * @param {Batch} batch
   */
  #s = (t) => {
    this.#e.delete(t);
    const r = Array.from(this.#e.values());
    for (const [n, c] of this.#r)
      r.includes(n) || (Ut(c.effect), this.#r.delete(n));
  };
  /**
   *
   * @param {any} key
   * @param {null | ((target: TemplateNode) => void)} fn
   */
  ensure(t, r) {
    var n = (
      /** @type {Batch} */
      ut
    ), c = Si();
    if (r && !this.#t.has(t) && !this.#r.has(t))
      if (c) {
        var l = document.createDocumentFragment(), u = ar();
        l.append(u), this.#r.set(t, {
          effect: tr(() => r(u)),
          fragment: l
        });
      } else
        this.#t.set(
          t,
          tr(() => r(this.anchor))
        );
    if (this.#e.set(n, t), c) {
      for (const [d, f] of this.#t)
        d === t ? n.unskip_effect(f) : n.skip_effect(f);
      for (const [d, f] of this.#r)
        d === t ? n.unskip_effect(f.effect) : n.skip_effect(f.effect);
      n.oncommit(this.#a), n.ondiscard(this.#s);
    } else
      it && (this.anchor = lt), this.#a();
  }
}
function Ki(e) {
  mt === null && ii(), ys && mt.l !== null ? Gc(mt).m.push(e) : Tt(() => {
    const t = os(e);
    if (typeof t == "function") return (
      /** @type {() => void} */
      t
    );
  });
}
function Vc(e) {
  mt === null && ii(), Ki(() => () => os(e));
}
function Gc(e) {
  var t = (
    /** @type {ComponentContextLegacy} */
    e.l
  );
  return t.u ??= { a: [], b: [], m: [] };
}
function A(e, t, r = !1) {
  it && xs();
  var n = new Bc(e), c = r ? ps : 0;
  function l(u, d) {
    if (it) {
      const m = oi(e);
      var f;
      if (m === tn ? f = 0 : m === la ? f = !1 : f = parseInt(m.substring(1)), u !== f) {
        var g = na();
        Wt(g), n.anchor = g, Er(!1), n.ensure(u, d), Er(!0);
        return;
      }
    }
    n.ensure(u, d);
  }
  xn(() => {
    var u = !1;
    t((d, f = 0) => {
      u = !0, l(f, d);
    }), u || l(!1, null);
  }, c);
}
function ft(e, t) {
  return t;
}
function qc(e, t, r) {
  for (var n = [], c = t.length, l, u = t.length, d = 0; d < c; d++) {
    let k = t[d];
    Jr(
      k,
      () => {
        if (l) {
          if (l.pending.delete(k), l.done.add(k), l.pending.size === 0) {
            var y = (
              /** @type {Set<EachOutroGroup>} */
              e.outrogroups
            );
            Ba(da(l.done)), y.delete(l), y.size === 0 && (e.outrogroups = null);
          }
        } else
          u -= 1;
      },
      !1
    );
  }
  if (u === 0) {
    var f = n.length === 0 && r !== null;
    if (f) {
      var g = (
        /** @type {Element} */
        r
      ), m = (
        /** @type {Element} */
        g.parentNode
      );
      un(m), m.append(g), e.items.clear();
    }
    Ba(t, !f);
  } else
    l = {
      pending: new Set(t),
      done: /* @__PURE__ */ new Set()
    }, (e.outrogroups ??= /* @__PURE__ */ new Set()).add(l);
}
function Ba(e, t = !0) {
  for (var r = 0; r < e.length; r++)
    Ut(e[r], t);
}
var Vn;
function He(e, t, r, n, c, l = null) {
  var u = e, d = /* @__PURE__ */ new Map(), f = (t & ei) !== 0;
  if (f) {
    var g = (
      /** @type {Element} */
      e
    );
    u = it ? Wt(/* @__PURE__ */ rr(g)) : g.appendChild(ar());
  }
  it && xs();
  var m = null, k = /* @__PURE__ */ us(() => {
    var U = r();
    return sn(U) ? U : U == null ? [] : da(U);
  }), y, P = !0;
  function E() {
    S.fallback = m, Hc(S, y, u, t, n), m !== null && (y.length === 0 ? (m.f & Sr) === 0 ? hn(m) : (m.f ^= Sr, Ps(m, null, u)) : Jr(m, () => {
      m = null;
    }));
  }
  var Q = xn(() => {
    y = /** @type {V[]} */
    s(k);
    var U = y.length;
    let ae = !1;
    if (it) {
      var we = oi(u) === la;
      we !== (U === 0) && (u = na(), Wt(u), Er(!1), ae = !0);
    }
    for (var le = /* @__PURE__ */ new Set(), ve = (
      /** @type {Batch} */
      ut
    ), de = Si(), Le = 0; Le < U; Le += 1) {
      it && lt.nodeType === is && /** @type {Comment} */
      lt.data === rn && (u = /** @type {Comment} */
      lt, ae = !0, Er(!1));
      var Ae = y[Le], De = n(Ae, Le), te = P ? null : d.get(De);
      te ? (te.v && gs(te.v, Ae), te.i && gs(te.i, Le), de && ve.unskip_effect(te.e)) : (te = Wc(
        d,
        P ? u : Vn ??= ar(),
        Ae,
        De,
        Le,
        c,
        t,
        r
      ), P || (te.e.f |= Sr), d.set(De, te)), le.add(De);
    }
    if (U === 0 && l && !m && (P ? m = tr(() => l(u)) : (m = tr(() => l(Vn ??= ar())), m.f |= Sr)), U > le.size && Zo(), it && U > 0 && Wt(na()), !P)
      if (de) {
        for (const [N, X] of d)
          le.has(N) || ve.skip_effect(X.e);
        ve.oncommit(E), ve.ondiscard(() => {
        });
      } else
        E();
    ae && Er(!0), s(k);
  }), S = { effect: Q, items: d, outrogroups: null, fallback: m };
  P = !1, it && (u = lt);
}
function Ss(e) {
  for (; e !== null && (e.f & fr) === 0; )
    e = e.next;
  return e;
}
function Hc(e, t, r, n, c) {
  var l = (n & Fo) !== 0, u = t.length, d = e.items, f = Ss(e.effect.first), g, m = null, k, y = [], P = [], E, Q, S, U;
  if (l)
    for (U = 0; U < u; U += 1)
      E = t[U], Q = c(E, U), S = /** @type {EachItem} */
      d.get(Q).e, (S.f & Sr) === 0 && (S.nodes?.a?.measure(), (k ??= /* @__PURE__ */ new Set()).add(S));
  for (U = 0; U < u; U += 1) {
    if (E = t[U], Q = c(E, U), S = /** @type {EachItem} */
    d.get(Q).e, e.outrogroups !== null)
      for (const te of e.outrogroups)
        te.pending.delete(S), te.done.delete(S);
    if ((S.f & Sr) !== 0)
      if (S.f ^= Sr, S === f)
        Ps(S, null, r);
      else {
        var ae = m ? m.next : f;
        S === e.effect.last && (e.effect.last = S.prev), S.prev && (S.prev.next = S.next), S.next && (S.next.prev = S.prev), Lr(e, m, S), Lr(e, S, ae), Ps(S, ae, r), m = S, y = [], P = [], f = Ss(m.next);
        continue;
      }
    if ((S.f & Xt) !== 0 && (hn(S), l && (S.nodes?.a?.unfix(), (k ??= /* @__PURE__ */ new Set()).delete(S))), S !== f) {
      if (g !== void 0 && g.has(S)) {
        if (y.length < P.length) {
          var we = P[0], le;
          m = we.prev;
          var ve = y[0], de = y[y.length - 1];
          for (le = 0; le < y.length; le += 1)
            Ps(y[le], we, r);
          for (le = 0; le < P.length; le += 1)
            g.delete(P[le]);
          Lr(e, ve.prev, de.next), Lr(e, m, ve), Lr(e, de, we), f = we, m = de, U -= 1, y = [], P = [];
        } else
          g.delete(S), Ps(S, f, r), Lr(e, S.prev, S.next), Lr(e, S, m === null ? e.effect.first : m.next), Lr(e, m, S), m = S;
        continue;
      }
      for (y = [], P = []; f !== null && f !== S; )
        (g ??= /* @__PURE__ */ new Set()).add(f), P.push(f), f = Ss(f.next);
      if (f === null)
        continue;
    }
    (S.f & Sr) === 0 && y.push(S), m = S, f = Ss(S.next);
  }
  if (e.outrogroups !== null) {
    for (const te of e.outrogroups)
      te.pending.size === 0 && (Ba(da(te.done)), e.outrogroups?.delete(te));
    e.outrogroups.size === 0 && (e.outrogroups = null);
  }
  if (f !== null || g !== void 0) {
    var Le = [];
    if (g !== void 0)
      for (S of g)
        (S.f & Xt) === 0 && Le.push(S);
    for (; f !== null; )
      (f.f & Xt) === 0 && f !== e.fallback && Le.push(f), f = Ss(f.next);
    var Ae = Le.length;
    if (Ae > 0) {
      var De = (n & ei) !== 0 && u === 0 ? r : null;
      if (l) {
        for (U = 0; U < Ae; U += 1)
          Le[U].nodes?.a?.measure();
        for (U = 0; U < Ae; U += 1)
          Le[U].nodes?.a?.fix();
      }
      qc(e, Le, De);
    }
  }
  l && mr(() => {
    if (k !== void 0)
      for (S of k)
        S.nodes?.a?.apply();
  });
}
function Wc(e, t, r, n, c, l, u, d) {
  var f = (u & Lo) !== 0 ? (u & jo) === 0 ? /* @__PURE__ */ dn(r, !1, !1) : rs(r) : null, g = (u & Oo) !== 0 ? rs(c) : null;
  return {
    v: f,
    i: g,
    e: tr(() => (l(t, f ?? r, g ?? c, d), () => {
      e.delete(n);
    }))
  };
}
function Ps(e, t, r) {
  if (e.nodes)
    for (var n = e.nodes.start, c = e.nodes.end, l = t && (t.f & Sr) === 0 ? (
      /** @type {EffectNodes} */
      t.nodes.start
    ) : r; n !== null; ) {
      var u = (
        /** @type {TemplateNode} */
        /* @__PURE__ */ pr(n)
      );
      if (l.before(n), n === c)
        return;
      n = u;
    }
}
function Lr(e, t, r) {
  t === null ? e.effect.first = r : t.next = r, r === null ? e.effect.last = t : r.prev = t;
}
function Uc(e, t, r = !1, n = !1, c = !1) {
  var l = e, u = "";
  $(() => {
    var d = (
      /** @type {Effect} */
      dt
    );
    if (u === (u = t() ?? "")) {
      it && xs();
      return;
    }
    if (d.nodes !== null && (Mi(
      d.nodes.start,
      /** @type {TemplateNode} */
      d.nodes.end
    ), d.nodes = null), u !== "") {
      if (it) {
        lt.data;
        for (var f = xs(), g = f; f !== null && (f.nodeType !== is || /** @type {Comment} */
        f.data !== ""); )
          g = f, f = /* @__PURE__ */ pr(f);
        if (f === null)
          throw Gs(), es;
        Gr(lt, g), l = Wt(f);
        return;
      }
      var m = u + "";
      r ? m = `<svg>${m}</svg>` : n && (m = `<math>${m}</math>`);
      var k = Ui(m);
      if ((r || n) && (k = /** @type {Element} */
      /* @__PURE__ */ rr(k)), Gr(
        /** @type {TemplateNode} */
        /* @__PURE__ */ rr(k),
        /** @type {TemplateNode} */
        k.lastChild
      ), r || n)
        for (; /* @__PURE__ */ rr(k); )
          l.before(
            /** @type {TemplateNode} */
            /* @__PURE__ */ rr(k)
          );
      else
        l.before(k);
    }
  });
}
function Ji(e, t) {
  pn(() => {
    var r = e.getRootNode(), n = (
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
    if (!n.querySelector("#" + t.hash)) {
      const c = document.createElement("style");
      c.id = t.hash, c.textContent = t.code, n.appendChild(c);
    }
  });
}
function zc(e, t, r) {
  var n = e == null ? "" : "" + e;
  return t && (n = n ? n + " " + t : t), n === "" ? null : n;
}
function Yc(e, t) {
  return e == null ? null : String(e);
}
function Ie(e, t, r, n, c, l) {
  var u = e.__className;
  if (it || u !== r || u === void 0) {
    var d = zc(r, n);
    (!it || d !== e.getAttribute("class")) && (d == null ? e.removeAttribute("class") : e.className = d), e.__className = r;
  }
  return l;
}
function gr(e, t, r, n) {
  var c = e.__style;
  if (it || c !== t) {
    var l = Yc(t);
    (!it || l !== e.getAttribute("style")) && (l == null ? e.removeAttribute("style") : e.style.cssText = l), e.__style = t;
  }
  return n;
}
function Qi(e, t, r = !1) {
  if (e.multiple) {
    if (t == null)
      return;
    if (!sn(t))
      return lc();
    for (var n of e.options)
      n.selected = t.includes(Ms(n));
    return;
  }
  for (n of e.options) {
    var c = Ms(n);
    if (Cc(c, t)) {
      n.selected = !0;
      return;
    }
  }
  (!r || t !== void 0) && (e.selectedIndex = -1);
}
function Kc(e) {
  var t = new MutationObserver(() => {
    Qi(e, e.__value);
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
  }), xa(() => {
    t.disconnect();
  });
}
function js(e, t, r = t) {
  var n = /* @__PURE__ */ new WeakSet(), c = !0;
  Ai(e, "change", (l) => {
    var u = l ? "[selected]" : ":checked", d;
    if (e.multiple)
      d = [].map.call(e.querySelectorAll(u), Ms);
    else {
      var f = e.querySelector(u) ?? // will fall back to first non-disabled option if no option is selected
      e.querySelector("option:not([disabled])");
      d = f && Ms(f);
    }
    r(d), ut !== null && n.add(ut);
  }), pn(() => {
    var l = t();
    if (e === document.activeElement) {
      var u = (
        /** @type {Batch} */
        ia ?? ut
      );
      if (n.has(u))
        return;
    }
    if (Qi(e, l, c), c && l === void 0) {
      var d = e.querySelector(":checked");
      d !== null && (l = Ms(d), r(l));
    }
    e.__value = l, c = !1;
  }), Kc(e);
}
function Ms(e) {
  return "__value" in e ? e.__value : e.value;
}
const Jc = /* @__PURE__ */ Symbol("is custom element"), Qc = /* @__PURE__ */ Symbol("is html");
function bt(e) {
  if (it) {
    var t = !1, r = () => {
      if (!t) {
        if (t = !0, e.hasAttribute("value")) {
          var n = e.value;
          Ot(e, "value", null), e.value = n;
        }
        if (e.hasAttribute("checked")) {
          var c = e.checked;
          Ot(e, "checked", null), e.checked = c;
        }
      }
    };
    e.__on_r = r, mr(r), Ei();
  }
}
function Ot(e, t, r, n) {
  var c = Xc(e);
  it && (c[t] = e.getAttribute(t), t === "src" || t === "srcset" || t === "href" && e.nodeName === "LINK") || c[t] !== (c[t] = r) && (t === "loading" && (e[Qo] = r), r == null ? e.removeAttribute(t) : typeof r != "string" && Zc(e).includes(t) ? e[t] = r : e.setAttribute(t, r));
}
function Xc(e) {
  return (
    /** @type {Record<string | symbol, unknown>} **/
    // @ts-expect-error
    e.__attributes ??= {
      [Jc]: e.nodeName.includes("-"),
      [Qc]: e.namespaceURI === Wo
    }
  );
}
var Gn = /* @__PURE__ */ new Map();
function Zc(e) {
  var t = e.getAttribute("is") || e.nodeName, r = Gn.get(t);
  if (r) return r;
  Gn.set(t, r = []);
  for (var n, c = e, l = Element.prototype; l !== c; ) {
    n = ti(c);
    for (var u in n)
      n[u].set && r.push(u);
    c = an(c);
  }
  return r;
}
function xt(e, t, r = t) {
  var n = /* @__PURE__ */ new WeakSet();
  Ai(e, "input", async (c) => {
    var l = c ? e.defaultValue : e.value;
    if (l = Ea(e) ? Aa(l) : l, r(l), ut !== null && n.add(ut), await _n(), l !== (l = t())) {
      var u = e.selectionStart, d = e.selectionEnd, f = e.value.length;
      if (e.value = l ?? "", d !== null) {
        var g = e.value.length;
        u === d && d === f && g > f ? (e.selectionStart = g, e.selectionEnd = g) : (e.selectionStart = u, e.selectionEnd = Math.min(d, g));
      }
    }
  }), // If we are hydrating and the value has since changed,
  // then use the updated value from the input instead.
  (it && e.defaultValue !== e.value || // If defaultValue is set, then value == defaultValue
  // TODO Svelte 6: remove input.value check and set to empty string?
  os(t) == null && e.value) && (r(Ea(e) ? Aa(e.value) : e.value), ut !== null && n.add(ut)), ha(() => {
    var c = t();
    if (e === document.activeElement) {
      var l = (
        /** @type {Batch} */
        ia ?? ut
      );
      if (n.has(l))
        return;
    }
    Ea(e) && c === Aa(e.value) || e.type === "date" && !c && !e.value || c !== e.value && (e.value = c ?? "");
  });
}
function Ea(e) {
  var t = e.type;
  return t === "number" || t === "range";
}
function Aa(e) {
  return e === "" ? null : +e;
}
function qn(e, t) {
  return e === t || e?.[Br] === t;
}
function Xr(e = {}, t, r, n) {
  return pn(() => {
    var c, l;
    return ha(() => {
      c = l, l = [], os(() => {
        e !== r(...l) && (t(e, ...l), c && qn(r(...c), e) && t(null, ...c));
      });
    }), () => {
      mr(() => {
        l && qn(r(...l), e) && t(null, ...l);
      });
    };
  }), e;
}
function el(e = !1) {
  const t = (
    /** @type {ComponentContextLegacy} */
    mt
  ), r = t.l.u;
  if (!r) return;
  let n = () => Ic(t.s);
  if (e) {
    let c = 0, l = (
      /** @type {Record<string, any>} */
      {}
    );
    const u = /* @__PURE__ */ Hs(() => {
      let d = !1;
      const f = t.s;
      for (const g in f)
        f[g] !== l[g] && (l[g] = f[g], d = !0);
      return d && c++, c;
    });
    n = () => s(u);
  }
  r.b.length && Ec(() => {
    Hn(t, n), sa(r.b);
  }), Tt(() => {
    const c = os(() => r.m.map(Ko));
    return () => {
      for (const l of c)
        typeof l == "function" && l();
    };
  }), r.a.length && Tt(() => {
    Hn(t, n), sa(r.a);
  });
}
function Hn(e, t) {
  if (e.l.s)
    for (const r of e.l.s) s(r);
  t();
}
function gn(e, t, r) {
  if (e == null)
    return t(void 0), r && r(void 0), Ar;
  const n = os(
    () => e.subscribe(
      t,
      // @ts-expect-error
      r
    )
  );
  return n.unsubscribe ? () => n.unsubscribe() : n;
}
const ls = [];
function tl(e, t) {
  return {
    subscribe: Ze(e, t).subscribe
  };
}
function Ze(e, t = Ar) {
  let r = null;
  const n = /* @__PURE__ */ new Set();
  function c(d) {
    if (li(e, d) && (e = d, r)) {
      const f = !ls.length;
      for (const g of n)
        g[1](), ls.push(g, e);
      if (f) {
        for (let g = 0; g < ls.length; g += 2)
          ls[g][0](ls[g + 1]);
        ls.length = 0;
      }
    }
  }
  function l(d) {
    c(d(
      /** @type {T} */
      e
    ));
  }
  function u(d, f = Ar) {
    const g = [d, f];
    return n.add(g), n.size === 1 && (r = t(c, l) || Ar), d(
      /** @type {T} */
      e
    ), () => {
      n.delete(g), n.size === 0 && r && (r(), r = null);
    };
  }
  return { set: c, update: l, subscribe: u };
}
function Pt(e, t, r) {
  const n = !Array.isArray(e), c = n ? [e] : e;
  if (!c.every(Boolean))
    throw new Error("derived() expects stores as input, got a falsy value");
  const l = t.length < 2;
  return tl(r, (u, d) => {
    let f = !1;
    const g = [];
    let m = 0, k = Ar;
    const y = () => {
      if (m)
        return;
      k();
      const E = t(n ? g[0] : g, u, d);
      l ? u(E) : k = typeof E == "function" ? E : Ar;
    }, P = c.map(
      (E, Q) => gn(
        E,
        (S) => {
          g[Q] = S, m &= ~(1 << Q), f && y();
        },
        () => {
          m |= 1 << Q;
        }
      )
    );
    return f = !0, y(), function() {
      sa(P), k(), f = !1;
    };
  });
}
function Qt(e) {
  let t;
  return gn(e, (r) => t = r)(), t;
}
let Va = /* @__PURE__ */ Symbol();
function ke(e, t, r) {
  const n = r[t] ??= {
    store: null,
    source: /* @__PURE__ */ dn(void 0),
    unsubscribe: Ar
  };
  if (n.store !== e && !(Va in r))
    if (n.unsubscribe(), n.store = e ?? null, e == null)
      n.source.v = void 0, n.unsubscribe = Ar;
    else {
      var c = !0;
      n.unsubscribe = gn(e, (l) => {
        c ? n.source.v = l : x(n.source, l);
      }), c = !1;
    }
  return e && Va in r ? Qt(e) : s(n.source);
}
function Vt() {
  const e = {};
  function t() {
    xa(() => {
      for (var r in e)
        e[r].unsubscribe();
      Os(e, Va, {
        enumerable: !1,
        value: !0
      });
    });
  }
  return [e, t];
}
function pt(e, t, r, n) {
  var c = !ys || (r & Bo) !== 0, l = (r & Go) !== 0, u = (
    /** @type {V} */
    n
  ), d = !0, f = () => (d && (d = !1, u = /** @type {V} */
  n), u), g;
  g = /** @type {V} */
  e[t], g === void 0 && n !== void 0 && (g = f());
  var m;
  if (c ? m = () => {
    var E = (
      /** @type {V} */
      e[t]
    );
    return E === void 0 ? f() : (d = !0, E);
  } : m = () => {
    var E = (
      /** @type {V} */
      e[t]
    );
    return E !== void 0 && (u = /** @type {V} */
    void 0), E === void 0 ? u : E;
  }, c && (r & Vo) === 0)
    return m;
  var k = !1, y = /* @__PURE__ */ Hs(() => (k = !1, m())), P = (
    /** @type {Effect} */
    dt
  );
  return (
    /** @type {() => V} */
    (function(E, Q) {
      if (arguments.length > 0) {
        const S = Q ? s(y) : c && l ? Bt(E) : E;
        return x(y, S), k = !0, u !== void 0 && (u = S), E;
      }
      return qr && k || (P.f & Dr) !== 0 ? y.v : s(y);
    })
  );
}
function rl(e) {
  return new sl(e);
}
class sl {
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
    var r = /* @__PURE__ */ new Map(), n = (l, u) => {
      var d = /* @__PURE__ */ dn(u, !1, !1);
      return r.set(l, d), d;
    };
    const c = new Proxy(
      { ...t.props || {}, $$events: {} },
      {
        get(l, u) {
          return s(r.get(u) ?? n(u, Reflect.get(l, u)));
        },
        has(l, u) {
          return u === Jo ? !0 : (s(r.get(u) ?? n(u, Reflect.get(l, u))), Reflect.has(l, u));
        },
        set(l, u, d) {
          return x(r.get(u) ?? n(u, d), d), Reflect.set(l, u, d);
        }
      }
    );
    this.#t = (t.hydrate ? Fc : zi)(t.component, {
      target: t.target,
      anchor: t.anchor,
      props: c,
      context: t.context,
      intro: t.intro ?? !1,
      recover: t.recover
    }), (!t?.props?.$$host || t.sync === !1) && vt(), this.#e = c.$$events;
    for (const l of Object.keys(this.#t))
      l === "$set" || l === "$destroy" || l === "$on" || Os(this, l, {
        get() {
          return this.#t[l];
        },
        /** @param {any} value */
        set(u) {
          this.#t[l] = u;
        },
        enumerable: !0
      });
    this.#t.$set = /** @param {Record<string, any>} next */
    (l) => {
      Object.assign(c, l);
    }, this.#t.$destroy = () => {
      jc(this.#t);
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
    const n = (...c) => r.call(this, ...c);
    return this.#e[t].push(n), () => {
      this.#e[t] = this.#e[t].filter(
        /** @param {any} fn */
        (c) => c !== n
      );
    };
  }
  $destroy() {
    this.#t.$destroy();
  }
}
let Xi;
typeof HTMLElement == "function" && (Xi = class extends HTMLElement {
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
      const n = this.$$c.$on(e, t);
      this.$$l_u.set(t, n);
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
      const n = this.$$l_u.get(t);
      n && (n(), this.$$l_u.delete(t));
    }
  }
  async connectedCallback() {
    if (this.$$cn = !0, !this.$$c) {
      let e = function(n) {
        return (c) => {
          const l = document.createElement("slot");
          n !== "default" && (l.name = n), v(c, l);
        };
      };
      if (await Promise.resolve(), !this.$$cn || this.$$c)
        return;
      const t = {}, r = al(this);
      for (const n of this.$$s)
        n in r && (n === "default" && !this.$$d.children ? (this.$$d.children = e(n), t.default = !0) : t[n] = e(n));
      for (const n of this.attributes) {
        const c = this.$$g_p(n.name);
        c in this.$$d || (this.$$d[c] = Js(c, n.value, this.$$p_d, "toProp"));
      }
      for (const n in this.$$p_d)
        !(n in this.$$d) && this[n] !== void 0 && (this.$$d[n] = this[n], delete this[n]);
      this.$$c = rl({
        component: this.$$ctor,
        target: this.$$shadowRoot || this,
        props: {
          ...this.$$d,
          $$slots: t,
          $$host: this
        }
      }), this.$$me = Ac(() => {
        ha(() => {
          this.$$r = !0;
          for (const n of ra(this.$$c)) {
            if (!this.$$p_d[n]?.reflect) continue;
            this.$$d[n] = this.$$c[n];
            const c = Js(
              n,
              this.$$d[n],
              this.$$p_d,
              "toAttribute"
            );
            c == null ? this.removeAttribute(this.$$p_d[n].attribute || n) : this.setAttribute(this.$$p_d[n].attribute || n, c);
          }
          this.$$r = !1;
        });
      });
      for (const n in this.$$l)
        for (const c of this.$$l[n]) {
          const l = this.$$c.$on(n, c);
          this.$$l_u.set(c, l);
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
    this.$$r || (e = this.$$g_p(e), this.$$d[e] = Js(e, r, this.$$p_d, "toProp"), this.$$c?.$set({ [e]: this.$$d[e] }));
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
    return ra(this.$$p_d).find(
      (t) => this.$$p_d[t].attribute === e || !this.$$p_d[t].attribute && t.toLowerCase() === e
    ) || e;
  }
});
function Js(e, t, r, n) {
  const c = r[e]?.type;
  if (t = c === "Boolean" && typeof t != "boolean" ? t != null : t, !n || !r[e])
    return t;
  if (n === "toAttribute")
    switch (c) {
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
    switch (c) {
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
function al(e) {
  const t = {};
  return e.childNodes.forEach((r) => {
    t[
      /** @type {Element} node */
      r.slot || "default"
    ] = !0;
  }), t;
}
function Et(e, t, r, n, c, l) {
  let u = class extends Xi {
    constructor() {
      super(e, r, c), this.$$p_d = t;
    }
    static get observedAttributes() {
      return ra(t).map(
        (d) => (t[d].attribute || d).toLowerCase()
      );
    }
  };
  return ra(t).forEach((d) => {
    Os(u.prototype, d, {
      get() {
        return this.$$c && d in this.$$c ? this.$$c[d] : this.$$d[d];
      },
      set(f) {
        f = Js(d, f, t), this.$$d[d] = f;
        var g = this.$$c;
        if (g) {
          var m = vs(g, d)?.get;
          m ? g[d] = f : g.$set({ [d]: f });
        }
      }
    });
  }), n.forEach((d) => {
    Os(u.prototype, d, {
      get() {
        return this.$$c?.[d];
      }
    });
  }), e.element = /** @type {any} */
  u, u;
}
const Zi = 8, eo = 16, Is = 64;
function Or(e, t) {
  return (e & t) !== 0;
}
function Ga(e, t) {
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
function qa(e, t) {
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
let Ha;
function nl(e) {
  Ha = e;
}
function Ke() {
  if (!Ha)
    throw new Error("Martha API not initialized. Call setApi() first.");
  return Ha;
}
const to = "hecate://localhost";
async function il() {
  try {
    const e = await fetch(`${to}/api/llm/models`);
    if (!e.ok) return [];
    const t = await e.json();
    return t.ok && Array.isArray(t.models) ? t.models.map((r) => r.name) : [];
  } catch {
    return [];
  }
}
function ol(e, t) {
  let r = null, n = null, c = null, l = !1;
  const u = {
    onChunk(d) {
      return r = d, u;
    },
    onDone(d) {
      return n = d, u;
    },
    onError(d) {
      return c = d, u;
    },
    async start() {
      if (!l)
        try {
          const d = await fetch(`${to}/api/llm/chat`, {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ model: e, messages: t })
          });
          if (l) return;
          if (!d.ok) {
            const g = await d.text().catch(() => d.statusText);
            c && c(g || "LLM request failed");
            return;
          }
          const f = await d.json();
          r && r({ content: f.content }), n && n({ content: "", done: !0 });
        } catch (d) {
          if (l) return;
          c && c(d.message || "LLM request failed");
        }
    },
    cancel() {
      l = !0;
    }
  };
  return u;
}
function ro() {
  return {
    stream: {
      chat: ol
    }
  };
}
const bn = Ze(!1), so = Ze(""), mn = Ze(null), ao = Ze(null), yn = Ze([]), wn = Pt(
  [ao, yn],
  ([e, t]) => e ?? t[0] ?? null
), no = "hecate-app-martha-phase-models";
function cl() {
  try {
    const e = localStorage.getItem(no);
    if (e)
      return { storming: null, planning: null, kanban: null, crafting: null, ...JSON.parse(e) };
  } catch {
  }
  return { storming: null, planning: null, kanban: null, crafting: null };
}
function ll(e) {
  try {
    localStorage.setItem(no, JSON.stringify(e));
  } catch {
  }
}
const io = Ze(cl()), dl = [
  /code/i,
  /coder/i,
  /codestral/i,
  /starcoder/i,
  /codellama/i,
  /wizard-?coder/i,
  /deepseek-coder/i
];
function Wn(e) {
  return dl.some((t) => t.test(e)) ? "code" : "general";
}
function ul(e) {
  return e === "crafting" ? "code" : "general";
}
function Cr(e, t) {
  mn.set(t ?? null), so.set(e), bn.set(!0);
}
function vl() {
  bn.set(!1), mn.set(null);
}
function kn(e) {
  ao.set(e);
}
function Un(e, t) {
  io.update((r) => {
    const n = { ...r, [e]: t };
    return ll(n), n;
  });
}
function fl(e) {
  return e.split(`
`).map((t) => t.replace(/^[\s\-*\u2022\d.]+/, "").trim()).filter((t) => t.length > 0 && t.length < 80 && !t.includes(":")).map((t) => t.replace(/["`]/g, ""));
}
const Zr = [
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
], bs = Ze([]), St = Ze(null), ss = Ze([]), Ns = Ze(null), wt = Ze(!1), hr = Ze(null), Ur = Pt(
  [ss, Ns],
  ([e, t]) => e.find((r) => r.division_id === t) ?? null
), _a = Pt(
  St,
  (e) => e ? Or(e.status, Is) ? "archived" : Or(e.status, eo) ? "discovery_paused" : Or(e.status, Zi) ? "discovering" : e.phase || "initiated" : "none"
);
function Ls(e) {
  St.set(e);
}
function Wa() {
  St.set(null);
}
async function ga() {
  try {
    const t = await Ke().get("/ventures");
    bs.set(t.ventures);
  } catch {
    bs.set([]);
  }
}
async function wr() {
  try {
    const t = await Ke().get("/venture");
    St.set(t.venture);
  } catch {
    St.set(null);
  }
}
async function ws(e) {
  try {
    const r = await Ke().get(
      `/ventures/${e}/divisions`
    );
    ss.set(r.divisions);
  } catch {
    ss.set([]);
  }
}
async function oo(e, t) {
  try {
    wt.set(!0);
    const n = await Ke().post("/ventures/initiate", { name: e, brief: t, initiated_by: "hecate-web" }), c = {
      venture_id: n.venture_id,
      name: n.name,
      vision: "",
      brief: n.brief || "",
      status: n.status,
      status_label: n.status_label,
      phase: "initiated",
      initiated_at: n.initiated_at,
      created_at: String(n.initiated_at),
      updated_at: String(n.initiated_at)
    };
    return bs.update((l) => [...l, c]), St.set(c), !0;
  } catch (r) {
    const n = r;
    return hr.set(n.message || "Failed to initiate venture"), !1;
  } finally {
    wt.set(!1);
  }
}
async function co(e, t, r, n, c) {
  try {
    return wt.set(!0), await Ke().post(`/ventures/${e}/repo`, {
      repo_url: t,
      vision: r || void 0,
      name: n || void 0,
      brief: c || void 0
    }), await wr(), !0;
  } catch (l) {
    const u = l;
    return hr.set(u.message || "Failed to scaffold venture repo"), !1;
  } finally {
    wt.set(!1);
  }
}
async function $n(e) {
  try {
    return wt.set(!0), await Ke().post(`/ventures/${e}/discovery/start`, {}), await wr(), !0;
  } catch (t) {
    const r = t;
    return hr.set(r.message || "Failed to start discovery"), !1;
  } finally {
    wt.set(!1);
  }
}
async function lo(e, t, r) {
  try {
    return wt.set(!0), await Ke().post(`/ventures/${e}/discovery/identify`, {
      context_name: t,
      description: r || null,
      identified_by: "hecate-web"
    }), await ws(e), !0;
  } catch (n) {
    const c = n;
    return hr.set(c.message || "Failed to identify division"), !1;
  } finally {
    wt.set(!1);
  }
}
async function uo(e, t) {
  try {
    return wt.set(!0), await Ke().post(`/ventures/${e}/discovery/pause`, {
      reason: t || null
    }), await wr(), !0;
  } catch (r) {
    const n = r;
    return hr.set(n.message || "Failed to pause discovery"), !1;
  } finally {
    wt.set(!1);
  }
}
async function vo(e) {
  try {
    return wt.set(!0), await Ke().post(`/ventures/${e}/discovery/resume`, {}), await wr(), !0;
  } catch (t) {
    const r = t;
    return hr.set(r.message || "Failed to resume discovery"), !1;
  } finally {
    wt.set(!1);
  }
}
async function fo(e) {
  try {
    return wt.set(!0), await Ke().post(`/ventures/${e}/discovery/complete`, {}), await wr(), !0;
  } catch (t) {
    const r = t;
    return hr.set(r.message || "Failed to complete discovery"), !1;
  } finally {
    wt.set(!1);
  }
}
const pl = /* @__PURE__ */ Object.freeze(/* @__PURE__ */ Object.defineProperty({
  __proto__: null,
  activeVenture: St,
  clearActiveVenture: Wa,
  completeDiscovery: fo,
  divisions: ss,
  fetchActiveVenture: wr,
  fetchDivisions: ws,
  fetchVentures: ga,
  identifyDivision: lo,
  initiateVenture: oo,
  isLoading: wt,
  pauseDiscovery: uo,
  resumeDiscovery: vo,
  scaffoldVentureRepo: co,
  selectVenture: Ls,
  selectedDivision: Ur,
  selectedDivisionId: Ns,
  startDiscovery: $n,
  ventureError: hr,
  ventureStep: _a,
  ventures: bs
}, Symbol.toStringTag, { value: "Module" })), Bs = Ze("storming"), ba = Ze(null), Rr = Ze(!1);
function ma(e) {
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
async function xl(e, t) {
  try {
    Rr.set(!0), await Ke().post(`/${ma(t)}/${e}/open`, {});
    const n = Qt(St);
    return n && await ws(n.venture_id), !0;
  } catch (r) {
    const n = r;
    return ba.set(n.message || `Failed to open ${t}`), !1;
  } finally {
    Rr.set(!1);
  }
}
async function hl(e, t, r) {
  try {
    Rr.set(!0), await Ke().post(`/${ma(t)}/${e}/shelve`, {
      reason: r || null
    });
    const c = Qt(St);
    return c && await ws(c.venture_id), !0;
  } catch (n) {
    const c = n;
    return ba.set(c.message || `Failed to shelve ${t}`), !1;
  } finally {
    Rr.set(!1);
  }
}
async function _l(e, t) {
  try {
    Rr.set(!0), await Ke().post(`/${ma(t)}/${e}/resume`, {});
    const n = Qt(St);
    return n && await ws(n.venture_id), !0;
  } catch (r) {
    const n = r;
    return ba.set(n.message || `Failed to resume ${t}`), !1;
  } finally {
    Rr.set(!1);
  }
}
async function gl(e, t) {
  try {
    Rr.set(!0), await Ke().post(`/${ma(t)}/${e}/conclude`, {});
    const n = Qt(St);
    return n && await ws(n.venture_id), !0;
  } catch (r) {
    const n = r;
    return ba.set(n.message || `Failed to conclude ${t}`), !1;
  } finally {
    Rr.set(!1);
  }
}
const as = Ze("ready"), ks = Ze([]), ya = Ze([]), Cn = Ze([]), ca = Ze(600), Sn = Ze([]), Ua = Ze(!1), Gt = Ze(null), za = Ze(!1);
let Kr = null;
const bl = Pt(
  ks,
  (e) => e.filter((t) => !t.cluster_id)
), ml = Pt(
  ks,
  (e) => {
    const t = /* @__PURE__ */ new Map();
    for (const r of e)
      if (r.stack_id) {
        const n = t.get(r.stack_id) || [];
        n.push(r), t.set(r.stack_id, n);
      }
    return t;
  }
), yl = Pt(
  ks,
  (e) => e.length
);
async function zt(e) {
  try {
    const n = (await Ke().get(
      `/ventures/${e}/storm/state`
    )).storm;
    as.set(n.phase), ks.set(n.stickies), ya.set(n.clusters), Cn.set(n.arrows);
  } catch {
    as.set("ready");
  }
}
async function zn(e, t = 0, r = 50) {
  try {
    const c = await Ke().get(
      `/ventures/${e}/events?offset=${t}&limit=${r}`
    );
    return Sn.set(c.events), { events: c.events, count: c.count };
  } catch {
    return { events: [], count: 0 };
  }
}
async function wl(e) {
  try {
    return za.set(!0), await Ke().post(`/ventures/${e}/storm/start`, {}), as.set("storm"), ca.set(600), Kr = setInterval(() => {
      ca.update((r) => r <= 1 ? (Kr && (clearInterval(Kr), Kr = null), 0) : r - 1);
    }, 1e3), !0;
  } catch (t) {
    const r = t;
    return Gt.set(r.message || "Failed to start storm"), !1;
  } finally {
    za.set(!1);
  }
}
async function Qs(e, t, r = "user") {
  try {
    return await Ke().post(`/ventures/${e}/storm/sticky`, { text: t, author: r }), await zt(e), !0;
  } catch (n) {
    const c = n;
    return Gt.set(c.message || "Failed to post sticky"), !1;
  }
}
async function kl(e, t) {
  try {
    return await Ke().post(`/ventures/${e}/storm/sticky/${t}/pull`, {}), await zt(e), !0;
  } catch (r) {
    const n = r;
    return Gt.set(n.message || "Failed to pull sticky"), !1;
  }
}
async function Yn(e, t, r) {
  try {
    return await Ke().post(`/ventures/${e}/storm/sticky/${t}/stack`, {
      target_sticky_id: r
    }), await zt(e), !0;
  } catch (n) {
    const c = n;
    return Gt.set(c.message || "Failed to stack sticky"), !1;
  }
}
async function $l(e, t) {
  try {
    return await Ke().post(`/ventures/${e}/storm/sticky/${t}/unstack`, {}), await zt(e), !0;
  } catch (r) {
    const n = r;
    return Gt.set(n.message || "Failed to unstack sticky"), !1;
  }
}
async function Cl(e, t, r) {
  try {
    return await Ke().post(`/ventures/${e}/storm/stack/${t}/groom`, {
      canonical_sticky_id: r
    }), await zt(e), !0;
  } catch (n) {
    const c = n;
    return Gt.set(c.message || "Failed to groom stack"), !1;
  }
}
async function Kn(e, t, r) {
  try {
    return await Ke().post(`/ventures/${e}/storm/sticky/${t}/cluster`, {
      target_cluster_id: r
    }), await zt(e), !0;
  } catch (n) {
    const c = n;
    return Gt.set(c.message || "Failed to cluster sticky"), !1;
  }
}
async function Sl(e, t) {
  try {
    return await Ke().post(`/ventures/${e}/storm/sticky/${t}/uncluster`, {}), await zt(e), !0;
  } catch (r) {
    const n = r;
    return Gt.set(n.message || "Failed to uncluster sticky"), !1;
  }
}
async function El(e, t) {
  try {
    return await Ke().post(`/ventures/${e}/storm/cluster/${t}/dissolve`, {}), await zt(e), !0;
  } catch (r) {
    const n = r;
    return Gt.set(n.message || "Failed to dissolve cluster"), !1;
  }
}
async function Al(e, t, r) {
  try {
    return await Ke().post(`/ventures/${e}/storm/cluster/${t}/name`, { name: r }), await zt(e), !0;
  } catch (n) {
    const c = n;
    return Gt.set(c.message || "Failed to name cluster"), !1;
  }
}
async function Dl(e, t, r, n) {
  try {
    return await Ke().post(`/ventures/${e}/storm/fact`, {
      from_cluster: t,
      to_cluster: r,
      fact_name: n
    }), await zt(e), !0;
  } catch (c) {
    const l = c;
    return Gt.set(l.message || "Failed to draw fact arrow"), !1;
  }
}
async function Pl(e, t) {
  try {
    return await Ke().post(`/ventures/${e}/storm/fact/${t}/erase`, {}), await zt(e), !0;
  } catch (r) {
    const n = r;
    return Gt.set(n.message || "Failed to erase fact arrow"), !1;
  }
}
async function Tl(e, t) {
  try {
    return await Ke().post(`/ventures/${e}/storm/cluster/${t}/promote`, {}), await zt(e), !0;
  } catch (r) {
    const n = r;
    return Gt.set(n.message || "Failed to promote cluster"), !1;
  }
}
async function Es(e, t) {
  try {
    return await Ke().post(`/ventures/${e}/storm/phase/advance`, {
      target_phase: t
    }), await zt(e), !0;
  } catch (r) {
    const n = r;
    return Gt.set(n.message || "Failed to advance phase"), !1;
  }
}
async function Rl(e) {
  try {
    return await Ke().post(`/ventures/${e}/storm/shelve`, {}), as.set("shelved"), !0;
  } catch (t) {
    const r = t;
    return Gt.set(r.message || "Failed to shelve storm"), !1;
  }
}
async function Ml(e) {
  try {
    return await Ke().post(`/ventures/${e}/storm/resume`, {}), await zt(e), !0;
  } catch (t) {
    const r = t;
    return Gt.set(r.message || "Failed to resume storm"), !1;
  }
}
async function Il(e) {
  const t = Qt(ya);
  let r = !0;
  for (const n of t) {
    if (n.status !== "active" || !n.name?.trim()) continue;
    await Tl(e, n.cluster_id) || (r = !1);
  }
  if (r) {
    const { fetchDivisions: n } = await Promise.resolve().then(() => pl);
    await n(e);
  }
  return r;
}
function Nl() {
  Kr && (clearInterval(Kr), Kr = null), as.set("ready"), ks.set([]), ya.set([]), Cn.set([]), Sn.set([]), ca.set(600);
}
const Hr = {
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
}, Ll = [
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
], Ol = Ze([]), $s = Ze([]), Zt = Ze(null), Ya = Ze(!1), Xs = Ze(null), Ka = Ze([]), zs = Ze([]), Fl = Pt(
  $s,
  (e) => e.filter((t) => Hr[t.role]?.tier === "creative")
), jl = Pt(
  $s,
  (e) => e.filter((t) => Hr[t.role]?.tier === "mechanical")
), Bl = Pt(
  $s,
  (e) => e.filter((t) => Hr[t.role]?.tier === "always_on")
), Vl = Pt(zs, (e) => e.length > 0);
async function ms(e) {
  try {
    Ya.set(!0), Zt.set(null);
    const r = await Ke().get(
      `/ventures/${e}/agents/sessions`
    );
    Ol.set(r.sessions ?? []), Yl(r.sessions ?? []), Kl(r.sessions ?? []);
  } catch (t) {
    const r = t;
    Zt.set(r.message || "Failed to fetch agent sessions");
  } finally {
    Ya.set(!1);
  }
}
async function Gl(e, t) {
  try {
    Zt.set(null);
    const n = await Ke().get(
      `/ventures/${e}/agents/sessions/${t}`
    );
    Xs.set(n.session);
  } catch (r) {
    const n = r;
    Zt.set(n.message || "Failed to fetch session detail");
  }
}
async function ql(e, t) {
  try {
    const n = await Ke().get(
      `/ventures/${e}/agents/sessions/${t}/turns`
    );
    Ka.set(n.turns ?? []);
  } catch {
    Ka.set([]);
  }
}
async function Hl(e, t, r) {
  try {
    Zt.set(null);
    const n = Ke(), c = {};
    return await n.post(`/ventures/${e}/agents/${t}/initiate`, c), await ms(e), !0;
  } catch (n) {
    const c = n;
    return Zt.set(c.message || `Failed to initiate ${t}`), !1;
  }
}
async function Wl(e, t, r) {
  try {
    return Zt.set(null), await Ke().post(`/ventures/${e}/agents/${t}/gate/pass`, { session_id: r }), await ms(e), !0;
  } catch (n) {
    const c = n;
    return Zt.set(c.message || "Failed to pass gate"), !1;
  }
}
async function Ul(e, t, r, n) {
  try {
    return Zt.set(null), await Ke().post(`/ventures/${e}/agents/${t}/gate/reject`, {
      session_id: r,
      reason: n
    }), await ms(e), !0;
  } catch (c) {
    const l = c;
    return Zt.set(l.message || "Failed to reject gate"), !1;
  }
}
async function zl(e, t) {
  try {
    return Zt.set(null), await Ke().post(`/ventures/${e}/agents/sessions/${t}/archive`, {}), await ms(e), !0;
  } catch (r) {
    const n = r;
    return Zt.set(n.message || "Failed to archive session"), !1;
  }
}
function Yl(e) {
  const t = Ll.map((r) => {
    const n = e.filter((f) => f.role === r), c = n.find((f) => f.status === "running" || f.status === "gate_pending"), l = n.filter((f) => f.status === "completed").sort((f, g) => (g.completed_at ?? 0) - (f.completed_at ?? 0))[0], u = n.find((f) => f.status === "failed");
    let d = "idle";
    return c?.status === "gate_pending" ? d = "gate_pending" : c?.status === "running" ? d = "running" : u && !l ? d = "failed" : l && (d = "completed"), {
      role: r,
      label: Hr[r].label,
      tier: Hr[r].tier,
      status: d,
      active_session: c ?? l ?? u ?? null,
      session_count: n.length
    };
  });
  $s.set(t);
}
function Kl(e) {
  zs.set(e.filter((t) => t.status === "gate_pending"));
}
const br = Ze("disconnected"), Jl = Ze(null), po = "hecate://localhost", xo = "/plugin/hecate-app-martha/api/events/stream", Ql = 3e3;
let Fr = null, Tr = null, Zs = [];
function Xl(e) {
  return Zs.push(e), () => {
    Zs = Zs.filter((t) => t !== e);
  };
}
function Zl(e) {
  Jl.set(e);
  for (const t of Zs)
    t(e);
}
function ho() {
  if (!Fr) {
    br.set("connecting");
    try {
      Fr = new EventSource(`${po}${xo}`), Fr.onopen = () => {
        br.set("connected"), Tr && (clearTimeout(Tr), Tr = null);
      }, Fr.onerror = () => {
        br.set("disconnected"), go(), Ja();
      }, Fr.onmessage = (e) => {
        _o("message", e.data);
      };
    } catch {
      br.set("disconnected"), ed();
    }
  }
}
async function ed() {
  br.set("connecting");
  try {
    const e = await fetch(`${po}${xo}`);
    if (!e.ok || !e.body) {
      br.set("disconnected"), Ja();
      return;
    }
    br.set("connected");
    const t = e.body.getReader(), r = new TextDecoder();
    let n = "";
    for (; ; ) {
      const { done: c, value: l } = await t.read();
      if (c) break;
      n += r.decode(l, { stream: !0 });
      const u = n.split(`

`);
      n = u.pop() ?? "";
      for (const d of u)
        !d.trim() || d.startsWith(":") || td(d);
    }
  } catch {
  }
  br.set("disconnected"), Ja();
}
function td(e) {
  let t = "message", r = "";
  for (const n of e.split(`
`))
    n.startsWith("event: ") ? t = n.slice(7) : n.startsWith("data: ") && (r = n.slice(6));
  r && _o(t, r);
}
function _o(e, t) {
  try {
    const r = JSON.parse(t);
    Zl({ type: e, data: r, receivedAt: Date.now() });
  } catch {
  }
}
function Ja() {
  Tr || (Tr = setTimeout(() => {
    Tr = null, ho();
  }, Ql));
}
function go() {
  Fr && (Fr.close(), Fr = null);
}
function rd() {
  Tr && (clearTimeout(Tr), Tr = null), go(), br.set("disconnected");
}
const Jn = 50;
let or = 0;
const bo = Ze([]), En = Ze(0), sd = Pt(
  bo,
  (e) => e.slice(0, 10)
);
function ad(e) {
  const { type: t, data: r, receivedAt: n } = e;
  if (t === "venture_initiated_v1")
    return {
      id: `act-${or++}`,
      type: t,
      summary: `Venture "${r.name ?? "unknown"}" initiated`,
      severity: "success",
      timestamp: n
    };
  if (t === "venture_archived_v1")
    return {
      id: `act-${or++}`,
      type: t,
      summary: "Venture archived",
      severity: "info",
      timestamp: n
    };
  if (t === "division_initiated_v1")
    return {
      id: `act-${or++}`,
      type: t,
      summary: `Division "${r.context_name ?? "unknown"}" initiated`,
      severity: "success",
      timestamp: n
    };
  if (t.includes("_opened_v")) {
    const c = Da(t);
    return {
      id: `act-${or++}`,
      type: t,
      summary: `${c} phase opened`,
      severity: "info",
      timestamp: n
    };
  }
  if (t.includes("_shelved_v")) {
    const c = Da(t);
    return {
      id: `act-${or++}`,
      type: t,
      summary: `${c} phase shelved`,
      detail: r.reason ?? void 0,
      severity: "warning",
      timestamp: n
    };
  }
  if (t.includes("_resumed_v")) {
    const c = Da(t);
    return {
      id: `act-${or++}`,
      type: t,
      summary: `${c} phase resumed`,
      severity: "info",
      timestamp: n
    };
  }
  if (t.includes("agent_") || t.includes("mentor_") || t.includes("coordinator_"))
    return {
      id: `act-${or++}`,
      type: t,
      summary: As(t),
      severity: "info",
      timestamp: n
    };
  if (t.startsWith("aggregate_designed_v") || t.startsWith("event_designed_v"))
    return {
      id: `act-${or++}`,
      type: t,
      summary: As(t),
      detail: r.name ?? void 0,
      severity: "info",
      timestamp: n
    };
  if (t.startsWith("kanban_card_"))
    return {
      id: `act-${or++}`,
      type: t,
      summary: As(t),
      severity: "info",
      timestamp: n
    };
  if (t.includes("_gate_")) {
    const c = t.includes("passed"), l = t.includes("rejected");
    return {
      id: `act-${or++}`,
      type: t,
      summary: As(t),
      severity: l ? "error" : c ? "success" : "warning",
      timestamp: n
    };
  }
  return {
    id: `act-${or++}`,
    type: t,
    summary: As(t),
    severity: "info",
    timestamp: n
  };
}
function Da(e) {
  return e.includes("planning") ? "Planning" : e.includes("crafting") ? "Crafting" : e.includes("storming") ? "Storming" : e.includes("kanban") ? "Kanban" : "Phase";
}
function As(e) {
  return e.replace(/_v\d+$/, "").replace(/_/g, " ").replace(/\b\w/g, (t) => t.toUpperCase());
}
function nd() {
  return Xl((e) => {
    const t = ad(e);
    t && (bo.update((r) => {
      const n = [t, ...r];
      return n.length > Jn && (n.length = Jn), n;
    }), En.update((r) => r + 1));
  });
}
function id() {
  En.set(0);
}
const Qn = Ze(!1), od = Ze(null), cd = Ze(null);
async function ld(e, t) {
  try {
    Qn.set(!0);
    const n = await Ke().post(
      `/ventures/${e}/vision/refine`,
      { vision: t }
    );
    return od.set(n.refined), await wr(), !0;
  } catch (r) {
    const n = r;
    return cd.set(n.message || "Failed to refine vision"), !1;
  } finally {
    Qn.set(!1);
  }
}
var dd = /* @__PURE__ */ p('<div class="text-[10px] text-surface-400 truncate mt-0.5"> </div>'), ud = /* @__PURE__ */ p('<button><div class="font-medium"> </div> <!></button>'), vd = /* @__PURE__ */ p(`<div class="absolute top-full left-0 mt-1 z-20 min-w-[220px]
						bg-surface-700 border border-surface-600 rounded-lg shadow-lg overflow-hidden"><!> <button class="w-full text-left px-3 py-2 text-xs text-hecate-400
							hover:bg-hecate-600/20 transition-colors border-t border-surface-600">+ New Venture</button></div>`), fd = /* @__PURE__ */ p('<span class="text-[11px] text-surface-400 truncate max-w-[300px]"> </span>'), pd = /* @__PURE__ */ p('<span class="text-[10px] text-surface-400"> </span>'), xd = /* @__PURE__ */ p('<span class="text-[10px] text-surface-400 italic">Oracle active</span>'), hd = /* @__PURE__ */ p(`<button class="text-[11px] px-2.5 py-1 rounded bg-hecate-600/20 text-hecate-300
					hover:bg-hecate-600/30 transition-colors disabled:opacity-50">Start Discovery</button>`), _d = /* @__PURE__ */ p(`<button class="text-[11px] px-2 py-1 rounded text-surface-400
						hover:text-health-ok hover:bg-surface-700 transition-colors disabled:opacity-50">Complete Discovery</button>`), gd = /* @__PURE__ */ p(
  `<button class="text-[11px] px-2.5 py-1 rounded bg-hecate-600/20 text-hecate-300
					hover:bg-hecate-600/30 transition-colors disabled:opacity-50">+ Identify Division</button> <button class="text-[11px] px-2 py-1 rounded text-surface-400
					hover:text-health-warn hover:bg-surface-700 transition-colors disabled:opacity-50">Pause</button> <!>`,
  1
), bd = /* @__PURE__ */ p(`<button class="text-[11px] px-2.5 py-1 rounded bg-health-warn/10 text-health-warn
					hover:bg-health-warn/20 transition-colors disabled:opacity-50">Resume Discovery</button>`), md = /* @__PURE__ */ p('<div class="mt-2 text-[11px] text-health-err bg-health-err/10 rounded px-3 py-1.5"> </div>'), yd = /* @__PURE__ */ p(`<div class="mt-3 flex gap-2 items-end"><div class="flex-1"><label for="refine-brief" class="text-[10px] text-surface-400 block mb-1">Vision Brief</label> <textarea id="refine-brief" placeholder="Describe what this venture aims to achieve..." class="w-full bg-surface-700 border border-surface-600 rounded px-3 py-2 text-xs
						text-surface-100 placeholder-surface-400 resize-none
						focus:outline-none focus:border-hecate-500"></textarea></div> <button class="px-3 py-2 rounded text-xs bg-hecate-600 text-surface-50
					hover:bg-hecate-500 transition-colors disabled:opacity-50 disabled:cursor-not-allowed">Refine</button> <button class="px-3 py-2 rounded text-xs text-surface-400 hover:text-surface-100 transition-colors">Cancel</button></div>`), wd = /* @__PURE__ */ p(`<div class="mt-3 flex gap-2 items-end"><div class="flex-1"><label for="div-name" class="text-[10px] text-surface-400 block mb-1">Context Name</label> <input id="div-name" placeholder="e.g., authentication, billing, notifications" class="w-full bg-surface-700 border border-surface-600 rounded px-3 py-1.5 text-xs
						text-surface-100 placeholder-surface-400
						focus:outline-none focus:border-hecate-500"/></div> <div class="flex-1"><label for="div-desc" class="text-[10px] text-surface-400 block mb-1">Description (optional)</label> <input id="div-desc" placeholder="Brief description of this bounded context" class="w-full bg-surface-700 border border-surface-600 rounded px-3 py-1.5 text-xs
						text-surface-100 placeholder-surface-400
						focus:outline-none focus:border-hecate-500"/></div> <button class="px-3 py-1.5 rounded text-xs bg-hecate-600 text-surface-50
					hover:bg-hecate-500 transition-colors disabled:opacity-50 disabled:cursor-not-allowed">Identify</button> <button class="px-3 py-1.5 rounded text-xs text-surface-400 hover:text-surface-100 transition-colors">Cancel</button></div>`), kd = /* @__PURE__ */ p(`<div class="border-b border-surface-600 bg-surface-800/50 px-4 py-3 shrink-0"><div class="flex items-center gap-3"><button class="flex items-center gap-1 text-xs text-surface-400 hover:text-hecate-300
				transition-colors shrink-0 -ml-1 px-1.5 py-1 rounded hover:bg-surface-700"><span class="text-sm"></span> <span>Ventures</span></button> <span class="text-surface-600 text-xs">|</span> <div class="relative flex items-center gap-2"><span class="text-hecate-400 text-lg"></span> <button class="flex items-center gap-1.5 text-sm font-semibold text-surface-100
					hover:text-hecate-300 transition-colors"> <span class="text-[9px] text-surface-400"></span></button> <!></div> <span> </span> <!> <div class="flex-1"></div> <!> <!></div> <!> <!> <!></div>`);
function mo(e, t) {
  kt(t, !0);
  const r = () => ke(St, "$activeVenture", f), n = () => ke(bs, "$ventures", f), c = () => ke(_a, "$ventureStep", f), l = () => ke(ss, "$divisions", f), u = () => ke(wt, "$isLoading", f), d = () => ke(hr, "$ventureError", f), [f, g] = Vt();
  let m = /* @__PURE__ */ oe(!1), k = /* @__PURE__ */ oe(!1), y = /* @__PURE__ */ oe(!1), P = /* @__PURE__ */ oe(""), E = /* @__PURE__ */ oe(""), Q = /* @__PURE__ */ oe("");
  async function S() {
    if (!r() || !s(P).trim()) return;
    await ld(r().venture_id, s(P).trim()) && (x(m, !1), x(P, ""));
  }
  async function U() {
    r() && await $n(r().venture_id);
  }
  async function ae() {
    if (!r() || !s(E).trim()) return;
    await lo(r().venture_id, s(E).trim(), s(Q).trim() || void 0) && (x(k, !1), x(E, ""), x(Q, ""));
  }
  function we(w) {
    switch (w) {
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
  var le = kd(), ve = i(le), de = i(ve);
  de.__click = () => Wa();
  var Le = i(de);
  Le.textContent = "←", At(2), a(de);
  var Ae = o(de, 4), De = i(Ae);
  De.textContent = "◆";
  var te = o(De, 2);
  te.__click = () => x(y, !s(y));
  var N = i(te), X = o(N);
  X.textContent = "▾", a(te);
  var ue = o(te, 2);
  {
    var Te = (w) => {
      var D = vd(), z = i(D);
      He(z, 1, () => n().filter((Ce) => !(Ce.status & Is)), ft, (Ce, T) => {
        var I = ud();
        I.__click = () => {
          Ls(s(T)), x(y, !1);
        };
        var Z = i(I), Me = i(Z, !0);
        a(Z);
        var Be = o(Z, 2);
        {
          var Je = (ze) => {
            var tt = dd(), qe = i(tt, !0);
            a(tt), $(() => h(qe, s(T).brief)), v(ze, tt);
          };
          A(Be, (ze) => {
            s(T).brief && ze(Je);
          });
        }
        a(I), $(() => {
          Ie(I, 1, `w-full text-left px-3 py-2 text-xs transition-colors
								${s(T).venture_id === r()?.venture_id ? "bg-hecate-600/20 text-hecate-300" : "text-surface-200 hover:bg-surface-600"}`), h(Me, s(T).name);
        }), v(Ce, I);
      });
      var pe = o(z, 2);
      pe.__click = () => {
        Wa(), x(y, !1);
      }, a(D), v(w, D);
    };
    A(ue, (w) => {
      s(y) && w(Te);
    });
  }
  a(Ae);
  var ne = o(Ae, 2), W = i(ne, !0);
  a(ne);
  var K = o(ne, 2);
  {
    var Fe = (w) => {
      var D = fd(), z = i(D, !0);
      a(D), $(() => h(z, r().brief)), v(w, D);
    };
    A(K, (w) => {
      r()?.brief && w(Fe);
    });
  }
  var et = o(K, 4);
  {
    var st = (w) => {
      var D = pd(), z = i(D);
      a(D), $(() => h(z, `${l().length ?? ""} division${l().length !== 1 ? "s" : ""}`)), v(w, D);
    };
    A(et, (w) => {
      l().length > 0 && w(st);
    });
  }
  var Xe = o(et, 2);
  {
    var We = (w) => {
      var D = xd();
      v(w, D);
    }, Ge = (w) => {
      var D = hd();
      D.__click = U, $(() => D.disabled = u()), v(w, D);
    }, C = (w) => {
      var D = gd(), z = ct(D);
      z.__click = () => x(k, !s(k));
      var pe = o(z, 2);
      pe.__click = () => r() && uo(r().venture_id);
      var Ce = o(pe, 2);
      {
        var T = (I) => {
          var Z = _d();
          Z.__click = () => r() && fo(r().venture_id), $(() => Z.disabled = u()), v(I, Z);
        };
        A(Ce, (I) => {
          l().length > 0 && I(T);
        });
      }
      $(() => {
        z.disabled = u(), pe.disabled = u();
      }), v(w, D);
    }, V = (w) => {
      var D = bd();
      D.__click = () => r() && vo(r().venture_id), $(() => D.disabled = u()), v(w, D);
    };
    A(Xe, (w) => {
      c() === "initiated" || c() === "vision_refined" ? w(We) : c() === "vision_submitted" ? w(Ge, 1) : c() === "discovering" ? w(C, 2) : c() === "discovery_paused" && w(V, 3);
    });
  }
  a(ve);
  var H = o(ve, 2);
  {
    var M = (w) => {
      var D = md(), z = i(D, !0);
      a(D), $(() => h(z, d())), v(w, D);
    };
    A(H, (w) => {
      d() && w(M);
    });
  }
  var L = o(H, 2);
  {
    var F = (w) => {
      var D = yd(), z = i(D), pe = o(i(z), 2);
      Ws(pe), Ot(pe, "rows", 2), a(z);
      var Ce = o(z, 2);
      Ce.__click = S;
      var T = o(Ce, 2);
      T.__click = () => x(m, !1), a(D), $((I) => Ce.disabled = I, [() => !s(P).trim() || u()]), xt(pe, () => s(P), (I) => x(P, I)), v(w, D);
    };
    A(L, (w) => {
      s(m) && w(F);
    });
  }
  var J = o(L, 2);
  {
    var $e = (w) => {
      var D = wd(), z = i(D), pe = o(i(z), 2);
      bt(pe), a(z);
      var Ce = o(z, 2), T = o(i(Ce), 2);
      bt(T), a(Ce);
      var I = o(Ce, 2);
      I.__click = ae;
      var Z = o(I, 2);
      Z.__click = () => x(k, !1), a(D), $((Me) => I.disabled = Me, [() => !s(E).trim() || u()]), xt(pe, () => s(E), (Me) => x(E, Me)), xt(T, () => s(Q), (Me) => x(Q, Me)), v(w, D);
    };
    A(J, (w) => {
      s(k) && w($e);
    });
  }
  a(le), $(
    (w) => {
      h(N, `${r()?.name ?? "Venture" ?? ""} `), Ie(ne, 1, `text-[10px] px-2 py-0.5 rounded-full border ${w ?? ""}`), h(W, r()?.status_label ?? "New");
    },
    [() => we(c())]
  ), v(e, le), $t(), g();
}
Mt(["click"]);
Et(mo, {}, [], [], { mode: "open" });
var $d = /* @__PURE__ */ p('<p class="text-xs text-surface-300 mt-1.5 max-w-md mx-auto"> </p>'), Cd = /* @__PURE__ */ p("<span></span>"), Sd = /* @__PURE__ */ p('<div class="flex items-center gap-1"><div class="flex flex-col items-center gap-0.5 px-2"><span> </span> <span> </span></div> <!></div>'), Ed = /* @__PURE__ */ p('<div class="rounded-lg border border-surface-600 bg-surface-800 p-3 col-span-2"><div class="text-[9px] text-surface-400 uppercase tracking-wider mb-1">Repository</div> <div class="text-xs text-surface-200 font-mono"> </div></div>'), Ad = /* @__PURE__ */ p('<div class="rounded-lg border border-hecate-600/30 bg-hecate-600/5 p-5 text-center"><div class="text-xs text-surface-200 mb-3">Your venture repo has been scaffolded. The next step is <strong class="text-hecate-300">Big Picture Event Storming</strong> </div> <button> </button></div>'), Dd = /* @__PURE__ */ p(`<div class="rounded-lg border border-surface-600 bg-surface-800 p-5 text-center"><div class="text-xs text-surface-200 mb-2">Discovery is complete. Identify divisions (bounded contexts)
						from the events you discovered.</div> <div class="text-[10px] text-surface-400">Use the header controls to identify divisions.</div></div>`), Pd = /* @__PURE__ */ p('<div class="rounded-lg border border-surface-600 bg-surface-800 p-5 text-center"><div class="text-xs text-surface-200">Continue from the header controls to advance through the lifecycle.</div></div>'), Td = /* @__PURE__ */ p('<div class="text-center"><div class="text-3xl mb-3 text-hecate-400"></div> <h2 class="text-lg font-semibold text-surface-100"> </h2> <!></div> <div class="flex items-center justify-center gap-1 py-4"></div> <div class="grid grid-cols-2 gap-3"><div class="rounded-lg border border-surface-600 bg-surface-800 p-3"><div class="text-[9px] text-surface-400 uppercase tracking-wider mb-1">Status</div> <div class="text-xs text-surface-100"> </div></div> <div class="rounded-lg border border-surface-600 bg-surface-800 p-3"><div class="text-[9px] text-surface-400 uppercase tracking-wider mb-1">Initiated</div> <div class="text-xs text-surface-100"> </div></div> <!></div> <!>', 1), Rd = /* @__PURE__ */ p('<div class="flex flex-col h-full overflow-y-auto"><div class="max-w-2xl mx-auto w-full p-8 space-y-6"><!></div></div>');
function ea(e, t) {
  kt(t, !0);
  const r = () => ke(St, "$activeVenture", l), n = () => ke(_a, "$ventureStep", l), c = () => ke(wt, "$isLoading", l), [l, u] = Vt();
  let d = pt(t, "nextAction", 7);
  function f(ae) {
    return ae ? new Date(ae * 1e3).toLocaleDateString("en-US", {
      year: "numeric",
      month: "short",
      day: "numeric",
      hour: "2-digit",
      minute: "2-digit"
    }) : "";
  }
  async function g() {
    if (!r()) return;
    await $n(r().venture_id) && (await wr(), await ga());
  }
  const m = [
    { key: "vision", label: "Vision", icon: "◇" },
    { key: "discovery", label: "Discovery", icon: "○" },
    { key: "design", label: "Design", icon: "△" },
    { key: "plan", label: "Plan", icon: "□" },
    { key: "implement", label: "Implement", icon: "⚙" },
    { key: "deploy", label: "Deploy", icon: "▲" },
    { key: "monitor", label: "Monitor", icon: "◉" },
    { key: "rescue", label: "Rescue", icon: "↺" }
  ];
  let k = /* @__PURE__ */ Ee(() => {
    const ae = n();
    return ae === "initiated" || ae === "vision_refined" || ae === "vision_submitted" ? 0 : ae === "discovering" || ae === "discovery_paused" || ae === "discovery_completed" ? 1 : 0;
  });
  var y = {
    get nextAction() {
      return d();
    },
    set nextAction(ae) {
      d(ae), vt();
    }
  }, P = Rd(), E = i(P), Q = i(E);
  {
    var S = (ae) => {
      var we = Td(), le = ct(we), ve = i(le);
      ve.textContent = "◆";
      var de = o(ve, 2), Le = i(de, !0);
      a(de);
      var Ae = o(de, 2);
      {
        var De = (C) => {
          var V = $d(), H = i(V, !0);
          a(V), $(() => h(H, r().brief)), v(C, V);
        };
        A(Ae, (C) => {
          r().brief && C(De);
        });
      }
      a(le);
      var te = o(le, 2);
      He(te, 21, () => m, ft, (C, V, H) => {
        const M = /* @__PURE__ */ Ee(() => H < s(k)), L = /* @__PURE__ */ Ee(() => H === s(k)), F = /* @__PURE__ */ Ee(() => H === s(k) + 1);
        var J = Sd(), $e = i(J), w = i($e), D = i(w, !0);
        a(w);
        var z = o(w, 2), pe = i(z, !0);
        a(z), a($e);
        var Ce = o($e, 2);
        {
          var T = (I) => {
            var Z = Cd();
            Z.textContent = "→", $(() => Ie(Z, 1, `text-[10px]
									${s(M) ? "text-health-ok/40" : "text-surface-700"}`)), v(I, Z);
          };
          A(Ce, (I) => {
            H < m.length - 1 && I(T);
          });
        }
        a(J), $(() => {
          Ot($e, "title", s(V).label), Ie(w, 1, `text-sm transition-colors
									${s(M) ? "text-health-ok" : s(L) ? "text-hecate-400" : "text-surface-600"}`), h(D, s(M) ? "✓" : s(V).icon), Ie(z, 1, `text-[9px] transition-colors
									${s(M) ? "text-health-ok/70" : s(L) ? "text-hecate-300" : s(F) ? "text-surface-400" : "text-surface-600"}`), h(pe, s(V).label);
        }), v(C, J);
      }), a(te);
      var N = o(te, 2), X = i(N), ue = o(i(X), 2), Te = i(ue, !0);
      a(ue), a(X);
      var ne = o(X, 2), W = o(i(ne), 2), K = i(W, !0);
      a(W), a(ne);
      var Fe = o(ne, 2);
      {
        var et = (C) => {
          var V = Ed(), H = o(i(V), 2), M = i(H, !0);
          a(H), a(V), $(() => h(M, r().repos[0])), v(C, V);
        };
        A(Fe, (C) => {
          r().repos && r().repos.length > 0 && C(et);
        });
      }
      a(N);
      var st = o(N, 2);
      {
        var Xe = (C) => {
          var V = Ad(), H = i(V), M = o(i(H), 2);
          M.nodeValue = " — discover the domain events that define your system.", a(H);
          var L = o(H, 2);
          L.__click = g;
          var F = i(L, !0);
          a(L), a(V), $(() => {
            L.disabled = c(), Ie(L, 1, `px-5 py-2.5 rounded-lg text-sm font-medium transition-colors
							${c() ? "bg-surface-600 text-surface-400 cursor-not-allowed" : "bg-hecate-600 text-surface-50 hover:bg-hecate-500"}`), h(F, c() ? "Starting..." : "Start Discovery");
          }), v(C, V);
        }, We = (C) => {
          var V = Dd();
          v(C, V);
        }, Ge = (C) => {
          var V = Pd();
          v(C, V);
        };
        A(st, (C) => {
          d() === "discovery" && n() === "vision_submitted" ? C(Xe) : d() === "identify" ? C(We, 1) : C(Ge, !1);
        });
      }
      $(
        (C) => {
          h(Le, r().name), h(Te, r().status_label), h(K, C);
        },
        [() => f(r().initiated_at ?? 0)]
      ), v(ae, we);
    };
    A(Q, (ae) => {
      r() && ae(S);
    });
  }
  a(E), a(P), v(e, P);
  var U = $t(y);
  return u(), U;
}
Mt(["click"]);
Et(ea, { nextAction: {} }, [], [], { mode: "open" });
const yo = Ze(
  "You are Martha, an AI assistant specializing in software architecture and domain-driven design."
), Md = `You are The Oracle, a vision architect. You interview the user about their venture and build a vision document.

RULES:
1. Ask ONE question per response. Keep it short (2-3 sentences + question).
2. After EVERY response, include a vision draft inside a \`\`\`markdown code fence.
3. Cover 5 topics: Problem, Users, Capabilities, Constraints, Success Criteria.

Be warm but direct. Push for specifics when answers are vague.`, Id = "Be concise and practical. Suggest specific, actionable items. When suggesting domain elements, use snake_case naming. When suggesting events, use the format: {subject}_{verb_past}_v{N}.", Nd = [
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
], Ld = [
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
], Od = Ze(Nd), Fd = Ze(Ld), jd = Ze(Md), Bd = Ze(Id);
function Vd(e, t) {
  return e.replace(/\{\{(\w+)\}\}/g, (r, n) => t[n] ?? `{{${n}}}`);
}
var Gd = /* @__PURE__ */ p('<span class="text-[8px] text-hecate-400"></span>'), qd = /* @__PURE__ */ p('<span class="truncate"> </span> <!>', 1), Hd = /* @__PURE__ */ p('<span class="text-surface-500">Select model</span>'), Wd = /* @__PURE__ */ p('<span class="text-hecate-400 ml-1">(code-optimized)</span>'), Ud = /* @__PURE__ */ p('<button class="text-[9px] text-surface-500 hover:text-surface-300" title="Clear pinned model for this phase">Unpin</button>'), zd = /* @__PURE__ */ p('<div class="px-2 py-1.5 border-b border-surface-700 flex items-center justify-between"><span class="text-[9px] text-surface-400">Phase: <span class="text-surface-200"> </span> <!></span> <!></div>'), Yd = /* @__PURE__ */ p('<div class="p-3 text-center text-[11px] text-surface-500"> </div>'), Kd = /* @__PURE__ */ p('<span class="text-[9px] text-hecate-400 shrink-0"></span>'), Jd = /* @__PURE__ */ p('<div><span class="truncate flex-1"> </span> <span class="text-[9px] text-surface-500 shrink-0"> </span> <!></div>'), Qd = /* @__PURE__ */ p('<div class="py-1 border-b border-surface-700"><div class="px-2 py-1 text-[9px] text-hecate-400 uppercase tracking-wider font-medium">Recommended</div> <!></div>'), Xd = /* @__PURE__ */ p('<span class="text-[8px] w-3 text-center"> </span>'), Zd = /* @__PURE__ */ p('<span class="text-[8px] text-hecate-500 shrink-0"></span>'), eu = /* @__PURE__ */ p('<span class="text-[8px] text-hecate-400 shrink-0" title="Code model"></span>'), tu = /* @__PURE__ */ p('<span class="text-[8px] text-hecate-400 shrink-0" title="Pinned for this phase"></span>'), ru = /* @__PURE__ */ p('<span class="text-[9px] text-hecate-400 shrink-0"></span>'), su = /* @__PURE__ */ p('<button class="text-[8px] text-surface-600 hover:text-hecate-400 shrink-0">pin</button>'), au = /* @__PURE__ */ p('<div><span class="truncate flex-1"> </span> <!> <!> <!> <!> <!></div>'), nu = /* @__PURE__ */ p('<div class="py-0.5"><div><!> <span> </span> <span class="text-surface-600 font-normal"> </span></div> <!></div>'), iu = /* @__PURE__ */ p(`<div class="absolute top-full left-0 mt-1 w-80 max-h-[420px] overflow-hidden
				bg-surface-800 border border-surface-600 rounded-lg shadow-xl z-50
				flex flex-col"><div class="p-2 border-b border-surface-700"><input class="w-full bg-surface-700 border border-surface-600 rounded px-2 py-1
						text-[11px] text-surface-100 placeholder-surface-500
						focus:outline-none focus:border-hecate-500"/></div> <!> <div class="overflow-y-auto flex-1"><!> <!> <!></div></div>`), ou = /* @__PURE__ */ p(`<div class="relative"><button class="text-[10px] px-2 py-0.5 rounded bg-surface-700 text-surface-300
			hover:bg-surface-600 transition-colors truncate max-w-[200px] flex items-center gap-1"><!> <span class="text-[8px] ml-0.5"> </span></button> <!></div>`);
function wa(e, t) {
  kt(t, !0);
  const r = () => ke(yn, "$availableModels", n), [n, c] = Vt();
  let l = pt(t, "currentModel", 7), u = pt(t, "onSelect", 7), d = pt(t, "showPhaseInfo", 7, !1), f = pt(t, "phasePreference", 7, null), g = pt(t, "phaseAffinity", 7, "general"), m = pt(t, "onPinModel", 7), k = pt(t, "onClearPin", 7), y = pt(t, "phaseName", 7, ""), P = /* @__PURE__ */ oe(!1), E = /* @__PURE__ */ oe(""), Q = /* @__PURE__ */ oe(void 0), S = /* @__PURE__ */ oe(void 0), U = /* @__PURE__ */ oe(Bt(/* @__PURE__ */ new Set()));
  const ae = [
    "Anthropic",
    "OpenAI",
    "Google",
    "Groq",
    "Meta",
    "Ollama",
    "Alibaba",
    "Other"
  ], we = {
    Anthropic: /^claude-sonnet/,
    OpenAI: /^gpt-4o$/,
    Google: /^gemini-2\.5-flash$/,
    Groq: /^llama-3\.3/,
    Ollama: /./
  };
  let le = /* @__PURE__ */ Ee(() => {
    const C = r(), V = s(E).toLowerCase(), H = V ? C.filter((J) => J.toLowerCase().includes(V)) : C, M = /* @__PURE__ */ new Map();
    for (const J of H) {
      const $e = Le(J), w = M.get($e) ?? [];
      w.push(J), M.set($e, w);
    }
    const L = [], F = [...M.keys()].sort((J, $e) => {
      const w = ae.indexOf(J), D = ae.indexOf($e);
      return (w === -1 ? 999 : w) - (D === -1 ? 999 : D) || J.localeCompare($e);
    });
    for (const J of F) {
      const $e = M.get(J) ?? [], w = we[J], D = w ? $e.find((z) => w.test(z)) ?? null : null;
      L.push({ provider: J, models: $e, recommended: D });
    }
    return L;
  }), ve = /* @__PURE__ */ Ee(() => s(le).filter((C) => C.recommended !== null).map((C) => ({ provider: C.provider, model: C.recommended }))), de = /* @__PURE__ */ Ee(() => s(le).reduce((C, V) => C + V.models.length, 0));
  function Le(C) {
    return C.startsWith("claude") || C.startsWith("anthropic") ? "Anthropic" : C.startsWith("gemini") ? "Google" : C.startsWith("llama") || C.startsWith("meta-llama") ? "Meta" : C.startsWith("qwen") ? "Alibaba" : C.startsWith("groq/") ? "Groq" : C.startsWith("openai/") || C.startsWith("gpt") || C.startsWith("o1") || C.startsWith("o3") || C.startsWith("o4") ? "OpenAI" : C.startsWith("allam") || C.startsWith("moonshotai/") ? "Groq" : C.includes("/") ? C.split("/")[0] : "Ollama";
  }
  function Ae(C) {
    u()(C), x(P, !1), x(E, ""), x(U, /* @__PURE__ */ new Set(), !0);
  }
  function De(C) {
    const V = new Set(s(U));
    V.has(C) ? V.delete(C) : V.add(C), x(U, V, !0);
  }
  function te(C) {
    s(Q) && !s(Q).contains(C.target) && (x(P, !1), x(E, ""), x(U, /* @__PURE__ */ new Set(), !0));
  }
  function N(C) {
    return C.length <= 24 ? C : C.slice(0, 22) + "…";
  }
  function X(C) {
    C.key === "Escape" && (x(P, !1), x(E, ""));
  }
  Tt(() => (s(P) ? (document.addEventListener("click", te, !0), requestAnimationFrame(() => s(S)?.focus())) : document.removeEventListener("click", te, !0), () => document.removeEventListener("click", te, !0)));
  var ue = {
    get currentModel() {
      return l();
    },
    set currentModel(C) {
      l(C), vt();
    },
    get onSelect() {
      return u();
    },
    set onSelect(C) {
      u(C), vt();
    },
    get showPhaseInfo() {
      return d();
    },
    set showPhaseInfo(C = !1) {
      d(C), vt();
    },
    get phasePreference() {
      return f();
    },
    set phasePreference(C = null) {
      f(C), vt();
    },
    get phaseAffinity() {
      return g();
    },
    set phaseAffinity(C = "general") {
      g(C), vt();
    },
    get onPinModel() {
      return m();
    },
    set onPinModel(C) {
      m(C), vt();
    },
    get onClearPin() {
      return k();
    },
    set onClearPin(C) {
      k(C), vt();
    },
    get phaseName() {
      return y();
    },
    set phaseName(C = "") {
      y(C), vt();
    }
  }, Te = ou(), ne = i(Te);
  ne.__click = () => x(P, !s(P));
  var W = i(ne);
  {
    var K = (C) => {
      var V = qd(), H = ct(V), M = i(H, !0);
      a(H);
      var L = o(H, 2);
      {
        var F = ($e) => {
          var w = Gd();
          w.textContent = "•", v($e, w);
        }, J = /* @__PURE__ */ Ee(() => Wn(l()) === "code");
        A(L, ($e) => {
          s(J) && $e(F);
        });
      }
      $(($e) => h(M, $e), [() => N(l())]), v(C, V);
    }, Fe = (C) => {
      var V = Hd();
      v(C, V);
    };
    A(W, (C) => {
      l() ? C(K) : C(Fe, !1);
    });
  }
  var et = o(W, 2), st = i(et, !0);
  a(et), a(ne);
  var Xe = o(ne, 2);
  {
    var We = (C) => {
      var V = iu();
      V.__keydown = X;
      var H = i(V), M = i(H);
      bt(M), Xr(M, (Ce) => x(S, Ce), () => s(S)), a(H);
      var L = o(H, 2);
      {
        var F = (Ce) => {
          var T = zd(), I = i(T), Z = o(i(I)), Me = i(Z, !0);
          a(Z);
          var Be = o(Z, 2);
          {
            var Je = (qe) => {
              var Ue = Wd();
              v(qe, Ue);
            };
            A(Be, (qe) => {
              g() === "code" && qe(Je);
            });
          }
          a(I);
          var ze = o(I, 2);
          {
            var tt = (qe) => {
              var Ue = Ud();
              Ue.__click = () => k()?.(), v(qe, Ue);
            };
            A(ze, (qe) => {
              f() && qe(tt);
            });
          }
          a(T), $(() => h(Me, y())), v(Ce, T);
        };
        A(L, (Ce) => {
          d() && y() && Ce(F);
        });
      }
      var J = o(L, 2), $e = i(J);
      {
        var w = (Ce) => {
          var T = Yd(), I = i(T, !0);
          a(T), $(() => h(I, r().length === 0 ? "No models available" : "No matching models")), v(Ce, T);
        };
        A($e, (Ce) => {
          s(le).length === 0 && Ce(w);
        });
      }
      var D = o($e, 2);
      {
        var z = (Ce) => {
          var T = Qd(), I = o(i(T), 2);
          He(I, 17, () => s(ve), ft, (Z, Me) => {
            let Be = () => s(Me).provider, Je = () => s(Me).model;
            const ze = /* @__PURE__ */ Ee(() => Je() === l());
            var tt = Jd();
            tt.__click = () => Ae(Je());
            var qe = i(tt), Ue = i(qe, !0);
            a(qe);
            var Ye = o(qe, 2), je = i(Ye, !0);
            a(Ye);
            var R = o(Ye, 2);
            {
              var G = (fe) => {
                var Pe = Kd();
                Pe.textContent = "✓", v(fe, Pe);
              };
              A(R, (fe) => {
                s(ze) && fe(G);
              });
            }
            a(tt), $(() => {
              Ie(tt, 1, `w-full text-left px-2 py-1.5 text-[11px] flex items-center gap-1.5
									transition-colors cursor-pointer
									${s(ze) ? "bg-hecate-600/20 text-hecate-300" : "text-surface-200 hover:bg-surface-700"}`), h(Ue, Je()), h(je, Be());
            }), v(Z, tt);
          }), a(T), v(Ce, T);
        };
        A(D, (Ce) => {
          !s(E) && s(ve).length > 0 && Ce(z);
        });
      }
      var pe = o(D, 2);
      He(pe, 17, () => s(le), ft, (Ce, T) => {
        const I = /* @__PURE__ */ Ee(() => s(E) !== "" || s(U).has(s(T).provider));
        var Z = nu(), Me = i(Z);
        Me.__click = () => !s(E) && De(s(T).provider);
        var Be = i(Me);
        {
          var Je = (R) => {
            var G = Xd(), fe = i(G, !0);
            a(G), $(() => h(fe, s(I) ? "▼" : "▶")), v(R, G);
          };
          A(Be, (R) => {
            s(E) || R(Je);
          });
        }
        var ze = o(Be, 2), tt = i(ze, !0);
        a(ze);
        var qe = o(ze, 2), Ue = i(qe);
        a(qe), a(Me);
        var Ye = o(Me, 2);
        {
          var je = (R) => {
            var G = ur(), fe = ct(G);
            He(fe, 17, () => s(T).models, ft, (Pe, ie) => {
              const be = /* @__PURE__ */ Ee(() => s(ie) === l()), Ne = /* @__PURE__ */ Ee(() => s(ie) === f()), Oe = /* @__PURE__ */ Ee(() => s(ie) === s(T).recommended && !s(E));
              var _ = au();
              _.__click = () => Ae(s(ie));
              var b = i(_), re = i(b, !0);
              a(b);
              var j = o(b, 2);
              {
                var ce = (B) => {
                  var O = Zd();
                  O.textContent = "★", v(B, O);
                };
                A(j, (B) => {
                  s(Oe) && B(ce);
                });
              }
              var _e = o(j, 2);
              {
                var ge = (B) => {
                  var O = eu();
                  O.textContent = "• code", v(B, O);
                }, Y = /* @__PURE__ */ Ee(() => Wn(s(ie)) === "code");
                A(_e, (B) => {
                  s(Y) && B(ge);
                });
              }
              var q = o(_e, 2);
              {
                var ee = (B) => {
                  var O = tu();
                  O.textContent = "📌", v(B, O);
                };
                A(q, (B) => {
                  s(Ne) && B(ee);
                });
              }
              var me = o(q, 2);
              {
                var xe = (B) => {
                  var O = ru();
                  O.textContent = "✓", v(B, O);
                };
                A(me, (B) => {
                  s(be) && B(xe);
                });
              }
              var he = o(me, 2);
              {
                var se = (B) => {
                  var O = su();
                  O.__click = (ye) => {
                    ye.stopPropagation(), m()?.(s(ie));
                  }, $(() => Ot(O, "title", `Pin for ${y() ?? ""} phase`)), v(B, O);
                };
                A(he, (B) => {
                  d() && m() && !s(Ne) && B(se);
                });
              }
              a(_), $(() => {
                Ie(_, 1, `w-full text-left px-2 py-1.5 text-[11px] flex items-center gap-1.5
										transition-colors cursor-pointer
										${s(E) ? "pl-2" : "pl-6"}
										${s(be) ? "bg-hecate-600/20 text-hecate-300" : "text-surface-200 hover:bg-surface-700"}`), h(re, s(ie));
              }), v(Pe, _);
            }), v(R, G);
          };
          A(Ye, (R) => {
            s(I) && R(je);
          });
        }
        a(Z), $(() => {
          Ie(Me, 1, `px-2 py-1 text-[9px] uppercase tracking-wider font-medium flex items-center gap-1
								${s(E) ? "text-surface-500" : "text-surface-500 hover:text-surface-300 cursor-pointer"}`), h(tt, s(T).provider), h(Ue, `(${s(T).models.length ?? ""})`);
        }), v(Ce, Z);
      }), a(J), a(V), $(() => Ot(M, "placeholder", `Search ${s(de) ?? ""} models...`)), xt(M, () => s(E), (Ce) => x(E, Ce)), v(C, V);
    };
    A(Xe, (C) => {
      s(P) && C(We);
    });
  }
  a(Te), Xr(Te, (C) => x(Q, C), () => s(Q)), $(() => {
    Ot(ne, "title", l() ?? "No model selected"), h(st, s(P) ? "▲" : "▼");
  }), v(e, Te);
  var Ge = $t(ue);
  return c(), Ge;
}
Mt(["click", "keydown"]);
Et(
  wa,
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
var cu = /* @__PURE__ */ p(`<div class="flex justify-end"><div class="max-w-[85%] rounded-lg px-3 py-2 text-[11px]
							bg-hecate-600/20 text-surface-100 border border-hecate-600/20"><div class="whitespace-pre-wrap break-words"> </div></div></div>`), lu = /* @__PURE__ */ p(`<details class="mb-1.5 group"><summary class="text-[10px] text-surface-400 cursor-pointer hover:text-surface-300
											select-none flex items-center gap-1"><span class="text-[9px] transition-transform group-open:rotate-90"></span> Reasoning</summary> <div class="mt-1 pl-2 border-l-2 border-surface-600 text-[10px] text-surface-400
											whitespace-pre-wrap break-words max-h-32 overflow-y-auto"> </div></details>`), du = /* @__PURE__ */ p('<div class="whitespace-pre-wrap break-words"> </div>'), uu = /* @__PURE__ */ p('<div class="flex justify-start"><div></div></div>'), vu = /* @__PURE__ */ p(`<details class="group"><summary class="text-[10px] text-surface-500 cursor-pointer hover:text-surface-400
										select-none flex items-center gap-1"><span class="text-[9px] transition-transform group-open:rotate-90"></span> Show reasoning</summary> <div class="mt-1 pl-2 border-l-2 border-surface-600 text-[10px] text-surface-400
										whitespace-pre-wrap break-words max-h-32 overflow-y-auto"> <span class="inline-block w-1 h-3 bg-accent-400/50 animate-pulse ml-0.5"></span></div></details>`), fu = /* @__PURE__ */ p('<div class="flex items-center gap-2 text-surface-400 mb-1"><span class="flex gap-1"><span class="w-1.5 h-1.5 rounded-full bg-accent-500/60 animate-bounce" style="animation-delay: 0ms"></span> <span class="w-1.5 h-1.5 rounded-full bg-accent-500/60 animate-bounce" style="animation-delay: 150ms"></span> <span class="w-1.5 h-1.5 rounded-full bg-accent-500/60 animate-bounce" style="animation-delay: 300ms"></span></span> <span class="text-[10px] text-accent-400/70">Reasoning...</span></div> <!>', 1), pu = /* @__PURE__ */ p(`<details class="mb-1.5 group"><summary class="text-[10px] text-surface-400 cursor-pointer hover:text-surface-300
										select-none flex items-center gap-1"><span class="text-[9px] transition-transform group-open:rotate-90"></span> Reasoning</summary> <div class="mt-1 pl-2 border-l-2 border-surface-600 text-[10px] text-surface-400
										whitespace-pre-wrap break-words max-h-32 overflow-y-auto"> </div></details>`), xu = /* @__PURE__ */ p('<!> <div class="whitespace-pre-wrap break-words"> <span class="inline-block w-1.5 h-3 bg-hecate-400 animate-pulse ml-0.5"></span></div>', 1), hu = /* @__PURE__ */ p('<div class="flex items-center gap-1.5 text-surface-400"><span class="animate-bounce" style="animation-delay: 0ms">.</span> <span class="animate-bounce" style="animation-delay: 150ms">.</span> <span class="animate-bounce" style="animation-delay: 300ms">.</span></div>'), _u = /* @__PURE__ */ p(`<div class="flex justify-start"><div class="max-w-[85%] rounded-lg px-3 py-2 text-[11px]
						bg-surface-700 text-surface-200 border border-surface-600"><!></div></div>`), gu = /* @__PURE__ */ p('<div class="flex items-center justify-center h-full"><div class="text-center text-surface-400"><div class="text-2xl mb-2"></div> <div class="text-[11px]">The Oracle is preparing...</div></div></div>'), bu = /* @__PURE__ */ p('<span class="text-[10px] text-health-ok"></span>'), mu = /* @__PURE__ */ p('<span class="text-[10px] text-accent-400"></span>'), yu = /* @__PURE__ */ p('<span class="text-[10px] text-surface-400"></span>'), wu = /* @__PURE__ */ p('<span class="text-[10px] text-surface-400">Waiting for Oracle...</span>'), ku = /* @__PURE__ */ p('<div class="mt-4 p-2 rounded bg-surface-700 border border-surface-600"><div class="text-[9px] text-surface-400 uppercase tracking-wider mb-1">Brief</div> <div class="text-[11px] text-surface-200"> </div></div>'), $u = /* @__PURE__ */ p('<div class="prose prose-sm prose-invert"><!></div> <!>', 1), Cu = /* @__PURE__ */ p(`<div class="flex items-center justify-center h-full"><div class="text-center text-surface-400 max-w-[220px]"><div class="text-2xl mb-2"></div> <div class="text-[11px]">Your vision will take shape here as the Oracle
							gathers context about your venture.</div></div></div>`), Su = /* @__PURE__ */ p('<div class="text-[10px] text-health-err bg-health-err/10 rounded px-2 py-1"> </div>'), Eu = /* @__PURE__ */ p(`<div class="space-y-2"><div><label for="repo-path" class="text-[10px] text-surface-400 block mb-1">Repository Path</label> <input id="repo-path" placeholder="~/ventures/my-venture" class="w-full bg-surface-700 border border-surface-600 rounded px-3 py-1.5
								text-[11px] text-surface-100 placeholder-surface-400
								focus:outline-none focus:border-hecate-500"/></div> <!> <button> </button></div>`), Au = /* @__PURE__ */ p('<div class="text-center text-[10px] text-surface-400 py-2"></div>'), Du = /* @__PURE__ */ p('<div class="text-center text-[10px] text-surface-400 py-2">The Oracle will guide you through defining your venture</div>'), Pu = /* @__PURE__ */ p(`<div class="flex h-full overflow-hidden"><div class="flex flex-col overflow-hidden"><div class="flex items-center gap-2 px-4 py-2.5 border-b border-surface-600 shrink-0"><span class="text-hecate-400"></span> <span class="text-xs font-semibold text-surface-100">The Oracle</span> <span class="text-[10px] text-surface-400">Vision Architect</span> <div class="flex-1"></div> <!></div> <div class="flex-1 overflow-y-auto p-4 space-y-3"><!> <!> <!></div> <div class="border-t border-surface-600 p-3 shrink-0"><div class="flex gap-2"><textarea class="flex-1 bg-surface-700 border border-surface-600 rounded-lg px-3 py-2
						text-[11px] text-surface-100 placeholder-surface-400 resize-none
						focus:outline-none focus:border-hecate-500
						disabled:opacity-50 disabled:cursor-not-allowed"></textarea> <button>Send</button></div></div></div>  <div></div> <div class="flex flex-col overflow-hidden flex-1"><div class="flex items-center gap-2 px-4 py-2.5 border-b border-surface-600 shrink-0"><span class="text-surface-400 text-xs"></span> <span class="text-xs font-semibold text-surface-100">Vision Preview</span> <div class="flex-1"></div> <!></div> <div class="flex-1 overflow-y-auto p-4"><!></div> <div class="border-t border-surface-600 p-3 shrink-0"><!></div></div></div>`);
function wo(e, t) {
  kt(t, !0);
  const r = () => ke(St, "$activeVenture", l), n = () => ke(wn, "$aiModel", l), c = () => ke(wt, "$isLoading", l), [l, u] = Vt(), d = ro();
  let f = /* @__PURE__ */ oe(Bt([])), g = /* @__PURE__ */ oe(""), m = /* @__PURE__ */ oe(!1), k = /* @__PURE__ */ oe(""), y = /* @__PURE__ */ oe(void 0), P = /* @__PURE__ */ oe(!1), E = /* @__PURE__ */ oe(""), Q = /* @__PURE__ */ oe(""), S = /* @__PURE__ */ oe(null), U = /* @__PURE__ */ oe(null), ae = /* @__PURE__ */ oe(65), we = /* @__PURE__ */ oe(!1), le = /* @__PURE__ */ oe(void 0);
  function ve(_) {
    let b = _.replace(/```markdown\n[\s\S]*?```/g, "◇ Vision updated ↗");
    return b = b.replace(/```markdown\n[\s\S]*$/, "◇ Synthesizing vision... ↗"), b;
  }
  function de(_) {
    const b = ve(_), re = [];
    let j = b;
    for (; j.length > 0; ) {
      const ce = j.indexOf("<think>");
      if (ce === -1) {
        j.trim() && re.push({ type: "text", content: j });
        break;
      }
      if (ce > 0) {
        const Y = j.slice(0, ce);
        Y.trim() && re.push({ type: "text", content: Y });
      }
      const _e = j.indexOf("</think>", ce);
      if (_e === -1) {
        const Y = j.slice(ce + 7);
        Y.trim() && re.push({ type: "think", content: Y });
        break;
      }
      const ge = j.slice(ce + 7, _e);
      ge.trim() && re.push({ type: "think", content: ge }), j = j.slice(_e + 8);
    }
    return re.length > 0 ? re : [{ type: "text", content: b }];
  }
  function Le(_) {
    return _.includes("<think>") && !_.includes("</think>");
  }
  function Ae(_) {
    const b = ve(_);
    return b.includes("</think>") ? (b.split("</think>").pop() || "").trim() : b.includes("<think>") ? "" : b;
  }
  function De(_) {
    const b = ve(_), re = b.indexOf("<think>");
    if (re === -1) return "";
    const j = b.indexOf("</think>");
    return j === -1 ? b.slice(re + 7) : b.slice(re + 7, j);
  }
  let te = /* @__PURE__ */ Ee(() => {
    for (let _ = s(f).length - 1; _ >= 0; _--)
      if (s(f)[_].role === "assistant") {
        const b = s(f)[_].content.match(/```markdown\n([\s\S]*?)```/);
        if (b) return b[1].trim();
      }
    if (s(k)) {
      const _ = s(k).match(/```markdown\n([\s\S]*?)```/);
      if (_) return _[1].trim();
      const b = s(k).match(/```markdown\n([\s\S]*)$/);
      if (b) return b[1].trim();
    }
    return null;
  }), N = /* @__PURE__ */ Ee(() => s(te) !== null && !s(te).includes("(Not yet explored)") && !s(te).includes("*(Hypothetical)*")), X = /* @__PURE__ */ Ee(() => {
    if (!s(te)) return null;
    const _ = s(te).match(/<!--\s*brief:\s*(.*?)\s*-->/);
    return _ ? _[1].trim() : null;
  }), ue = /* @__PURE__ */ oe(null);
  Tt(() => {
    const _ = r(), b = _?.venture_id ?? null;
    if (b !== s(ue) && (x(f, [], !0), x(k, ""), x(m, !1), x(E, ""), x(Q, ""), x(ue, b, !0)), _ && !s(Q)) {
      const re = "~/ventures", j = _.name.toLowerCase().replace(/[^a-z0-9-]/g, "-");
      x(Q, `${re}/${j}`);
    }
  }), Tt(() => {
    const _ = n();
    s(U) !== null && s(U) !== _ && (s(S) && (s(S).cancel(), x(S, null)), x(f, [], !0), x(k, ""), x(m, !1)), x(U, _, !0);
  }), Tt(() => {
    const _ = r();
    if (_ && s(f).length === 0 && !s(m)) {
      const b = `I just initiated a new venture called "${_.name}". ${_.brief ? `Here's what I know so far: ${_.brief}` : "I need help defining the vision for this venture."}`;
      ne(b);
    }
  });
  function Te() {
    const _ = [], b = Qt(yo);
    b && _.push(b);
    const re = Qt(jd);
    if (_.push(Vd(re, { venture_name: r()?.name ?? "Unnamed" })), r()) {
      let j = `The venture is called "${r().name}"`;
      r().brief && (j += `. Initial brief: ${r().brief}`), _.push(j);
    }
    return _.join(`

---

`);
  }
  async function ne(_) {
    const b = n();
    if (!b || !_.trim() || s(m)) return;
    const re = { role: "user", content: _.trim() };
    x(f, [...s(f), re], !0), x(g, "");
    const j = [], ce = Te();
    ce && j.push({ role: "system", content: ce }), j.push(...s(f)), x(m, !0), x(k, "");
    let _e = "";
    const ge = d.stream.chat(b, j);
    x(S, ge, !0), ge.onChunk((Y) => {
      Y.content && (_e += Y.content, x(k, _e, !0));
    }).onDone(async (Y) => {
      Y.content && (_e += Y.content);
      const q = {
        role: "assistant",
        content: _e || "(empty response)"
      };
      x(f, [...s(f), q], !0), x(k, ""), x(m, !1), x(S, null);
    }).onError((Y) => {
      const q = { role: "assistant", content: `Error: ${Y}` };
      x(f, [...s(f), q], !0), x(k, ""), x(m, !1), x(S, null);
    });
    try {
      await ge.start();
    } catch (Y) {
      const q = { role: "assistant", content: `Error: ${String(Y)}` };
      x(f, [...s(f), q], !0), x(m, !1);
    }
  }
  async function W() {
    if (!r() || !s(te) || !s(Q).trim()) return;
    x(P, !0), x(E, ""), await co(r().venture_id, s(Q).trim(), s(te), r().name, s(X) ?? void 0) ? (await wr(), await ga()) : x(E, Qt(hr) || "Failed to scaffold venture repo", !0), x(P, !1);
  }
  let K = /* @__PURE__ */ oe(void 0);
  function Fe(_) {
    _.key === "Enter" && !_.shiftKey && (_.preventDefault(), ne(s(g)), s(K) && (s(K).style.height = "auto"));
  }
  function et(_) {
    const b = _.target;
    b.style.height = "auto", b.style.height = Math.min(b.scrollHeight, 150) + "px";
  }
  function st(_) {
    x(we, !0), _.preventDefault();
  }
  function Xe(_) {
    if (!s(we) || !s(le)) return;
    const b = s(le).getBoundingClientRect(), j = (_.clientX - b.left) / b.width * 100;
    x(ae, Math.max(30, Math.min(80, j)), !0);
  }
  function We() {
    x(we, !1);
  }
  Tt(() => {
    s(f), s(k), _n().then(() => {
      s(y) && (s(y).scrollTop = s(y).scrollHeight);
    });
  });
  function Ge(_) {
    return _.replace(/<!--.*?-->/gs, "").replace(/^### (.*$)/gm, '<h3 class="text-xs font-semibold text-surface-100 mt-3 mb-1">$1</h3>').replace(/^## (.*$)/gm, '<h2 class="text-sm font-semibold text-hecate-300 mt-4 mb-1.5">$1</h2>').replace(/^# (.*$)/gm, '<h1 class="text-base font-bold text-surface-100 mb-2">$1</h1>').replace(/^(\d+)\.\s+(.*$)/gm, '<div class="text-[11px] text-surface-200 ml-3 mb-1"><span class="text-surface-400 mr-1.5">$1.</span>$2</div>').replace(/^\- (.*$)/gm, '<div class="text-[11px] text-surface-200 ml-3 mb-1"><span class="text-surface-400 mr-1.5">&bull;</span>$1</div>').replace(/\*\*(.*?)\*\*/g, '<strong class="text-surface-100">$1</strong>').replace(/\*(.*?)\*/g, '<em class="text-surface-300">$1</em>').replace(/\n\n/g, "<br/><br/>").trim();
  }
  var C = Pu();
  C.__mousemove = Xe, C.__mouseup = We;
  var V = i(C), H = i(V), M = i(H);
  M.textContent = "◇";
  var L = o(M, 8);
  wa(L, {
    get currentModel() {
      return n();
    },
    onSelect: (_) => kn(_)
  }), a(H);
  var F = o(H, 2), J = i(F);
  He(J, 17, () => s(f), ft, (_, b) => {
    var re = ur(), j = ct(re);
    {
      var ce = (ge) => {
        var Y = cu(), q = i(Y), ee = i(q), me = i(ee, !0);
        a(ee), a(q), a(Y), $(() => h(me, s(b).content)), v(ge, Y);
      }, _e = (ge) => {
        var Y = uu(), q = i(Y);
        He(q, 21, () => de(s(b).content), ft, (ee, me) => {
          var xe = ur(), he = ct(xe);
          {
            var se = (O) => {
              var ye = lu(), Se = i(ye), Re = i(Se);
              Re.textContent = "▶", At(), a(Se);
              var Ve = o(Se, 2), Qe = i(Ve, !0);
              a(Ve), a(ye), $((rt) => h(Qe, rt), [() => s(me).content.trim()]), v(O, ye);
            }, B = (O) => {
              var ye = du(), Se = i(ye, !0);
              a(ye), $((Re) => h(Se, Re), [() => s(me).content.trim()]), v(O, ye);
            };
            A(he, (O) => {
              s(me).type === "think" ? O(se) : O(B, !1);
            });
          }
          v(ee, xe);
        }), a(q), a(Y), $(
          (ee) => Ie(q, 1, `max-w-[85%] rounded-lg px-3 py-2 text-[11px]
							bg-surface-700 text-surface-200 border border-surface-600
							${ee ?? ""}`),
          [
            () => s(b).content.startsWith("Error:") ? "border-health-err/30 text-health-err" : ""
          ]
        ), v(ge, Y);
      };
      A(j, (ge) => {
        s(b).role === "user" ? ge(ce) : s(b).role === "assistant" && ge(_e, 1);
      });
    }
    v(_, re);
  });
  var $e = o(J, 2);
  {
    var w = (_) => {
      var b = _u(), re = i(b), j = i(re);
      {
        var ce = (q) => {
          var ee = fu(), me = o(ct(ee), 2);
          {
            var xe = (se) => {
              var B = vu(), O = i(B), ye = i(O);
              ye.textContent = "▶", At(), a(O);
              var Se = o(O, 2), Re = i(Se, !0);
              At(), a(Se), a(B), $((Ve) => h(Re, Ve), [
                () => De(s(k)).trim()
              ]), v(se, B);
            }, he = /* @__PURE__ */ Ee(() => De(s(k)).trim());
            A(me, (se) => {
              s(he) && se(xe);
            });
          }
          v(q, ee);
        }, _e = /* @__PURE__ */ Ee(() => s(k) && Le(s(k))), ge = (q) => {
          var ee = xu(), me = ct(ee);
          {
            var xe = (O) => {
              var ye = pu(), Se = i(ye), Re = i(Se);
              Re.textContent = "▶", At(), a(Se);
              var Ve = o(Se, 2), Qe = i(Ve, !0);
              a(Ve), a(ye), $((rt) => h(Qe, rt), [
                () => De(s(k)).trim()
              ]), v(O, ye);
            }, he = /* @__PURE__ */ Ee(() => De(s(k)).trim());
            A(me, (O) => {
              s(he) && O(xe);
            });
          }
          var se = o(me, 2), B = i(se, !0);
          At(), a(se), $((O) => h(B, O), [() => Ae(s(k))]), v(q, ee);
        }, Y = (q) => {
          var ee = hu();
          v(q, ee);
        };
        A(j, (q) => {
          s(_e) ? q(ce) : s(k) ? q(ge, 1) : q(Y, !1);
        });
      }
      a(re), a(b), v(_, b);
    };
    A($e, (_) => {
      s(m) && _(w);
    });
  }
  var D = o($e, 2);
  {
    var z = (_) => {
      var b = gu(), re = i(b), j = i(re);
      j.textContent = "◇", At(2), a(re), a(b), v(_, b);
    };
    A(D, (_) => {
      s(f).length === 0 && !s(m) && _(z);
    });
  }
  a(F), Xr(F, (_) => x(y, _), () => s(y));
  var pe = o(F, 2), Ce = i(pe), T = i(Ce);
  Ws(T), T.__keydown = Fe, T.__input = et, Ot(T, "rows", 1), Xr(T, (_) => x(K, _), () => s(K));
  var I = o(T, 2);
  I.__click = () => ne(s(g)), a(Ce), a(pe), a(V);
  var Z = o(V, 2);
  Z.__mousedown = st;
  var Me = o(Z, 2), Be = i(Me), Je = i(Be);
  Je.textContent = "📄";
  var ze = o(Je, 6);
  {
    var tt = (_) => {
      var b = bu();
      b.textContent = "● Complete", v(_, b);
    }, qe = (_) => {
      var b = mu();
      b.textContent = "◐ Drafting...", v(_, b);
    }, Ue = (_) => {
      var b = yu();
      b.textContent = "◐ Listening...", v(_, b);
    }, Ye = (_) => {
      var b = wu();
      v(_, b);
    };
    A(ze, (_) => {
      s(N) ? _(tt) : s(te) ? _(qe, 1) : s(m) ? _(Ue, 2) : _(Ye, !1);
    });
  }
  a(Be);
  var je = o(Be, 2), R = i(je);
  {
    var G = (_) => {
      var b = $u(), re = ct(b), j = i(re);
      Uc(j, () => Ge(s(te))), a(re);
      var ce = o(re, 2);
      {
        var _e = (ge) => {
          var Y = ku(), q = o(i(Y), 2), ee = i(q, !0);
          a(q), a(Y), $(() => h(ee, s(X))), v(ge, Y);
        };
        A(ce, (ge) => {
          s(X) && ge(_e);
        });
      }
      v(_, b);
    }, fe = (_) => {
      var b = Cu(), re = i(b), j = i(re);
      j.textContent = "📄", At(2), a(re), a(b), v(_, b);
    };
    A(R, (_) => {
      s(te) ? _(G) : _(fe, !1);
    });
  }
  a(je);
  var Pe = o(je, 2), ie = i(Pe);
  {
    var be = (_) => {
      var b = Eu(), re = i(b), j = o(i(re), 2);
      bt(j), a(re);
      var ce = o(re, 2);
      {
        var _e = (q) => {
          var ee = Su(), me = i(ee, !0);
          a(ee), $(() => h(me, s(E))), v(q, ee);
        };
        A(ce, (q) => {
          s(E) && q(_e);
        });
      }
      var ge = o(ce, 2);
      ge.__click = W;
      var Y = i(ge, !0);
      a(ge), a(b), $(
        (q, ee) => {
          ge.disabled = q, Ie(ge, 1, `w-full px-3 py-2 rounded-lg text-xs font-medium transition-colors
							${ee ?? ""}`), h(Y, s(P) ? "Scaffolding..." : "Scaffold Venture");
        },
        [
          () => s(P) || c() || !s(Q).trim(),
          () => s(P) || c() || !s(Q).trim() ? "bg-surface-600 text-surface-400 cursor-not-allowed" : "bg-hecate-600 text-surface-50 hover:bg-hecate-500"
        ]
      ), xt(j, () => s(Q), (q) => x(Q, q)), v(_, b);
    }, Ne = (_) => {
      var b = Au();
      b.textContent = "Vision is taking shape — keep exploring with the Oracle", v(_, b);
    }, Oe = (_) => {
      var b = Du();
      v(_, b);
    };
    A(ie, (_) => {
      s(N) ? _(be) : s(te) ? _(Ne, 1) : _(Oe, !1);
    });
  }
  a(Pe), a(Me), a(C), Xr(C, (_) => x(le, _), () => s(le)), $(
    (_, b) => {
      gr(V, `width: ${s(ae) ?? ""}%`), Ot(T, "placeholder", s(m) ? "Oracle is thinking..." : "Describe your venture..."), T.disabled = s(m) || !n(), I.disabled = _, Ie(I, 1, `px-3 rounded-lg text-[11px] transition-colors self-end
						${b ?? ""}`), Ie(Z, 1, `w-1 cursor-col-resize shrink-0 transition-colors
			${s(we) ? "bg-hecate-500" : "bg-surface-600 hover:bg-surface-500"}`);
    },
    [
      () => s(m) || !s(g).trim() || !n(),
      () => s(m) || !s(g).trim() || !n() ? "bg-surface-600 text-surface-400 cursor-not-allowed" : "bg-hecate-600 text-surface-50 hover:bg-hecate-500"
    ]
  ), Nt("mouseleave", C, We), xt(T, () => s(g), (_) => x(g, _)), v(e, C), $t(), u();
}
Mt([
  "mousemove",
  "mouseup",
  "keydown",
  "input",
  "click",
  "mousedown"
]);
Et(wo, {}, [], [], { mode: "open" });
var Tu = /* @__PURE__ */ p("<div></div>"), Ru = /* @__PURE__ */ p('<!> <div><span class="svelte-gwxd3p"> </span> <span class="svelte-gwxd3p"> </span></div>', 1), Mu = /* @__PURE__ */ p("<span> </span>"), Iu = /* @__PURE__ */ p("<span> </span>"), Nu = /* @__PURE__ */ p(
  `<button title="Toggle event stream viewer">Stream</button> <button class="text-[9px] px-2 py-0.5 rounded ml-1
						text-surface-400 hover:text-health-warn hover:bg-surface-700 transition-colors svelte-gwxd3p" title="Shelve storm">Shelve</button>`,
  1
), Lu = /* @__PURE__ */ p(`<button class="flex items-center gap-1 text-[10px] px-2 py-1 rounded
										text-surface-400 hover:text-hecate-300
										hover:bg-hecate-600/10 transition-colors svelte-gwxd3p"><span class="svelte-gwxd3p"> </span> <span class="svelte-gwxd3p"> </span></button>`), Ou = /* @__PURE__ */ p(`<div class="flex items-center justify-center h-full svelte-gwxd3p"><div class="text-center max-w-lg mx-4 svelte-gwxd3p"><div class="text-4xl mb-4 text-es-event svelte-gwxd3p"></div> <h2 class="text-lg font-semibold text-surface-100 mb-3 svelte-gwxd3p">Big Picture Event Storming</h2> <p class="text-xs text-surface-400 leading-relaxed mb-6 svelte-gwxd3p">Discover the domain landscape by storming events onto the board.
						Start with a 10-minute high octane phase where everyone
						(including AI agents) throws domain events as fast as possible. <br class="svelte-gwxd3p"/><br class="svelte-gwxd3p"/> Volume over quality. The thick stacks reveal what matters.
						Natural clusters become your divisions (bounded contexts).</p> <div class="flex flex-col items-center gap-4 svelte-gwxd3p"><button class="px-6 py-3 rounded-lg text-sm font-medium
								bg-es-event text-surface-50 hover:bg-es-event/90
								transition-colors shadow-lg shadow-es-event/20 svelte-gwxd3p"></button> <div class="flex gap-2 svelte-gwxd3p"></div></div></div></div>`), Fu = /* @__PURE__ */ p(`<div class="group relative px-3 py-2 rounded text-xs
									bg-es-event/15 border border-es-event/30 text-surface-100
									hover:border-es-event/50 transition-all duration-200
									storm-sticky svelte-gwxd3p"><span class="svelte-gwxd3p"> </span> <span class="text-[8px] text-es-event/60 ml-1.5 svelte-gwxd3p"> </span> <button class="absolute -top-1 -right-1 w-4 h-4 rounded-full
										bg-surface-700 border border-surface-600
										text-surface-400 hover:text-health-err
										text-[8px] flex items-center justify-center
										opacity-0 group-hover:opacity-100 transition-opacity svelte-gwxd3p"></button></div>`), ju = /* @__PURE__ */ p(`<div class="group relative px-3 py-2 rounded text-xs
									border-2 border-dashed border-es-event/40 text-surface-300
									opacity-50 hover:opacity-80 transition-all duration-200
									storm-sticky ghost-sticky svelte-gwxd3p"><span class="italic svelte-gwxd3p"> </span> <span class="text-[8px] text-es-event/40 ml-1.5 svelte-gwxd3p">oracle</span> <div class="absolute -top-1 -right-1 flex gap-0.5
									opacity-0 group-hover:opacity-100 transition-opacity svelte-gwxd3p"><button class="w-4 h-4 rounded-full bg-health-ok/20 border border-health-ok/40
											text-health-ok text-[8px] flex items-center justify-center
											hover:bg-health-ok/30 svelte-gwxd3p" title="Accept"></button> <button class="w-4 h-4 rounded-full bg-surface-700 border border-surface-600
											text-surface-400 hover:text-health-err
											text-[8px] flex items-center justify-center svelte-gwxd3p" title="Dismiss"></button></div></div>`), Bu = /* @__PURE__ */ p('<div class="text-surface-500 text-xs italic svelte-gwxd3p">Start throwing events! Type below or ask an AI agent...</div>'), Vu = /* @__PURE__ */ p(`<button class="flex items-center gap-1 text-[9px] px-1.5 py-0.5 rounded
										text-surface-400 hover:text-hecate-300
										hover:bg-hecate-600/10 transition-colors svelte-gwxd3p"><span class="svelte-gwxd3p"> </span> <span class="svelte-gwxd3p"> </span></button>`), Gu = /* @__PURE__ */ p(`<div class="flex flex-col h-full svelte-gwxd3p"><div class="flex-1 overflow-y-auto p-4 svelte-gwxd3p"><div class="flex flex-wrap gap-2 content-start storm-board svelte-gwxd3p"><!> <!> <!></div></div> <div class="border-t border-surface-600 p-3 shrink-0 svelte-gwxd3p"><div class="flex gap-2 mb-2 svelte-gwxd3p"><input placeholder="Type a domain event (past tense)... e.g., order_placed" class="flex-1 bg-surface-700 border border-es-event/30 rounded px-3 py-2
								text-xs text-surface-100 placeholder-surface-400
								focus:outline-none focus:border-es-event svelte-gwxd3p"/> <button>Add</button></div> <div class="flex items-center justify-between svelte-gwxd3p"><div class="flex gap-1.5 svelte-gwxd3p"></div> <button class="text-[10px] px-3 py-1 rounded
								bg-surface-700 text-surface-300
								hover:text-surface-100 hover:bg-surface-600 transition-colors svelte-gwxd3p"></button></div></div></div>`), qu = /* @__PURE__ */ p('<span class="text-[8px] px-1 rounded bg-es-event/20 text-es-event svelte-gwxd3p"> </span>'), Hu = /* @__PURE__ */ p(`<div draggable="true" class="group flex items-center gap-1.5 px-2 py-1.5 rounded text-[11px]
											bg-es-event/15 border border-es-event/30 text-surface-100
											cursor-grab active:cursor-grabbing hover:border-es-event/50 svelte-gwxd3p"><span class="flex-1 truncate svelte-gwxd3p"> </span> <!></div>`), Wu = /* @__PURE__ */ p(`<div class="group flex items-center gap-1 px-2 py-1 rounded text-[10px]
														bg-es-event/10 text-surface-200 svelte-gwxd3p"><span class="flex-1 truncate svelte-gwxd3p"> </span> <button class="text-[8px] text-surface-500 hover:text-surface-300
															opacity-0 group-hover:opacity-100 svelte-gwxd3p" title="Unstack"></button></div>`), Uu = /* @__PURE__ */ p('<div><div class="flex items-center gap-2 mb-2 svelte-gwxd3p"><span class="text-[10px] font-bold text-es-event svelte-gwxd3p"> </span> <span class="text-[9px] text-surface-500 font-mono svelte-gwxd3p"> </span></div> <div class="space-y-1 svelte-gwxd3p"></div></div>'), zu = /* @__PURE__ */ p(`<div class="col-span-2 text-center py-8 text-surface-500 text-xs
											border border-dashed border-surface-600 rounded-lg svelte-gwxd3p">Drag stickies onto each other to create stacks.</div>`), Yu = /* @__PURE__ */ p(`<button class="flex items-center gap-1 text-[9px] px-1.5 py-0.5 rounded
										text-surface-400 hover:text-hecate-300
										hover:bg-hecate-600/10 transition-colors svelte-gwxd3p"><span class="svelte-gwxd3p"> </span> <span class="svelte-gwxd3p"> </span></button>`), Ku = /* @__PURE__ */ p(`<div class="flex flex-col h-full svelte-gwxd3p"><div class="flex-1 overflow-y-auto p-4 svelte-gwxd3p"><p class="text-xs text-surface-400 mb-3 svelte-gwxd3p">Drag duplicate or related stickies onto each other to form stacks.
						Thick stacks reveal what matters most.</p> <div class="flex gap-4 svelte-gwxd3p"><div class="w-64 shrink-0 svelte-gwxd3p"><h3 class="text-[10px] font-semibold text-surface-300 mb-2 uppercase tracking-wider svelte-gwxd3p"> </h3> <div class="space-y-1 min-h-[200px] rounded-lg border border-dashed border-surface-600 p-2 svelte-gwxd3p"></div></div> <div class="flex-1 svelte-gwxd3p"><h3 class="text-[10px] font-semibold text-surface-300 mb-2 uppercase tracking-wider svelte-gwxd3p"> </h3> <div class="grid grid-cols-2 gap-3 svelte-gwxd3p"><!> <!></div></div></div></div> <div class="border-t border-surface-600 p-3 shrink-0 svelte-gwxd3p"><div class="flex items-center justify-between svelte-gwxd3p"><div class="flex gap-1.5 svelte-gwxd3p"></div> <button class="text-[10px] px-3 py-1 rounded transition-colors
								bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30 svelte-gwxd3p"></button></div></div></div>`), Ju = /* @__PURE__ */ p('<button><span></span> <span class="flex-1 svelte-gwxd3p"> </span> <span class="text-[8px] text-surface-400 svelte-gwxd3p"> </span></button>'), Qu = /* @__PURE__ */ p('<div class="rounded-lg border border-surface-600 bg-surface-800 p-4 svelte-gwxd3p"><div class="flex items-center gap-2 mb-3 svelte-gwxd3p"><span class="text-xs font-semibold text-surface-200 svelte-gwxd3p"> </span> <div class="flex-1 svelte-gwxd3p"></div> <button></button></div> <div class="space-y-1.5 svelte-gwxd3p"></div></div>'), Xu = /* @__PURE__ */ p('<div class="space-y-4 mb-6 svelte-gwxd3p"></div>'), Zu = /* @__PURE__ */ p(`<div class="text-center py-8 text-surface-500 text-xs
									border border-dashed border-surface-600 rounded-lg mb-6 svelte-gwxd3p">No stacks to groom. All stickies are unique.</div>`), ev = /* @__PURE__ */ p('<span class="text-[8px] text-es-event ml-1 svelte-gwxd3p"> </span>'), tv = /* @__PURE__ */ p(`<span class="text-[10px] px-2 py-1 rounded
												bg-es-event/10 text-surface-200 svelte-gwxd3p"> <!></span>`), rv = /* @__PURE__ */ p('<div class="svelte-gwxd3p"><h3 class="text-[10px] font-semibold text-surface-300 mb-2 uppercase tracking-wider svelte-gwxd3p"> </h3> <div class="flex flex-wrap gap-1.5 svelte-gwxd3p"></div></div>'), sv = /* @__PURE__ */ p(`<div class="flex flex-col h-full svelte-gwxd3p"><div class="flex-1 overflow-y-auto p-4 svelte-gwxd3p"><div class="max-w-2xl mx-auto svelte-gwxd3p"><p class="text-xs text-surface-400 mb-4 svelte-gwxd3p">For each stack, select the best representative sticky. The winner
							gets the stack's weight (vote count). Other stickies are absorbed.</p> <!> <!></div></div> <div class="border-t border-surface-600 p-3 shrink-0 svelte-gwxd3p"><div class="flex items-center justify-end svelte-gwxd3p"><button class="text-[10px] px-3 py-1 rounded transition-colors
								bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30 svelte-gwxd3p"></button></div></div></div>`), av = /* @__PURE__ */ p('<span class="text-[8px] px-1 rounded bg-es-event/20 text-es-event svelte-gwxd3p"> </span>'), nv = /* @__PURE__ */ p(`<div draggable="true" class="group flex items-center gap-1.5 px-2 py-1.5 rounded text-[11px]
											bg-es-event/15 border border-es-event/30 text-surface-100
											cursor-grab active:cursor-grabbing hover:border-es-event/50 svelte-gwxd3p"><span class="flex-1 truncate svelte-gwxd3p"> </span> <!></div>`), iv = /* @__PURE__ */ p('<div class="text-[10px] text-surface-500 text-center py-4 italic svelte-gwxd3p">All events clustered</div>'), ov = /* @__PURE__ */ p('<span class="text-[8px] text-es-event/60 svelte-gwxd3p"> </span>'), cv = /* @__PURE__ */ p(`<div draggable="true" class="group flex items-center gap-1 px-2 py-1 rounded text-[10px]
														bg-es-event/10 text-surface-200
														cursor-grab active:cursor-grabbing svelte-gwxd3p"><span class="flex-1 truncate svelte-gwxd3p"> </span> <!> <button class="text-[8px] text-surface-500 hover:text-surface-300
															opacity-0 group-hover:opacity-100 svelte-gwxd3p" title="Remove from cluster"></button></div>`), lv = /* @__PURE__ */ p('<div><div class="flex items-center gap-2 mb-2 svelte-gwxd3p"><div class="w-3 h-3 rounded-sm shrink-0 svelte-gwxd3p"></div> <span class="flex-1 text-xs font-semibold text-surface-100 truncate svelte-gwxd3p"> </span> <span class="text-[9px] text-surface-400 svelte-gwxd3p"> </span> <button class="text-[9px] text-surface-500 hover:text-health-err transition-colors svelte-gwxd3p" title="Dissolve cluster"></button></div> <div class="space-y-1 svelte-gwxd3p"></div></div>'), dv = /* @__PURE__ */ p(`<div class="col-span-2 text-center py-8 text-surface-500 text-xs
											border border-dashed border-surface-600 rounded-lg svelte-gwxd3p">Drag stickies onto each other to create clusters.</div>`), uv = /* @__PURE__ */ p(`<button class="flex items-center gap-1 text-[9px] px-1.5 py-0.5 rounded
										text-surface-400 hover:text-hecate-300
										hover:bg-hecate-600/10 transition-colors svelte-gwxd3p"><span class="svelte-gwxd3p"> </span> <span class="svelte-gwxd3p"> </span></button>`), vv = /* @__PURE__ */ p(`<div class="flex flex-col h-full svelte-gwxd3p"><div class="flex-1 overflow-y-auto p-4 svelte-gwxd3p"><p class="text-xs text-surface-400 mb-3 svelte-gwxd3p">Drag related stickies onto each other to form clusters.
						Clusters become candidate divisions (bounded contexts).</p> <div class="flex gap-4 svelte-gwxd3p"><div class="w-64 shrink-0 svelte-gwxd3p"><h3 class="text-[10px] font-semibold text-surface-300 mb-2 uppercase tracking-wider svelte-gwxd3p"> </h3> <div class="space-y-1 min-h-[200px] rounded-lg border border-dashed border-surface-600 p-2 svelte-gwxd3p"><!> <!></div></div> <div class="flex-1 svelte-gwxd3p"><h3 class="text-[10px] font-semibold text-surface-300 mb-2 uppercase tracking-wider svelte-gwxd3p"> </h3> <div class="grid grid-cols-2 gap-3 svelte-gwxd3p"><!> <!></div></div></div></div> <div class="border-t border-surface-600 p-3 shrink-0 svelte-gwxd3p"><div class="flex items-center justify-between svelte-gwxd3p"><div class="flex gap-1.5 svelte-gwxd3p"></div> <button></button></div></div></div>`), fv = /* @__PURE__ */ p(`<input class="flex-1 bg-surface-700 border border-surface-500 rounded px-3 py-1.5
													text-sm text-surface-100 focus:outline-none focus:border-hecate-500 svelte-gwxd3p" placeholder="division_name (snake_case)"/>`), pv = /* @__PURE__ */ p('<button title="Click to name"> </button>'), xv = /* @__PURE__ */ p('<span class="text-es-event/50 svelte-gwxd3p"> </span>'), hv = /* @__PURE__ */ p(`<span class="text-[9px] px-1.5 py-0.5 rounded
													bg-es-event/10 text-es-event/80 svelte-gwxd3p"> <!></span>`), _v = /* @__PURE__ */ p('<div class="rounded-lg border bg-surface-800 p-4 svelte-gwxd3p"><div class="flex items-center gap-3 mb-2 svelte-gwxd3p"><div class="w-4 h-4 rounded svelte-gwxd3p"></div> <!> <span class="text-[10px] text-surface-400 svelte-gwxd3p"> </span></div> <div class="flex flex-wrap gap-1.5 ml-7 svelte-gwxd3p"></div></div>'), gv = /* @__PURE__ */ p(`<div class="flex flex-col h-full svelte-gwxd3p"><div class="flex-1 overflow-y-auto p-4 svelte-gwxd3p"><div class="max-w-2xl mx-auto svelte-gwxd3p"><p class="text-xs text-surface-400 mb-4 svelte-gwxd3p">Name each cluster as a bounded context (division). These become
							the divisions in your venture. Use snake_case naming.</p> <div class="space-y-3 svelte-gwxd3p"></div></div></div> <div class="border-t border-surface-600 p-3 shrink-0 svelte-gwxd3p"><div class="flex items-center justify-end svelte-gwxd3p"><button class="text-[10px] px-3 py-1 rounded
								bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30 transition-colors svelte-gwxd3p"></button></div></div></div>`), bv = /* @__PURE__ */ p('<div class="px-4 py-2 rounded-lg border-2 text-xs font-semibold text-surface-100 svelte-gwxd3p"> <span class="text-[9px] text-surface-400 ml-1 svelte-gwxd3p"> </span></div>'), mv = /* @__PURE__ */ p(`<div class="flex items-center gap-2 px-3 py-1.5 rounded
												bg-surface-800 border border-surface-600 text-xs svelte-gwxd3p"><span class="px-1.5 py-0.5 rounded text-[10px] font-medium svelte-gwxd3p"> </span> <span class="text-surface-400 svelte-gwxd3p"></span> <span class="text-es-event font-mono text-[10px] svelte-gwxd3p"> </span> <span class="text-surface-400 svelte-gwxd3p"></span> <span class="px-1.5 py-0.5 rounded text-[10px] font-medium svelte-gwxd3p"> </span> <div class="flex-1 svelte-gwxd3p"></div> <button class="text-surface-500 hover:text-health-err text-[9px] transition-colors svelte-gwxd3p"></button></div>`), yv = /* @__PURE__ */ p('<div class="space-y-1.5 mb-4 svelte-gwxd3p"></div>'), wv = /* @__PURE__ */ p('<option class="svelte-gwxd3p"> </option>'), kv = /* @__PURE__ */ p('<option class="svelte-gwxd3p"> </option>'), $v = /* @__PURE__ */ p(`<div class="rounded-lg border border-surface-600 bg-surface-800 p-4 svelte-gwxd3p"><h4 class="text-[10px] font-semibold text-surface-300 uppercase tracking-wider mb-3 svelte-gwxd3p">Add Integration Fact</h4> <div class="flex items-end gap-2 svelte-gwxd3p"><div class="flex-1 svelte-gwxd3p"><label class="text-[9px] text-surface-400 block mb-1 svelte-gwxd3p">From (publishes)</label> <select class="w-full bg-surface-700 border border-surface-600 rounded px-2 py-1.5
												text-[10px] text-surface-100 focus:outline-none focus:border-hecate-500 svelte-gwxd3p"><option class="svelte-gwxd3p">Select...</option><!></select></div> <div class="flex-1 svelte-gwxd3p"><label class="text-[9px] text-surface-400 block mb-1 svelte-gwxd3p">Fact name</label> <input placeholder="e.g., order_confirmed" class="w-full bg-surface-700 border border-surface-600 rounded px-2 py-1.5
												text-[10px] text-surface-100 placeholder-surface-400
												focus:outline-none focus:border-hecate-500 svelte-gwxd3p"/></div> <div class="flex-1 svelte-gwxd3p"><label class="text-[9px] text-surface-400 block mb-1 svelte-gwxd3p">To (consumes)</label> <select class="w-full bg-surface-700 border border-surface-600 rounded px-2 py-1.5
												text-[10px] text-surface-100 focus:outline-none focus:border-hecate-500 svelte-gwxd3p"><option class="svelte-gwxd3p">Select...</option><!></select></div> <button>Add</button></div></div>`), Cv = /* @__PURE__ */ p(`<button class="flex items-center gap-1 text-[9px] px-1.5 py-0.5 rounded
										text-surface-400 hover:text-hecate-300
										hover:bg-hecate-600/10 transition-colors svelte-gwxd3p"><span class="svelte-gwxd3p"> </span> <span class="svelte-gwxd3p"> </span></button>`), Sv = /* @__PURE__ */ p(`<div class="flex flex-col h-full svelte-gwxd3p"><div class="flex-1 overflow-y-auto p-4 svelte-gwxd3p"><div class="max-w-3xl mx-auto svelte-gwxd3p"><p class="text-xs text-surface-400 mb-4 svelte-gwxd3p">Map how divisions communicate. Each arrow represents an
							integration fact that flows from one context to another.
							This is your Context Map.</p> <div class="mb-6 svelte-gwxd3p"><div class="flex flex-wrap gap-3 justify-center mb-4 svelte-gwxd3p"></div> <!></div> <!></div></div> <div class="border-t border-surface-600 p-3 shrink-0 svelte-gwxd3p"><div class="flex items-center justify-between svelte-gwxd3p"><div class="flex gap-2 svelte-gwxd3p"></div> <button> </button></div></div></div>`), Ev = /* @__PURE__ */ p(`<div class="flex items-center justify-center h-full svelte-gwxd3p"><div class="text-center max-w-md mx-4 svelte-gwxd3p"><div class="text-4xl mb-4 text-health-ok svelte-gwxd3p"></div> <h2 class="text-lg font-semibold text-surface-100 mb-2 svelte-gwxd3p">Context Map Complete</h2> <p class="text-xs text-surface-400 mb-4 svelte-gwxd3p"> </p> <p class="text-xs text-surface-400 mb-6 svelte-gwxd3p">Select a division from the sidebar to begin Design-Level
						Event Storming in its DnA phase.</p> <button class="text-[10px] px-3 py-1 rounded
							text-surface-400 hover:text-surface-200 hover:bg-surface-700 transition-colors svelte-gwxd3p">Reset Board</button></div></div>`), Av = /* @__PURE__ */ p(`<div class="flex items-center justify-center h-full svelte-gwxd3p"><div class="text-center max-w-md mx-4 svelte-gwxd3p"><div class="text-4xl mb-4 text-health-warn svelte-gwxd3p"></div> <h2 class="text-lg font-semibold text-surface-100 mb-2 svelte-gwxd3p">Storm Shelved</h2> <p class="text-xs text-surface-400 mb-6 svelte-gwxd3p">This storm session has been shelved. You can resume it at any time
						to continue where you left off.</p> <button class="px-6 py-3 rounded-lg text-sm font-medium
							bg-hecate-600 text-surface-50 hover:bg-hecate-500
							transition-colors svelte-gwxd3p">Resume Storm</button></div></div>`), Dv = /* @__PURE__ */ p('<div class="flex flex-col h-full svelte-gwxd3p"><div class="border-b border-surface-600 bg-surface-800/50 px-4 py-2 shrink-0 svelte-gwxd3p"><div class="flex items-center gap-1 svelte-gwxd3p"><span class="text-xs text-surface-400 mr-2 svelte-gwxd3p">Big Picture</span> <!> <div class="flex-1 svelte-gwxd3p"></div> <!> <!> <!></div></div> <div><!></div></div>');
const Pv = {
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
function Qa(e, t) {
  kt(t, !0), Ji(e, Pv);
  const r = () => ke(Od, "$bigPictureAgents", E), n = () => ke(yl, "$bigPictureEventCount", E), c = () => ke(as, "$bigPicturePhase", E), l = () => ke(St, "$activeVenture", E), u = () => ke(ks, "$bigPictureEvents", E), d = () => ke(ya, "$eventClusters", E), f = () => ke(Cn, "$factArrows", E), g = () => ke(ca, "$highOctaneRemaining", E), m = () => ke(Ua, "$showEventStream", E), k = () => ke(ml, "$stickyStacks", E), y = () => ke(bl, "$unclusteredEvents", E), P = () => ke(za, "$isLoading", E), [E, Q] = Vt();
  let S = /* @__PURE__ */ oe(""), U = /* @__PURE__ */ oe(null), ae = /* @__PURE__ */ oe(""), we = /* @__PURE__ */ oe(null), le = /* @__PURE__ */ oe(null), ve = /* @__PURE__ */ oe(""), de = /* @__PURE__ */ oe(null), Le = /* @__PURE__ */ oe(Bt({})), Ae = /* @__PURE__ */ oe(Bt(/* @__PURE__ */ new Map()));
  function De(R) {
    return s(Ae).has(R) || s(Ae).set(R, {
      rotate: (Math.random() - 0.5) * 6,
      // -3 to +3 degrees
      dx: (Math.random() - 0.5) * 4,
      // -2 to +2 px
      dy: (Math.random() - 0.5) * 4
    }), s(Ae).get(R);
  }
  let te = /* @__PURE__ */ oe(Bt([]));
  async function N(R) {
    await Qs(K(), R.text, "oracle"), x(te, s(te).filter((G) => G.id !== R.id), !0);
  }
  function X(R) {
    x(te, s(te).filter((G) => G.id !== R.id), !0);
  }
  let ue = /* @__PURE__ */ oe(!1), Te = /* @__PURE__ */ oe(0);
  Tt(() => {
    const R = n();
    R > s(Te) && s(Te) > 0 && (x(ue, !0), setTimeout(() => x(ue, !1), 300)), x(Te, R, !0);
  });
  let ne = /* @__PURE__ */ oe(!1), W = /* @__PURE__ */ oe("");
  Tt(() => {
    const R = c();
    R !== s(W) && s(W) !== "" && (x(ne, !0), setTimeout(() => x(ne, !1), 300)), x(W, R, !0);
  });
  function K() {
    return l()?.venture_id ?? "";
  }
  function Fe(R) {
    const G = Math.floor(R / 60), fe = R % 60;
    return `${G}:${fe.toString().padStart(2, "0")}`;
  }
  async function et(R) {
    R.key === "Enter" && !R.shiftKey && s(S).trim() && (R.preventDefault(), await Qs(K(), s(S)), x(S, ""));
  }
  async function st(R, G) {
    R.key === "Enter" && s(ae).trim() ? (await Al(K(), G, s(ae).trim()), x(U, null), x(ae, "")) : R.key === "Escape" && x(U, null);
  }
  function Xe(R) {
    x(U, R.cluster_id, !0), x(ae, R.name ?? "", !0);
  }
  async function We() {
    s(we) && s(le) && s(we) !== s(le) && s(ve).trim() && (await Dl(K(), s(we), s(le), s(ve).trim()), x(ve, ""));
  }
  async function Ge() {
    await Il(K());
  }
  function C(R) {
    return u().filter((G) => G.cluster_id === R);
  }
  let V = /* @__PURE__ */ Ee(() => u().filter((R) => !R.stack_id));
  function H(R) {
    const G = l(), fe = u(), Pe = d(), ie = f();
    let be = R + `

---

`;
    if (G && (be += `Venture: "${G.name}"`, G.brief && (be += ` — ${G.brief}`), be += `

`), fe.length > 0 && (be += `Events on the board:
`, be += fe.map((Ne) => `- ${Ne.text}${Ne.weight > 1 ? ` (x${Ne.weight})` : ""}`).join(`
`), be += `

`), Pe.length > 0) {
      be += `Current clusters (candidate divisions):
`;
      for (const Ne of Pe) {
        const Oe = fe.filter((_) => _.cluster_id === Ne.cluster_id);
        be += `- ${Ne.name ?? "(unnamed)"}: ${Oe.map((_) => _.text).join(", ") || "(empty)"}
`;
      }
      be += `
`;
    }
    if (ie.length > 0) {
      be += `Integration fact arrows:
`;
      for (const Ne of ie) {
        const Oe = Pe.find((b) => b.cluster_id === Ne.from_cluster)?.name ?? "?", _ = Pe.find((b) => b.cluster_id === Ne.to_cluster)?.name ?? "?";
        be += `- ${Oe} → ${Ne.fact_name} → ${_}
`;
      }
    }
    return be;
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
  Tt(() => {
    const R = l();
    R && zt(R.venture_id);
  });
  var L = Dv(), F = i(L), J = i(F), $e = o(i(J), 2);
  He($e, 17, () => M, ft, (R, G, fe) => {
    const Pe = /* @__PURE__ */ Ee(() => c() === s(G).phase), ie = /* @__PURE__ */ Ee(() => M.findIndex((_e) => _e.phase === c()) > fe);
    var be = Ru(), Ne = ct(be);
    {
      var Oe = (_e) => {
        var ge = Tu();
        $(() => Ie(ge, 1, `w-6 h-px ${s(ie) ? "bg-hecate-400/60" : "bg-surface-600"}`, "svelte-gwxd3p")), v(_e, ge);
      };
      A(Ne, (_e) => {
        fe > 0 && _e(Oe);
      });
    }
    var _ = o(Ne, 2), b = i(_), re = i(b, !0);
    a(b);
    var j = o(b, 2), ce = i(j, !0);
    a(j), a(_), $(() => {
      Ie(
        _,
        1,
        `flex items-center gap-1 px-2 py-1 rounded text-[10px]
						${s(Pe) ? "bg-surface-700 border border-hecate-500/40 text-hecate-300" : s(ie) ? "text-hecate-400/60" : "text-surface-500"}`,
        "svelte-gwxd3p"
      ), h(re, s(G).icon), h(ce, s(G).label);
    }), v(R, be);
  });
  var w = o($e, 4);
  {
    var D = (R) => {
      var G = Mu(), fe = i(G);
      a(G), $(() => {
        Ie(
          G,
          1,
          `text-[10px] transition-all duration-300
						${s(ue) ? "scale-110 text-es-event font-bold" : "text-surface-400"}`,
          "svelte-gwxd3p"
        ), h(fe, `${n() ?? ""} events`);
      }), v(R, G);
    };
    A(w, (R) => {
      c() !== "ready" && c() !== "promoted" && c() !== "shelved" && R(D);
    });
  }
  var z = o(w, 2);
  {
    var pe = (R) => {
      var G = Iu(), fe = i(G, !0);
      a(G), $(
        (Pe) => {
          Ie(
            G,
            1,
            `text-sm font-bold tabular-nums ml-2
						${g() <= 60 ? "text-health-err animate-pulse" : g() <= 180 ? "text-health-warn" : "text-es-event"}`,
            "svelte-gwxd3p"
          ), h(fe, Pe);
        },
        [() => Fe(g())]
      ), v(R, G);
    };
    A(z, (R) => {
      c() === "storm" && R(pe);
    });
  }
  var Ce = o(z, 2);
  {
    var T = (R) => {
      var G = Nu(), fe = ct(G);
      fe.__click = () => Ua.update((ie) => !ie);
      var Pe = o(fe, 2);
      Pe.__click = () => Rl(K()), $(() => Ie(
        fe,
        1,
        `text-[9px] px-2 py-0.5 rounded ml-1
						${m() ? "text-hecate-300 bg-hecate-600/20" : "text-surface-400 hover:text-surface-200 hover:bg-surface-700"} transition-colors`,
        "svelte-gwxd3p"
      )), v(R, G);
    };
    A(Ce, (R) => {
      c() !== "ready" && c() !== "promoted" && c() !== "shelved" && R(T);
    });
  }
  a(J), a(F);
  var I = o(F, 2), Z = i(I);
  {
    var Me = (R) => {
      var G = Ou(), fe = i(G), Pe = i(fe);
      Pe.textContent = "⚡";
      var ie = o(Pe, 6), be = i(ie);
      be.__click = () => wl(K()), be.textContent = "⚡ Start High Octane (10 min)";
      var Ne = o(be, 2);
      He(Ne, 5, r, ft, (Oe, _) => {
        var b = Lu();
        b.__click = () => Cr(H(s(_).prompt), s(_).id);
        var re = i(b), j = i(re, !0);
        a(re);
        var ce = o(re, 2), _e = i(ce, !0);
        a(ce), a(b), $(() => {
          Ot(b, "title", s(_).description), h(j, s(_).icon), h(_e, s(_).name);
        }), v(Oe, b);
      }), a(Ne), a(ie), a(fe), a(G), v(R, G);
    }, Be = (R) => {
      var G = Gu(), fe = i(G), Pe = i(fe), ie = i(Pe);
      He(ie, 1, u, (Y) => Y.sticky_id, (Y, q) => {
        const ee = /* @__PURE__ */ Ee(() => De(s(q).sticky_id));
        var me = Fu(), xe = i(me), he = i(xe, !0);
        a(xe);
        var se = o(xe, 2), B = i(se, !0);
        a(se);
        var O = o(se, 2);
        O.__click = () => kl(K(), s(q).sticky_id), O.textContent = "✕", a(me), $(() => {
          gr(me, `transform: rotate(${s(ee).rotate ?? ""}deg) translate(${s(ee).dx ?? ""}px, ${s(ee).dy ?? ""}px)`), h(he, s(q).text), h(B, s(q).author === "user" ? "" : s(q).author);
        }), v(Y, me);
      });
      var be = o(ie, 2);
      He(be, 17, () => s(te), (Y) => Y.id, (Y, q) => {
        var ee = ju();
        gr(ee, `transform: rotate(${(Math.random() - 0.5) * 4}deg)`);
        var me = i(ee), xe = i(me, !0);
        a(me);
        var he = o(me, 4), se = i(he);
        se.__click = () => N(s(q)), se.textContent = "✓";
        var B = o(se, 2);
        B.__click = () => X(s(q)), B.textContent = "✕", a(he), a(ee), $(() => h(xe, s(q).text)), v(Y, ee);
      });
      var Ne = o(be, 2);
      {
        var Oe = (Y) => {
          var q = Bu();
          v(Y, q);
        };
        A(Ne, (Y) => {
          u().length === 0 && s(te).length === 0 && Y(Oe);
        });
      }
      a(Pe), a(fe);
      var _ = o(fe, 2), b = i(_), re = i(b);
      bt(re), re.__keydown = et;
      var j = o(re, 2);
      j.__click = async () => {
        s(S).trim() && (await Qs(K(), s(S)), x(S, ""));
      }, a(b);
      var ce = o(b, 2), _e = i(ce);
      He(_e, 5, r, ft, (Y, q) => {
        var ee = Vu();
        ee.__click = () => Cr(H(s(q).prompt), s(q).id);
        var me = i(ee), xe = i(me, !0);
        a(me);
        var he = o(me, 2), se = i(he, !0);
        a(he), a(ee), $(() => {
          Ot(ee, "title", s(q).description), h(xe, s(q).icon), h(se, s(q).role);
        }), v(Y, ee);
      }), a(_e);
      var ge = o(_e, 2);
      ge.__click = () => Es(K(), "stack"), ge.textContent = "End Storm → Stack", a(ce), a(_), a(G), $(
        (Y, q) => {
          j.disabled = Y, Ie(
            j,
            1,
            `px-3 py-2 rounded text-xs transition-colors
								${q ?? ""}`,
            "svelte-gwxd3p"
          );
        },
        [
          () => !s(S).trim(),
          () => s(S).trim() ? "bg-es-event text-surface-50 hover:bg-es-event/80" : "bg-surface-600 text-surface-400 cursor-not-allowed"
        ]
      ), xt(re, () => s(S), (Y) => x(S, Y)), v(R, G);
    }, Je = (R) => {
      var G = Ku(), fe = i(G), Pe = o(i(fe), 2), ie = i(Pe), be = i(ie), Ne = i(be);
      a(be);
      var Oe = o(be, 2);
      He(Oe, 21, () => s(V), (xe) => xe.sticky_id, (xe, he) => {
        var se = Hu(), B = i(se), O = i(B, !0);
        a(B);
        var ye = o(B, 2);
        {
          var Se = (Re) => {
            var Ve = qu(), Qe = i(Ve);
            a(Ve), $(() => h(Qe, `x${s(he).weight ?? ""}`)), v(Re, Ve);
          };
          A(ye, (Re) => {
            s(he).weight > 1 && Re(Se);
          });
        }
        a(se), $(() => h(O, s(he).text)), Nt("dragstart", se, () => x(de, s(he).sticky_id, !0)), Nt("dragend", se, () => x(de, null)), Nt("dragover", se, (Re) => Re.preventDefault()), Nt("drop", se, () => {
          s(de) && s(de) !== s(he).sticky_id && (Yn(K(), s(de), s(he).sticky_id), x(de, null));
        }), v(xe, se);
      }), a(Oe), a(ie);
      var _ = o(ie, 2), b = i(_), re = i(b);
      a(b);
      var j = o(b, 2), ce = i(j);
      He(ce, 1, () => [...k().entries()], ([xe, he]) => xe, (xe, he) => {
        var se = /* @__PURE__ */ Ee(() => Ta(s(he), 2));
        let B = () => s(se)[0], O = () => s(se)[1];
        var ye = Uu(), Se = i(ye), Re = i(Se), Ve = i(Re);
        a(Re);
        var Qe = o(Re, 2), rt = i(Qe, !0);
        a(Qe), a(Se);
        var at = o(Se, 2);
        He(at, 21, O, (nt) => nt.sticky_id, (nt, Ct) => {
          var ht = Wu(), It = i(ht), qt = i(It, !0);
          a(It);
          var gt = o(It, 2);
          gt.__click = () => $l(K(), s(Ct).sticky_id), gt.textContent = "↩", a(ht), $(() => h(qt, s(Ct).text)), v(nt, ht);
        }), a(at), a(ye), $(
          (nt) => {
            Ie(
              ye,
              1,
              `rounded-lg border-2 p-3 min-h-[80px] transition-colors
											${s(de) ? "border-dashed border-hecate-500/50 bg-hecate-600/5" : "border-surface-600 bg-surface-800"}`,
              "svelte-gwxd3p"
            ), h(Ve, `${O().length ?? ""}x`), h(rt, nt);
          },
          [() => B().slice(0, 8)]
        ), Nt("dragover", ye, (nt) => nt.preventDefault()), Nt("drop", ye, () => {
          s(de) && O().length > 0 && (Yn(K(), s(de), O()[0].sticky_id), x(de, null));
        }), v(xe, ye);
      });
      var _e = o(ce, 2);
      {
        var ge = (xe) => {
          var he = zu();
          v(xe, he);
        };
        A(_e, (xe) => {
          k().size === 0 && xe(ge);
        });
      }
      a(j), a(_), a(Pe), a(fe);
      var Y = o(fe, 2), q = i(Y), ee = i(q);
      He(ee, 5, () => r().slice(0, 2), ft, (xe, he) => {
        var se = Yu();
        se.__click = () => Cr(H(s(he).prompt), s(he).id);
        var B = i(se), O = i(B, !0);
        a(B);
        var ye = o(B, 2), Se = i(ye);
        a(ye), a(se), $(() => {
          h(O, s(he).icon), h(Se, `Ask ${s(he).name ?? ""}`);
        }), v(xe, se);
      }), a(ee);
      var me = o(ee, 2);
      me.__click = () => Es(K(), "groom"), me.textContent = "Groom Stacks →", a(q), a(Y), a(G), $(() => {
        h(Ne, `Stickies (${s(V).length ?? ""})`), h(re, `Stacks (${k().size ?? ""})`);
      }), v(R, G);
    }, ze = (R) => {
      var G = sv(), fe = i(G), Pe = i(fe), ie = o(i(Pe), 2);
      {
        var be = (ce) => {
          var _e = Xu();
          He(_e, 5, () => [...k().entries()], ([ge, Y]) => ge, (ge, Y) => {
            var q = /* @__PURE__ */ Ee(() => Ta(s(Y), 2));
            let ee = () => s(q)[0], me = () => s(q)[1];
            const xe = /* @__PURE__ */ Ee(() => s(Le)[ee()]);
            var he = Qu(), se = i(he), B = i(se), O = i(B);
            a(B);
            var ye = o(B, 4);
            ye.__click = () => {
              s(xe) && Cl(K(), ee(), s(xe));
            }, ye.textContent = "Groom ✂", a(se);
            var Se = o(se, 2);
            He(Se, 21, me, (Re) => Re.sticky_id, (Re, Ve) => {
              var Qe = Ju();
              Qe.__click = () => x(Le, { ...s(Le), [ee()]: s(Ve).sticky_id }, !0);
              var rt = i(Qe), at = o(rt, 2), nt = i(at, !0);
              a(at);
              var Ct = o(at, 2), ht = i(Ct, !0);
              a(Ct), a(Qe), $(() => {
                Ie(
                  Qe,
                  1,
                  `w-full text-left flex items-center gap-2 px-3 py-2 rounded text-[11px]
														transition-colors
														${s(xe) === s(Ve).sticky_id ? "bg-hecate-600/20 border border-hecate-500/40 text-hecate-200" : "bg-surface-700/50 border border-transparent text-surface-200 hover:border-surface-500"}`,
                  "svelte-gwxd3p"
                ), Ie(
                  rt,
                  1,
                  `w-3 h-3 rounded-full border-2 shrink-0
															${s(xe) === s(Ve).sticky_id ? "border-hecate-400 bg-hecate-400" : "border-surface-500"}`,
                  "svelte-gwxd3p"
                ), h(nt, s(Ve).text), h(ht, s(Ve).author === "user" ? "" : s(Ve).author);
              }), v(Re, Qe);
            }), a(Se), a(he), $(() => {
              h(O, `Stack (${me().length ?? ""} stickies)`), ye.disabled = !s(xe), Ie(
                ye,
                1,
                `text-[10px] px-2 py-1 rounded transition-colors
													${s(xe) ? "bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30" : "bg-surface-700 text-surface-500 cursor-not-allowed"}`,
                "svelte-gwxd3p"
              );
            }), v(ge, he);
          }), a(_e), v(ce, _e);
        }, Ne = (ce) => {
          var _e = Zu();
          v(ce, _e);
        };
        A(ie, (ce) => {
          k().size > 0 ? ce(be) : ce(Ne, !1);
        });
      }
      var Oe = o(ie, 2);
      {
        var _ = (ce) => {
          var _e = rv(), ge = i(_e), Y = i(ge);
          a(ge);
          var q = o(ge, 2);
          He(q, 21, () => s(V), (ee) => ee.sticky_id, (ee, me) => {
            var xe = tv(), he = i(xe), se = o(he);
            {
              var B = (O) => {
                var ye = ev(), Se = i(ye);
                a(ye), $(() => h(Se, `x${s(me).weight ?? ""}`)), v(O, ye);
              };
              A(se, (O) => {
                s(me).weight > 1 && O(B);
              });
            }
            a(xe), $(() => h(he, `${s(me).text ?? ""} `)), v(ee, xe);
          }), a(q), a(_e), $(() => h(Y, `Standalone Stickies (${s(V).length ?? ""})`)), v(ce, _e);
        };
        A(Oe, (ce) => {
          s(V).length > 0 && ce(_);
        });
      }
      a(Pe), a(fe);
      var b = o(fe, 2), re = i(b), j = i(re);
      j.__click = () => Es(K(), "cluster"), j.textContent = "Cluster Events →", a(re), a(b), a(G), v(R, G);
    }, tt = (R) => {
      var G = vv(), fe = i(G), Pe = o(i(fe), 2), ie = i(Pe), be = i(ie), Ne = i(be);
      a(be);
      var Oe = o(be, 2), _ = i(Oe);
      He(_, 1, y, (B) => B.sticky_id, (B, O) => {
        var ye = nv(), Se = i(ye), Re = i(Se, !0);
        a(Se);
        var Ve = o(Se, 2);
        {
          var Qe = (rt) => {
            var at = av(), nt = i(at);
            a(at), $(() => h(nt, `x${s(O).weight ?? ""}`)), v(rt, at);
          };
          A(Ve, (rt) => {
            s(O).weight > 1 && rt(Qe);
          });
        }
        a(ye), $(() => h(Re, s(O).text)), Nt("dragstart", ye, () => x(de, s(O).sticky_id, !0)), Nt("dragend", ye, () => x(de, null)), Nt("dragover", ye, (rt) => rt.preventDefault()), Nt("drop", ye, () => {
          s(de) && s(de) !== s(O).sticky_id && (Kn(K(), s(de), s(O).sticky_id), x(de, null));
        }), v(B, ye);
      });
      var b = o(_, 2);
      {
        var re = (B) => {
          var O = iv();
          v(B, O);
        };
        A(b, (B) => {
          y().length === 0 && B(re);
        });
      }
      a(Oe), a(ie);
      var j = o(ie, 2), ce = i(j), _e = i(ce);
      a(ce);
      var ge = o(ce, 2), Y = i(ge);
      He(Y, 1, d, (B) => B.cluster_id, (B, O) => {
        const ye = /* @__PURE__ */ Ee(() => C(s(O).cluster_id));
        var Se = lv(), Re = i(Se), Ve = i(Re), Qe = o(Ve, 2), rt = i(Qe, !0);
        a(Qe);
        var at = o(Qe, 2), nt = i(at, !0);
        a(at);
        var Ct = o(at, 2);
        Ct.__click = () => El(K(), s(O).cluster_id), Ct.textContent = "✕", a(Re);
        var ht = o(Re, 2);
        He(ht, 21, () => s(ye), (It) => It.sticky_id, (It, qt) => {
          var gt = cv(), Ht = i(gt), kr = i(Ht, !0);
          a(Ht);
          var Pn = o(Ht, 2);
          {
            var Io = (Ca) => {
              var Sa = ov(), No = i(Sa);
              a(Sa), $(() => h(No, `x${s(qt).weight ?? ""}`)), v(Ca, Sa);
            };
            A(Pn, (Ca) => {
              s(qt).weight > 1 && Ca(Io);
            });
          }
          var Tn = o(Pn, 2);
          Tn.__click = () => Sl(K(), s(qt).sticky_id), Tn.textContent = "↩", a(gt), $(() => h(kr, s(qt).text)), Nt("dragstart", gt, () => x(de, s(qt).sticky_id, !0)), Nt("dragend", gt, () => x(de, null)), v(It, gt);
        }), a(ht), a(Se), $(() => {
          Ie(
            Se,
            1,
            `rounded-lg border-2 p-3 min-h-[120px] transition-colors
											${s(de) ? "border-dashed border-hecate-500/50 bg-hecate-600/5" : "border-surface-600 bg-surface-800"}`,
            "svelte-gwxd3p"
          ), gr(Se, `border-color: ${s(de) ? "" : s(O).color + "40"}`), gr(Ve, `background-color: ${s(O).color ?? ""}`), h(rt, s(O).name ?? "Unnamed"), h(nt, s(ye).length);
        }), Nt("dragover", Se, (It) => It.preventDefault()), Nt("drop", Se, () => {
          s(de) && s(ye).length > 0 && (Kn(K(), s(de), s(ye)[0].sticky_id), x(de, null));
        }), v(B, Se);
      });
      var q = o(Y, 2);
      {
        var ee = (B) => {
          var O = dv();
          v(B, O);
        };
        A(q, (B) => {
          d().length === 0 && B(ee);
        });
      }
      a(ge), a(j), a(Pe), a(fe);
      var me = o(fe, 2), xe = i(me), he = i(xe);
      He(he, 5, () => r().slice(0, 2), ft, (B, O) => {
        var ye = uv();
        ye.__click = () => Cr(H(s(O).prompt), s(O).id);
        var Se = i(ye), Re = i(Se, !0);
        a(Se);
        var Ve = o(Se, 2), Qe = i(Ve);
        a(Ve), a(ye), $(() => {
          h(Re, s(O).icon), h(Qe, `Ask ${s(O).name ?? ""}`);
        }), v(B, ye);
      }), a(he);
      var se = o(he, 2);
      se.__click = () => Es(K(), "name"), se.textContent = "Name Divisions →", a(xe), a(me), a(G), $(() => {
        h(Ne, `Unclustered (${y().length ?? ""})`), h(_e, `Clusters (${d().length ?? ""})`), se.disabled = d().length === 0, Ie(
          se,
          1,
          `text-[10px] px-3 py-1 rounded transition-colors
								${d().length === 0 ? "bg-surface-700 text-surface-500 cursor-not-allowed" : "bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30"}`,
          "svelte-gwxd3p"
        );
      }), v(R, G);
    }, qe = (R) => {
      var G = gv(), fe = i(G), Pe = i(fe), ie = o(i(Pe), 2);
      He(ie, 5, d, (_) => _.cluster_id, (_, b) => {
        const re = /* @__PURE__ */ Ee(() => C(s(b).cluster_id));
        var j = _v(), ce = i(j), _e = i(ce), ge = o(_e, 2);
        {
          var Y = (he) => {
            var se = fv();
            bt(se), se.__keydown = (B) => st(B, s(b).cluster_id), Nt("blur", se, () => x(U, null)), xt(se, () => s(ae), (B) => x(ae, B)), v(he, se);
          }, q = (he) => {
            var se = pv();
            se.__click = () => Xe(s(b));
            var B = i(se, !0);
            a(se), $(() => {
              Ie(
                se,
                1,
                `flex-1 text-left text-sm font-semibold transition-colors
													${s(b).name ? "text-surface-100 hover:text-hecate-300" : "text-surface-400 italic hover:text-hecate-300"}`,
                "svelte-gwxd3p"
              ), h(B, s(b).name ?? "Click to name...");
            }), v(he, se);
          };
          A(ge, (he) => {
            s(U) === s(b).cluster_id ? he(Y) : he(q, !1);
          });
        }
        var ee = o(ge, 2), me = i(ee);
        a(ee), a(ce);
        var xe = o(ce, 2);
        He(xe, 21, () => s(re), (he) => he.sticky_id, (he, se) => {
          var B = hv(), O = i(B), ye = o(O);
          {
            var Se = (Re) => {
              var Ve = xv(), Qe = i(Ve);
              a(Ve), $(() => h(Qe, `x${s(se).weight ?? ""}`)), v(Re, Ve);
            };
            A(ye, (Re) => {
              s(se).weight > 1 && Re(Se);
            });
          }
          a(B), $(() => h(O, `${s(se).text ?? ""} `)), v(he, B);
        }), a(xe), a(j), $(() => {
          gr(j, `border-color: ${s(b).color ?? ""}40`), gr(_e, `background-color: ${s(b).color ?? ""}`), h(me, `${s(re).length ?? ""} events`);
        }), v(_, j);
      }), a(ie), a(Pe), a(fe);
      var be = o(fe, 2), Ne = i(be), Oe = i(Ne);
      Oe.__click = () => Es(K(), "map"), Oe.textContent = "Map Integration Facts →", a(Ne), a(be), a(G), v(R, G);
    }, Ue = (R) => {
      var G = Sv(), fe = i(G), Pe = i(fe), ie = o(i(Pe), 2), be = i(ie);
      He(be, 5, d, (Y) => Y.cluster_id, (Y, q) => {
        var ee = bv(), me = i(ee), xe = o(me), he = i(xe);
        a(xe), a(ee), $(
          (se) => {
            gr(ee, `border-color: ${s(q).color ?? ""}; background-color: ${s(q).color ?? ""}15`), h(me, `${s(q).name ?? "Unnamed" ?? ""} `), h(he, `(${se ?? ""})`);
          },
          [() => C(s(q).cluster_id).length]
        ), v(Y, ee);
      }), a(be);
      var Ne = o(be, 2);
      {
        var Oe = (Y) => {
          var q = yv();
          He(q, 5, f, (ee) => ee.arrow_id, (ee, me) => {
            const xe = /* @__PURE__ */ Ee(() => d().find((nt) => nt.cluster_id === s(me).from_cluster)), he = /* @__PURE__ */ Ee(() => d().find((nt) => nt.cluster_id === s(me).to_cluster));
            var se = mv(), B = i(se), O = i(B, !0);
            a(B);
            var ye = o(B, 2);
            ye.textContent = "→";
            var Se = o(ye, 2), Re = i(Se, !0);
            a(Se);
            var Ve = o(Se, 2);
            Ve.textContent = "→";
            var Qe = o(Ve, 2), rt = i(Qe, !0);
            a(Qe);
            var at = o(Qe, 4);
            at.__click = () => Pl(K(), s(me).arrow_id), at.textContent = "✕", a(se), $(() => {
              gr(B, `color: ${s(xe)?.color ?? "#888" ?? ""}; background-color: ${s(xe)?.color ?? "#888" ?? ""}15`), h(O, s(xe)?.name ?? "?"), h(Re, s(me).fact_name), gr(Qe, `color: ${s(he)?.color ?? "#888" ?? ""}; background-color: ${s(he)?.color ?? "#888" ?? ""}15`), h(rt, s(he)?.name ?? "?");
            }), v(ee, se);
          }), a(q), v(Y, q);
        };
        A(Ne, (Y) => {
          f().length > 0 && Y(Oe);
        });
      }
      a(ie);
      var _ = o(ie, 2);
      {
        var b = (Y) => {
          var q = $v(), ee = o(i(q), 2), me = i(ee), xe = o(i(me), 2), he = i(xe);
          he.value = (he.__value = null) ?? "";
          var se = o(he);
          He(se, 1, d, ft, (rt, at) => {
            var nt = wv(), Ct = i(nt, !0);
            a(nt);
            var ht = {};
            $(() => {
              h(Ct, s(at).name ?? "Unnamed"), ht !== (ht = s(at).cluster_id) && (nt.value = (nt.__value = s(at).cluster_id) ?? "");
            }), v(rt, nt);
          }), a(xe), a(me);
          var B = o(me, 2), O = o(i(B), 2);
          bt(O), a(B);
          var ye = o(B, 2), Se = o(i(ye), 2), Re = i(Se);
          Re.value = (Re.__value = null) ?? "";
          var Ve = o(Re);
          He(Ve, 1, d, ft, (rt, at) => {
            var nt = kv(), Ct = i(nt, !0);
            a(nt);
            var ht = {};
            $(() => {
              h(Ct, s(at).name ?? "Unnamed"), ht !== (ht = s(at).cluster_id) && (nt.value = (nt.__value = s(at).cluster_id) ?? "");
            }), v(rt, nt);
          }), a(Se), a(ye);
          var Qe = o(ye, 2);
          Qe.__click = We, a(ee), a(q), $(
            (rt, at) => {
              Qe.disabled = rt, Ie(
                Qe,
                1,
                `px-3 py-1.5 rounded text-[10px] transition-colors shrink-0
											${at ?? ""}`,
                "svelte-gwxd3p"
              );
            },
            [
              () => !s(we) || !s(le) || s(we) === s(le) || !s(ve).trim(),
              () => s(we) && s(le) && s(we) !== s(le) && s(ve).trim() ? "bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30" : "bg-surface-700 text-surface-500 cursor-not-allowed"
            ]
          ), js(xe, () => s(we), (rt) => x(we, rt)), xt(O, () => s(ve), (rt) => x(ve, rt)), js(Se, () => s(le), (rt) => x(le, rt)), v(Y, q);
        };
        A(_, (Y) => {
          d().length >= 2 && Y(b);
        });
      }
      a(Pe), a(fe);
      var re = o(fe, 2), j = i(re), ce = i(j);
      He(ce, 5, () => r().slice(2), ft, (Y, q) => {
        var ee = Cv();
        ee.__click = () => Cr(H(s(q).prompt), s(q).id);
        var me = i(ee), xe = i(me, !0);
        a(me);
        var he = o(me, 2), se = i(he);
        a(he), a(ee), $(() => {
          h(xe, s(q).icon), h(se, `Ask ${s(q).name ?? ""}`);
        }), v(Y, ee);
      }), a(ce);
      var _e = o(ce, 2);
      _e.__click = Ge;
      var ge = i(_e, !0);
      a(_e), a(j), a(re), a(G), $(() => {
        _e.disabled = P(), Ie(
          _e,
          1,
          `text-[10px] px-4 py-1.5 rounded font-medium transition-colors
								${P() ? "bg-surface-700 text-surface-500 cursor-not-allowed" : "bg-hecate-600 text-surface-50 hover:bg-hecate-500"}`,
          "svelte-gwxd3p"
        ), h(ge, P() ? "Promoting..." : "Promote to Divisions");
      }), v(R, G);
    }, Ye = (R) => {
      var G = Ev(), fe = i(G), Pe = i(fe);
      Pe.textContent = "✓";
      var ie = o(Pe, 4), be = i(ie);
      a(ie);
      var Ne = o(ie, 4);
      Ne.__click = function(...Oe) {
        Nl?.apply(this, Oe);
      }, a(fe), a(G), $(() => h(be, `${d().length ?? ""} divisions identified from
						${n() ?? ""} domain events, with
						${f().length ?? ""} integration fact${f().length !== 1 ? "s" : ""} mapped.`)), v(R, G);
    }, je = (R) => {
      var G = Av(), fe = i(G), Pe = i(fe);
      Pe.textContent = "⏸";
      var ie = o(Pe, 6);
      ie.__click = () => Ml(K()), a(fe), a(G), v(R, G);
    };
    A(Z, (R) => {
      c() === "ready" ? R(Me) : c() === "storm" ? R(Be, 1) : c() === "stack" ? R(Je, 2) : c() === "groom" ? R(ze, 3) : c() === "cluster" ? R(tt, 4) : c() === "name" ? R(qe, 5) : c() === "map" ? R(Ue, 6) : c() === "promoted" ? R(Ye, 7) : c() === "shelved" && R(je, 8);
    });
  }
  a(I), a(L), $(() => Ie(
    I,
    1,
    `flex-1 overflow-y-auto transition-opacity duration-150
		${s(ne) ? "opacity-0" : "opacity-100"}`,
    "svelte-gwxd3p"
  )), v(e, L), $t(), Q();
}
Mt(["click", "keydown"]);
Et(Qa, {}, [], [], { mode: "open" });
const _r = Ze([]), An = Ze(null), Tv = Pt(_r, (e) => {
  const t = /* @__PURE__ */ new Set();
  for (const r of e)
    r.aggregate && t.add(r.aggregate);
  return Array.from(t).sort();
}), Rv = Pt(_r, (e) => {
  const t = /* @__PURE__ */ new Map(), r = [];
  for (const n of e)
    if (n.aggregate) {
      const c = t.get(n.aggregate) || [];
      c.push(n), t.set(n.aggregate, c);
    } else
      r.push(n);
  return { grouped: t, ungrouped: r };
});
function Mv(e, t, r = "human") {
  const n = crypto.randomUUID(), c = {
    id: n,
    name: e.trim(),
    aggregate: t?.trim() || void 0,
    execution: r,
    policies: [],
    events: []
  };
  return _r.update((l) => [...l, c]), n;
}
function Iv(e) {
  _r.update((t) => t.filter((r) => r.id !== e));
}
function Nv(e, t) {
  _r.update(
    (r) => r.map((n) => n.id === e ? { ...n, ...t } : n)
  );
}
function Lv(e, t) {
  _r.update(
    (r) => r.map((n) => n.id === e ? { ...n, execution: t } : n)
  );
}
function Ov(e, t) {
  const r = { id: crypto.randomUUID(), text: t.trim() };
  _r.update(
    (n) => n.map(
      (c) => c.id === e ? { ...c, policies: [...c.policies, r] } : c
    )
  );
}
function Fv(e, t) {
  _r.update(
    (r) => r.map(
      (n) => n.id === e ? { ...n, policies: n.policies.filter((c) => c.id !== t) } : n
    )
  );
}
function jv(e, t) {
  const r = { id: crypto.randomUUID(), text: t.trim() };
  _r.update(
    (n) => n.map(
      (c) => c.id === e ? { ...c, events: [...c.events, r] } : c
    )
  );
}
function Bv(e, t) {
  _r.update(
    (r) => r.map(
      (n) => n.id === e ? { ...n, events: n.events.filter((c) => c.id !== t) } : n
    )
  );
}
async function Vv(e, t) {
  try {
    return await Ke().post(`/stormings/${e}/design-aggregate`, t), !0;
  } catch (r) {
    const n = r;
    return An.set(n.message || "Failed to design aggregate"), !1;
  }
}
async function Gv(e, t) {
  try {
    return await Ke().post(`/stormings/${e}/design-event`, t), !0;
  } catch (r) {
    const n = r;
    return An.set(n.message || "Failed to design event"), !1;
  }
}
async function Xn(e, t) {
  try {
    return await Ke().post(`/stormings/${e}/plan-desk`, t), !0;
  } catch (r) {
    const n = r;
    return An.set(n.message || "Failed to plan desk"), !1;
  }
}
var qv = /* @__PURE__ */ p(`<button class="text-[10px] px-2.5 py-1 rounded bg-hecate-600/20 text-hecate-300
					hover:bg-hecate-600/30 transition-colors disabled:opacity-50 disabled:cursor-not-allowed"> </button>`), Hv = /* @__PURE__ */ p(`<button class="text-[10px] px-2 py-1 rounded text-surface-400
					hover:text-hecate-300 hover:bg-hecate-600/10 transition-colors" title="Get AI assistance"></button>`), Wv = /* @__PURE__ */ p('<div><div class="flex items-start gap-2"><span class="text-hecate-400 text-sm mt-0.5"> </span> <div class="flex-1 min-w-0"><div class="flex items-center gap-2"><h3 class="text-xs font-semibold text-surface-100"> </h3> <span> </span></div> <p class="text-[11px] text-surface-400 mt-1"> </p></div></div> <div class="flex items-center gap-2 mt-1"><!> <!></div></div>');
function _t(e, t) {
  kt(t, !0);
  let r = pt(t, "title", 7), n = pt(t, "description", 7), c = pt(t, "icon", 7, "■"), l = pt(t, "status", 7, "pending"), u = pt(t, "aiContext", 7), d = pt(t, "onaction", 7), f = pt(t, "actionLabel", 7, "Execute"), g = pt(t, "disabled", 7, !1), m = /* @__PURE__ */ Ee(() => y(l()));
  function k(ne) {
    switch (ne) {
      case "active":
        return "border-hecate-600/40";
      case "done":
        return "border-health-ok/30";
      default:
        return "border-surface-600";
    }
  }
  function y(ne) {
    switch (ne) {
      case "active":
        return { text: "Active", cls: "bg-hecate-600/20 text-hecate-300" };
      case "done":
        return { text: "Done", cls: "bg-health-ok/10 text-health-ok" };
      default:
        return { text: "Pending", cls: "bg-surface-700 text-surface-400" };
    }
  }
  var P = {
    get title() {
      return r();
    },
    set title(ne) {
      r(ne), vt();
    },
    get description() {
      return n();
    },
    set description(ne) {
      n(ne), vt();
    },
    get icon() {
      return c();
    },
    set icon(ne = "■") {
      c(ne), vt();
    },
    get status() {
      return l();
    },
    set status(ne = "pending") {
      l(ne), vt();
    },
    get aiContext() {
      return u();
    },
    set aiContext(ne) {
      u(ne), vt();
    },
    get onaction() {
      return d();
    },
    set onaction(ne) {
      d(ne), vt();
    },
    get actionLabel() {
      return f();
    },
    set actionLabel(ne = "Execute") {
      f(ne), vt();
    },
    get disabled() {
      return g();
    },
    set disabled(ne = !1) {
      g(ne), vt();
    }
  }, E = Wv(), Q = i(E), S = i(Q), U = i(S, !0);
  a(S);
  var ae = o(S, 2), we = i(ae), le = i(we), ve = i(le, !0);
  a(le);
  var de = o(le, 2), Le = i(de, !0);
  a(de), a(we);
  var Ae = o(we, 2), De = i(Ae, !0);
  a(Ae), a(ae), a(Q);
  var te = o(Q, 2), N = i(te);
  {
    var X = (ne) => {
      var W = qv();
      W.__click = function(...Fe) {
        d()?.apply(this, Fe);
      };
      var K = i(W, !0);
      a(W), $(() => {
        W.disabled = g(), h(K, f());
      }), v(ne, W);
    };
    A(N, (ne) => {
      d() && ne(X);
    });
  }
  var ue = o(N, 2);
  {
    var Te = (ne) => {
      var W = Hv();
      W.__click = () => Cr(u()), W.textContent = "✦ AI", v(ne, W);
    };
    A(ue, (ne) => {
      u() && ne(Te);
    });
  }
  return a(te), a(E), $(
    (ne) => {
      Ie(E, 1, `rounded-lg bg-surface-800 border ${ne ?? ""} p-4 flex flex-col gap-2 transition-colors hover:border-surface-500`), h(U, c()), h(ve, r()), Ie(de, 1, `text-[9px] px-1.5 py-0.5 rounded ${s(m).cls ?? ""}`), h(Le, s(m).text), h(De, n());
    },
    [() => k(l())]
  ), v(e, E), $t(P);
}
Mt(["click"]);
Et(
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
var Uv = /* @__PURE__ */ p(`<div class="group/policy flex items-center gap-1 px-2 py-1 rounded-l rounded-r-sm
						bg-es-policy/15 border border-es-policy/30 text-[9px] text-surface-200
						max-w-[160px]"><span class="truncate flex-1"> </span> <button class="text-[7px] text-surface-500 hover:text-health-err
							opacity-0 group-hover/policy:opacity-100 transition-opacity shrink-0"></button></div>`), zv = /* @__PURE__ */ p(`<input class="flex-1 bg-surface-700 border border-es-command/30 rounded px-2 py-0.5
							text-xs font-semibold text-surface-100
							focus:outline-none focus:border-es-command"/>`), Yv = /* @__PURE__ */ p(`<button class="flex-1 text-left text-xs font-semibold text-surface-100
							hover:text-es-command transition-colors" title="Double-click to rename"> </button>`), Kv = /* @__PURE__ */ p('<span class="text-[9px] text-es-aggregate/70"> </span>'), Jv = /* @__PURE__ */ p(`<div class="group/event flex items-center gap-1 px-2 py-1 rounded-r rounded-l-sm
						bg-es-event/15 border border-es-event/30 text-[9px] text-surface-200
						max-w-[200px]"><span class="truncate flex-1"> </span> <button class="text-[7px] text-surface-500 hover:text-health-err
							opacity-0 group-hover/event:opacity-100 transition-opacity shrink-0"></button></div>`), Qv = /* @__PURE__ */ p(`<div class="flex items-stretch gap-0 group/card"><div class="flex flex-col items-end gap-1 -mr-2 z-10 pt-1 min-w-[100px]"><!> <input placeholder="+ policy" class="w-24 bg-transparent border border-dashed border-es-policy/20 rounded
					px-1.5 py-0.5 text-[8px] text-surface-400 placeholder-surface-500
					focus:outline-none focus:border-es-policy/40
					opacity-0 group-hover/card:opacity-100 transition-opacity"/></div> <div class="relative flex-1 rounded-lg border-2 border-es-command/40 bg-es-command/10
				px-4 py-3 min-h-[72px] z-20"><div class="flex items-center gap-2 mb-1"><button> </button> <!> <div class="flex items-center gap-1 opacity-0 group-hover/card:opacity-100 transition-opacity"><button class="text-[8px] px-1.5 py-0.5 rounded text-health-ok
							hover:bg-health-ok/10 transition-colors" title="Promote to daemon"></button> <button class="text-[8px] px-1 py-0.5 rounded text-surface-500
							hover:text-health-err hover:bg-health-err/10 transition-colors" title="Remove desk"></button></div></div> <!></div> <div class="flex flex-col items-start gap-1 -ml-2 z-10 pt-1 min-w-[100px]"><!> <input placeholder="+ event" class="w-32 bg-transparent border border-dashed border-es-event/20 rounded
					px-1.5 py-0.5 text-[8px] text-surface-400 placeholder-surface-500
					focus:outline-none focus:border-es-event/40
					opacity-0 group-hover/card:opacity-100 transition-opacity"/></div></div>`), Xv = /* @__PURE__ */ p("<option></option>"), Zv = /* @__PURE__ */ p('<div class="space-y-2"><div class="flex items-center gap-2"><div class="w-3 h-3 rounded-sm bg-es-aggregate/40"></div> <span class="text-[10px] font-semibold text-es-aggregate uppercase tracking-wider"> </span> <div class="flex-1 h-px bg-es-aggregate/20"></div> <span class="text-[9px] text-surface-400"> </span></div> <div class="space-y-3 ml-5"></div></div>'), ef = /* @__PURE__ */ p('<div class="flex items-center gap-2"><span class="text-[10px] font-semibold text-surface-400 uppercase tracking-wider">No Aggregate</span> <div class="flex-1 h-px bg-surface-600"></div></div>'), tf = /* @__PURE__ */ p("<!> <div></div>", 1), rf = /* @__PURE__ */ p("<!> <!>", 1), sf = /* @__PURE__ */ p(`<div class="text-center py-8 text-surface-500 text-xs border border-dashed border-surface-600 rounded-lg">No desk cards yet. Add your first command desk above,
				or ask an AI agent for suggestions.</div>`), af = /* @__PURE__ */ p(`<button class="rounded-lg border border-surface-600 bg-surface-800/50
							p-3 text-left transition-all hover:border-hecate-500/40
							hover:bg-surface-700/50 group"><div class="flex items-center gap-2 mb-1.5"><span class="text-hecate-400 group-hover:text-hecate-300 transition-colors"> </span> <span class="text-[11px] font-semibold text-surface-100"> </span></div> <div class="text-[10px] text-surface-400 mb-1"> </div> <div class="text-[9px] text-surface-500"> </div></button>`), nf = /* @__PURE__ */ p(
  `<div class="rounded-lg border border-es-command/20 bg-es-command/5 p-3"><div class="flex items-end gap-2"><div class="flex-1"><label class="text-[9px] text-surface-400 block mb-1">Desk Name (command)</label> <input placeholder="e.g., register_user, process_order" class="w-full bg-surface-700 border border-surface-600 rounded px-2.5 py-1.5
							text-xs text-surface-100 placeholder-surface-400
							focus:outline-none focus:border-es-command/50"/></div> <div class="w-40"><label class="text-[9px] text-surface-400 block mb-1">Aggregate</label> <input placeholder="e.g., user, order" list="existing-aggregates" class="w-full bg-surface-700 border border-surface-600 rounded px-2.5 py-1.5
							text-xs text-surface-100 placeholder-surface-400
							focus:outline-none focus:border-surface-500"/> <datalist id="existing-aggregates"></datalist></div> <div class="w-24"><label class="text-[9px] text-surface-400 block mb-1">Execution</label> <select class="w-full bg-surface-700 border border-surface-600 rounded px-2 py-1.5
							text-xs text-surface-100
							focus:outline-none focus:border-surface-500"><option>Human</option><option>Agent</option><option>Both</option></select></div> <button>+ Desk</button></div></div> <!> <div class="rounded-lg border border-hecate-600/20 bg-hecate-950/20 p-4"><div class="flex items-center gap-2 mb-3"><span class="text-hecate-400"></span> <h4 class="text-xs font-semibold text-surface-100">AI Domain Experts</h4> <span class="text-[10px] text-surface-400">Ask a virtual agent to analyze the domain and suggest desk cards</span></div> <div class="grid grid-cols-2 md:grid-cols-4 gap-2"></div></div> <div><h4 class="text-xs font-semibold text-surface-100 mb-3">Design Tasks</h4> <div class="grid grid-cols-1 md:grid-cols-2 gap-3"><!> <!> <!> <!></div></div>`,
  1
), of = /* @__PURE__ */ p(`<div class="flex gap-2 items-end mb-4 p-3 rounded bg-surface-700/30"><div class="flex-1"><label for="desk-name" class="text-[10px] text-surface-400 block mb-1">Desk Name</label> <input id="desk-name" placeholder="e.g., register_user" class="w-full bg-surface-700 border border-surface-600 rounded
								px-2.5 py-1.5 text-xs text-surface-100 placeholder-surface-400
								focus:outline-none focus:border-hecate-500/50"/></div> <div class="flex-1"><label for="desk-desc" class="text-[10px] text-surface-400 block mb-1">Description</label> <input id="desk-desc" placeholder="Brief purpose of this desk" class="w-full bg-surface-700 border border-surface-600 rounded
								px-2.5 py-1.5 text-xs text-surface-100 placeholder-surface-400
								focus:outline-none focus:border-hecate-500/50"/></div> <div><label for="desk-dept" class="text-[10px] text-surface-400 block mb-1">Dept</label> <select id="desk-dept" class="bg-surface-700 border border-surface-600 rounded
								px-2 py-1.5 text-xs text-surface-100
								focus:outline-none focus:border-hecate-500/50 cursor-pointer"><option>CMD</option><option>QRY</option><option>PRJ</option></select></div> <button class="px-3 py-1.5 rounded text-xs bg-hecate-600/20 text-hecate-300
							hover:bg-hecate-600/30 transition-colors disabled:opacity-50">Plan</button> <button class="px-3 py-1.5 rounded text-xs text-surface-400 hover:text-surface-100">Cancel</button></div>`), cf = /* @__PURE__ */ p(
  `<div class="rounded-lg border border-surface-600 bg-surface-800/50 p-4"><div class="flex items-center justify-between mb-3"><h4 class="text-xs font-semibold text-surface-100">Desk Inventory</h4> <button class="text-[10px] px-2 py-0.5 rounded bg-hecate-600/10 text-hecate-300
						hover:bg-hecate-600/20 transition-colors">+ Plan Desk</button></div> <!> <p class="text-[10px] text-surface-400">Desks are individual capabilities within a department. Each desk owns a
				vertical slice: command + event + handler + projection.</p></div> <div><h4 class="text-xs font-semibold text-surface-100 mb-3">Planning Tasks</h4> <div class="grid grid-cols-1 md:grid-cols-2 gap-3"><!> <!> <!> <!></div></div>`,
  1
), lf = /* @__PURE__ */ p('<div class="p-4 space-y-6"><div class="flex items-center justify-between"><div><h3 class="text-sm font-semibold text-surface-100">Storming</h3> <p class="text-[11px] text-surface-400 mt-0.5">Design aggregates, events, desks, and dependencies for <span class="text-surface-200"> </span></p></div> <div class="flex items-center gap-1 bg-surface-700/50 rounded-lg p-0.5"><button>Event Storm</button> <button>Desk Inventory</button></div></div> <!></div>');
function ko(e, t) {
  kt(t, !0);
  const r = () => ke(Ur, "$selectedDivision", f), n = () => ke(_r, "$deskCards", f), c = () => ke(Tv, "$deskAggregates", f), l = () => ke(Rv, "$deskCardsByAggregate", f), u = () => ke(Fd, "$designLevelAgents", f), d = () => ke(wt, "$isLoading", f), [f, g] = Vt(), m = (w, D = Ar) => {
    var z = Qv(), pe = i(z), Ce = i(pe);
    He(Ce, 17, () => D().policies, (ie) => ie.id, (ie, be) => {
      var Ne = Uv(), Oe = i(Ne), _ = i(Oe, !0);
      a(Oe);
      var b = o(Oe, 2);
      b.__click = () => Fv(D().id, s(be).id), b.textContent = "✕", a(Ne), $(() => h(_, s(be).text)), v(ie, Ne);
    });
    var T = o(Ce, 2);
    bt(T), T.__keydown = (ie) => De(ie, D().id), a(pe);
    var I = o(pe, 2), Z = i(I), Me = i(Z);
    Me.__click = () => ue(D());
    var Be = i(Me, !0);
    a(Me);
    var Je = o(Me, 2);
    {
      var ze = (ie) => {
        var be = zv();
        bt(be), be.__keydown = (Ne) => {
          Ne.key === "Enter" && X(D().id), Ne.key === "Escape" && x(S, null);
        }, Nt("blur", be, () => X(D().id)), xt(be, () => s(U), (Ne) => x(U, Ne)), v(ie, be);
      }, tt = (ie) => {
        var be = Yv();
        be.__dblclick = () => N(D());
        var Ne = i(be, !0);
        a(be), $(() => h(Ne, D().name)), v(ie, be);
      };
      A(Je, (ie) => {
        s(S) === D().id ? ie(ze) : ie(tt, !1);
      });
    }
    var qe = o(Je, 2), Ue = i(qe);
    Ue.__click = () => K(D()), Ue.textContent = "↑ promote";
    var Ye = o(Ue, 2);
    Ye.__click = () => Iv(D().id), Ye.textContent = "✕", a(qe), a(Z);
    var je = o(Z, 2);
    {
      var R = (ie) => {
        var be = Kv(), Ne = i(be);
        a(be), $(() => h(Ne, `■ ${D().aggregate ?? ""}`)), v(ie, be);
      };
      A(je, (ie) => {
        D().aggregate && ie(R);
      });
    }
    a(I);
    var G = o(I, 2), fe = i(G);
    He(fe, 17, () => D().events, (ie) => ie.id, (ie, be) => {
      var Ne = Jv(), Oe = i(Ne), _ = i(Oe, !0);
      a(Oe);
      var b = o(Oe, 2);
      b.__click = () => Bv(D().id, s(be).id), b.textContent = "✕", a(Ne), $(() => h(_, s(be).text)), v(ie, Ne);
    });
    var Pe = o(fe, 2);
    bt(Pe), Pe.__keydown = (ie) => te(ie, D().id), a(G), a(z), $(
      (ie, be, Ne) => {
        Ie(Me, 1, `text-sm ${ie ?? ""}
						hover:scale-110 transition-transform`), Ot(Me, "title", `${be ?? ""} — click to cycle`), h(Be, Ne);
      },
      [
        () => W(D().execution),
        () => ne(D().execution),
        () => Te(D().execution)
      ]
    ), xt(T, () => s(E)[D().id], (ie) => s(E)[D().id] = ie), xt(Pe, () => s(Q)[D().id], (ie) => s(Q)[D().id] = ie), v(w, z);
  };
  let k = /* @__PURE__ */ oe(""), y = /* @__PURE__ */ oe(""), P = /* @__PURE__ */ oe("human"), E = /* @__PURE__ */ oe(Bt({})), Q = /* @__PURE__ */ oe(Bt({})), S = /* @__PURE__ */ oe(null), U = /* @__PURE__ */ oe(""), ae = /* @__PURE__ */ oe(!1), we = /* @__PURE__ */ oe(""), le = /* @__PURE__ */ oe(""), ve = /* @__PURE__ */ oe("cmd"), de = /* @__PURE__ */ oe("design");
  function Le() {
    s(k).trim() && (Mv(s(k), s(y) || void 0, s(P)), x(k, ""), x(y, ""), x(P, "human"));
  }
  function Ae(w) {
    w.key === "Enter" && !w.shiftKey && s(k).trim() && (w.preventDefault(), Le());
  }
  function De(w, D) {
    w.key === "Enter" && s(E)[D]?.trim() && (w.preventDefault(), Ov(D, s(E)[D]), s(E)[D] = "");
  }
  function te(w, D) {
    w.key === "Enter" && s(Q)[D]?.trim() && (w.preventDefault(), jv(D, s(Q)[D]), s(Q)[D] = "");
  }
  function N(w) {
    x(S, w.id, !0), x(U, w.name, !0);
  }
  function X(w) {
    s(U).trim() && Nv(w, { name: s(U).trim() }), x(S, null);
  }
  function ue(w) {
    const D = ["human", "agent", "both"], z = D.indexOf(w.execution);
    Lv(w.id, D[(z + 1) % D.length]);
  }
  function Te(w) {
    switch (w) {
      case "human":
        return "𝗨";
      case "agent":
        return "⚙";
      case "both":
      case "pair":
        return "✦";
    }
  }
  function ne(w) {
    switch (w) {
      case "human":
        return "Interactive (human)";
      case "agent":
        return "Automated (AI agent)";
      case "both":
      case "pair":
        return "Assisted (human + AI)";
    }
  }
  function W(w) {
    switch (w) {
      case "human":
        return "text-es-command";
      case "agent":
        return "text-hecate-400";
      case "both":
      case "pair":
        return "text-phase-crafting";
    }
  }
  async function K(w) {
    if (!r()) return;
    const D = r().division_id;
    await Xn(D, {
      desk_name: w.name,
      description: [
        w.execution === "agent" ? "AI-automated" : w.execution === "both" ? "Human+AI assisted" : "Interactive",
        w.policies.length > 0 ? `Policies: ${w.policies.map((z) => z.text).join(", ")}` : "",
        w.events.length > 0 ? `Emits: ${w.events.map((z) => z.text).join(", ")}` : ""
      ].filter(Boolean).join(". "),
      department: "CMD"
    });
    for (const z of w.events)
      await Gv(D, {
        event_name: z.text,
        aggregate_type: w.aggregate || w.name
      });
    w.aggregate && await Vv(D, { aggregate_name: w.aggregate });
  }
  async function Fe() {
    if (!r() || !s(we).trim()) return;
    await Xn(r().division_id, {
      desk_name: s(we).trim(),
      description: s(le).trim() || void 0,
      department: s(ve)
    }) && (x(we, ""), x(le, ""), x(ae, !1));
  }
  function et(w) {
    const D = r()?.context_name ?? "this division", z = n(), pe = z.map((Z) => Z.name).join(", "), Ce = z.flatMap((Z) => Z.events.map((Me) => Me.text)).join(", "), T = z.flatMap((Z) => Z.policies.map((Me) => Me.text)).join(", ");
    let I = `We are doing Design-Level Event Storming for the "${D}" division.

`;
    return I += `Our board uses command-centric desk cards:
`, I += `- Each card = a desk (command/slice)
`, I += `- Left side: policies (grey) = filter/guard conditions
`, I += `- Right side: events (orange) = what the desk emits
`, I += `- Cards can be human (interactive), agent (AI), or both

`, pe && (I += `Desks so far: ${pe}
`), Ce && (I += `Events so far: ${Ce}
`), T && (I += `Policies so far: ${T}
`), I += `
${w.prompt}

Please analyze and suggest items for the board.`, I;
  }
  var st = lf(), Xe = i(st), We = i(Xe), Ge = o(i(We), 2), C = o(i(Ge)), V = i(C, !0);
  a(C), a(Ge), a(We);
  var H = o(We, 2), M = i(H);
  M.__click = () => x(de, "design");
  var L = o(M, 2);
  L.__click = () => x(de, "plan"), a(H), a(Xe);
  var F = o(Xe, 2);
  {
    var J = (w) => {
      var D = nf(), z = ct(D), pe = i(z), Ce = i(pe), T = o(i(Ce), 2);
      bt(T), T.__keydown = Ae, a(Ce);
      var I = o(Ce, 2), Z = o(i(I), 2);
      bt(Z);
      var Me = o(Z, 2);
      He(Me, 5, c, ft, (j, ce) => {
        var _e = Xv(), ge = {};
        $(() => {
          ge !== (ge = s(ce)) && (_e.value = (_e.__value = s(ce)) ?? "");
        }), v(j, _e);
      }), a(Me), a(I);
      var Be = o(I, 2), Je = o(i(Be), 2), ze = i(Je);
      ze.value = ze.__value = "human";
      var tt = o(ze);
      tt.value = tt.__value = "agent";
      var qe = o(tt);
      qe.value = qe.__value = "both", a(Je), a(Be);
      var Ue = o(Be, 2);
      Ue.__click = Le, a(pe), a(z);
      var Ye = o(z, 2);
      {
        var je = (j) => {
          const ce = /* @__PURE__ */ Ee(() => {
            const { grouped: ee, ungrouped: me } = l();
            return { grouped: ee, ungrouped: me };
          });
          var _e = rf(), ge = ct(_e);
          He(ge, 17, () => [...s(ce).grouped.entries()], ft, (ee, me) => {
            var xe = /* @__PURE__ */ Ee(() => Ta(s(me), 2));
            let he = () => s(xe)[0], se = () => s(xe)[1];
            var B = Zv(), O = i(B), ye = o(i(O), 2), Se = i(ye, !0);
            a(ye);
            var Re = o(ye, 4), Ve = i(Re);
            a(Re), a(O);
            var Qe = o(O, 2);
            He(Qe, 21, se, (rt) => rt.id, (rt, at) => {
              m(rt, () => s(at));
            }), a(Qe), a(B), $(() => {
              h(Se, he()), h(Ve, `${se().length ?? ""} desk${se().length !== 1 ? "s" : ""}`);
            }), v(ee, B);
          });
          var Y = o(ge, 2);
          {
            var q = (ee) => {
              var me = tf(), xe = ct(me);
              {
                var he = (B) => {
                  var O = ef();
                  v(B, O);
                };
                A(xe, (B) => {
                  s(ce).grouped.size > 0 && B(he);
                });
              }
              var se = o(xe, 2);
              He(se, 21, () => s(ce).ungrouped, (B) => B.id, (B, O) => {
                m(B, () => s(O));
              }), a(se), $(() => Ie(se, 1, `space-y-3 ${s(ce).grouped.size > 0 ? "ml-5" : ""}`)), v(ee, me);
            };
            A(Y, (ee) => {
              s(ce).ungrouped.length > 0 && ee(q);
            });
          }
          v(j, _e);
        }, R = (j) => {
          var ce = sf();
          v(j, ce);
        };
        A(Ye, (j) => {
          n().length > 0 ? j(je) : j(R, !1);
        });
      }
      var G = o(Ye, 2), fe = i(G), Pe = i(fe);
      Pe.textContent = "✦", At(4), a(fe);
      var ie = o(fe, 2);
      He(ie, 5, u, ft, (j, ce) => {
        var _e = af();
        _e.__click = () => Cr(et(s(ce)));
        var ge = i(_e), Y = i(ge), q = i(Y, !0);
        a(Y);
        var ee = o(Y, 2), me = i(ee, !0);
        a(ee), a(ge);
        var xe = o(ge, 2), he = i(xe, !0);
        a(xe);
        var se = o(xe, 2), B = i(se, !0);
        a(se), a(_e), $(() => {
          h(q, s(ce).icon), h(me, s(ce).name), h(he, s(ce).role), h(B, s(ce).description);
        }), v(j, _e);
      }), a(ie), a(G);
      var be = o(G, 2), Ne = o(i(be), 2), Oe = i(Ne);
      {
        let j = /* @__PURE__ */ Ee(() => `Help me design aggregates for the "${r()?.context_name}" division. What are the natural consistency boundaries? What entities accumulate history over time?`);
        _t(Oe, {
          title: "Design Aggregates",
          description: "Identify aggregate boundaries, define stream patterns and status flags",
          icon: "■",
          get aiContext() {
            return s(j);
          }
        });
      }
      var _ = o(Oe, 2);
      {
        let j = /* @__PURE__ */ Ee(() => `Help me define status bit flags for aggregates in the "${r()?.context_name}" division. Each aggregate needs lifecycle states as bit flags (powers of 2).`);
        _t(_, {
          title: "Define Status Flags",
          description: "Design bit flag status fields for each aggregate lifecycle",
          icon: "⚑",
          get aiContext() {
            return s(j);
          }
        });
      }
      var b = o(_, 2);
      {
        let j = /* @__PURE__ */ Ee(() => `Help me identify read models for the "${r()?.context_name}" division. What queries will users run? What data views are needed?`);
        _t(b, {
          title: "Map Read Models",
          description: "Identify what queries users will run and what data they need",
          icon: "▶",
          get aiContext() {
            return s(j);
          }
        });
      }
      var re = o(b, 2);
      {
        let j = /* @__PURE__ */ Ee(() => `Help me create a domain glossary for the "${r()?.context_name}" division. Define key terms, bounded context boundaries, and ubiquitous language.`);
        _t(re, {
          title: "Domain Glossary",
          description: "Document ubiquitous language and bounded context definitions",
          icon: "✎",
          get aiContext() {
            return s(j);
          }
        });
      }
      a(Ne), a(be), $(
        (j, ce) => {
          Ue.disabled = j, Ie(Ue, 1, `px-3 py-1.5 rounded text-xs transition-colors shrink-0
						${ce ?? ""}`);
        },
        [
          () => !s(k).trim(),
          () => s(k).trim() ? "bg-es-command/20 text-es-command hover:bg-es-command/30" : "bg-surface-700 text-surface-500 cursor-not-allowed"
        ]
      ), xt(T, () => s(k), (j) => x(k, j)), xt(Z, () => s(y), (j) => x(y, j)), js(Je, () => s(P), (j) => x(P, j)), v(w, D);
    }, $e = (w) => {
      var D = cf(), z = ct(D), pe = i(z), Ce = o(i(pe), 2);
      Ce.__click = () => x(ae, !s(ae)), a(pe);
      var T = o(pe, 2);
      {
        var I = (qe) => {
          var Ue = of(), Ye = i(Ue), je = o(i(Ye), 2);
          bt(je), a(Ye);
          var R = o(Ye, 2), G = o(i(R), 2);
          bt(G), a(R);
          var fe = o(R, 2), Pe = o(i(fe), 2), ie = i(Pe);
          ie.value = ie.__value = "cmd";
          var be = o(ie);
          be.value = be.__value = "qry";
          var Ne = o(be);
          Ne.value = Ne.__value = "prj", a(Pe), a(fe);
          var Oe = o(fe, 2);
          Oe.__click = Fe;
          var _ = o(Oe, 2);
          _.__click = () => x(ae, !1), a(Ue), $((b) => Oe.disabled = b, [() => !s(we).trim() || d()]), xt(je, () => s(we), (b) => x(we, b)), xt(G, () => s(le), (b) => x(le, b)), js(Pe, () => s(ve), (b) => x(ve, b)), v(qe, Ue);
        };
        A(T, (qe) => {
          s(ae) && qe(I);
        });
      }
      At(2), a(z);
      var Z = o(z, 2), Me = o(i(Z), 2), Be = i(Me);
      {
        let qe = /* @__PURE__ */ Ee(() => `Help me create a desk inventory for the "${r()?.context_name}" division. Each desk is a vertical slice (command + event + handler). Organize by CMD (write), QRY (read), and PRJ (projection) departments.`);
        _t(Be, {
          title: "Desk Inventory",
          description: "Create a complete inventory of desks needed for this division, organized by department (CMD, QRY, PRJ)",
          icon: "▣",
          get aiContext() {
            return s(qe);
          }
        });
      }
      var Je = o(Be, 2);
      {
        let qe = /* @__PURE__ */ Ee(() => `Help me map dependencies between desks in the "${r()?.context_name}" division. Which desks depend on which? What's the optimal implementation order?`);
        _t(Je, {
          title: "Dependency Mapping",
          description: "Map dependencies between desks to determine implementation order",
          icon: "⇄",
          get aiContext() {
            return s(qe);
          }
        });
      }
      var ze = o(Je, 2);
      {
        let qe = /* @__PURE__ */ Ee(() => `Help me sequence the implementation of desks in the "${r()?.context_name}" division into logical sprints. Consider dependencies, walking skeleton principles, and the "initiate + archive" first rule.`);
        _t(ze, {
          title: "Sprint Sequencing",
          description: "Prioritize and sequence desks into implementation sprints",
          icon: "☰",
          get aiContext() {
            return s(qe);
          }
        });
      }
      var tt = o(ze, 2);
      {
        let qe = /* @__PURE__ */ Ee(() => `Help me design REST API endpoints for the "${r()?.context_name}" division. Follow the pattern: POST /api/{resource}/{action} for commands, GET /api/{resource} for queries.`);
        _t(tt, {
          title: "API Design",
          description: "Design REST API endpoints for each desk's capabilities",
          icon: "↔",
          get aiContext() {
            return s(qe);
          }
        });
      }
      a(Me), a(Z), v(w, D);
    };
    A(F, (w) => {
      s(de) === "design" ? w(J) : w($e, !1);
    });
  }
  a(st), $(() => {
    h(V, r()?.context_name), Ie(M, 1, `px-3 py-1 rounded text-[11px] transition-colors
					${s(de) === "design" ? "bg-surface-600 text-surface-100" : "text-surface-400 hover:text-surface-200"}`), Ie(L, 1, `px-3 py-1 rounded text-[11px] transition-colors
					${s(de) === "plan" ? "bg-surface-600 text-surface-100" : "text-surface-400 hover:text-surface-200"}`);
  }), v(e, st), $t(), g();
}
Mt(["click", "keydown", "dblclick"]);
Et(ko, {}, [], [], { mode: "open" });
vc();
var df = /* @__PURE__ */ p(`<div class="p-4 space-y-6"><div><h3 class="text-sm font-semibold text-surface-100">Planning</h3> <p class="text-[11px] text-surface-400 mt-0.5">Lifecycle management for <span class="text-surface-200"> </span></p></div> <div class="rounded-lg border border-surface-600 bg-surface-800/50 p-4"><h4 class="text-xs font-semibold text-surface-100 mb-3">Division Lifecycle</h4> <p class="text-[10px] text-surface-400 leading-relaxed">Use the phase controls above to manage this division's planning lifecycle: <span class="text-surface-300">Open</span> to begin work, <span class="text-surface-300">Shelve</span> to pause, <span class="text-surface-300">Resume</span> to continue, or <span class="text-surface-300">Conclude</span> when planning is complete.</p> <p class="text-[10px] text-surface-400 mt-2 leading-relaxed">Content work (designing aggregates, events, desks) happens in the <span class="text-es-event">Storming</span> phase.
			Implementation items are tracked on the <span class="text-hecate-400">Kanban</span> board.</p></div> <div><h4 class="text-xs font-semibold text-surface-100 mb-3">Planning Tasks</h4> <div class="grid grid-cols-1 md:grid-cols-2 gap-3"><!> <!> <!> <!></div></div></div>`);
function $o(e, t) {
  kt(t, !1);
  const r = () => ke(Ur, "$selectedDivision", n), [n, c] = Vt();
  el();
  var l = df(), u = i(l), d = o(i(u), 2), f = o(i(d)), g = i(f, !0);
  a(f), a(d), a(u);
  var m = o(u, 4), k = o(i(m), 2), y = i(k);
  {
    let S = /* @__PURE__ */ us(() => `Help me create a desk inventory for the "${r()?.context_name}" division. Each desk is a vertical slice (command + event + handler). Organize by CMD (write), QRY (read), and PRJ (projection) departments.`);
    _t(y, {
      title: "Desk Inventory",
      description: "Create a complete inventory of desks needed for this division, organized by department (CMD, QRY, PRJ)",
      icon: "▣",
      get aiContext() {
        return s(S);
      }
    });
  }
  var P = o(y, 2);
  {
    let S = /* @__PURE__ */ us(() => `Help me map dependencies between desks in the "${r()?.context_name}" division. Which desks depend on which? What's the optimal implementation order?`);
    _t(P, {
      title: "Dependency Mapping",
      description: "Map dependencies between desks to determine implementation order",
      icon: "⇄",
      get aiContext() {
        return s(S);
      }
    });
  }
  var E = o(P, 2);
  {
    let S = /* @__PURE__ */ us(() => `Help me sequence the implementation of desks in the "${r()?.context_name}" division into logical sprints. Consider dependencies, walking skeleton principles, and the "initiate + archive" first rule.`);
    _t(E, {
      title: "Sprint Sequencing",
      description: "Prioritize and sequence desks into implementation sprints",
      icon: "☰",
      get aiContext() {
        return s(S);
      }
    });
  }
  var Q = o(E, 2);
  {
    let S = /* @__PURE__ */ us(() => `Help me design REST API endpoints for the "${r()?.context_name}" division. Follow the pattern: POST /api/{resource}/{action} for commands, GET /api/{resource} for queries.`);
    _t(Q, {
      title: "API Design",
      description: "Design REST API endpoints for each desk's capabilities",
      icon: "↔",
      get aiContext() {
        return s(S);
      }
    });
  }
  a(k), a(m), a(l), $(() => h(g, r()?.context_name)), v(e, l), $t(), c();
}
Et($o, {}, [], [], { mode: "open" });
const Dn = 2, Cs = 4, ka = 8, $a = 16, Zn = Ze(null), Mr = Ze([]), Dt = Ze(null), Xa = Ze(!1), uf = Pt(
  Mr,
  (e) => e.filter(
    (t) => (t.status & ka) === 0 && (t.status & $a) === 0 && (t.status & Dn) === 0 && (t.status & Cs) === 0
  )
), vf = Pt(
  Mr,
  (e) => e.filter(
    (t) => (t.status & Dn) !== 0 && (t.status & ka) === 0 && (t.status & $a) === 0 && (t.status & Cs) === 0
  )
), ff = Pt(
  Mr,
  (e) => e.filter((t) => (t.status & Cs) !== 0)
), pf = Pt(
  Mr,
  (e) => e.filter((t) => (t.status & ka) !== 0 && (t.status & Cs) === 0)
), xf = Pt(
  Mr,
  (e) => e.filter((t) => (t.status & $a) !== 0 && (t.status & Cs) === 0)
), hf = Pt(Mr, (e) => {
  let t = 0, r = 0, n = 0, c = 0, l = 0;
  for (const u of e)
    u.status & Cs ? n++ : u.status & ka ? c++ : u.status & $a ? l++ : u.status & Dn ? r++ : t++;
  return { posted: t, picked: r, finished: n, parked: c, blocked: l, total: e.length };
});
async function Nr(e) {
  try {
    Xa.set(!0), Dt.set(null);
    const r = await Ke().get(
      `/kanbans/${e}`
    );
    Zn.set(r.board), Mr.set(r.cards ?? []);
  } catch (t) {
    const r = t;
    Dt.set(r.message || "Failed to fetch kanban board"), Zn.set(null), Mr.set([]);
  } finally {
    Xa.set(!1);
  }
}
async function _f(e, t) {
  try {
    return Dt.set(null), await Ke().post(`/kanbans/${e}/cards`, t), await Nr(e), !0;
  } catch (r) {
    const n = r;
    return Dt.set(n.message || "Failed to post card"), !1;
  }
}
async function gf(e, t, r = "hecate-web") {
  try {
    return Dt.set(null), await Ke().post(`/kanbans/${e}/cards/${t}/pick`, {
      picked_by: r
    }), await Nr(e), !0;
  } catch (n) {
    const c = n;
    return Dt.set(c.message || "Failed to pick card"), !1;
  }
}
async function bf(e, t) {
  try {
    return Dt.set(null), await Ke().post(`/kanbans/${e}/cards/${t}/finish`, {}), await Nr(e), !0;
  } catch (r) {
    const n = r;
    return Dt.set(n.message || "Failed to finish card"), !1;
  }
}
async function mf(e, t, r) {
  try {
    return Dt.set(null), await Ke().post(`/kanbans/${e}/cards/${t}/unpick`, { reason: r }), await Nr(e), !0;
  } catch (n) {
    const c = n;
    return Dt.set(c.message || "Failed to unpick card"), !1;
  }
}
async function yf(e, t, r, n = "hecate-web") {
  try {
    return Dt.set(null), await Ke().post(`/kanbans/${e}/cards/${t}/park`, {
      reason: r,
      parked_by: n
    }), await Nr(e), !0;
  } catch (c) {
    const l = c;
    return Dt.set(l.message || "Failed to park card"), !1;
  }
}
async function wf(e, t) {
  try {
    return Dt.set(null), await Ke().post(`/kanbans/${e}/cards/${t}/unpark`, {}), await Nr(e), !0;
  } catch (r) {
    const n = r;
    return Dt.set(n.message || "Failed to unpark card"), !1;
  }
}
async function kf(e, t, r, n = "hecate-web") {
  try {
    return Dt.set(null), await Ke().post(`/kanbans/${e}/cards/${t}/block`, {
      reason: r,
      blocked_by: n
    }), await Nr(e), !0;
  } catch (c) {
    const l = c;
    return Dt.set(l.message || "Failed to block card"), !1;
  }
}
async function $f(e, t) {
  try {
    return Dt.set(null), await Ke().post(`/kanbans/${e}/cards/${t}/unblock`, {}), await Nr(e), !0;
  } catch (r) {
    const n = r;
    return Dt.set(n.message || "Failed to unblock card"), !1;
  }
}
var Cf = /* @__PURE__ */ p('<span class="text-health-warn"> </span>'), Sf = /* @__PURE__ */ p('<span class="text-health-err"> </span>'), Ef = /* @__PURE__ */ p('<div class="flex items-center gap-3 text-[10px] text-surface-400 mr-2"><span> </span> <span> </span> <span> </span> <!> <!></div>'), Af = /* @__PURE__ */ p('<div class="text-[11px] text-health-err bg-health-err/10 rounded px-3 py-2"> </div>'), Df = /* @__PURE__ */ p(`<div class="rounded-lg border border-hecate-600/30 bg-surface-800/80 p-4 space-y-3"><h4 class="text-xs font-medium text-hecate-300 uppercase tracking-wider">New Work Card</h4> <div class="grid grid-cols-[1fr_auto] gap-3"><div><label for="card-title" class="text-[10px] text-surface-400 block mb-1">Title (desk name)</label> <input id="card-title" placeholder="e.g., register_user" class="w-full bg-surface-700 border border-surface-600 rounded px-2.5 py-1.5
							text-xs text-surface-100 placeholder-surface-400
							focus:outline-none focus:border-hecate-500/50"/></div> <div><label for="card-type" class="text-[10px] text-surface-400 block mb-1">Department</label> <select id="card-type" class="bg-surface-700 border border-surface-600 rounded px-2 py-1.5
							text-xs text-surface-100
							focus:outline-none focus:border-hecate-500/50 cursor-pointer"><option>CMD</option><option>PRJ</option><option>QRY</option></select></div></div> <div><label for="card-desc" class="text-[10px] text-surface-400 block mb-1">Description (optional)</label> <input id="card-desc" placeholder="Brief description of this desk" class="w-full bg-surface-700 border border-surface-600 rounded px-2.5 py-1.5
						text-xs text-surface-100 placeholder-surface-400
						focus:outline-none focus:border-hecate-500/50"/></div> <div class="flex gap-2"><button>Post</button> <button class="px-3 py-1.5 rounded text-xs text-surface-400 hover:text-surface-100">Cancel</button></div></div>`), Pf = /* @__PURE__ */ p('<div class="text-center py-8 text-surface-400 text-xs animate-pulse">Loading kanban board...</div>'), Tf = /* @__PURE__ */ p('<div class="text-center py-12 text-surface-500 text-xs border border-dashed border-surface-600 rounded-lg"><div class="text-2xl mb-3 text-surface-400"></div> <p class="mb-1">No work cards yet.</p> <p class="text-[10px] text-surface-500">Post cards from storming output, or add them manually above.</p></div>'), Rf = /* @__PURE__ */ p('<p class="text-[10px] text-surface-400 mb-2 leading-relaxed"> </p>'), Mf = /* @__PURE__ */ p(`<div class="rounded border border-surface-600 bg-surface-800/60 p-2.5 group"><div class="flex items-start gap-2 mb-1.5"><span class="text-xs font-medium text-surface-100 flex-1 leading-tight"> </span> <span> </span></div> <!> <div class="flex items-center justify-between"><span class="text-[9px] text-surface-500"> </span> <div class="flex items-center gap-1
									opacity-0 group-hover:opacity-100 transition-opacity"><button class="text-[10px] px-2 py-0.5 rounded bg-hecate-600/15 text-hecate-300
											hover:bg-hecate-600/25 transition-colors">Pick</button> <button class="text-[10px] px-1.5 py-0.5 rounded text-surface-400
											hover:text-health-warn hover:bg-health-warn/10 transition-colors" title="Park card"></button> <button class="text-[10px] px-1.5 py-0.5 rounded text-surface-400
											hover:text-health-err hover:bg-health-err/10 transition-colors" title="Block card"></button></div></div></div>`), If = /* @__PURE__ */ p('<p class="text-[10px] text-surface-400 mb-2 leading-relaxed"> </p>'), Nf = /* @__PURE__ */ p('<div class="text-[9px] text-surface-400 mb-2"> </div>'), Lf = /* @__PURE__ */ p(`<div class="rounded border border-surface-600 bg-surface-800/60 p-2.5 group"><div class="flex items-start gap-2 mb-1.5"><span class="text-xs font-medium text-surface-100 flex-1 leading-tight"> </span> <span> </span></div> <!> <!> <div class="flex items-center gap-1 justify-end
								opacity-0 group-hover:opacity-100 transition-opacity"><button class="text-[10px] px-2 py-0.5 rounded text-health-warn
										hover:bg-health-warn/10 transition-colors">Unpick</button> <button class="text-[10px] px-1.5 py-0.5 rounded text-surface-400
										hover:text-health-warn hover:bg-health-warn/10 transition-colors" title="Park card"></button> <button class="text-[10px] px-1.5 py-0.5 rounded text-surface-400
										hover:text-health-err hover:bg-health-err/10 transition-colors" title="Block card"></button> <button class="text-[10px] px-2 py-0.5 rounded bg-health-ok/15 text-health-ok
										hover:bg-health-ok/25 transition-colors">Finish</button></div></div>`), Of = /* @__PURE__ */ p('<div class="rounded border border-surface-600/50 bg-surface-800/30 p-2.5 opacity-70"><div class="flex items-start gap-2 mb-1"><span class="text-[10px] text-health-ok"></span> <span class="text-xs text-surface-300 flex-1 leading-tight"> </span> <span> </span></div> <div class="text-[9px] text-surface-500 ml-4"> </div></div>'), Ff = /* @__PURE__ */ p('<p class="text-[10px] text-health-warn/80 mb-2 italic leading-relaxed"> </p>'), jf = /* @__PURE__ */ p(`<div class="rounded border border-health-warn/20 bg-surface-800/60 p-2.5 group"><div class="flex items-start gap-2 mb-1"><span class="text-xs font-medium text-surface-200 flex-1 leading-tight"> </span> <span> </span></div> <!> <div class="flex items-center justify-between"><span class="text-[9px] text-surface-500"> </span> <button class="text-[10px] px-2 py-0.5 rounded text-health-warn
											hover:bg-health-warn/15 transition-colors
											opacity-0 group-hover:opacity-100">Unpark</button></div></div>`), Bf = /* @__PURE__ */ p('<p class="text-[10px] text-health-err/80 mb-2 italic leading-relaxed"> </p>'), Vf = /* @__PURE__ */ p(`<div class="rounded border border-health-err/20 bg-surface-800/60 p-2.5 group"><div class="flex items-start gap-2 mb-1"><span class="text-xs font-medium text-surface-200 flex-1 leading-tight"> </span> <span> </span></div> <!> <div class="flex items-center justify-between"><span class="text-[9px] text-surface-500"> </span> <button class="text-[10px] px-2 py-0.5 rounded text-health-err
											hover:bg-health-err/15 transition-colors
											opacity-0 group-hover:opacity-100">Unblock</button></div></div>`), Gf = /* @__PURE__ */ p('<div class="grid grid-cols-2 gap-3"><div class="rounded-lg border border-health-warn/30 bg-health-warn/5"><div class="px-3 py-2 border-b border-health-warn/20 flex items-center gap-2"><span class="text-[11px]"></span> <span class="text-[11px] font-semibold text-health-warn">Parked</span> <span class="text-[10px] text-surface-500 ml-auto"> </span></div> <div class="p-2 space-y-2"></div></div> <div class="rounded-lg border border-health-err/30 bg-health-err/5"><div class="px-3 py-2 border-b border-health-err/20 flex items-center gap-2"><span class="text-[11px]"></span> <span class="text-[11px] font-semibold text-health-err">Blocked</span> <span class="text-[10px] text-surface-500 ml-auto"> </span></div> <div class="p-2 space-y-2"></div></div></div>'), qf = /* @__PURE__ */ p('<div class="grid grid-cols-3 gap-3 min-h-[300px]"><div class="rounded-lg border border-surface-600 bg-surface-800/30"><div class="px-3 py-2 border-b border-surface-600 flex items-center gap-2"><span class="w-2 h-2 rounded-full bg-hecate-400"></span> <span class="text-[11px] font-semibold text-surface-200">Posted</span> <span class="text-[10px] text-surface-500 ml-auto"> </span></div> <div class="p-2 space-y-2"></div></div> <div class="rounded-lg border border-surface-600 bg-surface-800/30"><div class="px-3 py-2 border-b border-surface-600 flex items-center gap-2"><span class="w-2 h-2 rounded-full bg-phase-crafting"></span> <span class="text-[11px] font-semibold text-surface-200">Picked</span> <span class="text-[10px] text-surface-500 ml-auto"> </span></div> <div class="p-2 space-y-2"></div></div> <div class="rounded-lg border border-surface-600 bg-surface-800/30"><div class="px-3 py-2 border-b border-surface-600 flex items-center gap-2"><span class="w-2 h-2 rounded-full bg-health-ok"></span> <span class="text-[11px] font-semibold text-surface-200">Finished</span> <span class="text-[10px] text-surface-500 ml-auto"> </span></div> <div class="p-2 space-y-2"></div></div></div> <!>', 1), Hf = /* @__PURE__ */ p('<div class="fixed inset-0 bg-black/50 z-50 flex items-center justify-center" role="dialog" aria-modal="true"><div class="bg-surface-800 border border-surface-600 rounded-xl p-5 w-96 space-y-3"><h4 class="text-sm font-semibold text-surface-100"> </h4> <p class="text-[11px] text-surface-400"> <span class="text-surface-200 font-medium"> </span></p> <div><label for="modal-reason" class="text-[10px] text-surface-400 block mb-1">Reason</label> <textarea id="modal-reason" rows="3"></textarea></div> <div class="flex gap-2 justify-end"><button class="px-3 py-1.5 rounded text-xs text-surface-400 hover:text-surface-100">Cancel</button> <button> </button></div></div></div>'), Wf = /* @__PURE__ */ p(`<div class="p-4 space-y-4"><div class="flex items-center justify-between"><div><h3 class="text-sm font-semibold text-surface-100">Kanban</h3> <p class="text-[11px] text-surface-400 mt-0.5">Work cards for <span class="text-surface-200"> </span></p></div> <div class="flex items-center gap-2"><!> <button class="text-[11px] px-3 py-1 rounded bg-hecate-600/20 text-hecate-300
					hover:bg-hecate-600/30 transition-colors">+ Post Card</button></div></div> <!> <!> <!> <!></div>`);
function Co(e, t) {
  kt(t, !0);
  const r = () => ke(Ur, "$selectedDivision", y), n = () => ke(Mr, "$kanbanCards", y), c = () => ke(hf, "$cardCounts", y), l = () => ke(Dt, "$kanbanError", y), u = () => ke(Xa, "$kanbanLoading", y), d = () => ke(uf, "$postedCards", y), f = () => ke(vf, "$pickedCards", y), g = () => ke(ff, "$finishedCards", y), m = () => ke(pf, "$parkedCards", y), k = () => ke(xf, "$blockedCards", y), [y, P] = Vt();
  let E = /* @__PURE__ */ oe(null);
  Tt(() => {
    const T = r();
    T && T.division_id !== s(E) && (x(E, T.division_id, !0), Nr(T.division_id));
  });
  let Q = /* @__PURE__ */ oe(!1), S = /* @__PURE__ */ oe(""), U = /* @__PURE__ */ oe(""), ae = /* @__PURE__ */ oe("cmd_desk"), we = /* @__PURE__ */ oe(null), le = /* @__PURE__ */ oe("unpick"), ve = /* @__PURE__ */ oe("");
  async function de() {
    if (!r() || !s(S).trim()) return;
    await _f(r().division_id, {
      title: s(S).trim(),
      description: s(U).trim() || void 0,
      card_type: s(ae),
      posted_by: "hecate-web"
    }) && (x(S, ""), x(U, ""), x(Q, !1));
  }
  async function Le(T) {
    r() && await gf(r().division_id, T.card_id);
  }
  async function Ae(T) {
    r() && await bf(r().division_id, T.card_id);
  }
  function De(T, I) {
    x(we, T, !0), x(le, I, !0), x(ve, "");
  }
  async function te() {
    if (!r() || !s(we) || !s(ve).trim()) return;
    const T = r().division_id, I = s(we).card_id;
    let Z = !1;
    s(le) === "unpick" ? Z = await mf(T, I, s(ve).trim()) : s(le) === "park" ? Z = await yf(T, I, s(ve).trim()) : s(le) === "block" && (Z = await kf(T, I, s(ve).trim())), Z && (x(we, null), x(ve, ""));
  }
  async function N(T) {
    r() && await wf(r().division_id, T.card_id);
  }
  async function X(T) {
    r() && await $f(r().division_id, T.card_id);
  }
  function ue(T) {
    switch (T) {
      case "cmd_desk":
        return "CMD";
      case "prj_desk":
        return "PRJ";
      case "qry_desk":
        return "QRY";
      default:
        return T;
    }
  }
  function Te(T) {
    switch (T) {
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
  function ne(T) {
    return T ? new Date(T).toLocaleDateString(void 0, {
      month: "short",
      day: "numeric",
      hour: "2-digit",
      minute: "2-digit"
    }) : "";
  }
  const W = {
    unpick: { title: "Unpick Card", verb: "Unpick", color: "health-warn" },
    park: { title: "Park Card", verb: "Park", color: "health-warn" },
    block: { title: "Block Card", verb: "Block", color: "health-err" }
  };
  var K = Wf(), Fe = i(K), et = i(Fe), st = o(i(et), 2), Xe = o(i(st)), We = i(Xe, !0);
  a(Xe), a(st), a(et);
  var Ge = o(et, 2), C = i(Ge);
  {
    var V = (T) => {
      var I = Ef(), Z = i(I), Me = i(Z);
      a(Z);
      var Be = o(Z, 2), Je = i(Be);
      a(Be);
      var ze = o(Be, 2), tt = i(ze);
      a(ze);
      var qe = o(ze, 2);
      {
        var Ue = (R) => {
          var G = Cf(), fe = i(G);
          a(G), $(() => h(fe, `${c().parked ?? ""} parked`)), v(R, G);
        };
        A(qe, (R) => {
          c().parked > 0 && R(Ue);
        });
      }
      var Ye = o(qe, 2);
      {
        var je = (R) => {
          var G = Sf(), fe = i(G);
          a(G), $(() => h(fe, `${c().blocked ?? ""} blocked`)), v(R, G);
        };
        A(Ye, (R) => {
          c().blocked > 0 && R(je);
        });
      }
      a(I), $(() => {
        h(Me, `${c().posted ?? ""} posted`), h(Je, `${c().picked ?? ""} picked`), h(tt, `${c().finished ?? ""} done`);
      }), v(T, I);
    };
    A(C, (T) => {
      n().length > 0 && T(V);
    });
  }
  var H = o(C, 2);
  H.__click = () => x(Q, !s(Q)), a(Ge), a(Fe);
  var M = o(Fe, 2);
  {
    var L = (T) => {
      var I = Af(), Z = i(I, !0);
      a(I), $(() => h(Z, l())), v(T, I);
    };
    A(M, (T) => {
      l() && T(L);
    });
  }
  var F = o(M, 2);
  {
    var J = (T) => {
      var I = Df(), Z = o(i(I), 2), Me = i(Z), Be = o(i(Me), 2);
      bt(Be), a(Me);
      var Je = o(Me, 2), ze = o(i(Je), 2), tt = i(ze);
      tt.value = tt.__value = "cmd_desk";
      var qe = o(tt);
      qe.value = qe.__value = "prj_desk";
      var Ue = o(qe);
      Ue.value = Ue.__value = "qry_desk", a(ze), a(Je), a(Z);
      var Ye = o(Z, 2), je = o(i(Ye), 2);
      bt(je), a(Ye);
      var R = o(Ye, 2), G = i(R);
      G.__click = de;
      var fe = o(G, 2);
      fe.__click = () => x(Q, !1), a(R), a(I), $(
        (Pe, ie) => {
          G.disabled = Pe, Ie(G, 1, `px-3 py-1.5 rounded text-xs transition-colors
						${ie ?? ""}`);
        },
        [
          () => !s(S).trim(),
          () => s(S).trim() ? "bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30" : "bg-surface-700 text-surface-500 cursor-not-allowed"
        ]
      ), xt(Be, () => s(S), (Pe) => x(S, Pe)), js(ze, () => s(ae), (Pe) => x(ae, Pe)), xt(je, () => s(U), (Pe) => x(U, Pe)), v(T, I);
    };
    A(F, (T) => {
      s(Q) && T(J);
    });
  }
  var $e = o(F, 2);
  {
    var w = (T) => {
      var I = Pf();
      v(T, I);
    }, D = (T) => {
      var I = Tf(), Z = i(I);
      Z.textContent = "☐", At(4), a(I), v(T, I);
    }, z = (T) => {
      var I = qf(), Z = ct(I), Me = i(Z), Be = i(Me), Je = o(i(Be), 4), ze = i(Je, !0);
      a(Je), a(Be);
      var tt = o(Be, 2);
      He(tt, 5, d, (_) => _.card_id, (_, b) => {
        var re = Mf(), j = i(re), ce = i(j), _e = i(ce, !0);
        a(ce);
        var ge = o(ce, 2), Y = i(ge, !0);
        a(ge), a(j);
        var q = o(j, 2);
        {
          var ee = (Se) => {
            var Re = Rf(), Ve = i(Re, !0);
            a(Re), $(() => h(Ve, s(b).description)), v(Se, Re);
          };
          A(q, (Se) => {
            s(b).description && Se(ee);
          });
        }
        var me = o(q, 2), xe = i(me), he = i(xe, !0);
        a(xe);
        var se = o(xe, 2), B = i(se);
        B.__click = () => Le(s(b));
        var O = o(B, 2);
        O.__click = () => De(s(b), "park"), O.textContent = "⏸";
        var ye = o(O, 2);
        ye.__click = () => De(s(b), "block"), ye.textContent = "⛔", a(se), a(me), a(re), $(
          (Se, Re, Ve) => {
            h(_e, s(b).title), Ie(ge, 1, `text-[9px] px-1.5 py-0.5 rounded ${Se ?? ""} shrink-0`), h(Y, Re), h(he, Ve);
          },
          [
            () => Te(s(b).card_type),
            () => ue(s(b).card_type),
            () => ne(s(b).posted_at)
          ]
        ), v(_, re);
      }), a(tt), a(Me);
      var qe = o(Me, 2), Ue = i(qe), Ye = o(i(Ue), 4), je = i(Ye, !0);
      a(Ye), a(Ue);
      var R = o(Ue, 2);
      He(R, 5, f, (_) => _.card_id, (_, b) => {
        var re = Lf(), j = i(re), ce = i(j), _e = i(ce, !0);
        a(ce);
        var ge = o(ce, 2), Y = i(ge, !0);
        a(ge), a(j);
        var q = o(j, 2);
        {
          var ee = (Se) => {
            var Re = If(), Ve = i(Re, !0);
            a(Re), $(() => h(Ve, s(b).description)), v(Se, Re);
          };
          A(q, (Se) => {
            s(b).description && Se(ee);
          });
        }
        var me = o(q, 2);
        {
          var xe = (Se) => {
            var Re = Nf(), Ve = i(Re);
            a(Re), $(() => h(Ve, `Picked by ${s(b).picked_by ?? ""}`)), v(Se, Re);
          };
          A(me, (Se) => {
            s(b).picked_by && Se(xe);
          });
        }
        var he = o(me, 2), se = i(he);
        se.__click = () => De(s(b), "unpick");
        var B = o(se, 2);
        B.__click = () => De(s(b), "park"), B.textContent = "⏸";
        var O = o(B, 2);
        O.__click = () => De(s(b), "block"), O.textContent = "⛔";
        var ye = o(O, 2);
        ye.__click = () => Ae(s(b)), a(he), a(re), $(
          (Se, Re) => {
            h(_e, s(b).title), Ie(ge, 1, `text-[9px] px-1.5 py-0.5 rounded ${Se ?? ""} shrink-0`), h(Y, Re);
          },
          [
            () => Te(s(b).card_type),
            () => ue(s(b).card_type)
          ]
        ), v(_, re);
      }), a(R), a(qe);
      var G = o(qe, 2), fe = i(G), Pe = o(i(fe), 4), ie = i(Pe, !0);
      a(Pe), a(fe);
      var be = o(fe, 2);
      He(be, 5, g, (_) => _.card_id, (_, b) => {
        var re = Of(), j = i(re), ce = i(j);
        ce.textContent = "✓";
        var _e = o(ce, 2), ge = i(_e, !0);
        a(_e);
        var Y = o(_e, 2), q = i(Y, !0);
        a(Y), a(j);
        var ee = o(j, 2), me = i(ee, !0);
        a(ee), a(re), $(
          (xe, he, se) => {
            h(ge, s(b).title), Ie(Y, 1, `text-[9px] px-1.5 py-0.5 rounded ${xe ?? ""} shrink-0`), h(q, he), h(me, se);
          },
          [
            () => Te(s(b).card_type),
            () => ue(s(b).card_type),
            () => ne(s(b).finished_at)
          ]
        ), v(_, re);
      }), a(be), a(G), a(Z);
      var Ne = o(Z, 2);
      {
        var Oe = (_) => {
          var b = Gf(), re = i(b), j = i(re), ce = i(j);
          ce.textContent = "⏸";
          var _e = o(ce, 4), ge = i(_e, !0);
          a(_e), a(j);
          var Y = o(j, 2);
          He(Y, 5, m, (B) => B.card_id, (B, O) => {
            var ye = jf(), Se = i(ye), Re = i(Se), Ve = i(Re, !0);
            a(Re);
            var Qe = o(Re, 2), rt = i(Qe, !0);
            a(Qe), a(Se);
            var at = o(Se, 2);
            {
              var nt = (gt) => {
                var Ht = Ff(), kr = i(Ht, !0);
                a(Ht), $(() => h(kr, s(O).park_reason)), v(gt, Ht);
              };
              A(at, (gt) => {
                s(O).park_reason && gt(nt);
              });
            }
            var Ct = o(at, 2), ht = i(Ct), It = i(ht);
            a(ht);
            var qt = o(ht, 2);
            qt.__click = () => N(s(O)), a(Ct), a(ye), $(
              (gt, Ht, kr) => {
                h(Ve, s(O).title), Ie(Qe, 1, `text-[9px] px-1.5 py-0.5 rounded ${gt ?? ""} shrink-0`), h(rt, Ht), h(It, `${s(O).parked_by ? `by ${s(O).parked_by}` : ""}
										${kr ?? ""}`);
              },
              [
                () => Te(s(O).card_type),
                () => ue(s(O).card_type),
                () => ne(s(O).parked_at)
              ]
            ), v(B, ye);
          }), a(Y), a(re);
          var q = o(re, 2), ee = i(q), me = i(ee);
          me.textContent = "⛔";
          var xe = o(me, 4), he = i(xe, !0);
          a(xe), a(ee);
          var se = o(ee, 2);
          He(se, 5, k, (B) => B.card_id, (B, O) => {
            var ye = Vf(), Se = i(ye), Re = i(Se), Ve = i(Re, !0);
            a(Re);
            var Qe = o(Re, 2), rt = i(Qe, !0);
            a(Qe), a(Se);
            var at = o(Se, 2);
            {
              var nt = (gt) => {
                var Ht = Bf(), kr = i(Ht, !0);
                a(Ht), $(() => h(kr, s(O).block_reason)), v(gt, Ht);
              };
              A(at, (gt) => {
                s(O).block_reason && gt(nt);
              });
            }
            var Ct = o(at, 2), ht = i(Ct), It = i(ht);
            a(ht);
            var qt = o(ht, 2);
            qt.__click = () => X(s(O)), a(Ct), a(ye), $(
              (gt, Ht, kr) => {
                h(Ve, s(O).title), Ie(Qe, 1, `text-[9px] px-1.5 py-0.5 rounded ${gt ?? ""} shrink-0`), h(rt, Ht), h(It, `${s(O).blocked_by ? `by ${s(O).blocked_by}` : ""}
										${kr ?? ""}`);
              },
              [
                () => Te(s(O).card_type),
                () => ue(s(O).card_type),
                () => ne(s(O).blocked_at)
              ]
            ), v(B, ye);
          }), a(se), a(q), a(b), $(() => {
            h(ge, m().length), h(he, k().length);
          }), v(_, b);
        };
        A(Ne, (_) => {
          (m().length > 0 || k().length > 0) && _(Oe);
        });
      }
      $(() => {
        h(ze, d().length), h(je, f().length), h(ie, g().length);
      }), v(T, I);
    };
    A($e, (T) => {
      u() ? T(w) : n().length === 0 && !s(Q) ? T(D, 1) : T(z, !1);
    });
  }
  var pe = o($e, 2);
  {
    var Ce = (T) => {
      const I = /* @__PURE__ */ Ee(() => W[s(le)]);
      var Z = Hf(), Me = i(Z), Be = i(Me), Je = i(Be, !0);
      a(Be);
      var ze = o(Be, 2), tt = i(ze), qe = o(tt), Ue = i(qe, !0);
      a(qe), a(ze);
      var Ye = o(ze, 2), je = o(i(Ye), 2);
      Ws(je), a(Ye);
      var R = o(Ye, 2), G = i(R);
      G.__click = () => x(we, null);
      var fe = o(G, 2);
      fe.__click = te;
      var Pe = i(fe, !0);
      a(fe), a(R), a(Me), a(Z), $(
        (ie, be) => {
          h(Je, s(I).title), h(tt, `${s(I).verb ?? ""}ing `), h(Ue, s(we).title), Ot(je, "placeholder", `Why is this card being ${s(le) === "unpick" ? "unpicked" : s(le) === "park" ? "parked" : "blocked"}?`), Ie(je, 1, `w-full bg-surface-700 border border-surface-600 rounded px-2.5 py-1.5
							text-xs text-surface-100 placeholder-surface-400
							focus:outline-none focus:border-${s(I).color ?? ""}/50 resize-none`), fe.disabled = ie, Ie(fe, 1, `px-3 py-1.5 rounded text-xs transition-colors
							${be ?? ""}`), h(Pe, s(I).verb);
        },
        [
          () => !s(ve).trim(),
          () => s(ve).trim() ? `bg-${s(I).color}/20 text-${s(I).color} hover:bg-${s(I).color}/30` : "bg-surface-700 text-surface-500 cursor-not-allowed"
        ]
      ), xt(je, () => s(ve), (ie) => x(ve, ie)), v(T, Z);
    };
    A(pe, (T) => {
      s(we) && T(Ce);
    });
  }
  a(K), $(() => h(We, r()?.context_name)), v(e, K), $t(), P();
}
Mt(["click"]);
Et(Co, {}, [], [], { mode: "open" });
const So = Ze(null);
async function Uf(e, t) {
  try {
    return await Ke().post(`/craftings/${e}/generate-module`, t), !0;
  } catch (r) {
    const n = r;
    return So.set(n.message || "Failed to generate module"), !1;
  }
}
async function zf(e, t) {
  try {
    return await Ke().post(`/craftings/${e}/deliver-release`, { version: t }), !0;
  } catch (r) {
    const n = r;
    return So.set(n.message || "Failed to deliver release"), !1;
  }
}
var Yf = /* @__PURE__ */ p(`<div class="flex gap-2 items-end mb-4 p-3 rounded bg-surface-700/30"><div class="flex-1"><label for="mod-name" class="text-[10px] text-surface-400 block mb-1">Module Name</label> <input id="mod-name" placeholder="e.g., register_user_v1" class="w-full bg-surface-700 border border-surface-600 rounded
								px-2.5 py-1.5 text-xs text-surface-100 placeholder-surface-400
								focus:outline-none focus:border-hecate-500/50"/></div> <div class="flex-1"><label for="mod-template" class="text-[10px] text-surface-400 block mb-1">Template (optional)</label> <input id="mod-template" placeholder="e.g., command, event, handler" class="w-full bg-surface-700 border border-surface-600 rounded
								px-2.5 py-1.5 text-xs text-surface-100 placeholder-surface-400
								focus:outline-none focus:border-hecate-500/50"/></div> <button class="px-3 py-1.5 rounded text-xs bg-hecate-600/20 text-hecate-300
							hover:bg-hecate-600/30 transition-colors disabled:opacity-50">Generate</button> <button class="px-3 py-1.5 rounded text-xs text-surface-400 hover:text-surface-100">Cancel</button></div>`), Kf = /* @__PURE__ */ p(
  `<div class="rounded-lg border border-surface-600 bg-surface-800/50 p-4"><div class="flex items-center justify-between mb-3"><h4 class="text-xs font-semibold text-surface-100">Code Generation</h4> <button class="text-[10px] px-2 py-0.5 rounded bg-hecate-600/10 text-hecate-300
						hover:bg-hecate-600/20 transition-colors">+ Generate Module</button></div> <!> <p class="text-[10px] text-surface-400">Generate Erlang modules from templates based on planned desks and design
				artifacts.</p></div> <div><h4 class="text-xs font-semibold text-surface-100 mb-3">Implementation Tasks</h4> <div class="grid grid-cols-1 md:grid-cols-2 gap-3"><!> <!> <!> <!> <!> <!></div></div>`,
  1
), Jf = /* @__PURE__ */ p(`<div class="flex gap-2 items-end mb-4 p-3 rounded bg-surface-700/30"><div class="flex-1"><label for="rel-version" class="text-[10px] text-surface-400 block mb-1">Version</label> <input id="rel-version" placeholder="e.g., 0.1.0" class="w-full bg-surface-700 border border-surface-600 rounded
								px-2.5 py-1.5 text-xs text-surface-100 placeholder-surface-400
								focus:outline-none focus:border-hecate-500/50"/></div> <button class="px-3 py-1.5 rounded text-xs bg-hecate-600/20 text-hecate-300
							hover:bg-hecate-600/30 transition-colors disabled:opacity-50">Deliver</button> <button class="px-3 py-1.5 rounded text-xs text-surface-400 hover:text-surface-100">Cancel</button></div>`), Qf = /* @__PURE__ */ p(
  `<div class="rounded-lg border border-surface-600 bg-surface-800/50 p-4"><div class="flex items-center justify-between mb-3"><h4 class="text-xs font-semibold text-surface-100">Releases</h4> <button class="text-[10px] px-2 py-0.5 rounded bg-hecate-600/10 text-hecate-300
						hover:bg-hecate-600/20 transition-colors">+ Deliver Release</button></div> <!> <p class="text-[10px] text-surface-400">Deliver through GitOps: version bump, git tag, CI/CD builds and deploys.</p></div> <div><h4 class="text-xs font-semibold text-surface-100 mb-3">Delivery Tasks</h4> <div class="grid grid-cols-1 md:grid-cols-2 gap-3"><!> <!> <!> <!></div></div>`,
  1
), Xf = /* @__PURE__ */ p('<div class="p-4 space-y-6"><div class="flex items-center justify-between"><div><h3 class="text-sm font-semibold text-surface-100">Crafting</h3> <p class="text-[11px] text-surface-400 mt-0.5">Generate code, run tests, and deliver releases for <span class="text-surface-200"> </span></p></div> <div class="flex items-center gap-1 bg-surface-700/50 rounded-lg p-0.5"><button>Implementation</button> <button>Delivery</button></div></div> <!></div>');
function Eo(e, t) {
  kt(t, !0);
  const r = () => ke(Ur, "$selectedDivision", c), n = () => ke(wt, "$isLoading", c), [c, l] = Vt();
  let u = /* @__PURE__ */ oe("implement"), d = /* @__PURE__ */ oe(!1), f = /* @__PURE__ */ oe(""), g = /* @__PURE__ */ oe(""), m = /* @__PURE__ */ oe(!1), k = /* @__PURE__ */ oe("");
  async function y() {
    if (!r() || !s(f).trim()) return;
    await Uf(r().division_id, {
      module_name: s(f).trim(),
      template: s(g).trim() || void 0
    }) && (x(f, ""), x(g, ""), x(d, !1));
  }
  async function P() {
    if (!r() || !s(k).trim()) return;
    await zf(r().division_id, s(k).trim()) && (x(k, ""), x(m, !1));
  }
  var E = Xf(), Q = i(E), S = i(Q), U = o(i(S), 2), ae = o(i(U)), we = i(ae, !0);
  a(ae), a(U), a(S);
  var le = o(S, 2), ve = i(le);
  ve.__click = () => x(u, "implement");
  var de = o(ve, 2);
  de.__click = () => x(u, "deliver"), a(le), a(Q);
  var Le = o(Q, 2);
  {
    var Ae = (te) => {
      var N = Kf(), X = ct(N), ue = i(X), Te = o(i(ue), 2);
      Te.__click = () => x(d, !s(d)), a(ue);
      var ne = o(ue, 2);
      {
        var W = (V) => {
          var H = Yf(), M = i(H), L = o(i(M), 2);
          bt(L), a(M);
          var F = o(M, 2), J = o(i(F), 2);
          bt(J), a(F);
          var $e = o(F, 2);
          $e.__click = y;
          var w = o($e, 2);
          w.__click = () => x(d, !1), a(H), $((D) => $e.disabled = D, [() => !s(f).trim() || n()]), xt(L, () => s(f), (D) => x(f, D)), xt(J, () => s(g), (D) => x(g, D)), v(V, H);
        };
        A(ne, (V) => {
          s(d) && V(W);
        });
      }
      At(2), a(X);
      var K = o(X, 2), Fe = o(i(K), 2), et = i(Fe);
      {
        let V = /* @__PURE__ */ Ee(() => `Help me implement the walking skeleton for the "${r()?.context_name}" division. We need initiate_{aggregate} and archive_{aggregate} desks first. Generate the Erlang module structure for each.`);
        _t(et, {
          title: "Walking Skeleton",
          description: "Generate initiate + archive desks first, establishing the aggregate lifecycle foundation",
          icon: "⚲",
          get aiContext() {
            return s(V);
          }
        });
      }
      var st = o(et, 2);
      {
        let V = /* @__PURE__ */ Ee(() => `Help me generate Erlang command modules for the "${r()?.context_name}" division. Each command needs: module, record, to_map/1, from_map/1. Use the evoq command pattern.`);
        _t(st, {
          title: "Generate Commands",
          description: "Create command modules from the desk inventory with proper versioning",
          icon: "▶",
          get aiContext() {
            return s(V);
          }
        });
      }
      var Xe = o(st, 2);
      {
        let V = /* @__PURE__ */ Ee(() => `Help me generate Erlang event modules for the "${r()?.context_name}" division. Each event needs: module, record, to_map/1, from_map/1. Follow the event naming convention: {subject}_{verb_past}_v{N}.`);
        _t(Xe, {
          title: "Generate Events",
          description: "Create event modules matching the designed domain events",
          icon: "◆",
          get aiContext() {
            return s(V);
          }
        });
      }
      var We = o(Xe, 2);
      {
        let V = /* @__PURE__ */ Ee(() => `Help me write EUnit tests for the "${r()?.context_name}" division. Cover aggregate behavior (execute + apply), handler dispatch, and projection state updates.`);
        _t(We, {
          title: "Write Tests",
          description: "Generate EUnit test modules for aggregates, handlers, and projections",
          icon: "✓",
          get aiContext() {
            return s(V);
          }
        });
      }
      var Ge = o(We, 2);
      {
        let V = /* @__PURE__ */ Ee(() => `Help me analyze test results for the "${r()?.context_name}" division. What patterns should I look for? How do I ensure adequate coverage of the aggregate lifecycle?`);
        _t(Ge, {
          title: "Run Test Suite",
          description: "Execute all tests and review results for quality gates",
          icon: "▷",
          get aiContext() {
            return s(V);
          }
        });
      }
      var C = o(Ge, 2);
      {
        let V = /* @__PURE__ */ Ee(() => `Help me define acceptance criteria for the "${r()?.context_name}" division. What must be true before we can say this division is implemented correctly?`);
        _t(C, {
          title: "Acceptance Criteria",
          description: "Validate that implementation meets the design specifications",
          icon: "☑",
          get aiContext() {
            return s(V);
          }
        });
      }
      a(Fe), a(K), v(te, N);
    }, De = (te) => {
      var N = Qf(), X = ct(N), ue = i(X), Te = o(i(ue), 2);
      Te.__click = () => x(m, !s(m)), a(ue);
      var ne = o(ue, 2);
      {
        var W = (Ge) => {
          var C = Jf(), V = i(C), H = o(i(V), 2);
          bt(H), a(V);
          var M = o(V, 2);
          M.__click = P;
          var L = o(M, 2);
          L.__click = () => x(m, !1), a(C), $((F) => M.disabled = F, [() => !s(k).trim() || n()]), xt(H, () => s(k), (F) => x(k, F)), v(Ge, C);
        };
        A(ne, (Ge) => {
          s(m) && Ge(W);
        });
      }
      At(2), a(X);
      var K = o(X, 2), Fe = o(i(K), 2), et = i(Fe);
      {
        let Ge = /* @__PURE__ */ Ee(() => `Help me prepare a release for the "${r()?.context_name}" division. Walk me through the GitOps flow: version bump, CHANGELOG update, git tag, and CI/CD pipeline.`);
        _t(et, {
          title: "Release Management",
          description: "Prepare release: version bump, changelog, git tag, push for CI/CD",
          icon: "↑",
          get aiContext() {
            return s(Ge);
          }
        });
      }
      var st = o(et, 2);
      {
        let Ge = /* @__PURE__ */ Ee(() => `Help me plan a staged rollout for the "${r()?.context_name}" division. How should we structure canary deployments with health gates on the beam cluster?`);
        _t(st, {
          title: "Staged Rollout",
          description: "Plan a staged rollout with canary deployment and health gates",
          icon: "▤",
          get aiContext() {
            return s(Ge);
          }
        });
      }
      var Xe = o(st, 2);
      {
        let Ge = /* @__PURE__ */ Ee(() => `Help me set up health monitoring for the "${r()?.context_name}" division. What health checks should we configure? What SLA thresholds make sense?`);
        _t(Xe, {
          title: "Health Monitoring",
          description: "Configure health checks and SLA thresholds",
          icon: "♥",
          get aiContext() {
            return s(Ge);
          }
        });
      }
      var We = o(Xe, 2);
      {
        let Ge = /* @__PURE__ */ Ee(() => `Help me set up observability for the "${r()?.context_name}" division. What should we log? What metrics should we track? How do we set up distributed tracing on the beam cluster?`);
        _t(We, {
          title: "Observability",
          description: "Set up logging, metrics, and tracing for production visibility",
          icon: "◎",
          get aiContext() {
            return s(Ge);
          }
        });
      }
      a(Fe), a(K), v(te, N);
    };
    A(Le, (te) => {
      s(u) === "implement" ? te(Ae) : te(De, !1);
    });
  }
  a(E), $(() => {
    h(we, r()?.context_name), Ie(ve, 1, `px-3 py-1 rounded text-[11px] transition-colors
					${s(u) === "implement" ? "bg-surface-600 text-surface-100" : "text-surface-400 hover:text-surface-200"}`), Ie(de, 1, `px-3 py-1 rounded text-[11px] transition-colors
					${s(u) === "deliver" ? "bg-surface-600 text-surface-100" : "text-surface-400 hover:text-surface-200"}`);
  }), v(e, E), $t(), l();
}
Mt(["click"]);
Et(Eo, {}, [], [], { mode: "open" });
var Zf = /* @__PURE__ */ p('<div class="svelte-1ug3tqa"> </div>'), ep = /* @__PURE__ */ p('<div class="svelte-1ug3tqa"> </div>'), tp = /* @__PURE__ */ p('<div class="text-[9px] text-surface-400 space-y-0.5 svelte-1ug3tqa"><!> <!></div>'), rp = /* @__PURE__ */ p('<div class="text-[9px] text-surface-500 italic svelte-1ug3tqa">No sessions</div>'), sp = /* @__PURE__ */ p(`<span role="button" tabindex="0" class="absolute top-1.5 right-1.5 text-[8px] px-1.5 py-0.5 rounded cursor-pointer
				bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30
				opacity-0 group-hover:opacity-100 transition-opacity svelte-1ug3tqa">Run</span>`), ap = /* @__PURE__ */ p('<button><div class="flex items-center gap-2 mb-1.5 svelte-1ug3tqa"><span></span> <span class="text-xs font-semibold text-surface-100 flex-1 truncate svelte-1ug3tqa"> </span> <span class="text-[9px] text-surface-500 svelte-1ug3tqa"> </span></div> <!> <!></button>');
const np = {
  hash: "svelte-1ug3tqa",
  code: `
	@keyframes svelte-1ug3tqa-pulse-subtle {
		0%, 100% { opacity: 1; }
		50% { opacity: 0.85; }
	}.animate-pulse-subtle {
		animation: svelte-1ug3tqa-pulse-subtle 2s ease-in-out infinite;}`
};
function ta(e, t) {
  kt(t, !0), Ji(e, np);
  let r = pt(t, "roleStatus", 7), n = pt(t, "onSelect", 7), c = pt(t, "onInitiate", 7);
  const l = /* @__PURE__ */ Ee(() => Hr[r().role]), u = {
    idle: { dot: "bg-surface-500", label: "Idle" },
    running: { dot: "bg-hecate-400 animate-pulse", label: "Active" },
    completed: { dot: "bg-health-ok", label: "Done" },
    failed: { dot: "bg-health-err", label: "Failed" },
    gate_pending: { dot: "bg-health-warn animate-pulse", label: "Gate" }
  }, d = /* @__PURE__ */ Ee(() => u[r().status]);
  var f = {
    get roleStatus() {
      return r();
    },
    set roleStatus(ve) {
      r(ve), vt();
    },
    get onSelect() {
      return n();
    },
    set onSelect(ve) {
      n(ve), vt();
    },
    get onInitiate() {
      return c();
    },
    set onInitiate(ve) {
      c(ve), vt();
    }
  }, g = ap();
  g.__click = () => n()(r().role);
  var m = i(g), k = i(m), y = o(k, 2), P = i(y, !0);
  a(y);
  var E = o(y, 2), Q = i(E, !0);
  a(E), a(m);
  var S = o(m, 2);
  {
    var U = (ve) => {
      const de = /* @__PURE__ */ Ee(() => r().active_session);
      var Le = tp(), Ae = i(Le);
      {
        var De = (X) => {
          var ue = Zf(), Te = i(ue);
          a(ue), $((ne, W) => h(Te, `${ne ?? ""} in / ${W ?? ""} out`), [
            () => s(de).tokens_in.toLocaleString(),
            () => s(de).tokens_out.toLocaleString()
          ]), v(X, ue);
        };
        A(Ae, (X) => {
          (s(de).tokens_in > 0 || s(de).tokens_out > 0) && X(De);
        });
      }
      var te = o(Ae, 2);
      {
        var N = (X) => {
          var ue = ep(), Te = i(ue);
          a(ue), $(() => h(Te, `${r().session_count ?? ""} sessions`)), v(X, ue);
        };
        A(te, (X) => {
          r().session_count > 1 && X(N);
        });
      }
      a(Le), v(ve, Le);
    }, ae = (ve) => {
      var de = rp();
      v(ve, de);
    };
    A(S, (ve) => {
      r().active_session ? ve(U) : ve(ae, !1);
    });
  }
  var we = o(S, 2);
  {
    var le = (ve) => {
      var de = sp();
      de.__click = (Le) => {
        Le.stopPropagation(), c()(r().role);
      }, de.__keydown = (Le) => {
        Le.key === "Enter" && (Le.stopPropagation(), c()(r().role));
      }, v(ve, de);
    };
    A(we, (ve) => {
      r().status === "idle" && ve(le);
    });
  }
  return a(g), $(() => {
    Ie(
      g,
      1,
      `group relative text-left p-3 rounded-lg border transition-all
		${r().status === "gate_pending" ? "border-health-warn/50 bg-health-warn/5 shadow-sm shadow-health-warn/10 animate-pulse-subtle" : r().status === "running" ? "border-hecate-500/40 bg-hecate-600/5" : "border-surface-600 bg-surface-800/60 hover:border-surface-500"}`,
      "svelte-1ug3tqa"
    ), Ie(k, 1, `w-2 h-2 rounded-full ${s(d).dot ?? ""} shrink-0`, "svelte-1ug3tqa"), h(P, s(l).label), h(Q, s(d).label);
  }), v(e, g), $t(f);
}
Mt(["click", "keydown"]);
Et(ta, { roleStatus: {}, onSelect: {}, onInitiate: {} }, [], [], { mode: "open" });
var ip = /* @__PURE__ */ p('<span class="text-[9px] text-surface-400 ml-auto"> </span>'), op = /* @__PURE__ */ p('<div class="rounded bg-surface-800 border border-surface-600 p-3 max-h-64 overflow-y-auto"><pre class="text-[10px] text-surface-200 whitespace-pre-wrap font-mono leading-relaxed"> </pre></div>'), cp = /* @__PURE__ */ p('<div class="rounded bg-surface-800 border border-surface-600 p-3 max-h-64 overflow-y-auto"><pre class="text-[10px] text-surface-200 whitespace-pre-wrap font-mono leading-relaxed"> </pre></div>'), lp = /* @__PURE__ */ p("<span> </span>"), dp = /* @__PURE__ */ p(`<div class="space-y-2"><textarea placeholder="Reason for rejecting..." rows="2" class="w-full bg-surface-700 border border-surface-600 rounded px-2.5 py-1.5
					text-xs text-surface-100 placeholder-surface-400
					focus:outline-none focus:border-health-err/50 resize-none"></textarea> <div class="flex gap-2"><button>Confirm Reject</button> <button class="px-3 py-1.5 rounded text-xs text-surface-400 hover:text-surface-100">Cancel</button></div></div>`), up = /* @__PURE__ */ p(`<div class="flex gap-2"><button class="px-4 py-1.5 rounded text-xs font-medium
					bg-health-ok/20 text-health-ok hover:bg-health-ok/30 transition-colors"></button> <button class="px-4 py-1.5 rounded text-xs font-medium
					bg-health-err/20 text-health-err hover:bg-health-err/30 transition-colors"></button></div>`), vp = /* @__PURE__ */ p('<div class="rounded-lg border-2 border-health-warn/40 bg-health-warn/5 p-4 space-y-3"><div class="flex items-center gap-2"><span class="w-2 h-2 rounded-full bg-health-warn animate-pulse"></span> <h4 class="text-xs font-semibold text-health-warn uppercase tracking-wider"> </h4> <!></div> <!> <div class="flex items-center gap-4 text-[9px] text-surface-400"><span> </span> <!></div> <!></div>');
function Ao(e, t) {
  kt(t, !0);
  let r = pt(t, "session", 7), n = pt(t, "onPass", 7), c = pt(t, "onReject", 7), l = /* @__PURE__ */ oe(""), u = /* @__PURE__ */ oe(!1);
  const d = /* @__PURE__ */ Ee(() => Hr[r().role]);
  function f() {
    s(l).trim() && (c()(s(l).trim()), x(l, ""), x(u, !1));
  }
  var g = {
    get session() {
      return r();
    },
    set session(N) {
      r(N), vt();
    },
    get onPass() {
      return n();
    },
    set onPass(N) {
      n(N), vt();
    },
    get onReject() {
      return c();
    },
    set onReject(N) {
      c(N), vt();
    }
  }, m = vp(), k = i(m), y = o(i(k), 2), P = i(y);
  a(y);
  var E = o(y, 2);
  {
    var Q = (N) => {
      var X = ip(), ue = i(X, !0);
      a(X), $(() => h(ue, r().division_id)), v(N, X);
    };
    A(E, (N) => {
      r().division_id && N(Q);
    });
  }
  a(k);
  var S = o(k, 2);
  {
    var U = (N) => {
      var X = op(), ue = i(X), Te = i(ue, !0);
      a(ue), a(X), $(() => h(Te, r().gate_output)), v(N, X);
    }, ae = (N) => {
      var X = cp(), ue = i(X), Te = i(ue, !0);
      a(ue), a(X), $(() => h(Te, r().output)), v(N, X);
    };
    A(S, (N) => {
      r().gate_output ? N(U) : r().output && N(ae, 1);
    });
  }
  var we = o(S, 2), le = i(we), ve = i(le);
  a(le);
  var de = o(le, 2);
  {
    var Le = (N) => {
      var X = lp(), ue = i(X);
      a(X), $((Te) => h(ue, `Started: ${Te ?? ""}`), [() => new Date(r().started_at).toLocaleTimeString()]), v(N, X);
    };
    A(de, (N) => {
      r().started_at && N(Le);
    });
  }
  a(we);
  var Ae = o(we, 2);
  {
    var De = (N) => {
      var X = dp(), ue = i(X);
      Ws(ue);
      var Te = o(ue, 2), ne = i(Te);
      ne.__click = f;
      var W = o(ne, 2);
      W.__click = () => x(u, !1), a(Te), a(X), $(
        (K, Fe) => {
          ne.disabled = K, Ie(ne, 1, `px-3 py-1.5 rounded text-xs transition-colors
						${Fe ?? ""}`);
        },
        [
          () => !s(l).trim(),
          () => s(l).trim() ? "bg-health-err/20 text-health-err hover:bg-health-err/30" : "bg-surface-700 text-surface-500 cursor-not-allowed"
        ]
      ), xt(ue, () => s(l), (K) => x(l, K)), v(N, X);
    }, te = (N) => {
      var X = up(), ue = i(X);
      ue.__click = function(...ne) {
        n()?.apply(this, ne);
      }, ue.textContent = "✓ Pass Gate";
      var Te = o(ue, 2);
      Te.__click = () => x(u, !0), Te.textContent = "✕ Reject Gate", a(X), v(N, X);
    };
    A(Ae, (N) => {
      s(u) ? N(De) : N(te, !1);
    });
  }
  return a(m), $(
    (N, X) => {
      h(P, `Gate Review: ${s(d).label ?? ""}`), h(ve, `Tokens: ${N ?? ""} in / ${X ?? ""} out`);
    },
    [
      () => r().tokens_in.toLocaleString(),
      () => r().tokens_out.toLocaleString()
    ]
  ), v(e, m), $t(g);
}
Mt(["click"]);
Et(Ao, { session: {}, onPass: {}, onReject: {} }, [], [], { mode: "open" });
var fp = /* @__PURE__ */ p(`<button class="text-[10px] px-2 py-0.5 rounded text-surface-400
					hover:text-surface-200 hover:bg-surface-700 transition-colors">Archive</button>`), pp = /* @__PURE__ */ p('<div class="px-4 py-3 border-b border-surface-600 shrink-0"><h4 class="text-[10px] font-semibold text-surface-300 uppercase tracking-wider mb-2">Output</h4> <div class="rounded bg-surface-800 border border-surface-600 p-3 max-h-48 overflow-y-auto"><pre class="text-[10px] text-surface-200 whitespace-pre-wrap font-mono leading-relaxed"> </pre></div></div>'), xp = /* @__PURE__ */ p('<div class="px-4 py-3 border-b border-surface-600 shrink-0"><div class="text-[10px] text-health-err bg-health-err/10 rounded px-3 py-2"> </div></div>'), hp = /* @__PURE__ */ p('<div><div class="flex items-center gap-2 mb-1"><span> </span> <span class="text-[8px] text-surface-500"> </span></div> <pre class="text-[10px] text-surface-200 whitespace-pre-wrap font-mono leading-relaxed"> </pre></div>'), _p = /* @__PURE__ */ p('<div class="flex-1 overflow-y-auto px-4 py-3"><h4 class="text-[10px] font-semibold text-surface-300 uppercase tracking-wider mb-2"> </h4> <div class="space-y-2"></div></div>'), gp = /* @__PURE__ */ p('<div class="flex-1 flex items-center justify-center text-surface-500 text-xs">No conversation turns recorded</div>'), bp = /* @__PURE__ */ p('<div class="flex flex-col h-full"><div class="px-4 py-3 border-b border-surface-600 flex items-center gap-3 shrink-0"><button class="text-surface-400 hover:text-surface-100 transition-colors text-sm"></button> <span class="text-sm"> </span> <h3 class="text-sm font-semibold text-surface-100"> </h3> <span> </span> <div class="flex-1"></div> <!></div> <div class="px-4 py-3 border-b border-surface-600 shrink-0"><div class="grid grid-cols-4 gap-3 text-[10px]"><div><span class="text-surface-500 block">Started</span> <span class="text-surface-200"> </span></div> <div><span class="text-surface-500 block">Completed</span> <span class="text-surface-200"> </span></div> <div><span class="text-surface-500 block">Tokens In</span> <span class="text-surface-200"> </span></div> <div><span class="text-surface-500 block">Tokens Out</span> <span class="text-surface-200"> </span></div></div></div> <!> <!> <!></div>');
function Do(e, t) {
  kt(t, !0);
  let r = pt(t, "session", 7), n = pt(t, "turns", 7), c = pt(t, "onClose", 7), l = pt(t, "onArchive", 7);
  const u = /* @__PURE__ */ Ee(() => Hr[r().role]);
  function d(H) {
    return H ? new Date(H).toLocaleString(void 0, {
      month: "short",
      day: "numeric",
      hour: "2-digit",
      minute: "2-digit",
      second: "2-digit"
    }) : "-";
  }
  const f = {
    pending: "text-surface-400",
    running: "text-hecate-300",
    completed: "text-health-ok",
    failed: "text-health-err",
    gate_pending: "text-health-warn",
    gate_passed: "text-health-ok",
    gate_rejected: "text-health-err",
    archived: "text-surface-500"
  };
  var g = {
    get session() {
      return r();
    },
    set session(H) {
      r(H), vt();
    },
    get turns() {
      return n();
    },
    set turns(H) {
      n(H), vt();
    },
    get onClose() {
      return c();
    },
    set onClose(H) {
      c(H), vt();
    },
    get onArchive() {
      return l();
    },
    set onArchive(H) {
      l(H), vt();
    }
  }, m = bp(), k = i(m), y = i(k);
  y.__click = function(...H) {
    c()?.apply(this, H);
  }, y.textContent = "←";
  var P = o(y, 2), E = i(P, !0);
  a(P);
  var Q = o(P, 2), S = i(Q);
  a(Q);
  var U = o(Q, 2), ae = i(U, !0);
  a(U);
  var we = o(U, 4);
  {
    var le = (H) => {
      var M = fp();
      M.__click = function(...L) {
        l()?.apply(this, L);
      }, v(H, M);
    };
    A(we, (H) => {
      r().status !== "archived" && r().status !== "running" && H(le);
    });
  }
  a(k);
  var ve = o(k, 2), de = i(ve), Le = i(de), Ae = o(i(Le), 2), De = i(Ae, !0);
  a(Ae), a(Le);
  var te = o(Le, 2), N = o(i(te), 2), X = i(N, !0);
  a(N), a(te);
  var ue = o(te, 2), Te = o(i(ue), 2), ne = i(Te, !0);
  a(Te), a(ue);
  var W = o(ue, 2), K = o(i(W), 2), Fe = i(K, !0);
  a(K), a(W), a(de), a(ve);
  var et = o(ve, 2);
  {
    var st = (H) => {
      var M = pp(), L = o(i(M), 2), F = i(L), J = i(F, !0);
      a(F), a(L), a(M), $(() => h(J, r().output)), v(H, M);
    };
    A(et, (H) => {
      r().output && H(st);
    });
  }
  var Xe = o(et, 2);
  {
    var We = (H) => {
      var M = xp(), L = i(M), F = i(L, !0);
      a(L), a(M), $(() => h(F, r().error)), v(H, M);
    };
    A(Xe, (H) => {
      r().error && H(We);
    });
  }
  var Ge = o(Xe, 2);
  {
    var C = (H) => {
      var M = _p(), L = i(M), F = i(L);
      a(L);
      var J = o(L, 2);
      He(J, 21, n, ($e) => $e.turn_id, ($e, w) => {
        var D = hp(), z = i(D), pe = i(z), Ce = i(pe, !0);
        a(pe);
        var T = o(pe, 2), I = i(T, !0);
        a(T), a(z);
        var Z = o(z, 2), Me = i(Z, !0);
        a(Z), a(D), $(
          (Be) => {
            Ie(D, 1, `rounded p-2.5
						${s(w).role === "assistant" ? "bg-hecate-600/10 border border-hecate-600/20" : s(w).role === "user" ? "bg-surface-700/50 border border-surface-600" : "bg-surface-800 border border-surface-600/50"}`), Ie(pe, 1, `text-[9px] font-semibold uppercase tracking-wider
								${s(w).role === "assistant" ? "text-hecate-300" : "text-surface-400"}`), h(Ce, s(w).role), h(I, Be), h(Me, s(w).content);
          },
          [() => new Date(s(w).timestamp).toLocaleTimeString()]
        ), v($e, D);
      }), a(J), a(M), $(() => h(F, `Conversation (${n().length ?? ""} turns)`)), v(H, M);
    }, V = (H) => {
      var M = gp();
      v(H, M);
    };
    A(Ge, (H) => {
      n().length > 0 ? H(C) : H(V, !1);
    });
  }
  return a(m), $(
    (H, M, L, F) => {
      h(E, s(u).icon), h(S, `${s(u).label ?? ""} Session`), Ie(U, 1, `text-[10px] ${f[r().status] ?? "text-surface-400" ?? ""}`), h(ae, r().status), h(De, H), h(X, M), h(ne, L), h(Fe, F);
    },
    [
      () => d(r().started_at),
      () => d(r().completed_at),
      () => r().tokens_in.toLocaleString(),
      () => r().tokens_out.toLocaleString()
    ]
  ), v(e, m), $t(g);
}
Mt(["click"]);
Et(Do, { session: {}, turns: {}, onClose: {}, onArchive: {} }, [], [], { mode: "open" });
var mp = /* @__PURE__ */ p('<span class="text-[10px] text-surface-400 animate-pulse">Refreshing...</span>'), yp = /* @__PURE__ */ p('<div class="text-[11px] text-health-err bg-health-err/10 rounded px-3 py-2"> </div>'), wp = /* @__PURE__ */ p('<div class="space-y-3"></div>'), kp = /* @__PURE__ */ p('<div class="p-4 space-y-4 overflow-y-auto h-full"><div class="flex items-center justify-between"><div><h3 class="text-sm font-semibold text-surface-100">Agent Pipeline</h3> <p class="text-[11px] text-surface-400 mt-0.5">12 roles across the venture lifecycle</p></div> <!></div> <!> <!> <div><h4 class="text-[10px] font-semibold text-surface-400 uppercase tracking-wider mb-2">Tier 1 — Creative</h4> <div class="grid grid-cols-4 gap-2"></div></div> <div class="flex items-center gap-2 px-2"><div class="flex-1 h-px bg-surface-600"></div> <span class="text-[9px] text-surface-500">gate</span> <span class="text-surface-500"></span> <div class="flex-1 h-px bg-surface-600"></div></div> <div><h4 class="text-[10px] font-semibold text-surface-400 uppercase tracking-wider mb-2">Tier 2 — Mechanical</h4> <div class="grid grid-cols-4 gap-2"></div></div> <div><h4 class="text-[10px] font-semibold text-surface-400 uppercase tracking-wider mb-2">Always-On</h4> <div class="grid grid-cols-4 gap-2"></div></div></div>');
function Po(e, t) {
  kt(t, !0);
  const r = () => ke(St, "$activeVenture", y), n = () => ke($s, "$agentRoleStatuses", y), c = () => ke(Xs, "$selectedSession", y), l = () => ke(Ka, "$sessionTurns", y), u = () => ke(Ya, "$agentLoading", y), d = () => ke(Zt, "$agentError", y), f = () => ke(zs, "$pendingGates", y), g = () => ke(Fl, "$creativeRoles", y), m = () => ke(jl, "$mechanicalRoles", y), k = () => ke(Bl, "$alwaysOnRoles", y), [y, P] = Vt();
  let E = /* @__PURE__ */ oe(null);
  Tt(() => {
    const N = r();
    N && N.venture_id !== s(E) && (x(E, N.venture_id, !0), ms(N.venture_id));
  });
  let Q;
  Tt(() => {
    const N = r();
    if (N)
      return Q = setInterval(() => ms(N.venture_id), 1e4), () => {
        Q && clearInterval(Q);
      };
  });
  let S = /* @__PURE__ */ oe(!1);
  function U() {
    return r()?.venture_id ?? "";
  }
  async function ae(N) {
    const X = n().find((ue) => ue.role === N);
    X?.active_session && (await Gl(U(), X.active_session.session_id), (N === "coordinator" || N === "mentor") && await ql(U(), X.active_session.session_id), x(S, !0));
  }
  async function we(N) {
    await Hl(U(), N);
  }
  async function le(N, X) {
    await Wl(U(), X, N);
  }
  async function ve(N, X, ue) {
    await Ul(U(), X, N, ue);
  }
  async function de() {
    const N = c();
    N && (await zl(U(), N.session_id), x(S, !1), Xs.set(null));
  }
  var Le = ur(), Ae = ct(Le);
  {
    var De = (N) => {
      Do(N, {
        get session() {
          return c();
        },
        get turns() {
          return l();
        },
        onClose: () => {
          x(S, !1), Xs.set(null);
        },
        onArchive: de
      });
    }, te = (N) => {
      var X = kp(), ue = i(X), Te = o(i(ue), 2);
      {
        var ne = (L) => {
          var F = mp();
          v(L, F);
        };
        A(Te, (L) => {
          u() && L(ne);
        });
      }
      a(ue);
      var W = o(ue, 2);
      {
        var K = (L) => {
          var F = yp(), J = i(F, !0);
          a(F), $(() => h(J, d())), v(L, F);
        };
        A(W, (L) => {
          d() && L(K);
        });
      }
      var Fe = o(W, 2);
      {
        var et = (L) => {
          var F = wp();
          He(F, 5, f, (J) => J.session_id, (J, $e) => {
            Ao(J, {
              get session() {
                return s($e);
              },
              onPass: () => le(s($e).session_id, s($e).role),
              onReject: (w) => ve(s($e).session_id, s($e).role, w)
            });
          }), a(F), v(L, F);
        };
        A(Fe, (L) => {
          f().length > 0 && L(et);
        });
      }
      var st = o(Fe, 2), Xe = o(i(st), 2);
      He(Xe, 5, g, (L) => L.role, (L, F) => {
        ta(L, {
          get roleStatus() {
            return s(F);
          },
          onSelect: ae,
          onInitiate: we
        });
      }), a(Xe), a(st);
      var We = o(st, 2), Ge = o(i(We), 4);
      Ge.textContent = "↓", At(2), a(We);
      var C = o(We, 2), V = o(i(C), 2);
      He(V, 5, m, (L) => L.role, (L, F) => {
        ta(L, {
          get roleStatus() {
            return s(F);
          },
          onSelect: ae,
          onInitiate: we
        });
      }), a(V), a(C);
      var H = o(C, 2), M = o(i(H), 2);
      He(M, 5, k, (L) => L.role, (L, F) => {
        ta(L, {
          get roleStatus() {
            return s(F);
          },
          onSelect: ae,
          onInitiate: we
        });
      }), a(M), a(H), a(X), v(N, X);
    };
    A(Ae, (N) => {
      s(S) && c() ? N(De) : N(te, !1);
    });
  }
  v(e, Le), $t(), P();
}
Et(Po, {}, [], [], { mode: "open" });
var $p = /* @__PURE__ */ p("<div></div>"), Cp = /* @__PURE__ */ p("<!> <button><span> </span> <span> </span></button>", 1), Sp = /* @__PURE__ */ p('<span class="text-[10px] text-surface-500 mr-1">Pending</span>'), Ep = /* @__PURE__ */ p("<button> </button>"), Ap = /* @__PURE__ */ p(`<div class="border-b border-surface-600 bg-surface-800/30 px-4 py-2 shrink-0"><div class="flex items-center gap-1"><!> <div class="flex-1"></div> <span class="text-[10px] text-surface-400 mr-2"> </span> <!> <button class="text-[10px] px-2 py-0.5 rounded text-hecate-400
					hover:bg-hecate-600/20 transition-colors ml-1" title="Open AI Assistant"></button></div></div>`);
function To(e, t) {
  kt(t, !0);
  const r = () => ke(Ur, "$selectedDivision", l), n = () => ke(Bs, "$selectedPhase", l), c = () => ke(Rr, "$isLoading", l), [l, u] = Vt();
  let d = /* @__PURE__ */ Ee(() => r() ? qa(r(), n()) : []);
  function f(U) {
    Bs.set(U);
  }
  function g(U, ae) {
    switch (U) {
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
  function m(U, ae) {
    return U ? ae.length === 0 ? { icon: "✓", css: "text-health-ok" } : ae.includes("resume") ? { icon: "◐", css: "text-health-warn" } : ae.includes("shelve") || ae.includes("conclude") || ae.includes("archive") ? { icon: "●", css: "text-hecate-400 animate-pulse" } : ae.includes("open") ? { icon: "○", css: "text-surface-300" } : { icon: "○", css: "text-surface-500" } : { icon: "○", css: "text-surface-500" };
  }
  function k(U) {
    switch (U) {
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
  function y(U) {
    return U.charAt(0).toUpperCase() + U.slice(1);
  }
  async function P(U) {
    if (!r()) return;
    const ae = r().division_id, we = n();
    switch (U) {
      case "open":
        await xl(ae, we);
        break;
      case "shelve":
        await hl(ae, we);
        break;
      case "resume":
        await _l(ae, we);
        break;
      case "conclude":
        await gl(ae, we);
        break;
    }
  }
  var E = ur(), Q = ct(E);
  {
    var S = (U) => {
      var ae = Ap(), we = i(ae), le = i(we);
      He(le, 17, () => Zr, ft, (N, X, ue) => {
        const Te = /* @__PURE__ */ Ee(() => Ga(r(), s(X).code)), ne = /* @__PURE__ */ Ee(() => qa(r(), s(X).code)), W = /* @__PURE__ */ Ee(() => n() === s(X).code), K = /* @__PURE__ */ Ee(() => {
          const { icon: M, css: L } = m(s(Te), s(ne));
          return { icon: M, css: L };
        }), Fe = /* @__PURE__ */ Ee(() => s(Te) && s(ne).length === 0);
        var et = Cp(), st = ct(et);
        {
          var Xe = (M) => {
            var L = $p();
            $(() => Ie(L, 1, `w-4 h-px ${s(Fe) ? "bg-health-ok/40" : "bg-surface-600"}`)), v(M, L);
          };
          A(st, (M) => {
            ue > 0 && M(Xe);
          });
        }
        var We = o(st, 2);
        We.__click = () => f(s(X).code);
        var Ge = i(We), C = i(Ge, !0);
        a(Ge);
        var V = o(Ge, 2), H = i(V, !0);
        a(V), a(We), $(
          (M) => {
            Ie(We, 1, `flex items-center gap-1.5 px-3 py-1.5 rounded text-xs transition-all
						border
						${M ?? ""}`), Ie(Ge, 1, `${s(K).css ?? ""} text-[10px]`), h(C, s(K).icon), h(H, s(X).shortName);
          },
          [
            () => s(W) ? `bg-surface-700 border-current ${g(s(X).code)}` : "border-transparent text-surface-400 hover:text-surface-200 hover:bg-surface-700/50"
          ]
        ), v(N, et);
      });
      var ve = o(le, 4), de = i(ve, !0);
      a(ve);
      var Le = o(ve, 2);
      {
        var Ae = (N) => {
          const X = /* @__PURE__ */ Ee(() => Ga(r(), n()));
          var ue = ur(), Te = ct(ue);
          {
            var ne = (W) => {
              var K = Sp();
              v(W, K);
            };
            A(Te, (W) => {
              s(X) || W(ne);
            });
          }
          v(N, ue);
        }, De = (N) => {
          var X = ur(), ue = ct(X);
          He(ue, 17, () => s(d), ft, (Te, ne) => {
            var W = Ep();
            W.__click = () => P(s(ne));
            var K = i(W, !0);
            a(W), $(
              (Fe, et) => {
                W.disabled = c(), Ie(W, 1, `text-[10px] px-2 py-0.5 rounded transition-colors disabled:opacity-50
							${Fe ?? ""}`), h(K, et);
              },
              [
                () => k(s(ne)),
                () => y(s(ne))
              ]
            ), v(Te, W);
          }), v(N, X);
        };
        A(Le, (N) => {
          s(d).length === 0 ? N(Ae) : N(De, !1);
        });
      }
      var te = o(Le, 2);
      te.__click = () => Cr(`Help with ${Zr.find((N) => N.code === n())?.name} phase for division "${r()?.context_name}"`), te.textContent = "✦ AI Assist", a(we), a(ae), $((N) => h(de, N), [() => Zr.find((N) => N.code === n())?.name]), v(U, ae);
    };
    A(Q, (U) => {
      r() && U(S);
    });
  }
  v(e, E), $t(), u();
}
Mt(["click"]);
Et(To, {}, [], [], { mode: "open" });
var Dp = /* @__PURE__ */ p('<span class="text-[9px] text-surface-500"> </span>'), Pp = /* @__PURE__ */ p('<div class="flex items-center justify-center h-full"><div class="text-center text-surface-400"><div class="text-sm mb-2 animate-pulse">...</div> <div class="text-[10px]">Loading events</div></div></div>'), Tp = /* @__PURE__ */ p('<div class="flex items-center justify-center h-full"><div class="text-center text-surface-500 text-xs">Select a venture to view its event stream.</div></div>'), Rp = /* @__PURE__ */ p('<div class="flex items-center justify-center h-full"><div class="text-center text-surface-500"><div class="text-lg mb-2"></div> <div class="text-xs">No events recorded yet.</div> <div class="text-[10px] mt-1">Events will appear here as the venture progresses.</div></div></div>'), Mp = /* @__PURE__ */ p('<span class="text-[9px] px-1 py-0.5 rounded bg-surface-700 text-surface-400 shrink-0"> </span>'), Ip = /* @__PURE__ */ p('<span class="text-[9px] text-surface-500 shrink-0 tabular-nums"> </span>'), Np = /* @__PURE__ */ p(`<div class="px-4 pb-3 pt-0 ml-5"><pre class="text-[10px] text-surface-300 bg-surface-800 border border-surface-600
									rounded p-3 overflow-x-auto whitespace-pre-wrap break-words
									font-mono leading-relaxed"> </pre></div>`), Lp = /* @__PURE__ */ p(`<div class="group"><button class="w-full text-left px-4 py-2 flex items-start gap-2
								hover:bg-surface-700/30 transition-colors"><span class="text-[9px] text-surface-500 mt-0.5 shrink-0 w-3"> </span> <span> </span> <!> <!></button> <!></div>`), Op = /* @__PURE__ */ p('<div class="p-3 border-t border-surface-700/50"><button> </button></div>'), Fp = /* @__PURE__ */ p('<div class="divide-y divide-surface-700/50"></div> <!>', 1), jp = /* @__PURE__ */ p('<div class="flex flex-col h-full"><div class="border-b border-surface-600 bg-surface-800/50 px-4 py-2 shrink-0"><div class="flex items-center gap-2"><span class="text-xs text-surface-400">Event Stream</span> <!> <div class="flex-1"></div> <button title="Refresh events"> </button></div></div> <div class="flex-1 overflow-y-auto"><!></div></div>');
function Za(e, t) {
  kt(t, !0);
  const r = () => ke(Sn, "$ventureRawEvents", c), n = () => ke(St, "$activeVenture", c), [c, l] = Vt(), u = 50;
  let d = /* @__PURE__ */ oe(!1), f = /* @__PURE__ */ oe(0), g = /* @__PURE__ */ oe(0), m = /* @__PURE__ */ oe(Bt(/* @__PURE__ */ new Set())), k = /* @__PURE__ */ Ee(() => s(g) + u < s(f)), y = /* @__PURE__ */ Ee(r);
  async function P(W, K = !0) {
    x(d, !0), K && (x(g, 0), x(m, /* @__PURE__ */ new Set(), !0));
    try {
      const Fe = await zn(W, s(g), u);
      x(f, Fe.count, !0);
    } finally {
      x(d, !1);
    }
  }
  async function E() {
    const W = n();
    if (!(!W || s(d))) {
      x(g, s(g) + u), x(d, !0);
      try {
        const K = await zn(W.venture_id, s(g), u);
        x(f, K.count, !0);
      } finally {
        x(d, !1);
      }
    }
  }
  function Q(W) {
    const K = new Set(s(m));
    K.has(W) ? K.delete(W) : K.add(W), x(m, K, !0);
  }
  function S(W) {
    return W.startsWith("venture_") || W.startsWith("big_picture_storm_") ? "text-hecate-400" : W.startsWith("event_sticky_") ? "text-es-event" : W.startsWith("event_stack_") || W.startsWith("event_cluster_") ? "text-success-400" : W.startsWith("fact_arrow_") ? "text-sky-400" : W.startsWith("storm_phase_") ? "text-accent-400" : "text-surface-400";
  }
  function U(W) {
    if (!W) return "";
    const K = typeof W == "string" ? Number(W) || new Date(W).getTime() : W;
    if (isNaN(K)) return "";
    const Fe = new Date(K), st = Date.now() - K, Xe = Math.floor(st / 1e3);
    if (Xe < 60) return `${Xe}s ago`;
    const We = Math.floor(Xe / 60);
    if (We < 60) return `${We}m ago`;
    const Ge = Math.floor(We / 60);
    if (Ge < 24) return `${Ge}h ago`;
    const C = Math.floor(Ge / 24);
    return C < 7 ? `${C}d ago` : Fe.toLocaleDateString("en-US", {
      month: "short",
      day: "numeric",
      hour: "2-digit",
      minute: "2-digit"
    });
  }
  function ae(W) {
    try {
      return JSON.stringify(W, null, 2);
    } catch {
      return String(W);
    }
  }
  Tt(() => {
    const W = n();
    W && P(W.venture_id);
  });
  var we = jp(), le = i(we), ve = i(le), de = o(i(ve), 2);
  {
    var Le = (W) => {
      var K = Dp(), Fe = i(K);
      a(K), $(() => h(Fe, `${s(y).length ?? ""}${s(f) > s(y).length ? ` / ${s(f)}` : ""} events`)), v(W, K);
    };
    A(de, (W) => {
      s(y).length > 0 && W(Le);
    });
  }
  var Ae = o(de, 4);
  Ae.__click = () => {
    const W = n();
    W && P(W.venture_id);
  };
  var De = i(Ae, !0);
  a(Ae), a(ve), a(le);
  var te = o(le, 2), N = i(te);
  {
    var X = (W) => {
      var K = Pp();
      v(W, K);
    }, ue = (W) => {
      var K = Tp();
      v(W, K);
    }, Te = (W) => {
      var K = Rp(), Fe = i(K), et = i(Fe);
      et.textContent = "○", At(4), a(Fe), a(K), v(W, K);
    }, ne = (W) => {
      var K = Fp(), Fe = ct(K);
      He(Fe, 21, () => s(y), ft, (Xe, We, Ge) => {
        const C = /* @__PURE__ */ Ee(() => s(m).has(Ge)), V = /* @__PURE__ */ Ee(() => S(s(We).event_type));
        var H = Lp(), M = i(H);
        M.__click = () => Q(Ge);
        var L = i(M), F = i(L, !0);
        a(L);
        var J = o(L, 2), $e = i(J, !0);
        a(J);
        var w = o(J, 2);
        {
          var D = (I) => {
            var Z = Mp(), Me = i(Z);
            a(Z), $(() => h(Me, `v${s(We).version ?? ""}`)), v(I, Z);
          };
          A(w, (I) => {
            s(We).version !== void 0 && I(D);
          });
        }
        var z = o(w, 2);
        {
          var pe = (I) => {
            var Z = Ip(), Me = i(Z, !0);
            a(Z), $((Be) => h(Me, Be), [() => U(s(We).timestamp)]), v(I, Z);
          };
          A(z, (I) => {
            s(We).timestamp && I(pe);
          });
        }
        a(M);
        var Ce = o(M, 2);
        {
          var T = (I) => {
            var Z = Np(), Me = i(Z), Be = i(Me, !0);
            a(Me), a(Z), $((Je) => h(Be, Je), [() => ae(s(We).data)]), v(I, Z);
          };
          A(Ce, (I) => {
            s(C) && I(T);
          });
        }
        a(H), $(() => {
          h(F, s(C) ? "▾" : "▸"), Ie(J, 1, `text-[11px] font-mono ${s(V) ?? ""} flex-1 min-w-0 truncate`), h($e, s(We).event_type);
        }), v(Xe, H);
      }), a(Fe);
      var et = o(Fe, 2);
      {
        var st = (Xe) => {
          var We = Op(), Ge = i(We);
          Ge.__click = E;
          var C = i(Ge, !0);
          a(Ge), a(We), $(() => {
            Ge.disabled = s(d), Ie(Ge, 1, `w-full text-[10px] py-1.5 rounded transition-colors
							${s(d) ? "bg-surface-700 text-surface-500 cursor-not-allowed" : "bg-surface-700 text-surface-300 hover:text-surface-100 hover:bg-surface-600"}`), h(C, s(d) ? "Loading..." : `Load More (${s(f) - s(y).length} remaining)`);
          }), v(Xe, We);
        };
        A(et, (Xe) => {
          s(k) && Xe(st);
        });
      }
      v(W, K);
    };
    A(N, (W) => {
      s(d) && s(y).length === 0 ? W(X) : n() ? s(y).length === 0 ? W(Te, 2) : W(ne, !1) : W(ue, 1);
    });
  }
  a(te), a(we), $(() => {
    Ae.disabled = s(d) || !n(), Ie(Ae, 1, `text-[10px] px-2 py-0.5 rounded transition-colors
					${s(d) || !n() ? "text-surface-500 cursor-not-allowed" : "text-surface-400 hover:text-surface-200 hover:bg-surface-700"}`), h(De, s(d) ? "Loading..." : "Refresh");
  }), v(e, we), $t(), l();
}
Mt(["click"]);
Et(Za, {}, [], [], { mode: "open" });
var Bp = /* @__PURE__ */ p(`<div class="flex justify-end"><div class="max-w-[85%] rounded-lg px-3 py-2 text-[11px]
						bg-hecate-600/20 text-surface-100 border border-hecate-600/20"> </div></div>`), Vp = /* @__PURE__ */ p('<div class="flex justify-start"><div><div class="whitespace-pre-wrap break-words"> </div></div></div>'), Gp = /* @__PURE__ */ p('<div class="whitespace-pre-wrap break-words"> <span class="inline-block w-1.5 h-3 bg-hecate-400 animate-pulse ml-0.5"></span></div>'), qp = /* @__PURE__ */ p('<div class="flex items-center gap-1.5 text-surface-400"><span class="animate-bounce" style="animation-delay: 0ms">.</span> <span class="animate-bounce" style="animation-delay: 150ms">.</span> <span class="animate-bounce" style="animation-delay: 300ms">.</span></div>'), Hp = /* @__PURE__ */ p(`<div class="flex justify-start"><div class="max-w-[85%] rounded-lg px-3 py-2 text-[11px]
					bg-surface-700 text-surface-200 border border-surface-600"><!></div></div>`), Wp = /* @__PURE__ */ p('<span class="text-[9px] text-hecate-400 ml-1">(code-optimized)</span>'), Up = /* @__PURE__ */ p('<span class="text-hecate-400"> </span> <!>', 1), zp = /* @__PURE__ */ p('<span class="text-health-warn">No model available</span>'), Yp = /* @__PURE__ */ p('<div class="flex items-center justify-center h-full"><div class="text-center text-surface-400"><div class="text-xl mb-2"></div> <div class="text-[11px]">AI Assistant ready <br/> <!></div></div></div>'), Kp = /* @__PURE__ */ p(`<div class="w-[380px] border-l border-surface-600 bg-surface-800 flex flex-col shrink-0 overflow-hidden"><div class="flex items-center gap-2 px-3 py-2 border-b border-surface-600 shrink-0"><span class="text-hecate-400"></span> <span class="text-xs font-semibold text-surface-100">AI</span> <!> <div class="flex-1"></div> <span class="text-[9px] text-surface-400"> </span> <button class="text-surface-400 hover:text-surface-100 transition-colors px-1" title="Close AI Assistant"></button></div> <div class="flex-1 overflow-y-auto p-3 space-y-3"><!> <!> <!></div> <div class="border-t border-surface-600 p-2 shrink-0"><div class="flex gap-1.5"><textarea class="flex-1 bg-surface-700 border border-surface-600 rounded px-2.5 py-1.5
					text-[11px] text-surface-100 placeholder-surface-400 resize-none
					focus:outline-none focus:border-hecate-500
					disabled:opacity-50 disabled:cursor-not-allowed"></textarea> <button>Send</button></div></div></div>`);
function en(e, t) {
  kt(t, !0);
  const r = () => ke(Bs, "$selectedPhase", f), n = () => ke(io, "$phaseModelPrefs", f), c = () => ke(wn, "$aiModel", f), l = () => ke(so, "$aiAssistContext", f), u = () => ke(St, "$activeVenture", f), d = () => ke(Ur, "$selectedDivision", f), [f, g] = Vt(), m = ro();
  let k = /* @__PURE__ */ oe(Bt([])), y = /* @__PURE__ */ oe(""), P = /* @__PURE__ */ oe(!1), E = /* @__PURE__ */ oe(""), Q = /* @__PURE__ */ oe(void 0), S = /* @__PURE__ */ oe(null), U = /* @__PURE__ */ oe(null), ae = /* @__PURE__ */ Ee(() => ul(r())), we = /* @__PURE__ */ Ee(() => n()[r()]);
  Tt(() => {
    const M = c();
    s(U) !== null && s(U) !== M && (s(S) && (s(S).cancel(), x(S, null)), x(k, [], !0), x(E, ""), x(P, !1)), x(U, M, !0);
  }), Tt(() => {
    const M = l();
    M && s(k).length === 0 && ve(M);
  });
  function le() {
    const M = [], L = Qt(yo);
    L && M.push(L);
    const F = Zr.find((J) => J.code === r());
    if (F && M.push(`You are currently assisting with the ${F.name} phase. ${F.description}.`), u()) {
      let J = `Venture: "${u().name}"`;
      u().brief && (J += ` — ${u().brief}`), M.push(J);
    }
    return d() && M.push(`Division: "${d().context_name}" (bounded context)`), M.push(Qt(Bd)), M.join(`

---

`);
  }
  async function ve(M) {
    const L = c();
    if (!L || !M.trim() || s(P)) return;
    const F = { role: "user", content: M.trim() };
    x(k, [...s(k), F], !0), x(y, "");
    const J = [], $e = le();
    $e && J.push({ role: "system", content: $e }), J.push(...s(k)), x(P, !0), x(E, "");
    let w = "";
    const D = m.stream.chat(L, J);
    x(S, D, !0), D.onChunk((z) => {
      z.content && (w += z.content, x(E, w, !0));
    }).onDone(async (z) => {
      x(S, null), z.content && (w += z.content);
      const pe = {
        role: "assistant",
        content: w || "(empty response)"
      };
      if (x(k, [...s(k), pe], !0), x(E, ""), x(P, !1), Qt(mn) === "oracle" && w) {
        const T = Qt(St)?.venture_id;
        if (T) {
          const I = fl(w);
          for (const Z of I)
            await Qs(T, Z, "oracle");
        }
      }
    }).onError((z) => {
      x(S, null);
      const pe = { role: "assistant", content: `Error: ${z}` };
      x(k, [...s(k), pe], !0), x(E, ""), x(P, !1);
    });
    try {
      await D.start();
    } catch (z) {
      const pe = { role: "assistant", content: `Error: ${String(z)}` };
      x(k, [...s(k), pe], !0), x(P, !1);
    }
  }
  let de = /* @__PURE__ */ oe(void 0);
  function Le(M) {
    M.key === "Enter" && !M.shiftKey && (M.preventDefault(), ve(s(y)), s(de) && (s(de).style.height = "auto"));
  }
  function Ae(M) {
    const L = M.target;
    L.style.height = "auto", L.style.height = Math.min(L.scrollHeight, 120) + "px";
  }
  function De() {
    vl(), x(k, [], !0), x(E, "");
  }
  Tt(() => {
    s(k), s(E), _n().then(() => {
      s(Q) && (s(Q).scrollTop = s(Q).scrollHeight);
    });
  });
  var te = Kp(), N = i(te), X = i(N);
  X.textContent = "✦";
  var ue = o(X, 4);
  {
    let M = /* @__PURE__ */ Ee(() => Zr.find((L) => L.code === r())?.shortName ?? "");
    wa(ue, {
      get currentModel() {
        return c();
      },
      onSelect: (L) => kn(L),
      showPhaseInfo: !0,
      get phasePreference() {
        return s(we);
      },
      get phaseAffinity() {
        return s(ae);
      },
      onPinModel: (L) => Un(r(), L),
      onClearPin: () => Un(r(), null),
      get phaseName() {
        return s(M);
      }
    });
  }
  var Te = o(ue, 4), ne = i(Te, !0);
  a(Te);
  var W = o(Te, 2);
  W.__click = De, W.textContent = "✕", a(N);
  var K = o(N, 2), Fe = i(K);
  He(Fe, 17, () => s(k), ft, (M, L) => {
    var F = ur(), J = ct(F);
    {
      var $e = (D) => {
        var z = Bp(), pe = i(z), Ce = i(pe, !0);
        a(pe), a(z), $(() => h(Ce, s(L).content)), v(D, z);
      }, w = (D) => {
        var z = Vp(), pe = i(z), Ce = i(pe), T = i(Ce, !0);
        a(Ce), a(pe), a(z), $(
          (I) => {
            Ie(pe, 1, `max-w-[85%] rounded-lg px-3 py-2 text-[11px]
						bg-surface-700 text-surface-200 border border-surface-600
						${I ?? ""}`), h(T, s(L).content);
          },
          [
            () => s(L).content.startsWith("Error:") ? "border-health-err/30 text-health-err" : ""
          ]
        ), v(D, z);
      };
      A(J, (D) => {
        s(L).role === "user" ? D($e) : s(L).role === "assistant" && D(w, 1);
      });
    }
    v(M, F);
  });
  var et = o(Fe, 2);
  {
    var st = (M) => {
      var L = Hp(), F = i(L), J = i(F);
      {
        var $e = (D) => {
          var z = Gp(), pe = i(z, !0);
          At(), a(z), $(() => h(pe, s(E))), v(D, z);
        }, w = (D) => {
          var z = qp();
          v(D, z);
        };
        A(J, (D) => {
          s(E) ? D($e) : D(w, !1);
        });
      }
      a(F), a(L), v(M, L);
    };
    A(et, (M) => {
      s(P) && M(st);
    });
  }
  var Xe = o(et, 2);
  {
    var We = (M) => {
      var L = Yp(), F = i(L), J = i(F);
      J.textContent = "✦";
      var $e = o(J, 2), w = o(i($e), 3);
      {
        var D = (pe) => {
          var Ce = Up(), T = ct(Ce), I = i(T, !0);
          a(T);
          var Z = o(T, 2);
          {
            var Me = (Be) => {
              var Je = Wp();
              v(Be, Je);
            };
            A(Z, (Be) => {
              s(ae) === "code" && Be(Me);
            });
          }
          $(() => h(I, c())), v(pe, Ce);
        }, z = (pe) => {
          var Ce = zp();
          v(pe, Ce);
        };
        A(w, (pe) => {
          c() ? pe(D) : pe(z, !1);
        });
      }
      a($e), a(F), a(L), v(M, L);
    };
    A(Xe, (M) => {
      s(k).length === 0 && !s(P) && M(We);
    });
  }
  a(K), Xr(K, (M) => x(Q, M), () => s(Q));
  var Ge = o(K, 2), C = i(Ge), V = i(C);
  Ws(V), V.__keydown = Le, V.__input = Ae, Ot(V, "rows", 1), Xr(V, (M) => x(de, M), () => s(de));
  var H = o(V, 2);
  H.__click = () => ve(s(y)), a(C), a(Ge), a(te), $(
    (M, L, F) => {
      h(ne, M), Ot(V, "placeholder", s(P) ? "Waiting..." : "Ask about this phase..."), V.disabled = s(P) || !c(), H.disabled = L, Ie(H, 1, `px-2.5 rounded text-[11px] transition-colors self-end
					${F ?? ""}`);
    },
    [
      () => Zr.find((M) => M.code === r())?.shortName ?? "",
      () => s(P) || !s(y).trim() || !c(),
      () => s(P) || !s(y).trim() || !c() ? "bg-surface-600 text-surface-400 cursor-not-allowed" : "bg-hecate-600 text-surface-50 hover:bg-hecate-500"
    ]
  ), xt(V, () => s(y), (M) => x(y, M)), v(e, te), $t(), g();
}
Mt(["click", "keydown", "input"]);
Et(en, {}, [], [], { mode: "open" });
var Jp = /* @__PURE__ */ p("<button> </button>"), Qp = /* @__PURE__ */ p('<div><button><span class="font-medium truncate block"> </span></button> <div class="flex items-center gap-1 ml-2 mt-0.5"></div></div>'), Xp = /* @__PURE__ */ p('<div class="text-[10px] text-surface-500 px-2 py-3 text-center">No divisions yet</div>'), Zp = /* @__PURE__ */ p('<div class="px-2 pb-2 space-y-1"><!> <!></div>'), e0 = /* @__PURE__ */ p('<span class="ml-auto text-[9px] px-1.5 py-0.5 rounded-full bg-hecate-600/20 text-hecate-300"> </span>'), t0 = /* @__PURE__ */ p('<span class="ml-auto text-[9px] text-surface-500 truncate max-w-[60px]"> </span>'), r0 = /* @__PURE__ */ p(`<button class="w-full flex items-center gap-1.5 px-2 py-1 rounded text-[10px]
							text-surface-300 hover:bg-surface-700/50 transition-colors"><span> </span> <span class="truncate"> </span> <!></button>`), s0 = /* @__PURE__ */ p('<div class="px-2 pb-2 space-y-0.5"></div>'), a0 = /* @__PURE__ */ p('<span class="ml-auto text-[9px] px-1.5 py-0.5 rounded-full bg-amber-500/20 text-amber-300 animate-pulse"> </span>'), n0 = /* @__PURE__ */ p('<div class="text-[9px] text-surface-400 ml-3 mt-0.5 truncate"> </div>'), i0 = /* @__PURE__ */ p(`<button class="w-full text-left p-2 rounded bg-amber-500/5 border border-amber-500/20
							hover:bg-amber-500/10 transition-colors"><div class="flex items-center gap-1.5"><span class="text-[8px] text-amber-400 animate-pulse"></span> <span class="text-[10px] font-medium text-amber-300 truncate"> </span></div> <!></button>`), o0 = /* @__PURE__ */ p('<div class="text-[10px] text-surface-500 px-2 py-3 text-center">No pending gates</div>'), c0 = /* @__PURE__ */ p('<div class="px-2 pb-2 space-y-1"><!> <!></div>'), l0 = /* @__PURE__ */ p(`<div class="w-52 border-r border-surface-600 bg-surface-800/30 overflow-y-auto shrink-0 flex flex-col"><div class="border-b border-surface-700/50"><button class="w-full flex items-center gap-1.5 px-3 py-2 text-[10px] text-surface-400
				uppercase tracking-wider hover:text-surface-300 transition-colors"><span class="text-[8px]"> </span> <span>Divisions</span> <span class="ml-auto text-surface-500"> </span></button> <!></div> <div class="border-b border-surface-700/50"><button class="w-full flex items-center gap-1.5 px-3 py-2 text-[10px] text-surface-400
				uppercase tracking-wider hover:text-surface-300 transition-colors"><span class="text-[8px]"> </span> <span>Agents</span> <!></button> <!></div> <div class="flex-1"><button class="w-full flex items-center gap-1.5 px-3 py-2 text-[10px] text-surface-400
				uppercase tracking-wider hover:text-surface-300 transition-colors"><span class="text-[8px]"> </span> <span>Gates</span> <!></button> <!></div></div>`);
function Ro(e, t) {
  kt(t, !0);
  const r = () => ke(ss, "$divisions", u), n = () => ke(Ns, "$selectedDivisionId", u), c = () => ke($s, "$agentRoleStatuses", u), l = () => ke(zs, "$pendingGates", u), [u, d] = Vt();
  let f = pt(t, "onSelectAgent", 7), g = pt(t, "onSelectGate", 7), m = /* @__PURE__ */ oe(Bt({}));
  function k(F) {
    x(
      m,
      {
        ...s(m),
        [F]: !s(m)[F]
      },
      !0
    );
  }
  function y(F) {
    Ns.set(F);
  }
  function P(F, J) {
    Ns.set(F), Bs.set(J);
  }
  function E(F, J) {
    return F ? J.length === 0 ? { icon: "●", css: "text-health-ok" } : J.includes("resume") ? { icon: "○", css: "text-health-warn" } : J.includes("shelve") || J.includes("conclude") || J.includes("archive") ? { icon: "◐", css: "text-hecate-400" } : J.includes("open") ? { icon: "○", css: "text-surface-300" } : { icon: "○", css: "text-surface-500" } : { icon: "○", css: "text-surface-500" };
  }
  function Q(F) {
    switch (F) {
      case "active":
        return { icon: "●", css: "text-hecate-400 animate-pulse" };
      case "gate_pending":
        return { icon: "●", css: "text-amber-400 animate-pulse" };
      case "completed":
        return { icon: "✓", css: "text-health-ok" };
      case "failed":
        return { icon: "✗", css: "text-health-err" };
      default:
        return { icon: "○", css: "text-surface-500" };
    }
  }
  function S(F) {
    return F.replace(/_/g, " ").replace(/\b\w/g, (J) => J.toUpperCase());
  }
  var U = {
    get onSelectAgent() {
      return f();
    },
    set onSelectAgent(F) {
      f(F), vt();
    },
    get onSelectGate() {
      return g();
    },
    set onSelectGate(F) {
      g(F), vt();
    }
  }, ae = l0(), we = i(ae), le = i(we);
  le.__click = () => k("divisions");
  var ve = i(le), de = i(ve, !0);
  a(ve);
  var Le = o(ve, 4), Ae = i(Le, !0);
  a(Le), a(le);
  var De = o(le, 2);
  {
    var te = (F) => {
      var J = Zp(), $e = i(J);
      He($e, 1, r, ft, (z, pe) => {
        const Ce = /* @__PURE__ */ Ee(() => n() === s(pe).division_id);
        var T = Qp(), I = i(T);
        I.__click = () => y(s(pe).division_id);
        var Z = i(I), Me = i(Z, !0);
        a(Z), a(I);
        var Be = o(I, 2);
        He(Be, 21, () => Zr, ft, (Je, ze) => {
          const tt = /* @__PURE__ */ Ee(() => Ga(s(pe), s(ze).code)), qe = /* @__PURE__ */ Ee(() => qa(s(pe), s(ze).code)), Ue = /* @__PURE__ */ Ee(() => {
            const { icon: R, css: G } = E(s(tt), s(qe));
            return { icon: R, css: G };
          });
          var Ye = Jp();
          Ye.__click = () => P(s(pe).division_id, s(ze).code);
          var je = i(Ye, !0);
          a(Ye), $(() => {
            Ie(Ye, 1, `text-[9px] ${s(Ue).css ?? ""} hover:opacity-80 transition-opacity`), Ot(Ye, "title", `${s(ze).shortName ?? ""}: ${(s(tt) || "Pending") ?? ""}`), h(je, s(Ue).icon);
          }), v(Je, Ye);
        }), a(Be), a(T), $(() => {
          Ie(I, 1, `w-full text-left px-2 py-1 rounded text-xs transition-colors
								${s(Ce) ? "bg-surface-700 text-surface-100" : "text-surface-300 hover:bg-surface-700/50 hover:text-surface-100"}`), h(Me, s(pe).context_name);
        }), v(z, T);
      });
      var w = o($e, 2);
      {
        var D = (z) => {
          var pe = Xp();
          v(z, pe);
        };
        A(w, (z) => {
          r().length === 0 && z(D);
        });
      }
      a(J), v(F, J);
    };
    A(De, (F) => {
      s(m).divisions || F(te);
    });
  }
  a(we);
  var N = o(we, 2), X = i(N);
  X.__click = () => k("agents");
  var ue = i(X), Te = i(ue, !0);
  a(ue);
  var ne = o(ue, 4);
  {
    var W = (F) => {
      var J = e0(), $e = i(J);
      a(J), $((w) => h($e, `${w ?? ""} active`), [
        () => c().filter((w) => w.status === "active").length
      ]), v(F, J);
    }, K = /* @__PURE__ */ Ee(() => c().filter((F) => F.status === "active").length > 0);
    A(ne, (F) => {
      s(K) && F(W);
    });
  }
  a(X);
  var Fe = o(X, 2);
  {
    var et = (F) => {
      var J = s0();
      He(J, 5, c, ft, ($e, w) => {
        const D = /* @__PURE__ */ Ee(() => {
          const { icon: Be, css: Je } = Q(s(w).status);
          return { icon: Be, css: Je };
        });
        var z = r0();
        z.__click = () => f()?.(s(w).role);
        var pe = i(z), Ce = i(pe, !0);
        a(pe);
        var T = o(pe, 2), I = i(T, !0);
        a(T);
        var Z = o(T, 2);
        {
          var Me = (Be) => {
            var Je = t0(), ze = i(Je, !0);
            a(Je), $(() => h(ze, s(w).active_session.division_name)), v(Be, Je);
          };
          A(Z, (Be) => {
            s(w).active_session?.division_id && Be(Me);
          });
        }
        a(z), $(
          (Be) => {
            Ie(pe, 1, `${s(D).css ?? ""} text-[8px]`), h(Ce, s(D).icon), h(I, Be);
          },
          [() => S(s(w).role)]
        ), v($e, z);
      }), a(J), v(F, J);
    };
    A(Fe, (F) => {
      s(m).agents || F(et);
    });
  }
  a(N);
  var st = o(N, 2), Xe = i(st);
  Xe.__click = () => k("gates");
  var We = i(Xe), Ge = i(We, !0);
  a(We);
  var C = o(We, 4);
  {
    var V = (F) => {
      var J = a0(), $e = i(J, !0);
      a(J), $(() => h($e, l().length)), v(F, J);
    };
    A(C, (F) => {
      l().length > 0 && F(V);
    });
  }
  a(Xe);
  var H = o(Xe, 2);
  {
    var M = (F) => {
      var J = c0(), $e = i(J);
      He($e, 1, l, ft, (z, pe) => {
        var Ce = i0();
        Ce.__click = () => g()?.(s(pe).session_id);
        var T = i(Ce), I = i(T);
        I.textContent = "●";
        var Z = o(I, 2), Me = i(Z, !0);
        a(Z), a(T);
        var Be = o(T, 2);
        {
          var Je = (ze) => {
            var tt = n0(), qe = i(tt, !0);
            a(tt), $(() => h(qe, s(pe).division_id)), v(ze, tt);
          };
          A(Be, (ze) => {
            s(pe).division_id && ze(Je);
          });
        }
        a(Ce), $((ze) => h(Me, ze), [() => S(s(pe).role)]), v(z, Ce);
      });
      var w = o($e, 2);
      {
        var D = (z) => {
          var pe = o0();
          v(z, pe);
        };
        A(w, (z) => {
          l().length === 0 && z(D);
        });
      }
      a(J), v(F, J);
    };
    A(H, (F) => {
      s(m).gates || F(M);
    });
  }
  a(st), a(ae), $(() => {
    h(de, s(m).divisions ? "▶" : "▼"), h(Ae, r().length), h(Te, s(m).agents ? "▶" : "▼"), h(Ge, s(m).gates ? "▶" : "▼");
  }), v(e, ae);
  var L = $t(U);
  return d(), L;
}
Mt(["click"]);
Et(Ro, { onSelectAgent: {}, onSelectGate: {} }, [], [], { mode: "open" });
var d0 = /* @__PURE__ */ p('<span class="text-surface-500 truncate"> </span>'), u0 = /* @__PURE__ */ p('<span> </span> <!> <span class="text-surface-500 shrink-0"> </span>', 1), v0 = /* @__PURE__ */ p('<span class="text-surface-500">No recent activity</span>'), f0 = /* @__PURE__ */ p('<span class="px-1.5 py-0.5 rounded-full bg-hecate-600/30 text-hecate-300 text-[9px]"> </span>'), p0 = /* @__PURE__ */ p('<span class="text-[9px] text-surface-500 ml-1"> </span>'), x0 = /* @__PURE__ */ p(`<div class="flex items-start gap-2 px-4 py-1.5 hover:bg-surface-700/20
					transition-colors border-b border-surface-700/30 last:border-b-0"><span></span> <div class="min-w-0 flex-1"><span> </span> <!></div> <span class="text-[9px] text-surface-500 shrink-0 tabular-nums"> </span></div>`), h0 = /* @__PURE__ */ p('<div class="text-center py-4 text-[10px] text-surface-500">Activity will appear here as events stream in</div>'), _0 = /* @__PURE__ */ p('<div class="max-h-48 overflow-y-auto border-t border-surface-700/50"><!> <!></div>'), g0 = /* @__PURE__ */ p(`<div class="border-t border-surface-600 bg-surface-800/50 shrink-0"><button class="w-full flex items-center gap-2 px-4 py-1.5 text-[10px]
			hover:bg-surface-700/30 transition-colors"><span></span> <!> <span class="flex-1"></span> <!> <span class="text-surface-500 text-[8px]"> </span></button> <!></div>`);
function Mo(e, t) {
  kt(t, !0);
  const r = () => ke(br, "$sseStatus", l), n = () => ke(sd, "$recentActivity", l), c = () => ke(En, "$unreadCount", l), [l, u] = Vt();
  let d = /* @__PURE__ */ oe(!1);
  function f(Ae) {
    switch (Ae) {
      case "success":
        return "text-health-ok";
      case "warning":
        return "text-amber-400";
      case "error":
        return "text-health-err";
      default:
        return "text-surface-400";
    }
  }
  function g(Ae) {
    switch (Ae) {
      case "success":
        return "bg-health-ok";
      case "warning":
        return "bg-amber-400";
      case "error":
        return "bg-health-err";
      default:
        return "bg-surface-500";
    }
  }
  function m(Ae) {
    const De = Math.floor((Date.now() - Ae) / 1e3);
    if (De < 5) return "now";
    if (De < 60) return `${De}s ago`;
    const te = Math.floor(De / 60);
    return te < 60 ? `${te}m ago` : `${Math.floor(te / 60)}h ago`;
  }
  function k() {
    x(d, !s(d)), s(d) && id();
  }
  var y = g0(), P = i(y);
  P.__click = k;
  var E = i(P), Q = o(E, 2);
  {
    var S = (Ae) => {
      const De = /* @__PURE__ */ Ee(() => n()[0]);
      var te = u0(), N = ct(te), X = i(N, !0);
      a(N);
      var ue = o(N, 2);
      {
        var Te = (K) => {
          var Fe = d0(), et = i(Fe, !0);
          a(Fe), $(() => h(et, s(De).detail)), v(K, Fe);
        };
        A(ue, (K) => {
          s(De).detail && K(Te);
        });
      }
      var ne = o(ue, 2), W = i(ne, !0);
      a(ne), $(
        (K, Fe) => {
          Ie(N, 1, `${K ?? ""} truncate`), h(X, s(De).summary), h(W, Fe);
        },
        [
          () => f(s(De).severity),
          () => m(s(De).timestamp)
        ]
      ), v(Ae, te);
    }, U = (Ae) => {
      var De = v0();
      v(Ae, De);
    };
    A(Q, (Ae) => {
      n().length > 0 ? Ae(S) : Ae(U, !1);
    });
  }
  var ae = o(Q, 4);
  {
    var we = (Ae) => {
      var De = f0(), te = i(De, !0);
      a(De), $(() => h(te, c())), v(Ae, De);
    };
    A(ae, (Ae) => {
      c() > 0 && Ae(we);
    });
  }
  var le = o(ae, 2), ve = i(le, !0);
  a(le), a(P);
  var de = o(P, 2);
  {
    var Le = (Ae) => {
      var De = _0(), te = i(De);
      He(te, 1, n, (ue) => ue.id, (ue, Te) => {
        var ne = x0(), W = i(ne), K = o(W, 2), Fe = i(K), et = i(Fe, !0);
        a(Fe);
        var st = o(Fe, 2);
        {
          var Xe = (C) => {
            var V = p0(), H = i(V, !0);
            a(V), $(() => h(H, s(Te).detail)), v(C, V);
          };
          A(st, (C) => {
            s(Te).detail && C(Xe);
          });
        }
        a(K);
        var We = o(K, 2), Ge = i(We, !0);
        a(We), a(ne), $(
          (C, V, H) => {
            Ie(W, 1, `inline-block w-1.5 h-1.5 rounded-full mt-1 shrink-0
						${C ?? ""}`), Ie(Fe, 1, `text-[10px] ${V ?? ""}`), h(et, s(Te).summary), h(Ge, H);
          },
          [
            () => g(s(Te).severity),
            () => f(s(Te).severity),
            () => m(s(Te).timestamp)
          ]
        ), v(ue, ne);
      });
      var N = o(te, 2);
      {
        var X = (ue) => {
          var Te = h0();
          v(ue, Te);
        };
        A(N, (ue) => {
          n().length === 0 && ue(X);
        });
      }
      a(De), v(Ae, De);
    };
    A(de, (Ae) => {
      s(d) && Ae(Le);
    });
  }
  a(y), $(() => {
    Ie(E, 1, `inline-block w-1.5 h-1.5 rounded-full shrink-0
				${r() === "connected" ? "bg-health-ok" : r() === "connecting" ? "bg-amber-400 animate-pulse" : "bg-surface-500"}`), Ot(E, "title", `SSE: ${r() ?? ""}`), h(ve, s(d) ? "▼" : "▲");
  }), v(e, y), $t(), u();
}
Mt(["click"]);
Et(Mo, {}, [], [], { mode: "open" });
var b0 = /* @__PURE__ */ p('<div class="flex items-center justify-center h-full"><div class="text-center text-surface-400"><div class="text-2xl mb-2 animate-pulse"></div> <div class="text-sm">Loading venture...</div></div></div>'), m0 = /* @__PURE__ */ p('<div class="text-[11px] text-health-err bg-health-err/10 rounded px-3 py-2"> </div>'), y0 = /* @__PURE__ */ p(`<div class="rounded-xl border border-hecate-600/30 bg-surface-800/80 p-5 space-y-4"><h3 class="text-xs font-medium text-hecate-300 uppercase tracking-wider">New Venture</h3> <div class="grid grid-cols-[1fr_2fr] gap-4"><div><label for="venture-name" class="text-[11px] text-surface-300 block mb-1.5">Name</label> <input id="venture-name" placeholder="e.g., my-saas-app" class="w-full bg-surface-700 border border-surface-600 rounded-lg
										px-3 py-2 text-sm text-surface-100 placeholder-surface-500
										focus:outline-none focus:border-hecate-500"/></div> <div><label for="venture-brief" class="text-[11px] text-surface-300 block mb-1.5">Brief (optional)</label> <input id="venture-brief" placeholder="What does this venture aim to achieve?" class="w-full bg-surface-700 border border-surface-600 rounded-lg
										px-3 py-2 text-sm text-surface-100 placeholder-surface-500
										focus:outline-none focus:border-hecate-500"/></div></div> <!> <div class="flex gap-3"><button> </button> <button class="px-4 py-2 rounded-lg text-xs text-hecate-400 border border-hecate-600/30
									hover:bg-hecate-600/10 transition-colors"></button></div></div>`), w0 = /* @__PURE__ */ p(`<div class="flex flex-col items-center justify-center py-20 text-center"><div class="text-4xl mb-4 text-hecate-400"></div> <h2 class="text-lg font-semibold text-surface-100 mb-2">No Ventures Yet</h2> <p class="text-xs text-surface-400 leading-relaxed max-w-sm mb-6">A venture is the umbrella for your software endeavor. It houses
							divisions (bounded contexts) and guides them through the development
							lifecycle.</p> <button class="px-5 py-2.5 rounded-lg text-sm font-medium bg-hecate-600 text-surface-50
								hover:bg-hecate-500 transition-colors">+ Create Your First Venture</button></div>`), k0 = /* @__PURE__ */ p('<div class="text-[11px] text-surface-400 truncate mt-1.5 ml-5"> </div>'), $0 = /* @__PURE__ */ p(`<button class="group text-left p-4 rounded-xl bg-surface-800/80 border border-surface-600
												hover:border-hecate-500 hover:bg-hecate-600/5 transition-all"><div class="flex items-center gap-2"><span class="text-hecate-400 group-hover:text-hecate-300"></span> <span class="font-medium text-sm text-surface-100 truncate"> </span> <span class="text-[10px] px-1.5 py-0.5 rounded-full bg-surface-700 text-surface-300 border border-surface-600 shrink-0"> </span></div> <!></button>`), C0 = /* @__PURE__ */ p('<div><h3 class="text-[11px] text-surface-400 uppercase tracking-wider mb-3">Recently Updated</h3> <div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-3"></div></div>'), S0 = /* @__PURE__ */ p('<div class="text-[11px] text-surface-500 truncate mt-1.5 ml-5"> </div>'), E0 = /* @__PURE__ */ p(`<button class="group text-left p-4 rounded-xl bg-surface-800/40 border border-surface-700
													hover:border-surface-500 transition-all opacity-60 hover:opacity-80"><div class="flex items-center gap-2"><span class="text-surface-500"></span> <span class="font-medium text-sm text-surface-300 truncate"> </span> <span class="text-[10px] px-1.5 py-0.5 rounded-full bg-surface-700 text-surface-400 border border-surface-600 shrink-0">Archived</span></div> <!></button>`), A0 = /* @__PURE__ */ p('<div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-3"></div>'), D0 = /* @__PURE__ */ p(`<div><button class="flex items-center gap-2 text-[11px] text-surface-500 uppercase tracking-wider
										hover:text-surface-300 transition-colors mb-3"><span class="text-[9px]"> </span> <span class="text-surface-600"> </span></button> <!></div>`), P0 = /* @__PURE__ */ p('<div class="text-[11px] text-surface-400 truncate mt-1.5 ml-5"> </div>'), T0 = /* @__PURE__ */ p(`<button class="group text-left p-4 rounded-xl bg-surface-800/80 border border-surface-600
												hover:border-hecate-500 hover:bg-hecate-600/5 transition-all"><div class="flex items-center gap-2"><span class="text-hecate-400 group-hover:text-hecate-300"></span> <span class="font-medium text-sm text-surface-100 truncate"> </span> <span class="text-[10px] px-1.5 py-0.5 rounded-full bg-surface-700 text-surface-300 border border-surface-600 shrink-0"> </span></div> <!></button>`), R0 = /* @__PURE__ */ p('<div><h3 class="text-[11px] text-surface-400 uppercase tracking-wider mb-3"> </h3> <div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-3"></div></div>'), M0 = /* @__PURE__ */ p('<div class="text-center py-12 text-surface-400 text-sm"> </div>'), I0 = /* @__PURE__ */ p("<!>  <!> <!>", 1), N0 = /* @__PURE__ */ p('<div class="absolute top-0 right-0 bottom-0 z-10"><!></div>'), L0 = /* @__PURE__ */ p(
  `<div class="flex flex-col h-full overflow-hidden"><div class="border-b border-surface-600 bg-surface-800/50 px-4 py-3 shrink-0"><div class="flex items-center gap-3"><span class="text-hecate-400 text-lg"></span> <h1 class="text-sm font-semibold text-surface-100">Ventures</h1> <div class="flex items-center gap-1.5 text-[10px]"><span></span> <span class="text-surface-500"> </span></div> <!> <div class="flex-1"></div> <input placeholder="Search ventures..." class="w-48 bg-surface-700 border border-surface-600 rounded-lg
							px-3 py-1.5 text-xs text-surface-100 placeholder-surface-500
							focus:outline-none focus:border-hecate-500"/> <button> </button></div></div> <div class="flex-1 overflow-y-auto p-4 space-y-6"><!> <!></div></div> <!>`,
  1
), O0 = /* @__PURE__ */ p(`<span class="px-2 py-0.5 rounded-full bg-amber-500/20 text-amber-300
					border border-amber-500/30 animate-pulse"> </span>`), F0 = /* @__PURE__ */ p('<!> <div class="flex-1 overflow-y-auto"><!></div>', 1), j0 = /* @__PURE__ */ p('<div class="w-80 border-l border-surface-600 overflow-hidden shrink-0"><!></div>'), B0 = /* @__PURE__ */ p('<div class="flex h-full"><div class="flex-1 overflow-hidden"><!></div> <!></div>'), V0 = /* @__PURE__ */ p('<div class="w-80 border-l border-surface-600 overflow-hidden shrink-0"><!></div>'), G0 = /* @__PURE__ */ p('<div class="flex h-full"><div class="flex-1 overflow-hidden"><!></div> <!></div>'), q0 = /* @__PURE__ */ p('<div class="flex items-center justify-center h-full text-surface-400 text-sm">Select a division from the Nerve Center</div>'), H0 = /* @__PURE__ */ p('<!> <div class="absolute top-2 right-2 flex items-center gap-2 text-[10px] z-10"><!> <button title="Agent Pipeline">Agents</button> <span class="flex items-center gap-1.5"><span></span> <span class="text-surface-500"> </span></span></div> <div class="flex flex-1 overflow-hidden relative"><!> <div class="flex-1 flex flex-col overflow-hidden"><div class="flex-1 flex flex-col overflow-hidden"><!></div> <!></div> <!></div>', 1), W0 = /* @__PURE__ */ p('<div class="flex flex-col h-full"><!></div>');
function U0(e, t) {
  kt(t, !0);
  const r = () => ke(wt, "$isLoading", S), n = () => ke(St, "$activeVenture", S), c = () => ke(wn, "$aiModel", S), l = () => ke(hr, "$ventureError", S), u = () => ke(bs, "$ventures", S), d = () => ke(bn, "$showAIAssist", S), f = () => ke(Vl, "$hasPendingGates", S), g = () => ke(zs, "$pendingGates", S), m = () => ke(ss, "$divisions", S), k = () => ke(Ur, "$selectedDivision", S), y = () => ke(Bs, "$selectedPhase", S), P = () => ke(_a, "$ventureStep", S), E = () => ke(as, "$bigPicturePhase", S), Q = () => ke(Ua, "$showEventStream", S), [S, U] = Vt();
  let ae = pt(t, "api", 7), we = /* @__PURE__ */ oe(null), le = /* @__PURE__ */ oe("connecting"), ve, de = /* @__PURE__ */ oe(""), Le = /* @__PURE__ */ oe(""), Ae = /* @__PURE__ */ oe(""), De = /* @__PURE__ */ oe(!1), te = /* @__PURE__ */ oe(!1), N = /* @__PURE__ */ oe(!1), X;
  function ue(C, V) {
    let H = C;
    if (V.trim()) {
      const w = V.toLowerCase();
      H = C.filter((D) => D.name.toLowerCase().includes(w) || D.brief && D.brief.toLowerCase().includes(w));
    }
    const M = [], L = [], F = [], J = [];
    for (const w of H)
      Or(w.status, Is) ? J.push(w) : Or(w.status, Zi) || Or(w.status, eo) ? L.push(w) : w.phase === "initiated" || w.phase === "vision_refined" || w.phase === "vision_submitted" ? M.push(w) : w.phase === "discovery_completed" || w.phase === "designing" || w.phase === "planning" || w.phase === "crafting" || w.phase === "deploying" ? F.push(w) : M.push(w);
    const $e = [];
    return M.length > 0 && $e.push({ label: "Setup", ventures: M }), L.length > 0 && $e.push({ label: "Discovery", ventures: L }), F.length > 0 && $e.push({ label: "Building", ventures: F }), J.length > 0 && $e.push({ label: "Archived", ventures: J }), $e;
  }
  function Te(C) {
    return C.filter((V) => !Or(V.status, Is)).sort((V, H) => (H.updated_at ?? "").localeCompare(V.updated_at ?? "")).slice(0, 5);
  }
  async function ne() {
    try {
      x(we, await ae().get("/health"), !0), x(le, "connected");
    } catch {
      x(we, null), x(le, "disconnected");
    }
  }
  Ki(async () => {
    nl(ae()), ne(), ve = setInterval(ne, 5e3), wr(), ga();
    const C = await il();
    yn.set(C), ho(), X = nd();
  }), Vc(() => {
    ve && clearInterval(ve), rd(), X && X();
  });
  async function W() {
    if (!s(de).trim()) return;
    await oo(s(de).trim(), s(Le).trim() || "") && (x(de, ""), x(Le, ""), x(De, !1));
  }
  var K = {
    get api() {
      return ae();
    },
    set api(C) {
      ae(C), vt();
    }
  }, Fe = W0(), et = i(Fe);
  {
    var st = (C) => {
      var V = b0(), H = i(V), M = i(H);
      M.textContent = "◆", At(2), a(H), a(V), v(C, V);
    }, Xe = (C) => {
      var V = L0(), H = ct(V), M = i(H), L = i(M), F = i(L);
      F.textContent = "◆";
      var J = o(F, 4), $e = i(J), w = o($e, 2), D = i(w, !0);
      a(w), a(J);
      var z = o(J, 2);
      wa(z, {
        get currentModel() {
          return c();
        },
        onSelect: (Ue) => kn(Ue)
      });
      var pe = o(z, 4);
      bt(pe);
      var Ce = o(pe, 2);
      Ce.__click = () => x(De, !s(De));
      var T = i(Ce, !0);
      a(Ce), a(L), a(M);
      var I = o(M, 2), Z = i(I);
      {
        var Me = (Ue) => {
          var Ye = y0(), je = o(i(Ye), 2), R = i(je), G = o(i(R), 2);
          bt(G), a(R);
          var fe = o(R, 2), Pe = o(i(fe), 2);
          bt(Pe), a(fe), a(je);
          var ie = o(je, 2);
          {
            var be = (re) => {
              var j = m0(), ce = i(j, !0);
              a(j), $(() => h(ce, l())), v(re, j);
            };
            A(ie, (re) => {
              l() && re(be);
            });
          }
          var Ne = o(ie, 2), Oe = i(Ne);
          Oe.__click = W;
          var _ = i(Oe, !0);
          a(Oe);
          var b = o(Oe, 2);
          b.__click = () => Cr("Help me define a new venture. What should I consider? Ask me about the problem domain, target users, and core functionality."), b.textContent = "✦ AI Help", a(Ne), a(Ye), $(
            (re, j) => {
              Oe.disabled = re, Ie(Oe, 1, `px-4 py-2 rounded-lg text-xs font-medium transition-colors
									${j ?? ""}`), h(_, r() ? "Initiating..." : "Initiate Venture");
            },
            [
              () => !s(de).trim() || r(),
              () => !s(de).trim() || r() ? "bg-surface-600 text-surface-400 cursor-not-allowed" : "bg-hecate-600 text-surface-50 hover:bg-hecate-500"
            ]
          ), xt(G, () => s(de), (re) => x(de, re)), xt(Pe, () => s(Le), (re) => x(Le, re)), v(Ue, Ye);
        };
        A(Z, (Ue) => {
          s(De) && Ue(Me);
        });
      }
      var Be = o(Z, 2);
      {
        var Je = (Ue) => {
          var Ye = w0(), je = i(Ye);
          je.textContent = "◆";
          var R = o(je, 6);
          R.__click = () => x(De, !0), a(Ye), v(Ue, Ye);
        }, ze = (Ue) => {
          const Ye = /* @__PURE__ */ Ee(() => ue(u(), s(Ae)));
          var je = I0(), R = ct(je);
          {
            var G = (Oe) => {
              const _ = /* @__PURE__ */ Ee(() => Te(u()));
              var b = ur(), re = ct(b);
              {
                var j = (ce) => {
                  var _e = C0(), ge = o(i(_e), 2);
                  He(ge, 21, () => s(_), ft, (Y, q) => {
                    var ee = $0();
                    ee.__click = () => Ls(s(q));
                    var me = i(ee), xe = i(me);
                    xe.textContent = "◆";
                    var he = o(xe, 2), se = i(he, !0);
                    a(he);
                    var B = o(he, 2), O = i(B, !0);
                    a(B), a(me);
                    var ye = o(me, 2);
                    {
                      var Se = (Re) => {
                        var Ve = k0(), Qe = i(Ve, !0);
                        a(Ve), $(() => h(Qe, s(q).brief)), v(Re, Ve);
                      };
                      A(ye, (Re) => {
                        s(q).brief && Re(Se);
                      });
                    }
                    a(ee), $(() => {
                      h(se, s(q).name), h(O, s(q).status_label ?? s(q).phase);
                    }), v(Y, ee);
                  }), a(ge), a(_e), v(ce, _e);
                };
                A(re, (ce) => {
                  s(_).length > 0 && ce(j);
                });
              }
              v(Oe, b);
            }, fe = /* @__PURE__ */ Ee(() => !s(Ae).trim() && u().filter((Oe) => !Or(Oe.status, Is)).length > 3);
            A(R, (Oe) => {
              s(fe) && Oe(G);
            });
          }
          var Pe = o(R, 2);
          He(Pe, 17, () => s(Ye), ft, (Oe, _) => {
            var b = ur(), re = ct(b);
            {
              var j = (_e) => {
                var ge = D0(), Y = i(ge);
                Y.__click = () => x(te, !s(te));
                var q = i(Y), ee = i(q, !0);
                a(q);
                var me = o(q), xe = o(me), he = i(xe);
                a(xe), a(Y);
                var se = o(Y, 2);
                {
                  var B = (O) => {
                    var ye = A0();
                    He(ye, 21, () => s(_).ventures, ft, (Se, Re) => {
                      var Ve = E0();
                      Ve.__click = () => Ls(s(Re));
                      var Qe = i(Ve), rt = i(Qe);
                      rt.textContent = "◆";
                      var at = o(rt, 2), nt = i(at, !0);
                      a(at), At(2), a(Qe);
                      var Ct = o(Qe, 2);
                      {
                        var ht = (It) => {
                          var qt = S0(), gt = i(qt, !0);
                          a(qt), $(() => h(gt, s(Re).brief)), v(It, qt);
                        };
                        A(Ct, (It) => {
                          s(Re).brief && It(ht);
                        });
                      }
                      a(Ve), $(() => h(nt, s(Re).name)), v(Se, Ve);
                    }), a(ye), v(O, ye);
                  };
                  A(se, (O) => {
                    s(te) && O(B);
                  });
                }
                a(ge), $(() => {
                  h(ee, s(te) ? "▼" : "▶"), h(me, ` ${s(_).label ?? ""} `), h(he, `(${s(_).ventures.length ?? ""})`);
                }), v(_e, ge);
              }, ce = (_e) => {
                var ge = R0(), Y = i(ge), q = i(Y, !0);
                a(Y);
                var ee = o(Y, 2);
                He(ee, 21, () => s(_).ventures, ft, (me, xe) => {
                  var he = T0();
                  he.__click = () => Ls(s(xe));
                  var se = i(he), B = i(se);
                  B.textContent = "◆";
                  var O = o(B, 2), ye = i(O, !0);
                  a(O);
                  var Se = o(O, 2), Re = i(Se, !0);
                  a(Se), a(se);
                  var Ve = o(se, 2);
                  {
                    var Qe = (rt) => {
                      var at = P0(), nt = i(at, !0);
                      a(at), $(() => h(nt, s(xe).brief)), v(rt, at);
                    };
                    A(Ve, (rt) => {
                      s(xe).brief && rt(Qe);
                    });
                  }
                  a(he), $(() => {
                    h(ye, s(xe).name), h(Re, s(xe).status_label ?? s(xe).phase);
                  }), v(me, he);
                }), a(ee), a(ge), $(() => h(q, s(_).label)), v(_e, ge);
              };
              A(re, (_e) => {
                s(_).label === "Archived" ? _e(j) : _e(ce, !1);
              });
            }
            v(Oe, b);
          });
          var ie = o(Pe, 2);
          {
            var be = (Oe) => {
              var _ = M0(), b = i(_);
              a(_), $(() => h(b, `No ventures matching "${s(Ae) ?? ""}"`)), v(Oe, _);
            }, Ne = /* @__PURE__ */ Ee(() => s(Ye).length === 0 && s(Ae).trim());
            A(ie, (Oe) => {
              s(Ne) && Oe(be);
            });
          }
          v(Ue, je);
        };
        A(Be, (Ue) => {
          u().length === 0 && !s(De) ? Ue(Je) : Ue(ze, !1);
        });
      }
      a(I), a(H);
      var tt = o(H, 2);
      {
        var qe = (Ue) => {
          var Ye = N0(), je = i(Ye);
          en(je, {}), a(Ye), v(Ue, Ye);
        };
        A(tt, (Ue) => {
          d() && Ue(qe);
        });
      }
      $(() => {
        Ie($e, 1, `inline-block w-1.5 h-1.5 rounded-full ${s(le) === "connected" ? "bg-success-400" : s(le) === "connecting" ? "bg-yellow-400 animate-pulse" : "bg-danger-400"}`), h(D, s(le) === "connected" ? `v${s(we)?.version ?? "?"}` : s(le)), Ie(Ce, 1, `px-3 py-1.5 rounded-lg text-xs font-medium transition-colors
							${s(De) ? "bg-surface-600 text-surface-300" : "bg-hecate-600 text-surface-50 hover:bg-hecate-500"}`), h(T, s(De) ? "Cancel" : "+ New Venture");
      }), xt(pe, () => s(Ae), (Ue) => x(Ae, Ue)), v(C, V);
    }, We = (C) => {
      var V = H0(), H = ct(V);
      mo(H, {});
      var M = o(H, 2), L = i(M);
      {
        var F = (je) => {
          var R = O0(), G = i(R);
          a(R), $(() => h(G, `${g().length ?? ""} gate${g().length !== 1 ? "s" : ""}`)), v(je, R);
        };
        A(L, (je) => {
          f() && je(F);
        });
      }
      var J = o(L, 2);
      J.__click = () => x(N, !s(N));
      var $e = o(J, 2), w = i($e), D = o(w, 2), z = i(D, !0);
      a(D), a($e), a(M);
      var pe = o(M, 2), Ce = i(pe);
      {
        var T = (je) => {
          Ro(je, {
            onSelectAgent: (R) => {
              x(N, !0);
            },
            onSelectGate: (R) => {
              x(N, !0);
            }
          });
        };
        A(Ce, (je) => {
          (m().length > 0 || g().length > 0) && je(T);
        });
      }
      var I = o(Ce, 2), Z = i(I), Me = i(Z);
      {
        var Be = (je) => {
          Po(je, {});
        }, Je = (je) => {
          var R = F0(), G = ct(R);
          To(G, {});
          var fe = o(G, 2), Pe = i(fe);
          {
            var ie = (_) => {
              ko(_, {});
            }, be = (_) => {
              $o(_, {});
            }, Ne = (_) => {
              Co(_, {});
            }, Oe = (_) => {
              Eo(_, {});
            };
            A(Pe, (_) => {
              y() === "storming" ? _(ie) : y() === "planning" ? _(be, 1) : y() === "kanban" ? _(Ne, 2) : y() === "crafting" && _(Oe, 3);
            });
          }
          a(fe), v(je, R);
        }, ze = (je) => {
          var R = ur(), G = ct(R);
          {
            var fe = (_) => {
              var b = B0(), re = i(b), j = i(re);
              Qa(j, {}), a(re);
              var ce = o(re, 2);
              {
                var _e = (ge) => {
                  var Y = j0(), q = i(Y);
                  Za(q, {}), a(Y), v(ge, Y);
                };
                A(ce, (ge) => {
                  Q() && ge(_e);
                });
              }
              a(b), v(_, b);
            }, Pe = (_) => {
              wo(_, {});
            }, ie = (_) => {
              ea(_, { nextAction: "discovery" });
            }, be = (_) => {
              var b = G0(), re = i(b), j = i(re);
              Qa(j, {}), a(re);
              var ce = o(re, 2);
              {
                var _e = (ge) => {
                  var Y = V0(), q = i(Y);
                  Za(q, {}), a(Y), v(ge, Y);
                };
                A(ce, (ge) => {
                  Q() && ge(_e);
                });
              }
              a(b), v(_, b);
            }, Ne = (_) => {
              ea(_, { nextAction: "identify" });
            }, Oe = (_) => {
              ea(_, { nextAction: "discovery" });
            };
            A(G, (_) => {
              P() === "discovering" || E() !== "ready" ? _(fe) : P() === "initiated" || P() === "vision_refined" ? _(Pe, 1) : P() === "vision_submitted" ? _(ie, 2) : P() === "discovery_paused" ? _(be, 3) : P() === "discovery_completed" ? _(Ne, 4) : _(Oe, !1);
            });
          }
          v(je, R);
        }, tt = (je) => {
          var R = q0();
          v(je, R);
        };
        A(Me, (je) => {
          s(N) ? je(Be) : k() ? je(Je, 1) : m().length === 0 ? je(ze, 2) : je(tt, !1);
        });
      }
      a(Z);
      var qe = o(Z, 2);
      Mo(qe, {}), a(I);
      var Ue = o(I, 2);
      {
        var Ye = (je) => {
          en(je, {});
        };
        A(Ue, (je) => {
          d() && je(Ye);
        });
      }
      a(pe), $(() => {
        Ie(J, 1, `px-2 py-0.5 rounded transition-colors
					${s(N) ? "bg-hecate-600/20 text-hecate-300" : "text-surface-400 hover:text-surface-200 hover:bg-surface-700"}`), Ie(w, 1, `inline-block w-1.5 h-1.5 rounded-full ${s(le) === "connected" ? "bg-success-400" : s(le) === "connecting" ? "bg-yellow-400 animate-pulse" : "bg-danger-400"}`), h(z, s(le) === "connected" ? `v${s(we)?.version ?? "?"}` : s(le));
      }), v(C, V);
    };
    A(et, (C) => {
      r() && !n() ? C(st) : n() ? C(We, !1) : C(Xe, 1);
    });
  }
  a(Fe), v(e, Fe);
  var Ge = $t(K);
  return U(), Ge;
}
Mt(["click"]);
customElements.define("martha-studio", Et(U0, { api: {} }, [], []));
export {
  U0 as default
};
