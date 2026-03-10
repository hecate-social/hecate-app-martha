typeof window < "u" && ((window.__svelte ??= {}).v ??= /* @__PURE__ */ new Set()).add("5");
const Ui = 1, zi = 2, yn = 4, Gi = 8, Yi = 16, Ki = 2, Ji = 4, Qi = 8, Xi = 1, Zi = 2, Sa = "[", qs = "[!", Aa = "]", Lr = {}, yt = /* @__PURE__ */ Symbol(), eo = "http://www.w3.org/1999/xhtml", oa = !1;
var Da = Array.isArray, to = Array.prototype.indexOf, Qr = Array.prototype.includes, Us = Array.from, Ns = Object.keys, ms = Object.defineProperty, Gr = Object.getOwnPropertyDescriptor, wn = Object.getOwnPropertyDescriptors, ro = Object.prototype, so = Array.prototype, Pa = Object.getPrototypeOf, Za = Object.isExtensible;
const fr = () => {
};
function ao(e) {
  return e();
}
function Os(e) {
  for (var t = 0; t < e.length; t++)
    e[t]();
}
function $n() {
  var e, t, r = new Promise((a, o) => {
    e = a, t = o;
  });
  return { promise: r, resolve: e, reject: t };
}
function ca(e, t) {
  if (Array.isArray(e))
    return e;
  if (!(Symbol.iterator in e))
    return Array.from(e);
  const r = [];
  for (const a of e)
    if (r.push(a), r.length === t) break;
  return r;
}
const $t = 2, Ls = 4, $s = 8, kn = 1 << 24, xr = 16, tr = 32, Cr = 64, Ta = 128, Ut = 512, gt = 1024, kt = 2048, er = 4096, Lt = 8192, pr = 16384, zs = 32768, Xr = 65536, en = 1 << 17, En = 1 << 18, Br = 1 << 19, Cn = 1 << 20, dr = 1 << 25, Rr = 32768, la = 1 << 21, Ma = 1 << 22, yr = 1 << 23, wr = /* @__PURE__ */ Symbol("$state"), no = /* @__PURE__ */ Symbol("legacy props"), io = /* @__PURE__ */ Symbol(""), zr = new class extends Error {
  name = "StaleReactionError";
  message = "The reaction that called `getAbortSignal()` was re-run or destroyed";
}(), Gs = 3, Hr = 8;
function Sn(e) {
  throw new Error("https://svelte.dev/e/lifecycle_outside_component");
}
function oo() {
  throw new Error("https://svelte.dev/e/async_derived_orphan");
}
function co(e, t, r) {
  throw new Error("https://svelte.dev/e/each_key_duplicate");
}
function lo(e) {
  throw new Error("https://svelte.dev/e/effect_in_teardown");
}
function uo() {
  throw new Error("https://svelte.dev/e/effect_in_unowned_derived");
}
function vo(e) {
  throw new Error("https://svelte.dev/e/effect_orphan");
}
function fo() {
  throw new Error("https://svelte.dev/e/effect_update_depth_exceeded");
}
function po() {
  throw new Error("https://svelte.dev/e/hydration_failed");
}
function ho() {
  throw new Error("https://svelte.dev/e/state_descriptors_fixed");
}
function _o() {
  throw new Error("https://svelte.dev/e/state_prototype_fixed");
}
function xo() {
  throw new Error("https://svelte.dev/e/state_unsafe_mutation");
}
function go() {
  throw new Error("https://svelte.dev/e/svelte_boundary_reset_onerror");
}
function ks(e) {
  console.warn("https://svelte.dev/e/hydration_mismatch");
}
function bo() {
  console.warn("https://svelte.dev/e/select_multiple_invalid_value");
}
function mo() {
  console.warn("https://svelte.dev/e/svelte_boundary_reset_noop");
}
let Xe = !1;
function vr(e) {
  Xe = e;
}
let rt;
function Ct(e) {
  if (e === null)
    throw ks(), Lr;
  return rt = e;
}
function Zr() {
  return Ct(/* @__PURE__ */ rr(rt));
}
function n(e) {
  if (Xe) {
    if (/* @__PURE__ */ rr(rt) !== null)
      throw ks(), Lr;
    rt = e;
  }
}
function wt(e = 1) {
  if (Xe) {
    for (var t = e, r = rt; t--; )
      r = /** @type {TemplateNode} */
      /* @__PURE__ */ rr(r);
    rt = r;
  }
}
function Rs(e = !0) {
  for (var t = 0, r = rt; ; ) {
    if (r.nodeType === Hr) {
      var a = (
        /** @type {Comment} */
        r.data
      );
      if (a === Aa) {
        if (t === 0) return r;
        t -= 1;
      } else (a === Sa || a === qs || // "[1", "[2", etc. for if blocks
      a[0] === "[" && !isNaN(Number(a.slice(1)))) && (t += 1);
    }
    var o = (
      /** @type {TemplateNode} */
      /* @__PURE__ */ rr(r)
    );
    e && r.remove(), r = o;
  }
}
function An(e) {
  if (!e || e.nodeType !== Hr)
    throw ks(), Lr;
  return (
    /** @type {Comment} */
    e.data
  );
}
function Dn(e) {
  return e === this.v;
}
function Pn(e, t) {
  return e != e ? t == t : e !== t || e !== null && typeof e == "object" || typeof e == "function";
}
function Tn(e) {
  return !Pn(e, this.v);
}
let as = !1, yo = !1;
function wo() {
  as = !0;
}
let dt = null;
function es(e) {
  dt = e;
}
function Tt(e, t = !1, r) {
  dt = {
    p: dt,
    i: !1,
    c: null,
    e: null,
    s: e,
    x: null,
    l: as && !t ? { s: null, u: null, $: [] } : null
  };
}
function Mt(e) {
  var t = (
    /** @type {ComponentContext} */
    dt
  ), r = t.e;
  if (r !== null) {
    t.e = null;
    for (var a of r)
      Qn(a);
  }
  return e !== void 0 && (t.x = e), t.i = !0, dt = t.p, e ?? /** @type {T} */
  {};
}
function Es() {
  return !as || dt !== null && dt.l === null;
}
let Ar = [];
function Mn() {
  var e = Ar;
  Ar = [], Os(e);
}
function or(e) {
  if (Ar.length === 0 && !fs) {
    var t = Ar;
    queueMicrotask(() => {
      t === Ar && Mn();
    });
  }
  Ar.push(e);
}
function $o() {
  for (; Ar.length > 0; )
    Mn();
}
function In(e) {
  var t = st;
  if (t === null)
    return tt.f |= yr, e;
  if ((t.f & zs) === 0) {
    if ((t.f & Ta) === 0)
      throw e;
    t.b.error(e);
  } else
    ts(e, t);
}
function ts(e, t) {
  for (; t !== null; ) {
    if ((t.f & Ta) !== 0)
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
const ko = -7169;
function vt(e, t) {
  e.f = e.f & ko | t;
}
function Ia(e) {
  (e.f & Ut) !== 0 || e.deps === null ? vt(e, gt) : vt(e, er);
}
function Nn(e) {
  if (e !== null)
    for (const t of e)
      (t.f & $t) === 0 || (t.f & Rr) === 0 || (t.f ^= Rr, Nn(
        /** @type {Derived} */
        t.deps
      ));
}
function On(e, t, r) {
  (e.f & kt) !== 0 ? t.add(e) : (e.f & er) !== 0 && r.add(e), Nn(e.deps), vt(e, gt);
}
const Ps = /* @__PURE__ */ new Set();
let at = null, Fs = null, Qt = null, Dt = [], Ys = null, ua = !1, fs = !1;
class hr {
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
      for (var a of r.d)
        vt(a, kt), Xt(a);
      for (a of r.m)
        vt(a, er), Xt(a);
    }
  }
  /**
   *
   * @param {Effect[]} root_effects
   */
  process(t) {
    Dt = [], this.apply();
    var r = [], a = [];
    for (const o of t)
      this.#l(o, r, a);
    if (this.is_deferred()) {
      this.#d(a), this.#d(r);
      for (const [o, c] of this.#n)
        Vn(o, c);
    } else {
      for (const o of this.#e) o();
      this.#e.clear(), this.#r === 0 && this.#u(), Fs = this, at = null, tn(a), tn(r), Fs = null, this.#o?.resolve();
    }
    Qt = null;
  }
  /**
   * Traverse the effect tree, executing effects or stashing
   * them for later execution as appropriate
   * @param {Effect} root
   * @param {Effect[]} effects
   * @param {Effect[]} render_effects
   */
  #l(t, r, a) {
    t.f ^= gt;
    for (var o = t.first, c = null; o !== null; ) {
      var u = o.f, d = (u & (tr | Cr)) !== 0, v = d && (u & gt) !== 0, _ = v || (u & Lt) !== 0 || this.#n.has(o);
      if (!_ && o.fn !== null) {
        d ? o.f ^= gt : c !== null && (u & (Ls | $s | kn)) !== 0 ? c.b.defer_effect(o) : (u & Ls) !== 0 ? r.push(o) : Ss(o) && ((u & xr) !== 0 && this.#s.add(o), ys(o));
        var x = o.first;
        if (x !== null) {
          o = x;
          continue;
        }
      }
      var b = o.parent;
      for (o = o.next; o === null && b !== null; )
        b === c && (c = null), o = b.next, b = b.parent;
    }
  }
  /**
   * @param {Effect[]} effects
   */
  #d(t) {
    for (var r = 0; r < t.length; r += 1)
      On(t[r], this.#a, this.#s);
  }
  /**
   * Associate a change to a given source with the current
   * batch, noting its previous and current values
   * @param {Source} source
   * @param {any} value
   */
  capture(t, r) {
    r !== yt && !this.previous.has(t) && this.previous.set(t, r), (t.f & yr) === 0 && (this.current.set(t, t.v), Qt?.set(t, t.v));
  }
  activate() {
    at = this, this.apply();
  }
  deactivate() {
    at === this && (at = null, Qt = null);
  }
  flush() {
    if (this.activate(), Dt.length > 0) {
      if (Ln(), at !== null && at !== this)
        return;
    } else this.#r === 0 && this.process([]);
    this.deactivate();
  }
  discard() {
    for (const t of this.#t) t(this);
    this.#t.clear();
  }
  #u() {
    if (Ps.size > 1) {
      this.previous.clear();
      var t = Qt, r = !0;
      for (const o of Ps) {
        if (o === this) {
          r = !1;
          continue;
        }
        const c = [];
        for (const [d, v] of this.current) {
          if (o.current.has(d))
            if (r && v !== o.current.get(d))
              o.current.set(d, v);
            else
              continue;
          c.push(d);
        }
        if (c.length === 0)
          continue;
        const u = [...o.current.keys()].filter((d) => !this.current.has(d));
        if (u.length > 0) {
          var a = Dt;
          Dt = [];
          const d = /* @__PURE__ */ new Set(), v = /* @__PURE__ */ new Map();
          for (const _ of c)
            Rn(_, u, d, v);
          if (Dt.length > 0) {
            at = o, o.apply();
            for (const _ of Dt)
              o.#l(_, [], []);
            o.deactivate();
          }
          Dt = a;
        }
      }
      at = null, Qt = t;
    }
    this.committed = !0, Ps.delete(this);
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
    this.#r -= 1, t && (this.#i -= 1), !this.#c && (this.#c = !0, or(() => {
      this.#c = !1, this.is_deferred() ? Dt.length > 0 && this.flush() : this.revive();
    }));
  }
  revive() {
    for (const t of this.#a)
      this.#s.delete(t), vt(t, kt), Xt(t);
    for (const t of this.#s)
      vt(t, er), Xt(t);
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
    return (this.#o ??= $n()).promise;
  }
  static ensure() {
    if (at === null) {
      const t = at = new hr();
      Ps.add(at), fs || or(() => {
        at === t && t.flush();
      });
    }
    return at;
  }
  apply() {
  }
}
function pt(e) {
  var t = fs;
  fs = !0;
  try {
    for (var r; ; ) {
      if ($o(), Dt.length === 0 && (at?.flush(), Dt.length === 0))
        return Ys = null, /** @type {T} */
        r;
      Ln();
    }
  } finally {
    fs = t;
  }
}
function Ln() {
  ua = !0;
  var e = null;
  try {
    for (var t = 0; Dt.length > 0; ) {
      var r = hr.ensure();
      if (t++ > 1e3) {
        var a, o;
        Eo();
      }
      r.process(Dt), $r.clear();
    }
  } finally {
    Dt = [], ua = !1, Ys = null;
  }
}
function Eo() {
  try {
    fo();
  } catch (e) {
    ts(e, Ys);
  }
}
let lr = null;
function tn(e) {
  var t = e.length;
  if (t !== 0) {
    for (var r = 0; r < t; ) {
      var a = e[r++];
      if ((a.f & (pr | Lt)) === 0 && Ss(a) && (lr = /* @__PURE__ */ new Set(), ys(a), a.deps === null && a.first === null && a.nodes === null && (a.teardown === null && a.ac === null ? ri(a) : a.fn = null), lr?.size > 0)) {
        $r.clear();
        for (const o of lr) {
          if ((o.f & (pr | Lt)) !== 0) continue;
          const c = [o];
          let u = o.parent;
          for (; u !== null; )
            lr.has(u) && (lr.delete(u), c.push(u)), u = u.parent;
          for (let d = c.length - 1; d >= 0; d--) {
            const v = c[d];
            (v.f & (pr | Lt)) === 0 && ys(v);
          }
        }
        lr.clear();
      }
    }
    lr = null;
  }
}
function Rn(e, t, r, a) {
  if (!r.has(e) && (r.add(e), e.reactions !== null))
    for (const o of e.reactions) {
      const c = o.f;
      (c & $t) !== 0 ? Rn(
        /** @type {Derived} */
        o,
        t,
        r,
        a
      ) : (c & (Ma | xr)) !== 0 && (c & kt) === 0 && Fn(o, t, a) && (vt(o, kt), Xt(
        /** @type {Effect} */
        o
      ));
    }
}
function Fn(e, t, r) {
  const a = r.get(e);
  if (a !== void 0) return a;
  if (e.deps !== null)
    for (const o of e.deps) {
      if (Qr.call(t, o))
        return !0;
      if ((o.f & $t) !== 0 && Fn(
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
function Xt(e) {
  for (var t = Ys = e; t.parent !== null; ) {
    t = t.parent;
    var r = t.f;
    if (ua && t === st && (r & xr) !== 0 && (r & En) === 0)
      return;
    if ((r & (Cr | tr)) !== 0) {
      if ((r & gt) === 0) return;
      t.f ^= gt;
    }
  }
  Dt.push(t);
}
function Vn(e, t) {
  if (!((e.f & tr) !== 0 && (e.f & gt) !== 0)) {
    (e.f & kt) !== 0 ? t.d.push(e) : (e.f & er) !== 0 && t.m.push(e), vt(e, gt);
    for (var r = e.first; r !== null; )
      Vn(r, t), r = r.next;
  }
}
function Co(e) {
  let t = 0, r = Fr(0), a;
  return () => {
    Va() && (s(r), Qs(() => (t === 0 && (a = Wr(() => e(() => ps(r)))), t += 1, () => {
      or(() => {
        t -= 1, t === 0 && (a?.(), a = void 0, ps(r));
      });
    })));
  };
}
var So = Xr | Br | Ta;
function Ao(e, t, r) {
  new Do(e, t, r);
}
class Do {
  /** @type {Boundary | null} */
  parent;
  is_pending = !1;
  /** @type {TemplateNode} */
  #e;
  /** @type {TemplateNode | null} */
  #t = Xe ? rt : null;
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
  #d = 0;
  #u = 0;
  #p = !1;
  #f = !1;
  /** @type {Set<Effect>} */
  #h = /* @__PURE__ */ new Set();
  /** @type {Set<Effect>} */
  #_ = /* @__PURE__ */ new Set();
  /**
   * A source containing the number of pending async deriveds/expressions.
   * Only created if `$effect.pending()` is used inside the boundary,
   * otherwise updating the source results in needless `Batch.ensure()`
   * calls followed by no-op flushes
   * @type {Source<number> | null}
   */
  #v = null;
  #y = Co(() => (this.#v = Fr(this.#d), () => {
    this.#v = null;
  }));
  /**
   * @param {TemplateNode} node
   * @param {BoundaryProps} props
   * @param {((anchor: Node) => void)} children
   */
  constructor(t, r, a) {
    this.#e = t, this.#r = r, this.#i = a, this.parent = /** @type {Effect} */
    st.b, this.is_pending = !!this.#r.pending, this.#o = ja(() => {
      if (st.b = this, Xe) {
        const c = this.#t;
        Zr(), /** @type {Comment} */
        c.nodeType === Hr && /** @type {Comment} */
        c.data === qs ? this.#$() : (this.#w(), this.#u === 0 && (this.is_pending = !1));
      } else {
        var o = this.#b();
        try {
          this.#a = Ht(() => a(o));
        } catch (c) {
          this.error(c);
        }
        this.#u > 0 ? this.#g() : this.is_pending = !1;
      }
      return () => {
        this.#l?.remove();
      };
    }, So), Xe && (this.#e = rt);
  }
  #w() {
    try {
      this.#a = Ht(() => this.#i(this.#e));
    } catch (t) {
      this.error(t);
    }
  }
  #$() {
    const t = this.#r.pending;
    t && (this.#s = Ht(() => t(this.#e)), or(() => {
      var r = this.#b();
      this.#a = this.#x(() => (hr.ensure(), Ht(() => this.#i(r)))), this.#u > 0 ? this.#g() : (Mr(
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
    return this.is_pending && (this.#l = zt(), this.#e.before(this.#l), t = this.#l), t;
  }
  /**
   * Defer an effect inside a pending boundary until the boundary resolves
   * @param {Effect} effect
   */
  defer_effect(t) {
    On(t, this.#h, this.#_);
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
  #x(t) {
    var r = st, a = tt, o = dt;
    cr(this.#o), Yt(this.#o), es(this.#o.ctx);
    try {
      return t();
    } catch (c) {
      return In(c), null;
    } finally {
      cr(r), Yt(a), es(o);
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
    ), ni(this.#a, this.#c)), this.#s === null && (this.#s = Ht(() => t(this.#e)));
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
    if (this.#u += t, this.#u === 0) {
      this.is_pending = !1;
      for (const r of this.#h)
        vt(r, kt), Xt(r);
      for (const r of this.#_)
        vt(r, er), Xt(r);
      this.#h.clear(), this.#_.clear(), this.#s && Mr(this.#s, () => {
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
    this.#m(t), this.#d += t, !(!this.#v || this.#p) && (this.#p = !0, or(() => {
      this.#p = !1, this.#v && rs(this.#v, this.#d);
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
    this.#a && (St(this.#a), this.#a = null), this.#s && (St(this.#s), this.#s = null), this.#n && (St(this.#n), this.#n = null), Xe && (Ct(
      /** @type {TemplateNode} */
      this.#t
    ), wt(), Ct(Rs()));
    var o = !1, c = !1;
    const u = () => {
      if (o) {
        mo();
        return;
      }
      o = !0, c && go(), hr.ensure(), this.#d = 0, this.#n !== null && Mr(this.#n, () => {
        this.#n = null;
      }), this.is_pending = this.has_pending_snippet(), this.#a = this.#x(() => (this.#f = !1, Ht(() => this.#i(this.#e)))), this.#u > 0 ? this.#g() : this.is_pending = !1;
    };
    or(() => {
      try {
        c = !0, r?.(t, u), c = !1;
      } catch (d) {
        ts(d, this.#o && this.#o.parent);
      }
      a && (this.#n = this.#x(() => {
        hr.ensure(), this.#f = !0;
        try {
          return Ht(() => {
            a(
              this.#e,
              () => t,
              () => u
            );
          });
        } catch (d) {
          return ts(
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
function Po(e, t, r, a) {
  const o = Es() ? Cs : Vs;
  var c = e.filter((y) => !y.settled);
  if (r.length === 0 && c.length === 0) {
    a(t.map(o));
    return;
  }
  var u = at, d = (
    /** @type {Effect} */
    st
  ), v = To(), _ = c.length === 1 ? c[0].promise : c.length > 1 ? Promise.all(c.map((y) => y.promise)) : null;
  function x(y) {
    v();
    try {
      a(y);
    } catch (P) {
      (d.f & pr) === 0 && ts(P, d);
    }
    u?.deactivate(), da();
  }
  if (r.length === 0) {
    _.then(() => x(t.map(o)));
    return;
  }
  function b() {
    v(), Promise.all(r.map((y) => /* @__PURE__ */ Mo(y))).then((y) => x([...t.map(o), ...y])).catch((y) => ts(y, d));
  }
  _ ? _.then(b) : b();
}
function To() {
  var e = st, t = tt, r = dt, a = at;
  return function(c = !0) {
    cr(e), Yt(t), es(r), c && a?.activate();
  };
}
function da() {
  cr(null), Yt(null), es(null);
}
// @__NO_SIDE_EFFECTS__
function Cs(e) {
  var t = $t | kt, r = tt !== null && (tt.f & $t) !== 0 ? (
    /** @type {Derived} */
    tt
  ) : null;
  return st !== null && (st.f |= Br), {
    ctx: dt,
    deps: null,
    effects: null,
    equals: Dn,
    f: t,
    fn: e,
    reactions: null,
    rv: 0,
    v: (
      /** @type {V} */
      yt
    ),
    wv: 0,
    parent: r ?? st,
    ac: null
  };
}
// @__NO_SIDE_EFFECTS__
function Mo(e, t, r) {
  let a = (
    /** @type {Effect | null} */
    st
  );
  a === null && oo();
  var o = (
    /** @type {Boundary} */
    a.b
  ), c = (
    /** @type {Promise<V>} */
    /** @type {unknown} */
    void 0
  ), u = Fr(
    /** @type {V} */
    yt
  ), d = !tt, v = /* @__PURE__ */ new Map();
  return jo(() => {
    var _ = $n();
    c = _.promise;
    try {
      Promise.resolve(e()).then(_.resolve, _.reject).then(() => {
        x === at && x.committed && x.deactivate(), da();
      });
    } catch (P) {
      _.reject(P), da();
    }
    var x = (
      /** @type {Batch} */
      at
    );
    if (d) {
      var b = o.is_rendered();
      o.update_pending_count(1), x.increment(b), v.get(x)?.reject(zr), v.delete(x), v.set(x, _);
    }
    const y = (P, $ = void 0) => {
      if (x.activate(), $)
        $ !== zr && (u.f |= yr, rs(u, $));
      else {
        (u.f & yr) !== 0 && (u.f ^= yr), rs(u, P);
        for (const [N, C] of v) {
          if (v.delete(N), N === x) break;
          C.reject(zr);
        }
      }
      d && (o.update_pending_count(-1), x.decrement(b));
    };
    _.promise.then(y, (P) => y(null, P || "unknown"));
  }), Js(() => {
    for (const _ of v.values())
      _.reject(zr);
  }), new Promise((_) => {
    function x(b) {
      function y() {
        b === c ? _(u) : x(c);
      }
      b.then(y, y);
    }
    x(c);
  });
}
// @__NO_SIDE_EFFECTS__
function ge(e) {
  const t = /* @__PURE__ */ Cs(e);
  return ii(t), t;
}
// @__NO_SIDE_EFFECTS__
function Vs(e) {
  const t = /* @__PURE__ */ Cs(e);
  return t.equals = Tn, t;
}
function jn(e) {
  var t = e.effects;
  if (t !== null) {
    e.effects = null;
    for (var r = 0; r < t.length; r += 1)
      St(
        /** @type {Effect} */
        t[r]
      );
  }
}
function Io(e) {
  for (var t = e.parent; t !== null; ) {
    if ((t.f & $t) === 0)
      return (t.f & pr) === 0 ? (
        /** @type {Effect} */
        t
      ) : null;
    t = t.parent;
  }
  return null;
}
function Na(e) {
  var t, r = st;
  cr(Io(e));
  try {
    e.f &= ~Rr, jn(e), t = ui(e);
  } finally {
    cr(r);
  }
  return t;
}
function Bn(e) {
  var t = Na(e);
  if (!e.equals(t) && (e.wv = ci(), (!at?.is_fork || e.deps === null) && (e.v = t, e.deps === null))) {
    vt(e, gt);
    return;
  }
  Er || (Qt !== null ? (Va() || at?.is_fork) && Qt.set(e, t) : Ia(e));
}
let va = /* @__PURE__ */ new Set();
const $r = /* @__PURE__ */ new Map();
let Hn = !1;
function Fr(e, t) {
  var r = {
    f: 0,
    // TODO ideally we could skip this altogether, but it causes type errors
    v: e,
    reactions: null,
    equals: Dn,
    rv: 0,
    wv: 0
  };
  return r;
}
// @__NO_SIDE_EFFECTS__
function se(e, t) {
  const r = Fr(e);
  return ii(r), r;
}
// @__NO_SIDE_EFFECTS__
function Oa(e, t = !1, r = !0) {
  const a = Fr(e);
  return t || (a.equals = Tn), as && r && dt !== null && dt.l !== null && (dt.l.s ??= []).push(a), a;
}
function h(e, t, r = !1) {
  tt !== null && // since we are untracking the function inside `$inspect.with` we need to add this check
  // to ensure we error if state is set inside an inspect effect
  (!Zt || (tt.f & en) !== 0) && Es() && (tt.f & ($t | xr | Ma | en)) !== 0 && (Gt === null || !Qr.call(Gt, e)) && xo();
  let a = r ? Wt(t) : t;
  return rs(e, a);
}
function rs(e, t) {
  if (!e.equals(t)) {
    var r = e.v;
    Er ? $r.set(e, t) : $r.set(e, r), e.v = t;
    var a = hr.ensure();
    if (a.capture(e, r), (e.f & $t) !== 0) {
      const o = (
        /** @type {Derived} */
        e
      );
      (e.f & kt) !== 0 && Na(o), Ia(o);
    }
    e.wv = ci(), Wn(e, kt), Es() && st !== null && (st.f & gt) !== 0 && (st.f & (tr | Cr)) === 0 && (Bt === null ? Ho([e]) : Bt.push(e)), !a.is_fork && va.size > 0 && !Hn && No();
  }
  return t;
}
function No() {
  Hn = !1;
  for (const e of va)
    (e.f & gt) !== 0 && vt(e, er), Ss(e) && ys(e);
  va.clear();
}
function ps(e) {
  h(e, e.v + 1);
}
function Wn(e, t) {
  var r = e.reactions;
  if (r !== null)
    for (var a = Es(), o = r.length, c = 0; c < o; c++) {
      var u = r[c], d = u.f;
      if (!(!a && u === st)) {
        var v = (d & kt) === 0;
        if (v && vt(u, t), (d & $t) !== 0) {
          var _ = (
            /** @type {Derived} */
            u
          );
          Qt?.delete(_), (d & Rr) === 0 && (d & Ut && (u.f |= Rr), Wn(_, er));
        } else v && ((d & xr) !== 0 && lr !== null && lr.add(
          /** @type {Effect} */
          u
        ), Xt(
          /** @type {Effect} */
          u
        ));
      }
    }
}
function Wt(e) {
  if (typeof e != "object" || e === null || wr in e)
    return e;
  const t = Pa(e);
  if (t !== ro && t !== so)
    return e;
  var r = /* @__PURE__ */ new Map(), a = Da(e), o = /* @__PURE__ */ se(0), c = Ir, u = (d) => {
    if (Ir === c)
      return d();
    var v = tt, _ = Ir;
    Yt(null), on(c);
    var x = d();
    return Yt(v), on(_), x;
  };
  return a && r.set("length", /* @__PURE__ */ se(
    /** @type {any[]} */
    e.length
  )), new Proxy(
    /** @type {any} */
    e,
    {
      defineProperty(d, v, _) {
        (!("value" in _) || _.configurable === !1 || _.enumerable === !1 || _.writable === !1) && ho();
        var x = r.get(v);
        return x === void 0 ? u(() => {
          var b = /* @__PURE__ */ se(_.value);
          return r.set(v, b), b;
        }) : h(x, _.value, !0), !0;
      },
      deleteProperty(d, v) {
        var _ = r.get(v);
        if (_ === void 0) {
          if (v in d) {
            const x = u(() => /* @__PURE__ */ se(yt));
            r.set(v, x), ps(o);
          }
        } else
          h(_, yt), ps(o);
        return !0;
      },
      get(d, v, _) {
        if (v === wr)
          return e;
        var x = r.get(v), b = v in d;
        if (x === void 0 && (!b || Gr(d, v)?.writable) && (x = u(() => {
          var P = Wt(b ? d[v] : yt), $ = /* @__PURE__ */ se(P);
          return $;
        }), r.set(v, x)), x !== void 0) {
          var y = s(x);
          return y === yt ? void 0 : y;
        }
        return Reflect.get(d, v, _);
      },
      getOwnPropertyDescriptor(d, v) {
        var _ = Reflect.getOwnPropertyDescriptor(d, v);
        if (_ && "value" in _) {
          var x = r.get(v);
          x && (_.value = s(x));
        } else if (_ === void 0) {
          var b = r.get(v), y = b?.v;
          if (b !== void 0 && y !== yt)
            return {
              enumerable: !0,
              configurable: !0,
              value: y,
              writable: !0
            };
        }
        return _;
      },
      has(d, v) {
        if (v === wr)
          return !0;
        var _ = r.get(v), x = _ !== void 0 && _.v !== yt || Reflect.has(d, v);
        if (_ !== void 0 || st !== null && (!x || Gr(d, v)?.writable)) {
          _ === void 0 && (_ = u(() => {
            var y = x ? Wt(d[v]) : yt, P = /* @__PURE__ */ se(y);
            return P;
          }), r.set(v, _));
          var b = s(_);
          if (b === yt)
            return !1;
        }
        return x;
      },
      set(d, v, _, x) {
        var b = r.get(v), y = v in d;
        if (a && v === "length")
          for (var P = _; P < /** @type {Source<number>} */
          b.v; P += 1) {
            var $ = r.get(P + "");
            $ !== void 0 ? h($, yt) : P in d && ($ = u(() => /* @__PURE__ */ se(yt)), r.set(P + "", $));
          }
        if (b === void 0)
          (!y || Gr(d, v)?.writable) && (b = u(() => /* @__PURE__ */ se(void 0)), h(b, Wt(_)), r.set(v, b));
        else {
          y = b.v !== yt;
          var N = u(() => Wt(_));
          h(b, N);
        }
        var C = Reflect.getOwnPropertyDescriptor(d, v);
        if (C?.set && C.set.call(x, _), !y) {
          if (a && typeof v == "string") {
            var ee = (
              /** @type {Source<number>} */
              r.get("length")
            ), te = Number(v);
            Number.isInteger(te) && te >= ee.v && h(ee, te + 1);
          }
          ps(o);
        }
        return !0;
      },
      ownKeys(d) {
        s(o);
        var v = Reflect.ownKeys(d).filter((b) => {
          var y = r.get(b);
          return y === void 0 || y.v !== yt;
        });
        for (var [_, x] of r)
          x.v !== yt && !(_ in d) && v.push(_);
        return v;
      },
      setPrototypeOf() {
        _o();
      }
    }
  );
}
function rn(e) {
  try {
    if (e !== null && typeof e == "object" && wr in e)
      return e[wr];
  } catch {
  }
  return e;
}
function Oo(e, t) {
  return Object.is(rn(e), rn(t));
}
var sn, qn, Un, zn;
function fa() {
  if (sn === void 0) {
    sn = window, qn = /Firefox/.test(navigator.userAgent);
    var e = Element.prototype, t = Node.prototype, r = Text.prototype;
    Un = Gr(t, "firstChild").get, zn = Gr(t, "nextSibling").get, Za(e) && (e.__click = void 0, e.__className = void 0, e.__attributes = null, e.__style = void 0, e.__e = void 0), Za(r) && (r.__t = void 0);
  }
}
function zt(e = "") {
  return document.createTextNode(e);
}
// @__NO_SIDE_EFFECTS__
function qt(e) {
  return (
    /** @type {TemplateNode | null} */
    Un.call(e)
  );
}
// @__NO_SIDE_EFFECTS__
function rr(e) {
  return (
    /** @type {TemplateNode | null} */
    zn.call(e)
  );
}
function i(e, t) {
  if (!Xe)
    return /* @__PURE__ */ qt(e);
  var r = /* @__PURE__ */ qt(rt);
  if (r === null)
    r = rt.appendChild(zt());
  else if (t && r.nodeType !== Gs) {
    var a = zt();
    return r?.before(a), Ct(a), a;
  }
  return t && Ra(
    /** @type {Text} */
    r
  ), Ct(r), r;
}
function it(e, t = !1) {
  if (!Xe) {
    var r = /* @__PURE__ */ qt(e);
    return r instanceof Comment && r.data === "" ? /* @__PURE__ */ rr(r) : r;
  }
  if (t) {
    if (rt?.nodeType !== Gs) {
      var a = zt();
      return rt?.before(a), Ct(a), a;
    }
    Ra(
      /** @type {Text} */
      rt
    );
  }
  return rt;
}
function l(e, t = 1, r = !1) {
  let a = Xe ? rt : e;
  for (var o; t--; )
    o = a, a = /** @type {TemplateNode} */
    /* @__PURE__ */ rr(a);
  if (!Xe)
    return a;
  if (r) {
    if (a?.nodeType !== Gs) {
      var c = zt();
      return a === null ? o?.after(c) : a.before(c), Ct(c), c;
    }
    Ra(
      /** @type {Text} */
      a
    );
  }
  return Ct(a), a;
}
function La(e) {
  e.textContent = "";
}
function Gn() {
  return !1;
}
function Ra(e) {
  if (
    /** @type {string} */
    e.nodeValue.length < 65536
  )
    return;
  let t = e.nextSibling;
  for (; t !== null && t.nodeType === Gs; )
    t.remove(), e.nodeValue += /** @type {string} */
    t.nodeValue, t = e.nextSibling;
}
function Fa(e) {
  Xe && /* @__PURE__ */ qt(e) !== null && La(e);
}
let an = !1;
function Yn() {
  an || (an = !0, document.addEventListener(
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
function Ks(e) {
  var t = tt, r = st;
  Yt(null), cr(null);
  try {
    return e();
  } finally {
    Yt(t), cr(r);
  }
}
function Kn(e, t, r, a = r) {
  e.addEventListener(t, () => Ks(r));
  const o = e.__on_r;
  o ? e.__on_r = () => {
    o(), a(!0);
  } : e.__on_r = () => a(!0), Yn();
}
function Jn(e) {
  st === null && (tt === null && vo(), uo()), Er && lo();
}
function Lo(e, t) {
  var r = t.last;
  r === null ? t.last = t.first = e : (r.next = e, e.prev = r, t.last = e);
}
function sr(e, t, r) {
  var a = st;
  a !== null && (a.f & Lt) !== 0 && (e |= Lt);
  var o = {
    ctx: dt,
    deps: null,
    nodes: null,
    f: e | kt | Ut,
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
      ys(o), o.f |= zs;
    } catch (d) {
      throw St(o), d;
    }
  else t !== null && Xt(o);
  var c = o;
  if (r && c.deps === null && c.teardown === null && c.nodes === null && c.first === c.last && // either `null`, or a singular child
  (c.f & Br) === 0 && (c = c.first, (e & xr) !== 0 && (e & Xr) !== 0 && c !== null && (c.f |= Xr)), c !== null && (c.parent = a, a !== null && Lo(c, a), tt !== null && (tt.f & $t) !== 0 && (e & Cr) === 0)) {
    var u = (
      /** @type {Derived} */
      tt
    );
    (u.effects ??= []).push(c);
  }
  return o;
}
function Va() {
  return tt !== null && !Zt;
}
function Js(e) {
  const t = sr($s, null, !1);
  return vt(t, gt), t.teardown = e, t;
}
function Nt(e) {
  Jn();
  var t = (
    /** @type {Effect} */
    st.f
  ), r = !tt && (t & tr) !== 0 && (t & zs) === 0;
  if (r) {
    var a = (
      /** @type {ComponentContext} */
      dt
    );
    (a.e ??= []).push(e);
  } else
    return Qn(e);
}
function Qn(e) {
  return sr(Ls | Cn, e, !1);
}
function Ro(e) {
  return Jn(), sr($s | Cn, e, !0);
}
function Fo(e) {
  hr.ensure();
  const t = sr(Cr | Br, e, !0);
  return () => {
    St(t);
  };
}
function Vo(e) {
  hr.ensure();
  const t = sr(Cr | Br, e, !0);
  return (r = {}) => new Promise((a) => {
    r.outro ? Mr(t, () => {
      St(t), a(void 0);
    }) : (St(t), a(void 0));
  });
}
function Xn(e) {
  return sr(Ls, e, !1);
}
function jo(e) {
  return sr(Ma | Br, e, !0);
}
function Qs(e, t = 0) {
  return sr($s | t, e, !0);
}
function D(e, t = [], r = [], a = []) {
  Po(a, t, r, (o) => {
    sr($s, () => e(...o.map(s)), !0);
  });
}
function ja(e, t = 0) {
  var r = sr(xr | t, e, !0);
  return r;
}
function Ht(e) {
  return sr(tr | Br, e, !0);
}
function Zn(e) {
  var t = e.teardown;
  if (t !== null) {
    const r = Er, a = tt;
    nn(!0), Yt(null);
    try {
      t.call(null);
    } finally {
      nn(r), Yt(a);
    }
  }
}
function ei(e, t = !1) {
  var r = e.first;
  for (e.first = e.last = null; r !== null; ) {
    const o = r.ac;
    o !== null && Ks(() => {
      o.abort(zr);
    });
    var a = r.next;
    (r.f & Cr) !== 0 ? r.parent = null : St(r, t), r = a;
  }
}
function Bo(e) {
  for (var t = e.first; t !== null; ) {
    var r = t.next;
    (t.f & tr) === 0 && St(t), t = r;
  }
}
function St(e, t = !0) {
  var r = !1;
  (t || (e.f & En) !== 0) && e.nodes !== null && e.nodes.end !== null && (ti(
    e.nodes.start,
    /** @type {TemplateNode} */
    e.nodes.end
  ), r = !0), ei(e, t && !r), js(e, 0), vt(e, pr);
  var a = e.nodes && e.nodes.t;
  if (a !== null)
    for (const c of a)
      c.stop();
  Zn(e);
  var o = e.parent;
  o !== null && o.first !== null && ri(e), e.next = e.prev = e.teardown = e.ctx = e.deps = e.fn = e.nodes = e.ac = null;
}
function ti(e, t) {
  for (; e !== null; ) {
    var r = e === t ? null : /* @__PURE__ */ rr(e);
    e.remove(), e = r;
  }
}
function ri(e) {
  var t = e.parent, r = e.prev, a = e.next;
  r !== null && (r.next = a), a !== null && (a.prev = r), t !== null && (t.first === e && (t.first = a), t.last === e && (t.last = r));
}
function Mr(e, t, r = !0) {
  var a = [];
  si(e, a, !0);
  var o = () => {
    r && St(e), t && t();
  }, c = a.length;
  if (c > 0) {
    var u = () => --c || o();
    for (var d of a)
      d.out(u);
  } else
    o();
}
function si(e, t, r) {
  if ((e.f & Lt) === 0) {
    e.f ^= Lt;
    var a = e.nodes && e.nodes.t;
    if (a !== null)
      for (const d of a)
        (d.is_global || r) && t.push(d);
    for (var o = e.first; o !== null; ) {
      var c = o.next, u = (o.f & Xr) !== 0 || // If this is a branch effect without a block effect parent,
      // it means the parent block effect was pruned. In that case,
      // transparency information was transferred to the branch effect.
      (o.f & tr) !== 0 && (e.f & xr) !== 0;
      si(o, t, u ? r : !1), o = c;
    }
  }
}
function Ba(e) {
  ai(e, !0);
}
function ai(e, t) {
  if ((e.f & Lt) !== 0) {
    e.f ^= Lt, (e.f & gt) === 0 && (vt(e, kt), Xt(e));
    for (var r = e.first; r !== null; ) {
      var a = r.next, o = (r.f & Xr) !== 0 || (r.f & tr) !== 0;
      ai(r, o ? t : !1), r = a;
    }
    var c = e.nodes && e.nodes.t;
    if (c !== null)
      for (const u of c)
        (u.is_global || t) && u.in();
  }
}
function ni(e, t) {
  if (e.nodes)
    for (var r = e.nodes.start, a = e.nodes.end; r !== null; ) {
      var o = r === a ? null : /* @__PURE__ */ rr(r);
      t.append(r), r = o;
    }
}
let Ts = !1, Er = !1;
function nn(e) {
  Er = e;
}
let tt = null, Zt = !1;
function Yt(e) {
  tt = e;
}
let st = null;
function cr(e) {
  st = e;
}
let Gt = null;
function ii(e) {
  tt !== null && (Gt === null ? Gt = [e] : Gt.push(e));
}
let Pt = null, It = 0, Bt = null;
function Ho(e) {
  Bt = e;
}
let oi = 1, Dr = 0, Ir = Dr;
function on(e) {
  Ir = e;
}
function ci() {
  return ++oi;
}
function Ss(e) {
  var t = e.f;
  if ((t & kt) !== 0)
    return !0;
  if (t & $t && (e.f &= ~Rr), (t & er) !== 0) {
    for (var r = (
      /** @type {Value[]} */
      e.deps
    ), a = r.length, o = 0; o < a; o++) {
      var c = r[o];
      if (Ss(
        /** @type {Derived} */
        c
      ) && Bn(
        /** @type {Derived} */
        c
      ), c.wv > e.wv)
        return !0;
    }
    (t & Ut) !== 0 && // During time traveling we don't want to reset the status so that
    // traversal of the graph in the other batches still happens
    Qt === null && vt(e, gt);
  }
  return !1;
}
function li(e, t, r = !0) {
  var a = e.reactions;
  if (a !== null && !(Gt !== null && Qr.call(Gt, e)))
    for (var o = 0; o < a.length; o++) {
      var c = a[o];
      (c.f & $t) !== 0 ? li(
        /** @type {Derived} */
        c,
        t,
        !1
      ) : t === c && (r ? vt(c, kt) : (c.f & gt) !== 0 && vt(c, er), Xt(
        /** @type {Effect} */
        c
      ));
    }
}
function ui(e) {
  var t = Pt, r = It, a = Bt, o = tt, c = Gt, u = dt, d = Zt, v = Ir, _ = e.f;
  Pt = /** @type {null | Value[]} */
  null, It = 0, Bt = null, tt = (_ & (tr | Cr)) === 0 ? e : null, Gt = null, es(e.ctx), Zt = !1, Ir = ++Dr, e.ac !== null && (Ks(() => {
    e.ac.abort(zr);
  }), e.ac = null);
  try {
    e.f |= la;
    var x = (
      /** @type {Function} */
      e.fn
    ), b = x(), y = e.deps, P = at?.is_fork;
    if (Pt !== null) {
      var $;
      if (P || js(e, It), y !== null && It > 0)
        for (y.length = It + Pt.length, $ = 0; $ < Pt.length; $++)
          y[It + $] = Pt[$];
      else
        e.deps = y = Pt;
      if (Va() && (e.f & Ut) !== 0)
        for ($ = It; $ < y.length; $++)
          (y[$].reactions ??= []).push(e);
    } else !P && y !== null && It < y.length && (js(e, It), y.length = It);
    if (Es() && Bt !== null && !Zt && y !== null && (e.f & ($t | er | kt)) === 0)
      for ($ = 0; $ < /** @type {Source[]} */
      Bt.length; $++)
        li(
          Bt[$],
          /** @type {Effect} */
          e
        );
    if (o !== null && o !== e) {
      if (Dr++, o.deps !== null)
        for (let N = 0; N < r; N += 1)
          o.deps[N].rv = Dr;
      if (t !== null)
        for (const N of t)
          N.rv = Dr;
      Bt !== null && (a === null ? a = Bt : a.push(.../** @type {Source[]} */
      Bt));
    }
    return (e.f & yr) !== 0 && (e.f ^= yr), b;
  } catch (N) {
    return In(N);
  } finally {
    e.f ^= la, Pt = t, It = r, Bt = a, tt = o, Gt = c, es(u), Zt = d, Ir = v;
  }
}
function Wo(e, t) {
  let r = t.reactions;
  if (r !== null) {
    var a = to.call(r, e);
    if (a !== -1) {
      var o = r.length - 1;
      o === 0 ? r = t.reactions = null : (r[a] = r[o], r.pop());
    }
  }
  if (r === null && (t.f & $t) !== 0 && // Destroying a child effect while updating a parent effect can cause a dependency to appear
  // to be unused, when in fact it is used by the currently-updating parent. Checking `new_deps`
  // allows us to skip the expensive work of disconnecting and immediately reconnecting it
  (Pt === null || !Qr.call(Pt, t))) {
    var c = (
      /** @type {Derived} */
      t
    );
    (c.f & Ut) !== 0 && (c.f ^= Ut, c.f &= ~Rr), Ia(c), jn(c), js(c, 0);
  }
}
function js(e, t) {
  var r = e.deps;
  if (r !== null)
    for (var a = t; a < r.length; a++)
      Wo(e, r[a]);
}
function ys(e) {
  var t = e.f;
  if ((t & pr) === 0) {
    vt(e, gt);
    var r = st, a = Ts;
    st = e, Ts = !0;
    try {
      (t & (xr | kn)) !== 0 ? Bo(e) : ei(e), Zn(e);
      var o = ui(e);
      e.teardown = typeof o == "function" ? o : null, e.wv = oi;
      var c;
      oa && yo && (e.f & kt) !== 0 && e.deps;
    } finally {
      Ts = a, st = r;
    }
  }
}
async function Ha() {
  await Promise.resolve(), pt();
}
function s(e) {
  var t = e.f, r = (t & $t) !== 0;
  if (tt !== null && !Zt) {
    var a = st !== null && (st.f & pr) !== 0;
    if (!a && (Gt === null || !Qr.call(Gt, e))) {
      var o = tt.deps;
      if ((tt.f & la) !== 0)
        e.rv < Dr && (e.rv = Dr, Pt === null && o !== null && o[It] === e ? It++ : Pt === null ? Pt = [e] : Pt.push(e));
      else {
        (tt.deps ??= []).push(e);
        var c = e.reactions;
        c === null ? e.reactions = [tt] : Qr.call(c, tt) || c.push(tt);
      }
    }
  }
  if (Er && $r.has(e))
    return $r.get(e);
  if (r) {
    var u = (
      /** @type {Derived} */
      e
    );
    if (Er) {
      var d = u.v;
      return ((u.f & gt) === 0 && u.reactions !== null || vi(u)) && (d = Na(u)), $r.set(u, d), d;
    }
    var v = (u.f & Ut) === 0 && !Zt && tt !== null && (Ts || (tt.f & Ut) !== 0), _ = u.deps === null;
    Ss(u) && (v && (u.f |= Ut), Bn(u)), v && !_ && di(u);
  }
  if (Qt?.has(e))
    return Qt.get(e);
  if ((e.f & yr) !== 0)
    throw e.v;
  return e.v;
}
function di(e) {
  if (e.deps !== null) {
    e.f |= Ut;
    for (const t of e.deps)
      (t.reactions ??= []).push(e), (t.f & $t) !== 0 && (t.f & Ut) === 0 && di(
        /** @type {Derived} */
        t
      );
  }
}
function vi(e) {
  if (e.v === yt) return !0;
  if (e.deps === null) return !1;
  for (const t of e.deps)
    if ($r.has(t) || (t.f & $t) !== 0 && vi(
      /** @type {Derived} */
      t
    ))
      return !0;
  return !1;
}
function Wr(e) {
  var t = Zt;
  try {
    return Zt = !0, e();
  } finally {
    Zt = t;
  }
}
function qo(e) {
  if (!(typeof e != "object" || !e || e instanceof EventTarget)) {
    if (wr in e)
      pa(e);
    else if (!Array.isArray(e))
      for (let t in e) {
        const r = e[t];
        typeof r == "object" && r && wr in r && pa(r);
      }
  }
}
function pa(e, t = /* @__PURE__ */ new Set()) {
  if (typeof e == "object" && e !== null && // We don't want to traverse DOM elements
  !(e instanceof EventTarget) && !t.has(e)) {
    t.add(e), e instanceof Date && e.getTime();
    for (let a in e)
      try {
        pa(e[a], t);
      } catch {
      }
    const r = Pa(e);
    if (r !== Object.prototype && r !== Array.prototype && r !== Map.prototype && r !== Set.prototype && r !== Date.prototype) {
      const a = wn(r);
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
const fi = /* @__PURE__ */ new Set(), ha = /* @__PURE__ */ new Set();
function Uo(e, t, r, a = {}) {
  function o(c) {
    if (a.capture || ds.call(t, c), !c.cancelBubble)
      return Ks(() => r?.call(this, c));
  }
  return e.startsWith("pointer") || e.startsWith("touch") || e === "wheel" ? or(() => {
    t.addEventListener(e, o, a);
  }) : t.addEventListener(e, o, a), o;
}
function mt(e, t, r, a, o) {
  var c = { capture: a, passive: o }, u = Uo(e, t, r, c);
  (t === document.body || // @ts-ignore
  t === window || // @ts-ignore
  t === document || // Firefox has quirky behavior, it can happen that we still get "canplay" events when the element is already removed
  t instanceof HTMLMediaElement) && Js(() => {
    t.removeEventListener(e, u, c);
  });
}
function Ft(e) {
  for (var t = 0; t < e.length; t++)
    fi.add(e[t]);
  for (var r of ha)
    r(e);
}
let cn = null;
function ds(e) {
  var t = this, r = (
    /** @type {Node} */
    t.ownerDocument
  ), a = e.type, o = e.composedPath?.() || [], c = (
    /** @type {null | Element} */
    o[0] || e.target
  );
  cn = e;
  var u = 0, d = cn === e && e.__root;
  if (d) {
    var v = o.indexOf(d);
    if (v !== -1 && (t === document || t === /** @type {any} */
    window)) {
      e.__root = t;
      return;
    }
    var _ = o.indexOf(t);
    if (_ === -1)
      return;
    v <= _ && (u = v);
  }
  if (c = /** @type {Element} */
  o[u] || e.target, c !== t) {
    ms(e, "currentTarget", {
      configurable: !0,
      get() {
        return c || r;
      }
    });
    var x = tt, b = st;
    Yt(null), cr(null);
    try {
      for (var y, P = []; c !== null; ) {
        var $ = c.assignedSlot || c.parentNode || /** @type {any} */
        c.host || null;
        try {
          var N = c["__" + a];
          N != null && (!/** @type {any} */
          c.disabled || // DOM could've been updated already by the time this is reached, so we check this as well
          // -> the target could not have been disabled because it emits the event in the first place
          e.target === c) && N.call(c, e);
        } catch (C) {
          y ? P.push(C) : y = C;
        }
        if (e.cancelBubble || $ === t || $ === null)
          break;
        c = $;
      }
      if (y) {
        for (let C of P)
          queueMicrotask(() => {
            throw C;
          });
        throw y;
      }
    } finally {
      e.__root = t, delete e.currentTarget, Yt(x), cr(b);
    }
  }
}
function pi(e) {
  var t = document.createElement("template");
  return t.innerHTML = e.replaceAll("<!>", "<!---->"), t.content;
}
function kr(e, t) {
  var r = (
    /** @type {Effect} */
    st
  );
  r.nodes === null && (r.nodes = { start: e, end: t, a: null, t: null });
}
// @__NO_SIDE_EFFECTS__
function p(e, t) {
  var r = (t & Xi) !== 0, a = (t & Zi) !== 0, o, c = !e.startsWith("<!>");
  return () => {
    if (Xe)
      return kr(rt, null), rt;
    o === void 0 && (o = pi(c ? e : "<!>" + e), r || (o = /** @type {TemplateNode} */
    /* @__PURE__ */ qt(o)));
    var u = (
      /** @type {TemplateNode} */
      a || qn ? document.importNode(o, !0) : o.cloneNode(!0)
    );
    if (r) {
      var d = (
        /** @type {TemplateNode} */
        /* @__PURE__ */ qt(u)
      ), v = (
        /** @type {TemplateNode} */
        u.lastChild
      );
      kr(d, v);
    } else
      kr(u, u);
    return u;
  };
}
function Nr() {
  if (Xe)
    return kr(rt, null), rt;
  var e = document.createDocumentFragment(), t = document.createComment(""), r = zt();
  return e.append(t, r), kr(t, r), e;
}
function f(e, t) {
  if (Xe) {
    var r = (
      /** @type {Effect & { nodes: EffectNodes }} */
      st
    );
    ((r.f & zs) === 0 || r.nodes.end === null) && (r.nodes.end = rt), Zr();
    return;
  }
  e !== null && e.before(
    /** @type {Node} */
    t
  );
}
const zo = ["touchstart", "touchmove"];
function Go(e) {
  return zo.includes(e);
}
function E(e, t) {
  var r = t == null ? "" : typeof t == "object" ? t + "" : t;
  r !== (e.__t ??= e.nodeValue) && (e.__t = r, e.nodeValue = r + "");
}
function hi(e, t) {
  return _i(e, t);
}
function Yo(e, t) {
  fa(), t.intro = t.intro ?? !1;
  const r = t.target, a = Xe, o = rt;
  try {
    for (var c = /* @__PURE__ */ qt(r); c && (c.nodeType !== Hr || /** @type {Comment} */
    c.data !== Sa); )
      c = /* @__PURE__ */ rr(c);
    if (!c)
      throw Lr;
    vr(!0), Ct(
      /** @type {Comment} */
      c
    );
    const u = _i(e, { ...t, anchor: c });
    return vr(!1), /**  @type {Exports} */
    u;
  } catch (u) {
    if (u instanceof Error && u.message.split(`
`).some((d) => d.startsWith("https://svelte.dev/e/")))
      throw u;
    return u !== Lr && console.warn("Failed to hydrate: ", u), t.recover === !1 && po(), fa(), La(r), vr(!1), hi(e, t);
  } finally {
    vr(a), Ct(o);
  }
}
const qr = /* @__PURE__ */ new Map();
function _i(e, { target: t, anchor: r, props: a = {}, events: o, context: c, intro: u = !0 }) {
  fa();
  var d = /* @__PURE__ */ new Set(), v = (b) => {
    for (var y = 0; y < b.length; y++) {
      var P = b[y];
      if (!d.has(P)) {
        d.add(P);
        var $ = Go(P);
        t.addEventListener(P, ds, { passive: $ });
        var N = qr.get(P);
        N === void 0 ? (document.addEventListener(P, ds, { passive: $ }), qr.set(P, 1)) : qr.set(P, N + 1);
      }
    }
  };
  v(Us(fi)), ha.add(v);
  var _ = void 0, x = Vo(() => {
    var b = r ?? t.appendChild(zt());
    return Ao(
      /** @type {TemplateNode} */
      b,
      {
        pending: () => {
        }
      },
      (y) => {
        Tt({});
        var P = (
          /** @type {ComponentContext} */
          dt
        );
        if (c && (P.c = c), o && (a.$$events = o), Xe && kr(
          /** @type {TemplateNode} */
          y,
          null
        ), _ = e(y, a) || {}, Xe && (st.nodes.end = rt, rt === null || rt.nodeType !== Hr || /** @type {Comment} */
        rt.data !== Aa))
          throw ks(), Lr;
        Mt();
      }
    ), () => {
      for (var y of d) {
        t.removeEventListener(y, ds);
        var P = (
          /** @type {number} */
          qr.get(y)
        );
        --P === 0 ? (document.removeEventListener(y, ds), qr.delete(y)) : qr.set(y, P);
      }
      ha.delete(v), b !== r && b.parentNode?.removeChild(b);
    };
  });
  return _a.set(_, x), _;
}
let _a = /* @__PURE__ */ new WeakMap();
function Ko(e, t) {
  const r = _a.get(e);
  return r ? (_a.delete(e), r(t)) : Promise.resolve();
}
class Jo {
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
      at
    );
    if (this.#e.has(t)) {
      var r = (
        /** @type {Key} */
        this.#e.get(t)
      ), a = this.#t.get(r);
      if (a)
        Ba(a), this.#i.delete(r);
      else {
        var o = this.#r.get(r);
        o && (this.#t.set(r, o.effect), this.#r.delete(r), o.fragment.lastChild.remove(), this.anchor.before(o.fragment), a = o.effect);
      }
      for (const [c, u] of this.#e) {
        if (this.#e.delete(c), c === t)
          break;
        const d = this.#r.get(u);
        d && (St(d.effect), this.#r.delete(u));
      }
      for (const [c, u] of this.#t) {
        if (c === r || this.#i.has(c)) continue;
        const d = () => {
          if (Array.from(this.#e.values()).includes(c)) {
            var _ = document.createDocumentFragment();
            ni(u, _), _.append(zt()), this.#r.set(c, { effect: u, fragment: _ });
          } else
            St(u);
          this.#i.delete(c), this.#t.delete(c);
        };
        this.#o || !a ? (this.#i.add(c), Mr(u, d, !1)) : d();
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
      r.includes(a) || (St(o.effect), this.#r.delete(a));
  };
  /**
   *
   * @param {any} key
   * @param {null | ((target: TemplateNode) => void)} fn
   */
  ensure(t, r) {
    var a = (
      /** @type {Batch} */
      at
    ), o = Gn();
    if (r && !this.#t.has(t) && !this.#r.has(t))
      if (o) {
        var c = document.createDocumentFragment(), u = zt();
        c.append(u), this.#r.set(t, {
          effect: Ht(() => r(u)),
          fragment: c
        });
      } else
        this.#t.set(
          t,
          Ht(() => r(this.anchor))
        );
    if (this.#e.set(a, t), o) {
      for (const [d, v] of this.#t)
        d === t ? a.unskip_effect(v) : a.skip_effect(v);
      for (const [d, v] of this.#r)
        d === t ? a.unskip_effect(v.effect) : a.skip_effect(v.effect);
      a.oncommit(this.#a), a.ondiscard(this.#s);
    } else
      Xe && (this.anchor = rt), this.#a();
  }
}
function xi(e) {
  dt === null && Sn(), as && dt.l !== null ? Xo(dt).m.push(e) : Nt(() => {
    const t = Wr(e);
    if (typeof t == "function") return (
      /** @type {() => void} */
      t
    );
  });
}
function Qo(e) {
  dt === null && Sn(), xi(() => () => Wr(e));
}
function Xo(e) {
  var t = (
    /** @type {ComponentContextLegacy} */
    e.l
  );
  return t.u ??= { a: [], b: [], m: [] };
}
function I(e, t, r = !1) {
  Xe && Zr();
  var a = new Jo(e), o = r ? Xr : 0;
  function c(u, d) {
    if (Xe) {
      const x = An(e);
      var v;
      if (x === Sa ? v = 0 : x === qs ? v = !1 : v = parseInt(x.substring(1)), u !== v) {
        var _ = Rs();
        Ct(_), a.anchor = _, vr(!1), a.ensure(u, d), vr(!0);
        return;
      }
    }
    a.ensure(u, d);
  }
  ja(() => {
    var u = !1;
    t((d, v = 0) => {
      u = !0, c(v, d);
    }), u || c(!1, null);
  }, o);
}
function ot(e, t) {
  return t;
}
function Zo(e, t, r) {
  for (var a = [], o = t.length, c, u = t.length, d = 0; d < o; d++) {
    let b = t[d];
    Mr(
      b,
      () => {
        if (c) {
          if (c.pending.delete(b), c.done.add(b), c.pending.size === 0) {
            var y = (
              /** @type {Set<EachOutroGroup>} */
              e.outrogroups
            );
            xa(Us(c.done)), y.delete(c), y.size === 0 && (e.outrogroups = null);
          }
        } else
          u -= 1;
      },
      !1
    );
  }
  if (u === 0) {
    var v = a.length === 0 && r !== null;
    if (v) {
      var _ = (
        /** @type {Element} */
        r
      ), x = (
        /** @type {Element} */
        _.parentNode
      );
      La(x), x.append(_), e.items.clear();
    }
    xa(t, !v);
  } else
    c = {
      pending: new Set(t),
      done: /* @__PURE__ */ new Set()
    }, (e.outrogroups ??= /* @__PURE__ */ new Set()).add(c);
}
function xa(e, t = !0) {
  for (var r = 0; r < e.length; r++)
    St(e[r], t);
}
var ln;
function We(e, t, r, a, o, c = null) {
  var u = e, d = /* @__PURE__ */ new Map(), v = (t & yn) !== 0;
  if (v) {
    var _ = (
      /** @type {Element} */
      e
    );
    u = Xe ? Ct(/* @__PURE__ */ qt(_)) : _.appendChild(zt());
  }
  Xe && Zr();
  var x = null, b = /* @__PURE__ */ Vs(() => {
    var ee = r();
    return Da(ee) ? ee : ee == null ? [] : Us(ee);
  }), y, P = !0;
  function $() {
    C.fallback = x, ec(C, y, u, t, a), x !== null && (y.length === 0 ? (x.f & dr) === 0 ? Ba(x) : (x.f ^= dr, vs(x, null, u)) : Mr(x, () => {
      x = null;
    }));
  }
  var N = ja(() => {
    y = /** @type {V[]} */
    s(b);
    var ee = y.length;
    let te = !1;
    if (Xe) {
      var $e = An(u) === qs;
      $e !== (ee === 0) && (u = Rs(), Ct(u), vr(!1), te = !0);
    }
    for (var he = /* @__PURE__ */ new Set(), Te = (
      /** @type {Batch} */
      at
    ), Q = Gn(), Ie = 0; Ie < ee; Ie += 1) {
      Xe && rt.nodeType === Hr && /** @type {Comment} */
      rt.data === Aa && (u = /** @type {Comment} */
      rt, te = !0, vr(!1));
      var _e = y[Ie], Je = a(_e, Ie), xe = P ? null : d.get(Je);
      xe ? (xe.v && rs(xe.v, _e), xe.i && rs(xe.i, Ie), Q && Te.unskip_effect(xe.e)) : (xe = tc(
        d,
        P ? u : ln ??= zt(),
        _e,
        Je,
        Ie,
        o,
        t,
        r
      ), P || (xe.e.f |= dr), d.set(Je, xe)), he.add(Je);
    }
    if (ee === 0 && c && !x && (P ? x = Ht(() => c(u)) : (x = Ht(() => c(ln ??= zt())), x.f |= dr)), ee > he.size && co(), Xe && ee > 0 && Ct(Rs()), !P)
      if (Q) {
        for (const [qe, Ue] of d)
          he.has(qe) || Te.skip_effect(Ue.e);
        Te.oncommit($), Te.ondiscard(() => {
        });
      } else
        $();
    te && vr(!0), s(b);
  }), C = { effect: N, items: d, outrogroups: null, fallback: x };
  P = !1, Xe && (u = rt);
}
function ls(e) {
  for (; e !== null && (e.f & tr) === 0; )
    e = e.next;
  return e;
}
function ec(e, t, r, a, o) {
  var c = (a & Gi) !== 0, u = t.length, d = e.items, v = ls(e.effect.first), _, x = null, b, y = [], P = [], $, N, C, ee;
  if (c)
    for (ee = 0; ee < u; ee += 1)
      $ = t[ee], N = o($, ee), C = /** @type {EachItem} */
      d.get(N).e, (C.f & dr) === 0 && (C.nodes?.a?.measure(), (b ??= /* @__PURE__ */ new Set()).add(C));
  for (ee = 0; ee < u; ee += 1) {
    if ($ = t[ee], N = o($, ee), C = /** @type {EachItem} */
    d.get(N).e, e.outrogroups !== null)
      for (const xe of e.outrogroups)
        xe.pending.delete(C), xe.done.delete(C);
    if ((C.f & dr) !== 0)
      if (C.f ^= dr, C === v)
        vs(C, null, r);
      else {
        var te = x ? x.next : v;
        C === e.effect.last && (e.effect.last = C.prev), C.prev && (C.prev.next = C.next), C.next && (C.next.prev = C.prev), br(e, x, C), br(e, C, te), vs(C, te, r), x = C, y = [], P = [], v = ls(x.next);
        continue;
      }
    if ((C.f & Lt) !== 0 && (Ba(C), c && (C.nodes?.a?.unfix(), (b ??= /* @__PURE__ */ new Set()).delete(C))), C !== v) {
      if (_ !== void 0 && _.has(C)) {
        if (y.length < P.length) {
          var $e = P[0], he;
          x = $e.prev;
          var Te = y[0], Q = y[y.length - 1];
          for (he = 0; he < y.length; he += 1)
            vs(y[he], $e, r);
          for (he = 0; he < P.length; he += 1)
            _.delete(P[he]);
          br(e, Te.prev, Q.next), br(e, x, Te), br(e, Q, $e), v = $e, x = Q, ee -= 1, y = [], P = [];
        } else
          _.delete(C), vs(C, v, r), br(e, C.prev, C.next), br(e, C, x === null ? e.effect.first : x.next), br(e, x, C), x = C;
        continue;
      }
      for (y = [], P = []; v !== null && v !== C; )
        (_ ??= /* @__PURE__ */ new Set()).add(v), P.push(v), v = ls(v.next);
      if (v === null)
        continue;
    }
    (C.f & dr) === 0 && y.push(C), x = C, v = ls(C.next);
  }
  if (e.outrogroups !== null) {
    for (const xe of e.outrogroups)
      xe.pending.size === 0 && (xa(Us(xe.done)), e.outrogroups?.delete(xe));
    e.outrogroups.size === 0 && (e.outrogroups = null);
  }
  if (v !== null || _ !== void 0) {
    var Ie = [];
    if (_ !== void 0)
      for (C of _)
        (C.f & Lt) === 0 && Ie.push(C);
    for (; v !== null; )
      (v.f & Lt) === 0 && v !== e.fallback && Ie.push(v), v = ls(v.next);
    var _e = Ie.length;
    if (_e > 0) {
      var Je = (a & yn) !== 0 && u === 0 ? r : null;
      if (c) {
        for (ee = 0; ee < _e; ee += 1)
          Ie[ee].nodes?.a?.measure();
        for (ee = 0; ee < _e; ee += 1)
          Ie[ee].nodes?.a?.fix();
      }
      Zo(e, Ie, Je);
    }
  }
  c && or(() => {
    if (b !== void 0)
      for (C of b)
        C.nodes?.a?.apply();
  });
}
function tc(e, t, r, a, o, c, u, d) {
  var v = (u & Ui) !== 0 ? (u & Yi) === 0 ? /* @__PURE__ */ Oa(r, !1, !1) : Fr(r) : null, _ = (u & zi) !== 0 ? Fr(o) : null;
  return {
    v,
    i: _,
    e: Ht(() => (c(t, v ?? r, _ ?? o, d), () => {
      e.delete(a);
    }))
  };
}
function vs(e, t, r) {
  if (e.nodes)
    for (var a = e.nodes.start, o = e.nodes.end, c = t && (t.f & dr) === 0 ? (
      /** @type {EffectNodes} */
      t.nodes.start
    ) : r; a !== null; ) {
      var u = (
        /** @type {TemplateNode} */
        /* @__PURE__ */ rr(a)
      );
      if (c.before(a), a === o)
        return;
      a = u;
    }
}
function br(e, t, r) {
  t === null ? e.effect.first = r : t.next = r, r === null ? e.effect.last = t : r.prev = t;
}
function rc(e, t, r = !1, a = !1, o = !1) {
  var c = e, u = "";
  D(() => {
    var d = (
      /** @type {Effect} */
      st
    );
    if (u === (u = t() ?? "")) {
      Xe && Zr();
      return;
    }
    if (d.nodes !== null && (ti(
      d.nodes.start,
      /** @type {TemplateNode} */
      d.nodes.end
    ), d.nodes = null), u !== "") {
      if (Xe) {
        rt.data;
        for (var v = Zr(), _ = v; v !== null && (v.nodeType !== Hr || /** @type {Comment} */
        v.data !== ""); )
          _ = v, v = /* @__PURE__ */ rr(v);
        if (v === null)
          throw ks(), Lr;
        kr(rt, _), c = Ct(v);
        return;
      }
      var x = u + "";
      r ? x = `<svg>${x}</svg>` : a && (x = `<math>${x}</math>`);
      var b = pi(x);
      if ((r || a) && (b = /** @type {Element} */
      /* @__PURE__ */ qt(b)), kr(
        /** @type {TemplateNode} */
        /* @__PURE__ */ qt(b),
        /** @type {TemplateNode} */
        b.lastChild
      ), r || a)
        for (; /* @__PURE__ */ qt(b); )
          c.before(
            /** @type {TemplateNode} */
            /* @__PURE__ */ qt(b)
          );
      else
        c.before(b);
    }
  });
}
function gi(e) {
  var t, r, a = "";
  if (typeof e == "string" || typeof e == "number") a += e;
  else if (typeof e == "object") if (Array.isArray(e)) {
    var o = e.length;
    for (t = 0; t < o; t++) e[t] && (r = gi(e[t])) && (a && (a += " "), a += r);
  } else for (r in e) e[r] && (a && (a += " "), a += r);
  return a;
}
function sc() {
  for (var e, t, r = 0, a = "", o = arguments.length; r < o; r++) (e = arguments[r]) && (t = gi(e)) && (a && (a += " "), a += t);
  return a;
}
function ac(e) {
  return typeof e == "object" ? sc(e) : e ?? "";
}
function nc(e, t, r) {
  var a = e == null ? "" : "" + e;
  return a === "" ? null : a;
}
function ic(e, t) {
  return e == null ? null : String(e);
}
function Fe(e, t, r, a, o, c) {
  var u = e.__className;
  if (Xe || u !== r || u === void 0) {
    var d = nc(r);
    (!Xe || d !== e.getAttribute("class")) && (d == null ? e.removeAttribute("class") : e.className = d), e.__className = r;
  }
  return c;
}
function mr(e, t, r, a) {
  var o = e.__style;
  if (Xe || o !== t) {
    var c = ic(t);
    (!Xe || c !== e.getAttribute("style")) && (c == null ? e.removeAttribute("style") : e.style.cssText = c), e.__style = t;
  }
  return a;
}
function bi(e, t, r = !1) {
  if (e.multiple) {
    if (t == null)
      return;
    if (!Da(t))
      return bo();
    for (var a of e.options)
      a.selected = t.includes(hs(a));
    return;
  }
  for (a of e.options) {
    var o = hs(a);
    if (Oo(o, t)) {
      a.selected = !0;
      return;
    }
  }
  (!r || t !== void 0) && (e.selectedIndex = -1);
}
function oc(e) {
  var t = new MutationObserver(() => {
    bi(e, e.__value);
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
  }), Js(() => {
    t.disconnect();
  });
}
function Bs(e, t, r = t) {
  var a = /* @__PURE__ */ new WeakSet(), o = !0;
  Kn(e, "change", (c) => {
    var u = c ? "[selected]" : ":checked", d;
    if (e.multiple)
      d = [].map.call(e.querySelectorAll(u), hs);
    else {
      var v = e.querySelector(u) ?? // will fall back to first non-disabled option if no option is selected
      e.querySelector("option:not([disabled])");
      d = v && hs(v);
    }
    r(d), at !== null && a.add(at);
  }), Xn(() => {
    var c = t();
    if (e === document.activeElement) {
      var u = (
        /** @type {Batch} */
        Fs ?? at
      );
      if (a.has(u))
        return;
    }
    if (bi(e, c, o), o && c === void 0) {
      var d = e.querySelector(":checked");
      d !== null && (c = hs(d), r(c));
    }
    e.__value = c, o = !1;
  }), oc(e);
}
function hs(e) {
  return "__value" in e ? e.__value : e.value;
}
const cc = /* @__PURE__ */ Symbol("is custom element"), lc = /* @__PURE__ */ Symbol("is html");
function ht(e) {
  if (Xe) {
    var t = !1, r = () => {
      if (!t) {
        if (t = !0, e.hasAttribute("value")) {
          var a = e.value;
          Rt(e, "value", null), e.value = a;
        }
        if (e.hasAttribute("checked")) {
          var o = e.checked;
          Rt(e, "checked", null), e.checked = o;
        }
      }
    };
    e.__on_r = r, or(r), Yn();
  }
}
function Rt(e, t, r, a) {
  var o = uc(e);
  Xe && (o[t] = e.getAttribute(t), t === "src" || t === "srcset" || t === "href" && e.nodeName === "LINK") || o[t] !== (o[t] = r) && (t === "loading" && (e[io] = r), r == null ? e.removeAttribute(t) : typeof r != "string" && dc(e).includes(t) ? e[t] = r : e.setAttribute(t, r));
}
function uc(e) {
  return (
    /** @type {Record<string | symbol, unknown>} **/
    // @ts-expect-error
    e.__attributes ??= {
      [cc]: e.nodeName.includes("-"),
      [lc]: e.namespaceURI === eo
    }
  );
}
var un = /* @__PURE__ */ new Map();
function dc(e) {
  var t = e.getAttribute("is") || e.nodeName, r = un.get(t);
  if (r) return r;
  un.set(t, r = []);
  for (var a, o = e, c = Element.prototype; c !== o; ) {
    a = wn(o);
    for (var u in a)
      a[u].set && r.push(u);
    o = Pa(o);
  }
  return r;
}
function lt(e, t, r = t) {
  var a = /* @__PURE__ */ new WeakSet();
  Kn(e, "input", async (o) => {
    var c = o ? e.defaultValue : e.value;
    if (c = na(e) ? ia(c) : c, r(c), at !== null && a.add(at), await Ha(), c !== (c = t())) {
      var u = e.selectionStart, d = e.selectionEnd, v = e.value.length;
      if (e.value = c ?? "", d !== null) {
        var _ = e.value.length;
        u === d && d === v && _ > v ? (e.selectionStart = _, e.selectionEnd = _) : (e.selectionStart = u, e.selectionEnd = Math.min(d, _));
      }
    }
  }), // If we are hydrating and the value has since changed,
  // then use the updated value from the input instead.
  (Xe && e.defaultValue !== e.value || // If defaultValue is set, then value == defaultValue
  // TODO Svelte 6: remove input.value check and set to empty string?
  Wr(t) == null && e.value) && (r(na(e) ? ia(e.value) : e.value), at !== null && a.add(at)), Qs(() => {
    var o = t();
    if (e === document.activeElement) {
      var c = (
        /** @type {Batch} */
        Fs ?? at
      );
      if (a.has(c))
        return;
    }
    na(e) && o === ia(e.value) || e.type === "date" && !o && !e.value || o !== e.value && (e.value = o ?? "");
  });
}
function na(e) {
  var t = e.type;
  return t === "number" || t === "range";
}
function ia(e) {
  return e === "" ? null : +e;
}
function dn(e, t) {
  return e === t || e?.[wr] === t;
}
function Yr(e = {}, t, r, a) {
  return Xn(() => {
    var o, c;
    return Qs(() => {
      o = c, c = [], Wr(() => {
        e !== r(...c) && (t(e, ...c), o && dn(r(...o), e) && t(null, ...o));
      });
    }), () => {
      or(() => {
        c && dn(r(...c), e) && t(null, ...c);
      });
    };
  }), e;
}
function vc(e = !1) {
  const t = (
    /** @type {ComponentContextLegacy} */
    dt
  ), r = t.l.u;
  if (!r) return;
  let a = () => qo(t.s);
  if (e) {
    let o = 0, c = (
      /** @type {Record<string, any>} */
      {}
    );
    const u = /* @__PURE__ */ Cs(() => {
      let d = !1;
      const v = t.s;
      for (const _ in v)
        v[_] !== c[_] && (c[_] = v[_], d = !0);
      return d && o++, o;
    });
    a = () => s(u);
  }
  r.b.length && Ro(() => {
    vn(t, a), Os(r.b);
  }), Nt(() => {
    const o = Wr(() => r.m.map(ao));
    return () => {
      for (const c of o)
        typeof c == "function" && c();
    };
  }), r.a.length && Nt(() => {
    vn(t, a), Os(r.a);
  });
}
function vn(e, t) {
  if (e.l.s)
    for (const r of e.l.s) s(r);
  t();
}
function Wa(e, t, r) {
  if (e == null)
    return t(void 0), r && r(void 0), fr;
  const a = Wr(
    () => e.subscribe(
      t,
      // @ts-expect-error
      r
    )
  );
  return a.unsubscribe ? () => a.unsubscribe() : a;
}
const Ur = [];
function fc(e, t) {
  return {
    subscribe: Ze(e, t).subscribe
  };
}
function Ze(e, t = fr) {
  let r = null;
  const a = /* @__PURE__ */ new Set();
  function o(d) {
    if (Pn(e, d) && (e = d, r)) {
      const v = !Ur.length;
      for (const _ of a)
        _[1](), Ur.push(_, e);
      if (v) {
        for (let _ = 0; _ < Ur.length; _ += 2)
          Ur[_][0](Ur[_ + 1]);
        Ur.length = 0;
      }
    }
  }
  function c(d) {
    o(d(
      /** @type {T} */
      e
    ));
  }
  function u(d, v = fr) {
    const _ = [d, v];
    return a.add(_), a.size === 1 && (r = t(o, c) || fr), d(
      /** @type {T} */
      e
    ), () => {
      a.delete(_), a.size === 0 && r && (r(), r = null);
    };
  }
  return { set: o, update: c, subscribe: u };
}
function Sr(e, t, r) {
  const a = !Array.isArray(e), o = a ? [e] : e;
  if (!o.every(Boolean))
    throw new Error("derived() expects stores as input, got a falsy value");
  const c = t.length < 2;
  return fc(r, (u, d) => {
    let v = !1;
    const _ = [];
    let x = 0, b = fr;
    const y = () => {
      if (x)
        return;
      b();
      const $ = t(a ? _[0] : _, u, d);
      c ? u($) : b = typeof $ == "function" ? $ : fr;
    }, P = o.map(
      ($, N) => Wa(
        $,
        (C) => {
          _[N] = C, x &= ~(1 << N), v && y();
        },
        () => {
          x |= 1 << N;
        }
      )
    );
    return v = !0, y(), function() {
      Os(P), b(), v = !1;
    };
  });
}
function Ot(e) {
  let t;
  return Wa(e, (r) => t = r)(), t;
}
let ga = /* @__PURE__ */ Symbol();
function Ae(e, t, r) {
  const a = r[t] ??= {
    store: null,
    source: /* @__PURE__ */ Oa(void 0),
    unsubscribe: fr
  };
  if (a.store !== e && !(ga in r))
    if (a.unsubscribe(), a.store = e ?? null, e == null)
      a.source.v = void 0, a.unsubscribe = fr;
    else {
      var o = !0;
      a.unsubscribe = Wa(e, (c) => {
        o ? a.source.v = c : h(a.source, c);
      }), o = !1;
    }
  return e && ga in r ? Ot(e) : s(a.source);
}
function Kt() {
  const e = {};
  function t() {
    Js(() => {
      for (var r in e)
        e[r].unsubscribe();
      ms(e, ga, {
        enumerable: !1,
        value: !0
      });
    });
  }
  return [e, t];
}
function xt(e, t, r, a) {
  var o = !as || (r & Ki) !== 0, c = (r & Qi) !== 0, u = (
    /** @type {V} */
    a
  ), d = !0, v = () => (d && (d = !1, u = /** @type {V} */
  a), u), _;
  _ = /** @type {V} */
  e[t], _ === void 0 && a !== void 0 && (_ = v());
  var x;
  if (o ? x = () => {
    var $ = (
      /** @type {V} */
      e[t]
    );
    return $ === void 0 ? v() : (d = !0, $);
  } : x = () => {
    var $ = (
      /** @type {V} */
      e[t]
    );
    return $ !== void 0 && (u = /** @type {V} */
    void 0), $ === void 0 ? u : $;
  }, o && (r & Ji) === 0)
    return x;
  var b = !1, y = /* @__PURE__ */ Cs(() => (b = !1, x())), P = (
    /** @type {Effect} */
    st
  );
  return (
    /** @type {() => V} */
    (function($, N) {
      if (arguments.length > 0) {
        const C = N ? s(y) : o && c ? Wt($) : $;
        return h(y, C), b = !0, u !== void 0 && (u = C), $;
      }
      return Er && b || (P.f & pr) !== 0 ? y.v : s(y);
    })
  );
}
function pc(e) {
  return new hc(e);
}
class hc {
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
    var r = /* @__PURE__ */ new Map(), a = (c, u) => {
      var d = /* @__PURE__ */ Oa(u, !1, !1);
      return r.set(c, d), d;
    };
    const o = new Proxy(
      { ...t.props || {}, $$events: {} },
      {
        get(c, u) {
          return s(r.get(u) ?? a(u, Reflect.get(c, u)));
        },
        has(c, u) {
          return u === no ? !0 : (s(r.get(u) ?? a(u, Reflect.get(c, u))), Reflect.has(c, u));
        },
        set(c, u, d) {
          return h(r.get(u) ?? a(u, d), d), Reflect.set(c, u, d);
        }
      }
    );
    this.#t = (t.hydrate ? Yo : hi)(t.component, {
      target: t.target,
      anchor: t.anchor,
      props: o,
      context: t.context,
      intro: t.intro ?? !1,
      recover: t.recover
    }), (!t?.props?.$$host || t.sync === !1) && pt(), this.#e = o.$$events;
    for (const c of Object.keys(this.#t))
      c === "$set" || c === "$destroy" || c === "$on" || ms(this, c, {
        get() {
          return this.#t[c];
        },
        /** @param {any} value */
        set(u) {
          this.#t[c] = u;
        },
        enumerable: !0
      });
    this.#t.$set = /** @param {Record<string, any>} next */
    (c) => {
      Object.assign(o, c);
    }, this.#t.$destroy = () => {
      Ko(this.#t);
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
let mi;
typeof HTMLElement == "function" && (mi = class extends HTMLElement {
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
      const t = {}, r = _c(this);
      for (const a of this.$$s)
        a in r && (a === "default" && !this.$$d.children ? (this.$$d.children = e(a), t.default = !0) : t[a] = e(a));
      for (const a of this.attributes) {
        const o = this.$$g_p(a.name);
        o in this.$$d || (this.$$d[o] = Ms(o, a.value, this.$$p_d, "toProp"));
      }
      for (const a in this.$$p_d)
        !(a in this.$$d) && this[a] !== void 0 && (this.$$d[a] = this[a], delete this[a]);
      this.$$c = pc({
        component: this.$$ctor,
        target: this.$$shadowRoot || this,
        props: {
          ...this.$$d,
          $$slots: t,
          $$host: this
        }
      }), this.$$me = Fo(() => {
        Qs(() => {
          this.$$r = !0;
          for (const a of Ns(this.$$c)) {
            if (!this.$$p_d[a]?.reflect) continue;
            this.$$d[a] = this.$$c[a];
            const o = Ms(
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
    this.$$r || (e = this.$$g_p(e), this.$$d[e] = Ms(e, r, this.$$p_d, "toProp"), this.$$c?.$set({ [e]: this.$$d[e] }));
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
    return Ns(this.$$p_d).find(
      (t) => this.$$p_d[t].attribute === e || !this.$$p_d[t].attribute && t.toLowerCase() === e
    ) || e;
  }
});
function Ms(e, t, r, a) {
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
function _c(e) {
  const t = {};
  return e.childNodes.forEach((r) => {
    t[
      /** @type {Element} node */
      r.slot || "default"
    ] = !0;
  }), t;
}
function Vt(e, t, r, a, o, c) {
  let u = class extends mi {
    constructor() {
      super(e, r, o), this.$$p_d = t;
    }
    static get observedAttributes() {
      return Ns(t).map(
        (d) => (t[d].attribute || d).toLowerCase()
      );
    }
  };
  return Ns(t).forEach((d) => {
    ms(u.prototype, d, {
      get() {
        return this.$$c && d in this.$$c ? this.$$c[d] : this.$$d[d];
      },
      set(v) {
        v = Ms(d, v, t), this.$$d[d] = v;
        var _ = this.$$c;
        if (_) {
          var x = Gr(_, d)?.get;
          x ? _[d] = v : _.$set({ [d]: v });
        }
      }
    });
  }), a.forEach((d) => {
    ms(u.prototype, d, {
      get() {
        return this.$$c?.[d];
      }
    });
  }), e.element = /** @type {any} */
  u, u;
}
const yi = 1, wi = 2, _s = 4, xs = 1, $i = 2, Pr = 4, Kr = 8, Jr = 16;
function et(e, t) {
  return (e & t) !== 0;
}
function Hs(e, t) {
  switch (t) {
    case "planning":
      return e.planning_status ?? 0;
    case "crafting":
      return e.crafting_status ?? 0;
  }
}
function xc(e, t) {
  const r = typeof e == "number" ? e : Hs(e, t);
  return et(r, Jr) ? "Concluded" : et(r, Kr) ? "Shelved" : et(r, Pr) ? "Open" : et(r, $i) ? "Archived" : et(r, xs) ? "Initiated" : "Pending";
}
function fn(e) {
  return et(e, Jr) ? "text-health-ok" : et(e, Pr) ? "text-hecate-400" : et(e, Kr) ? "text-health-warn" : et(e, $i) ? "text-surface-500" : et(e, xs) ? "text-surface-300" : "text-surface-500";
}
let ba;
function gc(e) {
  ba = e;
}
function Qe() {
  if (!ba)
    throw new Error("Martha API not initialized. Call setApi() first.");
  return ba;
}
const ki = "hecate://localhost";
async function bc() {
  try {
    const e = await fetch(`${ki}/api/llm/models`);
    if (!e.ok) return [];
    const t = await e.json();
    return t.ok && Array.isArray(t.models) ? t.models.map((r) => r.name) : [];
  } catch {
    return [];
  }
}
function mc(e, t) {
  let r = null, a = null, o = null, c = !1;
  const u = {
    onChunk(d) {
      return r = d, u;
    },
    onDone(d) {
      return a = d, u;
    },
    onError(d) {
      return o = d, u;
    },
    async start() {
      if (!c)
        try {
          const d = await fetch(`${ki}/api/llm/chat`, {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ model: e, messages: t })
          });
          if (c) return;
          if (!d.ok) {
            const _ = await d.text().catch(() => d.statusText);
            o && o(_ || "LLM request failed");
            return;
          }
          const v = await d.json();
          r && r({ content: v.content }), a && a({ content: "", done: !0 });
        } catch (d) {
          if (c) return;
          o && o(d.message || "LLM request failed");
        }
    },
    cancel() {
      c = !0;
    }
  };
  return u;
}
function Ei() {
  return {
    stream: {
      chat: mc
    }
  };
}
const qa = Ze(!1), Ci = Ze(""), Ua = Ze(null), Si = Ze(null), za = Ze([]), Ga = Sr(
  [Si, za],
  ([e, t]) => e ?? t[0] ?? null
), Ai = "hecate-app-martha-phase-models";
function yc() {
  try {
    const e = localStorage.getItem(Ai);
    if (e) {
      const t = JSON.parse(e);
      return "dna" in t && !("planning" in t) ? { planning: null, crafting: null } : t;
    }
  } catch {
  }
  return { planning: null, crafting: null };
}
function wc(e) {
  try {
    localStorage.setItem(Ai, JSON.stringify(e));
  } catch {
  }
}
const Di = Ze(yc()), $c = [
  /code/i,
  /coder/i,
  /codestral/i,
  /starcoder/i,
  /codellama/i,
  /wizard-?coder/i,
  /deepseek-coder/i
];
function pn(e) {
  return $c.some((t) => t.test(e)) ? "code" : "general";
}
function kc(e) {
  return e === "crafting" ? "code" : "general";
}
function ur(e, t) {
  Ua.set(t ?? null), Ci.set(e), qa.set(!0);
}
function Ec() {
  qa.set(!1), Ua.set(null);
}
function Ya(e) {
  Si.set(e);
}
function hn(e, t) {
  Di.update((r) => {
    const a = { ...r, [e]: t };
    return wc(a), a;
  });
}
function Cc(e) {
  return e.split(`
`).map((t) => t.replace(/^[\s\-*\u2022\d.]+/, "").trim()).filter((t) => t.length > 0 && t.length < 80 && !t.includes(":")).map((t) => t.replace(/["`]/g, ""));
}
const Or = [
  {
    code: "planning",
    name: "Planning",
    shortName: "Planning",
    description: "Design aggregates, plan desks, map dependencies",
    role: "planning",
    color: "phase-planning"
  },
  {
    code: "crafting",
    name: "Crafting",
    shortName: "Crafting",
    description: "Generate code, run tests, deliver releases",
    role: "crafting",
    color: "phase-crafting"
  }
], ws = Ze([]), bt = Ze(null), Vr = Ze([]), gs = Ze(null), ft = Ze(!1), ar = Ze(null), ns = Sr(
  [Vr, gs],
  ([e, t]) => e.find((r) => r.division_id === t) ?? null
), Xs = Sr(
  bt,
  (e) => e ? et(e.status, _s) ? "archived" : et(e.status, wi) ? "discovery_paused" : et(e.status, yi) ? "discovering" : e.phase || "initiated" : "none"
);
function bs(e) {
  bt.set(e);
}
function ma() {
  bt.set(null);
}
async function As() {
  try {
    const t = await Qe().get("/ventures");
    ws.set(t.ventures);
  } catch {
    ws.set([]);
  }
}
async function nr() {
  try {
    const t = await Qe().get("/ventures/active");
    bt.set(t.venture);
  } catch {
    bt.set(null);
  }
}
async function is(e) {
  try {
    const r = await Qe().get(
      `/ventures/${e}/divisions`
    );
    Vr.set(r.divisions);
  } catch {
    Vr.set([]);
  }
}
async function Pi(e, t) {
  try {
    return ft.set(!0), await Qe().post("/ventures/initiate", { name: e, brief: t, initiated_by: "hecate-web" }), await As(), await nr(), !0;
  } catch (r) {
    const a = r;
    return ar.set(a.message || "Failed to initiate venture"), !1;
  } finally {
    ft.set(!1);
  }
}
async function Ti(e, t, r, a, o) {
  try {
    return ft.set(!0), await Qe().post(`/ventures/${e}/repo`, {
      repo_url: t,
      vision: r || void 0,
      name: a || void 0,
      brief: o || void 0
    }), await nr(), !0;
  } catch (c) {
    const u = c;
    return ar.set(u.message || "Failed to scaffold venture repo"), !1;
  } finally {
    ft.set(!1);
  }
}
async function Ka(e) {
  try {
    return ft.set(!0), await Qe().post(`/ventures/${e}/discovery/start`, {}), await nr(), !0;
  } catch (t) {
    const r = t;
    return ar.set(r.message || "Failed to start discovery"), !1;
  } finally {
    ft.set(!1);
  }
}
async function Mi(e, t, r) {
  try {
    return ft.set(!0), await Qe().post(`/ventures/${e}/discovery/identify`, {
      context_name: t,
      description: r || null,
      identified_by: "hecate-web"
    }), await is(e), !0;
  } catch (a) {
    const o = a;
    return ar.set(o.message || "Failed to identify division"), !1;
  } finally {
    ft.set(!1);
  }
}
async function Ii(e, t) {
  try {
    return ft.set(!0), await Qe().post(`/ventures/${e}/discovery/pause`, {
      reason: t || null
    }), await nr(), !0;
  } catch (r) {
    const a = r;
    return ar.set(a.message || "Failed to pause discovery"), !1;
  } finally {
    ft.set(!1);
  }
}
async function Ni(e) {
  try {
    return ft.set(!0), await Qe().post(`/ventures/${e}/discovery/resume`, {}), await nr(), !0;
  } catch (t) {
    const r = t;
    return ar.set(r.message || "Failed to resume discovery"), !1;
  } finally {
    ft.set(!1);
  }
}
async function Oi(e) {
  try {
    return ft.set(!0), await Qe().post(`/ventures/${e}/discovery/complete`, {}), await nr(), !0;
  } catch (t) {
    const r = t;
    return ar.set(r.message || "Failed to complete discovery"), !1;
  } finally {
    ft.set(!1);
  }
}
const Sc = /* @__PURE__ */ Object.freeze(/* @__PURE__ */ Object.defineProperty({
  __proto__: null,
  activeVenture: bt,
  clearActiveVenture: ma,
  completeDiscovery: Oi,
  divisions: Vr,
  fetchActiveVenture: nr,
  fetchDivisions: is,
  fetchVentures: As,
  identifyDivision: Mi,
  initiateVenture: Pi,
  isLoading: ft,
  pauseDiscovery: Ii,
  resumeDiscovery: Ni,
  scaffoldVentureRepo: Ti,
  selectVenture: bs,
  selectedDivision: ns,
  selectedDivisionId: gs,
  startDiscovery: Ka,
  ventureError: ar,
  ventureStep: Xs,
  ventures: ws
}, Symbol.toStringTag, { value: "Module" })), ss = Ze("planning"), Zs = Ze(null), _r = Ze(!1);
function ea(e) {
  return e === "planning" ? "plannings" : "craftings";
}
async function Ac(e, t) {
  try {
    _r.set(!0), await Qe().post(`/${ea(t)}/${e}/open`, {});
    const a = Ot(bt);
    return a && await is(a.venture_id), !0;
  } catch (r) {
    const a = r;
    return Zs.set(a.message || `Failed to open ${t}`), !1;
  } finally {
    _r.set(!1);
  }
}
async function Dc(e, t, r) {
  try {
    _r.set(!0), await Qe().post(`/${ea(t)}/${e}/shelve`, {
      reason: r || null
    });
    const o = Ot(bt);
    return o && await is(o.venture_id), !0;
  } catch (a) {
    const o = a;
    return Zs.set(o.message || `Failed to shelve ${t}`), !1;
  } finally {
    _r.set(!1);
  }
}
async function Pc(e, t) {
  try {
    _r.set(!0), await Qe().post(`/${ea(t)}/${e}/resume`, {});
    const a = Ot(bt);
    return a && await is(a.venture_id), !0;
  } catch (r) {
    const a = r;
    return Zs.set(a.message || `Failed to resume ${t}`), !1;
  } finally {
    _r.set(!1);
  }
}
async function Tc(e, t) {
  try {
    _r.set(!0), await Qe().post(`/${ea(t)}/${e}/conclude`, {});
    const a = Ot(bt);
    return a && await is(a.venture_id), !0;
  } catch (r) {
    const a = r;
    return Zs.set(a.message || `Failed to conclude ${t}`), !1;
  } finally {
    _r.set(!1);
  }
}
const jr = Ze("ready"), os = Ze([]), ta = Ze([]), Ja = Ze([]), Ws = Ze(600), Qa = Ze([]), ya = Ze(!1), Et = Ze(null), wa = Ze(!1);
let Tr = null;
const Mc = Sr(
  os,
  (e) => e.filter((t) => !t.cluster_id)
), Ic = Sr(
  os,
  (e) => {
    const t = /* @__PURE__ */ new Map();
    for (const r of e)
      if (r.stack_id) {
        const a = t.get(r.stack_id) || [];
        a.push(r), t.set(r.stack_id, a);
      }
    return t;
  }
), Nc = Sr(
  os,
  (e) => e.length
);
async function At(e) {
  try {
    const a = (await Qe().get(
      `/ventures/${e}/storm/state`
    )).storm;
    jr.set(a.phase), os.set(a.stickies), ta.set(a.clusters), Ja.set(a.arrows);
  } catch {
    jr.set("ready");
  }
}
async function _n(e, t = 0, r = 50) {
  try {
    const o = await Qe().get(
      `/ventures/${e}/events?offset=${t}&limit=${r}`
    );
    return Qa.set(o.events), { events: o.events, count: o.count };
  } catch {
    return { events: [], count: 0 };
  }
}
async function Oc(e) {
  try {
    return wa.set(!0), await Qe().post(`/ventures/${e}/storm/start`, {}), jr.set("storm"), Ws.set(600), Tr = setInterval(() => {
      Ws.update((r) => r <= 1 ? (Tr && (clearInterval(Tr), Tr = null), 0) : r - 1);
    }, 1e3), !0;
  } catch (t) {
    const r = t;
    return Et.set(r.message || "Failed to start storm"), !1;
  } finally {
    wa.set(!1);
  }
}
async function $a(e, t, r = "user") {
  try {
    return await Qe().post(`/ventures/${e}/storm/sticky`, { text: t, author: r }), await At(e), !0;
  } catch (a) {
    const o = a;
    return Et.set(o.message || "Failed to post sticky"), !1;
  }
}
async function Lc(e, t) {
  try {
    return await Qe().post(`/ventures/${e}/storm/sticky/${t}/pull`, {}), await At(e), !0;
  } catch (r) {
    const a = r;
    return Et.set(a.message || "Failed to pull sticky"), !1;
  }
}
async function xn(e, t, r) {
  try {
    return await Qe().post(`/ventures/${e}/storm/sticky/${t}/stack`, {
      target_sticky_id: r
    }), await At(e), !0;
  } catch (a) {
    const o = a;
    return Et.set(o.message || "Failed to stack sticky"), !1;
  }
}
async function Rc(e, t) {
  try {
    return await Qe().post(`/ventures/${e}/storm/sticky/${t}/unstack`, {}), await At(e), !0;
  } catch (r) {
    const a = r;
    return Et.set(a.message || "Failed to unstack sticky"), !1;
  }
}
async function Fc(e, t, r) {
  try {
    return await Qe().post(`/ventures/${e}/storm/stack/${t}/groom`, {
      canonical_sticky_id: r
    }), await At(e), !0;
  } catch (a) {
    const o = a;
    return Et.set(o.message || "Failed to groom stack"), !1;
  }
}
async function gn(e, t, r) {
  try {
    return await Qe().post(`/ventures/${e}/storm/sticky/${t}/cluster`, {
      target_cluster_id: r
    }), await At(e), !0;
  } catch (a) {
    const o = a;
    return Et.set(o.message || "Failed to cluster sticky"), !1;
  }
}
async function Vc(e, t) {
  try {
    return await Qe().post(`/ventures/${e}/storm/sticky/${t}/uncluster`, {}), await At(e), !0;
  } catch (r) {
    const a = r;
    return Et.set(a.message || "Failed to uncluster sticky"), !1;
  }
}
async function jc(e, t) {
  try {
    return await Qe().post(`/ventures/${e}/storm/cluster/${t}/dissolve`, {}), await At(e), !0;
  } catch (r) {
    const a = r;
    return Et.set(a.message || "Failed to dissolve cluster"), !1;
  }
}
async function Bc(e, t, r) {
  try {
    return await Qe().post(`/ventures/${e}/storm/cluster/${t}/name`, { name: r }), await At(e), !0;
  } catch (a) {
    const o = a;
    return Et.set(o.message || "Failed to name cluster"), !1;
  }
}
async function Hc(e, t, r, a) {
  try {
    return await Qe().post(`/ventures/${e}/storm/fact`, {
      from_cluster: t,
      to_cluster: r,
      fact_name: a
    }), await At(e), !0;
  } catch (o) {
    const c = o;
    return Et.set(c.message || "Failed to draw fact arrow"), !1;
  }
}
async function Wc(e, t) {
  try {
    return await Qe().post(`/ventures/${e}/storm/fact/${t}/erase`, {}), await At(e), !0;
  } catch (r) {
    const a = r;
    return Et.set(a.message || "Failed to erase fact arrow"), !1;
  }
}
async function qc(e, t) {
  try {
    return await Qe().post(`/ventures/${e}/storm/cluster/${t}/promote`, {}), await At(e), !0;
  } catch (r) {
    const a = r;
    return Et.set(a.message || "Failed to promote cluster"), !1;
  }
}
async function us(e, t) {
  try {
    return await Qe().post(`/ventures/${e}/storm/phase/advance`, {
      target_phase: t
    }), await At(e), !0;
  } catch (r) {
    const a = r;
    return Et.set(a.message || "Failed to advance phase"), !1;
  }
}
async function Uc(e) {
  try {
    return await Qe().post(`/ventures/${e}/storm/shelve`, {}), jr.set("shelved"), !0;
  } catch (t) {
    const r = t;
    return Et.set(r.message || "Failed to shelve storm"), !1;
  }
}
async function zc(e) {
  try {
    return await Qe().post(`/ventures/${e}/storm/resume`, {}), await At(e), !0;
  } catch (t) {
    const r = t;
    return Et.set(r.message || "Failed to resume storm"), !1;
  }
}
async function Gc(e) {
  const t = Ot(ta);
  let r = !0;
  for (const a of t) {
    if (a.status !== "active" || !a.name?.trim()) continue;
    await qc(e, a.cluster_id) || (r = !1);
  }
  if (r) {
    const { fetchDivisions: a } = await Promise.resolve().then(() => Sc);
    await a(e);
  }
  return r;
}
function Yc() {
  Tr && (clearInterval(Tr), Tr = null), jr.set("ready"), os.set([]), ta.set([]), Ja.set([]), Qa.set([]), Ws.set(600);
}
const bn = Ze(!1), Kc = Ze(null), Jc = Ze(null);
async function Qc(e, t) {
  try {
    bn.set(!0);
    const a = await Qe().post(
      `/ventures/${e}/vision/refine`,
      { vision: t }
    );
    return Kc.set(a.refined), await nr(), !0;
  } catch (r) {
    const a = r;
    return Jc.set(a.message || "Failed to refine vision"), !1;
  } finally {
    bn.set(!1);
  }
}
var Xc = /* @__PURE__ */ p('<div class="text-[10px] text-surface-400 truncate mt-0.5"> </div>'), Zc = /* @__PURE__ */ p('<button><div class="font-medium"> </div> <!></button>'), el = /* @__PURE__ */ p(`<div class="absolute top-full left-0 mt-1 z-20 min-w-[220px]
						bg-surface-700 border border-surface-600 rounded-lg shadow-lg overflow-hidden"><!> <button class="w-full text-left px-3 py-2 text-xs text-hecate-400
							hover:bg-hecate-600/20 transition-colors border-t border-surface-600">+ New Venture</button></div>`), tl = /* @__PURE__ */ p('<span class="text-[11px] text-surface-400 truncate max-w-[300px]"> </span>'), rl = /* @__PURE__ */ p('<span class="text-[10px] text-surface-400"> </span>'), sl = /* @__PURE__ */ p('<span class="text-[10px] text-surface-400 italic">Oracle active</span>'), al = /* @__PURE__ */ p(`<button class="text-[11px] px-2.5 py-1 rounded bg-hecate-600/20 text-hecate-300
					hover:bg-hecate-600/30 transition-colors disabled:opacity-50">Start Discovery</button>`), nl = /* @__PURE__ */ p(`<button class="text-[11px] px-2 py-1 rounded text-surface-400
						hover:text-health-ok hover:bg-surface-700 transition-colors disabled:opacity-50">Complete Discovery</button>`), il = /* @__PURE__ */ p(
  `<button class="text-[11px] px-2.5 py-1 rounded bg-hecate-600/20 text-hecate-300
					hover:bg-hecate-600/30 transition-colors disabled:opacity-50">+ Identify Division</button> <button class="text-[11px] px-2 py-1 rounded text-surface-400
					hover:text-health-warn hover:bg-surface-700 transition-colors disabled:opacity-50">Pause</button> <!>`,
  1
), ol = /* @__PURE__ */ p(`<button class="text-[11px] px-2.5 py-1 rounded bg-health-warn/10 text-health-warn
					hover:bg-health-warn/20 transition-colors disabled:opacity-50">Resume Discovery</button>`), cl = /* @__PURE__ */ p('<div class="mt-2 text-[11px] text-health-err bg-health-err/10 rounded px-3 py-1.5"> </div>'), ll = /* @__PURE__ */ p(`<div class="mt-3 flex gap-2 items-end"><div class="flex-1"><label for="refine-brief" class="text-[10px] text-surface-400 block mb-1">Vision Brief</label> <textarea id="refine-brief" placeholder="Describe what this venture aims to achieve..." class="w-full bg-surface-700 border border-surface-600 rounded px-3 py-2 text-xs
						text-surface-100 placeholder-surface-400 resize-none
						focus:outline-none focus:border-hecate-500"></textarea></div> <button class="px-3 py-2 rounded text-xs bg-hecate-600 text-surface-50
					hover:bg-hecate-500 transition-colors disabled:opacity-50 disabled:cursor-not-allowed">Refine</button> <button class="px-3 py-2 rounded text-xs text-surface-400 hover:text-surface-100 transition-colors">Cancel</button></div>`), ul = /* @__PURE__ */ p(`<div class="mt-3 flex gap-2 items-end"><div class="flex-1"><label for="div-name" class="text-[10px] text-surface-400 block mb-1">Context Name</label> <input id="div-name" placeholder="e.g., authentication, billing, notifications" class="w-full bg-surface-700 border border-surface-600 rounded px-3 py-1.5 text-xs
						text-surface-100 placeholder-surface-400
						focus:outline-none focus:border-hecate-500"/></div> <div class="flex-1"><label for="div-desc" class="text-[10px] text-surface-400 block mb-1">Description (optional)</label> <input id="div-desc" placeholder="Brief description of this bounded context" class="w-full bg-surface-700 border border-surface-600 rounded px-3 py-1.5 text-xs
						text-surface-100 placeholder-surface-400
						focus:outline-none focus:border-hecate-500"/></div> <button class="px-3 py-1.5 rounded text-xs bg-hecate-600 text-surface-50
					hover:bg-hecate-500 transition-colors disabled:opacity-50 disabled:cursor-not-allowed">Identify</button> <button class="px-3 py-1.5 rounded text-xs text-surface-400 hover:text-surface-100 transition-colors">Cancel</button></div>`), dl = /* @__PURE__ */ p(`<div class="border-b border-surface-600 bg-surface-800/50 px-4 py-3 shrink-0"><div class="flex items-center gap-3"><button class="flex items-center gap-1 text-xs text-surface-400 hover:text-hecate-300
				transition-colors shrink-0 -ml-1 px-1.5 py-1 rounded hover:bg-surface-700"><span class="text-sm"></span> <span>Ventures</span></button> <span class="text-surface-600 text-xs">|</span> <div class="relative flex items-center gap-2"><span class="text-hecate-400 text-lg"></span> <button class="flex items-center gap-1.5 text-sm font-semibold text-surface-100
					hover:text-hecate-300 transition-colors"> <span class="text-[9px] text-surface-400"></span></button> <!></div> <span> </span> <!> <div class="flex-1"></div> <!> <!></div> <!> <!> <!></div>`);
function Li(e, t) {
  Tt(t, !0);
  const r = () => Ae(bt, "$activeVenture", v), a = () => Ae(ws, "$ventures", v), o = () => Ae(Xs, "$ventureStep", v), c = () => Ae(Vr, "$divisions", v), u = () => Ae(ft, "$isLoading", v), d = () => Ae(ar, "$ventureError", v), [v, _] = Kt();
  let x = /* @__PURE__ */ se(!1), b = /* @__PURE__ */ se(!1), y = /* @__PURE__ */ se(!1), P = /* @__PURE__ */ se(""), $ = /* @__PURE__ */ se(""), N = /* @__PURE__ */ se("");
  async function C() {
    if (!r() || !s(P).trim()) return;
    await Qc(r().venture_id, s(P).trim()) && (h(x, !1), h(P, ""));
  }
  async function ee() {
    r() && await Ka(r().venture_id);
  }
  async function te() {
    if (!r() || !s($).trim()) return;
    await Mi(r().venture_id, s($).trim(), s(N).trim() || void 0) && (h(b, !1), h($, ""), h(N, ""));
  }
  function $e(S) {
    switch (S) {
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
  var he = dl(), Te = i(he), Q = i(Te);
  Q.__click = () => ma();
  var Ie = i(Q);
  Ie.textContent = "←", wt(2), n(Q);
  var _e = l(Q, 4), Je = i(_e);
  Je.textContent = "◆";
  var xe = l(Je, 2);
  xe.__click = () => h(y, !s(y));
  var qe = i(xe), Ue = l(qe);
  Ue.textContent = "▾", n(xe);
  var Ke = l(xe, 2);
  {
    var Ne = (S) => {
      var k = el(), W = i(k);
      We(W, 1, () => a().filter((X) => !(X.status & _s)), ot, (X, oe) => {
        var Z = Zc();
        Z.__click = () => {
          bs(s(oe)), h(y, !1);
        };
        var ke = i(Z), T = i(ke, !0);
        n(ke);
        var H = l(ke, 2);
        {
          var B = (ue) => {
            var ne = Xc(), ie = i(ne, !0);
            n(ne), D(() => E(ie, s(oe).brief)), f(ue, ne);
          };
          I(H, (ue) => {
            s(oe).brief && ue(B);
          });
        }
        n(Z), D(() => {
          Fe(Z, 1, `w-full text-left px-3 py-2 text-xs transition-colors
								${s(oe).venture_id === r()?.venture_id ? "bg-hecate-600/20 text-hecate-300" : "text-surface-200 hover:bg-surface-600"}`), E(T, s(oe).name);
        }), f(X, Z);
      });
      var V = l(W, 2);
      V.__click = () => {
        ma(), h(y, !1);
      }, n(k), f(S, k);
    };
    I(Ke, (S) => {
      s(y) && S(Ne);
    });
  }
  n(_e);
  var K = l(_e, 2), w = i(K, !0);
  n(K);
  var G = l(K, 2);
  {
    var Oe = (S) => {
      var k = tl(), W = i(k, !0);
      n(k), D(() => E(W, r().brief)), f(S, k);
    };
    I(G, (S) => {
      r()?.brief && S(Oe);
    });
  }
  var ze = l(G, 4);
  {
    var De = (S) => {
      var k = rl(), W = i(k);
      n(k), D(() => E(W, `${c().length ?? ""} division${c().length !== 1 ? "s" : ""}`)), f(S, k);
    };
    I(ze, (S) => {
      c().length > 0 && S(De);
    });
  }
  var we = l(ze, 2);
  {
    var me = (S) => {
      var k = sl();
      f(S, k);
    }, ae = (S) => {
      var k = al();
      k.__click = ee, D(() => k.disabled = u()), f(S, k);
    }, ve = (S) => {
      var k = il(), W = it(k);
      W.__click = () => h(b, !s(b));
      var V = l(W, 2);
      V.__click = () => r() && Ii(r().venture_id);
      var X = l(V, 2);
      {
        var oe = (Z) => {
          var ke = nl();
          ke.__click = () => r() && Oi(r().venture_id), D(() => ke.disabled = u()), f(Z, ke);
        };
        I(X, (Z) => {
          c().length > 0 && Z(oe);
        });
      }
      D(() => {
        W.disabled = u(), V.disabled = u();
      }), f(S, k);
    }, re = (S) => {
      var k = ol();
      k.__click = () => r() && Ni(r().venture_id), D(() => k.disabled = u()), f(S, k);
    };
    I(we, (S) => {
      o() === "initiated" || o() === "vision_refined" ? S(me) : o() === "vision_submitted" ? S(ae, 1) : o() === "discovering" ? S(ve, 2) : o() === "discovery_paused" && S(re, 3);
    });
  }
  n(Te);
  var ye = l(Te, 2);
  {
    var O = (S) => {
      var k = cl(), W = i(k, !0);
      n(k), D(() => E(W, d())), f(S, k);
    };
    I(ye, (S) => {
      d() && S(O);
    });
  }
  var F = l(ye, 2);
  {
    var Ce = (S) => {
      var k = ll(), W = i(k), V = l(i(W), 2);
      Fa(V), Rt(V, "rows", 2), n(W);
      var X = l(W, 2);
      X.__click = C;
      var oe = l(X, 2);
      oe.__click = () => h(x, !1), n(k), D((Z) => X.disabled = Z, [() => !s(P).trim() || u()]), lt(V, () => s(P), (Z) => h(P, Z)), f(S, k);
    };
    I(F, (S) => {
      s(x) && S(Ce);
    });
  }
  var Re = l(F, 2);
  {
    var Ge = (S) => {
      var k = ul(), W = i(k), V = l(i(W), 2);
      ht(V), n(W);
      var X = l(W, 2), oe = l(i(X), 2);
      ht(oe), n(X);
      var Z = l(X, 2);
      Z.__click = te;
      var ke = l(Z, 2);
      ke.__click = () => h(b, !1), n(k), D((T) => Z.disabled = T, [() => !s($).trim() || u()]), lt(V, () => s($), (T) => h($, T)), lt(oe, () => s(N), (T) => h(N, T)), f(S, k);
    };
    I(Re, (S) => {
      s(b) && S(Ge);
    });
  }
  n(he), D(
    (S) => {
      E(qe, `${r()?.name ?? "Venture" ?? ""} `), Fe(K, 1, `text-[10px] px-2 py-0.5 rounded-full border ${S ?? ""}`), E(w, r()?.status_label ?? "New");
    },
    [() => $e(o())]
  ), f(e, he), Mt(), _();
}
Ft(["click"]);
Vt(Li, {}, [], [], { mode: "open" });
var vl = /* @__PURE__ */ p('<p class="text-xs text-surface-300 mt-1.5 max-w-md mx-auto"> </p>'), fl = /* @__PURE__ */ p("<span></span>"), pl = /* @__PURE__ */ p('<div class="flex items-center gap-1"><div class="flex flex-col items-center gap-0.5 px-2"><span> </span> <span> </span></div> <!></div>'), hl = /* @__PURE__ */ p('<div class="rounded-lg border border-surface-600 bg-surface-800 p-3 col-span-2"><div class="text-[9px] text-surface-400 uppercase tracking-wider mb-1">Repository</div> <div class="text-xs text-surface-200 font-mono"> </div></div>'), _l = /* @__PURE__ */ p('<div class="rounded-lg border border-hecate-600/30 bg-hecate-600/5 p-5 text-center"><div class="text-xs text-surface-200 mb-3">Your venture repo has been scaffolded. The next step is <strong class="text-hecate-300">Big Picture Event Storming</strong> </div> <button> </button></div>'), xl = /* @__PURE__ */ p(`<div class="rounded-lg border border-surface-600 bg-surface-800 p-5 text-center"><div class="text-xs text-surface-200 mb-2">Discovery is complete. Identify divisions (bounded contexts)
						from the events you discovered.</div> <div class="text-[10px] text-surface-400">Use the header controls to identify divisions.</div></div>`), gl = /* @__PURE__ */ p('<div class="rounded-lg border border-surface-600 bg-surface-800 p-5 text-center"><div class="text-xs text-surface-200">Continue from the header controls to advance through the lifecycle.</div></div>'), bl = /* @__PURE__ */ p('<div class="text-center"><div class="text-3xl mb-3 text-hecate-400"></div> <h2 class="text-lg font-semibold text-surface-100"> </h2> <!></div> <div class="flex items-center justify-center gap-1 py-4"></div> <div class="grid grid-cols-2 gap-3"><div class="rounded-lg border border-surface-600 bg-surface-800 p-3"><div class="text-[9px] text-surface-400 uppercase tracking-wider mb-1">Status</div> <div class="text-xs text-surface-100"> </div></div> <div class="rounded-lg border border-surface-600 bg-surface-800 p-3"><div class="text-[9px] text-surface-400 uppercase tracking-wider mb-1">Initiated</div> <div class="text-xs text-surface-100"> </div></div> <!></div> <!>', 1), ml = /* @__PURE__ */ p('<div class="flex flex-col h-full overflow-y-auto"><div class="max-w-2xl mx-auto w-full p-8 space-y-6"><!></div></div>');
function Is(e, t) {
  Tt(t, !0);
  const r = () => Ae(bt, "$activeVenture", c), a = () => Ae(Xs, "$ventureStep", c), o = () => Ae(ft, "$isLoading", c), [c, u] = Kt();
  let d = xt(t, "nextAction", 7);
  function v(te) {
    return te ? new Date(te * 1e3).toLocaleDateString("en-US", {
      year: "numeric",
      month: "short",
      day: "numeric",
      hour: "2-digit",
      minute: "2-digit"
    }) : "";
  }
  async function _() {
    if (!r()) return;
    await Ka(r().venture_id) && (await nr(), await As());
  }
  const x = [
    { key: "vision", label: "Vision", icon: "◇" },
    { key: "discovery", label: "Discovery", icon: "○" },
    { key: "design", label: "Design", icon: "△" },
    { key: "plan", label: "Plan", icon: "□" },
    { key: "implement", label: "Implement", icon: "⚙" },
    { key: "deploy", label: "Deploy", icon: "▲" },
    { key: "monitor", label: "Monitor", icon: "◉" },
    { key: "rescue", label: "Rescue", icon: "↺" }
  ];
  let b = /* @__PURE__ */ ge(() => {
    const te = a();
    return te === "initiated" || te === "vision_refined" || te === "vision_submitted" ? 0 : te === "discovering" || te === "discovery_paused" || te === "discovery_completed" ? 1 : 0;
  });
  var y = {
    get nextAction() {
      return d();
    },
    set nextAction(te) {
      d(te), pt();
    }
  }, P = ml(), $ = i(P), N = i($);
  {
    var C = (te) => {
      var $e = bl(), he = it($e), Te = i(he);
      Te.textContent = "◆";
      var Q = l(Te, 2), Ie = i(Q, !0);
      n(Q);
      var _e = l(Q, 2);
      {
        var Je = (ve) => {
          var re = vl(), ye = i(re, !0);
          n(re), D(() => E(ye, r().brief)), f(ve, re);
        };
        I(_e, (ve) => {
          r().brief && ve(Je);
        });
      }
      n(he);
      var xe = l(he, 2);
      We(xe, 21, () => x, ot, (ve, re, ye) => {
        const O = /* @__PURE__ */ ge(() => ye < s(b)), F = /* @__PURE__ */ ge(() => ye === s(b)), Ce = /* @__PURE__ */ ge(() => ye === s(b) + 1);
        var Re = pl(), Ge = i(Re), S = i(Ge), k = i(S, !0);
        n(S);
        var W = l(S, 2), V = i(W, !0);
        n(W), n(Ge);
        var X = l(Ge, 2);
        {
          var oe = (Z) => {
            var ke = fl();
            ke.textContent = "→", D(() => Fe(ke, 1, `text-[10px]
									${s(O) ? "text-health-ok/40" : "text-surface-700"}`)), f(Z, ke);
          };
          I(X, (Z) => {
            ye < x.length - 1 && Z(oe);
          });
        }
        n(Re), D(() => {
          Rt(Ge, "title", s(re).label), Fe(S, 1, `text-sm transition-colors
									${s(O) ? "text-health-ok" : s(F) ? "text-hecate-400" : "text-surface-600"}`), E(k, s(O) ? "✓" : s(re).icon), Fe(W, 1, `text-[9px] transition-colors
									${s(O) ? "text-health-ok/70" : s(F) ? "text-hecate-300" : s(Ce) ? "text-surface-400" : "text-surface-600"}`), E(V, s(re).label);
        }), f(ve, Re);
      }), n(xe);
      var qe = l(xe, 2), Ue = i(qe), Ke = l(i(Ue), 2), Ne = i(Ke, !0);
      n(Ke), n(Ue);
      var K = l(Ue, 2), w = l(i(K), 2), G = i(w, !0);
      n(w), n(K);
      var Oe = l(K, 2);
      {
        var ze = (ve) => {
          var re = hl(), ye = l(i(re), 2), O = i(ye, !0);
          n(ye), n(re), D(() => E(O, r().repos[0])), f(ve, re);
        };
        I(Oe, (ve) => {
          r().repos && r().repos.length > 0 && ve(ze);
        });
      }
      n(qe);
      var De = l(qe, 2);
      {
        var we = (ve) => {
          var re = _l(), ye = i(re), O = l(i(ye), 2);
          O.nodeValue = " — discover the domain events that define your system.", n(ye);
          var F = l(ye, 2);
          F.__click = _;
          var Ce = i(F, !0);
          n(F), n(re), D(() => {
            F.disabled = o(), Fe(F, 1, `px-5 py-2.5 rounded-lg text-sm font-medium transition-colors
							${o() ? "bg-surface-600 text-surface-400 cursor-not-allowed" : "bg-hecate-600 text-surface-50 hover:bg-hecate-500"}`), E(Ce, o() ? "Starting..." : "Start Discovery");
          }), f(ve, re);
        }, me = (ve) => {
          var re = xl();
          f(ve, re);
        }, ae = (ve) => {
          var re = gl();
          f(ve, re);
        };
        I(De, (ve) => {
          d() === "discovery" && a() === "vision_submitted" ? ve(we) : d() === "identify" ? ve(me, 1) : ve(ae, !1);
        });
      }
      D(
        (ve) => {
          E(Ie, r().name), E(Ne, r().status_label), E(G, ve);
        },
        [() => v(r().initiated_at ?? 0)]
      ), f(te, $e);
    };
    I(N, (te) => {
      r() && te(C);
    });
  }
  n($), n(P), f(e, P);
  var ee = Mt(y);
  return u(), ee;
}
Ft(["click"]);
Vt(Is, { nextAction: {} }, [], [], { mode: "open" });
wo();
var yl = /* @__PURE__ */ p("<button><span> </span> <span> </span> <span> </span></button>"), wl = /* @__PURE__ */ p('<div class="ml-2 mt-1 space-y-0.5"></div>'), $l = /* @__PURE__ */ p('<div class="mb-2"><button><span class="font-medium"> </span></button> <!></div>'), kl = /* @__PURE__ */ p('<div class="text-[10px] text-surface-400 px-2 py-4 text-center">No divisions yet. <br/> Start discovery to identify them.</div>'), El = /* @__PURE__ */ p('<div class="w-48 border-r border-surface-600 bg-surface-800/30 overflow-y-auto shrink-0"><div class="p-3"><div class="text-[10px] text-surface-400 uppercase tracking-wider mb-2">Divisions</div> <!> <!></div></div>');
function Ri(e, t) {
  Tt(t, !1);
  const r = () => Ae(Vr, "$divisions", c), a = () => Ae(gs, "$selectedDivisionId", c), o = () => Ae(ss, "$selectedPhase", c), [c, u] = Kt();
  function d(N) {
    gs.set(N);
  }
  function v(N, C) {
    gs.set(N), ss.set(C);
  }
  function _(N) {
    return et(N, Jr) ? "●" : et(N, Pr) ? "◐" : (et(N, Kr), "○");
  }
  vc();
  var x = El(), b = i(x), y = l(i(b), 2);
  We(y, 1, r, ot, (N, C) => {
    const ee = /* @__PURE__ */ Vs(() => a() === s(C).division_id);
    var te = $l(), $e = i(te);
    $e.__click = () => d(s(C).division_id);
    var he = i($e), Te = i(he, !0);
    n(he), n($e);
    var Q = l($e, 2);
    {
      var Ie = (_e) => {
        var Je = wl();
        We(Je, 5, () => Or, ot, (xe, qe) => {
          const Ue = /* @__PURE__ */ Vs(() => Hs(s(C), s(qe).code));
          var Ke = yl();
          Ke.__click = () => v(s(C).division_id, s(qe).code);
          var Ne = i(Ke), K = i(Ne, !0);
          n(Ne);
          var w = l(Ne, 2), G = i(w, !0);
          n(w);
          var Oe = l(w, 2), ze = i(Oe, !0);
          n(Oe), n(Ke), D(
            (De, we, me, ae) => {
              Fe(Ke, 1, `w-full flex items-center gap-1.5 px-2 py-0.5 rounded text-[10px]
									transition-colors
									${o() === s(qe).code ? "bg-surface-600/50 text-surface-100" : "text-surface-400 hover:text-surface-300"}`), Fe(Ne, 1, De), E(K, we), E(G, s(qe).shortName), Fe(Oe, 1, `ml-auto text-[9px] ${me ?? ""}`), E(ze, ae);
            },
            [
              () => ac(fn(s(Ue))),
              () => _(s(Ue)),
              () => fn(s(Ue)),
              () => xc(s(Ue))
            ]
          ), f(xe, Ke);
        }), n(Je), f(_e, Je);
      };
      I(Q, (_e) => {
        s(ee) && _e(Ie);
      });
    }
    n(te), D(() => {
      Fe($e, 1, `w-full text-left px-2 py-1.5 rounded text-xs transition-colors
						${s(ee) ? "bg-surface-700 text-surface-100" : "text-surface-300 hover:bg-surface-700/50 hover:text-surface-100"}`), E(Te, s(C).context_name);
    }), f(N, te);
  });
  var P = l(y, 2);
  {
    var $ = (N) => {
      var C = kl();
      f(N, C);
    };
    I(P, (N) => {
      r().length === 0 && N($);
    });
  }
  n(b), n(x), f(e, x), Mt(), u();
}
Ft(["click"]);
Vt(Ri, {}, [], [], { mode: "open" });
const Fi = Ze(
  "You are Martha, an AI assistant specializing in software architecture and domain-driven design."
), Cl = `You are The Oracle, a vision architect. You interview the user about their venture and build a vision document.

RULES:
1. Ask ONE question per response. Keep it short (2-3 sentences + question).
2. After EVERY response, include a vision draft inside a \`\`\`markdown code fence.
3. Cover 5 topics: Problem, Users, Capabilities, Constraints, Success Criteria.

Be warm but direct. Push for specifics when answers are vague.`, Sl = "Be concise and practical. Suggest specific, actionable items. When suggesting domain elements, use snake_case naming. When suggesting events, use the format: {subject}_{verb_past}_v{N}.", Al = [
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
], Dl = [
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
], Pl = Ze(Al), Tl = Ze(Dl), Ml = Ze(Cl), Il = Ze(Sl);
function Nl(e, t) {
  return e.replace(/\{\{(\w+)\}\}/g, (r, a) => t[a] ?? `{{${a}}}`);
}
var Ol = /* @__PURE__ */ p('<span class="text-[8px] text-hecate-400"></span>'), Ll = /* @__PURE__ */ p('<span class="truncate"> </span> <!>', 1), Rl = /* @__PURE__ */ p('<span class="text-surface-500">Select model</span>'), Fl = /* @__PURE__ */ p('<span class="text-hecate-400 ml-1">(code-optimized)</span>'), Vl = /* @__PURE__ */ p('<button class="text-[9px] text-surface-500 hover:text-surface-300" title="Clear pinned model for this phase">Unpin</button>'), jl = /* @__PURE__ */ p('<div class="px-2 py-1.5 border-b border-surface-700 flex items-center justify-between"><span class="text-[9px] text-surface-400">Phase: <span class="text-surface-200"> </span> <!></span> <!></div>'), Bl = /* @__PURE__ */ p('<div class="p-3 text-center text-[11px] text-surface-500"> </div>'), Hl = /* @__PURE__ */ p('<span class="text-[8px] text-hecate-400 shrink-0" title="Code model"></span>'), Wl = /* @__PURE__ */ p('<span class="text-[8px] text-hecate-400 shrink-0" title="Pinned for this phase"></span>'), ql = /* @__PURE__ */ p('<span class="text-[9px] text-hecate-400 shrink-0"></span>'), Ul = /* @__PURE__ */ p('<button class="text-[8px] text-surface-600 hover:text-hecate-400 shrink-0">pin</button>'), zl = /* @__PURE__ */ p('<div><span class="truncate flex-1"> </span> <!> <!> <!> <!></div>'), Gl = /* @__PURE__ */ p('<div class="py-1"><div class="px-2 py-1 text-[9px] text-surface-500 uppercase tracking-wider font-medium"> </div> <!></div>'), Yl = /* @__PURE__ */ p(`<div class="absolute top-full left-0 mt-1 w-72 max-h-80 overflow-hidden
				bg-surface-800 border border-surface-600 rounded-lg shadow-xl z-50
				flex flex-col"><div class="p-2 border-b border-surface-700"><input placeholder="Search models..." class="w-full bg-surface-700 border border-surface-600 rounded px-2 py-1
						text-[11px] text-surface-100 placeholder-surface-500
						focus:outline-none focus:border-hecate-500"/></div> <!> <div class="overflow-y-auto flex-1"><!> <!></div></div>`), Kl = /* @__PURE__ */ p(`<div class="relative"><button class="text-[10px] px-2 py-0.5 rounded bg-surface-700 text-surface-300
			hover:bg-surface-600 transition-colors truncate max-w-[180px] flex items-center gap-1"><!> <span class="text-[8px] ml-0.5"> </span></button> <!></div>`);
function ra(e, t) {
  Tt(t, !0);
  const r = () => Ae(za, "$availableModels", a), [a, o] = Kt();
  let c = xt(t, "currentModel", 7), u = xt(t, "onSelect", 7), d = xt(t, "showPhaseInfo", 7, !1), v = xt(t, "phasePreference", 7, null), _ = xt(t, "phaseAffinity", 7, "general"), x = xt(t, "onPinModel", 7), b = xt(t, "onClearPin", 7), y = xt(t, "phaseName", 7, ""), P = /* @__PURE__ */ se(!1), $ = /* @__PURE__ */ se(""), N = /* @__PURE__ */ se(void 0), C = /* @__PURE__ */ ge(() => {
    const w = r(), G = s($).toLowerCase(), Oe = G ? w.filter((we) => we.toLowerCase().includes(G)) : w, ze = /* @__PURE__ */ new Map();
    for (const we of Oe) {
      const me = ee(we), ae = ze.get(me) ?? [];
      ae.push(we), ze.set(me, ae);
    }
    const De = [];
    for (const [we, me] of ze)
      De.push({ provider: we, models: me });
    return De;
  });
  function ee(w) {
    return w.startsWith("claude") || w.startsWith("anthropic") ? "Anthropic" : w.startsWith("gemini") || w.startsWith("gemma") ? "Google" : w.startsWith("llama") || w.startsWith("meta-llama") ? "Meta" : w.startsWith("qwen") ? "Alibaba" : w.startsWith("groq/") ? "Groq" : w.startsWith("openai/") || w.startsWith("gpt") ? "OpenAI" : w.includes("/") ? w.split("/")[0] : "Other";
  }
  function te(w) {
    u()(w), h(P, !1), h($, "");
  }
  function $e(w) {
    s(N) && !s(N).contains(w.target) && (h(P, !1), h($, ""));
  }
  function he(w) {
    return w.length <= 24 ? w : w.slice(0, 22) + "…";
  }
  Nt(() => (s(P) ? document.addEventListener("click", $e, !0) : document.removeEventListener("click", $e, !0), () => document.removeEventListener("click", $e, !0)));
  var Te = {
    get currentModel() {
      return c();
    },
    set currentModel(w) {
      c(w), pt();
    },
    get onSelect() {
      return u();
    },
    set onSelect(w) {
      u(w), pt();
    },
    get showPhaseInfo() {
      return d();
    },
    set showPhaseInfo(w = !1) {
      d(w), pt();
    },
    get phasePreference() {
      return v();
    },
    set phasePreference(w = null) {
      v(w), pt();
    },
    get phaseAffinity() {
      return _();
    },
    set phaseAffinity(w = "general") {
      _(w), pt();
    },
    get onPinModel() {
      return x();
    },
    set onPinModel(w) {
      x(w), pt();
    },
    get onClearPin() {
      return b();
    },
    set onClearPin(w) {
      b(w), pt();
    },
    get phaseName() {
      return y();
    },
    set phaseName(w = "") {
      y(w), pt();
    }
  }, Q = Kl(), Ie = i(Q);
  Ie.__click = () => h(P, !s(P));
  var _e = i(Ie);
  {
    var Je = (w) => {
      var G = Ll(), Oe = it(G), ze = i(Oe, !0);
      n(Oe);
      var De = l(Oe, 2);
      {
        var we = (ae) => {
          var ve = Ol();
          ve.textContent = "•", f(ae, ve);
        }, me = /* @__PURE__ */ ge(() => pn(c()) === "code");
        I(De, (ae) => {
          s(me) && ae(we);
        });
      }
      D((ae) => E(ze, ae), [() => he(c())]), f(w, G);
    }, xe = (w) => {
      var G = Rl();
      f(w, G);
    };
    I(_e, (w) => {
      c() ? w(Je) : w(xe, !1);
    });
  }
  var qe = l(_e, 2), Ue = i(qe, !0);
  n(qe), n(Ie);
  var Ke = l(Ie, 2);
  {
    var Ne = (w) => {
      var G = Yl(), Oe = i(G), ze = i(Oe);
      ht(ze), n(Oe);
      var De = l(Oe, 2);
      {
        var we = (ye) => {
          var O = jl(), F = i(O), Ce = l(i(F)), Re = i(Ce, !0);
          n(Ce);
          var Ge = l(Ce, 2);
          {
            var S = (V) => {
              var X = Fl();
              f(V, X);
            };
            I(Ge, (V) => {
              _() === "code" && V(S);
            });
          }
          n(F);
          var k = l(F, 2);
          {
            var W = (V) => {
              var X = Vl();
              X.__click = () => b()?.(), f(V, X);
            };
            I(k, (V) => {
              v() && V(W);
            });
          }
          n(O), D(() => E(Re, y())), f(ye, O);
        };
        I(De, (ye) => {
          d() && y() && ye(we);
        });
      }
      var me = l(De, 2), ae = i(me);
      {
        var ve = (ye) => {
          var O = Bl(), F = i(O, !0);
          n(O), D(() => E(F, r().length === 0 ? "No models available" : "No matching models")), f(ye, O);
        };
        I(ae, (ye) => {
          s(C).length === 0 && ye(ve);
        });
      }
      var re = l(ae, 2);
      We(re, 17, () => s(C), ot, (ye, O) => {
        var F = Gl(), Ce = i(F), Re = i(Ce, !0);
        n(Ce);
        var Ge = l(Ce, 2);
        We(Ge, 17, () => s(O).models, ot, (S, k) => {
          const W = /* @__PURE__ */ ge(() => s(k) === c()), V = /* @__PURE__ */ ge(() => s(k) === v());
          var X = zl();
          X.__click = () => te(s(k));
          var oe = i(X), Z = i(oe, !0);
          n(oe);
          var ke = l(oe, 2);
          {
            var T = (ce) => {
              var pe = Hl();
              pe.textContent = "• code", f(ce, pe);
            }, H = /* @__PURE__ */ ge(() => pn(s(k)) === "code");
            I(ke, (ce) => {
              s(H) && ce(T);
            });
          }
          var B = l(ke, 2);
          {
            var ue = (ce) => {
              var pe = Wl();
              pe.textContent = "📌", f(ce, pe);
            };
            I(B, (ce) => {
              s(V) && ce(ue);
            });
          }
          var ne = l(B, 2);
          {
            var ie = (ce) => {
              var pe = ql();
              pe.textContent = "✓", f(ce, pe);
            };
            I(ne, (ce) => {
              s(W) && ce(ie);
            });
          }
          var fe = l(ne, 2);
          {
            var Le = (ce) => {
              var pe = Ul();
              pe.__click = (He) => {
                He.stopPropagation(), x()?.(s(k));
              }, D(() => Rt(pe, "title", `Pin for ${y() ?? ""} phase`)), f(ce, pe);
            };
            I(fe, (ce) => {
              d() && x() && !s(V) && ce(Le);
            });
          }
          n(X), D(() => {
            Fe(X, 1, `w-full text-left px-2 py-1.5 text-[11px] flex items-center gap-1.5
									transition-colors cursor-pointer
									${s(W) ? "bg-hecate-600/20 text-hecate-300" : "text-surface-200 hover:bg-surface-700"}`), E(Z, s(k));
          }), f(S, X);
        }), n(F), D(() => E(Re, s(O).provider)), f(ye, F);
      }), n(me), n(G), lt(ze, () => s($), (ye) => h($, ye)), f(w, G);
    };
    I(Ke, (w) => {
      s(P) && w(Ne);
    });
  }
  n(Q), Yr(Q, (w) => h(N, w), () => s(N)), D(() => {
    Rt(Ie, "title", c() ?? "No model selected"), E(Ue, s(P) ? "▲" : "▼");
  }), f(e, Q);
  var K = Mt(Te);
  return o(), K;
}
Ft(["click"]);
Vt(
  ra,
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
var Jl = /* @__PURE__ */ p(`<div class="flex justify-end"><div class="max-w-[85%] rounded-lg px-3 py-2 text-[11px]
							bg-hecate-600/20 text-surface-100 border border-hecate-600/20"><div class="whitespace-pre-wrap break-words"> </div></div></div>`), Ql = /* @__PURE__ */ p(`<details class="mb-1.5 group"><summary class="text-[10px] text-surface-400 cursor-pointer hover:text-surface-300
											select-none flex items-center gap-1"><span class="text-[9px] transition-transform group-open:rotate-90"></span> Reasoning</summary> <div class="mt-1 pl-2 border-l-2 border-surface-600 text-[10px] text-surface-400
											whitespace-pre-wrap break-words max-h-32 overflow-y-auto"> </div></details>`), Xl = /* @__PURE__ */ p('<div class="whitespace-pre-wrap break-words"> </div>'), Zl = /* @__PURE__ */ p('<div class="flex justify-start"><div></div></div>'), eu = /* @__PURE__ */ p(`<details class="group"><summary class="text-[10px] text-surface-500 cursor-pointer hover:text-surface-400
										select-none flex items-center gap-1"><span class="text-[9px] transition-transform group-open:rotate-90"></span> Show reasoning</summary> <div class="mt-1 pl-2 border-l-2 border-surface-600 text-[10px] text-surface-400
										whitespace-pre-wrap break-words max-h-32 overflow-y-auto"> <span class="inline-block w-1 h-3 bg-accent-400/50 animate-pulse ml-0.5"></span></div></details>`), tu = /* @__PURE__ */ p('<div class="flex items-center gap-2 text-surface-400 mb-1"><span class="flex gap-1"><span class="w-1.5 h-1.5 rounded-full bg-accent-500/60 animate-bounce" style="animation-delay: 0ms"></span> <span class="w-1.5 h-1.5 rounded-full bg-accent-500/60 animate-bounce" style="animation-delay: 150ms"></span> <span class="w-1.5 h-1.5 rounded-full bg-accent-500/60 animate-bounce" style="animation-delay: 300ms"></span></span> <span class="text-[10px] text-accent-400/70">Reasoning...</span></div> <!>', 1), ru = /* @__PURE__ */ p(`<details class="mb-1.5 group"><summary class="text-[10px] text-surface-400 cursor-pointer hover:text-surface-300
										select-none flex items-center gap-1"><span class="text-[9px] transition-transform group-open:rotate-90"></span> Reasoning</summary> <div class="mt-1 pl-2 border-l-2 border-surface-600 text-[10px] text-surface-400
										whitespace-pre-wrap break-words max-h-32 overflow-y-auto"> </div></details>`), su = /* @__PURE__ */ p('<!> <div class="whitespace-pre-wrap break-words"> <span class="inline-block w-1.5 h-3 bg-hecate-400 animate-pulse ml-0.5"></span></div>', 1), au = /* @__PURE__ */ p('<div class="flex items-center gap-1.5 text-surface-400"><span class="animate-bounce" style="animation-delay: 0ms">.</span> <span class="animate-bounce" style="animation-delay: 150ms">.</span> <span class="animate-bounce" style="animation-delay: 300ms">.</span></div>'), nu = /* @__PURE__ */ p(`<div class="flex justify-start"><div class="max-w-[85%] rounded-lg px-3 py-2 text-[11px]
						bg-surface-700 text-surface-200 border border-surface-600"><!></div></div>`), iu = /* @__PURE__ */ p('<div class="flex items-center justify-center h-full"><div class="text-center text-surface-400"><div class="text-2xl mb-2"></div> <div class="text-[11px]">The Oracle is preparing...</div></div></div>'), ou = /* @__PURE__ */ p('<span class="text-[10px] text-health-ok"></span>'), cu = /* @__PURE__ */ p('<span class="text-[10px] text-accent-400"></span>'), lu = /* @__PURE__ */ p('<span class="text-[10px] text-surface-400"></span>'), uu = /* @__PURE__ */ p('<span class="text-[10px] text-surface-400">Waiting for Oracle...</span>'), du = /* @__PURE__ */ p('<div class="mt-4 p-2 rounded bg-surface-700 border border-surface-600"><div class="text-[9px] text-surface-400 uppercase tracking-wider mb-1">Brief</div> <div class="text-[11px] text-surface-200"> </div></div>'), vu = /* @__PURE__ */ p('<div class="prose prose-sm prose-invert"><!></div> <!>', 1), fu = /* @__PURE__ */ p(`<div class="flex items-center justify-center h-full"><div class="text-center text-surface-400 max-w-[220px]"><div class="text-2xl mb-2"></div> <div class="text-[11px]">Your vision will take shape here as the Oracle
							gathers context about your venture.</div></div></div>`), pu = /* @__PURE__ */ p('<div class="text-[10px] text-health-err bg-health-err/10 rounded px-2 py-1"> </div>'), hu = /* @__PURE__ */ p(`<div class="space-y-2"><div><label for="repo-path" class="text-[10px] text-surface-400 block mb-1">Repository Path</label> <input id="repo-path" placeholder="~/ventures/my-venture" class="w-full bg-surface-700 border border-surface-600 rounded px-3 py-1.5
								text-[11px] text-surface-100 placeholder-surface-400
								focus:outline-none focus:border-hecate-500"/></div> <!> <button> </button></div>`), _u = /* @__PURE__ */ p('<div class="text-center text-[10px] text-surface-400 py-2"></div>'), xu = /* @__PURE__ */ p('<div class="text-center text-[10px] text-surface-400 py-2">The Oracle will guide you through defining your venture</div>'), gu = /* @__PURE__ */ p(`<div class="flex h-full overflow-hidden"><div class="flex flex-col overflow-hidden"><div class="flex items-center gap-2 px-4 py-2.5 border-b border-surface-600 shrink-0"><span class="text-hecate-400"></span> <span class="text-xs font-semibold text-surface-100">The Oracle</span> <span class="text-[10px] text-surface-400">Vision Architect</span> <div class="flex-1"></div> <!></div> <div class="flex-1 overflow-y-auto p-4 space-y-3"><!> <!> <!></div> <div class="border-t border-surface-600 p-3 shrink-0"><div class="flex gap-2"><textarea class="flex-1 bg-surface-700 border border-surface-600 rounded-lg px-3 py-2
						text-[11px] text-surface-100 placeholder-surface-400 resize-none
						focus:outline-none focus:border-hecate-500
						disabled:opacity-50 disabled:cursor-not-allowed"></textarea> <button>Send</button></div></div></div>  <div></div> <div class="flex flex-col overflow-hidden flex-1"><div class="flex items-center gap-2 px-4 py-2.5 border-b border-surface-600 shrink-0"><span class="text-surface-400 text-xs"></span> <span class="text-xs font-semibold text-surface-100">Vision Preview</span> <div class="flex-1"></div> <!></div> <div class="flex-1 overflow-y-auto p-4"><!></div> <div class="border-t border-surface-600 p-3 shrink-0"><!></div></div></div>`);
function Vi(e, t) {
  Tt(t, !0);
  const r = () => Ae(bt, "$activeVenture", c), a = () => Ae(Ga, "$aiModel", c), o = () => Ae(ft, "$isLoading", c), [c, u] = Kt(), d = Ei();
  let v = /* @__PURE__ */ se(Wt([])), _ = /* @__PURE__ */ se(""), x = /* @__PURE__ */ se(!1), b = /* @__PURE__ */ se(""), y = /* @__PURE__ */ se(void 0), P = /* @__PURE__ */ se(!1), $ = /* @__PURE__ */ se(""), N = /* @__PURE__ */ se(""), C = /* @__PURE__ */ se(null), ee = /* @__PURE__ */ se(null), te = /* @__PURE__ */ se(65), $e = /* @__PURE__ */ se(!1), he = /* @__PURE__ */ se(void 0);
  function Te(g) {
    let m = g.replace(/```markdown\n[\s\S]*?```/g, "◇ Vision updated ↗");
    return m = m.replace(/```markdown\n[\s\S]*$/, "◇ Synthesizing vision... ↗"), m;
  }
  function Q(g) {
    const m = Te(g), L = [];
    let A = m;
    for (; A.length > 0; ) {
      const M = A.indexOf("<think>");
      if (M === -1) {
        A.trim() && L.push({ type: "text", content: A });
        break;
      }
      if (M > 0) {
        const R = A.slice(0, M);
        R.trim() && L.push({ type: "text", content: R });
      }
      const q = A.indexOf("</think>", M);
      if (q === -1) {
        const R = A.slice(M + 7);
        R.trim() && L.push({ type: "think", content: R });
        break;
      }
      const U = A.slice(M + 7, q);
      U.trim() && L.push({ type: "think", content: U }), A = A.slice(q + 8);
    }
    return L.length > 0 ? L : [{ type: "text", content: m }];
  }
  function Ie(g) {
    return g.includes("<think>") && !g.includes("</think>");
  }
  function _e(g) {
    const m = Te(g);
    return m.includes("</think>") ? (m.split("</think>").pop() || "").trim() : m.includes("<think>") ? "" : m;
  }
  function Je(g) {
    const m = Te(g), L = m.indexOf("<think>");
    if (L === -1) return "";
    const A = m.indexOf("</think>");
    return A === -1 ? m.slice(L + 7) : m.slice(L + 7, A);
  }
  let xe = /* @__PURE__ */ ge(() => {
    for (let g = s(v).length - 1; g >= 0; g--)
      if (s(v)[g].role === "assistant") {
        const m = s(v)[g].content.match(/```markdown\n([\s\S]*?)```/);
        if (m) return m[1].trim();
      }
    if (s(b)) {
      const g = s(b).match(/```markdown\n([\s\S]*?)```/);
      if (g) return g[1].trim();
      const m = s(b).match(/```markdown\n([\s\S]*)$/);
      if (m) return m[1].trim();
    }
    return null;
  }), qe = /* @__PURE__ */ ge(() => s(xe) !== null && !s(xe).includes("(Not yet explored)") && !s(xe).includes("*(Hypothetical)*")), Ue = /* @__PURE__ */ ge(() => {
    if (!s(xe)) return null;
    const g = s(xe).match(/<!--\s*brief:\s*(.*?)\s*-->/);
    return g ? g[1].trim() : null;
  }), Ke = /* @__PURE__ */ se(null);
  Nt(() => {
    const g = r(), m = g?.venture_id ?? null;
    if (m !== s(Ke) && (h(v, [], !0), h(b, ""), h(x, !1), h($, ""), h(N, ""), h(Ke, m, !0)), g && !s(N)) {
      const L = "~/ventures", A = g.name.toLowerCase().replace(/[^a-z0-9-]/g, "-");
      h(N, `${L}/${A}`);
    }
  }), Nt(() => {
    const g = a();
    s(ee) !== null && s(ee) !== g && (s(C) && (s(C).cancel(), h(C, null)), h(v, [], !0), h(b, ""), h(x, !1)), h(ee, g, !0);
  }), Nt(() => {
    const g = r();
    if (g && s(v).length === 0 && !s(x)) {
      const m = `I just initiated a new venture called "${g.name}". ${g.brief ? `Here's what I know so far: ${g.brief}` : "I need help defining the vision for this venture."}`;
      K(m);
    }
  });
  function Ne() {
    const g = [], m = Ot(Fi);
    m && g.push(m);
    const L = Ot(Ml);
    if (g.push(Nl(L, { venture_name: r()?.name ?? "Unnamed" })), r()) {
      let A = `The venture is called "${r().name}"`;
      r().brief && (A += `. Initial brief: ${r().brief}`), g.push(A);
    }
    return g.join(`

---

`);
  }
  async function K(g) {
    const m = a();
    if (!m || !g.trim() || s(x)) return;
    const L = { role: "user", content: g.trim() };
    h(v, [...s(v), L], !0), h(_, "");
    const A = [], M = Ne();
    M && A.push({ role: "system", content: M }), A.push(...s(v)), h(x, !0), h(b, "");
    let q = "";
    const U = d.stream.chat(m, A);
    h(C, U, !0), U.onChunk((R) => {
      R.content && (q += R.content, h(b, q, !0));
    }).onDone(async (R) => {
      R.content && (q += R.content);
      const z = {
        role: "assistant",
        content: q || "(empty response)"
      };
      h(v, [...s(v), z], !0), h(b, ""), h(x, !1), h(C, null);
    }).onError((R) => {
      const z = { role: "assistant", content: `Error: ${R}` };
      h(v, [...s(v), z], !0), h(b, ""), h(x, !1), h(C, null);
    });
    try {
      await U.start();
    } catch (R) {
      const z = { role: "assistant", content: `Error: ${String(R)}` };
      h(v, [...s(v), z], !0), h(x, !1);
    }
  }
  async function w() {
    if (!r() || !s(xe) || !s(N).trim()) return;
    h(P, !0), h($, ""), await Ti(r().venture_id, s(N).trim(), s(xe), r().name, s(Ue) ?? void 0) ? (await nr(), await As()) : h($, Ot(ar) || "Failed to scaffold venture repo", !0), h(P, !1);
  }
  let G = /* @__PURE__ */ se(void 0);
  function Oe(g) {
    g.key === "Enter" && !g.shiftKey && (g.preventDefault(), K(s(_)), s(G) && (s(G).style.height = "auto"));
  }
  function ze(g) {
    const m = g.target;
    m.style.height = "auto", m.style.height = Math.min(m.scrollHeight, 150) + "px";
  }
  function De(g) {
    h($e, !0), g.preventDefault();
  }
  function we(g) {
    if (!s($e) || !s(he)) return;
    const m = s(he).getBoundingClientRect(), A = (g.clientX - m.left) / m.width * 100;
    h(te, Math.max(30, Math.min(80, A)), !0);
  }
  function me() {
    h($e, !1);
  }
  Nt(() => {
    s(v), s(b), Ha().then(() => {
      s(y) && (s(y).scrollTop = s(y).scrollHeight);
    });
  });
  function ae(g) {
    return g.replace(/<!--.*?-->/gs, "").replace(/^### (.*$)/gm, '<h3 class="text-xs font-semibold text-surface-100 mt-3 mb-1">$1</h3>').replace(/^## (.*$)/gm, '<h2 class="text-sm font-semibold text-hecate-300 mt-4 mb-1.5">$1</h2>').replace(/^# (.*$)/gm, '<h1 class="text-base font-bold text-surface-100 mb-2">$1</h1>').replace(/^(\d+)\.\s+(.*$)/gm, '<div class="text-[11px] text-surface-200 ml-3 mb-1"><span class="text-surface-400 mr-1.5">$1.</span>$2</div>').replace(/^\- (.*$)/gm, '<div class="text-[11px] text-surface-200 ml-3 mb-1"><span class="text-surface-400 mr-1.5">&bull;</span>$1</div>').replace(/\*\*(.*?)\*\*/g, '<strong class="text-surface-100">$1</strong>').replace(/\*(.*?)\*/g, '<em class="text-surface-300">$1</em>').replace(/\n\n/g, "<br/><br/>").trim();
  }
  var ve = gu();
  ve.__mousemove = we, ve.__mouseup = me;
  var re = i(ve), ye = i(re), O = i(ye);
  O.textContent = "◇";
  var F = l(O, 8);
  ra(F, {
    get currentModel() {
      return a();
    },
    onSelect: (g) => Ya(g)
  }), n(ye);
  var Ce = l(ye, 2), Re = i(Ce);
  We(Re, 17, () => s(v), ot, (g, m) => {
    var L = Nr(), A = it(L);
    {
      var M = (U) => {
        var R = Jl(), z = i(R), de = i(z), Se = i(de, !0);
        n(de), n(z), n(R), D(() => E(Se, s(m).content)), f(U, R);
      }, q = (U) => {
        var R = Zl(), z = i(R);
        We(z, 21, () => Q(s(m).content), ot, (de, Se) => {
          var Pe = Nr(), Be = it(Pe);
          {
            var Me = (Ve) => {
              var Ye = Ql(), nt = i(Ye), ct = i(nt);
              ct.textContent = "▶", wt(), n(nt);
              var ut = l(nt, 2), jt = i(ut, !0);
              n(ut), n(Ye), D((Jt) => E(jt, Jt), [() => s(Se).content.trim()]), f(Ve, Ye);
            }, Ee = (Ve) => {
              var Ye = Xl(), nt = i(Ye, !0);
              n(Ye), D((ct) => E(nt, ct), [() => s(Se).content.trim()]), f(Ve, Ye);
            };
            I(Be, (Ve) => {
              s(Se).type === "think" ? Ve(Me) : Ve(Ee, !1);
            });
          }
          f(de, Pe);
        }), n(z), n(R), D(
          (de) => Fe(z, 1, `max-w-[85%] rounded-lg px-3 py-2 text-[11px]
							bg-surface-700 text-surface-200 border border-surface-600
							${de ?? ""}`),
          [
            () => s(m).content.startsWith("Error:") ? "border-health-err/30 text-health-err" : ""
          ]
        ), f(U, R);
      };
      I(A, (U) => {
        s(m).role === "user" ? U(M) : s(m).role === "assistant" && U(q, 1);
      });
    }
    f(g, L);
  });
  var Ge = l(Re, 2);
  {
    var S = (g) => {
      var m = nu(), L = i(m), A = i(L);
      {
        var M = (z) => {
          var de = tu(), Se = l(it(de), 2);
          {
            var Pe = (Me) => {
              var Ee = eu(), Ve = i(Ee), Ye = i(Ve);
              Ye.textContent = "▶", wt(), n(Ve);
              var nt = l(Ve, 2), ct = i(nt, !0);
              wt(), n(nt), n(Ee), D((ut) => E(ct, ut), [
                () => Je(s(b)).trim()
              ]), f(Me, Ee);
            }, Be = /* @__PURE__ */ ge(() => Je(s(b)).trim());
            I(Se, (Me) => {
              s(Be) && Me(Pe);
            });
          }
          f(z, de);
        }, q = /* @__PURE__ */ ge(() => s(b) && Ie(s(b))), U = (z) => {
          var de = su(), Se = it(de);
          {
            var Pe = (Ve) => {
              var Ye = ru(), nt = i(Ye), ct = i(nt);
              ct.textContent = "▶", wt(), n(nt);
              var ut = l(nt, 2), jt = i(ut, !0);
              n(ut), n(Ye), D((Jt) => E(jt, Jt), [
                () => Je(s(b)).trim()
              ]), f(Ve, Ye);
            }, Be = /* @__PURE__ */ ge(() => Je(s(b)).trim());
            I(Se, (Ve) => {
              s(Be) && Ve(Pe);
            });
          }
          var Me = l(Se, 2), Ee = i(Me, !0);
          wt(), n(Me), D((Ve) => E(Ee, Ve), [() => _e(s(b))]), f(z, de);
        }, R = (z) => {
          var de = au();
          f(z, de);
        };
        I(A, (z) => {
          s(q) ? z(M) : s(b) ? z(U, 1) : z(R, !1);
        });
      }
      n(L), n(m), f(g, m);
    };
    I(Ge, (g) => {
      s(x) && g(S);
    });
  }
  var k = l(Ge, 2);
  {
    var W = (g) => {
      var m = iu(), L = i(m), A = i(L);
      A.textContent = "◇", wt(2), n(L), n(m), f(g, m);
    };
    I(k, (g) => {
      s(v).length === 0 && !s(x) && g(W);
    });
  }
  n(Ce), Yr(Ce, (g) => h(y, g), () => s(y));
  var V = l(Ce, 2), X = i(V), oe = i(X);
  Fa(oe), oe.__keydown = Oe, oe.__input = ze, Rt(oe, "rows", 1), Yr(oe, (g) => h(G, g), () => s(G));
  var Z = l(oe, 2);
  Z.__click = () => K(s(_)), n(X), n(V), n(re);
  var ke = l(re, 2);
  ke.__mousedown = De;
  var T = l(ke, 2), H = i(T), B = i(H);
  B.textContent = "📄";
  var ue = l(B, 6);
  {
    var ne = (g) => {
      var m = ou();
      m.textContent = "● Complete", f(g, m);
    }, ie = (g) => {
      var m = cu();
      m.textContent = "◐ Drafting...", f(g, m);
    }, fe = (g) => {
      var m = lu();
      m.textContent = "◐ Listening...", f(g, m);
    }, Le = (g) => {
      var m = uu();
      f(g, m);
    };
    I(ue, (g) => {
      s(qe) ? g(ne) : s(xe) ? g(ie, 1) : s(x) ? g(fe, 2) : g(Le, !1);
    });
  }
  n(H);
  var ce = l(H, 2), pe = i(ce);
  {
    var He = (g) => {
      var m = vu(), L = it(m), A = i(L);
      rc(A, () => ae(s(xe))), n(L);
      var M = l(L, 2);
      {
        var q = (U) => {
          var R = du(), z = l(i(R), 2), de = i(z, !0);
          n(z), n(R), D(() => E(de, s(Ue))), f(U, R);
        };
        I(M, (U) => {
          s(Ue) && U(q);
        });
      }
      f(g, m);
    }, je = (g) => {
      var m = fu(), L = i(m), A = i(L);
      A.textContent = "📄", wt(2), n(L), n(m), f(g, m);
    };
    I(pe, (g) => {
      s(xe) ? g(He) : g(je, !1);
    });
  }
  n(ce);
  var le = l(ce, 2), j = i(le);
  {
    var J = (g) => {
      var m = hu(), L = i(m), A = l(i(L), 2);
      ht(A), n(L);
      var M = l(L, 2);
      {
        var q = (z) => {
          var de = pu(), Se = i(de, !0);
          n(de), D(() => E(Se, s($))), f(z, de);
        };
        I(M, (z) => {
          s($) && z(q);
        });
      }
      var U = l(M, 2);
      U.__click = w;
      var R = i(U, !0);
      n(U), n(m), D(
        (z, de) => {
          U.disabled = z, Fe(U, 1, `w-full px-3 py-2 rounded-lg text-xs font-medium transition-colors
							${de ?? ""}`), E(R, s(P) ? "Scaffolding..." : "Scaffold Venture");
        },
        [
          () => s(P) || o() || !s(N).trim(),
          () => s(P) || o() || !s(N).trim() ? "bg-surface-600 text-surface-400 cursor-not-allowed" : "bg-hecate-600 text-surface-50 hover:bg-hecate-500"
        ]
      ), lt(A, () => s(N), (z) => h(N, z)), f(g, m);
    }, Y = (g) => {
      var m = _u();
      m.textContent = "Vision is taking shape — keep exploring with the Oracle", f(g, m);
    }, be = (g) => {
      var m = xu();
      f(g, m);
    };
    I(j, (g) => {
      s(qe) ? g(J) : s(xe) ? g(Y, 1) : g(be, !1);
    });
  }
  n(le), n(T), n(ve), Yr(ve, (g) => h(he, g), () => s(he)), D(
    (g, m) => {
      mr(re, `width: ${s(te) ?? ""}%`), Rt(oe, "placeholder", s(x) ? "Oracle is thinking..." : "Describe your venture..."), oe.disabled = s(x) || !a(), Z.disabled = g, Fe(Z, 1, `px-3 rounded-lg text-[11px] transition-colors self-end
						${m ?? ""}`), Fe(ke, 1, `w-1 cursor-col-resize shrink-0 transition-colors
			${s($e) ? "bg-hecate-500" : "bg-surface-600 hover:bg-surface-500"}`);
    },
    [
      () => s(x) || !s(_).trim() || !a(),
      () => s(x) || !s(_).trim() || !a() ? "bg-surface-600 text-surface-400 cursor-not-allowed" : "bg-hecate-600 text-surface-50 hover:bg-hecate-500"
    ]
  ), mt("mouseleave", ve, me), lt(oe, () => s(_), (g) => h(_, g)), f(e, ve), Mt(), u();
}
Ft([
  "mousemove",
  "mouseup",
  "keydown",
  "input",
  "click",
  "mousedown"
]);
Vt(Vi, {}, [], [], { mode: "open" });
var bu = /* @__PURE__ */ p("<div></div>"), mu = /* @__PURE__ */ p("<!> <div><span> </span> <span> </span></div>", 1), yu = /* @__PURE__ */ p('<span class="text-[10px] text-surface-400"> </span>'), wu = /* @__PURE__ */ p("<span> </span>"), $u = /* @__PURE__ */ p(
  `<button title="Toggle event stream viewer">Stream</button> <button class="text-[9px] px-2 py-0.5 rounded ml-1
						text-surface-400 hover:text-health-warn hover:bg-surface-700 transition-colors" title="Shelve storm">Shelve</button>`,
  1
), ku = /* @__PURE__ */ p(`<button class="flex items-center gap-1 text-[10px] px-2 py-1 rounded
										text-surface-400 hover:text-hecate-300
										hover:bg-hecate-600/10 transition-colors"><span> </span> <span> </span></button>`), Eu = /* @__PURE__ */ p(`<div class="flex items-center justify-center h-full"><div class="text-center max-w-lg mx-4"><div class="text-4xl mb-4 text-es-event"></div> <h2 class="text-lg font-semibold text-surface-100 mb-3">Big Picture Event Storming</h2> <p class="text-xs text-surface-400 leading-relaxed mb-6">Discover the domain landscape by storming events onto the board.
						Start with a 10-minute high octane phase where everyone
						(including AI agents) throws domain events as fast as possible. <br/><br/> Volume over quality. The thick stacks reveal what matters.
						Natural clusters become your divisions (bounded contexts).</p> <div class="flex flex-col items-center gap-4"><button class="px-6 py-3 rounded-lg text-sm font-medium
								bg-es-event text-surface-50 hover:bg-es-event/90
								transition-colors shadow-lg shadow-es-event/20"></button> <div class="flex gap-2"></div></div></div></div>`), Cu = /* @__PURE__ */ p(`<div class="group relative px-3 py-2 rounded text-xs
									bg-es-event/15 border border-es-event/30 text-surface-100
									hover:border-es-event/50 transition-colors"><span> </span> <span class="text-[8px] text-es-event/60 ml-1.5"> </span> <button class="absolute -top-1 -right-1 w-4 h-4 rounded-full
										bg-surface-700 border border-surface-600
										text-surface-400 hover:text-health-err
										text-[8px] flex items-center justify-center
										opacity-0 group-hover:opacity-100 transition-opacity"></button></div>`), Su = /* @__PURE__ */ p('<div class="text-surface-500 text-xs italic">Start throwing events! Type below or ask an AI agent...</div>'), Au = /* @__PURE__ */ p(`<button class="flex items-center gap-1 text-[9px] px-1.5 py-0.5 rounded
										text-surface-400 hover:text-hecate-300
										hover:bg-hecate-600/10 transition-colors"><span> </span> <span> </span></button>`), Du = /* @__PURE__ */ p(`<div class="flex flex-col h-full"><div class="flex-1 overflow-y-auto p-4"><div class="flex flex-wrap gap-2 content-start"><!> <!></div></div> <div class="border-t border-surface-600 p-3 shrink-0"><div class="flex gap-2 mb-2"><input placeholder="Type a domain event (past tense)... e.g., order_placed" class="flex-1 bg-surface-700 border border-es-event/30 rounded px-3 py-2
								text-xs text-surface-100 placeholder-surface-400
								focus:outline-none focus:border-es-event"/> <button>Add</button></div> <div class="flex items-center justify-between"><div class="flex gap-1.5"></div> <button class="text-[10px] px-3 py-1 rounded
								bg-surface-700 text-surface-300
								hover:text-surface-100 hover:bg-surface-600 transition-colors"></button></div></div></div>`), Pu = /* @__PURE__ */ p('<span class="text-[8px] px-1 rounded bg-es-event/20 text-es-event"> </span>'), Tu = /* @__PURE__ */ p(`<div draggable="true" class="group flex items-center gap-1.5 px-2 py-1.5 rounded text-[11px]
											bg-es-event/15 border border-es-event/30 text-surface-100
											cursor-grab active:cursor-grabbing hover:border-es-event/50"><span class="flex-1 truncate"> </span> <!></div>`), Mu = /* @__PURE__ */ p(`<div class="group flex items-center gap-1 px-2 py-1 rounded text-[10px]
														bg-es-event/10 text-surface-200"><span class="flex-1 truncate"> </span> <button class="text-[8px] text-surface-500 hover:text-surface-300
															opacity-0 group-hover:opacity-100" title="Unstack"></button></div>`), Iu = /* @__PURE__ */ p('<div><div class="flex items-center gap-2 mb-2"><span class="text-[10px] font-bold text-es-event"> </span> <span class="text-[9px] text-surface-500 font-mono"> </span></div> <div class="space-y-1"></div></div>'), Nu = /* @__PURE__ */ p(`<div class="col-span-2 text-center py-8 text-surface-500 text-xs
											border border-dashed border-surface-600 rounded-lg">Drag stickies onto each other to create stacks.</div>`), Ou = /* @__PURE__ */ p(`<button class="flex items-center gap-1 text-[9px] px-1.5 py-0.5 rounded
										text-surface-400 hover:text-hecate-300
										hover:bg-hecate-600/10 transition-colors"><span> </span> <span> </span></button>`), Lu = /* @__PURE__ */ p(`<div class="flex flex-col h-full"><div class="flex-1 overflow-y-auto p-4"><p class="text-xs text-surface-400 mb-3">Drag duplicate or related stickies onto each other to form stacks.
						Thick stacks reveal what matters most.</p> <div class="flex gap-4"><div class="w-64 shrink-0"><h3 class="text-[10px] font-semibold text-surface-300 mb-2 uppercase tracking-wider"> </h3> <div class="space-y-1 min-h-[200px] rounded-lg border border-dashed border-surface-600 p-2"></div></div> <div class="flex-1"><h3 class="text-[10px] font-semibold text-surface-300 mb-2 uppercase tracking-wider"> </h3> <div class="grid grid-cols-2 gap-3"><!> <!></div></div></div></div> <div class="border-t border-surface-600 p-3 shrink-0"><div class="flex items-center justify-between"><div class="flex gap-1.5"></div> <button class="text-[10px] px-3 py-1 rounded transition-colors
								bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30"></button></div></div></div>`), Ru = /* @__PURE__ */ p('<button><span></span> <span class="flex-1"> </span> <span class="text-[8px] text-surface-400"> </span></button>'), Fu = /* @__PURE__ */ p('<div class="rounded-lg border border-surface-600 bg-surface-800 p-4"><div class="flex items-center gap-2 mb-3"><span class="text-xs font-semibold text-surface-200"> </span> <div class="flex-1"></div> <button></button></div> <div class="space-y-1.5"></div></div>'), Vu = /* @__PURE__ */ p('<div class="space-y-4 mb-6"></div>'), ju = /* @__PURE__ */ p(`<div class="text-center py-8 text-surface-500 text-xs
									border border-dashed border-surface-600 rounded-lg mb-6">No stacks to groom. All stickies are unique.</div>`), Bu = /* @__PURE__ */ p('<span class="text-[8px] text-es-event ml-1"> </span>'), Hu = /* @__PURE__ */ p(`<span class="text-[10px] px-2 py-1 rounded
												bg-es-event/10 text-surface-200"> <!></span>`), Wu = /* @__PURE__ */ p('<div><h3 class="text-[10px] font-semibold text-surface-300 mb-2 uppercase tracking-wider"> </h3> <div class="flex flex-wrap gap-1.5"></div></div>'), qu = /* @__PURE__ */ p(`<div class="flex flex-col h-full"><div class="flex-1 overflow-y-auto p-4"><div class="max-w-2xl mx-auto"><p class="text-xs text-surface-400 mb-4">For each stack, select the best representative sticky. The winner
							gets the stack's weight (vote count). Other stickies are absorbed.</p> <!> <!></div></div> <div class="border-t border-surface-600 p-3 shrink-0"><div class="flex items-center justify-end"><button class="text-[10px] px-3 py-1 rounded transition-colors
								bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30"></button></div></div></div>`), Uu = /* @__PURE__ */ p('<span class="text-[8px] px-1 rounded bg-es-event/20 text-es-event"> </span>'), zu = /* @__PURE__ */ p(`<div draggable="true" class="group flex items-center gap-1.5 px-2 py-1.5 rounded text-[11px]
											bg-es-event/15 border border-es-event/30 text-surface-100
											cursor-grab active:cursor-grabbing hover:border-es-event/50"><span class="flex-1 truncate"> </span> <!></div>`), Gu = /* @__PURE__ */ p('<div class="text-[10px] text-surface-500 text-center py-4 italic">All events clustered</div>'), Yu = /* @__PURE__ */ p('<span class="text-[8px] text-es-event/60"> </span>'), Ku = /* @__PURE__ */ p(`<div draggable="true" class="group flex items-center gap-1 px-2 py-1 rounded text-[10px]
														bg-es-event/10 text-surface-200
														cursor-grab active:cursor-grabbing"><span class="flex-1 truncate"> </span> <!> <button class="text-[8px] text-surface-500 hover:text-surface-300
															opacity-0 group-hover:opacity-100" title="Remove from cluster"></button></div>`), Ju = /* @__PURE__ */ p('<div><div class="flex items-center gap-2 mb-2"><div class="w-3 h-3 rounded-sm shrink-0"></div> <span class="flex-1 text-xs font-semibold text-surface-100 truncate"> </span> <span class="text-[9px] text-surface-400"> </span> <button class="text-[9px] text-surface-500 hover:text-health-err transition-colors" title="Dissolve cluster"></button></div> <div class="space-y-1"></div></div>'), Qu = /* @__PURE__ */ p(`<div class="col-span-2 text-center py-8 text-surface-500 text-xs
											border border-dashed border-surface-600 rounded-lg">Drag stickies onto each other to create clusters.</div>`), Xu = /* @__PURE__ */ p(`<button class="flex items-center gap-1 text-[9px] px-1.5 py-0.5 rounded
										text-surface-400 hover:text-hecate-300
										hover:bg-hecate-600/10 transition-colors"><span> </span> <span> </span></button>`), Zu = /* @__PURE__ */ p(`<div class="flex flex-col h-full"><div class="flex-1 overflow-y-auto p-4"><p class="text-xs text-surface-400 mb-3">Drag related stickies onto each other to form clusters.
						Clusters become candidate divisions (bounded contexts).</p> <div class="flex gap-4"><div class="w-64 shrink-0"><h3 class="text-[10px] font-semibold text-surface-300 mb-2 uppercase tracking-wider"> </h3> <div class="space-y-1 min-h-[200px] rounded-lg border border-dashed border-surface-600 p-2"><!> <!></div></div> <div class="flex-1"><h3 class="text-[10px] font-semibold text-surface-300 mb-2 uppercase tracking-wider"> </h3> <div class="grid grid-cols-2 gap-3"><!> <!></div></div></div></div> <div class="border-t border-surface-600 p-3 shrink-0"><div class="flex items-center justify-between"><div class="flex gap-1.5"></div> <button></button></div></div></div>`), ed = /* @__PURE__ */ p(`<input class="flex-1 bg-surface-700 border border-surface-500 rounded px-3 py-1.5
													text-sm text-surface-100 focus:outline-none focus:border-hecate-500" placeholder="division_name (snake_case)"/>`), td = /* @__PURE__ */ p('<button title="Click to name"> </button>'), rd = /* @__PURE__ */ p('<span class="text-es-event/50"> </span>'), sd = /* @__PURE__ */ p(`<span class="text-[9px] px-1.5 py-0.5 rounded
													bg-es-event/10 text-es-event/80"> <!></span>`), ad = /* @__PURE__ */ p('<div class="rounded-lg border bg-surface-800 p-4"><div class="flex items-center gap-3 mb-2"><div class="w-4 h-4 rounded"></div> <!> <span class="text-[10px] text-surface-400"> </span></div> <div class="flex flex-wrap gap-1.5 ml-7"></div></div>'), nd = /* @__PURE__ */ p(`<div class="flex flex-col h-full"><div class="flex-1 overflow-y-auto p-4"><div class="max-w-2xl mx-auto"><p class="text-xs text-surface-400 mb-4">Name each cluster as a bounded context (division). These become
							the divisions in your venture. Use snake_case naming.</p> <div class="space-y-3"></div></div></div> <div class="border-t border-surface-600 p-3 shrink-0"><div class="flex items-center justify-end"><button class="text-[10px] px-3 py-1 rounded
								bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30 transition-colors"></button></div></div></div>`), id = /* @__PURE__ */ p('<div class="px-4 py-2 rounded-lg border-2 text-xs font-semibold text-surface-100"> <span class="text-[9px] text-surface-400 ml-1"> </span></div>'), od = /* @__PURE__ */ p(`<div class="flex items-center gap-2 px-3 py-1.5 rounded
												bg-surface-800 border border-surface-600 text-xs"><span class="px-1.5 py-0.5 rounded text-[10px] font-medium"> </span> <span class="text-surface-400"></span> <span class="text-es-event font-mono text-[10px]"> </span> <span class="text-surface-400"></span> <span class="px-1.5 py-0.5 rounded text-[10px] font-medium"> </span> <div class="flex-1"></div> <button class="text-surface-500 hover:text-health-err text-[9px] transition-colors"></button></div>`), cd = /* @__PURE__ */ p('<div class="space-y-1.5 mb-4"></div>'), ld = /* @__PURE__ */ p("<option> </option>"), ud = /* @__PURE__ */ p("<option> </option>"), dd = /* @__PURE__ */ p(`<div class="rounded-lg border border-surface-600 bg-surface-800 p-4"><h4 class="text-[10px] font-semibold text-surface-300 uppercase tracking-wider mb-3">Add Integration Fact</h4> <div class="flex items-end gap-2"><div class="flex-1"><label class="text-[9px] text-surface-400 block mb-1">From (publishes)</label> <select class="w-full bg-surface-700 border border-surface-600 rounded px-2 py-1.5
												text-[10px] text-surface-100 focus:outline-none focus:border-hecate-500"><option>Select...</option><!></select></div> <div class="flex-1"><label class="text-[9px] text-surface-400 block mb-1">Fact name</label> <input placeholder="e.g., order_confirmed" class="w-full bg-surface-700 border border-surface-600 rounded px-2 py-1.5
												text-[10px] text-surface-100 placeholder-surface-400
												focus:outline-none focus:border-hecate-500"/></div> <div class="flex-1"><label class="text-[9px] text-surface-400 block mb-1">To (consumes)</label> <select class="w-full bg-surface-700 border border-surface-600 rounded px-2 py-1.5
												text-[10px] text-surface-100 focus:outline-none focus:border-hecate-500"><option>Select...</option><!></select></div> <button>Add</button></div></div>`), vd = /* @__PURE__ */ p(`<button class="flex items-center gap-1 text-[9px] px-1.5 py-0.5 rounded
										text-surface-400 hover:text-hecate-300
										hover:bg-hecate-600/10 transition-colors"><span> </span> <span> </span></button>`), fd = /* @__PURE__ */ p(`<div class="flex flex-col h-full"><div class="flex-1 overflow-y-auto p-4"><div class="max-w-3xl mx-auto"><p class="text-xs text-surface-400 mb-4">Map how divisions communicate. Each arrow represents an
							integration fact that flows from one context to another.
							This is your Context Map.</p> <div class="mb-6"><div class="flex flex-wrap gap-3 justify-center mb-4"></div> <!></div> <!></div></div> <div class="border-t border-surface-600 p-3 shrink-0"><div class="flex items-center justify-between"><div class="flex gap-2"></div> <button> </button></div></div></div>`), pd = /* @__PURE__ */ p(`<div class="flex items-center justify-center h-full"><div class="text-center max-w-md mx-4"><div class="text-4xl mb-4 text-health-ok"></div> <h2 class="text-lg font-semibold text-surface-100 mb-2">Context Map Complete</h2> <p class="text-xs text-surface-400 mb-4"> </p> <p class="text-xs text-surface-400 mb-6">Select a division from the sidebar to begin Design-Level
						Event Storming in its DnA phase.</p> <button class="text-[10px] px-3 py-1 rounded
							text-surface-400 hover:text-surface-200 hover:bg-surface-700 transition-colors">Reset Board</button></div></div>`), hd = /* @__PURE__ */ p(`<div class="flex items-center justify-center h-full"><div class="text-center max-w-md mx-4"><div class="text-4xl mb-4 text-health-warn"></div> <h2 class="text-lg font-semibold text-surface-100 mb-2">Storm Shelved</h2> <p class="text-xs text-surface-400 mb-6">This storm session has been shelved. You can resume it at any time
						to continue where you left off.</p> <button class="px-6 py-3 rounded-lg text-sm font-medium
							bg-hecate-600 text-surface-50 hover:bg-hecate-500
							transition-colors">Resume Storm</button></div></div>`), _d = /* @__PURE__ */ p('<div class="flex flex-col h-full"><div class="border-b border-surface-600 bg-surface-800/50 px-4 py-2 shrink-0"><div class="flex items-center gap-1"><span class="text-xs text-surface-400 mr-2">Big Picture</span> <!> <div class="flex-1"></div> <!> <!> <!></div></div> <div class="flex-1 overflow-y-auto"><!></div></div>');
function ka(e, t) {
  Tt(t, !0);
  const r = () => Ae(bt, "$activeVenture", $), a = () => Ae(os, "$bigPictureEvents", $), o = () => Ae(ta, "$eventClusters", $), c = () => Ae(Ja, "$factArrows", $), u = () => Ae(jr, "$bigPicturePhase", $), d = () => Ae(Nc, "$bigPictureEventCount", $), v = () => Ae(Ws, "$highOctaneRemaining", $), _ = () => Ae(ya, "$showEventStream", $), x = () => Ae(Pl, "$bigPictureAgents", $), b = () => Ae(Ic, "$stickyStacks", $), y = () => Ae(Mc, "$unclusteredEvents", $), P = () => Ae(wa, "$isLoading", $), [$, N] = Kt();
  let C = /* @__PURE__ */ se(""), ee = /* @__PURE__ */ se(null), te = /* @__PURE__ */ se(""), $e = /* @__PURE__ */ se(null), he = /* @__PURE__ */ se(null), Te = /* @__PURE__ */ se(""), Q = /* @__PURE__ */ se(null), Ie = /* @__PURE__ */ se(Wt({}));
  function _e() {
    return r()?.venture_id ?? "";
  }
  function Je(T) {
    const H = Math.floor(T / 60), B = T % 60;
    return `${H}:${B.toString().padStart(2, "0")}`;
  }
  async function xe(T) {
    T.key === "Enter" && !T.shiftKey && s(C).trim() && (T.preventDefault(), await $a(_e(), s(C)), h(C, ""));
  }
  async function qe(T, H) {
    T.key === "Enter" && s(te).trim() ? (await Bc(_e(), H, s(te).trim()), h(ee, null), h(te, "")) : T.key === "Escape" && h(ee, null);
  }
  function Ue(T) {
    h(ee, T.cluster_id, !0), h(te, T.name ?? "", !0);
  }
  async function Ke() {
    s($e) && s(he) && s($e) !== s(he) && s(Te).trim() && (await Hc(_e(), s($e), s(he), s(Te).trim()), h(Te, ""));
  }
  async function Ne() {
    await Gc(_e());
  }
  function K(T) {
    return a().filter((H) => H.cluster_id === T);
  }
  let w = /* @__PURE__ */ ge(() => a().filter((T) => !T.stack_id));
  function G(T) {
    const H = r(), B = a(), ue = o(), ne = c();
    let ie = T + `

---

`;
    if (H && (ie += `Venture: "${H.name}"`, H.brief && (ie += ` — ${H.brief}`), ie += `

`), B.length > 0 && (ie += `Events on the board:
`, ie += B.map((fe) => `- ${fe.text}${fe.weight > 1 ? ` (x${fe.weight})` : ""}`).join(`
`), ie += `

`), ue.length > 0) {
      ie += `Current clusters (candidate divisions):
`;
      for (const fe of ue) {
        const Le = B.filter((ce) => ce.cluster_id === fe.cluster_id);
        ie += `- ${fe.name ?? "(unnamed)"}: ${Le.map((ce) => ce.text).join(", ") || "(empty)"}
`;
      }
      ie += `
`;
    }
    if (ne.length > 0) {
      ie += `Integration fact arrows:
`;
      for (const fe of ne) {
        const Le = ue.find((pe) => pe.cluster_id === fe.from_cluster)?.name ?? "?", ce = ue.find((pe) => pe.cluster_id === fe.to_cluster)?.name ?? "?";
        ie += `- ${Le} → ${fe.fact_name} → ${ce}
`;
      }
    }
    return ie;
  }
  const Oe = [
    { phase: "storm", label: "Storm", icon: "⚡" },
    { phase: "stack", label: "Stack", icon: "≡" },
    { phase: "groom", label: "Groom", icon: "✂" },
    { phase: "cluster", label: "Cluster", icon: "⭐" },
    { phase: "name", label: "Name", icon: "⬡" },
    { phase: "map", label: "Map", icon: "→" },
    { phase: "promoted", label: "Done", icon: "✓" }
  ];
  Nt(() => {
    const T = r();
    T && At(T.venture_id);
  });
  var ze = _d(), De = i(ze), we = i(De), me = l(i(we), 2);
  We(me, 17, () => Oe, ot, (T, H, B) => {
    const ue = /* @__PURE__ */ ge(() => u() === s(H).phase), ne = /* @__PURE__ */ ge(() => Oe.findIndex((j) => j.phase === u()) > B);
    var ie = mu(), fe = it(ie);
    {
      var Le = (j) => {
        var J = bu();
        D(() => Fe(J, 1, `w-6 h-px ${s(ne) ? "bg-hecate-400/60" : "bg-surface-600"}`)), f(j, J);
      };
      I(fe, (j) => {
        B > 0 && j(Le);
      });
    }
    var ce = l(fe, 2), pe = i(ce), He = i(pe, !0);
    n(pe);
    var je = l(pe, 2), le = i(je, !0);
    n(je), n(ce), D(() => {
      Fe(ce, 1, `flex items-center gap-1 px-2 py-1 rounded text-[10px]
						${s(ue) ? "bg-surface-700 border border-hecate-500/40 text-hecate-300" : s(ne) ? "text-hecate-400/60" : "text-surface-500"}`), E(He, s(H).icon), E(le, s(H).label);
    }), f(T, ie);
  });
  var ae = l(me, 4);
  {
    var ve = (T) => {
      var H = yu(), B = i(H);
      n(H), D(() => E(B, `${d() ?? ""} events`)), f(T, H);
    };
    I(ae, (T) => {
      u() !== "ready" && u() !== "promoted" && u() !== "shelved" && T(ve);
    });
  }
  var re = l(ae, 2);
  {
    var ye = (T) => {
      var H = wu(), B = i(H, !0);
      n(H), D(
        (ue) => {
          Fe(H, 1, `text-sm font-bold tabular-nums ml-2
						${v() <= 60 ? "text-health-err animate-pulse" : v() <= 180 ? "text-health-warn" : "text-es-event"}`), E(B, ue);
        },
        [() => Je(v())]
      ), f(T, H);
    };
    I(re, (T) => {
      u() === "storm" && T(ye);
    });
  }
  var O = l(re, 2);
  {
    var F = (T) => {
      var H = $u(), B = it(H);
      B.__click = () => ya.update((ne) => !ne);
      var ue = l(B, 2);
      ue.__click = () => Uc(_e()), D(() => Fe(B, 1, `text-[9px] px-2 py-0.5 rounded ml-1
						${_() ? "text-hecate-300 bg-hecate-600/20" : "text-surface-400 hover:text-surface-200 hover:bg-surface-700"} transition-colors`)), f(T, H);
    };
    I(O, (T) => {
      u() !== "ready" && u() !== "promoted" && u() !== "shelved" && T(F);
    });
  }
  n(we), n(De);
  var Ce = l(De, 2), Re = i(Ce);
  {
    var Ge = (T) => {
      var H = Eu(), B = i(H), ue = i(B);
      ue.textContent = "⚡";
      var ne = l(ue, 6), ie = i(ne);
      ie.__click = () => Oc(_e()), ie.textContent = "⚡ Start High Octane (10 min)";
      var fe = l(ie, 2);
      We(fe, 5, x, ot, (Le, ce) => {
        var pe = ku();
        pe.__click = () => ur(G(s(ce).prompt), s(ce).id);
        var He = i(pe), je = i(He, !0);
        n(He);
        var le = l(He, 2), j = i(le, !0);
        n(le), n(pe), D(() => {
          Rt(pe, "title", s(ce).description), E(je, s(ce).icon), E(j, s(ce).name);
        }), f(Le, pe);
      }), n(fe), n(ne), n(B), n(H), f(T, H);
    }, S = (T) => {
      var H = Du(), B = i(H), ue = i(B), ne = i(ue);
      We(ne, 1, a, (J) => J.sticky_id, (J, Y) => {
        var be = Cu(), g = i(be), m = i(g, !0);
        n(g);
        var L = l(g, 2), A = i(L, !0);
        n(L);
        var M = l(L, 2);
        M.__click = () => Lc(_e(), s(Y).sticky_id), M.textContent = "✕", n(be), D(() => {
          E(m, s(Y).text), E(A, s(Y).author === "user" ? "" : s(Y).author);
        }), f(J, be);
      });
      var ie = l(ne, 2);
      {
        var fe = (J) => {
          var Y = Su();
          f(J, Y);
        };
        I(ie, (J) => {
          a().length === 0 && J(fe);
        });
      }
      n(ue), n(B);
      var Le = l(B, 2), ce = i(Le), pe = i(ce);
      ht(pe), pe.__keydown = xe;
      var He = l(pe, 2);
      He.__click = async () => {
        s(C).trim() && (await $a(_e(), s(C)), h(C, ""));
      }, n(ce);
      var je = l(ce, 2), le = i(je);
      We(le, 5, x, ot, (J, Y) => {
        var be = Au();
        be.__click = () => ur(G(s(Y).prompt), s(Y).id);
        var g = i(be), m = i(g, !0);
        n(g);
        var L = l(g, 2), A = i(L, !0);
        n(L), n(be), D(() => {
          Rt(be, "title", s(Y).description), E(m, s(Y).icon), E(A, s(Y).role);
        }), f(J, be);
      }), n(le);
      var j = l(le, 2);
      j.__click = () => us(_e(), "stack"), j.textContent = "End Storm → Stack", n(je), n(Le), n(H), D(
        (J, Y) => {
          He.disabled = J, Fe(He, 1, `px-3 py-2 rounded text-xs transition-colors
								${Y ?? ""}`);
        },
        [
          () => !s(C).trim(),
          () => s(C).trim() ? "bg-es-event text-surface-50 hover:bg-es-event/80" : "bg-surface-600 text-surface-400 cursor-not-allowed"
        ]
      ), lt(pe, () => s(C), (J) => h(C, J)), f(T, H);
    }, k = (T) => {
      var H = Lu(), B = i(H), ue = l(i(B), 2), ne = i(ue), ie = i(ne), fe = i(ie);
      n(ie);
      var Le = l(ie, 2);
      We(Le, 21, () => s(w), (L) => L.sticky_id, (L, A) => {
        var M = Tu(), q = i(M), U = i(q, !0);
        n(q);
        var R = l(q, 2);
        {
          var z = (de) => {
            var Se = Pu(), Pe = i(Se);
            n(Se), D(() => E(Pe, `x${s(A).weight ?? ""}`)), f(de, Se);
          };
          I(R, (de) => {
            s(A).weight > 1 && de(z);
          });
        }
        n(M), D(() => E(U, s(A).text)), mt("dragstart", M, () => h(Q, s(A).sticky_id, !0)), mt("dragend", M, () => h(Q, null)), mt("dragover", M, (de) => de.preventDefault()), mt("drop", M, () => {
          s(Q) && s(Q) !== s(A).sticky_id && (xn(_e(), s(Q), s(A).sticky_id), h(Q, null));
        }), f(L, M);
      }), n(Le), n(ne);
      var ce = l(ne, 2), pe = i(ce), He = i(pe);
      n(pe);
      var je = l(pe, 2), le = i(je);
      We(le, 1, () => [...b().entries()], ([L, A]) => L, (L, A) => {
        var M = /* @__PURE__ */ ge(() => ca(s(A), 2));
        let q = () => s(M)[0], U = () => s(M)[1];
        var R = Iu(), z = i(R), de = i(z), Se = i(de);
        n(de);
        var Pe = l(de, 2), Be = i(Pe, !0);
        n(Pe), n(z);
        var Me = l(z, 2);
        We(Me, 21, U, (Ee) => Ee.sticky_id, (Ee, Ve) => {
          var Ye = Mu(), nt = i(Ye), ct = i(nt, !0);
          n(nt);
          var ut = l(nt, 2);
          ut.__click = () => Rc(_e(), s(Ve).sticky_id), ut.textContent = "↩", n(Ye), D(() => E(ct, s(Ve).text)), f(Ee, Ye);
        }), n(Me), n(R), D(
          (Ee) => {
            Fe(R, 1, `rounded-lg border-2 p-3 min-h-[80px] transition-colors
											${s(Q) ? "border-dashed border-hecate-500/50 bg-hecate-600/5" : "border-surface-600 bg-surface-800"}`), E(Se, `${U().length ?? ""}x`), E(Be, Ee);
          },
          [() => q().slice(0, 8)]
        ), mt("dragover", R, (Ee) => Ee.preventDefault()), mt("drop", R, () => {
          s(Q) && U().length > 0 && (xn(_e(), s(Q), U()[0].sticky_id), h(Q, null));
        }), f(L, R);
      });
      var j = l(le, 2);
      {
        var J = (L) => {
          var A = Nu();
          f(L, A);
        };
        I(j, (L) => {
          b().size === 0 && L(J);
        });
      }
      n(je), n(ce), n(ue), n(B);
      var Y = l(B, 2), be = i(Y), g = i(be);
      We(g, 5, () => x().slice(0, 2), ot, (L, A) => {
        var M = Ou();
        M.__click = () => ur(G(s(A).prompt), s(A).id);
        var q = i(M), U = i(q, !0);
        n(q);
        var R = l(q, 2), z = i(R);
        n(R), n(M), D(() => {
          E(U, s(A).icon), E(z, `Ask ${s(A).name ?? ""}`);
        }), f(L, M);
      }), n(g);
      var m = l(g, 2);
      m.__click = () => us(_e(), "groom"), m.textContent = "Groom Stacks →", n(be), n(Y), n(H), D(() => {
        E(fe, `Stickies (${s(w).length ?? ""})`), E(He, `Stacks (${b().size ?? ""})`);
      }), f(T, H);
    }, W = (T) => {
      var H = qu(), B = i(H), ue = i(B), ne = l(i(ue), 2);
      {
        var ie = (le) => {
          var j = Vu();
          We(j, 5, () => [...b().entries()], ([J, Y]) => J, (J, Y) => {
            var be = /* @__PURE__ */ ge(() => ca(s(Y), 2));
            let g = () => s(be)[0], m = () => s(be)[1];
            const L = /* @__PURE__ */ ge(() => s(Ie)[g()]);
            var A = Fu(), M = i(A), q = i(M), U = i(q);
            n(q);
            var R = l(q, 4);
            R.__click = () => {
              s(L) && Fc(_e(), g(), s(L));
            }, R.textContent = "Groom ✂", n(M);
            var z = l(M, 2);
            We(z, 21, m, (de) => de.sticky_id, (de, Se) => {
              var Pe = Ru();
              Pe.__click = () => h(Ie, { ...s(Ie), [g()]: s(Se).sticky_id }, !0);
              var Be = i(Pe), Me = l(Be, 2), Ee = i(Me, !0);
              n(Me);
              var Ve = l(Me, 2), Ye = i(Ve, !0);
              n(Ve), n(Pe), D(() => {
                Fe(Pe, 1, `w-full text-left flex items-center gap-2 px-3 py-2 rounded text-[11px]
														transition-colors
														${s(L) === s(Se).sticky_id ? "bg-hecate-600/20 border border-hecate-500/40 text-hecate-200" : "bg-surface-700/50 border border-transparent text-surface-200 hover:border-surface-500"}`), Fe(Be, 1, `w-3 h-3 rounded-full border-2 shrink-0
															${s(L) === s(Se).sticky_id ? "border-hecate-400 bg-hecate-400" : "border-surface-500"}`), E(Ee, s(Se).text), E(Ye, s(Se).author === "user" ? "" : s(Se).author);
              }), f(de, Pe);
            }), n(z), n(A), D(() => {
              E(U, `Stack (${m().length ?? ""} stickies)`), R.disabled = !s(L), Fe(R, 1, `text-[10px] px-2 py-1 rounded transition-colors
													${s(L) ? "bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30" : "bg-surface-700 text-surface-500 cursor-not-allowed"}`);
            }), f(J, A);
          }), n(j), f(le, j);
        }, fe = (le) => {
          var j = ju();
          f(le, j);
        };
        I(ne, (le) => {
          b().size > 0 ? le(ie) : le(fe, !1);
        });
      }
      var Le = l(ne, 2);
      {
        var ce = (le) => {
          var j = Wu(), J = i(j), Y = i(J);
          n(J);
          var be = l(J, 2);
          We(be, 21, () => s(w), (g) => g.sticky_id, (g, m) => {
            var L = Hu(), A = i(L), M = l(A);
            {
              var q = (U) => {
                var R = Bu(), z = i(R);
                n(R), D(() => E(z, `x${s(m).weight ?? ""}`)), f(U, R);
              };
              I(M, (U) => {
                s(m).weight > 1 && U(q);
              });
            }
            n(L), D(() => E(A, `${s(m).text ?? ""} `)), f(g, L);
          }), n(be), n(j), D(() => E(Y, `Standalone Stickies (${s(w).length ?? ""})`)), f(le, j);
        };
        I(Le, (le) => {
          s(w).length > 0 && le(ce);
        });
      }
      n(ue), n(B);
      var pe = l(B, 2), He = i(pe), je = i(He);
      je.__click = () => us(_e(), "cluster"), je.textContent = "Cluster Events →", n(He), n(pe), n(H), f(T, H);
    }, V = (T) => {
      var H = Zu(), B = i(H), ue = l(i(B), 2), ne = i(ue), ie = i(ne), fe = i(ie);
      n(ie);
      var Le = l(ie, 2), ce = i(Le);
      We(ce, 1, y, (q) => q.sticky_id, (q, U) => {
        var R = zu(), z = i(R), de = i(z, !0);
        n(z);
        var Se = l(z, 2);
        {
          var Pe = (Be) => {
            var Me = Uu(), Ee = i(Me);
            n(Me), D(() => E(Ee, `x${s(U).weight ?? ""}`)), f(Be, Me);
          };
          I(Se, (Be) => {
            s(U).weight > 1 && Be(Pe);
          });
        }
        n(R), D(() => E(de, s(U).text)), mt("dragstart", R, () => h(Q, s(U).sticky_id, !0)), mt("dragend", R, () => h(Q, null)), mt("dragover", R, (Be) => Be.preventDefault()), mt("drop", R, () => {
          s(Q) && s(Q) !== s(U).sticky_id && (gn(_e(), s(Q), s(U).sticky_id), h(Q, null));
        }), f(q, R);
      });
      var pe = l(ce, 2);
      {
        var He = (q) => {
          var U = Gu();
          f(q, U);
        };
        I(pe, (q) => {
          y().length === 0 && q(He);
        });
      }
      n(Le), n(ne);
      var je = l(ne, 2), le = i(je), j = i(le);
      n(le);
      var J = l(le, 2), Y = i(J);
      We(Y, 1, o, (q) => q.cluster_id, (q, U) => {
        const R = /* @__PURE__ */ ge(() => K(s(U).cluster_id));
        var z = Ju(), de = i(z), Se = i(de), Pe = l(Se, 2), Be = i(Pe, !0);
        n(Pe);
        var Me = l(Pe, 2), Ee = i(Me, !0);
        n(Me);
        var Ve = l(Me, 2);
        Ve.__click = () => jc(_e(), s(U).cluster_id), Ve.textContent = "✕", n(de);
        var Ye = l(de, 2);
        We(Ye, 21, () => s(R), (nt) => nt.sticky_id, (nt, ct) => {
          var ut = Ku(), jt = i(ut), Jt = i(jt, !0);
          n(jt);
          var gr = l(jt, 2);
          {
            var cs = (sa) => {
              var aa = Yu(), qi = i(aa);
              n(aa), D(() => E(qi, `x${s(ct).weight ?? ""}`)), f(sa, aa);
            };
            I(gr, (sa) => {
              s(ct).weight > 1 && sa(cs);
            });
          }
          var Ds = l(gr, 2);
          Ds.__click = () => Vc(_e(), s(ct).sticky_id), Ds.textContent = "↩", n(ut), D(() => E(Jt, s(ct).text)), mt("dragstart", ut, () => h(Q, s(ct).sticky_id, !0)), mt("dragend", ut, () => h(Q, null)), f(nt, ut);
        }), n(Ye), n(z), D(() => {
          Fe(z, 1, `rounded-lg border-2 p-3 min-h-[120px] transition-colors
											${s(Q) ? "border-dashed border-hecate-500/50 bg-hecate-600/5" : "border-surface-600 bg-surface-800"}`), mr(z, `border-color: ${s(Q) ? "" : s(U).color + "40"}`), mr(Se, `background-color: ${s(U).color ?? ""}`), E(Be, s(U).name ?? "Unnamed"), E(Ee, s(R).length);
        }), mt("dragover", z, (nt) => nt.preventDefault()), mt("drop", z, () => {
          s(Q) && s(R).length > 0 && (gn(_e(), s(Q), s(R)[0].sticky_id), h(Q, null));
        }), f(q, z);
      });
      var be = l(Y, 2);
      {
        var g = (q) => {
          var U = Qu();
          f(q, U);
        };
        I(be, (q) => {
          o().length === 0 && q(g);
        });
      }
      n(J), n(je), n(ue), n(B);
      var m = l(B, 2), L = i(m), A = i(L);
      We(A, 5, () => x().slice(0, 2), ot, (q, U) => {
        var R = Xu();
        R.__click = () => ur(G(s(U).prompt), s(U).id);
        var z = i(R), de = i(z, !0);
        n(z);
        var Se = l(z, 2), Pe = i(Se);
        n(Se), n(R), D(() => {
          E(de, s(U).icon), E(Pe, `Ask ${s(U).name ?? ""}`);
        }), f(q, R);
      }), n(A);
      var M = l(A, 2);
      M.__click = () => us(_e(), "name"), M.textContent = "Name Divisions →", n(L), n(m), n(H), D(() => {
        E(fe, `Unclustered (${y().length ?? ""})`), E(j, `Clusters (${o().length ?? ""})`), M.disabled = o().length === 0, Fe(M, 1, `text-[10px] px-3 py-1 rounded transition-colors
								${o().length === 0 ? "bg-surface-700 text-surface-500 cursor-not-allowed" : "bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30"}`);
      }), f(T, H);
    }, X = (T) => {
      var H = nd(), B = i(H), ue = i(B), ne = l(i(ue), 2);
      We(ne, 5, o, (ce) => ce.cluster_id, (ce, pe) => {
        const He = /* @__PURE__ */ ge(() => K(s(pe).cluster_id));
        var je = ad(), le = i(je), j = i(le), J = l(j, 2);
        {
          var Y = (A) => {
            var M = ed();
            ht(M), M.__keydown = (q) => qe(q, s(pe).cluster_id), mt("blur", M, () => h(ee, null)), lt(M, () => s(te), (q) => h(te, q)), f(A, M);
          }, be = (A) => {
            var M = td();
            M.__click = () => Ue(s(pe));
            var q = i(M, !0);
            n(M), D(() => {
              Fe(M, 1, `flex-1 text-left text-sm font-semibold transition-colors
													${s(pe).name ? "text-surface-100 hover:text-hecate-300" : "text-surface-400 italic hover:text-hecate-300"}`), E(q, s(pe).name ?? "Click to name...");
            }), f(A, M);
          };
          I(J, (A) => {
            s(ee) === s(pe).cluster_id ? A(Y) : A(be, !1);
          });
        }
        var g = l(J, 2), m = i(g);
        n(g), n(le);
        var L = l(le, 2);
        We(L, 21, () => s(He), (A) => A.sticky_id, (A, M) => {
          var q = sd(), U = i(q), R = l(U);
          {
            var z = (de) => {
              var Se = rd(), Pe = i(Se);
              n(Se), D(() => E(Pe, `x${s(M).weight ?? ""}`)), f(de, Se);
            };
            I(R, (de) => {
              s(M).weight > 1 && de(z);
            });
          }
          n(q), D(() => E(U, `${s(M).text ?? ""} `)), f(A, q);
        }), n(L), n(je), D(() => {
          mr(je, `border-color: ${s(pe).color ?? ""}40`), mr(j, `background-color: ${s(pe).color ?? ""}`), E(m, `${s(He).length ?? ""} events`);
        }), f(ce, je);
      }), n(ne), n(ue), n(B);
      var ie = l(B, 2), fe = i(ie), Le = i(fe);
      Le.__click = () => us(_e(), "map"), Le.textContent = "Map Integration Facts →", n(fe), n(ie), n(H), f(T, H);
    }, oe = (T) => {
      var H = fd(), B = i(H), ue = i(B), ne = l(i(ue), 2), ie = i(ne);
      We(ie, 5, o, (Y) => Y.cluster_id, (Y, be) => {
        var g = id(), m = i(g), L = l(m), A = i(L);
        n(L), n(g), D(
          (M) => {
            mr(g, `border-color: ${s(be).color ?? ""}; background-color: ${s(be).color ?? ""}15`), E(m, `${s(be).name ?? "Unnamed" ?? ""} `), E(A, `(${M ?? ""})`);
          },
          [() => K(s(be).cluster_id).length]
        ), f(Y, g);
      }), n(ie);
      var fe = l(ie, 2);
      {
        var Le = (Y) => {
          var be = cd();
          We(be, 5, c, (g) => g.arrow_id, (g, m) => {
            const L = /* @__PURE__ */ ge(() => o().find((Ee) => Ee.cluster_id === s(m).from_cluster)), A = /* @__PURE__ */ ge(() => o().find((Ee) => Ee.cluster_id === s(m).to_cluster));
            var M = od(), q = i(M), U = i(q, !0);
            n(q);
            var R = l(q, 2);
            R.textContent = "→";
            var z = l(R, 2), de = i(z, !0);
            n(z);
            var Se = l(z, 2);
            Se.textContent = "→";
            var Pe = l(Se, 2), Be = i(Pe, !0);
            n(Pe);
            var Me = l(Pe, 4);
            Me.__click = () => Wc(_e(), s(m).arrow_id), Me.textContent = "✕", n(M), D(() => {
              mr(q, `color: ${s(L)?.color ?? "#888" ?? ""}; background-color: ${s(L)?.color ?? "#888" ?? ""}15`), E(U, s(L)?.name ?? "?"), E(de, s(m).fact_name), mr(Pe, `color: ${s(A)?.color ?? "#888" ?? ""}; background-color: ${s(A)?.color ?? "#888" ?? ""}15`), E(Be, s(A)?.name ?? "?");
            }), f(g, M);
          }), n(be), f(Y, be);
        };
        I(fe, (Y) => {
          c().length > 0 && Y(Le);
        });
      }
      n(ne);
      var ce = l(ne, 2);
      {
        var pe = (Y) => {
          var be = dd(), g = l(i(be), 2), m = i(g), L = l(i(m), 2), A = i(L);
          A.value = (A.__value = null) ?? "";
          var M = l(A);
          We(M, 1, o, ot, (Be, Me) => {
            var Ee = ld(), Ve = i(Ee, !0);
            n(Ee);
            var Ye = {};
            D(() => {
              E(Ve, s(Me).name ?? "Unnamed"), Ye !== (Ye = s(Me).cluster_id) && (Ee.value = (Ee.__value = s(Me).cluster_id) ?? "");
            }), f(Be, Ee);
          }), n(L), n(m);
          var q = l(m, 2), U = l(i(q), 2);
          ht(U), n(q);
          var R = l(q, 2), z = l(i(R), 2), de = i(z);
          de.value = (de.__value = null) ?? "";
          var Se = l(de);
          We(Se, 1, o, ot, (Be, Me) => {
            var Ee = ud(), Ve = i(Ee, !0);
            n(Ee);
            var Ye = {};
            D(() => {
              E(Ve, s(Me).name ?? "Unnamed"), Ye !== (Ye = s(Me).cluster_id) && (Ee.value = (Ee.__value = s(Me).cluster_id) ?? "");
            }), f(Be, Ee);
          }), n(z), n(R);
          var Pe = l(R, 2);
          Pe.__click = Ke, n(g), n(be), D(
            (Be, Me) => {
              Pe.disabled = Be, Fe(Pe, 1, `px-3 py-1.5 rounded text-[10px] transition-colors shrink-0
											${Me ?? ""}`);
            },
            [
              () => !s($e) || !s(he) || s($e) === s(he) || !s(Te).trim(),
              () => s($e) && s(he) && s($e) !== s(he) && s(Te).trim() ? "bg-hecate-600/20 text-hecate-300 hover:bg-hecate-600/30" : "bg-surface-700 text-surface-500 cursor-not-allowed"
            ]
          ), Bs(L, () => s($e), (Be) => h($e, Be)), lt(U, () => s(Te), (Be) => h(Te, Be)), Bs(z, () => s(he), (Be) => h(he, Be)), f(Y, be);
        };
        I(ce, (Y) => {
          o().length >= 2 && Y(pe);
        });
      }
      n(ue), n(B);
      var He = l(B, 2), je = i(He), le = i(je);
      We(le, 5, () => x().slice(2), ot, (Y, be) => {
        var g = vd();
        g.__click = () => ur(G(s(be).prompt), s(be).id);
        var m = i(g), L = i(m, !0);
        n(m);
        var A = l(m, 2), M = i(A);
        n(A), n(g), D(() => {
          E(L, s(be).icon), E(M, `Ask ${s(be).name ?? ""}`);
        }), f(Y, g);
      }), n(le);
      var j = l(le, 2);
      j.__click = Ne;
      var J = i(j, !0);
      n(j), n(je), n(He), n(H), D(() => {
        j.disabled = P(), Fe(j, 1, `text-[10px] px-4 py-1.5 rounded font-medium transition-colors
								${P() ? "bg-surface-700 text-surface-500 cursor-not-allowed" : "bg-hecate-600 text-surface-50 hover:bg-hecate-500"}`), E(J, P() ? "Promoting..." : "Promote to Divisions");
      }), f(T, H);
    }, Z = (T) => {
      var H = pd(), B = i(H), ue = i(B);
      ue.textContent = "✓";
      var ne = l(ue, 4), ie = i(ne);
      n(ne);
      var fe = l(ne, 4);
      fe.__click = function(...Le) {
        Yc?.apply(this, Le);
      }, n(B), n(H), D(() => E(ie, `${o().length ?? ""} divisions identified from
						${d() ?? ""} domain events, with
						${c().length ?? ""} integration fact${c().length !== 1 ? "s" : ""} mapped.`)), f(T, H);
    }, ke = (T) => {
      var H = hd(), B = i(H), ue = i(B);
      ue.textContent = "⏸";
      var ne = l(ue, 6);
      ne.__click = () => zc(_e()), n(B), n(H), f(T, H);
    };
    I(Re, (T) => {
      u() === "ready" ? T(Ge) : u() === "storm" ? T(S, 1) : u() === "stack" ? T(k, 2) : u() === "groom" ? T(W, 3) : u() === "cluster" ? T(V, 4) : u() === "name" ? T(X, 5) : u() === "map" ? T(oe, 6) : u() === "promoted" ? T(Z, 7) : u() === "shelved" && T(ke, 8);
    });
  }
  n(Ce), n(ze), f(e, ze), Mt(), N();
}
Ft(["click", "keydown"]);
Vt(ka, {}, [], [], { mode: "open" });
const ir = Ze([]), Xa = Ze(null), xd = Sr(ir, (e) => {
  const t = /* @__PURE__ */ new Set();
  for (const r of e)
    r.aggregate && t.add(r.aggregate);
  return Array.from(t).sort();
}), gd = Sr(ir, (e) => {
  const t = /* @__PURE__ */ new Map(), r = [];
  for (const a of e)
    if (a.aggregate) {
      const o = t.get(a.aggregate) || [];
      o.push(a), t.set(a.aggregate, o);
    } else
      r.push(a);
  return { grouped: t, ungrouped: r };
});
function bd(e, t, r = "human") {
  const a = crypto.randomUUID(), o = {
    id: a,
    name: e.trim(),
    aggregate: t?.trim() || void 0,
    execution: r,
    policies: [],
    events: []
  };
  return ir.update((c) => [...c, o]), a;
}
function md(e) {
  ir.update((t) => t.filter((r) => r.id !== e));
}
function yd(e, t) {
  ir.update(
    (r) => r.map((a) => a.id === e ? { ...a, ...t } : a)
  );
}
function wd(e, t) {
  ir.update(
    (r) => r.map((a) => a.id === e ? { ...a, execution: t } : a)
  );
}
function $d(e, t) {
  const r = { id: crypto.randomUUID(), text: t.trim() };
  ir.update(
    (a) => a.map(
      (o) => o.id === e ? { ...o, policies: [...o.policies, r] } : o
    )
  );
}
function kd(e, t) {
  ir.update(
    (r) => r.map(
      (a) => a.id === e ? { ...a, policies: a.policies.filter((o) => o.id !== t) } : a
    )
  );
}
function Ed(e, t) {
  const r = { id: crypto.randomUUID(), text: t.trim() };
  ir.update(
    (a) => a.map(
      (o) => o.id === e ? { ...o, events: [...o.events, r] } : o
    )
  );
}
function Cd(e, t) {
  ir.update(
    (r) => r.map(
      (a) => a.id === e ? { ...a, events: a.events.filter((o) => o.id !== t) } : a
    )
  );
}
async function Sd(e, t) {
  try {
    return await Qe().post(`/plannings/${e}/design-aggregate`, t), !0;
  } catch (r) {
    const a = r;
    return Xa.set(a.message || "Failed to design aggregate"), !1;
  }
}
async function Ad(e, t) {
  try {
    return await Qe().post(`/plannings/${e}/design-event`, t), !0;
  } catch (r) {
    const a = r;
    return Xa.set(a.message || "Failed to design event"), !1;
  }
}
async function mn(e, t) {
  try {
    return await Qe().post(`/plannings/${e}/plan-desk`, t), !0;
  } catch (r) {
    const a = r;
    return Xa.set(a.message || "Failed to plan desk"), !1;
  }
}
var Dd = /* @__PURE__ */ p(`<button class="text-[10px] px-2.5 py-1 rounded bg-hecate-600/20 text-hecate-300
					hover:bg-hecate-600/30 transition-colors disabled:opacity-50 disabled:cursor-not-allowed"> </button>`), Pd = /* @__PURE__ */ p(`<button class="text-[10px] px-2 py-1 rounded text-surface-400
					hover:text-hecate-300 hover:bg-hecate-600/10 transition-colors" title="Get AI assistance"></button>`), Td = /* @__PURE__ */ p('<div><div class="flex items-start gap-2"><span class="text-hecate-400 text-sm mt-0.5"> </span> <div class="flex-1 min-w-0"><div class="flex items-center gap-2"><h3 class="text-xs font-semibold text-surface-100"> </h3> <span> </span></div> <p class="text-[11px] text-surface-400 mt-1"> </p></div></div> <div class="flex items-center gap-2 mt-1"><!> <!></div></div>');
function _t(e, t) {
  Tt(t, !0);
  let r = xt(t, "title", 7), a = xt(t, "description", 7), o = xt(t, "icon", 7, "■"), c = xt(t, "status", 7, "pending"), u = xt(t, "aiContext", 7), d = xt(t, "onaction", 7), v = xt(t, "actionLabel", 7, "Execute"), _ = xt(t, "disabled", 7, !1), x = /* @__PURE__ */ ge(() => y(c()));
  function b(K) {
    switch (K) {
      case "active":
        return "border-hecate-600/40";
      case "done":
        return "border-health-ok/30";
      default:
        return "border-surface-600";
    }
  }
  function y(K) {
    switch (K) {
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
    set title(K) {
      r(K), pt();
    },
    get description() {
      return a();
    },
    set description(K) {
      a(K), pt();
    },
    get icon() {
      return o();
    },
    set icon(K = "■") {
      o(K), pt();
    },
    get status() {
      return c();
    },
    set status(K = "pending") {
      c(K), pt();
    },
    get aiContext() {
      return u();
    },
    set aiContext(K) {
      u(K), pt();
    },
    get onaction() {
      return d();
    },
    set onaction(K) {
      d(K), pt();
    },
    get actionLabel() {
      return v();
    },
    set actionLabel(K = "Execute") {
      v(K), pt();
    },
    get disabled() {
      return _();
    },
    set disabled(K = !1) {
      _(K), pt();
    }
  }, $ = Td(), N = i($), C = i(N), ee = i(C, !0);
  n(C);
  var te = l(C, 2), $e = i(te), he = i($e), Te = i(he, !0);
  n(he);
  var Q = l(he, 2), Ie = i(Q, !0);
  n(Q), n($e);
  var _e = l($e, 2), Je = i(_e, !0);
  n(_e), n(te), n(N);
  var xe = l(N, 2), qe = i(xe);
  {
    var Ue = (K) => {
      var w = Dd();
      w.__click = function(...Oe) {
        d()?.apply(this, Oe);
      };
      var G = i(w, !0);
      n(w), D(() => {
        w.disabled = _(), E(G, v());
      }), f(K, w);
    };
    I(qe, (K) => {
      d() && K(Ue);
    });
  }
  var Ke = l(qe, 2);
  {
    var Ne = (K) => {
      var w = Pd();
      w.__click = () => ur(u()), w.textContent = "✦ AI", f(K, w);
    };
    I(Ke, (K) => {
      u() && K(Ne);
    });
  }
  return n(xe), n($), D(
    (K) => {
      Fe($, 1, `rounded-lg bg-surface-800 border ${K ?? ""} p-4 flex flex-col gap-2 transition-colors hover:border-surface-500`), E(ee, o()), E(Te, r()), Fe(Q, 1, `text-[9px] px-1.5 py-0.5 rounded ${s(x).cls ?? ""}`), E(Ie, s(x).text), E(Je, a());
    },
    [() => b(c())]
  ), f(e, $), Mt(P);
}
Ft(["click"]);
Vt(
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
var Md = /* @__PURE__ */ p(`<div class="group/policy flex items-center gap-1 px-2 py-1 rounded-l rounded-r-sm
						bg-es-policy/15 border border-es-policy/30 text-[9px] text-surface-200
						max-w-[160px]"><span class="truncate flex-1"> </span> <button class="text-[7px] text-surface-500 hover:text-health-err
							opacity-0 group-hover/policy:opacity-100 transition-opacity shrink-0"></button></div>`), Id = /* @__PURE__ */ p(`<input class="flex-1 bg-surface-700 border border-es-command/30 rounded px-2 py-0.5
							text-xs font-semibold text-surface-100
							focus:outline-none focus:border-es-command"/>`), Nd = /* @__PURE__ */ p(`<button class="flex-1 text-left text-xs font-semibold text-surface-100
							hover:text-es-command transition-colors" title="Double-click to rename"> </button>`), Od = /* @__PURE__ */ p('<span class="text-[9px] text-es-aggregate/70"> </span>'), Ld = /* @__PURE__ */ p(`<div class="group/event flex items-center gap-1 px-2 py-1 rounded-r rounded-l-sm
						bg-es-event/15 border border-es-event/30 text-[9px] text-surface-200
						max-w-[200px]"><span class="truncate flex-1"> </span> <button class="text-[7px] text-surface-500 hover:text-health-err
							opacity-0 group-hover/event:opacity-100 transition-opacity shrink-0"></button></div>`), Rd = /* @__PURE__ */ p(`<div class="flex items-stretch gap-0 group/card"><div class="flex flex-col items-end gap-1 -mr-2 z-10 pt-1 min-w-[100px]"><!> <input placeholder="+ policy" class="w-24 bg-transparent border border-dashed border-es-policy/20 rounded
					px-1.5 py-0.5 text-[8px] text-surface-400 placeholder-surface-500
					focus:outline-none focus:border-es-policy/40
					opacity-0 group-hover/card:opacity-100 transition-opacity"/></div> <div class="relative flex-1 rounded-lg border-2 border-es-command/40 bg-es-command/10
				px-4 py-3 min-h-[72px] z-20"><div class="flex items-center gap-2 mb-1"><button> </button> <!> <div class="flex items-center gap-1 opacity-0 group-hover/card:opacity-100 transition-opacity"><button class="text-[8px] px-1.5 py-0.5 rounded text-health-ok
							hover:bg-health-ok/10 transition-colors" title="Promote to daemon"></button> <button class="text-[8px] px-1 py-0.5 rounded text-surface-500
							hover:text-health-err hover:bg-health-err/10 transition-colors" title="Remove desk"></button></div></div> <!></div> <div class="flex flex-col items-start gap-1 -ml-2 z-10 pt-1 min-w-[100px]"><!> <input placeholder="+ event" class="w-32 bg-transparent border border-dashed border-es-event/20 rounded
					px-1.5 py-0.5 text-[8px] text-surface-400 placeholder-surface-500
					focus:outline-none focus:border-es-event/40
					opacity-0 group-hover/card:opacity-100 transition-opacity"/></div></div>`), Fd = /* @__PURE__ */ p("<option></option>"), Vd = /* @__PURE__ */ p('<div class="space-y-2"><div class="flex items-center gap-2"><div class="w-3 h-3 rounded-sm bg-es-aggregate/40"></div> <span class="text-[10px] font-semibold text-es-aggregate uppercase tracking-wider"> </span> <div class="flex-1 h-px bg-es-aggregate/20"></div> <span class="text-[9px] text-surface-400"> </span></div> <div class="space-y-3 ml-5"></div></div>'), jd = /* @__PURE__ */ p('<div class="flex items-center gap-2"><span class="text-[10px] font-semibold text-surface-400 uppercase tracking-wider">No Aggregate</span> <div class="flex-1 h-px bg-surface-600"></div></div>'), Bd = /* @__PURE__ */ p("<!> <div></div>", 1), Hd = /* @__PURE__ */ p("<!> <!>", 1), Wd = /* @__PURE__ */ p(`<div class="text-center py-8 text-surface-500 text-xs border border-dashed border-surface-600 rounded-lg">No desk cards yet. Add your first command desk above,
				or ask an AI agent for suggestions.</div>`), qd = /* @__PURE__ */ p(`<button class="rounded-lg border border-surface-600 bg-surface-800/50
							p-3 text-left transition-all hover:border-hecate-500/40
							hover:bg-surface-700/50 group"><div class="flex items-center gap-2 mb-1.5"><span class="text-hecate-400 group-hover:text-hecate-300 transition-colors"> </span> <span class="text-[11px] font-semibold text-surface-100"> </span></div> <div class="text-[10px] text-surface-400 mb-1"> </div> <div class="text-[9px] text-surface-500"> </div></button>`), Ud = /* @__PURE__ */ p(
  `<div class="rounded-lg border border-es-command/20 bg-es-command/5 p-3"><div class="flex items-end gap-2"><div class="flex-1"><label class="text-[9px] text-surface-400 block mb-1">Desk Name (command)</label> <input placeholder="e.g., register_user, process_order" class="w-full bg-surface-700 border border-surface-600 rounded px-2.5 py-1.5
							text-xs text-surface-100 placeholder-surface-400
							focus:outline-none focus:border-es-command/50"/></div> <div class="w-40"><label class="text-[9px] text-surface-400 block mb-1">Aggregate</label> <input placeholder="e.g., user, order" list="existing-aggregates" class="w-full bg-surface-700 border border-surface-600 rounded px-2.5 py-1.5
							text-xs text-surface-100 placeholder-surface-400
							focus:outline-none focus:border-surface-500"/> <datalist id="existing-aggregates"></datalist></div> <div class="w-24"><label class="text-[9px] text-surface-400 block mb-1">Execution</label> <select class="w-full bg-surface-700 border border-surface-600 rounded px-2 py-1.5
							text-xs text-surface-100
							focus:outline-none focus:border-surface-500"><option>Human</option><option>Agent</option><option>Both</option></select></div> <button>+ Desk</button></div></div> <!> <div class="rounded-lg border border-hecate-600/20 bg-hecate-950/20 p-4"><div class="flex items-center gap-2 mb-3"><span class="text-hecate-400"></span> <h4 class="text-xs font-semibold text-surface-100">AI Domain Experts</h4> <span class="text-[10px] text-surface-400">Ask a virtual agent to analyze the domain and suggest desk cards</span></div> <div class="grid grid-cols-2 md:grid-cols-4 gap-2"></div></div> <div><h4 class="text-xs font-semibold text-surface-100 mb-3">Design Tasks</h4> <div class="grid grid-cols-1 md:grid-cols-2 gap-3"><!> <!> <!> <!></div></div>`,
  1
), zd = /* @__PURE__ */ p(`<div class="flex gap-2 items-end mb-4 p-3 rounded bg-surface-700/30"><div class="flex-1"><label for="desk-name" class="text-[10px] text-surface-400 block mb-1">Desk Name</label> <input id="desk-name" placeholder="e.g., register_user" class="w-full bg-surface-700 border border-surface-600 rounded
								px-2.5 py-1.5 text-xs text-surface-100 placeholder-surface-400
								focus:outline-none focus:border-hecate-500/50"/></div> <div class="flex-1"><label for="desk-desc" class="text-[10px] text-surface-400 block mb-1">Description</label> <input id="desk-desc" placeholder="Brief purpose of this desk" class="w-full bg-surface-700 border border-surface-600 rounded
								px-2.5 py-1.5 text-xs text-surface-100 placeholder-surface-400
								focus:outline-none focus:border-hecate-500/50"/></div> <div><label for="desk-dept" class="text-[10px] text-surface-400 block mb-1">Dept</label> <select id="desk-dept" class="bg-surface-700 border border-surface-600 rounded
								px-2 py-1.5 text-xs text-surface-100
								focus:outline-none focus:border-hecate-500/50 cursor-pointer"><option>CMD</option><option>QRY</option><option>PRJ</option></select></div> <button class="px-3 py-1.5 rounded text-xs bg-hecate-600/20 text-hecate-300
							hover:bg-hecate-600/30 transition-colors disabled:opacity-50">Plan</button> <button class="px-3 py-1.5 rounded text-xs text-surface-400 hover:text-surface-100">Cancel</button></div>`), Gd = /* @__PURE__ */ p(
  `<div class="rounded-lg border border-surface-600 bg-surface-800/50 p-4"><div class="flex items-center justify-between mb-3"><h4 class="text-xs font-semibold text-surface-100">Desk Inventory</h4> <button class="text-[10px] px-2 py-0.5 rounded bg-hecate-600/10 text-hecate-300
						hover:bg-hecate-600/20 transition-colors">+ Plan Desk</button></div> <!> <p class="text-[10px] text-surface-400">Desks are individual capabilities within a department. Each desk owns a
				vertical slice: command + event + handler + projection.</p></div> <div><h4 class="text-xs font-semibold text-surface-100 mb-3">Planning Tasks</h4> <div class="grid grid-cols-1 md:grid-cols-2 gap-3"><!> <!> <!> <!></div></div>`,
  1
), Yd = /* @__PURE__ */ p('<div class="p-4 space-y-6"><div class="flex items-center justify-between"><div><h3 class="text-sm font-semibold text-surface-100">Planning</h3> <p class="text-[11px] text-surface-400 mt-0.5">Design aggregates, plan desks, and map dependencies for <span class="text-surface-200"> </span></p></div> <div class="flex items-center gap-1 bg-surface-700/50 rounded-lg p-0.5"><button>Event Storm</button> <button>Desk Inventory</button></div></div> <!></div>');
function ji(e, t) {
  Tt(t, !0);
  const r = () => Ae(ns, "$selectedDivision", v), a = () => Ae(ir, "$deskCards", v), o = () => Ae(xd, "$deskAggregates", v), c = () => Ae(gd, "$deskCardsByAggregate", v), u = () => Ae(Tl, "$designLevelAgents", v), d = () => Ae(ft, "$isLoading", v), [v, _] = Kt(), x = (S, k = fr) => {
    var W = Rd(), V = i(W), X = i(V);
    We(X, 17, () => k().policies, (j) => j.id, (j, J) => {
      var Y = Md(), be = i(Y), g = i(be, !0);
      n(be);
      var m = l(be, 2);
      m.__click = () => kd(k().id, s(J).id), m.textContent = "✕", n(Y), D(() => E(g, s(J).text)), f(j, Y);
    });
    var oe = l(X, 2);
    ht(oe), oe.__keydown = (j) => Je(j, k().id), n(V);
    var Z = l(V, 2), ke = i(Z), T = i(ke);
    T.__click = () => Ke(k());
    var H = i(T, !0);
    n(T);
    var B = l(T, 2);
    {
      var ue = (j) => {
        var J = Id();
        ht(J), J.__keydown = (Y) => {
          Y.key === "Enter" && Ue(k().id), Y.key === "Escape" && h(C, null);
        }, mt("blur", J, () => Ue(k().id)), lt(J, () => s(ee), (Y) => h(ee, Y)), f(j, J);
      }, ne = (j) => {
        var J = Nd();
        J.__dblclick = () => qe(k());
        var Y = i(J, !0);
        n(J), D(() => E(Y, k().name)), f(j, J);
      };
      I(B, (j) => {
        s(C) === k().id ? j(ue) : j(ne, !1);
      });
    }
    var ie = l(B, 2), fe = i(ie);
    fe.__click = () => G(k()), fe.textContent = "↑ promote";
    var Le = l(fe, 2);
    Le.__click = () => md(k().id), Le.textContent = "✕", n(ie), n(ke);
    var ce = l(ke, 2);
    {
      var pe = (j) => {
        var J = Od(), Y = i(J);
        n(J), D(() => E(Y, `■ ${k().aggregate ?? ""}`)), f(j, J);
      };
      I(ce, (j) => {
        k().aggregate && j(pe);
      });
    }
    n(Z);
    var He = l(Z, 2), je = i(He);
    We(je, 17, () => k().events, (j) => j.id, (j, J) => {
      var Y = Ld(), be = i(Y), g = i(be, !0);
      n(be);
      var m = l(be, 2);
      m.__click = () => Cd(k().id, s(J).id), m.textContent = "✕", n(Y), D(() => E(g, s(J).text)), f(j, Y);
    });
    var le = l(je, 2);
    ht(le), le.__keydown = (j) => xe(j, k().id), n(He), n(W), D(
      (j, J, Y) => {
        Fe(T, 1, `text-sm ${j ?? ""}
						hover:scale-110 transition-transform`), Rt(T, "title", `${J ?? ""} — click to cycle`), E(H, Y);
      },
      [
        () => w(k().execution),
        () => K(k().execution),
        () => Ne(k().execution)
      ]
    ), lt(oe, () => s($)[k().id], (j) => s($)[k().id] = j), lt(le, () => s(N)[k().id], (j) => s(N)[k().id] = j), f(S, W);
  };
  let b = /* @__PURE__ */ se(""), y = /* @__PURE__ */ se(""), P = /* @__PURE__ */ se("human"), $ = /* @__PURE__ */ se(Wt({})), N = /* @__PURE__ */ se(Wt({})), C = /* @__PURE__ */ se(null), ee = /* @__PURE__ */ se(""), te = /* @__PURE__ */ se(!1), $e = /* @__PURE__ */ se(""), he = /* @__PURE__ */ se(""), Te = /* @__PURE__ */ se("cmd"), Q = /* @__PURE__ */ se("design");
  function Ie() {
    s(b).trim() && (bd(s(b), s(y) || void 0, s(P)), h(b, ""), h(y, ""), h(P, "human"));
  }
  function _e(S) {
    S.key === "Enter" && !S.shiftKey && s(b).trim() && (S.preventDefault(), Ie());
  }
  function Je(S, k) {
    S.key === "Enter" && s($)[k]?.trim() && (S.preventDefault(), $d(k, s($)[k]), s($)[k] = "");
  }
  function xe(S, k) {
    S.key === "Enter" && s(N)[k]?.trim() && (S.preventDefault(), Ed(k, s(N)[k]), s(N)[k] = "");
  }
  function qe(S) {
    h(C, S.id, !0), h(ee, S.name, !0);
  }
  function Ue(S) {
    s(ee).trim() && yd(S, { name: s(ee).trim() }), h(C, null);
  }
  function Ke(S) {
    const k = ["human", "agent", "both"], W = k.indexOf(S.execution);
    wd(S.id, k[(W + 1) % k.length]);
  }
  function Ne(S) {
    switch (S) {
      case "human":
        return "𝗨";
      case "agent":
        return "⚙";
      case "both":
      case "pair":
        return "✦";
    }
  }
  function K(S) {
    switch (S) {
      case "human":
        return "Interactive (human)";
      case "agent":
        return "Automated (AI agent)";
      case "both":
      case "pair":
        return "Assisted (human + AI)";
    }
  }
  function w(S) {
    switch (S) {
      case "human":
        return "text-es-command";
      case "agent":
        return "text-hecate-400";
      case "both":
      case "pair":
        return "text-phase-crafting";
    }
  }
  async function G(S) {
    if (!r()) return;
    const k = r().division_id;
    await mn(k, {
      desk_name: S.name,
      description: [
        S.execution === "agent" ? "AI-automated" : S.execution === "both" ? "Human+AI assisted" : "Interactive",
        S.policies.length > 0 ? `Policies: ${S.policies.map((W) => W.text).join(", ")}` : "",
        S.events.length > 0 ? `Emits: ${S.events.map((W) => W.text).join(", ")}` : ""
      ].filter(Boolean).join(". "),
      department: "CMD"
    });
    for (const W of S.events)
      await Ad(k, {
        event_name: W.text,
        aggregate_type: S.aggregate || S.name
      });
    S.aggregate && await Sd(k, { aggregate_name: S.aggregate });
  }
  async function Oe() {
    if (!r() || !s($e).trim()) return;
    await mn(r().division_id, {
      desk_name: s($e).trim(),
      description: s(he).trim() || void 0,
      department: s(Te)
    }) && (h($e, ""), h(he, ""), h(te, !1));
  }
  function ze(S) {
    const k = r()?.context_name ?? "this division", W = a(), V = W.map((ke) => ke.name).join(", "), X = W.flatMap((ke) => ke.events.map((T) => T.text)).join(", "), oe = W.flatMap((ke) => ke.policies.map((T) => T.text)).join(", ");
    let Z = `We are doing Design-Level Event Storming for the "${k}" division.

`;
    return Z += `Our board uses command-centric desk cards:
`, Z += `- Each card = a desk (command/slice)
`, Z += `- Left side: policies (grey) = filter/guard conditions
`, Z += `- Right side: events (orange) = what the desk emits
`, Z += `- Cards can be human (interactive), agent (AI), or both

`, V && (Z += `Desks so far: ${V}
`), X && (Z += `Events so far: ${X}
`), oe && (Z += `Policies so far: ${oe}
`), Z += `
${S.prompt}

Please analyze and suggest items for the board.`, Z;
  }
  var De = Yd(), we = i(De), me = i(we), ae = l(i(me), 2), ve = l(i(ae)), re = i(ve, !0);
  n(ve), n(ae), n(me);
  var ye = l(me, 2), O = i(ye);
  O.__click = () => h(Q, "design");
  var F = l(O, 2);
  F.__click = () => h(Q, "plan"), n(ye), n(we);
  var Ce = l(we, 2);
  {
    var Re = (S) => {
      var k = Ud(), W = it(k), V = i(W), X = i(V), oe = l(i(X), 2);
      ht(oe), oe.__keydown = _e, n(X);
      var Z = l(X, 2), ke = l(i(Z), 2);
      ht(ke);
      var T = l(ke, 2);
      We(T, 5, o, ot, (A, M) => {
        var q = Fd(), U = {};
        D(() => {
          U !== (U = s(M)) && (q.value = (q.__value = s(M)) ?? "");
        }), f(A, q);
      }), n(T), n(Z);
      var H = l(Z, 2), B = l(i(H), 2), ue = i(B);
      ue.value = ue.__value = "human";
      var ne = l(ue);
      ne.value = ne.__value = "agent";
      var ie = l(ne);
      ie.value = ie.__value = "both", n(B), n(H);
      var fe = l(H, 2);
      fe.__click = Ie, n(V), n(W);
      var Le = l(W, 2);
      {
        var ce = (A) => {
          const M = /* @__PURE__ */ ge(() => {
            const { grouped: de, ungrouped: Se } = c();
            return { grouped: de, ungrouped: Se };
          });
          var q = Hd(), U = it(q);
          We(U, 17, () => [...s(M).grouped.entries()], ot, (de, Se) => {
            var Pe = /* @__PURE__ */ ge(() => ca(s(Se), 2));
            let Be = () => s(Pe)[0], Me = () => s(Pe)[1];
            var Ee = Vd(), Ve = i(Ee), Ye = l(i(Ve), 2), nt = i(Ye, !0);
            n(Ye);
            var ct = l(Ye, 4), ut = i(ct);
            n(ct), n(Ve);
            var jt = l(Ve, 2);
            We(jt, 21, Me, (Jt) => Jt.id, (Jt, gr) => {
              x(Jt, () => s(gr));
            }), n(jt), n(Ee), D(() => {
              E(nt, Be()), E(ut, `${Me().length ?? ""} desk${Me().length !== 1 ? "s" : ""}`);
            }), f(de, Ee);
          });
          var R = l(U, 2);
          {
            var z = (de) => {
              var Se = Bd(), Pe = it(Se);
              {
                var Be = (Ee) => {
                  var Ve = jd();
                  f(Ee, Ve);
                };
                I(Pe, (Ee) => {
                  s(M).grouped.size > 0 && Ee(Be);
                });
              }
              var Me = l(Pe, 2);
              We(Me, 21, () => s(M).ungrouped, (Ee) => Ee.id, (Ee, Ve) => {
                x(Ee, () => s(Ve));
              }), n(Me), D(() => Fe(Me, 1, `space-y-3 ${s(M).grouped.size > 0 ? "ml-5" : ""}`)), f(de, Se);
            };
            I(R, (de) => {
              s(M).ungrouped.length > 0 && de(z);
            });
          }
          f(A, q);
        }, pe = (A) => {
          var M = Wd();
          f(A, M);
        };
        I(Le, (A) => {
          a().length > 0 ? A(ce) : A(pe, !1);
        });
      }
      var He = l(Le, 2), je = i(He), le = i(je);
      le.textContent = "✦", wt(4), n(je);
      var j = l(je, 2);
      We(j, 5, u, ot, (A, M) => {
        var q = qd();
        q.__click = () => ur(ze(s(M)));
        var U = i(q), R = i(U), z = i(R, !0);
        n(R);
        var de = l(R, 2), Se = i(de, !0);
        n(de), n(U);
        var Pe = l(U, 2), Be = i(Pe, !0);
        n(Pe);
        var Me = l(Pe, 2), Ee = i(Me, !0);
        n(Me), n(q), D(() => {
          E(z, s(M).icon), E(Se, s(M).name), E(Be, s(M).role), E(Ee, s(M).description);
        }), f(A, q);
      }), n(j), n(He);
      var J = l(He, 2), Y = l(i(J), 2), be = i(Y);
      {
        let A = /* @__PURE__ */ ge(() => `Help me design aggregates for the "${r()?.context_name}" division. What are the natural consistency boundaries? What entities accumulate history over time?`);
        _t(be, {
          title: "Design Aggregates",
          description: "Identify aggregate boundaries, define stream patterns and status flags",
          icon: "■",
          get aiContext() {
            return s(A);
          }
        });
      }
      var g = l(be, 2);
      {
        let A = /* @__PURE__ */ ge(() => `Help me define status bit flags for aggregates in the "${r()?.context_name}" division. Each aggregate needs lifecycle states as bit flags (powers of 2).`);
        _t(g, {
          title: "Define Status Flags",
          description: "Design bit flag status fields for each aggregate lifecycle",
          icon: "⚑",
          get aiContext() {
            return s(A);
          }
        });
      }
      var m = l(g, 2);
      {
        let A = /* @__PURE__ */ ge(() => `Help me identify read models for the "${r()?.context_name}" division. What queries will users run? What data views are needed?`);
        _t(m, {
          title: "Map Read Models",
          description: "Identify what queries users will run and what data they need",
          icon: "▶",
          get aiContext() {
            return s(A);
          }
        });
      }
      var L = l(m, 2);
      {
        let A = /* @__PURE__ */ ge(() => `Help me create a domain glossary for the "${r()?.context_name}" division. Define key terms, bounded context boundaries, and ubiquitous language.`);
        _t(L, {
          title: "Domain Glossary",
          description: "Document ubiquitous language and bounded context definitions",
          icon: "✎",
          get aiContext() {
            return s(A);
          }
        });
      }
      n(Y), n(J), D(
        (A, M) => {
          fe.disabled = A, Fe(fe, 1, `px-3 py-1.5 rounded text-xs transition-colors shrink-0
						${M ?? ""}`);
        },
        [
          () => !s(b).trim(),
          () => s(b).trim() ? "bg-es-command/20 text-es-command hover:bg-es-command/30" : "bg-surface-700 text-surface-500 cursor-not-allowed"
        ]
      ), lt(oe, () => s(b), (A) => h(b, A)), lt(ke, () => s(y), (A) => h(y, A)), Bs(B, () => s(P), (A) => h(P, A)), f(S, k);
    }, Ge = (S) => {
      var k = Gd(), W = it(k), V = i(W), X = l(i(V), 2);
      X.__click = () => h(te, !s(te)), n(V);
      var oe = l(V, 2);
      {
        var Z = (ie) => {
          var fe = zd(), Le = i(fe), ce = l(i(Le), 2);
          ht(ce), n(Le);
          var pe = l(Le, 2), He = l(i(pe), 2);
          ht(He), n(pe);
          var je = l(pe, 2), le = l(i(je), 2), j = i(le);
          j.value = j.__value = "cmd";
          var J = l(j);
          J.value = J.__value = "qry";
          var Y = l(J);
          Y.value = Y.__value = "prj", n(le), n(je);
          var be = l(je, 2);
          be.__click = Oe;
          var g = l(be, 2);
          g.__click = () => h(te, !1), n(fe), D((m) => be.disabled = m, [() => !s($e).trim() || d()]), lt(ce, () => s($e), (m) => h($e, m)), lt(He, () => s(he), (m) => h(he, m)), Bs(le, () => s(Te), (m) => h(Te, m)), f(ie, fe);
        };
        I(oe, (ie) => {
          s(te) && ie(Z);
        });
      }
      wt(2), n(W);
      var ke = l(W, 2), T = l(i(ke), 2), H = i(T);
      {
        let ie = /* @__PURE__ */ ge(() => `Help me create a desk inventory for the "${r()?.context_name}" division. Each desk is a vertical slice (command + event + handler). Organize by CMD (write), QRY (read), and PRJ (projection) departments.`);
        _t(H, {
          title: "Desk Inventory",
          description: "Create a complete inventory of desks needed for this division, organized by department (CMD, QRY, PRJ)",
          icon: "▣",
          get aiContext() {
            return s(ie);
          }
        });
      }
      var B = l(H, 2);
      {
        let ie = /* @__PURE__ */ ge(() => `Help me map dependencies between desks in the "${r()?.context_name}" division. Which desks depend on which? What's the optimal implementation order?`);
        _t(B, {
          title: "Dependency Mapping",
          description: "Map dependencies between desks to determine implementation order",
          icon: "⇄",
          get aiContext() {
            return s(ie);
          }
        });
      }
      var ue = l(B, 2);
      {
        let ie = /* @__PURE__ */ ge(() => `Help me sequence the implementation of desks in the "${r()?.context_name}" division into logical sprints. Consider dependencies, walking skeleton principles, and the "initiate + archive" first rule.`);
        _t(ue, {
          title: "Sprint Sequencing",
          description: "Prioritize and sequence desks into implementation sprints",
          icon: "☰",
          get aiContext() {
            return s(ie);
          }
        });
      }
      var ne = l(ue, 2);
      {
        let ie = /* @__PURE__ */ ge(() => `Help me design REST API endpoints for the "${r()?.context_name}" division. Follow the pattern: POST /api/{resource}/{action} for commands, GET /api/{resource} for queries.`);
        _t(ne, {
          title: "API Design",
          description: "Design REST API endpoints for each desk's capabilities",
          icon: "↔",
          get aiContext() {
            return s(ie);
          }
        });
      }
      n(T), n(ke), f(S, k);
    };
    I(Ce, (S) => {
      s(Q) === "design" ? S(Re) : S(Ge, !1);
    });
  }
  n(De), D(() => {
    E(re, r()?.context_name), Fe(O, 1, `px-3 py-1 rounded text-[11px] transition-colors
					${s(Q) === "design" ? "bg-surface-600 text-surface-100" : "text-surface-400 hover:text-surface-200"}`), Fe(F, 1, `px-3 py-1 rounded text-[11px] transition-colors
					${s(Q) === "plan" ? "bg-surface-600 text-surface-100" : "text-surface-400 hover:text-surface-200"}`);
  }), f(e, De), Mt(), _();
}
Ft(["click", "keydown", "dblclick"]);
Vt(ji, {}, [], [], { mode: "open" });
const Bi = Ze(null);
async function Kd(e, t) {
  try {
    return await Qe().post(`/craftings/${e}/generate-module`, t), !0;
  } catch (r) {
    const a = r;
    return Bi.set(a.message || "Failed to generate module"), !1;
  }
}
async function Jd(e, t) {
  try {
    return await Qe().post(`/craftings/${e}/deliver-release`, { version: t }), !0;
  } catch (r) {
    const a = r;
    return Bi.set(a.message || "Failed to deliver release"), !1;
  }
}
var Qd = /* @__PURE__ */ p(`<div class="flex gap-2 items-end mb-4 p-3 rounded bg-surface-700/30"><div class="flex-1"><label for="mod-name" class="text-[10px] text-surface-400 block mb-1">Module Name</label> <input id="mod-name" placeholder="e.g., register_user_v1" class="w-full bg-surface-700 border border-surface-600 rounded
								px-2.5 py-1.5 text-xs text-surface-100 placeholder-surface-400
								focus:outline-none focus:border-hecate-500/50"/></div> <div class="flex-1"><label for="mod-template" class="text-[10px] text-surface-400 block mb-1">Template (optional)</label> <input id="mod-template" placeholder="e.g., command, event, handler" class="w-full bg-surface-700 border border-surface-600 rounded
								px-2.5 py-1.5 text-xs text-surface-100 placeholder-surface-400
								focus:outline-none focus:border-hecate-500/50"/></div> <button class="px-3 py-1.5 rounded text-xs bg-hecate-600/20 text-hecate-300
							hover:bg-hecate-600/30 transition-colors disabled:opacity-50">Generate</button> <button class="px-3 py-1.5 rounded text-xs text-surface-400 hover:text-surface-100">Cancel</button></div>`), Xd = /* @__PURE__ */ p(
  `<div class="rounded-lg border border-surface-600 bg-surface-800/50 p-4"><div class="flex items-center justify-between mb-3"><h4 class="text-xs font-semibold text-surface-100">Code Generation</h4> <button class="text-[10px] px-2 py-0.5 rounded bg-hecate-600/10 text-hecate-300
						hover:bg-hecate-600/20 transition-colors">+ Generate Module</button></div> <!> <p class="text-[10px] text-surface-400">Generate Erlang modules from templates based on planned desks and design
				artifacts.</p></div> <div><h4 class="text-xs font-semibold text-surface-100 mb-3">Implementation Tasks</h4> <div class="grid grid-cols-1 md:grid-cols-2 gap-3"><!> <!> <!> <!> <!> <!></div></div>`,
  1
), Zd = /* @__PURE__ */ p(`<div class="flex gap-2 items-end mb-4 p-3 rounded bg-surface-700/30"><div class="flex-1"><label for="rel-version" class="text-[10px] text-surface-400 block mb-1">Version</label> <input id="rel-version" placeholder="e.g., 0.1.0" class="w-full bg-surface-700 border border-surface-600 rounded
								px-2.5 py-1.5 text-xs text-surface-100 placeholder-surface-400
								focus:outline-none focus:border-hecate-500/50"/></div> <button class="px-3 py-1.5 rounded text-xs bg-hecate-600/20 text-hecate-300
							hover:bg-hecate-600/30 transition-colors disabled:opacity-50">Deliver</button> <button class="px-3 py-1.5 rounded text-xs text-surface-400 hover:text-surface-100">Cancel</button></div>`), ev = /* @__PURE__ */ p(
  `<div class="rounded-lg border border-surface-600 bg-surface-800/50 p-4"><div class="flex items-center justify-between mb-3"><h4 class="text-xs font-semibold text-surface-100">Releases</h4> <button class="text-[10px] px-2 py-0.5 rounded bg-hecate-600/10 text-hecate-300
						hover:bg-hecate-600/20 transition-colors">+ Deliver Release</button></div> <!> <p class="text-[10px] text-surface-400">Deliver through GitOps: version bump, git tag, CI/CD builds and deploys.</p></div> <div><h4 class="text-xs font-semibold text-surface-100 mb-3">Delivery Tasks</h4> <div class="grid grid-cols-1 md:grid-cols-2 gap-3"><!> <!> <!> <!></div></div>`,
  1
), tv = /* @__PURE__ */ p('<div class="p-4 space-y-6"><div class="flex items-center justify-between"><div><h3 class="text-sm font-semibold text-surface-100">Crafting</h3> <p class="text-[11px] text-surface-400 mt-0.5">Generate code, run tests, and deliver releases for <span class="text-surface-200"> </span></p></div> <div class="flex items-center gap-1 bg-surface-700/50 rounded-lg p-0.5"><button>Implementation</button> <button>Delivery</button></div></div> <!></div>');
function Hi(e, t) {
  Tt(t, !0);
  const r = () => Ae(ns, "$selectedDivision", o), a = () => Ae(ft, "$isLoading", o), [o, c] = Kt();
  let u = /* @__PURE__ */ se("implement"), d = /* @__PURE__ */ se(!1), v = /* @__PURE__ */ se(""), _ = /* @__PURE__ */ se(""), x = /* @__PURE__ */ se(!1), b = /* @__PURE__ */ se("");
  async function y() {
    if (!r() || !s(v).trim()) return;
    await Kd(r().division_id, {
      module_name: s(v).trim(),
      template: s(_).trim() || void 0
    }) && (h(v, ""), h(_, ""), h(d, !1));
  }
  async function P() {
    if (!r() || !s(b).trim()) return;
    await Jd(r().division_id, s(b).trim()) && (h(b, ""), h(x, !1));
  }
  var $ = tv(), N = i($), C = i(N), ee = l(i(C), 2), te = l(i(ee)), $e = i(te, !0);
  n(te), n(ee), n(C);
  var he = l(C, 2), Te = i(he);
  Te.__click = () => h(u, "implement");
  var Q = l(Te, 2);
  Q.__click = () => h(u, "deliver"), n(he), n(N);
  var Ie = l(N, 2);
  {
    var _e = (xe) => {
      var qe = Xd(), Ue = it(qe), Ke = i(Ue), Ne = l(i(Ke), 2);
      Ne.__click = () => h(d, !s(d)), n(Ke);
      var K = l(Ke, 2);
      {
        var w = (re) => {
          var ye = Qd(), O = i(ye), F = l(i(O), 2);
          ht(F), n(O);
          var Ce = l(O, 2), Re = l(i(Ce), 2);
          ht(Re), n(Ce);
          var Ge = l(Ce, 2);
          Ge.__click = y;
          var S = l(Ge, 2);
          S.__click = () => h(d, !1), n(ye), D((k) => Ge.disabled = k, [() => !s(v).trim() || a()]), lt(F, () => s(v), (k) => h(v, k)), lt(Re, () => s(_), (k) => h(_, k)), f(re, ye);
        };
        I(K, (re) => {
          s(d) && re(w);
        });
      }
      wt(2), n(Ue);
      var G = l(Ue, 2), Oe = l(i(G), 2), ze = i(Oe);
      {
        let re = /* @__PURE__ */ ge(() => `Help me implement the walking skeleton for the "${r()?.context_name}" division. We need initiate_{aggregate} and archive_{aggregate} desks first. Generate the Erlang module structure for each.`);
        _t(ze, {
          title: "Walking Skeleton",
          description: "Generate initiate + archive desks first, establishing the aggregate lifecycle foundation",
          icon: "⚲",
          get aiContext() {
            return s(re);
          }
        });
      }
      var De = l(ze, 2);
      {
        let re = /* @__PURE__ */ ge(() => `Help me generate Erlang command modules for the "${r()?.context_name}" division. Each command needs: module, record, to_map/1, from_map/1. Use the evoq command pattern.`);
        _t(De, {
          title: "Generate Commands",
          description: "Create command modules from the desk inventory with proper versioning",
          icon: "▶",
          get aiContext() {
            return s(re);
          }
        });
      }
      var we = l(De, 2);
      {
        let re = /* @__PURE__ */ ge(() => `Help me generate Erlang event modules for the "${r()?.context_name}" division. Each event needs: module, record, to_map/1, from_map/1. Follow the event naming convention: {subject}_{verb_past}_v{N}.`);
        _t(we, {
          title: "Generate Events",
          description: "Create event modules matching the designed domain events",
          icon: "◆",
          get aiContext() {
            return s(re);
          }
        });
      }
      var me = l(we, 2);
      {
        let re = /* @__PURE__ */ ge(() => `Help me write EUnit tests for the "${r()?.context_name}" division. Cover aggregate behavior (execute + apply), handler dispatch, and projection state updates.`);
        _t(me, {
          title: "Write Tests",
          description: "Generate EUnit test modules for aggregates, handlers, and projections",
          icon: "✓",
          get aiContext() {
            return s(re);
          }
        });
      }
      var ae = l(me, 2);
      {
        let re = /* @__PURE__ */ ge(() => `Help me analyze test results for the "${r()?.context_name}" division. What patterns should I look for? How do I ensure adequate coverage of the aggregate lifecycle?`);
        _t(ae, {
          title: "Run Test Suite",
          description: "Execute all tests and review results for quality gates",
          icon: "▷",
          get aiContext() {
            return s(re);
          }
        });
      }
      var ve = l(ae, 2);
      {
        let re = /* @__PURE__ */ ge(() => `Help me define acceptance criteria for the "${r()?.context_name}" division. What must be true before we can say this division is implemented correctly?`);
        _t(ve, {
          title: "Acceptance Criteria",
          description: "Validate that implementation meets the design specifications",
          icon: "☑",
          get aiContext() {
            return s(re);
          }
        });
      }
      n(Oe), n(G), f(xe, qe);
    }, Je = (xe) => {
      var qe = ev(), Ue = it(qe), Ke = i(Ue), Ne = l(i(Ke), 2);
      Ne.__click = () => h(x, !s(x)), n(Ke);
      var K = l(Ke, 2);
      {
        var w = (ae) => {
          var ve = Zd(), re = i(ve), ye = l(i(re), 2);
          ht(ye), n(re);
          var O = l(re, 2);
          O.__click = P;
          var F = l(O, 2);
          F.__click = () => h(x, !1), n(ve), D((Ce) => O.disabled = Ce, [() => !s(b).trim() || a()]), lt(ye, () => s(b), (Ce) => h(b, Ce)), f(ae, ve);
        };
        I(K, (ae) => {
          s(x) && ae(w);
        });
      }
      wt(2), n(Ue);
      var G = l(Ue, 2), Oe = l(i(G), 2), ze = i(Oe);
      {
        let ae = /* @__PURE__ */ ge(() => `Help me prepare a release for the "${r()?.context_name}" division. Walk me through the GitOps flow: version bump, CHANGELOG update, git tag, and CI/CD pipeline.`);
        _t(ze, {
          title: "Release Management",
          description: "Prepare release: version bump, changelog, git tag, push for CI/CD",
          icon: "↑",
          get aiContext() {
            return s(ae);
          }
        });
      }
      var De = l(ze, 2);
      {
        let ae = /* @__PURE__ */ ge(() => `Help me plan a staged rollout for the "${r()?.context_name}" division. How should we structure canary deployments with health gates on the beam cluster?`);
        _t(De, {
          title: "Staged Rollout",
          description: "Plan a staged rollout with canary deployment and health gates",
          icon: "▤",
          get aiContext() {
            return s(ae);
          }
        });
      }
      var we = l(De, 2);
      {
        let ae = /* @__PURE__ */ ge(() => `Help me set up health monitoring for the "${r()?.context_name}" division. What health checks should we configure? What SLA thresholds make sense?`);
        _t(we, {
          title: "Health Monitoring",
          description: "Configure health checks and SLA thresholds",
          icon: "♥",
          get aiContext() {
            return s(ae);
          }
        });
      }
      var me = l(we, 2);
      {
        let ae = /* @__PURE__ */ ge(() => `Help me set up observability for the "${r()?.context_name}" division. What should we log? What metrics should we track? How do we set up distributed tracing on the beam cluster?`);
        _t(me, {
          title: "Observability",
          description: "Set up logging, metrics, and tracing for production visibility",
          icon: "◎",
          get aiContext() {
            return s(ae);
          }
        });
      }
      n(Oe), n(G), f(xe, qe);
    };
    I(Ie, (xe) => {
      s(u) === "implement" ? xe(_e) : xe(Je, !1);
    });
  }
  n($), D(() => {
    E($e, r()?.context_name), Fe(Te, 1, `px-3 py-1 rounded text-[11px] transition-colors
					${s(u) === "implement" ? "bg-surface-600 text-surface-100" : "text-surface-400 hover:text-surface-200"}`), Fe(Q, 1, `px-3 py-1 rounded text-[11px] transition-colors
					${s(u) === "deliver" ? "bg-surface-600 text-surface-100" : "text-surface-400 hover:text-surface-200"}`);
  }), f(e, $), Mt(), c();
}
Ft(["click"]);
Vt(Hi, {}, [], [], { mode: "open" });
var rv = /* @__PURE__ */ p("<div></div>"), sv = /* @__PURE__ */ p('<span class="text-health-ok text-[10px]"></span>'), av = /* @__PURE__ */ p('<span class="text-hecate-400 text-[10px] animate-pulse"></span>'), nv = /* @__PURE__ */ p('<span class="text-health-warn text-[10px]"></span>'), iv = /* @__PURE__ */ p('<span class="text-surface-300 text-[10px]"></span>'), ov = /* @__PURE__ */ p('<span class="text-surface-500 text-[10px]"></span>'), cv = /* @__PURE__ */ p("<!> <button><!> <span> </span></button>", 1), lv = /* @__PURE__ */ p(`<button class="text-[10px] px-2 py-0.5 rounded bg-hecate-600/20 text-hecate-300
						hover:bg-hecate-600/30 transition-colors disabled:opacity-50">Open</button>`), uv = /* @__PURE__ */ p('<span class="text-[10px] text-surface-500 mr-1">Pending</span>'), dv = /* @__PURE__ */ p(
  `<button class="text-[10px] px-2 py-0.5 rounded text-surface-400
						hover:text-health-warn hover:bg-surface-700 transition-colors disabled:opacity-50">Shelve</button> <button class="text-[10px] px-2 py-0.5 rounded text-surface-400
						hover:text-health-ok hover:bg-surface-700 transition-colors disabled:opacity-50">Conclude</button>`,
  1
), vv = /* @__PURE__ */ p(`<button class="text-[10px] px-2 py-0.5 rounded bg-health-warn/10 text-health-warn
						hover:bg-health-warn/20 transition-colors disabled:opacity-50">Resume</button>`), fv = /* @__PURE__ */ p(`<div class="border-b border-surface-600 bg-surface-800/30 px-4 py-2 shrink-0"><div class="flex items-center gap-1"><!> <div class="flex-1"></div> <span class="text-[10px] text-surface-400 mr-2"> </span> <!> <button class="text-[10px] px-2 py-0.5 rounded text-hecate-400
					hover:bg-hecate-600/20 transition-colors ml-1" title="Open AI Assistant"></button></div></div>`);
function Wi(e, t) {
  Tt(t, !0);
  const r = () => Ae(ns, "$selectedDivision", c), a = () => Ae(ss, "$selectedPhase", c), o = () => Ae(_r, "$isLoading", c), [c, u] = Kt();
  let d = /* @__PURE__ */ ge(() => r() ? Hs(r(), a()) : 0);
  function v($) {
    ss.set($);
  }
  function _($, N) {
    switch ($) {
      case "planning":
        return "border-phase-planning text-phase-planning";
      case "crafting":
        return "border-phase-crafting text-phase-crafting";
    }
  }
  async function x($) {
    if (!r()) return;
    const N = r().division_id, C = a();
    switch ($) {
      case "open":
        await Ac(N, C);
        break;
      case "shelve":
        await Dc(N, C);
        break;
      case "resume":
        await Pc(N, C);
        break;
      case "conclude":
        await Tc(N, C);
        break;
    }
  }
  var b = Nr(), y = it(b);
  {
    var P = ($) => {
      var N = fv(), C = i(N), ee = i(C);
      We(ee, 17, () => Or, ot, (Ne, K, w) => {
        const G = /* @__PURE__ */ ge(() => Hs(r(), s(K).code)), Oe = /* @__PURE__ */ ge(() => a() === s(K).code), ze = /* @__PURE__ */ ge(() => et(s(G), Jr));
        var De = cv(), we = it(De);
        {
          var me = (V) => {
            var X = rv();
            D(() => Fe(X, 1, `w-4 h-px ${s(ze) ? "bg-health-ok/40" : "bg-surface-600"}`)), f(V, X);
          };
          I(we, (V) => {
            w > 0 && V(me);
          });
        }
        var ae = l(we, 2);
        ae.__click = () => v(s(K).code);
        var ve = i(ae);
        {
          var re = (V) => {
            var X = sv();
            X.textContent = "✓", f(V, X);
          }, ye = (V) => {
            var X = av();
            X.textContent = "●", f(V, X);
          }, O = /* @__PURE__ */ ge(() => et(s(G), Pr)), F = (V) => {
            var X = nv();
            X.textContent = "◐", f(V, X);
          }, Ce = /* @__PURE__ */ ge(() => et(s(G), Kr)), Re = (V) => {
            var X = iv();
            X.textContent = "○", f(V, X);
          }, Ge = /* @__PURE__ */ ge(() => et(s(G), xs)), S = (V) => {
            var X = ov();
            X.textContent = "○", f(V, X);
          };
          I(ve, (V) => {
            s(ze) ? V(re) : s(O) ? V(ye, 1) : s(Ce) ? V(F, 2) : s(Ge) ? V(Re, 3) : V(S, !1);
          });
        }
        var k = l(ve, 2), W = i(k, !0);
        n(k), n(ae), D(
          (V) => {
            Fe(ae, 1, `flex items-center gap-1.5 px-3 py-1.5 rounded text-xs transition-all
						border
						${V ?? ""}`), E(W, s(K).shortName);
          },
          [
            () => s(Oe) ? `bg-surface-700 border-current ${_(s(K).code)}` : "border-transparent text-surface-400 hover:text-surface-200 hover:bg-surface-700/50"
          ]
        ), f(Ne, De);
      });
      var te = l(ee, 4), $e = i(te, !0);
      n(te);
      var he = l(te, 2);
      {
        var Te = (Ne) => {
          var K = lv();
          K.__click = () => x("open"), D(() => K.disabled = o()), f(Ne, K);
        }, Q = /* @__PURE__ */ ge(() => et(s(d), xs) && !et(s(d), Pr) && !et(s(d), Jr) && !et(s(d), Kr)), Ie = (Ne) => {
          var K = uv();
          f(Ne, K);
        }, _e = /* @__PURE__ */ ge(() => !et(s(d), xs) && !et(s(d), Pr) && !et(s(d), Jr)), Je = (Ne) => {
          var K = dv(), w = it(K);
          w.__click = () => x("shelve");
          var G = l(w, 2);
          G.__click = () => x("conclude"), D(() => {
            w.disabled = o(), G.disabled = o();
          }), f(Ne, K);
        }, xe = /* @__PURE__ */ ge(() => et(s(d), Pr)), qe = (Ne) => {
          var K = vv();
          K.__click = () => x("resume"), D(() => K.disabled = o()), f(Ne, K);
        }, Ue = /* @__PURE__ */ ge(() => et(s(d), Kr));
        I(he, (Ne) => {
          s(Q) ? Ne(Te) : s(_e) ? Ne(Ie, 1) : s(xe) ? Ne(Je, 2) : s(Ue) && Ne(qe, 3);
        });
      }
      var Ke = l(he, 2);
      Ke.__click = () => ur(`Help with ${Or.find((Ne) => Ne.code === a())?.name} phase for division "${r()?.context_name}"`), Ke.textContent = "✦ AI Assist", n(C), n(N), D((Ne) => E($e, Ne), [() => Or.find((Ne) => Ne.code === a())?.name]), f($, N);
    };
    I(y, ($) => {
      r() && $(P);
    });
  }
  f(e, b), Mt(), u();
}
Ft(["click"]);
Vt(Wi, {}, [], [], { mode: "open" });
var pv = /* @__PURE__ */ p('<span class="text-[9px] text-surface-500"> </span>'), hv = /* @__PURE__ */ p('<div class="flex items-center justify-center h-full"><div class="text-center text-surface-400"><div class="text-sm mb-2 animate-pulse">...</div> <div class="text-[10px]">Loading events</div></div></div>'), _v = /* @__PURE__ */ p('<div class="flex items-center justify-center h-full"><div class="text-center text-surface-500 text-xs">Select a venture to view its event stream.</div></div>'), xv = /* @__PURE__ */ p('<div class="flex items-center justify-center h-full"><div class="text-center text-surface-500"><div class="text-lg mb-2"></div> <div class="text-xs">No events recorded yet.</div> <div class="text-[10px] mt-1">Events will appear here as the venture progresses.</div></div></div>'), gv = /* @__PURE__ */ p('<span class="text-[9px] px-1 py-0.5 rounded bg-surface-700 text-surface-400 shrink-0"> </span>'), bv = /* @__PURE__ */ p('<span class="text-[9px] text-surface-500 shrink-0 tabular-nums"> </span>'), mv = /* @__PURE__ */ p(`<div class="px-4 pb-3 pt-0 ml-5"><pre class="text-[10px] text-surface-300 bg-surface-800 border border-surface-600
									rounded p-3 overflow-x-auto whitespace-pre-wrap break-words
									font-mono leading-relaxed"> </pre></div>`), yv = /* @__PURE__ */ p(`<div class="group"><button class="w-full text-left px-4 py-2 flex items-start gap-2
								hover:bg-surface-700/30 transition-colors"><span class="text-[9px] text-surface-500 mt-0.5 shrink-0 w-3"> </span> <span> </span> <!> <!></button> <!></div>`), wv = /* @__PURE__ */ p('<div class="p-3 border-t border-surface-700/50"><button> </button></div>'), $v = /* @__PURE__ */ p('<div class="divide-y divide-surface-700/50"></div> <!>', 1), kv = /* @__PURE__ */ p('<div class="flex flex-col h-full"><div class="border-b border-surface-600 bg-surface-800/50 px-4 py-2 shrink-0"><div class="flex items-center gap-2"><span class="text-xs text-surface-400">Event Stream</span> <!> <div class="flex-1"></div> <button title="Refresh events"> </button></div></div> <div class="flex-1 overflow-y-auto"><!></div></div>');
function Ea(e, t) {
  Tt(t, !0);
  const r = () => Ae(Qa, "$ventureRawEvents", o), a = () => Ae(bt, "$activeVenture", o), [o, c] = Kt(), u = 50;
  let d = /* @__PURE__ */ se(!1), v = /* @__PURE__ */ se(0), _ = /* @__PURE__ */ se(0), x = /* @__PURE__ */ se(Wt(/* @__PURE__ */ new Set())), b = /* @__PURE__ */ ge(() => s(_) + u < s(v)), y = /* @__PURE__ */ ge(r);
  async function P(w, G = !0) {
    h(d, !0), G && (h(_, 0), h(x, /* @__PURE__ */ new Set(), !0));
    try {
      const Oe = await _n(w, s(_), u);
      h(v, Oe.count, !0);
    } finally {
      h(d, !1);
    }
  }
  async function $() {
    const w = a();
    if (!(!w || s(d))) {
      h(_, s(_) + u), h(d, !0);
      try {
        const G = await _n(w.venture_id, s(_), u);
        h(v, G.count, !0);
      } finally {
        h(d, !1);
      }
    }
  }
  function N(w) {
    const G = new Set(s(x));
    G.has(w) ? G.delete(w) : G.add(w), h(x, G, !0);
  }
  function C(w) {
    return w.startsWith("venture_") || w.startsWith("big_picture_storm_") ? "text-hecate-400" : w.startsWith("event_sticky_") ? "text-es-event" : w.startsWith("event_stack_") || w.startsWith("event_cluster_") ? "text-success-400" : w.startsWith("fact_arrow_") ? "text-sky-400" : w.startsWith("storm_phase_") ? "text-accent-400" : "text-surface-400";
  }
  function ee(w) {
    if (!w) return "";
    const G = typeof w == "string" ? Number(w) || new Date(w).getTime() : w;
    if (isNaN(G)) return "";
    const Oe = new Date(G), De = Date.now() - G, we = Math.floor(De / 1e3);
    if (we < 60) return `${we}s ago`;
    const me = Math.floor(we / 60);
    if (me < 60) return `${me}m ago`;
    const ae = Math.floor(me / 60);
    if (ae < 24) return `${ae}h ago`;
    const ve = Math.floor(ae / 24);
    return ve < 7 ? `${ve}d ago` : Oe.toLocaleDateString("en-US", {
      month: "short",
      day: "numeric",
      hour: "2-digit",
      minute: "2-digit"
    });
  }
  function te(w) {
    try {
      return JSON.stringify(w, null, 2);
    } catch {
      return String(w);
    }
  }
  Nt(() => {
    const w = a();
    w && P(w.venture_id);
  });
  var $e = kv(), he = i($e), Te = i(he), Q = l(i(Te), 2);
  {
    var Ie = (w) => {
      var G = pv(), Oe = i(G);
      n(G), D(() => E(Oe, `${s(y).length ?? ""}${s(v) > s(y).length ? ` / ${s(v)}` : ""} events`)), f(w, G);
    };
    I(Q, (w) => {
      s(y).length > 0 && w(Ie);
    });
  }
  var _e = l(Q, 4);
  _e.__click = () => {
    const w = a();
    w && P(w.venture_id);
  };
  var Je = i(_e, !0);
  n(_e), n(Te), n(he);
  var xe = l(he, 2), qe = i(xe);
  {
    var Ue = (w) => {
      var G = hv();
      f(w, G);
    }, Ke = (w) => {
      var G = _v();
      f(w, G);
    }, Ne = (w) => {
      var G = xv(), Oe = i(G), ze = i(Oe);
      ze.textContent = "○", wt(4), n(Oe), n(G), f(w, G);
    }, K = (w) => {
      var G = $v(), Oe = it(G);
      We(Oe, 21, () => s(y), ot, (we, me, ae) => {
        const ve = /* @__PURE__ */ ge(() => s(x).has(ae)), re = /* @__PURE__ */ ge(() => C(s(me).event_type));
        var ye = yv(), O = i(ye);
        O.__click = () => N(ae);
        var F = i(O), Ce = i(F, !0);
        n(F);
        var Re = l(F, 2), Ge = i(Re, !0);
        n(Re);
        var S = l(Re, 2);
        {
          var k = (Z) => {
            var ke = gv(), T = i(ke);
            n(ke), D(() => E(T, `v${s(me).version ?? ""}`)), f(Z, ke);
          };
          I(S, (Z) => {
            s(me).version !== void 0 && Z(k);
          });
        }
        var W = l(S, 2);
        {
          var V = (Z) => {
            var ke = bv(), T = i(ke, !0);
            n(ke), D((H) => E(T, H), [() => ee(s(me).timestamp)]), f(Z, ke);
          };
          I(W, (Z) => {
            s(me).timestamp && Z(V);
          });
        }
        n(O);
        var X = l(O, 2);
        {
          var oe = (Z) => {
            var ke = mv(), T = i(ke), H = i(T, !0);
            n(T), n(ke), D((B) => E(H, B), [() => te(s(me).data)]), f(Z, ke);
          };
          I(X, (Z) => {
            s(ve) && Z(oe);
          });
        }
        n(ye), D(() => {
          E(Ce, s(ve) ? "▾" : "▸"), Fe(Re, 1, `text-[11px] font-mono ${s(re) ?? ""} flex-1 min-w-0 truncate`), E(Ge, s(me).event_type);
        }), f(we, ye);
      }), n(Oe);
      var ze = l(Oe, 2);
      {
        var De = (we) => {
          var me = wv(), ae = i(me);
          ae.__click = $;
          var ve = i(ae, !0);
          n(ae), n(me), D(() => {
            ae.disabled = s(d), Fe(ae, 1, `w-full text-[10px] py-1.5 rounded transition-colors
							${s(d) ? "bg-surface-700 text-surface-500 cursor-not-allowed" : "bg-surface-700 text-surface-300 hover:text-surface-100 hover:bg-surface-600"}`), E(ve, s(d) ? "Loading..." : `Load More (${s(v) - s(y).length} remaining)`);
          }), f(we, me);
        };
        I(ze, (we) => {
          s(b) && we(De);
        });
      }
      f(w, G);
    };
    I(qe, (w) => {
      s(d) && s(y).length === 0 ? w(Ue) : a() ? s(y).length === 0 ? w(Ne, 2) : w(K, !1) : w(Ke, 1);
    });
  }
  n(xe), n($e), D(() => {
    _e.disabled = s(d) || !a(), Fe(_e, 1, `text-[10px] px-2 py-0.5 rounded transition-colors
					${s(d) || !a() ? "text-surface-500 cursor-not-allowed" : "text-surface-400 hover:text-surface-200 hover:bg-surface-700"}`), E(Je, s(d) ? "Loading..." : "Refresh");
  }), f(e, $e), Mt(), c();
}
Ft(["click"]);
Vt(Ea, {}, [], [], { mode: "open" });
var Ev = /* @__PURE__ */ p(`<div class="flex justify-end"><div class="max-w-[85%] rounded-lg px-3 py-2 text-[11px]
						bg-hecate-600/20 text-surface-100 border border-hecate-600/20"> </div></div>`), Cv = /* @__PURE__ */ p('<div class="flex justify-start"><div><div class="whitespace-pre-wrap break-words"> </div></div></div>'), Sv = /* @__PURE__ */ p('<div class="whitespace-pre-wrap break-words"> <span class="inline-block w-1.5 h-3 bg-hecate-400 animate-pulse ml-0.5"></span></div>'), Av = /* @__PURE__ */ p('<div class="flex items-center gap-1.5 text-surface-400"><span class="animate-bounce" style="animation-delay: 0ms">.</span> <span class="animate-bounce" style="animation-delay: 150ms">.</span> <span class="animate-bounce" style="animation-delay: 300ms">.</span></div>'), Dv = /* @__PURE__ */ p(`<div class="flex justify-start"><div class="max-w-[85%] rounded-lg px-3 py-2 text-[11px]
					bg-surface-700 text-surface-200 border border-surface-600"><!></div></div>`), Pv = /* @__PURE__ */ p('<span class="text-[9px] text-hecate-400 ml-1">(code-optimized)</span>'), Tv = /* @__PURE__ */ p('<span class="text-hecate-400"> </span> <!>', 1), Mv = /* @__PURE__ */ p('<span class="text-health-warn">No model available</span>'), Iv = /* @__PURE__ */ p('<div class="flex items-center justify-center h-full"><div class="text-center text-surface-400"><div class="text-xl mb-2"></div> <div class="text-[11px]">AI Assistant ready <br/> <!></div></div></div>'), Nv = /* @__PURE__ */ p(`<div class="w-[380px] border-l border-surface-600 bg-surface-800 flex flex-col shrink-0 overflow-hidden"><div class="flex items-center gap-2 px-3 py-2 border-b border-surface-600 shrink-0"><span class="text-hecate-400"></span> <span class="text-xs font-semibold text-surface-100">AI</span> <!> <div class="flex-1"></div> <span class="text-[9px] text-surface-400"> </span> <button class="text-surface-400 hover:text-surface-100 transition-colors px-1" title="Close AI Assistant"></button></div> <div class="flex-1 overflow-y-auto p-3 space-y-3"><!> <!> <!></div> <div class="border-t border-surface-600 p-2 shrink-0"><div class="flex gap-1.5"><textarea class="flex-1 bg-surface-700 border border-surface-600 rounded px-2.5 py-1.5
					text-[11px] text-surface-100 placeholder-surface-400 resize-none
					focus:outline-none focus:border-hecate-500
					disabled:opacity-50 disabled:cursor-not-allowed"></textarea> <button>Send</button></div></div></div>`);
function Ca(e, t) {
  Tt(t, !0);
  const r = () => Ae(ss, "$selectedPhase", v), a = () => Ae(Di, "$phaseModelPrefs", v), o = () => Ae(Ga, "$aiModel", v), c = () => Ae(Ci, "$aiAssistContext", v), u = () => Ae(bt, "$activeVenture", v), d = () => Ae(ns, "$selectedDivision", v), [v, _] = Kt(), x = Ei();
  let b = /* @__PURE__ */ se(Wt([])), y = /* @__PURE__ */ se(""), P = /* @__PURE__ */ se(!1), $ = /* @__PURE__ */ se(""), N = /* @__PURE__ */ se(void 0), C = /* @__PURE__ */ se(null), ee = /* @__PURE__ */ se(null), te = /* @__PURE__ */ ge(() => kc(r())), $e = /* @__PURE__ */ ge(() => a()[r()]);
  Nt(() => {
    const O = o();
    s(ee) !== null && s(ee) !== O && (s(C) && (s(C).cancel(), h(C, null)), h(b, [], !0), h($, ""), h(P, !1)), h(ee, O, !0);
  }), Nt(() => {
    const O = c();
    O && s(b).length === 0 && Te(O);
  });
  function he() {
    const O = [], F = Ot(Fi);
    F && O.push(F);
    const Ce = Or.find((Re) => Re.code === r());
    if (Ce && O.push(`You are currently assisting with the ${Ce.name} phase. ${Ce.description}.`), u()) {
      let Re = `Venture: "${u().name}"`;
      u().brief && (Re += ` — ${u().brief}`), O.push(Re);
    }
    return d() && O.push(`Division: "${d().context_name}" (bounded context)`), O.push(Ot(Il)), O.join(`

---

`);
  }
  async function Te(O) {
    const F = o();
    if (!F || !O.trim() || s(P)) return;
    const Ce = { role: "user", content: O.trim() };
    h(b, [...s(b), Ce], !0), h(y, "");
    const Re = [], Ge = he();
    Ge && Re.push({ role: "system", content: Ge }), Re.push(...s(b)), h(P, !0), h($, "");
    let S = "";
    const k = x.stream.chat(F, Re);
    h(C, k, !0), k.onChunk((W) => {
      W.content && (S += W.content, h($, S, !0));
    }).onDone(async (W) => {
      h(C, null), W.content && (S += W.content);
      const V = {
        role: "assistant",
        content: S || "(empty response)"
      };
      if (h(b, [...s(b), V], !0), h($, ""), h(P, !1), Ot(Ua) === "oracle" && S) {
        const oe = Ot(bt)?.venture_id;
        if (oe) {
          const Z = Cc(S);
          for (const ke of Z)
            await $a(oe, ke, "oracle");
        }
      }
    }).onError((W) => {
      h(C, null);
      const V = { role: "assistant", content: `Error: ${W}` };
      h(b, [...s(b), V], !0), h($, ""), h(P, !1);
    });
    try {
      await k.start();
    } catch (W) {
      const V = { role: "assistant", content: `Error: ${String(W)}` };
      h(b, [...s(b), V], !0), h(P, !1);
    }
  }
  let Q = /* @__PURE__ */ se(void 0);
  function Ie(O) {
    O.key === "Enter" && !O.shiftKey && (O.preventDefault(), Te(s(y)), s(Q) && (s(Q).style.height = "auto"));
  }
  function _e(O) {
    const F = O.target;
    F.style.height = "auto", F.style.height = Math.min(F.scrollHeight, 120) + "px";
  }
  function Je() {
    Ec(), h(b, [], !0), h($, "");
  }
  Nt(() => {
    s(b), s($), Ha().then(() => {
      s(N) && (s(N).scrollTop = s(N).scrollHeight);
    });
  });
  var xe = Nv(), qe = i(xe), Ue = i(qe);
  Ue.textContent = "✦";
  var Ke = l(Ue, 4);
  {
    let O = /* @__PURE__ */ ge(() => Or.find((F) => F.code === r())?.shortName ?? "");
    ra(Ke, {
      get currentModel() {
        return o();
      },
      onSelect: (F) => Ya(F),
      showPhaseInfo: !0,
      get phasePreference() {
        return s($e);
      },
      get phaseAffinity() {
        return s(te);
      },
      onPinModel: (F) => hn(r(), F),
      onClearPin: () => hn(r(), null),
      get phaseName() {
        return s(O);
      }
    });
  }
  var Ne = l(Ke, 4), K = i(Ne, !0);
  n(Ne);
  var w = l(Ne, 2);
  w.__click = Je, w.textContent = "✕", n(qe);
  var G = l(qe, 2), Oe = i(G);
  We(Oe, 17, () => s(b), ot, (O, F) => {
    var Ce = Nr(), Re = it(Ce);
    {
      var Ge = (k) => {
        var W = Ev(), V = i(W), X = i(V, !0);
        n(V), n(W), D(() => E(X, s(F).content)), f(k, W);
      }, S = (k) => {
        var W = Cv(), V = i(W), X = i(V), oe = i(X, !0);
        n(X), n(V), n(W), D(
          (Z) => {
            Fe(V, 1, `max-w-[85%] rounded-lg px-3 py-2 text-[11px]
						bg-surface-700 text-surface-200 border border-surface-600
						${Z ?? ""}`), E(oe, s(F).content);
          },
          [
            () => s(F).content.startsWith("Error:") ? "border-health-err/30 text-health-err" : ""
          ]
        ), f(k, W);
      };
      I(Re, (k) => {
        s(F).role === "user" ? k(Ge) : s(F).role === "assistant" && k(S, 1);
      });
    }
    f(O, Ce);
  });
  var ze = l(Oe, 2);
  {
    var De = (O) => {
      var F = Dv(), Ce = i(F), Re = i(Ce);
      {
        var Ge = (k) => {
          var W = Sv(), V = i(W, !0);
          wt(), n(W), D(() => E(V, s($))), f(k, W);
        }, S = (k) => {
          var W = Av();
          f(k, W);
        };
        I(Re, (k) => {
          s($) ? k(Ge) : k(S, !1);
        });
      }
      n(Ce), n(F), f(O, F);
    };
    I(ze, (O) => {
      s(P) && O(De);
    });
  }
  var we = l(ze, 2);
  {
    var me = (O) => {
      var F = Iv(), Ce = i(F), Re = i(Ce);
      Re.textContent = "✦";
      var Ge = l(Re, 2), S = l(i(Ge), 3);
      {
        var k = (V) => {
          var X = Tv(), oe = it(X), Z = i(oe, !0);
          n(oe);
          var ke = l(oe, 2);
          {
            var T = (H) => {
              var B = Pv();
              f(H, B);
            };
            I(ke, (H) => {
              s(te) === "code" && H(T);
            });
          }
          D(() => E(Z, o())), f(V, X);
        }, W = (V) => {
          var X = Mv();
          f(V, X);
        };
        I(S, (V) => {
          o() ? V(k) : V(W, !1);
        });
      }
      n(Ge), n(Ce), n(F), f(O, F);
    };
    I(we, (O) => {
      s(b).length === 0 && !s(P) && O(me);
    });
  }
  n(G), Yr(G, (O) => h(N, O), () => s(N));
  var ae = l(G, 2), ve = i(ae), re = i(ve);
  Fa(re), re.__keydown = Ie, re.__input = _e, Rt(re, "rows", 1), Yr(re, (O) => h(Q, O), () => s(Q));
  var ye = l(re, 2);
  ye.__click = () => Te(s(y)), n(ve), n(ae), n(xe), D(
    (O, F, Ce) => {
      E(K, O), Rt(re, "placeholder", s(P) ? "Waiting..." : "Ask about this phase..."), re.disabled = s(P) || !o(), ye.disabled = F, Fe(ye, 1, `px-2.5 rounded text-[11px] transition-colors self-end
					${Ce ?? ""}`);
    },
    [
      () => Or.find((O) => O.code === r())?.shortName ?? "",
      () => s(P) || !s(y).trim() || !o(),
      () => s(P) || !s(y).trim() || !o() ? "bg-surface-600 text-surface-400 cursor-not-allowed" : "bg-hecate-600 text-surface-50 hover:bg-hecate-500"
    ]
  ), lt(re, () => s(y), (O) => h(y, O)), f(e, xe), Mt(), _();
}
Ft(["click", "keydown", "input"]);
Vt(Ca, {}, [], [], { mode: "open" });
var Ov = /* @__PURE__ */ p('<div class="flex items-center justify-center h-full"><div class="text-center text-surface-400"><div class="text-2xl mb-2 animate-pulse"></div> <div class="text-sm">Loading venture...</div></div></div>'), Lv = /* @__PURE__ */ p('<div class="text-[11px] text-health-err bg-health-err/10 rounded px-3 py-2"> </div>'), Rv = /* @__PURE__ */ p(`<div class="rounded-xl border border-hecate-600/30 bg-surface-800/80 p-5 space-y-4"><h3 class="text-xs font-medium text-hecate-300 uppercase tracking-wider">New Venture</h3> <div class="grid grid-cols-[1fr_2fr] gap-4"><div><label for="venture-name" class="text-[11px] text-surface-300 block mb-1.5">Name</label> <input id="venture-name" placeholder="e.g., my-saas-app" class="w-full bg-surface-700 border border-surface-600 rounded-lg
										px-3 py-2 text-sm text-surface-100 placeholder-surface-500
										focus:outline-none focus:border-hecate-500"/></div> <div><label for="venture-brief" class="text-[11px] text-surface-300 block mb-1.5">Brief (optional)</label> <input id="venture-brief" placeholder="What does this venture aim to achieve?" class="w-full bg-surface-700 border border-surface-600 rounded-lg
										px-3 py-2 text-sm text-surface-100 placeholder-surface-500
										focus:outline-none focus:border-hecate-500"/></div></div> <!> <div class="flex gap-3"><button> </button> <button class="px-4 py-2 rounded-lg text-xs text-hecate-400 border border-hecate-600/30
									hover:bg-hecate-600/10 transition-colors"></button></div></div>`), Fv = /* @__PURE__ */ p(`<div class="flex flex-col items-center justify-center py-20 text-center"><div class="text-4xl mb-4 text-hecate-400"></div> <h2 class="text-lg font-semibold text-surface-100 mb-2">No Ventures Yet</h2> <p class="text-xs text-surface-400 leading-relaxed max-w-sm mb-6">A venture is the umbrella for your software endeavor. It houses
							divisions (bounded contexts) and guides them through the development
							lifecycle.</p> <button class="px-5 py-2.5 rounded-lg text-sm font-medium bg-hecate-600 text-surface-50
								hover:bg-hecate-500 transition-colors">+ Create Your First Venture</button></div>`), Vv = /* @__PURE__ */ p('<div class="text-[11px] text-surface-400 truncate mt-1.5 ml-5"> </div>'), jv = /* @__PURE__ */ p(`<button class="group text-left p-4 rounded-xl bg-surface-800/80 border border-surface-600
												hover:border-hecate-500 hover:bg-hecate-600/5 transition-all"><div class="flex items-center gap-2"><span class="text-hecate-400 group-hover:text-hecate-300"></span> <span class="font-medium text-sm text-surface-100 truncate"> </span> <span class="text-[10px] px-1.5 py-0.5 rounded-full bg-surface-700 text-surface-300 border border-surface-600 shrink-0"> </span></div> <!></button>`), Bv = /* @__PURE__ */ p('<div><h3 class="text-[11px] text-surface-400 uppercase tracking-wider mb-3">Recently Updated</h3> <div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-3"></div></div>'), Hv = /* @__PURE__ */ p('<div class="text-[11px] text-surface-500 truncate mt-1.5 ml-5"> </div>'), Wv = /* @__PURE__ */ p(`<button class="group text-left p-4 rounded-xl bg-surface-800/40 border border-surface-700
													hover:border-surface-500 transition-all opacity-60 hover:opacity-80"><div class="flex items-center gap-2"><span class="text-surface-500"></span> <span class="font-medium text-sm text-surface-300 truncate"> </span> <span class="text-[10px] px-1.5 py-0.5 rounded-full bg-surface-700 text-surface-400 border border-surface-600 shrink-0">Archived</span></div> <!></button>`), qv = /* @__PURE__ */ p('<div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-3"></div>'), Uv = /* @__PURE__ */ p(`<div><button class="flex items-center gap-2 text-[11px] text-surface-500 uppercase tracking-wider
										hover:text-surface-300 transition-colors mb-3"><span class="text-[9px]"> </span> <span class="text-surface-600"> </span></button> <!></div>`), zv = /* @__PURE__ */ p('<div class="text-[11px] text-surface-400 truncate mt-1.5 ml-5"> </div>'), Gv = /* @__PURE__ */ p(`<button class="group text-left p-4 rounded-xl bg-surface-800/80 border border-surface-600
												hover:border-hecate-500 hover:bg-hecate-600/5 transition-all"><div class="flex items-center gap-2"><span class="text-hecate-400 group-hover:text-hecate-300"></span> <span class="font-medium text-sm text-surface-100 truncate"> </span> <span class="text-[10px] px-1.5 py-0.5 rounded-full bg-surface-700 text-surface-300 border border-surface-600 shrink-0"> </span></div> <!></button>`), Yv = /* @__PURE__ */ p('<div><h3 class="text-[11px] text-surface-400 uppercase tracking-wider mb-3"> </h3> <div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-3"></div></div>'), Kv = /* @__PURE__ */ p('<div class="text-center py-12 text-surface-400 text-sm"> </div>'), Jv = /* @__PURE__ */ p("<!>  <!> <!>", 1), Qv = /* @__PURE__ */ p('<div class="absolute top-0 right-0 bottom-0 z-10"><!></div>'), Xv = /* @__PURE__ */ p(
  `<div class="flex flex-col h-full overflow-hidden"><div class="border-b border-surface-600 bg-surface-800/50 px-4 py-3 shrink-0"><div class="flex items-center gap-3"><span class="text-hecate-400 text-lg"></span> <h1 class="text-sm font-semibold text-surface-100">Martha Studio</h1> <div class="flex items-center gap-1.5 text-[10px]"><span></span> <span class="text-surface-500"> </span></div> <!> <div class="flex-1"></div> <input placeholder="Search ventures..." class="w-48 bg-surface-700 border border-surface-600 rounded-lg
							px-3 py-1.5 text-xs text-surface-100 placeholder-surface-500
							focus:outline-none focus:border-hecate-500"/> <button> </button></div></div> <div class="flex-1 overflow-y-auto p-4 space-y-6"><!> <!></div></div> <!>`,
  1
), Zv = /* @__PURE__ */ p('<!> <div class="flex-1 overflow-y-auto"><!></div>', 1), ef = /* @__PURE__ */ p('<div class="w-80 border-l border-surface-600 overflow-hidden shrink-0"><!></div>'), tf = /* @__PURE__ */ p('<div class="flex h-full"><div class="flex-1 overflow-hidden"><!></div> <!></div>'), rf = /* @__PURE__ */ p('<div class="w-80 border-l border-surface-600 overflow-hidden shrink-0"><!></div>'), sf = /* @__PURE__ */ p('<div class="flex h-full"><div class="flex-1 overflow-hidden"><!></div> <!></div>'), af = /* @__PURE__ */ p('<div class="flex items-center justify-center h-full text-surface-400 text-sm">Select a division from the sidebar</div>'), nf = /* @__PURE__ */ p('<!> <div class="absolute top-2 right-2 flex items-center gap-1.5 text-[10px] z-10"><span></span> <span class="text-surface-500"> </span></div> <div class="flex flex-1 overflow-hidden relative"><!> <div class="flex-1 flex flex-col overflow-hidden"><!></div> <!></div>', 1), of = /* @__PURE__ */ p('<div class="flex flex-col h-full"><!></div>');
function cf(e, t) {
  Tt(t, !0);
  const r = () => Ae(ft, "$isLoading", $), a = () => Ae(bt, "$activeVenture", $), o = () => Ae(Ga, "$aiModel", $), c = () => Ae(ar, "$ventureError", $), u = () => Ae(ws, "$ventures", $), d = () => Ae(qa, "$showAIAssist", $), v = () => Ae(Vr, "$divisions", $), _ = () => Ae(ns, "$selectedDivision", $), x = () => Ae(ss, "$selectedPhase", $), b = () => Ae(Xs, "$ventureStep", $), y = () => Ae(jr, "$bigPicturePhase", $), P = () => Ae(ya, "$showEventStream", $), [$, N] = Kt();
  let C = xt(t, "api", 7), ee = /* @__PURE__ */ se(null), te = /* @__PURE__ */ se("connecting"), $e, he = /* @__PURE__ */ se(""), Te = /* @__PURE__ */ se(""), Q = /* @__PURE__ */ se(""), Ie = /* @__PURE__ */ se(!1), _e = /* @__PURE__ */ se(!1);
  function Je(De, we) {
    let me = De;
    if (we.trim()) {
      const F = we.toLowerCase();
      me = De.filter((Ce) => Ce.name.toLowerCase().includes(F) || Ce.brief && Ce.brief.toLowerCase().includes(F));
    }
    const ae = [], ve = [], re = [], ye = [];
    for (const F of me)
      et(F.status, _s) ? ye.push(F) : et(F.status, yi) || et(F.status, wi) ? ve.push(F) : F.phase === "initiated" || F.phase === "vision_refined" || F.phase === "vision_submitted" ? ae.push(F) : F.phase === "discovery_completed" || F.phase === "designing" || F.phase === "planning" || F.phase === "crafting" || F.phase === "deploying" ? re.push(F) : ae.push(F);
    const O = [];
    return ae.length > 0 && O.push({ label: "Setup", ventures: ae }), ve.length > 0 && O.push({ label: "Discovery", ventures: ve }), re.length > 0 && O.push({ label: "Building", ventures: re }), ye.length > 0 && O.push({ label: "Archived", ventures: ye }), O;
  }
  function xe(De) {
    return De.filter((we) => !et(we.status, _s)).sort((we, me) => (me.updated_at ?? "").localeCompare(we.updated_at ?? "")).slice(0, 5);
  }
  async function qe() {
    try {
      h(ee, await C().get("/health"), !0), h(te, "connected");
    } catch {
      h(ee, null), h(te, "disconnected");
    }
  }
  xi(async () => {
    gc(C()), qe(), $e = setInterval(qe, 5e3), nr(), As();
    const De = await bc();
    za.set(De);
  }), Qo(() => {
    $e && clearInterval($e);
  });
  async function Ue() {
    if (!s(he).trim()) return;
    await Pi(s(he).trim(), s(Te).trim() || "") && (h(he, ""), h(Te, ""), h(Ie, !1));
  }
  var Ke = {
    get api() {
      return C();
    },
    set api(De) {
      C(De), pt();
    }
  }, Ne = of(), K = i(Ne);
  {
    var w = (De) => {
      var we = Ov(), me = i(we), ae = i(me);
      ae.textContent = "◆", wt(2), n(me), n(we), f(De, we);
    }, G = (De) => {
      var we = Xv(), me = it(we), ae = i(me), ve = i(ae), re = i(ve);
      re.textContent = "◆";
      var ye = l(re, 4), O = i(ye), F = l(O, 2), Ce = i(F, !0);
      n(F), n(ye);
      var Re = l(ye, 2);
      ra(Re, {
        get currentModel() {
          return o();
        },
        onSelect: (B) => Ya(B)
      });
      var Ge = l(Re, 4);
      ht(Ge);
      var S = l(Ge, 2);
      S.__click = () => h(Ie, !s(Ie));
      var k = i(S, !0);
      n(S), n(ve), n(ae);
      var W = l(ae, 2), V = i(W);
      {
        var X = (B) => {
          var ue = Rv(), ne = l(i(ue), 2), ie = i(ne), fe = l(i(ie), 2);
          ht(fe), n(ie);
          var Le = l(ie, 2), ce = l(i(Le), 2);
          ht(ce), n(Le), n(ne);
          var pe = l(ne, 2);
          {
            var He = (Y) => {
              var be = Lv(), g = i(be, !0);
              n(be), D(() => E(g, c())), f(Y, be);
            };
            I(pe, (Y) => {
              c() && Y(He);
            });
          }
          var je = l(pe, 2), le = i(je);
          le.__click = Ue;
          var j = i(le, !0);
          n(le);
          var J = l(le, 2);
          J.__click = () => ur("Help me define a new venture. What should I consider? Ask me about the problem domain, target users, and core functionality."), J.textContent = "✦ AI Help", n(je), n(ue), D(
            (Y, be) => {
              le.disabled = Y, Fe(le, 1, `px-4 py-2 rounded-lg text-xs font-medium transition-colors
									${be ?? ""}`), E(j, r() ? "Initiating..." : "Initiate Venture");
            },
            [
              () => !s(he).trim() || r(),
              () => !s(he).trim() || r() ? "bg-surface-600 text-surface-400 cursor-not-allowed" : "bg-hecate-600 text-surface-50 hover:bg-hecate-500"
            ]
          ), lt(fe, () => s(he), (Y) => h(he, Y)), lt(ce, () => s(Te), (Y) => h(Te, Y)), f(B, ue);
        };
        I(V, (B) => {
          s(Ie) && B(X);
        });
      }
      var oe = l(V, 2);
      {
        var Z = (B) => {
          var ue = Fv(), ne = i(ue);
          ne.textContent = "◆";
          var ie = l(ne, 6);
          ie.__click = () => h(Ie, !0), n(ue), f(B, ue);
        }, ke = (B) => {
          const ue = /* @__PURE__ */ ge(() => Je(u(), s(Q)));
          var ne = Jv(), ie = it(ne);
          {
            var fe = (le) => {
              const j = /* @__PURE__ */ ge(() => xe(u()));
              var J = Nr(), Y = it(J);
              {
                var be = (g) => {
                  var m = Bv(), L = l(i(m), 2);
                  We(L, 21, () => s(j), ot, (A, M) => {
                    var q = jv();
                    q.__click = () => bs(s(M));
                    var U = i(q), R = i(U);
                    R.textContent = "◆";
                    var z = l(R, 2), de = i(z, !0);
                    n(z);
                    var Se = l(z, 2), Pe = i(Se, !0);
                    n(Se), n(U);
                    var Be = l(U, 2);
                    {
                      var Me = (Ee) => {
                        var Ve = Vv(), Ye = i(Ve, !0);
                        n(Ve), D(() => E(Ye, s(M).brief)), f(Ee, Ve);
                      };
                      I(Be, (Ee) => {
                        s(M).brief && Ee(Me);
                      });
                    }
                    n(q), D(() => {
                      E(de, s(M).name), E(Pe, s(M).status_label ?? s(M).phase);
                    }), f(A, q);
                  }), n(L), n(m), f(g, m);
                };
                I(Y, (g) => {
                  s(j).length > 0 && g(be);
                });
              }
              f(le, J);
            }, Le = /* @__PURE__ */ ge(() => !s(Q).trim() && u().filter((le) => !et(le.status, _s)).length > 3);
            I(ie, (le) => {
              s(Le) && le(fe);
            });
          }
          var ce = l(ie, 2);
          We(ce, 17, () => s(ue), ot, (le, j) => {
            var J = Nr(), Y = it(J);
            {
              var be = (m) => {
                var L = Uv(), A = i(L);
                A.__click = () => h(_e, !s(_e));
                var M = i(A), q = i(M, !0);
                n(M);
                var U = l(M), R = l(U), z = i(R);
                n(R), n(A);
                var de = l(A, 2);
                {
                  var Se = (Pe) => {
                    var Be = qv();
                    We(Be, 21, () => s(j).ventures, ot, (Me, Ee) => {
                      var Ve = Wv();
                      Ve.__click = () => bs(s(Ee));
                      var Ye = i(Ve), nt = i(Ye);
                      nt.textContent = "◆";
                      var ct = l(nt, 2), ut = i(ct, !0);
                      n(ct), wt(2), n(Ye);
                      var jt = l(Ye, 2);
                      {
                        var Jt = (gr) => {
                          var cs = Hv(), Ds = i(cs, !0);
                          n(cs), D(() => E(Ds, s(Ee).brief)), f(gr, cs);
                        };
                        I(jt, (gr) => {
                          s(Ee).brief && gr(Jt);
                        });
                      }
                      n(Ve), D(() => E(ut, s(Ee).name)), f(Me, Ve);
                    }), n(Be), f(Pe, Be);
                  };
                  I(de, (Pe) => {
                    s(_e) && Pe(Se);
                  });
                }
                n(L), D(() => {
                  E(q, s(_e) ? "▼" : "▶"), E(U, ` ${s(j).label ?? ""} `), E(z, `(${s(j).ventures.length ?? ""})`);
                }), f(m, L);
              }, g = (m) => {
                var L = Yv(), A = i(L), M = i(A, !0);
                n(A);
                var q = l(A, 2);
                We(q, 21, () => s(j).ventures, ot, (U, R) => {
                  var z = Gv();
                  z.__click = () => bs(s(R));
                  var de = i(z), Se = i(de);
                  Se.textContent = "◆";
                  var Pe = l(Se, 2), Be = i(Pe, !0);
                  n(Pe);
                  var Me = l(Pe, 2), Ee = i(Me, !0);
                  n(Me), n(de);
                  var Ve = l(de, 2);
                  {
                    var Ye = (nt) => {
                      var ct = zv(), ut = i(ct, !0);
                      n(ct), D(() => E(ut, s(R).brief)), f(nt, ct);
                    };
                    I(Ve, (nt) => {
                      s(R).brief && nt(Ye);
                    });
                  }
                  n(z), D(() => {
                    E(Be, s(R).name), E(Ee, s(R).status_label ?? s(R).phase);
                  }), f(U, z);
                }), n(q), n(L), D(() => E(M, s(j).label)), f(m, L);
              };
              I(Y, (m) => {
                s(j).label === "Archived" ? m(be) : m(g, !1);
              });
            }
            f(le, J);
          });
          var pe = l(ce, 2);
          {
            var He = (le) => {
              var j = Kv(), J = i(j);
              n(j), D(() => E(J, `No ventures matching "${s(Q) ?? ""}"`)), f(le, j);
            }, je = /* @__PURE__ */ ge(() => s(ue).length === 0 && s(Q).trim());
            I(pe, (le) => {
              s(je) && le(He);
            });
          }
          f(B, ne);
        };
        I(oe, (B) => {
          u().length === 0 && !s(Ie) ? B(Z) : B(ke, !1);
        });
      }
      n(W), n(me);
      var T = l(me, 2);
      {
        var H = (B) => {
          var ue = Qv(), ne = i(ue);
          Ca(ne, {}), n(ue), f(B, ue);
        };
        I(T, (B) => {
          d() && B(H);
        });
      }
      D(() => {
        Fe(O, 1, `inline-block w-1.5 h-1.5 rounded-full ${s(te) === "connected" ? "bg-success-400" : s(te) === "connecting" ? "bg-yellow-400 animate-pulse" : "bg-danger-400"}`), E(Ce, s(te) === "connected" ? `v${s(ee)?.version ?? "?"}` : s(te)), Fe(S, 1, `px-3 py-1.5 rounded-lg text-xs font-medium transition-colors
							${s(Ie) ? "bg-surface-600 text-surface-300" : "bg-hecate-600 text-surface-50 hover:bg-hecate-500"}`), E(k, s(Ie) ? "Cancel" : "+ New Venture");
      }), lt(Ge, () => s(Q), (B) => h(Q, B)), f(De, we);
    }, Oe = (De) => {
      var we = nf(), me = it(we);
      Li(me, {});
      var ae = l(me, 2), ve = i(ae), re = l(ve, 2), ye = i(re, !0);
      n(re), n(ae);
      var O = l(ae, 2), F = i(O);
      {
        var Ce = (oe) => {
          Ri(oe, {});
        };
        I(F, (oe) => {
          v().length > 0 && oe(Ce);
        });
      }
      var Re = l(F, 2), Ge = i(Re);
      {
        var S = (oe) => {
          var Z = Zv(), ke = it(Z);
          Wi(ke, {});
          var T = l(ke, 2), H = i(T);
          {
            var B = (ne) => {
              ji(ne, {});
            }, ue = (ne) => {
              Hi(ne, {});
            };
            I(H, (ne) => {
              x() === "planning" ? ne(B) : x() === "crafting" && ne(ue, 1);
            });
          }
          n(T), f(oe, Z);
        }, k = (oe) => {
          var Z = Nr(), ke = it(Z);
          {
            var T = (fe) => {
              var Le = tf(), ce = i(Le), pe = i(ce);
              ka(pe, {}), n(ce);
              var He = l(ce, 2);
              {
                var je = (le) => {
                  var j = ef(), J = i(j);
                  Ea(J, {}), n(j), f(le, j);
                };
                I(He, (le) => {
                  P() && le(je);
                });
              }
              n(Le), f(fe, Le);
            }, H = (fe) => {
              Vi(fe, {});
            }, B = (fe) => {
              Is(fe, { nextAction: "discovery" });
            }, ue = (fe) => {
              var Le = sf(), ce = i(Le), pe = i(ce);
              ka(pe, {}), n(ce);
              var He = l(ce, 2);
              {
                var je = (le) => {
                  var j = rf(), J = i(j);
                  Ea(J, {}), n(j), f(le, j);
                };
                I(He, (le) => {
                  P() && le(je);
                });
              }
              n(Le), f(fe, Le);
            }, ne = (fe) => {
              Is(fe, { nextAction: "identify" });
            }, ie = (fe) => {
              Is(fe, { nextAction: "discovery" });
            };
            I(ke, (fe) => {
              b() === "discovering" || y() !== "ready" ? fe(T) : b() === "initiated" || b() === "vision_refined" ? fe(H, 1) : b() === "vision_submitted" ? fe(B, 2) : b() === "discovery_paused" ? fe(ue, 3) : b() === "discovery_completed" ? fe(ne, 4) : fe(ie, !1);
            });
          }
          f(oe, Z);
        }, W = (oe) => {
          var Z = af();
          f(oe, Z);
        };
        I(Ge, (oe) => {
          _() ? oe(S) : v().length === 0 ? oe(k, 1) : oe(W, !1);
        });
      }
      n(Re);
      var V = l(Re, 2);
      {
        var X = (oe) => {
          Ca(oe, {});
        };
        I(V, (oe) => {
          d() && oe(X);
        });
      }
      n(O), D(() => {
        Fe(ve, 1, `inline-block w-1.5 h-1.5 rounded-full ${s(te) === "connected" ? "bg-success-400" : s(te) === "connecting" ? "bg-yellow-400 animate-pulse" : "bg-danger-400"}`), E(ye, s(te) === "connected" ? `v${s(ee)?.version ?? "?"}` : s(te));
      }), f(De, we);
    };
    I(K, (De) => {
      r() && !a() ? De(w) : a() ? De(Oe, !1) : De(G, 1);
    });
  }
  n(Ne), f(e, Ne);
  var ze = Mt(Ke);
  return N(), ze;
}
Ft(["click"]);
customElements.define("martha-studio", Vt(cf, { api: {} }, [], []));
export {
  cf as default
};
