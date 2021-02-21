/**
 * Functions for working with intervals in an EDO
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module edo
 **/

const {gcd, egcd} = require('mathutils');
const Fraction = require('fraction.js');
const Interval = require('./interval.js');
const py = require('./pythagorean.js');

function mod(a,n) {
  return ((a % n) + n) % n;
}

/**
  * Returns the EDO step closest to the given interval
  *
  * @param {integer} edo
  * @param {Interval} i
  * @returns {integer}
  */
function edoApprox(edo,a,b) {
  return Math.round(edo * Interval(a,b).toCents() / 1200);
}

/**
  * Returns the EDO step which corresponds to the given pythagorean interval,
  * where a P5 corresponds to `edoApprox(edo,Interval(3,2))`
  *
  * @param {integer} edo
  * @param {Interval} i
  * @returns {integer}
  */
function edoPy(edo,a,b) {
  const i = Interval(a,b);
  const g = Fraction(py.generator(i) * edoApprox(edo,3,2), 4);
  const v = py.octaves(i);
  if (g.d != 1) {
    throw edo + "-EDO has no " + py.pySymb(i,{verbosity:1}) + " interval"
  }
  return g.s*g.n + v * edo;
}

/**
  * Returns the pythagorean interval with the smallest generator which
  * corresponds to the given EDO step, i.e. for which `edoPy` returns the given
  * EDO step. Note that there may be no such interval, in which case this
  * function returns `undefined`.
  *
  * All other intervals for which `edoPy` returns the given EDO step are
  * the result of this function times some power of `edoPyComma`.
  *
  * In summary, for every pythagorean interval i there exists some integer k
  * such that `edoPyInv(edo, edoPy(edo, i)) == i.mul(edoPyComma(edo).pow(k))`,
  * and for all integers n and k, either `edoPyInv(edo, n)` is undefined or
  * `edoPy(edo, edoPyInv(edo, n).mul(edoPyComma(edo).pow(k))) == n`.
  *
  * @param {integer} edo
  * @param {integer} n
  * @returns {Interval}
  */
function edoPyInv(edo,n) {
  const p5 = edoApprox(edo,3,2);
  // d == x * p5 + 4 * y * edo
  const [d, x, y] = egcd(p5, 4 * edo);
  if ((4 * n) % d == 0) {
    // n == (g / 4) * p5 + o * edo
    const [g, o] = [x * (4*n/d), y * (4*n/d)];
    // n == (g_bal / 4) * p5 + o_bal * edo && -(edo/d)/2 < g_bal/4 <= (edo/d)/2
    const edo_2d = Math.floor((4*edo/d - 1) / 2);
    const g_bal = mod(g + edo_2d, 4*edo/d) - edo_2d;
    const o_bal = o - p5 * ((g_bal - g) / (4 * edo));
    return Interval(3,2).pow(g_bal,4).mul(Interval(2).pow(o_bal));
  }
}

/**
  * The smallest pythagorean interval tempered out in the given edo (I believe
  * this is the correct description...)
  *
  * @param {integer} edo
  * @returns {Interval}
  */
function edoPyComma(edo) {
  const p5 = edoApprox(edo,3,2);
  const d = gcd(p5, 4 * edo);
  return Interval(3,2).pow(-4 * edo / d, 4).mul(Interval(2).pow(p5 / d));
}

/**
  * Checks whether neutral pythagorean intervals are realized in the given EDO,
  * i.e. if `edoApprox(edo,Interval(3,2))` is divisible by 2
  *
  * @param {integer} edo
  * @returns {boolean}
  */
function edoHasNeutrals(edo) {
  return edoApprox(edo,3,2) % 2 == 0;
}

/**
  * Checks whether semi-neutral pythagorean intervals are realized in the given
  * EDO, i.e. if `edoApprox(edo,Interval(3,2))` is divisible by 4
  *
  * @param {integer} edo
  * @returns {boolean}
  */
function edoHasSemiNeutrals(edo) {
  return edoApprox(edo,3,2) % 4 == 0;
}

// used in `updnsSymb` and `updnsNote`
function fillGens(edo, g, lo, hi) {
  let steps = [];
  for (let i = 0; i < edo; i++) { steps.push(Array(0)); }
  if (lo <= 0 && 0 <= hi) {
    steps[0].push(0)
  }
  for (let k = 1; k <= Math.max(Math.abs(lo), Math.abs(hi)); k++) {
    if (lo <=  k &&  k <= hi) { steps[mod( k*g,edo)].push( k); }
    if (lo <= -k && -k <= hi) { steps[mod(-k*g,edo)].push(-k); }
  }
  return steps;
}

// used in `updnsSymb` and `updnsNote`
function addUpdns(edo, steps) {
  let new_steps = steps.map(_ => Array(0));
  let [last_below, last_above] = [0,edo];
  for (let i = 0; i < edo; i++) {
    if (steps[i].length == 0) {
      new_steps[i].push(...steps[last_below].map(k => [i - last_below, k]));
    }
    else {
      new_steps[i].push(...steps[i].map(k => [0,k]));
      last_below = i;
    }
    const j = (edo-1)-i;
    if (steps[j].length == 0) {
      new_steps[j].push(...steps[mod(last_above,edo)].map(k => [j - last_above, k]));
    }
    else {
      last_above = j;
    }
  }
  for (let i = 0; i < edo; i++) {
    let [minUpdns, hasNonNeutral] = [edo, false];
    for (const [uds, k] of new_steps[i]) {
      minUpdns = Math.min(minUpdns, Math.abs(uds));
      hasNonNeutral = hasNonNeutral || Number.isInteger(k);
    }
    new_steps[i] = new_steps[i].filter(udsk => Math.abs(udsk[0]) <= minUpdns
                                               && (!hasNonNeutral || Number.isInteger(udsk[1])))
                               .sort((a,b) => a[0] == b[0] ? Math.abs(a[1]) - Math.abs(b[1])
                                                           : b[0] - a[0]);
  }
  return new_steps;
}

// used in `updnsSymb` and `updnsNote`
function cvtGensToPy(edo, steps) {
  for (let i = 0; i < edo; i++) {
    for (let j = 0; j < steps[i].length; j++) {
      let v = Interval(steps[i][j][1] == 0 && steps[i][j][0] < 0 ? 2 : 1);
      steps[i][j][1] = Interval(3,2).pow(steps[i][j][1]).red().mul(v);
    }
  }
  return steps;
}

let upsdnsSymbCache_var = {};

function updnsSymbCache(edo) {
  if (upsdnsSymbCache_var[edo]) {
    return upsdnsSymbCache_var[edo];
  }
  const fifth = edoApprox(edo,3,2);
  let [lo, hi] = [-6, 6]; // d5 m2 m6 m3 m7 P4 | P1 | P5 M2 M6 M3 M7 A4
  // Special case for perfect EDOs
  if (fifth/edo == 4/7) {
    [lo, hi] = [-1.5, 1.5]; // (~2) P4 (~6) | P1 | (~3) P5 (~7)
  }
  // Special case for pentatonic EDOs
  if (fifth/edo == 3/5) {
    [lo, hi] = [-4, 4]; // m6 m3 m7 P4 | P1 | P5 M2 M6 M3
  }
  let steps;
  if (fifth % 2 != 0) {
    steps = fillGens(edo, fifth, lo, hi);
  } else {
    steps = fillGens(edo, fifth/2, 2*lo, 2*hi);
    for (let i = 0; i < edo; i++) {
      steps[i] = steps[i].filter(k => k % 2 == 0 || Math.abs(k) <= 6).map(k => k/2);
    }
  }
  steps = cvtGensToPy(edo, addUpdns(edo, steps));
  upsdnsSymbCache_var[edo] = steps;
  return steps;
}

/**
  * Returns the ups-and-downs notation symbol for the given steps in the given
  * EDO
  *
  * @param {integer} edo
  * @param {integer} n
  * @returns {string}
  */
function updnsSymb(edo,n) {
  const nr = mod(n,edo);
  const vs = Interval(2).pow(n - nr, edo);
  const cache = updnsSymbCache(edo)[nr];
  let ret = [];
  for (let i = 0; i < cache.length; i++) {
    const updns = (cache[i][0] > 0 ? '^' : 'v').repeat(Math.abs(cache[i][0]));
    const str = updns + py.pySymb(cache[i][1].mul(vs));
    ret.push(str.replace("n","~").replace("sA","~").replace("sd","~"));
  }
  return ret;
}

let upsdnsNoteCache_var = {};

function updnsNoteCache(edo) {
  if (upsdnsNoteCache_var[edo]) {
    return upsdnsNoteCache_var[edo];
  }
  const fifth = edoApprox(edo,3,2);
  let [lo, hi] = [-9, 7]; // Gb Db Ab Eb Bb F C G D | A | E B F# C# G# D# A#
  // Special case for perfect EDOs
  if (fifth/edo == 4/7) {
    [lo, hi] = [-4, 2]; // F C G D | A | E B
  }
  // Special case for EDOs between perfect and 12-EDO
  if (fifth/edo > 4/7 && fifth/edo < 7/12) {
    [lo, hi] = [-11, 9]; // Fb Cb Gb ... | A | ... A# E# B#
  }
  const steps = cvtGensToPy(edo, addUpdns(edo, fillGens(edo, fifth, lo, hi)));
  upsdnsNoteCache_var[edo] = steps;
  return steps;
}

/**
  * Returns the ups-and-downs notation note name for the given steps to A4 in
  * the given EDO. The returned string uses ASCII instead of uniode wherever
  * possible iff the third argument is given and is true
  *
  * @param {integer} edo
  * @param {integer} n
  * @param {Boolean} [useASCII=false]
  * @returns {string}
  */
function updnsNote(edo, n, useASCII) {
  const nr = mod(n,edo);
  const vs = Interval(2).pow(n - nr, edo);
  const cache = updnsNoteCache(edo)[nr];
  let ret = [];
  for (let i = 0; i < cache.length; i++) {
    const updns = (cache[i][0] > 0 ? '^' : 'v').repeat(Math.abs(cache[i][0]));
    const str = updns + py.pyNote(cache[i][1].mul(vs), useASCII);
    ret.push(str);
  }
  return ret;
}

module['exports'].edoApprox = edoApprox;
module['exports'].edoPy = edoPy;
module['exports'].edoPyInv = edoPyInv;
module['exports'].edoPyComma = edoPyComma;
module['exports'].edoHasNeutrals = edoHasNeutrals;
module['exports'].edoHasSemiNeutrals = edoHasSemiNeutrals;
module['exports'].updnsSymbCache = updnsSymbCache;
module['exports'].updnsSymb = updnsSymb;
module['exports'].updnsNoteCache = updnsNoteCache;
module['exports'].updnsNote = updnsNote;
