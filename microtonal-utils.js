(function(f){if(typeof exports==="object"&&typeof module!=="undefined"){module.exports=f()}else if(typeof define==="function"&&define.amd){define([],f)}else{var g;if(typeof window!=="undefined"){g=window}else if(typeof global!=="undefined"){g=global}else if(typeof self!=="undefined"){g=self}else{g=this}g.microtonal_utils = f()}})(function(){var define,module,exports;return (function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
/**
 * Best rational and EDO approximations of intervals
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module approx
 **/

const pf = require('primes-and-factors');
const Fraction = require('fraction.js');
const Interval = require('./interval.js');
const {edoApprox} = require('./edo.js');

function signPerms(intv) {
  const keys = Object.keys(intv);
  let ret = [];
  for (let bits = 0; bits < (1 << keys.length); bits++) {
    ret.push(keys.map((_,i) => (bits & (1 << i)) == 0 ? 1 : -1));
  }
  return ret;
}

function applySignPerm(sp, intv) {
  let [i, ret] = [0, {}];
  for (const [p,e] of Object.entries(intv)) {
    ret[p] = e.mul(sp[i]);
    i++;
  }
  return Interval(ret);
}

/**
  * Finds best rational approximations of the given interval, sorted by Tenney
  * harmonic distance. Returns a pair whose first element is true iff no better
  * approximaions can be found - i.e. if either an exact approximation is found
  * or there are no more intervals in the given odd-limit to check.
  *
  * @param {Interval} i
  * @param {Object} [opts]
  * @param {integer} [opts.cutoff] defaults to 50 cents
  * @param {integer} [opts.primeLimit]
  * @param {integer} [opts.oddLimit]
  * @param {integer} [opts.startIteration] defaults to 0
  * @param {integer} [opts.numIterations] defaults to 1
  * @param {boolean} [opts.useExactDiffs] defaults to false, controls the type
  *                                       of each 'diff' property
  * @returns {Pair.<boolean, Array.<{ratio: Fraction, diff: (number|Interval)}>>}
  */
function bestRationalApproxs(a,b, opts) {
  // if only two arguments are given, the second one may be `opts`!
  if (!opts) {
    if (typeof b == 'object' && b != null) {
      opts = b;
      b = undefined;
    } else {
      opts = {};
    }
  }
  const intv = Interval(a,b);
  console.time("rationalApprox");
  let {cutoff, primeLimit, oddLimit, startIteration, numIterations, useExactDiffs} = opts;
  let [hitOddLimitMax, foundExact] = [false, false];

  // some heuristics for the iteration size, i.e. the number of odd numbers
  // to check in a given iteration
  let iterationSize = 100;
  if (primeLimit) {
    // for large prime limits, this iteration size is approximately half the
    // prime limit itself, but for small prime limits (roughly less than 47)
    // this is larger, to account for the fact that valid intervals are sparser
    iterationSize = Math.ceil(2000/primeLimit + (primeLimit+1)/2);
    // a prime limit of 2 means we also have an odd limit of 1!
    if (primeLimit <= 2) { oddLimit = 1; }
  }

  // the size of the largest odd number which would generate a valid interval
  // in our odd limit
  const oddLimit_max = oddLimit * Math.abs(oddLimit-2);
  if (oddLimit) {
    iterationSize = Math.min(iterationSize, Math.ceil((oddLimit_max+1)/2));
  }

  if (cutoff == undefined) { cutoff = Interval(2).pow(1,12).sqrt(); }
  if (primeLimit == undefined && oddLimit) { primeLimit = oddLimit; }
  if (startIteration == undefined) { startIteration = 0; }
  if (numIterations == undefined) { numIterations = 1; }
  let n_max = (startIteration + numIterations) * iterationSize;

  // if our n_max is greater than the largest odd number which would generate a
  // valid interval in our odd limit, we don't have to check any more than that!
  if (oddLimit && n_max >= (oddLimit_max+1)/2) {
    n_max = (oddLimit_max+1)/2;
    hitOddLimitMax = true;
  }

  const intv_red = intv.red();
  const vs = intv.div(intv_red);
  let [last_diff, ret] = [Interval(2), []];
  // this loop iterates through all odd numbers `2*n + 1` for `n` in the range
  // `[startIteration * iterationSize + 1, numIterations * iterationSize)`
  for (let n = startIteration * iterationSize; !foundExact && n < n_max; n++) {
    const i = Interval(2*n + 1);
    if (primeLimit && Object.keys(i).some(p => p > primeLimit)) {
      continue;
    }
    // For a given odd `i` with factorization `p1^e1 ... pm^em` (where `pk` is
    // prime and `ek > 0` for all `k`), `i_perms` is the array of all intervals
    // with factorizations `p1^(+/- e1) ... pm^(+/- em)`. For example, if
    // `i = 45` then `i = 3^2 * 5^1` and
    // `i_perms = [ 3^2 * 5^1, 3^(-2) * 5^1, 3^2 * 5^(-1), 3^(-2) * 5(-1) ]`.
    const i_perms = signPerms(i).map(sp => applySignPerm(sp, i));
    // For each of these factorizations, we then add in the power of 2 which
    // gets it closest to `intv`, then package the result up with its difference
    // to `intv`. We do the former by finding the balanced octave-reduced
    // difference to `intv` then adding this difference back to `intv`; the
    // result will always be our original factorization times the power of 2
    // which minimizes its difference to `intv`
    const to_check = i_perms.map(function (j) {
                       const diff = j.div(intv).reb();
                       return [intv.mul(diff), diff];
                     }).sort((a,b) => a[1].compare(b[1]));
    for (const [j, diff] of to_check) {
      const ratio = j.toFrac();
      if (oddLimit && (   (ratio.n % 2 != 0 && ratio.n > oddLimit)
                       || (ratio.d % 2 != 0 && ratio.d > oddLimit))) {
        continue;
      }
      const abs_diff = diff.compare(1) < 0 ? diff.recip() : diff;
      if (abs_diff.compare(cutoff) < 0 && abs_diff.compare(last_diff) <= 0) {
        ret.push({ ratio: ratio, diff: useExactDiffs ? diff : diff.toCents() });
        last_diff = abs_diff;
        if (last_diff.equals(1)) { foundExact = true };
      }
    }
  }
  console.timeEnd("rationalApprox");
  if (hitOddLimitMax || foundExact) { console.log("rationalApprox: exhausted") }
  return [hitOddLimitMax || foundExact, ret];
}

/**
  * Finds best EDO step approximations of the given interval, sorted by EDO
  * size.
  *
  * @param {Interval} i
  * @param {Object} [opts]
  * @param {integer} [opts.cutoff] defaults to 50 cents
  * @param {integer} [opts.startEDO] defaults to 5
  * @param {integer} [opts.endEDO] defaults to 60
  * @param {boolean} [opts.useExactDiffs] defaults to false, controls the type
  *                                       of each 'diff' property
  * @returns {Array.<{steps: Array, diff: (number|Interval)}>}
  */
function bestEDOApproxsByEDO(a,b, opts) {
  // if only two arguments are given, the second one may be `opts`!
  if (!opts) {
    if (typeof b == 'object' && b != null) {
      opts = b;
      b = undefined;
    } else {
      opts = {};
    }
  }
  const intv = Interval(a,b);
  if (opts == undefined) { opts = {}; }
  let {cutoff, startEDO, endEDO, useExactDiffs} = opts;
  if (cutoff == undefined) { cutoff = Interval(2).pow(1,12).sqrt(); }
  if (startEDO == undefined) { startEDO = 5; }
  if (endEDO == undefined) { endEDO = 60; }

  let foundExact = false;
  let [last_diff, ret] = [Interval(2), []];
  for (let edo = startEDO; !foundExact && edo <= endEDO; edo++) {
    const steps = edoApprox(edo, intv);
    const diff = intv.div(Interval(2).pow(steps,edo));
    const abs_diff = diff.compare(1) < 0 ? diff.recip() : diff;
    if (abs_diff.compare(cutoff) < 0 && abs_diff.compare(last_diff) <= 0) {
      if (abs_diff.compare(last_diff) == 0) {
        ret[ret.length - 1].steps.push([steps, edo]);
      }
      else {
        ret.push({ steps: [[steps,edo]], diff: useExactDiffs ? diff : diff.toCents() });
        last_diff = abs_diff;
        if (last_diff.equals(1)) { foundExact = true };
      }
    }
  }

  return ret;
}

/**
  * Finds best EDO step approximations of the given interval, sorted by error.
  *
  * @param {Interval} i
  * @param {Object} [opts]
  * @param {integer} [opts.startEDO] defaults to 5
  * @param {integer} [opts.endEDO] defaults to 60
  * @param {boolean} [opts.useExactDiffs] defaults to false, controls the type
  *                                       of each 'diff' property
  * @returns {Array.<{steps: Array, diff: (number|Interval)}>}
  */
function bestEDOApproxsByDiff(a,b, opts) {
  // if only two arguments are given, the second one may be `opts`!
  if (!opts) {
    if (typeof b == 'object' && b != null) {
      opts = b;
      b = undefined;
    } else {
      opts = {};
    }
  }
  const intv = Interval(a,b);
  if (opts == undefined) { opts = {}; }
  let {startEDO, endEDO, useExactDiffs} = opts;
  if (startEDO == undefined) { startEDO = 5; }
  if (endEDO == undefined) { endEDO = 60; }

  let ret = [];
  for (let edo = startEDO; edo <= endEDO; edo++) {
    const steps = edoApprox(edo, intv);
    const diff = intv.div(Interval(2).pow(steps,edo));
    const abs_diff = diff.compare(1) < 0 ? diff.recip() : diff;
    const to_add = { steps: [[steps, edo]], diff: diff, abs_diff: abs_diff };
    let added = false;
    for (let i = 0; !added && i < ret.length; i++) {
      if (diff.equals(ret[i].diff)) {
        ret[i].steps.push([steps,edo]);
        added = true;
      }
      else if (abs_diff.compare(ret[i].abs_diff) < 0) {
        ret.splice(i, 0, to_add);
        added = true;
      }
    }
    if (!added) {
      ret.push(to_add);
    }
  }

  return ret.map(x => ({ steps: x.steps, diff: useExactDiffs ? x.diff : x.diff.toCents() }));
}

module.exports.bestRationalApproxs = bestRationalApproxs;
module.exports.bestEDOApproxsByEDO = bestEDOApproxsByEDO;
module.exports.bestEDOApproxsByDiff = bestEDOApproxsByDiff;

},{"./edo.js":2,"./interval.js":6,"fraction.js":12,"primes-and-factors":16}],2:[function(require,module,exports){
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
  const g = Fraction(py.pyGenerator(i) * edoApprox(edo,3,2), 4);
  const v = py.pyOctaves(i);
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

},{"./interval.js":6,"./pythagorean.js":11,"fraction.js":12,"mathutils":13}],3:[function(require,module,exports){
/**
 * English names for intervals based on the Neutral FJS and ups-and-downs
 * notations (very much incomplete!)
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module english
 **/

const pf = require('primes-and-factors');
const Fraction = require('fraction.js');
const Interval = require('./interval.js');
const {pySymb, pyGenerator} = require('./pythagorean.js');
const {fjsFifthShift, fjsAccidentals, nfjsParams} = require('./fjs.js');
const {updnsSymbCache} = require('./edo.js');

const primeNames = { '5':  ["classic", "cls."]
                   , '7':  ["septimal", "sep."]
                   , '11': ["undecimal", "und."]
                   , '13': ["tridecimal", "trid."]
                   , '17': ["septendecimal", "sepd."]
                   , '19': ["undevicesimal", "undv."] };

/**
  * Attempts to give english names to the given interval based on the
  * Neutral FJS and ups-and-downs notations.
  *
  * @param {Interval} i
  * @param {{abbreviate: boolean, prefEDO: }=} opts
  * @returns {Array.<string>}
  */
function enNames(a,b, opts) {
  // if only two arguments are given, the second one may be `opts`!
  if (!opts && typeof b == 'object' && b != null) {
      opts = b;
      b = undefined;
  }
  const intv = Interval(a,b);
  const abbreviate = (opts || {}).abbreviate ? 1 : 0;
  const verbosity  = abbreviate ? 1 : 2;
  const prefEDO    = (opts || {}).prefEDO;

  let nms = [];

  // random special cases
  if (intv.equals(Interval(2).sqrt())) {
    nms.push("tritone");
  }

  // Neutral FJS intervals
  const fjs = fjsAccidentals(a,b, nfjsParams);
  if (fjs) {
    let pyi_symb = pySymb(fjs.pyi, {verbosity: verbosity});
    const resFact = Object.entries(intv).filter(pe => pe[0] > 3);
    // FJS intervals with no accidentials and a factor of 3 are Pythagorean
    if (resFact.length == 0) {
      if (intv['3'] && (intv['3'].d != 1 || intv['3'].n > 1)) {
        nms.push((abbreviate ? "py. " : "pythagorean ") + pyi_symb);
      }
      else {
        nms.push(pyi_symb);
      }
    }
    // FJS intervals with a single prime (>3) factor might be able to be named
    else if (resFact.length == 1) {
      const [p,e] = resFact[0];
      // We don't consider cases where the prime doesn't have the name, the FJS
      //  accidental is not an integer, or the pythagorean interval is an
      //  octave
      if (primeNames[p] && e.d == 1 && pyGenerator(fjs.pyi) != 0) {
        const fifthShift = fjsFifthShift(p, nfjsParams);
        const g = fjs.pyi['3'] || Fraction(0);
        // Ensure otonality matches (e.g. let through "M3^5" but not "M3_5")
        //  and neutral-ness matches (e.g. let through "M3^1" but not "n3^5")
        if (e.s == fifthShift.s * g.s && g.d == fifthShift.d) {
          // Ensure multiplicity matches, i.e. n-aug/dim have (n+1) primes
          //  (e.g. let through M3^5 and A4^5,5 but not M3^5,5 or A4^5)
          let multiplicityMatches = false;
          // Well, for primes with non-neutral fifth shifts, we do exactly
          //  what's stated above...
          if (fifthShift.d == 1) {
            if (g.n == 6) {
              multiplicityMatches = (e.n == 2);
            }
            else {
              multiplicityMatches = (e.n == 2 + Math.floor((g.n - 6)/7))
            }
          }
          // ...but for primes with neutral fifth shifts, we just handle cases
          //  where the neutral interval is small, since it's not clear to me
          //  what to do in the general case
          if (fifthShift.d == 2) {
            multiplicityMatches = (g.n <= 11 && e.n == 1);
          }
          if (multiplicityMatches) {
            // make sure we don't have "perfect" in the name for a 4th or 5th
            if (Math.abs(pyGenerator(fjs.pyi)) == 4) {
              const typ = intv.compare(fjs.pyi) > 0 ? "super" : "sub";
              if (abbreviate) { pyi_symb = pyi_symb.replace("perfect", typ); }
              else { pyi_symb = pyi_symb.replace("perfect ", typ + "-"); }
            }
            nms.push(primeNames[p][abbreviate] + " " + pyi_symb.replace("perfect ", ""));
          }
        }
      }
    }
  }

  // ups-and-downs intervals
  else if (Object.entries(intv).length == (intv['2'] != null)) {
    const e2 = intv['2'] || Fraction(0);
    const edo = prefEDO ? prefEDO : e2.d;
    const edo_str = edo + "-EDO ";
    let intv_strs = [];
    if (e2.mul(edo).d == 1 && (prefEDO || edo <= 60)) {
      const n = e2.s * e2.mul(edo).n;
      const n_mod = ((n % edo) + edo) % edo;
      for (const [uds, pyi] of updnsSymbCache(edo)[n_mod]) {
        let uds_str = "";
        if      (uds ==  1) { uds_str = "up"; }
        else if (uds == -1) { uds_str = "down"; }
        else if (uds ==  2 && !abbreviate) { uds_str = "double-up "; }
        else if (uds == -2 && !abbreviate) { uds_str = "double-down "; }
        else if (uds >=  2) { uds_str = uds + "-up "; }
        else if (uds <= -2) { uds_str = uds + "-down "; }
        let pyi_symb = pySymb(pyi, {verbosity: verbosity});
        if (abbreviate) { pyi_symb = pyi_symb.replace("perfect", ""); }
        else { pyi_symb = pyi_symb.replace("perfect ", "-"); }
        intv_strs.push(uds_str + pyi_symb);
      }
      nms.push(edo_str + intv_strs.join(" / "));
    }
  }

  return nms;
}

module.exports.enNames = enNames;

},{"./edo.js":2,"./fjs.js":4,"./interval.js":6,"./pythagorean.js":11,"fraction.js":12,"primes-and-factors":16}],4:[function(require,module,exports){
/**
 * Functions for working with FJS intervals
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module fjs
 **/

const pf = require('primes-and-factors');
const Fraction = require('fraction.js');
const Interval = require('./interval.js');
const py = require('./pythagorean.js');

/**
  * The radius of tolerance of the FJS, the interval `65/63` (about `54.11c`)
  *
  * @constant {Interval}
  */
const fjsRoT = Interval(65,63);

/**
  * The (infinite) fifths sequence of the FJS, `0, 1, -1, 2, -2, 3, -3, ...`
  *
  * @yields {Fraction}
  */
function* fjsFifthsSeq() {
  yield 0;
  for (let g = 1; true; g++) {
    yield Fraction(g);
    yield Fraction(-g);
  }
}

/**
  * The parameters of the FJS, `fjsRoT`, `fjsFifthsSeq`, and
  * `hasNeutrals = false`
  *
  * @constant {{RoT: Fraction, fifthSeq: Fraction, hasNeutrals: boolean}}
  */
const fjsParams = { RoT: fjsRoT, fifthsSeq: fjsFifthsSeq, hasNeutrals: false };

/**
  * The radius of tolerance of the Neutral FJS, a pythagorean
  * semi-diminished second ("sd2", the interval exactly halfway between a
  * pythagorean "d2" and "m2", or about `33.38c`)
  *
  * @constant {Interval}
  */
const nfjsRoT = py.pyInterval(2,-1); // "sd2" ~= 33.38c

/**
  * The (finite) fifths sequence of the Neutral FJS,
  * `0, 1, -1, 2, -2, ..., 6, -6, 1/2, -1/2, 3/2, -3/2, ..., 11/2, -11/2`
  *
  * @yields {Fraction}
  */
function* nfjsFifthsSeq() {
  yield 0;
  for (let g = 1; g <= 6; g++) {
    yield Fraction(g);
    yield Fraction(-g);
  }
  for (let g = 1; g < 6; g++) {
    yield Fraction(2*g-1, 2);
    yield Fraction(1-2*g, 2);
  }
}

/**
  * The parameters of the Neutral FJS, `nfjsRoT`, `nfjsFifthsSeq`, and
  * `hasNeutrals = true`
  *
  * @constant {{RoT: Fraction, fifthSeq: Fraction, hasNeutrals: boolean}}
  */
const nfjsParams = { RoT: nfjsRoT, fifthsSeq: nfjsFifthsSeq, hasNeutrals: true };

/**
  * Returns the FJS fifth shift associated to any interval.
  *
  * @param {Interval} i
  * @param {{RoT: Fraction, fifthSeq: Fraction}} [params=fjsParams]
  * @returns {Fraction}
  */
function fjsFifthShift(a,b, params) {
  // if only two arguments are given, the second one may be `params`!
  if (!params) {
    if (typeof b == 'object' && b != null) {
      params = b;
      b = undefined;
    } else {
      params = fjsParams;
    }
  }
  const intv = Interval(a,b);
  const fifthsSeqGen = params.fifthsSeq();
  for (const g of fifthsSeqGen) {
    let c = intv.div(Interval(3,2).pow(g)).reb();
    if (c.compare(params.RoT) < 0 && params.RoT.recip().compare(c) < 0) {
      return g;
    }
  }
}

/**
  * Returns the FJS comma associated to a prime interval greater than 3
  * (i.e. 5, 7, 11, etc.)
  *
  * @param {integer} p
  * @param {{RoT: Fraction, fifthSeq: Fraction}} [params=fjsParams]
  * @returns {Interval}
  */
function fjsComma(p, params) {
  if (!params) { params = fjsParams; }
  p = parseInt(p);
  if (!pf.isPrime(p) || p <= 3) {
    throw "input is not a prime interval greater than 3";
  }
  const fifthsSeqGen = params.fifthsSeq();
  for (const g of fifthsSeqGen) {
    let c = Interval(p).div(Interval(3,2).pow(g)).reb();
    if (c.compare(params.RoT) < 0 && params.RoT.recip().compare(c) < 0) {
      return c;
    }
  }
}

/**
  * Given an interval, returns the product of the FJS commas associated to each
  * of its prime factors raised to the exponents of those prime factors
  *
  * @param {Interval} k
  * @param {{RoT: Fraction, fifthSeq: Fraction}} [params=fjsParams]
  * @returns {Interval}
  */
function fjsFactor(a,b, params) {
  // if only two arguments are given, the second one may be `params`!
  if (!params) {
    if (typeof b == 'object' && b != null) {
      params = b;
      b = undefined;
    } else {
      params = fjsParams;
    }
  }
  const k = Interval(a,b);
  let ret = Interval(1);
  for (const [p,e] of Object.entries(k)) {
    ret = ret.mul(fjsComma(p,params).pow(e));
  }
  return ret;
}

/**
  * Returns the string of FJS accidentals for the given interval, as well as
  * the pythagorean interval which when applied to these accidentals
  * results in the given interval.
  *
  * @param {Interval} i
  * @param {{RoT: Fraction, fifthSeq: Fraction}} [params=fjsParams]
  * @returns {{ accStr: string, pyi: Interval }}
  */
function fjsAccidentals(a,b, params) {
  // if only two arguments are given, the second one may be `params`!
  if (!params) {
    if (typeof b == 'object' && b != null) {
      params = b;
      b = undefined;
    } else {
      params = fjsParams;
    }
  }
  const i = Interval(a,b);
  let pyi = i;
  let otos = [];
  let utos = [];
  for (let [p,e] of Object.entries(i)) {
    if (p != 2 && p != 3) {
      pyi = pyi.div(fjsComma(p,params).pow(e));
      // add otonal accidentals
      while (e >= 1) {
        otos.push(p);
        e = e.sub(1);
      }
      if (e > 0 && e.d == 2) {
        otos.push("sqrt(" + p + ")");
      }
      if (e > 0 && e.d > 2) {
        otos.push("root" + e.d + "(" + p + ")");
      }
      // add utonal accidentals
      while (e <= -1) {
        utos.push(p);
        e = e.add(1);
      }
      if (e < 0 && e.d == 2) {
        utos.push("sqrt(" + p + ")");
      }
      if (e < 0 && e.d > 2) {
        utos.push("root" + e.d + "(" + p + ")");
      }
    }
  }
  const modulus = params.hasNeutrals ? 2 : 4;
  if (py.isPythagorean(pyi) && py.pyGenerator(pyi) % modulus == 0) {
    const otoStr = otos.length == 0 ? "" : "^" + otos.join(",");
    const utoStr = utos.length == 0 ? "" : "_" + utos.join(",");
    return { accStr: otoStr + utoStr, pyi: pyi };
  }
}

/**
  * Returns the FJS symbol of the given interval, or undefined if no such symbol
  * exists
  *
  * @param {Interval} i
  * @param {{RoT: Fraction, fifthSeq: Fraction}} [params=fjsParams]
  * @returns {string}
  */
function fjsSymb(a,b, params) {
  const res = fjsAccidentals(a,b, params);
  if (res) {
    return py.pySymb(res.pyi) + res.accStr;
  }
}

/**
  * Returns the FJS note name of the given interval to A4, or undefined if no
  * such name exists
  *
  * @param {Interval} i
  * @param {{RoT: Fraction, fifthSeq: Fraction}} [params=fjsParams]
  * @returns {string}
  */
function fjsNote(a,b, params) {
  const res = fjsAccidentals(a,b, params);
  if (res) {
    return py.pyNote(res.pyi) + res.accStr;
  }
}

module['exports'].fjsRoT = fjsRoT;
module['exports'].fjsFifthsSeq = fjsFifthsSeq;
module['exports'].fjsParams = fjsParams;
module['exports'].nfjsRoT = nfjsRoT;
module['exports'].nfjsFifthsSeq = nfjsFifthsSeq;
module['exports'].nfjsParams = nfjsParams;
module['exports'].fjsFifthShift = fjsFifthShift;
module['exports'].fjsComma = fjsComma;
module['exports'].fjsFactor = fjsFactor;
module['exports'].fjsAccidentals = fjsAccidentals;
module['exports'].fjsSymb = fjsSymb;
module['exports'].fjsNote = fjsNote;

},{"./interval.js":6,"./pythagorean.js":11,"fraction.js":12,"primes-and-factors":16}],5:[function(require,module,exports){
// export everything from `lib/` as well as `Fraction` from fraction.js
module['exports']['Fraction'] = require('fraction.js');
module['exports']['Interval'] = require('./interval.js');
Object.assign(module['exports'], require('./pythagorean.js'));
Object.assign(module['exports'], require('./fjs.js'));
Object.assign(module['exports'], require('./edo.js'));
Object.assign(module['exports'], require('./approx.js'));
Object.assign(module['exports'], require('./english.js'));
Object.assign(module['exports'], require('./parser.js'));

},{"./approx.js":1,"./edo.js":2,"./english.js":3,"./fjs.js":4,"./interval.js":6,"./parser.js":7,"./pythagorean.js":11,"fraction.js":12}],6:[function(require,module,exports){
/**
 * The interval datatype, based on `Fraction` from `fraction.js` on npm
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module interval
 **/

const pf = require('primes-and-factors');
const Fraction = require('fraction.js');

const keys = function(a, b) {
  let ret = {};
  for (const [k,v] of Object.entries(a)) {
    ret[k] = 1;
  }
  if (b) {
    for (const [k,v] of Object.entries(b)) {
      ret[k] = 1;
    }
  }
  return ret;
}

const parse = function(a, b) {
  if (a === undefined || a === null) {
    return {};
  }
  else if (b !== undefined) {
    if (a == 0 || a < 0 || b == 0 || b < 0) {
      throw "non-positive number cannot be converted into an interval"
    }
    const afs = pf.getPrimeExponentObject(a);
    const bfs = pf.getPrimeExponentObject(b);
    let ret = keys(afs,bfs);
    for (const i in ret) {
      ret[i] = Fraction((afs[i] || 0) - (bfs[i] || 0));
    }
    return ret;
  }
  else if (typeof a == "object") {
    if ("d" in a && "n" in a) {
      let sn = a["n"];
      if ("s" in a) {
        sn *= a["s"];
      }
      return parse(sn, a["d"]);
    }
    else {
      let allPrimes = true
      let ret = {};
      for (const i in keys(a)) {
        allPrimes &= pf.isPrime(Number(i));
        if (Fraction(a[i]) != 0) {
          ret[i] = Fraction(a[i]);
        }
      }
      if (allPrimes) {
        return ret;
      } else {
        throw "argument is not a number, fraction, or interval"
      }
    }
  }
  else {
    return parse(Fraction(a));
  }
}

/**
  * Constructs an `Interval`. Valid argument combinations are two integers
  * (e.g. `Interval(3,2)`), a single number (e.g. `Interval(3/2)`), an array
  * containing 0-2 integers (e.g. `Interval([3,2])`), a factorization
  * (e.g. `Interval({2: -1, 3: 1})`), a `Fraction`, or an `Interval`.
  *
  * As a convention, all functions which have a JSDoc parameter of type
  * `Interval` should be able to accept any of these argument combinations in
  * place of that parameter. For example, `mul` in this file, or
  * `bestRationalApproxs` in `approx.js`.
  *
  * If both arguments are omitted, the result is `Interval(1)`.
  *
  * @constructor
  * @param {(number|Fraction|Object)=} a
  * @param {integer=} b
  */
function Interval(a,b) {

  if (!(this instanceof Interval)) {
    return new Interval(a,b);
  }

  const p = parse(a,b);
  for (const i in keys(p)) {
    this[i] = p[i]
  }

}

Interval.prototype = {

  /**
   * Multiplies (i.e. composes) two intervals.
   *
   * e.g. `Interval(3,2).mul(5,4)` is the composition of `3/2` and `5/4`, the
   * interval `15/8`, or `Interval(15,8)`
   *
   * @param {Interval} i
   * @returns {Interval}
   */
  "mul": function(a,b) {
    const rhs = parse(a,b);
    let ret = keys(this,rhs);
    for (const i in ret) {
      ret[i] = (this[i] || Fraction(0)).add(rhs[i] || Fraction(0));
    }
    return new Interval(ret);
  },

  /**
   * Divides two intervals.
   *
   * e.g. `Interval(2).div(3,2)` is exactly `Interval(4,3)`
   *
   * @param {Interval} i
   * @returns {Interval}
   */
  "div": function(a,b) {
    const rhs = parse(a,b);
    let ret = keys(this,rhs);
    for (const i in ret) {
      ret[i] = (this[i] || Fraction(0)).sub(rhs[i] || Fraction(0));
    }
    return new Interval(ret);
  },

  /**
   * Takes the reciprocal/inverse of an interval.
   *
   * e.g. `Interval(3,2).recip()` is exactly `Interval(2,3)`
   *
   * @returns {Interval}
   */
  "recip": function() {
    let ret = keys(this);
    for (const i in ret) {
      ret[i] = this[i].neg();
    }
    return new Interval(ret);
  },

  /**
   * Raises an interval to a fractional power.
   *
   * e.g. `Interval(4,3).pow(2)` is the composition of `4/3` with itself, the
   * interval `16/9`
   *
   * e.g. `Interval(2).pow(4,12)` is the interval `4\12`, four steps of 12-EDO
   *
   * e.g. `Interval(5).pow(1,4)` is the fourth root of `5`, the fifth in
   * quarter-comma meantone
   *
   * @param {Fraction} k
   * @returns {Interval}
   */
  "pow": function(a,b) {
    let ret = keys(this);
    for (const i in ret) {
      ret[i] = this[i].mul(Fraction(a,b));
    }
    return new Interval(ret);
  },

  /**
   * The nth root of an interval, i.e. `pow(1,n)`.
   *
   * @param {integer} n
   * @returns {Interval}
   */
  "root": function(a) {
    return this.pow(Fraction(1,a));
  },

  /**
   * The square root of an interval, i.e. `pow(1,2)`.
   *
   * @returns {Interval}
   */
  "sqrt": function() {
    return this.pow(1/2);
  },

  /**
   * Checks whether an interval has integer prime exponents, i.e. whether the
   * interval can be expressed as a fraction
   *
   * e.g. `Interval(3,2).isFrac()` returns `true`
   *
   * e.g. `Interval(2).sqrt().isFrac()` returns `false`
   *
   * @returns {bool}
   */
  "isFrac": function() {
    for (const i in keys(this)) {
      if (this[i].d != 1) {
        return false;
      }
    }
    return true;
  },

  /**
   * Converts an interval with integer prime exponents, i.e. an interval which
   * can be expressed as a fraction, to a `Fraction`.
   *
   * e.g. `Interval(3,2).toFrac()` is exactly `Fraction(3,2)`
   *
   * e.g. `Interval(2).sqrt().toFrac()` throws an error
   *
   * @returns {Fraction}
   */
  "toFrac": function() {
    let ret = Fraction(1);
    for (const i in keys(this)) {
      if (this[i].d == 1) {
        ret = ret.mul(Fraction(i).pow(this[i].s * this[i].n));
      } else {
        throw "interval does not have integer exponents";
      }
    }
    return ret;
  },

  /**
   * Takes the mediant of two intervals with integer prime exponents, i.e. two
   * intervals which can be expressed as fractions.
   *
   * e.g. `Interval(5,4).med(9,7)` is the mediant of the intervals `5/4` and
   * `9/7`, the interval `14/11`
   *
   * @returns {Interval}
   */
  "med": function(a,b) {
    let [f1, f2] = [this.toFrac(), Interval(a,b).toFrac()];
    return new Interval(f1.n + f2.n, f1.d + f2.d);
  },

  /**
   * Converts any interval to its representation as the nth root of a fraction.
   *
   * e.g. `Interval(3,2).toNthRoot()` is exactly `{k: Interval(3,2), n: 1}`
   *
   * e.g. `Interval({2: 1/2, 5: 1/3})` is exactly `{k: Interval({2: 3, 5: 2}), n: 6}`
   *
   * @returns {{k: Fraction, n: Integer}}
   */
  "toNthRoot": function() {
    let n_fr = Fraction(1);
    for (const i in keys(this)) {
      n_fr = n_fr.gcd(this[i]);
    }
    return { k: this.pow(n_fr.inverse()).toFrac(), n: n_fr.d };
  },

  /**
   * Converts an interval to its decimal value, for automatic use by javascript.
   * Note that this function should not be used to compare relative sizes of
   * intervals, use `valueOf_log` or `toCents` instead.
   *
   * e.g. `Interval(3,2).valueOf()` is exactly `1.5`
   *
   * @returns {number}
   */
  "valueOf": function() {
    let ret = 1;
    for (const i in keys(this)) {
      ret *= Math.pow(i,this[i].valueOf())
    }
    return ret;
  },

  /**
   * Performs an exact comparison of two intervals. Specifically, returns 0 if
   * the intervals are equal, 1 if the first interval is greater than the
   * second, and -1 if the second interval is greater than the first.
   *
   * In general, `i1 ineq i2`, where `i1`, `i2` are intervals and `ineq` is an
   * inequality (e.g. `>=`), can be incorrect since javascript uses `valueOf`
   * to convert both sides to finite precision floating point numbers before
   * doing the comparision. To perform an exact version of the same check,
   * use `i1.compare(i2) ineq 0`.
   *
   * e.g. `Interval(2).sqrt().compare(3,2) < 0` is true since `sqrt(2) < 3/2`
   *
   * @param {Interval} i
   * @returns {integer}
   */
  "compare": function(a,b) {
    const diff = this.div(a,b);
    const {k,n} = diff.toNthRoot();
    if (isFinite(k.n) && isFinite(k.d)) {
      // do an exact comparison if k is finite
      return k.compare(Fraction(1));
    }
    else {
      // otherwise the numbers are just too big - for now we approximate
      return (1 < diff.valueOf()) - (diff.valueOf() < 1);
    }
  },

  /**
   * Checks if the two intervals are the same. In general, `i1 == i2`, where
   * `i1`, `i2` are intervals, can give false positives since javascript uses
   * `valueOf` to convert both sides to finite precision floating point numbers
   * before doing the comparision.
   *
   * e.g. `Interval(4,3).pow(2).equals(16,9)` is true
   *
   * @param {Interval} i
   * @returns {boolean}
   */
  "equals": function(a,b) {
    return this.compare(a,b) == 0;
  },

  /**
   * If the given argument is a prime, returns a pair whose first element is the
   * exponent of that prime in this interval, and whose second element is the
   * interval without that prime (i.e. the rest of the factorization).
   *
   * e.g. `Interval(8*5,7).factorOut(2)` is exactly `[3, Interval(5,7)]`.
   *
   * More generally, if the given argument is an interval `i` with factorization
   * `p1^e1 ... pm^em` (where the `pk`s are prime and in ascending order, and
   * each `ek > 0`), returns a pair `[g, this.div(i.pow(g))]` where `g` is the
   * smallest fraction such that `this.div(i.pow(g))` contains no factors of
   * `pm` (the largest prime in the factorization of `i`).
   *
   * e.g. `Interval(9,8).factorOut(3,2)` is exactly `[2, Interval(1,2)]`.
   *
   * @param {Interval} i
   * @returns {Pair.<Fraction,Interval>}
   */
  "factorOut": function(a,b) {
    const base = new Interval(a,b);
    const gp = Math.max(...Object.keys(base));
    if (isFinite(gp)) {
      const g = (this[gp] || Fraction(0)).div(base[gp]);
      let res = keys(this, base);
      for (const i in res) {
        res[i] = (this[i] || Fraction(0)).sub((base[i] || Fraction(0)).mul(g));
      }
      return [g, new Interval(res)];
    }
    else {
      return [Fraction(0), this];
    }
  },

  /**
   * Converts an interval to its decimal value log the given base. If no
   * argument is given, the base is taken to be 2 (an octave).
   *
   * e.g. `Interval(3,2).valueOf_log()` gives `0.5849625007211561`
   *
   * Note that this function uses `factorOut` to preserve as much precision as
   * possible - for example, for any interval `i` and fraction `k`, then
   * `i.pow(k).valueOf_log(i) == k` *exactly*.
   *
   * e.g. `Interval(3,2).pow(1,2).valueOf_log(3,2)` gives `0.5`
   *
   * @param {Interval} [i=Interval(2)]
   * @returns {number}
   */
  "valueOf_log": function(a,b) {
    let base = new Interval(2);
    if (a != undefined || b != undefined) {
      base = new Interval(a,b);
    }
    const [g, res] = this.factorOut(base);
    return g + Math.log(res.valueOf()) / Math.log(base.valueOf());
  },

  /**
   * Reduces an interval w.r.t. another interval. If no argument is given, it
   * is taken to be 2 (an octave).
   *
   * e.g. `Interval(3,2).pow(2).red()` is exactly `Interval(9,8)`
   *
   * For all intervals `i`, `j` this function satisfies the equality:
   * `i.div(i.red(j)).equals(j.pow(Math.floor(i.valueOf_log(j))))`
   *
   * @param {Interval} [i=Interval(2)]
   * @returns {Interval}
   */
  "red": function(a,b) {
    let base = new Interval(2);
    if (a != undefined || b != undefined) {
      base = new Interval(a,b);
    }
    const e = Math.floor(this.valueOf_log(base));
    return this.div(base.pow(e));
  },

  /**
   * Balanced reduces an interval w.r.t. another interval. If no argument is
   * given, the it is taken to be 2 (an octave).
   *
   * e.g. `Interval(3,2).reb()` is exactly `Interval(2,3)`
   *
   * For all intervals `i`, `j` this function satisfies the equality:
   * `i.div(i.reb(j)).equals(j.pow(Math.round(i.valueOf_log(j))))`
   *
   * @param {Interval} [i=Interval(2)]
   * @returns {Interval}
   */
  "reb": function(a,b) {
    let base = new Interval(2);
    if (a != undefined || b != undefined) {
      base = new Interval(a,b);
    }
    const e = Math.round(this.valueOf_log(base));
    return this.div(base.pow(e));
  },

  /**
   * Converts an interval to its value in cents.
   *
   * e.g. `Interval(3,2).toCents()` gives `701.9550008653873`
   *
   * Note that this function uses `factorOut` to preserve as much precision as
   * possible - for example, for any fraction `k`,
   * `Interval(2).pow(k).toCents() == k.mul(1200)` *exactly*.
   *
   * e.g. `Interval(2).pow(4,12).toCents()` is exactly `400`
   *
   * @returns {number}
   */
  "toCents": function() {
    const [e2, res] = this.factorOut(2);
    return e2.mul(1200) + Math.log(res.valueOf()) / Math.log(2) * 1200;
  },

  /**
   * Converts an interval to its Tenney harmonic distance, or Tenney height.
   *
   * e.g. `Interval(3,2).tenneyHD()` gives `2.584962500721156`
   *
   * @returns {number}
   */
  "tenneyHD": function() {
    let ret = Interval(1);
    for (const i in keys(this)) {
      ret[i] = this[i].abs();
    }
    return ret.valueOf_log();
  },

  /**
   * Converts an interval to a monzo.
   *
   * e.g. `Interval(11,9).toMonzo()` gives `[0,-2,0,0,1]`
   *
   * @returns {number}
   */
  "toMonzo": function() {
    let max_p = 0;
    for (const i in keys(this)) {
      max_p = Math.max(max_p, i);
    }
    let [ret, isFrac] = [[], true];
    if (2 <= max_p) {
      ret[0] = this[2] || Fraction(0);
      isFrac &= !this[2] || this[2].d == 1;
    }
    let i = 1;
    for (let p = 3; p <= max_p; p += 2) {
      if (pf.isPrime(p)) {
        ret[i] = this[p] || Fraction(0);
        isFrac &= !this[p] || this[p].d == 1;
        i++;
      }
    }
    if (isFrac) {
      ret = ret.map(r => r.s * r.n);
    }
    return ret;
  }

}

module.exports = Interval;

},{"fraction.js":12,"primes-and-factors":16}],7:[function(require,module,exports){
/**
 * Interface for parsing interval/note expressions
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module parser
 **/

const ne = require('nearley');
const Fraction = require('fraction.js');
const Interval = require('./interval.js');
const grammar = require('./parser/grammar.js');
const {evalExpr} = require('./parser/eval.js');
const {isPythagorean, pySymb, pyNote} = require('./pythagorean.js');
const {fjsSymb, fjsNote, nfjsParams} = require('./fjs.js');
const {edoApprox, edoPy, updnsSymb, updnsNote} = require('./edo.js');
const {enNames} = require('./english.js');

function mod(a,n) {
  return ((a % n) + n) % n;
}

function expectedSymbols(parser) {
  let symbs = [];
  const lastColumnIndex = parser.table.length - 2;
  const lastColumn = parser.table[lastColumnIndex];
  const expectantStates = lastColumn.states
      .filter(function(state) {
          var nextSymbol = state.rule.symbols[state.dot];
          return nextSymbol && typeof nextSymbol !== "string";
      });

  const stateStacks = expectantStates
      .map(function(state) {
          return parser.buildFirstStateStack(state, []) || [state];
      }, parser);
  // Display each state that is expecting a terminal symbol next.
  stateStacks.forEach(function(stateStack) {
      var state = stateStack[0];
      var nextSymbol = state.rule.symbols[state.dot];
      var symbolDisplay = parser.getSymbolDisplay(nextSymbol);
      symbs.push(symbolDisplay);
  }, parser);

  // remove duplicates
  symbs = [...new Set(symbs)];
  symbs.sort((a,b) => a.length - b.length);
  if (symbs.length > 1) {
    symbs[symbs.length-1] = "or " + symbs[symbs.length-1];
  }
  return "expected a " + symbs.join(", ");
}

/**
 * @typedef {Object} RawParseResult
 * @property {string} type either "interval" or "note"
 * @property {Interval} intv the resulting interval (to the reference, if
 *                           type is "note")
 * @property {{hertz: Interval, intvToA4: Interval}} refNote the reference note
 * @property {integer=} prefEDO the preferred EDO, if any, of the interval
 */

/**
  * Parses the given string
  *
  * @param {string} str
  * @returns {RawParseResult}
  */
function parse(str) {

  const parser = new ne.Parser(ne.Grammar.fromCompiled(grammar));
  try { parser.feed(str); }
  catch (err) {
    throw new Error ("Parse error at col " + err.offset + ", " + expectedSymbols(parser));
  }
  let results = parser.results;

  for (let i = 0; i < results.length; i++) {
    const res = evalExpr(results[i].expr, results[i].refNote);
    results[i].val = res.val;
    results[i].prefEDO = res.prefEDO;
  }

  if (results.length == 0) {
    try { parser.feed("$"); }
    catch (err) {
      throw new Error ("Parse error at col " + err.offset + ", " + expectedSymbols(parser));
    }
  }
  if (results.some(d => d.type[0] == "interval" && d.type[1] == "symbol")) {
    results = results.filter(d => !(d.type[0] == "interval" && d.type[1] != "symbol"));
  }
  if (results.some(d => d.type[0] == "note" && d.type[1] == "symbol")) {
    results = results.filter(d => !(d.type[0] == "note" && d.type[1] != "symbol"));
  }
  if (results.length > 1) {
    console.log("Parse was ambiguous! Full results:");
    console.dir(parser.results, { depth: null });
  }
  let ret = { type: results[0].type[0]
            , intv: results[0].val
            , refNote: results[0].refNote
            , prefEDO: results[0].prefEDO };

  // If `intv` is an EDO step (i.e. a fractional power of two),
  if (Object.entries(ret.intv).length == (ret.intv['2'] != null)) {
    let e2 = ret.intv['2'] || Fraction(0);
    // forget `ret.prefEDO` if `ret.intv` is not `2^(k/prefEDO)` (sanity check)
    if (ret.prefEDO && e2.mul(ret.prefEDO).d != 1) {
      delete ret.prefEDO;
    }
    // set `ret.prefEDO` if `ret.intv` is a simple enough power of two
    if (!ret.prefEDO && (e2.d == 2 || e2.d == 3 || e2.d == 4)) {
      ret.prefEDO = 12;
    }
    if (!ret.prefEDO && 4 < e2.d && e2.d <= 60) {
      ret.prefEDO = e2.d;
    }
  }
  // Otherwise, forget `ret.prefEDO` (sanity check)
  else {
    delete ret.prefEDO;
  }

  return ret;
}

/**
 * @typedef {Object} IntvParseResult
 * @property {string} type always "interval"
 * @property {number} cents the resulting interval converted to cents
 * @property {Interval} intv the resulting interval object
 * @property {Fraction=} ratio the resulting interval as a JI ratio
 * @property {number=} tenneyHD the Tenney harmonic distance of the resulting
 *                              interval as a JI ratio
 * @property {Pair.<integer,integer>=} edoSteps the resulting interval as some
 *                                              number of EDO steps
 * @property {Object.<string,string>} symb various symbols for the resulting
 *                                         interval, including FJS,
 *                                         Neutral FJS, and ups-and-downs
 *                                         notations
 * @property {Array.<string>} english (experimental) english name for the
 *                                    resulting interval, based on
 *                                    Neutral FJS and ups-and-downs notations
 */

/**
 * @typedef {Object} NoteParseResult
 * @property {string} type always "note"
 * @property {number} freq the resulting interval converted to hertz
 * @property {Interval} intvToRef the resulting interval to the reference
 * @property {Pair.<integer,integer>=} edoStepsToRef the resulting interval as
 *                                                   some number of EDO steps
 *                                                   to the reference
 * @property {ReferenceNote} ref the reference note
 * @property {Object.<string,string>} symb various symbols for the resulting
 *                                         interval, including FJS and
 *                                         ups-and-downs notations
 */

/**
 * @typedef {Object} ReferenceNote
 * @property {Interval} hertz
 * @property {Interval} intvToA4
 * @property {Pair.<integer,integer>=} edoStepsToA4
 */

/**
  * Parses the given string and converts it to a few other convenient forms
  *
  * @param {string} str
  * @returns {IntvParseResult|NoteParseResult}
  */
function parseCvt(str) {
  let {type, intv, refNote, prefEDO} = parse(str);
  let ret = { type: type };
  if (type == "interval") {
    ret.cents = intv.toCents();
    ret.intv = intv;
    try {
      ret.ratio = intv.toFrac();
      ret.tenneyHD = intv.tenneyHD();
    } catch (_) {}
    if (prefEDO) {
      let e2 = (intv['2'] || Fraction(0)).mul(prefEDO);
      ret.edoSteps = [e2.s*e2.n, prefEDO];
    }
    ret.symb = {};
    let fjs = fjsSymb(intv);
    let nfjs = fjsSymb(intv, nfjsParams);
    if (fjs) {
      ret.symb['FJS'] = fjs;
    }
    if (nfjs && nfjs != fjs) {
      ret.symb['Neutral FJS'] = nfjs;
    }
    if (prefEDO) {
      let e2 = (intv['2'] || Fraction(0)).mul(prefEDO);
      ret.symb['ups-and-downs'] = updnsSymb(prefEDO,e2.s*e2.n).map(s => s + "\\" + prefEDO);
    }
    if (!nfjs && isPythagorean(intv)) {
      ret.symb['other'] = pySymb(intv);
    }
    if (intv.equals(Interval(2).sqrt())) {
      ret.symb['other'] = "TT";
    }
    const nms = enNames(intv, {prefEDO: prefEDO});
    if (nms.length > 0) {
      ret.english = nms;
    }
  }
  if (type == "note") {
    ret.hertz = refNote.hertz.mul(intv).valueOf();
    const intvToA4 = intv.mul(refNote.intvToA4);
    const closest12EDO = edoApprox(12, intvToA4);
    const diffTo12EDO = intvToA4.div(Interval(2).pow(closest12EDO,12)).toCents();
    ret.tuningMeter = updnsNote(12, mod(closest12EDO+9,12)-9).join("/") + " "
                      + (diffTo12EDO == 0 ? "±" : diffTo12EDO > 0 ? "+" : "-")
                      + Math.abs(diffTo12EDO).toFixed(1) + "c";
    ret.intvToRef = intv;
    if (prefEDO) {
      let e2 = (intv['2'] || Fraction(0)).mul(prefEDO);
      ret.edoStepsToRef = [e2.s*e2.n, prefEDO];
    }
    ret.ref = { hertz: refNote.hertz.valueOf()
              , intvToA4: refNote.intvToA4 };
    ret.symb = {};

    let fjs = fjsNote(intvToA4);
    if (fjs) {
      ret.symb['FJS'] = fjs;
    }
    if (prefEDO) {
      const refEDOStepsToA4 = edoPy(prefEDO, refNote.intvToA4);
      ret.ref.edoStepsToA4 = [refEDOStepsToA4, prefEDO];
      let e2 = (intv['2'] || Fraction(0)).mul(prefEDO).add(refEDOStepsToA4);
      ret.symb['ups-and-downs'] = updnsNote(prefEDO,e2.s*e2.n).map(s => s + "\\" + prefEDO);
    }
  }
  return ret;
}

module['exports'].parse = parse;
module['exports'].parseCvt = parseCvt;

},{"./edo.js":2,"./english.js":3,"./fjs.js":4,"./interval.js":6,"./parser/eval.js":8,"./parser/grammar.js":10,"./pythagorean.js":11,"fraction.js":12,"nearley":14}],8:[function(require,module,exports){
/**
 * A function for evaluating the results of running `grammar.ne`
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module eval
 **/

const Fraction = require('fraction.js');
const Interval = require('../interval.js');
const {pyInterval, isPerfectDeg, baseNoteIntvToA} = require('../pythagorean.js');
const {fjsFactor} = require('../fjs.js');
const {edoApprox, edoPy, edoHasNeutrals, edoHasSemiNeutrals} = require('../edo.js');

function cbnEDOs(a,b) {
  return a && b ? Fraction(1,a).gcd(1,b).d : undefined
}

/**
  * Evaluates the result of running `grammar.ne`
  *
  * @param {Array} e the expression to evaluate
  * @param {{hertz: Interval, intvToA4: Interval}} refNote the reference note
  * @returns {{val: Interval, prefEDO: integer}}
  */
function evalExpr(e, r, edo) {
  if (Array.isArray(e)) {
    // don't fail in the case of a nested array
    if (Array.isArray(e[0])) {
      console.log("evalExpr: nested arrays")
      return evalExpr(e[0], r, edo);
    }

    // 1 | Special cases:
    if (e[0] == "!refIntvToA4") {
      return { val: r.intvToA4 };
    }
    if (e[0] == "!refHertz") {
      return { val: r.hertz };
    }
    if (e[0] == "!med") { // `edo` should be undefined
      const arg0 = evalExpr(e[1],r).val;
      const arg1 = evalExpr(e[2],r).val;
      if (arg0.isFrac() && arg1.isFrac()) {
        return { val: arg0.med(arg1) };
      }
      else {
        throw "One of the arguments to `med` is not a fraction"
      }
    }
    if (e[0] == "!cents") { // `edo` should be undefined
      const arg0 = Fraction(evalExpr(e[1],r).val).div(1200);
      return { val: Interval(2).pow(arg0)
             , prefEDO: 48 % arg0.d == 0 ? 24 % arg0.d == 0 ? 12 % arg0.d == 0 ? 12 : 24 : 48 : undefined };
    }
    if (e[0] == "!edoApprox") { // `edo` should not be defined
      const arg0 = evalExpr(e[1],r).val;
      const arg1 = evalExpr(e[2],r).val;
      return { val: Interval(2).pow(edoApprox(arg1, arg0)).pow(1,arg1), prefEDO: arg1 };
    }
    if (e[0] == "!inEDO") { // `edo` should be undefined
      const arg1 = evalExpr(e[2],r).val;
      const arg0 = evalExpr(e[1],r,arg1).val;
      return { val: Interval(2).pow(arg0).pow(1,arg1), prefEDO: arg1 };
    }
    if (e[0] == "!edoTT") { // `edo` should be defined
      if (edo % 2 == 0) {
        return { val: edo / 2 };
      }
      else {
        throw edo + "-EDO does not have a tritone";
      }
    }
    if (e[0] == "!edoPy") { // `edo` should be defined
      const arg0 = evalExpr(e[1],r,edo).val;
      return { val: edoPy(edo, arg0) };
    }

    // for the remaining cases, we evaluate every argument
    const args = e.slice(1).map(ei => evalExpr(ei,r,edo));

    // 2 | Operators:
    if (e[0] == "+") { return { val: args[0].val + args[1].val }; }
    if (e[0] == "-") { return { val: args[0].val - args[1].val }; }
    if (e[0] == "*") { return { val: args[0].val * args[1].val }; }
    if (e[0] == "/") { return { val: args[0].val / args[1].val }; }

    // 3 | Otherwise, we assume `e[0]` is a method of `args[0]`
    let ret = { val: args[0].val[e[0]](...args.slice(1).map(argi => argi.val))
              , prefEDO: args.map(argi => argi.prefEDO).reduce(cbnEDOs) };
    // for some operations on intervals we don't want to look at every
    //  argument's perferred EDO
    if (e[0] == "pow") {
      ret.prefEDO = args[0].prefEDO;
    }
    if (e[0] == "red" || e[0] == "reb") {
      if (args[1] && args[1].equals && args[1].equals(2)) {
        ret.prefEDO = args[0].prefEDO;
      }
      else {
        ret.prefEDO = undefined;
      }
    }
    return ret;
  }
  return { val: e, prefEDO: e == 2 ? 1 : undefined };
}

module['exports'].evalExpr = evalExpr;

},{"../edo.js":2,"../fjs.js":4,"../interval.js":6,"../pythagorean.js":11,"fraction.js":12}],9:[function(require,module,exports){

const Fraction = require('fraction.js');
const Interval = require('../interval.js');
const {pyInterval, isPerfectDeg, baseNoteIntvToA} = require('../pythagorean.js');
const {fjsFactor} = require('../fjs.js');
const {edoPy, edoHasNeutrals, edoHasSemiNeutrals} = require('../edo.js');

const defaultRefNote = { intvToA4: Interval(1), hertz: Interval(440) };

function perfPyInterval(d,o,reject) {
  return isPerfectDeg(d) ? pyInterval(d,o) : reject;
}
function nonPerfPyInterval(d,o,reject) {
  return isPerfectDeg(d) ? reject : pyInterval(d,o);
}
function augOrDimPyInterval(d,a,b,reject) {
  const o = Fraction(a,b);
  if (o.d != b) {
    return reject;
  }
  const o_np = o.add(o.s,2);
  return isPerfectDeg(d) ? pyInterval(d,o) : pyInterval(d,o_np);
}

function ensureNo2Or3(i,reject) {
  return (i['2'] && i['2'] != 0) || (i['3'] && i['3'] != 0) ? reject : i;
}

function cbnEDOs(a,b) {
  if (a && b) { return Fraction(1,a).gcd(1,b).d; }
  else { return null; }
}

function baseNoteIntvToReference(x,referenceNoteIntvToA4) {
  return baseNoteIntvToA(x).div(referenceNoteIntvToA4);
}

module['exports'].defaultRefNote = defaultRefNote;
module['exports'].perfPyInterval = perfPyInterval;
module['exports'].nonPerfPyInterval = nonPerfPyInterval;
module['exports'].augOrDimPyInterval = augOrDimPyInterval;
module['exports'].ensureNo2Or3 = ensureNo2Or3;
module['exports'].cbnEDOs = cbnEDOs;
module['exports'].baseNoteIntvToReference = baseNoteIntvToReference;

},{"../edo.js":2,"../fjs.js":4,"../interval.js":6,"../pythagorean.js":11,"fraction.js":12}],10:[function(require,module,exports){
// Generated automatically by nearley, version 2.19.8
// http://github.com/Hardmath123/nearley
(function () {
function id(x) { return x[0]; }


const Fraction = require('fraction.js');
const Interval = require('../interval.js');
const {pyInterval, pyRedDeg, baseNoteIntvToA} = require('../pythagorean.js');
const {fjsFactor, fjsParams, nfjsParams} = require('../fjs.js');
const {edoPy} = require('../edo.js');
const helpers = require('./grammar-helpers.js');
const {evalExpr} = require('./eval.js');

var grammar = {
    Lexer: undefined,
    ParserRules: [
    {"name": "_$ebnf$1", "symbols": []},
    {"name": "_$ebnf$1", "symbols": ["_$ebnf$1", "wschar"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "_", "symbols": ["_$ebnf$1"], "postprocess": function(d) {return null;}},
    {"name": "__$ebnf$1", "symbols": ["wschar"]},
    {"name": "__$ebnf$1", "symbols": ["__$ebnf$1", "wschar"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "__", "symbols": ["__$ebnf$1"], "postprocess": function(d) {return null;}},
    {"name": "wschar", "symbols": [/[ \t\n\v\f]/], "postprocess": id},
    {"name": "top1", "symbols": ["_", "top2", "_"], "postprocess":  function (d,_,reject) { let d1 = Object.assign({},d[1]); // copy this!
        d1.refNote = helpers.defaultRefNote;
        return d1; } },
    {"name": "top1$string$1", "symbols": [{"literal":"w"}, {"literal":"h"}, {"literal":"e"}, {"literal":"r"}, {"literal":"e"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "top1$ebnf$1", "symbols": ["hertz"], "postprocess": id},
    {"name": "top1$ebnf$1", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "top1", "symbols": ["_", "top2", "__", "top1$string$1", "__", "pyNote", "_", {"literal":"="}, "_", "decimal", "top1$ebnf$1", "_"], "postprocess":  function (d,_,reject) { let d1 = Object.assign({},d[1]); // copy this!
        d1.refNote = {};
        d1.refNote.intvToA4 = evalExpr(d[5], helpers.defaultRefNote).val;
        d1.refNote.hertz    = Interval(d[9]);
        return d1; } },
    {"name": "top1$string$2", "symbols": [{"literal":"w"}, {"literal":"h"}, {"literal":"e"}, {"literal":"r"}, {"literal":"e"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "top1", "symbols": ["_", "top2", "__", "top1$string$2", "__", "pyNote", "_", {"literal":"="}, "_", "pyNote", "_", {"literal":"\\"}, "_", "posInt", "_"], "postprocess":  function (d,_,reject) { let d1 = Object.assign({},d[1]); // copy this!
        const d5 = evalExpr(d[5], helpers.defaultRefNote).val;
        const d9 = evalExpr(d[9], helpers.defaultRefNote).val;
        const d13 = parseInt(d[13]);
        if (!d5 || !d5.equals(d9)) { return reject; }
        d1.refNote = {};
        d1.refNote.intvToA4 = d9;
        d1.refNote.hertz    = Interval(2).pow(edoPy(d13,d9),d13).mul(440);
        return d1; } },
    {"name": "top2", "symbols": ["intvSExpr1"], "postprocess": d => ({type: ["interval", "symbol"], expr: d[0]})},
    {"name": "top2", "symbols": ["intvMExpr1"], "postprocess": d => ({type: ["interval", "multiplicative"], expr: d[0]})},
    {"name": "top2", "symbols": ["intvAExpr1"], "postprocess": d => ({type: ["interval", "additive"], expr: d[0]})},
    {"name": "top2", "symbols": ["noteSExpr1"], "postprocess": d => ({type: ["note", "symbol"], expr: d[0]})},
    {"name": "top2", "symbols": ["noteMExpr1"], "postprocess": d => ({type: ["note", "multiplicative"], expr: d[0]})},
    {"name": "top2", "symbols": ["noteAExpr1"], "postprocess": d => ({type: ["note", "additive"], expr: d[0]})},
    {"name": "intvMExpr1", "symbols": ["intvMExpr1", "_", {"literal":"*"}, "_", "intvMExpr2"], "postprocess": d => ["mul", d[0], d[4]]},
    {"name": "intvMExpr1", "symbols": ["intvMExpr1", "_", {"literal":"/"}, "_", "intvMExpr2"], "postprocess": d => ["div", d[0], d[4]]},
    {"name": "intvMExpr1", "symbols": ["noteMExpr1", "_", {"literal":"/"}, "_", "noteMExpr2"], "postprocess": d => ["div", d[0], d[4]]},
    {"name": "intvMExpr1", "symbols": ["intvMExpr2"], "postprocess": id},
    {"name": "intvMExpr2", "symbols": ["intvMExpr3", "_", {"literal":"^"}, "_", "frcExpr3"], "postprocess": d => ["pow", d[0], d[4]]},
    {"name": "intvMExpr2$string$1", "symbols": [{"literal":"s"}, {"literal":"q"}, {"literal":"r"}, {"literal":"t"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intvMExpr2", "symbols": ["intvMExpr2$string$1", "_", {"literal":"("}, "_", "intvMExpr1", "_", {"literal":")"}], "postprocess": d => ["sqrt", d[4]]},
    {"name": "intvMExpr2$string$2", "symbols": [{"literal":"r"}, {"literal":"e"}, {"literal":"d"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intvMExpr2", "symbols": ["intvMExpr2$string$2", "_", {"literal":"("}, "_", "intvMExpr1", "_", {"literal":")"}], "postprocess": d => ["red", d[4]]},
    {"name": "intvMExpr2$string$3", "symbols": [{"literal":"r"}, {"literal":"e"}, {"literal":"b"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intvMExpr2", "symbols": ["intvMExpr2$string$3", "_", {"literal":"("}, "_", "intvMExpr1", "_", {"literal":")"}], "postprocess": d => ["reb", d[4]]},
    {"name": "intvMExpr2$string$4", "symbols": [{"literal":"r"}, {"literal":"e"}, {"literal":"d"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intvMExpr2", "symbols": ["intvMExpr2$string$4", "_", {"literal":"("}, "_", "intvMExpr1", "_", {"literal":","}, "_", "intvMExpr1", "_", {"literal":")"}], "postprocess": d => ["red", d[4], d[8]]},
    {"name": "intvMExpr2$string$5", "symbols": [{"literal":"r"}, {"literal":"e"}, {"literal":"b"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intvMExpr2", "symbols": ["intvMExpr2$string$5", "_", {"literal":"("}, "_", "intvMExpr1", "_", {"literal":","}, "_", "intvMExpr1", "_", {"literal":")"}], "postprocess": d => ["reb", d[4], d[8]]},
    {"name": "intvMExpr2$string$6", "symbols": [{"literal":"m"}, {"literal":"e"}, {"literal":"d"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intvMExpr2", "symbols": ["intvMExpr2$string$6", "_", {"literal":"("}, "_", "intvMExpr1", "_", {"literal":","}, "_", "intvMExpr1", "_", {"literal":")"}], "postprocess": d => ["!med", d[4], d[8]]},
    {"name": "intvMExpr2$string$7", "symbols": [{"literal":"a"}, {"literal":"p"}, {"literal":"p"}, {"literal":"r"}, {"literal":"o"}, {"literal":"x"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intvMExpr2", "symbols": ["intvMExpr2$string$7", "_", {"literal":"("}, "_", "intvMExpr1", "_", {"literal":","}, "_", "posInt", "_", {"literal":")"}], "postprocess": d => ["!edoApprox", d[4], parseInt(d[8])]},
    {"name": "intvMExpr2", "symbols": ["intvSymbol"], "postprocess": id},
    {"name": "intvMExpr2", "symbols": ["intvMExpr3"], "postprocess": id},
    {"name": "intvMExpr3", "symbols": ["posInt"], "postprocess": d => Interval(d[0])},
    {"name": "intvMExpr3", "symbols": ["int", "_", {"literal":"\\"}, "_", "posInt"], "postprocess": d => ["!inEDO", parseInt(d[0]), parseInt(d[4])]},
    {"name": "intvMExpr3", "symbols": ["intvMEDOExpr3", "_", {"literal":"\\"}, "_", "posInt"], "postprocess": d => ["!inEDO", d[0], parseInt(d[4])]},
    {"name": "intvMExpr3", "symbols": [{"literal":"("}, "_", "intvMExpr1", "_", {"literal":")"}], "postprocess": d => d[2]},
    {"name": "noteMExpr1", "symbols": ["noteMExpr1", "_", {"literal":"*"}, "_", "intvMExpr2"], "postprocess": d => ["mul", d[0], d[4]]},
    {"name": "noteMExpr1", "symbols": ["intvMExpr1", "_", {"literal":"*"}, "_", "noteMExpr2"], "postprocess": d => ["mul", d[0], d[4]]},
    {"name": "noteMExpr1", "symbols": ["noteMExpr1", "_", {"literal":"/"}, "_", "intvMExpr2"], "postprocess": d => ["div", d[0], d[4]]},
    {"name": "noteMExpr1", "symbols": ["noteMExpr2"], "postprocess": id},
    {"name": "noteMExpr2$string$1", "symbols": [{"literal":"a"}, {"literal":"p"}, {"literal":"p"}, {"literal":"r"}, {"literal":"o"}, {"literal":"x"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "noteMExpr2", "symbols": ["noteMExpr2$string$1", "_", {"literal":"("}, "_", "noteMExpr1", "_", {"literal":","}, "_", "posInt", "_", {"literal":")"}], "postprocess": d => ["!edoApprox", d[4], parseInt(d[8])]},
    {"name": "noteMExpr2", "symbols": ["noteSymbol"], "postprocess": id},
    {"name": "noteMExpr2", "symbols": ["noteMEDOExpr2", "_", {"literal":"\\"}, "_", "posInt"], "postprocess": d => ["!inEDO", d[0], parseInt(d[4])]},
    {"name": "noteMExpr2", "symbols": ["decimal", "hertz"], "postprocess": d => ["div", Interval(d[0]), ["!refHertz"]]},
    {"name": "noteMExpr2", "symbols": [{"literal":"("}, "_", "noteMExpr1", "_", {"literal":")"}], "postprocess": d => d[2]},
    {"name": "intvAExpr1", "symbols": ["intvAExpr1", "_", {"literal":"+"}, "_", "intvAExpr2"], "postprocess": d => ["mul", d[0], d[4]]},
    {"name": "intvAExpr1", "symbols": ["intvAExpr1", "_", {"literal":"-"}, "_", "intvAExpr2"], "postprocess": d => ["div", d[0], d[4]]},
    {"name": "intvAExpr1", "symbols": ["noteAExpr1", "_", {"literal":"-"}, "_", "noteAExpr2"], "postprocess": d => ["div", d[0], d[4]]},
    {"name": "intvAExpr1", "symbols": ["intvAExpr2"], "postprocess": id},
    {"name": "intvAExpr2", "symbols": ["intvAExpr3", "_", {"literal":"x"}, "_", "frcExpr3"], "postprocess": d => ["pow", d[0], d[4]]},
    {"name": "intvAExpr2", "symbols": ["frcExpr3", "_", {"literal":"x"}, "_", "intvAExpr3"], "postprocess": d => ["pow", d[4], d[0]]},
    {"name": "intvAExpr2", "symbols": ["intvAExpr3"], "postprocess": id},
    {"name": "intvAExpr3$string$1", "symbols": [{"literal":"c"}, {"literal":"e"}, {"literal":"n"}, {"literal":"t"}, {"literal":"s"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intvAExpr3", "symbols": ["intvAExpr3$string$1", "_", {"literal":"("}, "_", "intvMExpr1", "_", {"literal":")"}], "postprocess": d => d[4]},
    {"name": "intvAExpr3$string$2", "symbols": [{"literal":"r"}, {"literal":"e"}, {"literal":"d"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intvAExpr3", "symbols": ["intvAExpr3$string$2", "_", {"literal":"("}, "_", "intvAExpr1", "_", {"literal":")"}], "postprocess": d => ["red", d[4]]},
    {"name": "intvAExpr3$string$3", "symbols": [{"literal":"r"}, {"literal":"e"}, {"literal":"b"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intvAExpr3", "symbols": ["intvAExpr3$string$3", "_", {"literal":"("}, "_", "intvAExpr1", "_", {"literal":")"}], "postprocess": d => ["reb", d[4]]},
    {"name": "intvAExpr3$string$4", "symbols": [{"literal":"r"}, {"literal":"e"}, {"literal":"d"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intvAExpr3", "symbols": ["intvAExpr3$string$4", "_", {"literal":"("}, "_", "intvAExpr1", "_", {"literal":","}, "_", "intvMExpr1", "_", {"literal":")"}], "postprocess": d => ["red", d[4], d[8]]},
    {"name": "intvAExpr3$string$5", "symbols": [{"literal":"r"}, {"literal":"e"}, {"literal":"b"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intvAExpr3", "symbols": ["intvAExpr3$string$5", "_", {"literal":"("}, "_", "intvAExpr1", "_", {"literal":","}, "_", "intvMExpr1", "_", {"literal":")"}], "postprocess": d => ["reb", d[4], d[8]]},
    {"name": "intvAExpr3$string$6", "symbols": [{"literal":"a"}, {"literal":"p"}, {"literal":"p"}, {"literal":"r"}, {"literal":"o"}, {"literal":"x"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intvAExpr3", "symbols": ["intvAExpr3$string$6", "_", {"literal":"("}, "_", "intvAExpr1", "_", {"literal":","}, "_", "posInt", "_", {"literal":")"}], "postprocess": d => ["!edoApprox", d[4], parseInt(d[8])]},
    {"name": "intvAExpr3", "symbols": ["intvSymbol"], "postprocess": id},
    {"name": "intvAExpr3", "symbols": ["intvAExpr4"], "postprocess": id},
    {"name": "intvAExpr4", "symbols": ["decimal", {"literal":"c"}], "postprocess": d => ["!cents", d[0]]},
    {"name": "intvAExpr4", "symbols": ["intvAEDOExpr3", "_", {"literal":"\\"}, "_", "posInt"], "postprocess": d => ["!inEDO", d[0], parseInt(d[4])]},
    {"name": "intvAExpr4", "symbols": [{"literal":"("}, "_", "intvAExpr1", "_", {"literal":")"}], "postprocess": d => d[2]},
    {"name": "noteAExpr1", "symbols": ["noteAExpr1", "_", {"literal":"+"}, "_", "intvAExpr2"], "postprocess": d => ["mul", d[0], d[4]]},
    {"name": "noteAExpr1", "symbols": ["intvAExpr1", "_", {"literal":"+"}, "_", "noteAExpr2"], "postprocess": d => ["mul", d[0], d[4]]},
    {"name": "noteAExpr1", "symbols": ["noteAExpr1", "_", {"literal":"-"}, "_", "intvAExpr2"], "postprocess": d => ["div", d[0], d[4]]},
    {"name": "noteAExpr1", "symbols": ["noteAExpr2"], "postprocess": id},
    {"name": "noteAExpr2$string$1", "symbols": [{"literal":"a"}, {"literal":"p"}, {"literal":"p"}, {"literal":"r"}, {"literal":"o"}, {"literal":"x"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "noteAExpr2", "symbols": ["noteAExpr2$string$1", "_", {"literal":"("}, "_", "noteAExpr1", "_", {"literal":","}, "_", "posInt", "_", {"literal":")"}], "postprocess": d => ["!edoApprox", d[4], parseInt(d[8])]},
    {"name": "noteAExpr2", "symbols": ["noteSymbol"], "postprocess": id},
    {"name": "noteAExpr2", "symbols": ["noteAEDOExpr2", "_", {"literal":"\\"}, "_", "posInt"], "postprocess": d => ["!inEDO", d[0], parseInt(d[4])]},
    {"name": "noteAExpr2", "symbols": [{"literal":"("}, "_", "noteAExpr1", "_", {"literal":")"}], "postprocess": d => d[2]},
    {"name": "intvMEDOExpr1", "symbols": ["intvMEDOExpr1", "_", {"literal":"*"}, "_", "intvMEDOExpr2"], "postprocess": d => ["+", d[0], d[4]]},
    {"name": "intvMEDOExpr1", "symbols": ["intvMEDOExpr1", "_", {"literal":"/"}, "_", "intvMEDOExpr2"], "postprocess": d => ["-", d[0], d[4]]},
    {"name": "intvMEDOExpr1", "symbols": ["noteMEDOExpr1", "_", {"literal":"/"}, "_", "noteMEDOExpr2"], "postprocess": d => ["-", d[0], d[4]]},
    {"name": "intvMEDOExpr1", "symbols": ["intvMEDOExpr2"], "postprocess": id},
    {"name": "intvMEDOExpr2", "symbols": ["intvMEDOExpr3", "_", {"literal":"^"}, "_", "intExpr1"], "postprocess": d => ["*", d[0], d[4]]},
    {"name": "intvMEDOExpr2", "symbols": ["intvMEDOExpr3"], "postprocess": id},
    {"name": "intvMEDOExpr3", "symbols": ["upsDnsIntv"], "postprocess": id},
    {"name": "intvMEDOExpr3$string$1", "symbols": [{"literal":"T"}, {"literal":"T"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intvMEDOExpr3", "symbols": ["intvMEDOExpr3$string$1"], "postprocess": d => ["!edoTT"]},
    {"name": "intvMEDOExpr3", "symbols": [{"literal":"("}, "_", "intvMEDOExpr1", "_", {"literal":")"}], "postprocess": d => d[2]},
    {"name": "noteMEDOExpr1", "symbols": ["noteMEDOExpr1", "_", {"literal":"*"}, "_", "intvMEDOExpr2"], "postprocess": d => ["+", d[0], d[4]]},
    {"name": "noteMEDOExpr1", "symbols": ["intvMEDOExpr1", "_", {"literal":"*"}, "_", "noteMEDOExpr2"], "postprocess": d => ["+", d[0], d[4]]},
    {"name": "noteMEDOExpr1", "symbols": ["noteMEDOExpr1", "_", {"literal":"/"}, "_", "intvMEDOExpr2"], "postprocess": d => ["-", d[0], d[4]]},
    {"name": "noteMEDOExpr1", "symbols": ["noteMEDOExpr2"], "postprocess": id},
    {"name": "noteMEDOExpr2", "symbols": ["upsDnsNote"], "postprocess": id},
    {"name": "noteMEDOExpr2", "symbols": [{"literal":"("}, "_", "noteMEDOExpr1", "_", {"literal":")"}], "postprocess": d => d[2]},
    {"name": "intvAEDOExpr1", "symbols": ["intvAEDOExpr1", "_", {"literal":"+"}, "_", "intvAEDOExpr2"], "postprocess": d => ["+", d[0], d[4]]},
    {"name": "intvAEDOExpr1", "symbols": ["intvAEDOExpr1", "_", {"literal":"-"}, "_", "intvAEDOExpr2"], "postprocess": d => ["-", d[0], d[4]]},
    {"name": "intvAEDOExpr1", "symbols": ["noteAEDOExpr1", "_", {"literal":"-"}, "_", "noteAEDOExpr2"], "postprocess": d => ["-", d[0], d[4]]},
    {"name": "intvAEDOExpr1", "symbols": ["intvAEDOExpr2"], "postprocess": id},
    {"name": "intvAEDOExpr2", "symbols": ["intvAEDOExpr3", "_", {"literal":"x"}, "_", "intExpr1"], "postprocess": d => ["*", d[0], d[4]]},
    {"name": "intvAEDOExpr2", "symbols": ["intExpr1", "_", {"literal":"x"}, "_", "intvAEDOExpr3"], "postprocess": d => ["*", d[0], d[4]]},
    {"name": "intvAEDOExpr2", "symbols": ["intvAEDOExpr3"], "postprocess": id},
    {"name": "intvAEDOExpr3", "symbols": [{"literal":"-"}, "_", "intvAEDOExpr4"], "postprocess": d => ["-", 0, d[2]]},
    {"name": "intvAEDOExpr3", "symbols": ["intvAEDOExpr4"], "postprocess": id},
    {"name": "intvAEDOExpr4", "symbols": ["nonNegInt"], "postprocess": d => parseInt(d[0])},
    {"name": "intvAEDOExpr4", "symbols": ["upsDnsIntv"], "postprocess": id},
    {"name": "intvAEDOExpr4$string$1", "symbols": [{"literal":"T"}, {"literal":"T"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intvAEDOExpr4", "symbols": ["intvAEDOExpr4$string$1"], "postprocess": d => ["!edoTT"]},
    {"name": "intvAEDOExpr4", "symbols": [{"literal":"("}, "_", "intvAEDOExpr1", "_", {"literal":")"}], "postprocess": d => d[2]},
    {"name": "noteAEDOExpr1", "symbols": ["noteAEDOExpr1", "_", {"literal":"+"}, "_", "intvAEDOExpr2"], "postprocess": d => ["+", d[0], d[4]]},
    {"name": "noteAEDOExpr1", "symbols": ["intvAEDOExpr1", "_", {"literal":"+"}, "_", "noteAEDOExpr2"], "postprocess": d => ["+", d[0], d[4]]},
    {"name": "noteAEDOExpr1", "symbols": ["noteAEDOExpr1", "_", {"literal":"-"}, "_", "intvAEDOExpr2"], "postprocess": d => ["-", d[0], d[4]]},
    {"name": "noteAEDOExpr1", "symbols": ["noteAEDOExpr2"], "postprocess": id},
    {"name": "noteAEDOExpr2", "symbols": ["upsDnsNote"], "postprocess": id},
    {"name": "noteAEDOExpr2", "symbols": [{"literal":"("}, "_", "noteAEDOExpr1", "_", {"literal":")"}], "postprocess": d => d[2]},
    {"name": "intvSExpr1$string$1", "symbols": [{"literal":"r"}, {"literal":"e"}, {"literal":"d"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intvSExpr1", "symbols": ["intvSExpr1$string$1", "_", {"literal":"("}, "_", "intvSExpr1", "_", {"literal":")"}], "postprocess": d => ["red", d[4]]},
    {"name": "intvSExpr1$string$2", "symbols": [{"literal":"r"}, {"literal":"e"}, {"literal":"b"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intvSExpr1", "symbols": ["intvSExpr1$string$2", "_", {"literal":"("}, "_", "intvSExpr1", "_", {"literal":")"}], "postprocess": d => ["reb", d[4]]},
    {"name": "intvSExpr1$string$3", "symbols": [{"literal":"r"}, {"literal":"e"}, {"literal":"d"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intvSExpr1", "symbols": ["intvSExpr1$string$3", "_", {"literal":"("}, "_", "intvSExpr1", "_", {"literal":","}, "_", "intvMExpr1", "_", {"literal":")"}], "postprocess": d => ["red", d[4], d[8]]},
    {"name": "intvSExpr1$string$4", "symbols": [{"literal":"r"}, {"literal":"e"}, {"literal":"b"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intvSExpr1", "symbols": ["intvSExpr1$string$4", "_", {"literal":"("}, "_", "intvSExpr1", "_", {"literal":","}, "_", "intvMExpr1", "_", {"literal":")"}], "postprocess": d => ["reb", d[4], d[8]]},
    {"name": "intvSExpr1$string$5", "symbols": [{"literal":"a"}, {"literal":"p"}, {"literal":"p"}, {"literal":"r"}, {"literal":"o"}, {"literal":"x"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intvSExpr1", "symbols": ["intvSExpr1$string$5", "_", {"literal":"("}, "_", "intvSExpr1", "_", {"literal":","}, "_", "posInt", "_", {"literal":")"}], "postprocess": d => ["!edoApprox", d[4], parseInt(d[8])]},
    {"name": "intvSExpr1", "symbols": ["intvSExpr2"], "postprocess": id},
    {"name": "intvSExpr2", "symbols": ["intvSymbol"], "postprocess": id},
    {"name": "intvSExpr2", "symbols": ["int", "_", {"literal":"\\"}, "_", "posInt"], "postprocess": d => ["!inEDO", parseInt(d[0]), parseInt(d[4])]},
    {"name": "intvSExpr2", "symbols": ["upsDnsIntv", "_", {"literal":"\\"}, "_", "posInt"], "postprocess": d => ["!inEDO", d[0], parseInt(d[4])]},
    {"name": "intvSExpr2$string$1", "symbols": [{"literal":"T"}, {"literal":"T"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intvSExpr2", "symbols": ["intvSExpr2$string$1", "_", {"literal":"\\"}, "_", "posInt"], "postprocess": d => ["!inEDO", ["!edoTT"], parseInt(d[4])]},
    {"name": "intvSExpr2", "symbols": [{"literal":"("}, "_", "intvSExpr1", "_", {"literal":")"}], "postprocess": d => d[2]},
    {"name": "noteSExpr1$string$1", "symbols": [{"literal":"a"}, {"literal":"p"}, {"literal":"p"}, {"literal":"r"}, {"literal":"o"}, {"literal":"x"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "noteSExpr1", "symbols": ["noteSExpr1$string$1", "_", {"literal":"("}, "_", "noteSExpr1", "_", {"literal":","}, "_", "posInt", "_", {"literal":")"}], "postprocess": d => ["!edoApprox", d[4], parseInt(d[8])]},
    {"name": "noteSExpr1", "symbols": ["noteSymbol"], "postprocess": id},
    {"name": "noteSExpr1", "symbols": ["upsDnsNote", "_", {"literal":"\\"}, "_", "posInt"], "postprocess": d => ["!inEDO", d[0], parseInt(d[4])]},
    {"name": "noteSExpr1", "symbols": [{"literal":"("}, "_", "noteSExpr1", "_", {"literal":")"}], "postprocess": d => d[2]},
    {"name": "intvSymbol", "symbols": ["fjsIntv"], "postprocess": id},
    {"name": "intvSymbol", "symbols": ["nfjsNeutIntv"], "postprocess": id},
    {"name": "intvSymbol$string$1", "symbols": [{"literal":"N"}, {"literal":"F"}, {"literal":"J"}, {"literal":"S"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intvSymbol", "symbols": ["intvSymbol$string$1", "_", {"literal":"("}, "_", "nfjsIntv", "_", {"literal":")"}], "postprocess": d => d[4]},
    {"name": "intvSymbol", "symbols": ["snpyIntv"], "postprocess": id},
    {"name": "intvSymbol$string$2", "symbols": [{"literal":"T"}, {"literal":"T"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "intvSymbol", "symbols": ["intvSymbol$string$2"], "postprocess": _ => Interval(2).sqrt()},
    {"name": "noteSymbol", "symbols": ["fjsNote"], "postprocess": id},
    {"name": "noteSymbol", "symbols": ["nfjsNeutNote"], "postprocess": id},
    {"name": "noteSymbol$string$1", "symbols": [{"literal":"N"}, {"literal":"F"}, {"literal":"J"}, {"literal":"S"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "noteSymbol", "symbols": ["noteSymbol$string$1", "_", {"literal":"("}, "_", "nfjsNote", "_", {"literal":")"}], "postprocess": d => d[4]},
    {"name": "noteSymbol", "symbols": ["npyNote"], "postprocess": id},
    {"name": "pyIntv", "symbols": [{"literal":"P"}, "pyDeg"], "postprocess": (d,_,reject) => helpers.perfPyInterval(d[1],0,reject)},
    {"name": "pyIntv", "symbols": [{"literal":"M"}, "pyDeg"], "postprocess": (d,_,reject) => helpers.nonPerfPyInterval(d[1],Fraction(1,2),reject)},
    {"name": "pyIntv", "symbols": [{"literal":"m"}, "pyDeg"], "postprocess": (d,_,reject) => helpers.nonPerfPyInterval(d[1],Fraction(-1,2),reject)},
    {"name": "pyIntv$ebnf$1", "symbols": [{"literal":"A"}]},
    {"name": "pyIntv$ebnf$1", "symbols": ["pyIntv$ebnf$1", {"literal":"A"}], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "pyIntv", "symbols": ["pyIntv$ebnf$1", "pyDeg"], "postprocess": (d,_,reject) => helpers.augOrDimPyInterval(d[1],d[0].length,1,reject)},
    {"name": "pyIntv$ebnf$2", "symbols": [{"literal":"d"}]},
    {"name": "pyIntv$ebnf$2", "symbols": ["pyIntv$ebnf$2", {"literal":"d"}], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "pyIntv", "symbols": ["pyIntv$ebnf$2", "pyDeg"], "postprocess": (d,_,reject) => helpers.augOrDimPyInterval(d[1],-d[0].length,1,reject)},
    {"name": "pyIntv", "symbols": ["posInt", {"literal":"A"}, "pyDeg"], "postprocess": (d,_,reject) => helpers.augOrDimPyInterval(d[2],d[0],1,reject)},
    {"name": "pyIntv", "symbols": ["posInt", {"literal":"d"}, "pyDeg"], "postprocess": (d,_,reject) => helpers.augOrDimPyInterval(d[2],d[0],1,reject)},
    {"name": "npyIntv$subexpression$1", "symbols": [/[nN]/], "postprocess": function(d) {return d.join(""); }},
    {"name": "npyIntv", "symbols": ["npyIntv$subexpression$1", "pyDeg"], "postprocess": (d,_,reject) => helpers.nonPerfPyInterval(d[1],0,reject)},
    {"name": "npyIntv$string$1", "symbols": [{"literal":"s"}, {"literal":"A"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "npyIntv", "symbols": ["npyIntv$string$1", "pyDeg"], "postprocess": (d,_,reject) => helpers.augOrDimPyInterval(d[1],1,2,reject)},
    {"name": "npyIntv$string$2", "symbols": [{"literal":"s"}, {"literal":"d"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "npyIntv", "symbols": ["npyIntv$string$2", "pyDeg"], "postprocess": (d,_,reject) => helpers.augOrDimPyInterval(d[1],-1,2,reject)},
    {"name": "npyIntv$string$3", "symbols": [{"literal":"/"}, {"literal":"2"}, {"literal":"-"}, {"literal":"A"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "npyIntv", "symbols": ["posInt", "npyIntv$string$3", "pyDeg"], "postprocess": (d,_,reject) => helpers.augOrDimPyInterval(d[2],d[0],2,reject)},
    {"name": "npyIntv$string$4", "symbols": [{"literal":"/"}, {"literal":"2"}, {"literal":"-"}, {"literal":"d"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "npyIntv", "symbols": ["posInt", "npyIntv$string$4", "pyDeg"], "postprocess": (d,_,reject) => helpers.augOrDimPyInterval(d[2],d[0],2,reject)},
    {"name": "snpyIntv$string$1", "symbols": [{"literal":"s"}, {"literal":"M"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "snpyIntv", "symbols": ["snpyIntv$string$1", "pyDeg"], "postprocess": (d,_,reject) => helpers.nonPerfPyInterval(d[1],Fraction(1,4),reject)},
    {"name": "snpyIntv$string$2", "symbols": [{"literal":"s"}, {"literal":"m"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "snpyIntv", "symbols": ["snpyIntv$string$2", "pyDeg"], "postprocess": (d,_,reject) => helpers.nonPerfPyInterval(d[1],Fraction(-1,4),reject)},
    {"name": "snpyIntv$string$3", "symbols": [{"literal":"/"}, {"literal":"4"}, {"literal":"-"}, {"literal":"A"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "snpyIntv", "symbols": ["posInt", "snpyIntv$string$3", "pyDeg"], "postprocess": (d,_,reject) => helpers.augOrDimPyInterval(d[2],d[0],4,reject)},
    {"name": "snpyIntv$string$4", "symbols": [{"literal":"/"}, {"literal":"4"}, {"literal":"-"}, {"literal":"d"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "snpyIntv", "symbols": ["posInt", "snpyIntv$string$4", "pyDeg"], "postprocess": (d,_,reject) => helpers.augOrDimPyInterval(d[2],d[0],4,reject)},
    {"name": "pyDeg", "symbols": ["posInt"], "postprocess": d => parseInt(d[0])},
    {"name": "pyDeg", "symbols": [{"literal":"-"}, "posInt"], "postprocess": d => - parseInt(d[1])},
    {"name": "pyNote", "symbols": [{"literal":"A"}], "postprocess": _ => ["recip", ["!refIntvToA4"]]},
    {"name": "pyNote$macrocall$2", "symbols": [/[B-G]/]},
    {"name": "pyNote$macrocall$3", "symbols": ["pyNoteNoAccs"]},
    {"name": "pyNote$macrocall$1$ebnf$1", "symbols": ["int"], "postprocess": id},
    {"name": "pyNote$macrocall$1$ebnf$1", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "pyNote$macrocall$1", "symbols": ["pyNote$macrocall$2", "pyNote$macrocall$3", "pyNote$macrocall$1$ebnf$1"], "postprocess":  function(d) {
        const d2 = d[2] ? parseInt(d[2]) : 4;
        return ["mul", ["div", baseNoteIntvToA(d[0][0]), ["!refIntvToA4"]]
                     , d[1][0].mul(Interval(2).pow(d2 - 4))]; } },
    {"name": "pyNote", "symbols": ["pyNote$macrocall$1"], "postprocess": id},
    {"name": "pyNote$macrocall$5", "symbols": [/[A-G]/]},
    {"name": "pyNote$macrocall$6", "symbols": ["pyNoteAccs"]},
    {"name": "pyNote$macrocall$4$ebnf$1", "symbols": ["int"], "postprocess": id},
    {"name": "pyNote$macrocall$4$ebnf$1", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "pyNote$macrocall$4", "symbols": ["pyNote$macrocall$5", "pyNote$macrocall$6", "pyNote$macrocall$4$ebnf$1"], "postprocess":  function(d) {
        const d2 = d[2] ? parseInt(d[2]) : 4;
        return ["mul", ["div", baseNoteIntvToA(d[0][0]), ["!refIntvToA4"]]
                     , d[1][0].mul(Interval(2).pow(d2 - 4))]; } },
    {"name": "pyNote", "symbols": ["pyNote$macrocall$4"], "postprocess": id},
    {"name": "pyNoteNoAccs", "symbols": [], "postprocess": _ => Interval(1)},
    {"name": "pyNoteAccs$subexpression$1", "symbols": [{"literal":"♮"}]},
    {"name": "pyNoteAccs$subexpression$1$string$1", "symbols": [{"literal":"n"}, {"literal":"a"}, {"literal":"t"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "pyNoteAccs$subexpression$1", "symbols": ["pyNoteAccs$subexpression$1$string$1"]},
    {"name": "pyNoteAccs", "symbols": ["pyNoteAccs$subexpression$1"], "postprocess": _ => Interval(1)},
    {"name": "pyNoteAccs$ebnf$1$subexpression$1", "symbols": [{"literal":"♯"}]},
    {"name": "pyNoteAccs$ebnf$1$subexpression$1", "symbols": [{"literal":"#"}]},
    {"name": "pyNoteAccs$ebnf$1", "symbols": ["pyNoteAccs$ebnf$1$subexpression$1"]},
    {"name": "pyNoteAccs$ebnf$1$subexpression$2", "symbols": [{"literal":"♯"}]},
    {"name": "pyNoteAccs$ebnf$1$subexpression$2", "symbols": [{"literal":"#"}]},
    {"name": "pyNoteAccs$ebnf$1", "symbols": ["pyNoteAccs$ebnf$1", "pyNoteAccs$ebnf$1$subexpression$2"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "pyNoteAccs", "symbols": ["pyNoteAccs$ebnf$1"], "postprocess": d => pyInterval(1, d[0].length)},
    {"name": "pyNoteAccs$ebnf$2$subexpression$1$string$1", "symbols": [{"literal":"\ud834"}, {"literal":"\udd2a"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "pyNoteAccs$ebnf$2$subexpression$1", "symbols": ["pyNoteAccs$ebnf$2$subexpression$1$string$1"]},
    {"name": "pyNoteAccs$ebnf$2$subexpression$1", "symbols": [{"literal":"X"}]},
    {"name": "pyNoteAccs$ebnf$2", "symbols": ["pyNoteAccs$ebnf$2$subexpression$1"]},
    {"name": "pyNoteAccs$ebnf$2$subexpression$2$string$1", "symbols": [{"literal":"\ud834"}, {"literal":"\udd2a"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "pyNoteAccs$ebnf$2$subexpression$2", "symbols": ["pyNoteAccs$ebnf$2$subexpression$2$string$1"]},
    {"name": "pyNoteAccs$ebnf$2$subexpression$2", "symbols": [{"literal":"X"}]},
    {"name": "pyNoteAccs$ebnf$2", "symbols": ["pyNoteAccs$ebnf$2", "pyNoteAccs$ebnf$2$subexpression$2"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "pyNoteAccs$ebnf$3", "symbols": []},
    {"name": "pyNoteAccs$ebnf$3$subexpression$1", "symbols": [{"literal":"♯"}]},
    {"name": "pyNoteAccs$ebnf$3$subexpression$1", "symbols": [{"literal":"#"}]},
    {"name": "pyNoteAccs$ebnf$3", "symbols": ["pyNoteAccs$ebnf$3", "pyNoteAccs$ebnf$3$subexpression$1"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "pyNoteAccs", "symbols": ["pyNoteAccs$ebnf$2", "pyNoteAccs$ebnf$3"], "postprocess": d => pyInterval(1, 2*d[0].length + d[1].length)},
    {"name": "pyNoteAccs$ebnf$4$subexpression$1", "symbols": [{"literal":"♭"}]},
    {"name": "pyNoteAccs$ebnf$4$subexpression$1", "symbols": [{"literal":"b"}]},
    {"name": "pyNoteAccs$ebnf$4", "symbols": ["pyNoteAccs$ebnf$4$subexpression$1"]},
    {"name": "pyNoteAccs$ebnf$4$subexpression$2", "symbols": [{"literal":"♭"}]},
    {"name": "pyNoteAccs$ebnf$4$subexpression$2", "symbols": [{"literal":"b"}]},
    {"name": "pyNoteAccs$ebnf$4", "symbols": ["pyNoteAccs$ebnf$4", "pyNoteAccs$ebnf$4$subexpression$2"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "pyNoteAccs", "symbols": ["pyNoteAccs$ebnf$4"], "postprocess": d => pyInterval(-1, d[0].length)},
    {"name": "pyNoteAccs$ebnf$5", "symbols": []},
    {"name": "pyNoteAccs$ebnf$5$subexpression$1", "symbols": [{"literal":"♭"}]},
    {"name": "pyNoteAccs$ebnf$5$subexpression$1", "symbols": [{"literal":"b"}]},
    {"name": "pyNoteAccs$ebnf$5", "symbols": ["pyNoteAccs$ebnf$5", "pyNoteAccs$ebnf$5$subexpression$1"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "pyNoteAccs$ebnf$6$string$1", "symbols": [{"literal":"\ud834"}, {"literal":"\udd2b"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "pyNoteAccs$ebnf$6", "symbols": ["pyNoteAccs$ebnf$6$string$1"]},
    {"name": "pyNoteAccs$ebnf$6$string$2", "symbols": [{"literal":"\ud834"}, {"literal":"\udd2b"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "pyNoteAccs$ebnf$6", "symbols": ["pyNoteAccs$ebnf$6", "pyNoteAccs$ebnf$6$string$2"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "pyNoteAccs", "symbols": ["pyNoteAccs$ebnf$5", "pyNoteAccs$ebnf$6"], "postprocess": d => pyInterval(-1, 2*d[0].length + d[1].length)},
    {"name": "npyNote$macrocall$2", "symbols": [/[A-G]/]},
    {"name": "npyNote$macrocall$3", "symbols": ["npyNoteAccs"]},
    {"name": "npyNote$macrocall$1$ebnf$1", "symbols": ["int"], "postprocess": id},
    {"name": "npyNote$macrocall$1$ebnf$1", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "npyNote$macrocall$1", "symbols": ["npyNote$macrocall$2", "npyNote$macrocall$3", "npyNote$macrocall$1$ebnf$1"], "postprocess":  function(d) {
        const d2 = d[2] ? parseInt(d[2]) : 4;
        return ["mul", ["div", baseNoteIntvToA(d[0][0]), ["!refIntvToA4"]]
                     , d[1][0].mul(Interval(2).pow(d2 - 4))]; } },
    {"name": "npyNote", "symbols": ["npyNote$macrocall$1"], "postprocess": id},
    {"name": "npyNoteAccs$ebnf$1", "symbols": []},
    {"name": "npyNoteAccs$ebnf$1$subexpression$1$string$1", "symbols": [{"literal":"\ud834"}, {"literal":"\udd2a"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "npyNoteAccs$ebnf$1$subexpression$1", "symbols": ["npyNoteAccs$ebnf$1$subexpression$1$string$1"]},
    {"name": "npyNoteAccs$ebnf$1$subexpression$1", "symbols": [{"literal":"X"}]},
    {"name": "npyNoteAccs$ebnf$1", "symbols": ["npyNoteAccs$ebnf$1", "npyNoteAccs$ebnf$1$subexpression$1"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "npyNoteAccs$ebnf$2", "symbols": []},
    {"name": "npyNoteAccs$ebnf$2$subexpression$1", "symbols": [{"literal":"♯"}]},
    {"name": "npyNoteAccs$ebnf$2$subexpression$1", "symbols": [{"literal":"#"}]},
    {"name": "npyNoteAccs$ebnf$2", "symbols": ["npyNoteAccs$ebnf$2", "npyNoteAccs$ebnf$2$subexpression$1"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "npyNoteAccs$ebnf$3$subexpression$1$string$1", "symbols": [{"literal":"\ud834"}, {"literal":"\udd32"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "npyNoteAccs$ebnf$3$subexpression$1", "symbols": ["npyNoteAccs$ebnf$3$subexpression$1$string$1"]},
    {"name": "npyNoteAccs$ebnf$3$subexpression$1", "symbols": [{"literal":"t"}]},
    {"name": "npyNoteAccs$ebnf$3", "symbols": ["npyNoteAccs$ebnf$3$subexpression$1"]},
    {"name": "npyNoteAccs$ebnf$3$subexpression$2$string$1", "symbols": [{"literal":"\ud834"}, {"literal":"\udd32"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "npyNoteAccs$ebnf$3$subexpression$2", "symbols": ["npyNoteAccs$ebnf$3$subexpression$2$string$1"]},
    {"name": "npyNoteAccs$ebnf$3$subexpression$2", "symbols": [{"literal":"t"}]},
    {"name": "npyNoteAccs$ebnf$3", "symbols": ["npyNoteAccs$ebnf$3", "npyNoteAccs$ebnf$3$subexpression$2"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "npyNoteAccs", "symbols": ["npyNoteAccs$ebnf$1", "npyNoteAccs$ebnf$2", "npyNoteAccs$ebnf$3"], "postprocess": d => pyInterval(1, 2*d[0].length + d[1].length + 0.5*d[2].length)},
    {"name": "npyNoteAccs$ebnf$4$subexpression$1$string$1", "symbols": [{"literal":"\ud834"}, {"literal":"\udd33"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "npyNoteAccs$ebnf$4$subexpression$1", "symbols": ["npyNoteAccs$ebnf$4$subexpression$1$string$1"]},
    {"name": "npyNoteAccs$ebnf$4$subexpression$1", "symbols": [{"literal":"d"}]},
    {"name": "npyNoteAccs$ebnf$4", "symbols": ["npyNoteAccs$ebnf$4$subexpression$1"]},
    {"name": "npyNoteAccs$ebnf$4$subexpression$2$string$1", "symbols": [{"literal":"\ud834"}, {"literal":"\udd33"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "npyNoteAccs$ebnf$4$subexpression$2", "symbols": ["npyNoteAccs$ebnf$4$subexpression$2$string$1"]},
    {"name": "npyNoteAccs$ebnf$4$subexpression$2", "symbols": [{"literal":"d"}]},
    {"name": "npyNoteAccs$ebnf$4", "symbols": ["npyNoteAccs$ebnf$4", "npyNoteAccs$ebnf$4$subexpression$2"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "npyNoteAccs$ebnf$5", "symbols": []},
    {"name": "npyNoteAccs$ebnf$5$subexpression$1", "symbols": [{"literal":"♭"}]},
    {"name": "npyNoteAccs$ebnf$5$subexpression$1", "symbols": [{"literal":"b"}]},
    {"name": "npyNoteAccs$ebnf$5", "symbols": ["npyNoteAccs$ebnf$5", "npyNoteAccs$ebnf$5$subexpression$1"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "npyNoteAccs$ebnf$6", "symbols": []},
    {"name": "npyNoteAccs$ebnf$6$string$1", "symbols": [{"literal":"\ud834"}, {"literal":"\udd2b"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "npyNoteAccs$ebnf$6", "symbols": ["npyNoteAccs$ebnf$6", "npyNoteAccs$ebnf$6$string$1"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "npyNoteAccs", "symbols": ["npyNoteAccs$ebnf$4", "npyNoteAccs$ebnf$5", "npyNoteAccs$ebnf$6"], "postprocess": d => pyInterval(-1, 2*d[0].length + d[1].length + 0.5*d[2].length)},
    {"name": "fjsIntv$macrocall$2", "symbols": ["pyIntv"]},
    {"name": "fjsIntv$macrocall$3", "symbols": ["fjsIntv"]},
    {"name": "fjsIntv$macrocall$1", "symbols": ["fjsIntv$macrocall$2"], "postprocess": d => _ => d[0][0]},
    {"name": "fjsIntv$macrocall$1", "symbols": ["fjsIntv$macrocall$3", {"literal":"^"}, "fjsAccs"], "postprocess": d => params => ["mul", d[0][0], d[2](params)]},
    {"name": "fjsIntv$macrocall$1", "symbols": ["fjsIntv$macrocall$3", {"literal":"_"}, "fjsAccs"], "postprocess": d => params => ["div", d[0][0], d[2](params)]},
    {"name": "fjsIntv", "symbols": ["fjsIntv$macrocall$1"], "postprocess": d => d[0](fjsParams)},
    {"name": "fjsNote$macrocall$2", "symbols": ["pyNote"]},
    {"name": "fjsNote$macrocall$3", "symbols": ["fjsNote"]},
    {"name": "fjsNote$macrocall$1", "symbols": ["fjsNote$macrocall$2"], "postprocess": d => _ => d[0][0]},
    {"name": "fjsNote$macrocall$1", "symbols": ["fjsNote$macrocall$3", {"literal":"^"}, "fjsAccs"], "postprocess": d => params => ["mul", d[0][0], d[2](params)]},
    {"name": "fjsNote$macrocall$1", "symbols": ["fjsNote$macrocall$3", {"literal":"_"}, "fjsAccs"], "postprocess": d => params => ["div", d[0][0], d[2](params)]},
    {"name": "fjsNote", "symbols": ["fjsNote$macrocall$1"], "postprocess": d => d[0](fjsParams)},
    {"name": "nfjsIntv", "symbols": ["nfjsNeutIntv"], "postprocess": id},
    {"name": "nfjsIntv", "symbols": ["nfjsNonNeutIntv"], "postprocess": id},
    {"name": "nfjsNote", "symbols": ["nfjsNeutNote"], "postprocess": id},
    {"name": "nfjsNote", "symbols": ["nfjsNonNeutNote"], "postprocess": id},
    {"name": "nfjsNeutIntv$macrocall$2", "symbols": ["npyIntv"]},
    {"name": "nfjsNeutIntv$macrocall$3", "symbols": ["nfjsNeutIntv"]},
    {"name": "nfjsNeutIntv$macrocall$1", "symbols": ["nfjsNeutIntv$macrocall$2"], "postprocess": d => _ => d[0][0]},
    {"name": "nfjsNeutIntv$macrocall$1", "symbols": ["nfjsNeutIntv$macrocall$3", {"literal":"^"}, "fjsAccs"], "postprocess": d => params => ["mul", d[0][0], d[2](params)]},
    {"name": "nfjsNeutIntv$macrocall$1", "symbols": ["nfjsNeutIntv$macrocall$3", {"literal":"_"}, "fjsAccs"], "postprocess": d => params => ["div", d[0][0], d[2](params)]},
    {"name": "nfjsNeutIntv", "symbols": ["nfjsNeutIntv$macrocall$1"], "postprocess": d => d[0](nfjsParams)},
    {"name": "nfjsNonNeutIntv$macrocall$2", "symbols": ["pyIntv"]},
    {"name": "nfjsNonNeutIntv$macrocall$3", "symbols": ["nfjsNonNeutIntv"]},
    {"name": "nfjsNonNeutIntv$macrocall$1", "symbols": ["nfjsNonNeutIntv$macrocall$2"], "postprocess": d => _ => d[0][0]},
    {"name": "nfjsNonNeutIntv$macrocall$1", "symbols": ["nfjsNonNeutIntv$macrocall$3", {"literal":"^"}, "fjsAccs"], "postprocess": d => params => ["mul", d[0][0], d[2](params)]},
    {"name": "nfjsNonNeutIntv$macrocall$1", "symbols": ["nfjsNonNeutIntv$macrocall$3", {"literal":"_"}, "fjsAccs"], "postprocess": d => params => ["div", d[0][0], d[2](params)]},
    {"name": "nfjsNonNeutIntv", "symbols": ["nfjsNonNeutIntv$macrocall$1"], "postprocess": d => d[0](nfjsParams)},
    {"name": "nfjsNeutNote$macrocall$2", "symbols": ["npyNote"]},
    {"name": "nfjsNeutNote$macrocall$3", "symbols": ["nfjsNeutNote"]},
    {"name": "nfjsNeutNote$macrocall$1", "symbols": ["nfjsNeutNote$macrocall$2"], "postprocess": d => _ => d[0][0]},
    {"name": "nfjsNeutNote$macrocall$1", "symbols": ["nfjsNeutNote$macrocall$3", {"literal":"^"}, "fjsAccs"], "postprocess": d => params => ["mul", d[0][0], d[2](params)]},
    {"name": "nfjsNeutNote$macrocall$1", "symbols": ["nfjsNeutNote$macrocall$3", {"literal":"_"}, "fjsAccs"], "postprocess": d => params => ["div", d[0][0], d[2](params)]},
    {"name": "nfjsNeutNote", "symbols": ["nfjsNeutNote$macrocall$1"], "postprocess": d => d[0](nfjsParams)},
    {"name": "nfjsNonNeutNote$macrocall$2", "symbols": ["pyNote"]},
    {"name": "nfjsNonNeutNote$macrocall$3", "symbols": ["nfjsNonNeutNote"]},
    {"name": "nfjsNonNeutNote$macrocall$1", "symbols": ["nfjsNonNeutNote$macrocall$2"], "postprocess": d => _ => d[0][0]},
    {"name": "nfjsNonNeutNote$macrocall$1", "symbols": ["nfjsNonNeutNote$macrocall$3", {"literal":"^"}, "fjsAccs"], "postprocess": d => params => ["mul", d[0][0], d[2](params)]},
    {"name": "nfjsNonNeutNote$macrocall$1", "symbols": ["nfjsNonNeutNote$macrocall$3", {"literal":"_"}, "fjsAccs"], "postprocess": d => params => ["div", d[0][0], d[2](params)]},
    {"name": "nfjsNonNeutNote", "symbols": ["nfjsNonNeutNote$macrocall$1"], "postprocess": d => d[0](nfjsParams)},
    {"name": "fjsAccs", "symbols": ["fjsAcc"], "postprocess": d => params => fjsFactor(d[0], params)},
    {"name": "fjsAccs", "symbols": ["fjsAccs", {"literal":","}, "fjsAcc"], "postprocess": d => params => d[0](params).mul(fjsFactor(d[2], params))},
    {"name": "fjsAcc", "symbols": ["posInt"], "postprocess": (d,_,reject) => helpers.ensureNo2Or3(Interval(d[0]),reject)},
    {"name": "fjsAcc$string$1", "symbols": [{"literal":"s"}, {"literal":"q"}, {"literal":"r"}, {"literal":"t"}, {"literal":"("}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "fjsAcc", "symbols": ["fjsAcc$string$1", "fjsAcc", {"literal":")"}], "postprocess": d => d[1].sqrt()},
    {"name": "fjsAcc$string$2", "symbols": [{"literal":"r"}, {"literal":"o"}, {"literal":"o"}, {"literal":"t"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "fjsAcc", "symbols": ["fjsAcc$string$2", "posInt", {"literal":"("}, "fjsAcc", {"literal":")"}], "postprocess": d => d[3].root(d[1])},
    {"name": "fjsAcc", "symbols": [{"literal":"("}, "fjsAcc", {"literal":"^"}, "frcExpr3", {"literal":")"}], "postprocess": d => d[1].pow(d[3])},
    {"name": "upsDnsIntv", "symbols": ["upsDns", "pyIntv"], "postprocess": d => ["+", d[0], ["!edoPy", d[1]]]},
    {"name": "upsDnsIntv", "symbols": ["upsDns", "npyIntv"], "postprocess": d => ["+", d[0], ["!edoPy", d[1]]]},
    {"name": "upsDnsIntv", "symbols": ["upsDns", "snpyIntv"], "postprocess": d => ["+", d[0], ["!edoPy", d[1]]]},
    {"name": "upsDnsIntv", "symbols": ["upsDns", "posInt"], "postprocess":  (d,_,reject) => (pyRedDeg(d[1]) == 4 || pyRedDeg(d[1]) == 5) && d[0] != 0
        ? ["+", d[0], ["!edoPy", parseIng(d[1])]] : reject },
    {"name": "upsDnsIntv", "symbols": ["upsDns", {"literal":"~"}, "posInt"], "postprocess":  (d,_,reject) => pyRedDeg(d[2]) == 1 ? reject :
        pyRedDeg(d[2]) == 4 ? ["+", d[0], ["!edoPy", pyInterval(d[2],1,2)]] :
        pyRedDeg(d[2]) == 5 ? ["+", d[0], ["!edoPy", pyInterval(d[2],-1,2)]] :
                            ["+", d[0], ["!edoPy", pyInterval(d[2],0)]] },
    {"name": "upsDnsNote", "symbols": ["upsDns", "pyNote"], "postprocess": d => ["+", d[0], ["!edoPy", d[1]]]},
    {"name": "upsDnsNote", "symbols": ["upsDns", "npyNote"], "postprocess": d => ["+", d[0], ["!edoPy", d[1]]]},
    {"name": "upsDns", "symbols": [], "postprocess": d => 0},
    {"name": "upsDns$ebnf$1", "symbols": [{"literal":"^"}]},
    {"name": "upsDns$ebnf$1", "symbols": ["upsDns$ebnf$1", {"literal":"^"}], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "upsDns", "symbols": ["upsDns$ebnf$1"], "postprocess": d => d[0].length},
    {"name": "upsDns$ebnf$2", "symbols": [{"literal":"v"}]},
    {"name": "upsDns$ebnf$2", "symbols": ["upsDns$ebnf$2", {"literal":"v"}], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "upsDns", "symbols": ["upsDns$ebnf$2"], "postprocess": d => - d[0].length},
    {"name": "frcExpr1", "symbols": ["frcExpr1", "_", {"literal":"+"}, "_", "frcExpr2"], "postprocess": d => d[0].add(d[4])},
    {"name": "frcExpr1", "symbols": ["frcExpr1", "_", {"literal":"-"}, "_", "frcExpr2"], "postprocess": d => d[0].sub(d[4])},
    {"name": "frcExpr1", "symbols": ["frcExpr2"], "postprocess": id},
    {"name": "frcExpr2", "symbols": ["frcExpr2", "_", {"literal":"*"}, "_", "frcExpr3"], "postprocess": d => d[0].mul(d[4])},
    {"name": "frcExpr2", "symbols": ["frcExpr2", "_", {"literal":"/"}, "_", "frcExpr3"], "postprocess": d => d[0].div(d[4])},
    {"name": "frcExpr2", "symbols": ["frcExpr3"], "postprocess": id},
    {"name": "frcExpr3", "symbols": [{"literal":"-"}, "_", "frcExpr4"], "postprocess": d => d[2].neg()},
    {"name": "frcExpr3", "symbols": ["frcExpr4"], "postprocess": id},
    {"name": "frcExpr4", "symbols": ["frcExpr5", "_", {"literal":"^"}, "_", "intExpr3"], "postprocess": d => d[0].pow(d[4])},
    {"name": "frcExpr4", "symbols": ["frcExpr5"], "postprocess": id},
    {"name": "frcExpr5", "symbols": ["nonNegInt"], "postprocess": d => Fraction(d[0])},
    {"name": "frcExpr5", "symbols": [{"literal":"("}, "_", "frcExpr1", "_", {"literal":")"}], "postprocess": d => d[2]},
    {"name": "intExpr1", "symbols": ["intExpr1", "_", {"literal":"+"}, "_", "intExpr2"], "postprocess": d => d[0] + d[4]},
    {"name": "intExpr1", "symbols": ["intExpr1", "_", {"literal":"-"}, "_", "intExpr2"], "postprocess": d => d[0] - d[4]},
    {"name": "intExpr1", "symbols": ["intExpr2"], "postprocess": id},
    {"name": "intExpr2", "symbols": ["intExpr2", "_", {"literal":"*"}, "_", "intExpr3"], "postprocess": d => d[0] * d[4]},
    {"name": "intExpr2", "symbols": ["intExpr3"], "postprocess": id},
    {"name": "intExpr3", "symbols": [{"literal":"-"}, "_", "intExpr4"], "postprocess": d => - d[2]},
    {"name": "intExpr3", "symbols": ["intExpr4"], "postprocess": id},
    {"name": "intExpr4", "symbols": ["intExpr5", "_", {"literal":"^"}, "_", "posInt"], "postprocess": d => Math.pow(d[0],d[4])},
    {"name": "intExpr4", "symbols": ["intExpr5"], "postprocess": id},
    {"name": "intExpr5", "symbols": ["nonNegInt"], "postprocess": d => parseInt(d[0])},
    {"name": "intExpr5", "symbols": [{"literal":"("}, "_", "intExpr1", "_", {"literal":")"}], "postprocess": d => d[2]},
    {"name": "posInt$ebnf$1", "symbols": []},
    {"name": "posInt$ebnf$1", "symbols": ["posInt$ebnf$1", /[0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "posInt", "symbols": [/[1-9]/, "posInt$ebnf$1"], "postprocess": d => d[0] + d[1].join("")},
    {"name": "nonNegInt", "symbols": [{"literal":"0"}], "postprocess": _ => "0"},
    {"name": "nonNegInt", "symbols": ["posInt"], "postprocess": id},
    {"name": "int$ebnf$1", "symbols": [{"literal":"-"}], "postprocess": id},
    {"name": "int$ebnf$1", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "int", "symbols": ["int$ebnf$1", "nonNegInt"], "postprocess": d => (d[0] || "") + d[1]},
    {"name": "decimal$ebnf$1", "symbols": [{"literal":"-"}], "postprocess": id},
    {"name": "decimal$ebnf$1", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "decimal$ebnf$2", "symbols": [/[0-9]/]},
    {"name": "decimal$ebnf$2", "symbols": ["decimal$ebnf$2", /[0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "decimal$ebnf$3$subexpression$1$ebnf$1", "symbols": []},
    {"name": "decimal$ebnf$3$subexpression$1$ebnf$1", "symbols": ["decimal$ebnf$3$subexpression$1$ebnf$1", /[0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "decimal$ebnf$3$subexpression$1$ebnf$2$subexpression$1$ebnf$1", "symbols": [/[0-9]/]},
    {"name": "decimal$ebnf$3$subexpression$1$ebnf$2$subexpression$1$ebnf$1", "symbols": ["decimal$ebnf$3$subexpression$1$ebnf$2$subexpression$1$ebnf$1", /[0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "decimal$ebnf$3$subexpression$1$ebnf$2$subexpression$1", "symbols": [{"literal":"("}, "decimal$ebnf$3$subexpression$1$ebnf$2$subexpression$1$ebnf$1", {"literal":")"}]},
    {"name": "decimal$ebnf$3$subexpression$1$ebnf$2", "symbols": ["decimal$ebnf$3$subexpression$1$ebnf$2$subexpression$1"], "postprocess": id},
    {"name": "decimal$ebnf$3$subexpression$1$ebnf$2", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "decimal$ebnf$3$subexpression$1", "symbols": [{"literal":"."}, "decimal$ebnf$3$subexpression$1$ebnf$1", "decimal$ebnf$3$subexpression$1$ebnf$2"]},
    {"name": "decimal$ebnf$3", "symbols": ["decimal$ebnf$3$subexpression$1"], "postprocess": id},
    {"name": "decimal$ebnf$3", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "decimal", "symbols": ["decimal$ebnf$1", "decimal$ebnf$2", "decimal$ebnf$3"], "postprocess":  d => (d[0] || "") + d[1].join("")
        + (d[2] ? "." + d[2][1].join("")
                      + (d[2][2] ? "("+d[2][2][1].join("")+")"
                                 : "")
                : "") },
    {"name": "hertz$string$1", "symbols": [{"literal":"h"}, {"literal":"z"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "hertz", "symbols": ["hertz$string$1"]},
    {"name": "hertz$string$2", "symbols": [{"literal":"H"}, {"literal":"z"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "hertz", "symbols": ["hertz$string$2"]}
]
  , ParserStart: "top1"
}
if (typeof module !== 'undefined'&& typeof module.exports !== 'undefined') {
   module.exports = grammar;
} else {
   window.grammar = grammar;
}
})();

},{"../edo.js":2,"../fjs.js":4,"../interval.js":6,"../pythagorean.js":11,"./eval.js":8,"./grammar-helpers.js":9,"fraction.js":12}],11:[function(require,module,exports){
/**
 * Functions for working with pythagorean and neutral pythagorean intervals
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module pythagorean
 **/

const pf = require('primes-and-factors');
const ntw = require('number-to-words');
const Fraction = require('fraction.js');
const Interval = require('./interval.js');

function mod(a,n) {
  return ((a % n) + n) % n;
}

/**
  * Constructs an interval from a pythagorean degree and offset
  *
  * @param {integer} d
  * @param {Fraction} o
  * @returns {Interval}
  */
function pyInterval(d,a,b) {
  const ox4 = Fraction(a,b).mul(4 * Math.sign(d));
  if (ox4.d != 1) {
    throw "offset does not have denominator 1, 2, or 4"
  }
  const zd = d - Math.sign(d);
  const ng = mod(zd * 4 + 3, 7) - 3;
  const g = ng * 2 + ox4.s * ox4.n * 7;
  return Interval({ 2: Fraction(g,4).sub((zd - g) / 7).neg(),
                    3: Fraction(g,4) });
}

/**
  * Checks whether the given interval is pythagorean
  *
  * @param {Interval} i
  * @returns {boolean}
  */
function isPythagorean(a,b) {
  const i = new Interval(a,b);
  const e2 = (i['2'] || Fraction(0));
  const e3 = (i['3'] || Fraction(0));
  return Object.entries(i).length == (!!i['2'] + !!i['3'])
         && e3.mul(4).d == 1 && e2.add(e3).d == 1;
}

/**
  * For a given pythagorean interval `(3/2)^(g/4) * 2^v`, returns the `g`.
  *
  * @param {Interval} i
  * @returns {integer}
  */
function pyGenerator(a,b) {
  const i = new Interval(a,b);
  const g = (i['3'] || Fraction(0)).mul(4);
  if (g.d != 1) {
    throw "interval is not pythagorean";
  }
  return g.s * g.n;
}

/**
  * For a given pythagorean interval `(3/2)^(g/4) * 2^v`, returns the `v`.
  *
  * @param {Interval} i
  * @returns {integer}
  */
function pyOctaves(a,b) {
  const i = new Interval(a,b);
  const e2 = (i['2'] || Fraction(0));
  const e3 = (i['3'] || Fraction(0));
  const v = e2.add(e3);
  if (v.d != 1) {
    throw "interval is not pythagorean";
  }
  return v.s * v.n;
}

/**
  * Returns the degree of the given pythagorean interval
  *
  * @param {Interval} i
  * @returns {integer}
  */
function pyDegree(a,b) {
  const i = new Interval(a,b);
  const g = pyGenerator(i);
  const v = pyOctaves(i);
  const zd = g + v * 7;
  return zd == 0 ? 1 : zd + Math.sign(zd);
}

/**
  * Returns the offset of the given pythagorean interval
  *
  * @param {Interval} i
  * @returns {Fraction}
  */
function pyOffset(a,b) {
  const i = new Interval(a,b);
  const g = pyGenerator(i);
  const v = pyOctaves(i);
  const zd = g + v * 7;
  const szd = zd == 0 ? 1 : Math.sign(zd)
  return Fraction(szd * (2 * Math.floor((4 * g + 3) / 7) - g), 4);
}

/**
  * Reduces a pythagorean degree so it lies between 1 and 7
  *
  * @param {integer} d
  * @returns {integer}
  */
function pyRedDeg(d) {
  return mod(d - Math.sign(d), 7) + 1;
}

/**
  * Checks whether a given degree (of a pythagorean interval) is a unison,
  * fourth, or fifth
  *
  * @param {integer} d
  * @returns {boolean}
  */
function isPerfectDeg(d) {
  return pyRedDeg(d) == 1 || pyRedDeg(d) == 4 || pyRedDeg(d) == 5;
}

function case2(n, a, b) {
  if (n == 0 || !n) { return a; }
  return b;
}

function case3(n, a, b, c) {
  if (n == 0 || !n) { return a; }
  if (n == 1) { return b; }
  return c;
}

/**
  * Returns the quality of the given pythagorean interval
  *
  * @param {Interval} i
  * @param {{verbosity: integer}=} opts verbosity can be the default 0
  *                                     (e.g. "d"), 1 (e.g. "dim"), or 2
  *                                     (e.g. "diminished")
  * @returns {string}
  */
function pyQuality(a,b, opts) {
  // if only two arguments are given, the second one may be `opts`!
  if (!opts && typeof b == 'object' && b != null) {
      opts = b;
      b = undefined;
  }
  const {verbosity} = opts || {};
  let o = pyOffset(a,b);
  if (isPerfectDeg(pyDegree(a,b))) {
    if (o == 0    ) { return case2(verbosity, "P", "perfect"); }
  }
  else {
    if (o == 0    ) { return case2(verbosity, "n", "neutral"); }
    if (o == 0.25 ) { return case2(verbosity, "sM", "semi-major"); }
    if (o == 0.5  ) { return case2(verbosity, "M", "major"); }
    if (o == -0.25) { return case2(verbosity, "sm", "semi-minor"); }
    if (o == -0.5 ) { return case2(verbosity, "m", "minor"); }
    o = o.sub(o.s,2);
  }
  if (o == 0.5 ) { return case3(verbosity, "sA", "semi-aug", "semi-augmented"); }
  if (o == 1   ) { return case3(verbosity, "A", "aug", "augmented"); }
  if (o == -0.5) { return case3(verbosity, "sd", "semi-dim", "semi-diminished"); }
  if (o == -1  ) { return case3(verbosity, "d", "dim", "diminished"); }
  if (o ==  2 && verbosity == 2) { return "doubly augmented"; }
  if (o == -2 && verbosity == 2) { return "doubly diminished"; }
  if (o > 0 && o.d == 1) { return o.n + case3(verbosity, "A", "-aug", "-augmented"); }
  if (o > 0 && o.d != 1) { return o.toFraction() + case3(verbosity, "-A", "-aug", "-augmented"); }
  if (o < 0 && o.d == 1) { return o.n + case3(verbosity, "d", "-dim", "-diminished"); }
  if (o < 0 && o.d != 1) { return o.neg().toFraction() + case3(verbosity, "-d", "-dim", "-diminished"); }
}

function pyDegreeString(d, verbosity) {
  if (verbosity == 0 || !verbosity) {
    return d;
  }
  if (verbosity == 1) {
    return ntw.toOrdinal(Math.abs(d));
  }
  if (Math.abs(d) == 1) {
    return "unison"
  }
  if (Math.abs(d) == 8) {
    return "octave"
  }
  return ntw.toWordsOrdinal(Math.abs(d));
}

/**
  * Returns the symbol of the given pythagorean interval
  *
  * @param {Interval} i
  * @param {{verbosity: integer}=} opts verbosity can be the default 0
  *                                     (e.g. "d2"), 1 (e.g. "dim 2nd"), or 2
  *                                     (e.g. "diminished second")
  * @returns {string}
  */
function pySymb(a,b, opts) {
  // if only two arguments are given, the second one may be `opts`!
  if (!opts && typeof b == 'object' && b != null) {
      opts = b;
      b = undefined;
  }
  const {verbosity} = opts || {};
  const d = pyDegree(a,b);
  const d_str = case2(verbosity, "", " ") + pyDegreeString(d, verbosity);
  const down_str = verbosity && d < 0 ? " down" : "";
  return pyQuality(a,b, opts) + d_str + down_str;
}

/**
  * Returns the interval to A of the given base note name (i.e. A, B, C, D, E,
  * F or G) within a single octave of scientific pitch notation
  *
  * @param {string} baseNote
  * @returns {Interval}
  */
function baseNoteIntvToA(x) {
  if (x == "C") { return pyInterval(-6, 0.5) /* M6 down */ }
  if (x == "D") { return pyInterval(-5, 0)   /* P5 down */ }
  if (x == "E") { return pyInterval(-4, 0)   /* P4 down */ }
  if (x == "F") { return pyInterval(-3, 0.5) /* M3 down */ }
  if (x == "G") { return pyInterval(-2, 0.5) /* M2 down */ }
  if (x == "A") { return Interval(1)         /* P1 */      }
  if (x == "B") { return pyInterval(2, 0.5)  /* M2 */      }
}

/**
  * Returns the octave in scientific pitch notation of the given interval to A4
  *
  * @param {Interval} intvToA4
  * @returns {integer}
  */
function octaveOfIntvToA4(a,b) {
  const intvToA4 = Interval(a,b);
  const intvToC4 = intvToA4.div(baseNoteIntvToA("C"));
  return 4 + Math.floor(Math.log(intvToC4.valueOf()) / Math.log(2));
}

/**
  * Returns the note name of the given non-neutral pythagorean interval to A4.
  * The returned string uses ASCII instead of uniode wherever possible iff the
  * second argument is given and is true
  *
  * @param {Interval} intvToA4
  * @param {Boolean} [useASCII=false]
  * @returns {string}
  */
function pyNote(intvToA4, useASCII) {
  const intvToF4 = Interval(intvToA4).div(baseNoteIntvToA("F"));
  if (!isPythagorean(intvToF4) || (intvToF4['3'] && intvToF4['3'].d != 1)) {
    throw "interval is not a non-neutral pythagorean interval"
  }
  const e3 = intvToF4['3'] ? intvToF4['3'].s * intvToF4['3'].n : Fraction(0);
  const zd = mod(4*e3, 7);
  let o = Math.floor(e3 / 7);

  let octave = octaveOfIntvToA4(intvToA4);
  if (octave == 4) { octave = ""; }

  let baseNote;
  if (zd == 0) { baseNote = "F"; }
  if (zd == 1) { baseNote = "G"; }
  if (zd == 2) { baseNote = "A"; }
  if (zd == 3) { baseNote = "B"; }
  if (zd == 4) { baseNote = "C"; }
  if (zd == 5) { baseNote = "D"; }
  if (zd == 6) { baseNote = "E"; }

  let accidentals = "";
  if (o == 0 && baseNote == "A" && octave != "") {
    accidentals += "♮";
  }
  while (o > 1) {
    accidentals += useASCII ? "X" : "𝄪";
    o -= 2;
  }
  if (o == 1) {
    accidentals += useASCII ? "#" : "♯";
  }
  while (o < -1) {
    if (useASCII) {
      accidentals += "b";
      o += 1;
    } else {
      accidentals += "𝄫";
      o += 2;
    }
  }
  if (o == -1) {
    accidentals += useASCII ? "b" : "♭";
  }

  return baseNote + accidentals + octave;
}

module['exports'].pyInterval = pyInterval;
module['exports'].isPythagorean = isPythagorean;
module['exports'].pyGenerator = pyGenerator;
module['exports'].pyOctaves = pyOctaves;
module['exports'].pyDegree = pyDegree;
module['exports'].pyOffset = pyOffset;
module['exports'].pyRedDeg = pyRedDeg;
module['exports'].isPerfectDeg = isPerfectDeg;
module['exports'].pyQuality = pyQuality;
module['exports'].pySymb = pySymb;
module['exports'].pySymb = pySymb;
module['exports'].baseNoteIntvToA = baseNoteIntvToA;
module['exports'].octaveOfIntvToA4 = octaveOfIntvToA4;
module['exports'].pyNote = pyNote;

},{"./interval.js":6,"fraction.js":12,"number-to-words":15,"primes-and-factors":16}],12:[function(require,module,exports){
/**
 * @license Fraction.js v4.0.12 09/09/2015
 * http://www.xarg.org/2014/03/rational-numbers-in-javascript/
 *
 * Copyright (c) 2015, Robert Eisele (robert@xarg.org)
 * Dual licensed under the MIT or GPL Version 2 licenses.
 **/


/**
 *
 * This class offers the possibility to calculate fractions.
 * You can pass a fraction in different formats. Either as array, as double, as string or as an integer.
 *
 * Array/Object form
 * [ 0 => <nominator>, 1 => <denominator> ]
 * [ n => <nominator>, d => <denominator> ]
 *
 * Integer form
 * - Single integer value
 *
 * Double form
 * - Single double value
 *
 * String form
 * 123.456 - a simple double
 * 123/456 - a string fraction
 * 123.'456' - a double with repeating decimal places
 * 123.(456) - synonym
 * 123.45'6' - a double with repeating last place
 * 123.45(6) - synonym
 *
 * Example:
 *
 * var f = new Fraction("9.4'31'");
 * f.mul([-4, 3]).div(4.9);
 *
 */

(function(root) {

  "use strict";

  // Maximum search depth for cyclic rational numbers. 2000 should be more than enough.
  // Example: 1/7 = 0.(142857) has 6 repeating decimal places.
  // If MAX_CYCLE_LEN gets reduced, long cycles will not be detected and toString() only gets the first 10 digits
  var MAX_CYCLE_LEN = 2000;

  // Parsed data to avoid calling "new" all the time
  var P = {
    "s": 1,
    "n": 0,
    "d": 1
  };

  function createError(name) {

    function errorConstructor() {
      var temp = Error.apply(this, arguments);
      temp['name'] = this['name'] = name;
      this['stack'] = temp['stack'];
      this['message'] = temp['message'];
    }

    /**
     * Error constructor
     *
     * @constructor
     */
    function IntermediateInheritor() { }
    IntermediateInheritor.prototype = Error.prototype;
    errorConstructor.prototype = new IntermediateInheritor();

    return errorConstructor;
  }

  var DivisionByZero = Fraction['DivisionByZero'] = createError('DivisionByZero');
  var InvalidParameter = Fraction['InvalidParameter'] = createError('InvalidParameter');

  function assign(n, s) {

    if (isNaN(n = parseInt(n, 10))) {
      throwInvalidParam();
    }
    return n * s;
  }

  function throwInvalidParam() {
    throw new InvalidParameter();
  }

  var parse = function(p1, p2) {

    var n = 0, d = 1, s = 1;
    var v = 0, w = 0, x = 0, y = 1, z = 1;

    var A = 0, B = 1;
    var C = 1, D = 1;

    var N = 10000000;
    var M;

    if (p1 === undefined || p1 === null) {
      /* void */
    } else if (p2 !== undefined) {
      n = p1;
      d = p2;
      s = n * d;
    } else
      switch (typeof p1) {

        case "object":
          {
            if ("d" in p1 && "n" in p1) {
              n = p1["n"];
              d = p1["d"];
              if ("s" in p1)
                n *= p1["s"];
            } else if (0 in p1) {
              n = p1[0];
              if (1 in p1)
                d = p1[1];
            } else {
              throwInvalidParam();
            }
            s = n * d;
            break;
          }
        case "number":
          {
            if (p1 < 0) {
              s = p1;
              p1 = -p1;
            }

            if (p1 % 1 === 0) {
              n = p1;
            } else if (p1 > 0) { // check for != 0, scale would become NaN (log(0)), which converges really slow

              if (p1 >= 1) {
                z = Math.pow(10, Math.floor(1 + Math.log(p1) / Math.LN10));
                p1 /= z;
              }

              // Using Farey Sequences
              // http://www.johndcook.com/blog/2010/10/20/best-rational-approximation/

              while (B <= N && D <= N) {
                M = (A + C) / (B + D);

                if (p1 === M) {
                  if (B + D <= N) {
                    n = A + C;
                    d = B + D;
                  } else if (D > B) {
                    n = C;
                    d = D;
                  } else {
                    n = A;
                    d = B;
                  }
                  break;

                } else {

                  if (p1 > M) {
                    A += C;
                    B += D;
                  } else {
                    C += A;
                    D += B;
                  }

                  if (B > N) {
                    n = C;
                    d = D;
                  } else {
                    n = A;
                    d = B;
                  }
                }
              }
              n *= z;
            } else if (isNaN(p1) || isNaN(p2)) {
              d = n = NaN;
            }
            break;
          }
        case "string":
          {
            B = p1.match(/\d+|./g);

            if (B === null)
              throwInvalidParam();

            if (B[A] === '-') {// Check for minus sign at the beginning
              s = -1;
              A++;
            } else if (B[A] === '+') {// Check for plus sign at the beginning
              A++;
            }

            if (B.length === A + 1) { // Check if it's just a simple number "1234"
              w = assign(B[A++], s);
            } else if (B[A + 1] === '.' || B[A] === '.') { // Check if it's a decimal number

              if (B[A] !== '.') { // Handle 0.5 and .5
                v = assign(B[A++], s);
              }
              A++;

              // Check for decimal places
              if (A + 1 === B.length || B[A + 1] === '(' && B[A + 3] === ')' || B[A + 1] === "'" && B[A + 3] === "'") {
                w = assign(B[A], s);
                y = Math.pow(10, B[A].length);
                A++;
              }

              // Check for repeating places
              if (B[A] === '(' && B[A + 2] === ')' || B[A] === "'" && B[A + 2] === "'") {
                x = assign(B[A + 1], s);
                z = Math.pow(10, B[A + 1].length) - 1;
                A += 3;
              }

            } else if (B[A + 1] === '/' || B[A + 1] === ':') { // Check for a simple fraction "123/456" or "123:456"
              w = assign(B[A], s);
              y = assign(B[A + 2], 1);
              A += 3;
            } else if (B[A + 3] === '/' && B[A + 1] === ' ') { // Check for a complex fraction "123 1/2"
              v = assign(B[A], s);
              w = assign(B[A + 2], s);
              y = assign(B[A + 4], 1);
              A += 5;
            }

            if (B.length <= A) { // Check for more tokens on the stack
              d = y * z;
              s = /* void */
              n = x + d * v + z * w;
              break;
            }

            /* Fall through on error */
          }
        default:
          throwInvalidParam();
      }

    if (d === 0) {
      throw new DivisionByZero();
    }

    P["s"] = s < 0 ? -1 : 1;
    P["n"] = Math.abs(n);
    P["d"] = Math.abs(d);
  };

  function modpow(b, e, m) {

    var r = 1;
    for (; e > 0; b = (b * b) % m, e >>= 1) {

      if (e & 1) {
        r = (r * b) % m;
      }
    }
    return r;
  }


  function cycleLen(n, d) {

    for (; d % 2 === 0;
      d /= 2) {
    }

    for (; d % 5 === 0;
      d /= 5) {
    }

    if (d === 1) // Catch non-cyclic numbers
      return 0;

    // If we would like to compute really large numbers quicker, we could make use of Fermat's little theorem:
    // 10^(d-1) % d == 1
    // However, we don't need such large numbers and MAX_CYCLE_LEN should be the capstone,
    // as we want to translate the numbers to strings.

    var rem = 10 % d;
    var t = 1;

    for (; rem !== 1; t++) {
      rem = rem * 10 % d;

      if (t > MAX_CYCLE_LEN)
        return 0; // Returning 0 here means that we don't print it as a cyclic number. It's likely that the answer is `d-1`
    }
    return t;
  }


  function cycleStart(n, d, len) {

    var rem1 = 1;
    var rem2 = modpow(10, len, d);

    for (var t = 0; t < 300; t++) { // s < ~log10(Number.MAX_VALUE)
      // Solve 10^s == 10^(s+t) (mod d)

      if (rem1 === rem2)
        return t;

      rem1 = rem1 * 10 % d;
      rem2 = rem2 * 10 % d;
    }
    return 0;
  }

  function gcd(a, b) {

    if (!a)
      return b;
    if (!b)
      return a;

    while (1) {
      a %= b;
      if (!a)
        return b;
      b %= a;
      if (!b)
        return a;
    }
  };

  /**
   * Module constructor
   *
   * @constructor
   * @param {number|Fraction=} a
   * @param {number=} b
   */
  function Fraction(a, b) {

    if (!(this instanceof Fraction)) {
      return new Fraction(a, b);
    }

    parse(a, b);

    if (Fraction['REDUCE']) {
      a = gcd(P["d"], P["n"]); // Abuse a
    } else {
      a = 1;
    }

    this["s"] = P["s"];
    this["n"] = P["n"] / a;
    this["d"] = P["d"] / a;
  }

  /**
   * Boolean global variable to be able to disable automatic reduction of the fraction
   *
   */
  Fraction['REDUCE'] = 1;

  Fraction.prototype = {

    "s": 1,
    "n": 0,
    "d": 1,

    /**
     * Calculates the absolute value
     *
     * Ex: new Fraction(-4).abs() => 4
     **/
    "abs": function() {

      return new Fraction(this["n"], this["d"]);
    },

    /**
     * Inverts the sign of the current fraction
     *
     * Ex: new Fraction(-4).neg() => 4
     **/
    "neg": function() {

      return new Fraction(-this["s"] * this["n"], this["d"]);
    },

    /**
     * Adds two rational numbers
     *
     * Ex: new Fraction({n: 2, d: 3}).add("14.9") => 467 / 30
     **/
    "add": function(a, b) {

      parse(a, b);
      return new Fraction(
        this["s"] * this["n"] * P["d"] + P["s"] * this["d"] * P["n"],
        this["d"] * P["d"]
      );
    },

    /**
     * Subtracts two rational numbers
     *
     * Ex: new Fraction({n: 2, d: 3}).add("14.9") => -427 / 30
     **/
    "sub": function(a, b) {

      parse(a, b);
      return new Fraction(
        this["s"] * this["n"] * P["d"] - P["s"] * this["d"] * P["n"],
        this["d"] * P["d"]
      );
    },

    /**
     * Multiplies two rational numbers
     *
     * Ex: new Fraction("-17.(345)").mul(3) => 5776 / 111
     **/
    "mul": function(a, b) {

      parse(a, b);
      return new Fraction(
        this["s"] * P["s"] * this["n"] * P["n"],
        this["d"] * P["d"]
      );
    },

    /**
     * Divides two rational numbers
     *
     * Ex: new Fraction("-17.(345)").inverse().div(3)
     **/
    "div": function(a, b) {

      parse(a, b);
      return new Fraction(
        this["s"] * P["s"] * this["n"] * P["d"],
        this["d"] * P["n"]
      );
    },

    /**
     * Clones the actual object
     *
     * Ex: new Fraction("-17.(345)").clone()
     **/
    "clone": function() {
      return new Fraction(this);
    },

    /**
     * Calculates the modulo of two rational numbers - a more precise fmod
     *
     * Ex: new Fraction('4.(3)').mod([7, 8]) => (13/3) % (7/8) = (5/6)
     **/
    "mod": function(a, b) {

      if (isNaN(this['n']) || isNaN(this['d'])) {
        return new Fraction(NaN);
      }

      if (a === undefined) {
        return new Fraction(this["s"] * this["n"] % this["d"], 1);
      }

      parse(a, b);
      if (0 === P["n"] && 0 === this["d"]) {
        Fraction(0, 0); // Throw DivisionByZero
      }

      /*
       * First silly attempt, kinda slow
       *
       return that["sub"]({
       "n": num["n"] * Math.floor((this.n / this.d) / (num.n / num.d)),
       "d": num["d"],
       "s": this["s"]
       });*/

      /*
       * New attempt: a1 / b1 = a2 / b2 * q + r
       * => b2 * a1 = a2 * b1 * q + b1 * b2 * r
       * => (b2 * a1 % a2 * b1) / (b1 * b2)
       */
      return new Fraction(
        this["s"] * (P["d"] * this["n"]) % (P["n"] * this["d"]),
        P["d"] * this["d"]
      );
    },

    /**
     * Calculates the fractional gcd of two rational numbers
     *
     * Ex: new Fraction(5,8).gcd(3,7) => 1/56
     */
    "gcd": function(a, b) {

      parse(a, b);

      // gcd(a / b, c / d) = gcd(a, c) / lcm(b, d)

      return new Fraction(gcd(P["n"], this["n"]) * gcd(P["d"], this["d"]), P["d"] * this["d"]);
    },

    /**
     * Calculates the fractional lcm of two rational numbers
     *
     * Ex: new Fraction(5,8).lcm(3,7) => 15
     */
    "lcm": function(a, b) {

      parse(a, b);

      // lcm(a / b, c / d) = lcm(a, c) / gcd(b, d)

      if (P["n"] === 0 && this["n"] === 0) {
        return new Fraction;
      }
      return new Fraction(P["n"] * this["n"], gcd(P["n"], this["n"]) * gcd(P["d"], this["d"]));
    },

    /**
     * Calculates the ceil of a rational number
     *
     * Ex: new Fraction('4.(3)').ceil() => (5 / 1)
     **/
    "ceil": function(places) {

      places = Math.pow(10, places || 0);

      if (isNaN(this["n"]) || isNaN(this["d"])) {
        return new Fraction(NaN);
      }
      return new Fraction(Math.ceil(places * this["s"] * this["n"] / this["d"]), places);
    },

    /**
     * Calculates the floor of a rational number
     *
     * Ex: new Fraction('4.(3)').floor() => (4 / 1)
     **/
    "floor": function(places) {

      places = Math.pow(10, places || 0);

      if (isNaN(this["n"]) || isNaN(this["d"])) {
        return new Fraction(NaN);
      }
      return new Fraction(Math.floor(places * this["s"] * this["n"] / this["d"]), places);
    },

    /**
     * Rounds a rational numbers
     *
     * Ex: new Fraction('4.(3)').round() => (4 / 1)
     **/
    "round": function(places) {

      places = Math.pow(10, places || 0);

      if (isNaN(this["n"]) || isNaN(this["d"])) {
        return new Fraction(NaN);
      }
      return new Fraction(Math.round(places * this["s"] * this["n"] / this["d"]), places);
    },

    /**
     * Gets the inverse of the fraction, means numerator and denominator are exchanged
     *
     * Ex: new Fraction([-3, 4]).inverse() => -4 / 3
     **/
    "inverse": function() {

      return new Fraction(this["s"] * this["d"], this["n"]);
    },

    /**
     * Calculates the fraction to some integer exponent
     *
     * Ex: new Fraction(-1,2).pow(-3) => -8
     */
    "pow": function(m) {

      if (m < 0) {
        return new Fraction(Math.pow(this['s'] * this["d"], -m), Math.pow(this["n"], -m));
      } else {
        return new Fraction(Math.pow(this['s'] * this["n"], m), Math.pow(this["d"], m));
      }
    },

    /**
     * Check if two rational numbers are the same
     *
     * Ex: new Fraction(19.6).equals([98, 5]);
     **/
    "equals": function(a, b) {

      parse(a, b);
      return this["s"] * this["n"] * P["d"] === P["s"] * P["n"] * this["d"]; // Same as compare() === 0
    },

    /**
     * Check if two rational numbers are the same
     *
     * Ex: new Fraction(19.6).equals([98, 5]);
     **/
    "compare": function(a, b) {

      parse(a, b);
      var t = (this["s"] * this["n"] * P["d"] - P["s"] * P["n"] * this["d"]);
      return (0 < t) - (t < 0);
    },

    "simplify": function(eps) {

      // First naive implementation, needs improvement

      if (isNaN(this['n']) || isNaN(this['d'])) {
        return this;
      }

      var cont = this['abs']()['toContinued']();

      eps = eps || 0.001;

      function rec(a) {
        if (a.length === 1)
          return new Fraction(a[0]);
        return rec(a.slice(1))['inverse']()['add'](a[0]);
      }

      for (var i = 0; i < cont.length; i++) {
        var tmp = rec(cont.slice(0, i + 1));
        if (tmp['sub'](this['abs']())['abs']().valueOf() < eps) {
          return tmp['mul'](this['s']);
        }
      }
      return this;
    },

    /**
     * Check if two rational numbers are divisible
     *
     * Ex: new Fraction(19.6).divisible(1.5);
     */
    "divisible": function(a, b) {

      parse(a, b);
      return !(!(P["n"] * this["d"]) || ((this["n"] * P["d"]) % (P["n"] * this["d"])));
    },

    /**
     * Returns a decimal representation of the fraction
     *
     * Ex: new Fraction("100.'91823'").valueOf() => 100.91823918239183
     **/
    'valueOf': function() {

      return this["s"] * this["n"] / this["d"];
    },

    /**
     * Returns a string-fraction representation of a Fraction object
     *
     * Ex: new Fraction("1.'3'").toFraction() => "4 1/3"
     **/
    'toFraction': function(excludeWhole) {

      var whole, str = "";
      var n = this["n"];
      var d = this["d"];
      if (this["s"] < 0) {
        str += '-';
      }

      if (d === 1) {
        str += n;
      } else {

        if (excludeWhole && (whole = Math.floor(n / d)) > 0) {
          str += whole;
          str += " ";
          n %= d;
        }

        str += n;
        str += '/';
        str += d;
      }
      return str;
    },

    /**
     * Returns a latex representation of a Fraction object
     *
     * Ex: new Fraction("1.'3'").toLatex() => "\frac{4}{3}"
     **/
    'toLatex': function(excludeWhole) {

      var whole, str = "";
      var n = this["n"];
      var d = this["d"];
      if (this["s"] < 0) {
        str += '-';
      }

      if (d === 1) {
        str += n;
      } else {

        if (excludeWhole && (whole = Math.floor(n / d)) > 0) {
          str += whole;
          n %= d;
        }

        str += "\\frac{";
        str += n;
        str += '}{';
        str += d;
        str += '}';
      }
      return str;
    },

    /**
     * Returns an array of continued fraction elements
     *
     * Ex: new Fraction("7/8").toContinued() => [0,1,7]
     */
    'toContinued': function() {

      var t;
      var a = this['n'];
      var b = this['d'];
      var res = [];

      if (isNaN(a) || isNaN(b)) {
        return res;
      }

      do {
        res.push(Math.floor(a / b));
        t = a % b;
        a = b;
        b = t;
      } while (a !== 1);

      return res;
    },

    /**
     * Creates a string representation of a fraction with all digits
     *
     * Ex: new Fraction("100.'91823'").toString() => "100.(91823)"
     **/
    'toString': function(dec) {

      var g;
      var N = this["n"];
      var D = this["d"];

      if (isNaN(N) || isNaN(D)) {
        return "NaN";
      }

      if (!Fraction['REDUCE']) {
        g = gcd(N, D);
        N /= g;
        D /= g;
      }

      dec = dec || 15; // 15 = decimal places when no repitation

      var cycLen = cycleLen(N, D); // Cycle length
      var cycOff = cycleStart(N, D, cycLen); // Cycle start

      var str = this['s'] === -1 ? "-" : "";

      str += N / D | 0;

      N %= D;
      N *= 10;

      if (N)
        str += ".";

      if (cycLen) {

        for (var i = cycOff; i--;) {
          str += N / D | 0;
          N %= D;
          N *= 10;
        }
        str += "(";
        for (var i = cycLen; i--;) {
          str += N / D | 0;
          N %= D;
          N *= 10;
        }
        str += ")";
      } else {
        for (var i = dec; N && i--;) {
          str += N / D | 0;
          N %= D;
          N *= 10;
        }
      }
      return str;
    }
  };

  if (typeof define === "function" && define["amd"]) {
    define([], function() {
      return Fraction;
    });
  } else if (typeof exports === "object") {
    Object.defineProperty(Fraction, "__esModule", { 'value': true });
    Fraction['default'] = Fraction;
    Fraction['Fraction'] = Fraction;
    module['exports'] = Fraction;
  } else {
    root['Fraction'] = Fraction;
  }

})(this);

},{}],13:[function(require,module,exports){
var MathUtils = module.exports = {
	isOdd: function(num){
		return num & 1 === 1;
	},
	isEven: function(num){
		return num & 1 === 0;
	},

	powermod: function powermod(num, exp, mod){
		if(exp === 1) return num % mod;
		if(MathUtils.isOdd(exp)){
			return (num * powermod(num, exp-1, mod)) % mod;
		}
		return Math.pow(powermod(num, exp/2, mod), 2) % mod;
	},

	isPrime: function(num){
		return MathUtils.fastIsPrime(num) && MathUtils.slowIsPrime(num);
	},
	slowIsPrime: function(num){
		if(MathUtils.isEven(num)) return false;
		for(var i = 3, max = Math.sqrt(num); i < max; i += 2){
			if(num % i === 0) return false;
		}
		return true;
	},
	fastIsPrime: function(num){
		return MathUtils.powermod(3, num-1, num) === 1;
	},

	randomPrime: function(len){
		var num = Math.floor(Math.pow(10, len || 3) * Math.random());
		if(MathUtils.isEven(num)) num++;
		while(!MathUtils.isPrime(num)) num += 2;
		return num;
	},

	gcd: function gcd(a, b){
		if(b === 0) return a;
		return gcd(b, a % b);
	},
	egcd: function eea(a, b){
		if(b === 0) return [a, 1, 0];
		var tmp = eea(b, a % b);
		var ss = tmp[1],
			ts = tmp[2];
		return [tmp[0], ts, ss - Math.floor(a/b) * ts];
	},

	modularInverse: function(a, b){
		var arr = MathUtils.egcd(a, b);
		//if(arr[1] * a + arr[2] * b !== arr[0]) throw Error("Wrong EGCD: " + sum);
		return arr[1];
	}
};
},{}],14:[function(require,module,exports){
(function(root, factory) {
    if (typeof module === 'object' && module.exports) {
        module.exports = factory();
    } else {
        root.nearley = factory();
    }
}(this, function() {

    function Rule(name, symbols, postprocess) {
        this.id = ++Rule.highestId;
        this.name = name;
        this.symbols = symbols;        // a list of literal | regex class | nonterminal
        this.postprocess = postprocess;
        return this;
    }
    Rule.highestId = 0;

    Rule.prototype.toString = function(withCursorAt) {
        var symbolSequence = (typeof withCursorAt === "undefined")
                             ? this.symbols.map(getSymbolShortDisplay).join(' ')
                             : (   this.symbols.slice(0, withCursorAt).map(getSymbolShortDisplay).join(' ')
                                 + " ● "
                                 + this.symbols.slice(withCursorAt).map(getSymbolShortDisplay).join(' ')     );
        return this.name + " → " + symbolSequence;
    }


    // a State is a rule at a position from a given starting point in the input stream (reference)
    function State(rule, dot, reference, wantedBy) {
        this.rule = rule;
        this.dot = dot;
        this.reference = reference;
        this.data = [];
        this.wantedBy = wantedBy;
        this.isComplete = this.dot === rule.symbols.length;
    }

    State.prototype.toString = function() {
        return "{" + this.rule.toString(this.dot) + "}, from: " + (this.reference || 0);
    };

    State.prototype.nextState = function(child) {
        var state = new State(this.rule, this.dot + 1, this.reference, this.wantedBy);
        state.left = this;
        state.right = child;
        if (state.isComplete) {
            state.data = state.build();
            // Having right set here will prevent the right state and its children
            // form being garbage collected
            state.right = undefined;
        }
        return state;
    };

    State.prototype.build = function() {
        var children = [];
        var node = this;
        do {
            children.push(node.right.data);
            node = node.left;
        } while (node.left);
        children.reverse();
        return children;
    };

    State.prototype.finish = function() {
        if (this.rule.postprocess) {
            this.data = this.rule.postprocess(this.data, this.reference, Parser.fail);
        }
    };


    function Column(grammar, index) {
        this.grammar = grammar;
        this.index = index;
        this.states = [];
        this.wants = {}; // states indexed by the non-terminal they expect
        this.scannable = []; // list of states that expect a token
        this.completed = {}; // states that are nullable
    }


    Column.prototype.process = function(nextColumn) {
        var states = this.states;
        var wants = this.wants;
        var completed = this.completed;

        for (var w = 0; w < states.length; w++) { // nb. we push() during iteration
            var state = states[w];

            if (state.isComplete) {
                state.finish();
                if (state.data !== Parser.fail) {
                    // complete
                    var wantedBy = state.wantedBy;
                    for (var i = wantedBy.length; i--; ) { // this line is hot
                        var left = wantedBy[i];
                        this.complete(left, state);
                    }

                    // special-case nullables
                    if (state.reference === this.index) {
                        // make sure future predictors of this rule get completed.
                        var exp = state.rule.name;
                        (this.completed[exp] = this.completed[exp] || []).push(state);
                    }
                }

            } else {
                // queue scannable states
                var exp = state.rule.symbols[state.dot];
                if (typeof exp !== 'string') {
                    this.scannable.push(state);
                    continue;
                }

                // predict
                if (wants[exp]) {
                    wants[exp].push(state);

                    if (completed.hasOwnProperty(exp)) {
                        var nulls = completed[exp];
                        for (var i = 0; i < nulls.length; i++) {
                            var right = nulls[i];
                            this.complete(state, right);
                        }
                    }
                } else {
                    wants[exp] = [state];
                    this.predict(exp);
                }
            }
        }
    }

    Column.prototype.predict = function(exp) {
        var rules = this.grammar.byName[exp] || [];

        for (var i = 0; i < rules.length; i++) {
            var r = rules[i];
            var wantedBy = this.wants[exp];
            var s = new State(r, 0, this.index, wantedBy);
            this.states.push(s);
        }
    }

    Column.prototype.complete = function(left, right) {
        var copy = left.nextState(right);
        this.states.push(copy);
    }


    function Grammar(rules, start) {
        this.rules = rules;
        this.start = start || this.rules[0].name;
        var byName = this.byName = {};
        this.rules.forEach(function(rule) {
            if (!byName.hasOwnProperty(rule.name)) {
                byName[rule.name] = [];
            }
            byName[rule.name].push(rule);
        });
    }

    // So we can allow passing (rules, start) directly to Parser for backwards compatibility
    Grammar.fromCompiled = function(rules, start) {
        var lexer = rules.Lexer;
        if (rules.ParserStart) {
          start = rules.ParserStart;
          rules = rules.ParserRules;
        }
        var rules = rules.map(function (r) { return (new Rule(r.name, r.symbols, r.postprocess)); });
        var g = new Grammar(rules, start);
        g.lexer = lexer; // nb. storing lexer on Grammar is iffy, but unavoidable
        return g;
    }


    function StreamLexer() {
      this.reset("");
    }

    StreamLexer.prototype.reset = function(data, state) {
        this.buffer = data;
        this.index = 0;
        this.line = state ? state.line : 1;
        this.lastLineBreak = state ? -state.col : 0;
    }

    StreamLexer.prototype.next = function() {
        if (this.index < this.buffer.length) {
            var ch = this.buffer[this.index++];
            if (ch === '\n') {
              this.line += 1;
              this.lastLineBreak = this.index;
            }
            return {value: ch};
        }
    }

    StreamLexer.prototype.save = function() {
      return {
        line: this.line,
        col: this.index - this.lastLineBreak,
      }
    }

    StreamLexer.prototype.formatError = function(token, message) {
        // nb. this gets called after consuming the offending token,
        // so the culprit is index-1
        var buffer = this.buffer;
        if (typeof buffer === 'string') {
            var lines = buffer
                .split("\n")
                .slice(
                    Math.max(0, this.line - 5), 
                    this.line
                );

            var nextLineBreak = buffer.indexOf('\n', this.index);
            if (nextLineBreak === -1) nextLineBreak = buffer.length;
            var col = this.index - this.lastLineBreak;
            var lastLineDigits = String(this.line).length;
            message += " at line " + this.line + " col " + col + ":\n\n";
            message += lines
                .map(function(line, i) {
                    return pad(this.line - lines.length + i + 1, lastLineDigits) + " " + line;
                }, this)
                .join("\n");
            message += "\n" + pad("", lastLineDigits + col) + "^\n";
            return message;
        } else {
            return message + " at index " + (this.index - 1);
        }

        function pad(n, length) {
            var s = String(n);
            return Array(length - s.length + 1).join(" ") + s;
        }
    }

    function Parser(rules, start, options) {
        if (rules instanceof Grammar) {
            var grammar = rules;
            var options = start;
        } else {
            var grammar = Grammar.fromCompiled(rules, start);
        }
        this.grammar = grammar;

        // Read options
        this.options = {
            keepHistory: false,
            lexer: grammar.lexer || new StreamLexer,
        };
        for (var key in (options || {})) {
            this.options[key] = options[key];
        }

        // Setup lexer
        this.lexer = this.options.lexer;
        this.lexerState = undefined;

        // Setup a table
        var column = new Column(grammar, 0);
        var table = this.table = [column];

        // I could be expecting anything.
        column.wants[grammar.start] = [];
        column.predict(grammar.start);
        // TODO what if start rule is nullable?
        column.process();
        this.current = 0; // token index
    }

    // create a reserved token for indicating a parse fail
    Parser.fail = {};

    Parser.prototype.feed = function(chunk) {
        var lexer = this.lexer;
        lexer.reset(chunk, this.lexerState);

        var token;
        while (true) {
            try {
                token = lexer.next();
                if (!token) {
                    break;
                }
            } catch (e) {
                // Create the next column so that the error reporter
                // can display the correctly predicted states.
                var nextColumn = new Column(this.grammar, this.current + 1);
                this.table.push(nextColumn);
                var err = new Error(this.reportLexerError(e));
                err.offset = this.current;
                err.token = e.token;
                throw err;
            }
            // We add new states to table[current+1]
            var column = this.table[this.current];

            // GC unused states
            if (!this.options.keepHistory) {
                delete this.table[this.current - 1];
            }

            var n = this.current + 1;
            var nextColumn = new Column(this.grammar, n);
            this.table.push(nextColumn);

            // Advance all tokens that expect the symbol
            var literal = token.text !== undefined ? token.text : token.value;
            var value = lexer.constructor === StreamLexer ? token.value : token;
            var scannable = column.scannable;
            for (var w = scannable.length; w--; ) {
                var state = scannable[w];
                var expect = state.rule.symbols[state.dot];
                // Try to consume the token
                // either regex or literal
                if (expect.test ? expect.test(value) :
                    expect.type ? expect.type === token.type
                                : expect.literal === literal) {
                    // Add it
                    var next = state.nextState({data: value, token: token, isToken: true, reference: n - 1});
                    nextColumn.states.push(next);
                }
            }

            // Next, for each of the rules, we either
            // (a) complete it, and try to see if the reference row expected that
            //     rule
            // (b) predict the next nonterminal it expects by adding that
            //     nonterminal's start state
            // To prevent duplication, we also keep track of rules we have already
            // added

            nextColumn.process();

            // If needed, throw an error:
            if (nextColumn.states.length === 0) {
                // No states at all! This is not good.
                var err = new Error(this.reportError(token));
                err.offset = this.current;
                err.token = token;
                throw err;
            }

            // maybe save lexer state
            if (this.options.keepHistory) {
              column.lexerState = lexer.save()
            }

            this.current++;
        }
        if (column) {
          this.lexerState = lexer.save()
        }

        // Incrementally keep track of results
        this.results = this.finish();

        // Allow chaining, for whatever it's worth
        return this;
    };

    Parser.prototype.reportLexerError = function(lexerError) {
        var tokenDisplay, lexerMessage;
        // Planning to add a token property to moo's thrown error
        // even on erroring tokens to be used in error display below
        var token = lexerError.token;
        if (token) {
            tokenDisplay = "input " + JSON.stringify(token.text[0]) + " (lexer error)";
            lexerMessage = this.lexer.formatError(token, "Syntax error");
        } else {
            tokenDisplay = "input (lexer error)";
            lexerMessage = lexerError.message;
        }
        return this.reportErrorCommon(lexerMessage, tokenDisplay);
    };

    Parser.prototype.reportError = function(token) {
        var tokenDisplay = (token.type ? token.type + " token: " : "") + JSON.stringify(token.value !== undefined ? token.value : token);
        var lexerMessage = this.lexer.formatError(token, "Syntax error");
        return this.reportErrorCommon(lexerMessage, tokenDisplay);
    };

    Parser.prototype.reportErrorCommon = function(lexerMessage, tokenDisplay) {
        var lines = [];
        lines.push(lexerMessage);
        var lastColumnIndex = this.table.length - 2;
        var lastColumn = this.table[lastColumnIndex];
        var expectantStates = lastColumn.states
            .filter(function(state) {
                var nextSymbol = state.rule.symbols[state.dot];
                return nextSymbol && typeof nextSymbol !== "string";
            });

        if (expectantStates.length === 0) {
            lines.push('Unexpected ' + tokenDisplay + '. I did not expect any more input. Here is the state of my parse table:\n');
            this.displayStateStack(lastColumn.states, lines);
        } else {
            lines.push('Unexpected ' + tokenDisplay + '. Instead, I was expecting to see one of the following:\n');
            // Display a "state stack" for each expectant state
            // - which shows you how this state came to be, step by step.
            // If there is more than one derivation, we only display the first one.
            var stateStacks = expectantStates
                .map(function(state) {
                    return this.buildFirstStateStack(state, []) || [state];
                }, this);
            // Display each state that is expecting a terminal symbol next.
            stateStacks.forEach(function(stateStack) {
                var state = stateStack[0];
                var nextSymbol = state.rule.symbols[state.dot];
                var symbolDisplay = this.getSymbolDisplay(nextSymbol);
                lines.push('A ' + symbolDisplay + ' based on:');
                this.displayStateStack(stateStack, lines);
            }, this);
        }
        lines.push("");
        return lines.join("\n");
    }
    
    Parser.prototype.displayStateStack = function(stateStack, lines) {
        var lastDisplay;
        var sameDisplayCount = 0;
        for (var j = 0; j < stateStack.length; j++) {
            var state = stateStack[j];
            var display = state.rule.toString(state.dot);
            if (display === lastDisplay) {
                sameDisplayCount++;
            } else {
                if (sameDisplayCount > 0) {
                    lines.push('    ^ ' + sameDisplayCount + ' more lines identical to this');
                }
                sameDisplayCount = 0;
                lines.push('    ' + display);
            }
            lastDisplay = display;
        }
    };

    Parser.prototype.getSymbolDisplay = function(symbol) {
        return getSymbolLongDisplay(symbol);
    };

    /*
    Builds a the first state stack. You can think of a state stack as the call stack
    of the recursive-descent parser which the Nearley parse algorithm simulates.
    A state stack is represented as an array of state objects. Within a
    state stack, the first item of the array will be the starting
    state, with each successive item in the array going further back into history.

    This function needs to be given a starting state and an empty array representing
    the visited states, and it returns an single state stack.

    */
    Parser.prototype.buildFirstStateStack = function(state, visited) {
        if (visited.indexOf(state) !== -1) {
            // Found cycle, return null
            // to eliminate this path from the results, because
            // we don't know how to display it meaningfully
            return null;
        }
        if (state.wantedBy.length === 0) {
            return [state];
        }
        var prevState = state.wantedBy[0];
        var childVisited = [state].concat(visited);
        var childResult = this.buildFirstStateStack(prevState, childVisited);
        if (childResult === null) {
            return null;
        }
        return [state].concat(childResult);
    };

    Parser.prototype.save = function() {
        var column = this.table[this.current];
        column.lexerState = this.lexerState;
        return column;
    };

    Parser.prototype.restore = function(column) {
        var index = column.index;
        this.current = index;
        this.table[index] = column;
        this.table.splice(index + 1);
        this.lexerState = column.lexerState;

        // Incrementally keep track of results
        this.results = this.finish();
    };

    // nb. deprecated: use save/restore instead!
    Parser.prototype.rewind = function(index) {
        if (!this.options.keepHistory) {
            throw new Error('set option `keepHistory` to enable rewinding')
        }
        // nb. recall column (table) indicies fall between token indicies.
        //        col 0   --   token 0   --   col 1
        this.restore(this.table[index]);
    };

    Parser.prototype.finish = function() {
        // Return the possible parsings
        var considerations = [];
        var start = this.grammar.start;
        var column = this.table[this.table.length - 1]
        column.states.forEach(function (t) {
            if (t.rule.name === start
                    && t.dot === t.rule.symbols.length
                    && t.reference === 0
                    && t.data !== Parser.fail) {
                considerations.push(t);
            }
        });
        return considerations.map(function(c) {return c.data; });
    };

    function getSymbolLongDisplay(symbol) {
        var type = typeof symbol;
        if (type === "string") {
            return symbol;
        } else if (type === "object") {
            if (symbol.literal) {
                return JSON.stringify(symbol.literal);
            } else if (symbol instanceof RegExp) {
                return 'character matching ' + symbol;
            } else if (symbol.type) {
                return symbol.type + ' token';
            } else if (symbol.test) {
                return 'token matching ' + String(symbol.test);
            } else {
                throw new Error('Unknown symbol type: ' + symbol);
            }
        }
    }

    function getSymbolShortDisplay(symbol) {
        var type = typeof symbol;
        if (type === "string") {
            return symbol;
        } else if (type === "object") {
            if (symbol.literal) {
                return JSON.stringify(symbol.literal);
            } else if (symbol instanceof RegExp) {
                return symbol.toString();
            } else if (symbol.type) {
                return '%' + symbol.type;
            } else if (symbol.test) {
                return '<' + String(symbol.test) + '>';
            } else {
                throw new Error('Unknown symbol type: ' + symbol);
            }
        }
    }

    return {
        Parser: Parser,
        Grammar: Grammar,
        Rule: Rule,
    };

}));

},{}],15:[function(require,module,exports){
(function (global){(function (){
/*!
 * Number-To-Words util
 * @version v1.2.4
 * @link https://github.com/marlun78/number-to-words
 * @author Martin Eneqvist (https://github.com/marlun78)
 * @contributors Aleksey Pilyugin (https://github.com/pilyugin),Jeremiah Hall (https://github.com/jeremiahrhall),Adriano Melo (https://github.com/adrianomelo),dmrzn (https://github.com/dmrzn)
 * @license MIT
 */
!function(){"use strict";var e="object"==typeof self&&self.self===self&&self||"object"==typeof global&&global.global===global&&global||this,t=9007199254740991;function f(e){return!("number"!=typeof e||e!=e||e===1/0||e===-1/0)}function l(e){return"number"==typeof e&&Math.abs(e)<=t}var n=/(hundred|thousand|(m|b|tr|quadr)illion)$/,r=/teen$/,o=/y$/,i=/(zero|one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve)$/,s={zero:"zeroth",one:"first",two:"second",three:"third",four:"fourth",five:"fifth",six:"sixth",seven:"seventh",eight:"eighth",nine:"ninth",ten:"tenth",eleven:"eleventh",twelve:"twelfth"};function h(e){return n.test(e)||r.test(e)?e+"th":o.test(e)?e.replace(o,"ieth"):i.test(e)?e.replace(i,a):e}function a(e,t){return s[t]}var u=10,d=100,p=1e3,v=1e6,b=1e9,y=1e12,c=1e15,g=9007199254740992,m=["zero","one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"],w=["zero","ten","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"];function x(e,t){var n,r=parseInt(e,10);if(!f(r))throw new TypeError("Not a finite number: "+e+" ("+typeof e+")");if(!l(r))throw new RangeError("Input is not a safe number, it’s either too large or too small.");return n=function e(t){var n,r,o=arguments[1];if(0===t)return o?o.join(" ").replace(/,$/,""):"zero";o||(o=[]);t<0&&(o.push("minus"),t=Math.abs(t));t<20?(n=0,r=m[t]):t<d?(n=t%u,r=w[Math.floor(t/u)],n&&(r+="-"+m[n],n=0)):t<p?(n=t%d,r=e(Math.floor(t/d))+" hundred"):t<v?(n=t%p,r=e(Math.floor(t/p))+" thousand,"):t<b?(n=t%v,r=e(Math.floor(t/v))+" million,"):t<y?(n=t%b,r=e(Math.floor(t/b))+" billion,"):t<c?(n=t%y,r=e(Math.floor(t/y))+" trillion,"):t<=g&&(n=t%c,r=e(Math.floor(t/c))+" quadrillion,");o.push(r);return e(n,o)}(r),t?h(n):n}var M={toOrdinal:function(e){var t=parseInt(e,10);if(!f(t))throw new TypeError("Not a finite number: "+e+" ("+typeof e+")");if(!l(t))throw new RangeError("Input is not a safe number, it’s either too large or too small.");var n=String(t),r=Math.abs(t%100),o=11<=r&&r<=13,i=n.charAt(n.length-1);return n+(o?"th":"1"===i?"st":"2"===i?"nd":"3"===i?"rd":"th")},toWords:x,toWordsOrdinal:function(e){return h(x(e))}};"undefined"!=typeof exports?("undefined"!=typeof module&&module.exports&&(exports=module.exports=M),exports.numberToWords=M):e.numberToWords=M}();
}).call(this)}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})

},{}],16:[function(require,module,exports){
"use strict";

var primeFactor = {

  isPrime: function isPrime(num) {
    // Non integer or any number less than 2 is not prime
    if (!Number.isInteger(num) || num < 2) return false;
    // Even number: only prime if it is 2
    if (num % 2 === 0) return num === 2;
    // Odd number divisible by 3: only prime if it is 3
    if (num % 3 === 0) return num === 3;
    // Search for factor 5, 7, 11, 13, 17, 19, 23, 25, 29, 31, 35, 37...
    // up to and including square root of input number
    var floorSqrt = Math.floor(Math.sqrt(num));
    for (var i = 5; i <= floorSqrt; i += 6) {
      if (num % i === 0 || num % (i + 2) === 0) return false;
    }
    return true;
  },

  calculate: function calculate(inputNum) {
    var result = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : [];
    var repeat = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : true;

    if (!Number.isInteger(inputNum)) return result;
    var num = Math.abs(inputNum);
    if (num < 2) return result;
    var theSqrt = Math.sqrt(num);
    var x = 2;
    if (num % x) {
      x = 3;
      if (num % x) {
        x = 5;
        var add = 2;
        while (num % x && x < theSqrt) {
          // search numbers: 5, 7, 11, 13, 17, 19, 23...
          x += add;
          // add each time: 2, 4, 2, 4, 2, 4, 2...
          add = 6 - add;
        }
      }
    }

    x = x <= theSqrt ? x : num;

    if (!repeat) {
      var index = result.indexOf(x);
      if (index < 0) result.push(x);
    } else result.push(x);

    return x === num ? result : this.calculate(num / x, result, repeat);
  },

  getFactors: function getFactors(num) {
    return this.calculate(num, [], true);
  },

  getUniqueFactors: function getUniqueFactors(num) {
    return this.calculate(num, [], false);
  },

  getPrimeExponentObject: function getPrimeExponentObject(num) {
    var factors = this.calculate(num);
    var countObject = {};
    var _iteratorNormalCompletion = true;
    var _didIteratorError = false;
    var _iteratorError = undefined;

    try {
      for (var _iterator = factors[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
        var factor = _step.value;

        if (Number.isFinite(countObject[factor])) {
          countObject[factor] += 1;
        } else {
          countObject[factor] = 1;
        }
      }
    } catch (err) {
      _didIteratorError = true;
      _iteratorError = err;
    } finally {
      try {
        if (!_iteratorNormalCompletion && _iterator.return) {
          _iterator.return();
        }
      } finally {
        if (_didIteratorError) {
          throw _iteratorError;
        }
      }
    }

    return countObject;
  },

  getFrequency: function getFrequency(num) {
    var countObject = this.getPrimeExponentObject(num);
    var result = [];

    for (var key in countObject) {
      if ({}.hasOwnProperty.call(countObject, key)) {
        var obj = {
          factor: Number(key),
          times: countObject[key]
        };
        result.push(obj);
      }
    }
    return result;
  }

};

module.exports = primeFactor;
},{}]},{},[5])(5)
});
//# sourceMappingURL=microtonal-utils.js.map
