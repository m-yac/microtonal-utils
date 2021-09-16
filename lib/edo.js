/**
 * Functions for working with intervals in an EDO
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module edo
 **/

const {mod} = require('./utils.js');
const {gcd, egcd} = require('mathutils');
const pf = require('primes-and-factors');
const Fraction = require('fraction.js');
const Interval = require('./interval.js');
const py = require('./pythagorean.js');

/**
  * Returns the EDO step closest to the given interval
  *
  * @param {integer} edo
  * @param {Interval} i
  * @returns {integer}
  */
function edoApprox(edo,a,b) {
  return Math.round(edo * Interval(a,b).valueOf_log());
}


let edoPrimeApprox_var = {};

/**
  * Returns the EDO step closest to the given prime interval, and its error
  *
  * @param {integer} edo
  * @param {integer} p
  * @returns {Pair.<integer,Interval>}
  */
function edoPrimeApprox(edo, p) {
  if (edoPrimeApprox_var[[edo,p]]) {
    return edoPrimeApprox_var[[edo,p]];
  }
  if (!pf.isPrime(p)) {
    throw new Error("Input to edoPrimeApprox is not a prime");
  }

  const n = edoApprox(edo, p);
  const diff = Interval(2).pow(n,edo).div(p);

  if (!edoPrimeApprox_var[edo]) { edoPrimeApprox_var[edo] = {}; }
  edoPrimeApprox_var[edo][p] = [n,diff];
  return [n,diff];
}

/**
  * Returns the EDO step which corresponds to the given interval using the EDO's
  * prime mappings (`edoPrimeApprox`) as well as the log values of the total
  * prime error and max prime error
  *
  * @param {integer} edo
  * @param {Interval} i
  * @returns {{n: integer, err: number, max_prime_err: number}}
  */
function edoApproxConsistentWithErr(edo,a,b) {
  const i = Interval(a,b);
  let ret = { n: 0, err: 0, max_prime_err: 0 };
  for (const [p,e] of i.factors()) {
    const [p_n, p_err] = edoPrimeApprox(edo, p);
    ret.n += e.s * e.n * p_n;
    const prime_err = e.n * Math.abs(p_err.valueOf_log());
    ret.err += prime_err;
    ret.max_prime_err = Math.max(prime_err, ret.max_prime_err);
  }
  return ret;
}

/**
  * Returns the EDO step which corresponds to the given interval using the EDO's
  * prime mappings (`edoPrimeApprox`)
  *
  * @param {integer} edo
  * @param {Interval} i
  * @returns {integer}
  */
function edoApproxConsistent(edo,a,b) {
  return edoApproxConsistentWithErr(edo,a,b).n;
}

/**
  * Returns true iff the given intervals is inconsistent in the given EDO, i.e.
  * if `edoApprox` and `edoApproxConsistent` give different values
  *
  * @param {integer} edo
  * @param {Interval} i
  * @returns {integer}
  */
function edoIntvIsConsistent(edo,a,b) {
  const i = Interval(a,b);
  return edoApprox(edo,i) == edoApproxConsistent(edo,i);
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
  const p = py.pyOctaves(i);
  if (g.d != 1) {
    throw new Error(edo + "-EDO has no " + py.pySymb(i,{verbosity:1}) + " interval");
  }
  return g.s*g.n + p * edo;
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
    steps[0].push([0,0]);
  }
  for (let k = 1; k <= Math.max(Math.abs(lo), Math.abs(hi)); k++) {
    if (lo <=  k &&  k <= hi) { steps[mod( k*g,edo)].push([0, k]); }
    if (lo <= -k && -k <= hi) { steps[mod(-k*g,edo)].push([0,-k]); }
  }
  return steps;
}

// The input to this function is an array of only the basic intervals,
//  usually only those with no ups or downs, but in the case of odd-EDOs,
//  up-mid and down-mid intervals as well. This function fills in the rest of
//  the steps with intervals with ups and downs added appropriately
// (Used in `updnsSymb` and `updnsNote`)
function addUpdns(edo, steps) {
  let new_steps = steps.map((_,i) => [...steps[i]]);
  let [last_below, last_above] = [0,edo];
  for (let i = 0; i < edo; i++) {
    // if the current step is empty or an up-mid or down-mid interval, add
    //  everything from last_below (with the appropriate number of ups)
    if (steps[i].length == 0 || (steps[i].length == 1 && steps[i][0][0] != 0)) {
      for (const [n,k] of steps[last_below]) {
        const diff = i - last_below;
        // only add if n is zero or the sign of n and diff match, this prevents
        //  up-mid and down-mid intervals from turning into mid intervals
        if (n * diff >= 0) {
          new_steps[i].push([n + diff, k]);
        }
      }
    }
    if (steps[i].length != 0) {
      last_below = i;
    }
    const j = (edo-1)-i;
    // if the current step is empty or an up-mid or down-mid interval, add
    //  everything from last_above (with the appropriate number of downs)
    if (steps[j].length == 0 || (steps[j].length == 1 && steps[j][0][0] != 0)) {
      for (const [n,k] of steps[mod(last_above,edo)]) {
        const diff = j - last_above;
        // only add if n is zero or the sign of n and diff match, this prevents
        //  up-mid and down-mid intervals from turning into mid intervals
        if (n * diff >= 0) {
          new_steps[j].push([n + diff, k]);
        }
      }
    }
    if (steps[j].length != 0) {
      last_above = j;
    }
  }
  for (let i = 0; i < edo; i++) {
    let [minUpdns, minUpdnsIsNeutral] = [edo, false];
    for (const [uds, k] of new_steps[i]) {
      const abs_uds = Math.abs(uds);
      if (abs_uds < minUpdns) {
        minUpdns = abs_uds;
        minUpdnsIsNeutral = !Number.isInteger(k);
      }
      if (abs_uds == minUpdns) {
        minUpdnsIsNeutral &= !Number.isInteger(k);
      }
    }
    new_steps[i] = new_steps[i].filter(udsk => Math.abs(udsk[0]) <= minUpdns
                                               && (minUpdnsIsNeutral || Number.isInteger(udsk[1])))
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
  if (edo <= 0) { return [] }
  // We treat 1, 2, 3, 4, 6, and 8-EDO as subsets of 24-EDO
  if ([1,2,3,4,6,8].includes(edo)) {
    const [cache, factor] = [updnsSymbCache(24), 24/edo];
    const steps = [...Array(edo).keys()].map(i => cache[i*factor]);
    upsdnsSymbCache_var[edo] = steps;
    return steps;
  }
  let fifth = edoApprox(edo,3,2);
  // We use the second-best fifth for 13-EDO and 18-EDO
  if (edo == 13 || edo == 18) { fifth--; }
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
    for (const k of [1/2, -1/2, 3/2, -3/2, 5/2, -5/2]) {
      const i = mod(Math.floor(k*fifth), edo);
      if (steps[i].length == 0) { steps[i].push([-1,k]); }
      const j = mod(Math.ceil(k*fifth), edo);
      if (steps[j].length == 0) { steps[j].push([1,k]); }
    }
  } else {
    steps = fillGens(edo, fifth/2, 2*lo, 2*hi);
    for (let i = 0; i < edo; i++) {
      steps[i] = steps[i].filter(([n,k]) => k % 2 == 0 || Math.abs(k) <= 6)
                         .map(([n,k]) => [n,k/2]);
    }
  }
  steps = cvtGensToPy(edo, addUpdns(edo, steps));
  upsdnsSymbCache_var[edo] = steps;
  return steps;
}

function fmtUpdnsSymb(uds, pyi, opts) {
  let {verbosity, maxTupleWord, usePerfEDONotation} = opts || {};
  if (verbosity == undefined) { verbosity = 0; }
  if (maxTupleWord == undefined) { maxTupleWord = 2; }
  const uds_abs = Math.abs(uds);
  let [uds_str, py_str] = ["", py.pySymb(pyi, {verbosity: verbosity})];
  if (verbosity == 0) {
    uds_str = (uds > 0 ? '^' : 'v').repeat(uds_abs);
    if (usePerfEDONotation) { py_str = py_str.replace("n", "P"); }
    if (uds_abs > 0) { py_str = py_str.replace("P", ""); }
    py_str = py_str.replace("n", "~").replace("sA", "~").replace("sd", "~");
  }
  else {
    const uds_suffix = uds > 0 ? "up" : "down";
    if (uds_abs == 0) {
      uds_str = "";
    }
    else if (uds_abs == 1) {
      uds_str = uds_suffix;
    }
    else if (uds_abs <= Math.min(maxTupleWord, 12)) {
      uds_str = ["double", "triple", "quadruple", "quintuple", "sextuple",
                 "septuple", "octuple", "nonuple", "decuple", "undecuple",
                 "duodecuple"][uds_abs-2] + "-" + uds_suffix + " ";
    }
    else {
      uds_str = uds_abs + "-" + uds_suffix + " ";
    }
    if (usePerfEDONotation) { py_str = py_str.replace("neutral", "perfect"); }
    if (uds_abs == 1) { py_str = py_str.replace("perfect ", " "); }
    if (uds_abs >= 2) { py_str = py_str.replace("perfect ", ""); }
    py_str = py_str.replace("neutral", "mid");
    if (verbosity == 1) {
      py_str = py_str.replace("semi-aug", "mid")
                     .replace("semi-dim", "mid");
    }
    else {
      py_str = py_str.replace("semi-augmented", "mid")
                     .replace("semi-diminished", "mid");
    }
  }
  return uds_str + py_str;
}

/**
  * Returns the ups-and-downs notation symbol for the given steps in the given
  * EDO
  *
  * @param {integer} edo
  * @param {integer} n
  * @param {Object=} opts
  * @param {integer=} opts.verbosity verbosity can be the default 0
  *                                  (e.g. "^^M3"), 1 (e.g. "3-up major 3rd"),
  *                                  or 2 (e.g. "triple-up major third")
  * @param {integer=} opts.maxTupleWord default is 2, maximum is 12
  * @param {boolean=} opts.useWordNegative defaults to false
  * @param {boolean=} opts.useWordDesc defaults to false
  * @param {boolean=} opts.useNeutNotationForPerfEDOs defaults to false
  * @returns {string}
  */
function updnsSymb(edo,n, opts) {
  let {verbosity, maxTupleWord, useWordDesc, useNeutNotationForPerfEDOs} = opts || {};
  if (verbosity == undefined) { verbosity = 0; }
  if (maxTupleWord == undefined) { maxTupleWord = 2; }
  const usePerfEDONotation = edo % 7 == 0 && !useNeutNotationForPerfEDOs;

  if (useWordDesc && n < 0) {
    pre_str = verbosity == 0 ? "desc. " : "descending ";
    return updnsSymb(edo, -n, opts).map(s => pre_str + s);
  }

  const nr = mod(n,edo);
  const vs = Interval(2).pow(n - nr, edo);
  const cache = updnsSymbCache(edo)[nr];
  const optsToPass = Object.assign({}, opts, {usePerfEDONotation: usePerfEDONotation});
  return cache.map(([uds, pyi_red]) => fmtUpdnsSymb(uds, pyi_red.mul(vs), optsToPass));
}

/**
  * Given the number of ups-and-downs and a Pythagorean interval, returns the
  * corresponding number of steps in ups-and-downs notation
  *
  * @param {integer} edo
  * @param {integer} uds
  * @param {Interval} pyi
  * @returns {integer}
  */
function updnsFromSymb(edo, uds, pyi) {
  // We treat 1, 2, 3, 4, 6, and 8-EDO as subsets of 24-EDO
  if ([1,2,3,4,6,8].includes(edo)) {
    const [n, factor] = [updnsFromSymb(24, uds, pyi), 24/edo];
    if (n % factor != 0) {
      throw new Error(edo + "-EDO has no " + fmtUpdnsSymb(uds, pyi) + " interval");
    }
    return n / factor;
  }
  let fifth = edoApprox(edo,3,2);
  // We use the second-best fifth for 13-EDO and 18-EDO
  if (edo == 13 || edo == 18) { fifth--; }
  const g = Fraction(py.pyGenerator(pyi) * fifth, 4);
  const p = py.pyOctaves(pyi);
  if (g.d == 2 && fifth % 2 != 0) {
    if (uds == 0) {
      throw new Error(edo + "-EDO has no " + fmtUpdnsSymb(uds, pyi) +
                      " interval (but it does have " + fmtUpdnsSymb(1, pyi) +
                      " and " + fmtUpdnsSymb(-1,pyi) + " intervals)");
    }
    return (uds-Math.sign(uds)) +
           (uds < 0 ? Math.floor(g.valueOf()) : Math.ceil(g.valueOf())) +
           p * edo;
  }
  if (g.d != 1) {
    throw new Error(edo + "-EDO has no " + fmtUpdnsSymb(uds, pyi) + " interval");
  }
  return uds + g.s*g.n + p * edo;
}

let upsdnsNoteCache_var = {};

function updnsNoteCache(edo) {
  if (upsdnsNoteCache_var[edo]) {
    return upsdnsNoteCache_var[edo];
  }
  if (edo <= 0) { return [] }
  // We treat 1, 2, 3, 4, 6, and 8-EDO as subsets of 24-EDO
  if ([1,2,3,4,6,8].includes(edo)) {
    const [cache, factor] = [updnsNoteCache(24), 24/edo];
    const steps = [...Array(edo).keys()].map(i => cache[i*factor]);
    upsdnsSymbCache_var[edo] = steps;
    return steps;
  }
  let fifth = edoApprox(edo,3,2);
  // We use the second-best fifth for 13-EDO and 18-EDO
  if (edo == 13 || edo == 18) { fifth--; }
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
  for (const [uds, pyi_red] of cache) {
    const updns = (uds > 0 ? '^' : 'v').repeat(Math.abs(uds));
    const str = updns + py.pyNote(pyi_red.mul(vs), useASCII);
    ret.push(str);
  }
  return ret;
}

module['exports'].edoApprox = edoApprox;
module['exports'].edoPrimeApprox = edoPrimeApprox;
module['exports'].edoApproxConsistentWithErr = edoApproxConsistentWithErr;
module['exports'].edoApproxConsistent = edoApproxConsistent;
module['exports'].edoIntvIsConsistent = edoIntvIsConsistent;
module['exports'].edoPy = edoPy;
module['exports'].edoPyInv = edoPyInv;
module['exports'].edoPyComma = edoPyComma;
module['exports'].edoHasNeutrals = edoHasNeutrals;
module['exports'].edoHasSemiNeutrals = edoHasSemiNeutrals;
module['exports'].updnsSymbCache = updnsSymbCache;
module['exports'].fmtUpdnsSymb = fmtUpdnsSymb;
module['exports'].updnsSymb = updnsSymb;
module['exports'].updnsFromSymb = updnsFromSymb;
module['exports'].updnsNoteCache = updnsNoteCache;
module['exports'].updnsNote = updnsNote;
