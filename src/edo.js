/**
 * @module edo.js
 * Copyright (c) 2020, Matthew Yacavone (matthew [at] yacavone [dot] net)
 **/

(function(root) {

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
  * @param {Integer} edo
  * @param {Interval} i
  * @returns {Integer}
  */
function edoApprox(edo,a,b) {
  return Math.round(edo * Interval(a,b).toCents() / 1200);
}

/**
  * Returns the EDO step which corresponds to the given pythagorean interval,
  * where a P5 corresponds to `edoApprox(edo,Interval(3,2))`
  *
  * @param {Integer} edo
  * @param {Interval} i
  * @returns {Integer}
  */
function edoPy(edo,a,b) {
  const i = Interval(a,b);
  const g = Fraction(py.generator(i) * edoApprox(edo,3,2), 4);
  const v = py.octaves(i);
  if (g.d != 1) {
    throw "neutral pythagorean interval not realized in this EDO";
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
  * @param {Integer} edo
  * @param {Integer} n
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
  * @param {Integer} edo
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
  * @param {Integer} edo
  * @returns {boolean}
  */
function edoHasNeutrals(edo) {
  return edoApprox(edo,3,2) % 2 == 0;
}

/**
  * Checks whether semi-neutral pythagorean intervals are realized in the given
  * EDO, i.e. if `edoApprox(edo,Interval(3,2))` is divisible by 4
  *
  * @param {Integer} edo
  * @returns {boolean}
  */
function edoHasSemiNeutrals(edo) {
  return edoApprox(edo,3,2) % 4 == 0;
}

let fifthGensCache = {};

function fifthGens(edo,n) {
  if (fifthGensCache[edo]) {
    return fifthGensCache[edo];
  }
  var steps = [];
  for (let i = 0; i < edo; i++) { steps.push(Array(0)); }
  const fifth = edoApprox(edo,3,2);
  // non-neutral intervals
  var g = 0;
  while (g != -6) {
    steps[mod(fifth*g,edo)].push(g*4);
    g = g > 0 ? -g : -g+1;
  }
  // neutral intervals
  if (fifth % 2 == 0) {
    g = 1;
    while (g != 7) {
      steps[mod(fifth*g/2,edo)].push(g*2);
      g = g > 0 ? -g : -g+2;
    }
  }
  fifthGensCache[edo] = steps;
  return n ? steps[mod(n,edo)] : steps;
}

/**
  * Returns the ups-and-downs notation for the given steps in the given EDO
  *
  * @param {Integer} edo
  * @param {Integer} n
  * @returns {String}
  */
function updnsSymb(edo,n) {
  const vs = Interval(2).pow(Math.floor(n / edo));
  const nr = mod(n,edo);
  const gs = fifthGens(edo);
  var ret;
  for (let i = 0; !ret; i++) {
    const gup = gs[mod(nr-i,edo)];
    var gupSymb;
    if (gup.length > 0) {
      gupSymb = "^".repeat(i) + py.pySymb(Interval(3,2).pow(gup[0],4).red().mul(vs));
    }
    const gdn = gs[mod(nr+i,edo)];
    var gdnSymb;
    if (gdn.length > 0) {
      gdnSymb = "v".repeat(i) + py.pySymb(Interval(3,2).pow(gdn[0],4).red().mul(vs));
    }
    // prefer non-neutral symbols over neutral ones
    if (gupSymb && mod(gup[0],4) == 0) { ret = gupSymb; }
    else if (gdnSymb && mod(gdn[0],4) == 0) { ret = gdnSymb; }
    else if (gupSymb) { ret = gupSymb; }
    else if (gdnSymb) { ret = gdnSymb; }
  }
  return ret.replace("n","~").replace("sA","~").replace("sd","~");
}

module['exports'].edoApprox = edoApprox;
module['exports'].edoPy = edoPy;
module['exports'].edoPyInv = edoPyInv;
module['exports'].edoPyComma = edoPyComma;
module['exports'].edoHasNeutrals = edoHasNeutrals;
module['exports'].edoHasSemiNeutrals = edoHasSemiNeutrals;
module['exports'].fifthGens = fifthGens;
module['exports'].updnsSymb = updnsSymb;

})(this);
