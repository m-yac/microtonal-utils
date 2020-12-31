/**
 * @module edo.js
 * Copyright (c) 2020, Matthew Yacavone (matthew [at] yacavone [dot] net)
 **/

(function(root) {

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

function fifthGens(edo) {
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
  return steps
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
module['exports'].edoHasNeutrals = edoHasNeutrals;
module['exports'].edoHasSemiNeutrals = edoHasSemiNeutrals;
module['exports'].updnsSymb = updnsSymb;

})(this);
