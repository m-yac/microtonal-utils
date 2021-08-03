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
  * A specficiation of an FJS-like system.
  *
  * @constructor
  * @param {Interval} RoT the radius of tolerance
  * @param {GeneratorFunction.<Fraction>} fifthSeq the fifth sequence
  * @param {boolean} [hasNeutrals=false] whether this FJS permits neutral Pythagorean intervals
  * @param {boolean} [hasSemiNeutrals=false] whether this FJS permits semi-neutral Pythagorean intervals
  */
function FJSLike(RoT, fifthSeq, hasNeutrals, hasSemiNeutrals) {

  if (!(this instanceof FJSLike)) {
    return new FJSLike(RoT, fifthSeq, hasNeutrals, hasSemiNeutrals);
  }

  this.RoT = RoT;
  this.fifthSeq = fifthSeq;
  this.hasNeutrals = !!hasNeutrals;
  this.hasSemiNeutrals = !!hasSemiNeutrals;

}

/**
  * The radius of tolerance of the FJS, the interval `65/63` (about `54.11c`)
  *
  * @constant {Interval}
  */
const fjsRoT = Interval(65,63);

/**
  * The (infinite) fifth sequence of the FJS, `0, 1, -1, 2, -2, 3, -3, ...`
  *
  * @yields {Fraction}
  */
function* fjsFifthSeq() {
  yield Fraction(0);
  for (let g = 1; true; g++) {
    yield Fraction(g);
    yield Fraction(-g);
  }
}

/**
  * The specificaion of the standard FJS, using `fjsRoT`, `fjsFifthSeq`,
  * `hasNeutrals = false`, and `hasSemiNeutrals = false`
  *
  * @constant {FJSLike}
  */
const fjsSpec = FJSLike(fjsRoT, fjsFifthSeq, false, false);

/**
  * The radius of tolerance of the Neutral FJS, a pythagorean
  * semi-diminished second ("sd2", the interval exactly halfway between a
  * pythagorean "d2" and "m2", or about `33.38c`)
  *
  * @constant {Interval}
  */
const nfjsRoT = py.pyInterval(2,-1); // "sd2" ~= 33.38c

/**
  * The (finite) fifth sequence of the Neutral FJS,
  * `0, 1, -1, 2, -2, ..., 6, -6, 1/2, -1/2, 3/2, -3/2, ..., 11/2, -11/2`
  *
  * @yields {Fraction}
  */
function* nfjsFifthSeq() {
  yield Fraction(0);
  for (let g = 1; g <= 6; g++) {
    yield Fraction(g);
    yield Fraction(-g);
  }
  for (let g = 1; g <= 6; g++) {
    yield Fraction(2*g-1, 2);
    yield Fraction(1-2*g, 2);
  }
}

/**
  * The specification of the Neutral FJS, using `nfjsRoT`, `nfjsFifthSeq`,
  * `hasNeutrals = true`, and `hasSemiNeutrals = false`
  *
  * @constant {FJSLike}
  */
const nfjsSpec = FJSLike(nfjsRoT, nfjsFifthSeq, true, false);

/**
  * Divides the octave intro regions based on what fifth shift each interval
  * is assigned.
  *
  * @param {FJSLike} [spec=fjsSpec]
  * @returns {Array.<{lo:Interval, hi:Interval, fifthShift:Fraction, index:integer}>}
  */
function fjsRegions(spec) {
  if (!spec) {
    spec = fjsSpec;
  }
  else if (spec.RoT.compare(Interval(2).sqrt()) >= 0) {
    throw new Error("RoT too big");
  }
  else if (spec.RoT.compare(Interval(1)) <= 0) {
    throw new Error("RoT < 1");
  }

  let index = 0;
  let regions = [{lo: Interval(1), hi: Interval(2), fifthShift: undefined}];
  function addRegion(lo, hi, fifthShift) {
    if (lo.compare(Interval(1)) < 0) {
      addRegion(Interval(1), hi, fifthShift);
      addRegion(lo.mul(2), Interval(2), fifthShift);
    }
    else if (hi.compare(Interval(2)) > 0) {
      addRegion(Interval(1), hi.div(2), fifthShift);
      addRegion(lo, Interval(2), fifthShift);
    }
    else {
      for (const [i,r] of regions.entries()) {
        // for the first undefined region we intersect:
        if (!r.fifthShift && lo.compare(r.hi) < 0 && r.lo.compare(hi) < 0) {
          const lo_vs_rlo = lo.compare(r.lo);
          const hi_vs_rhi = hi.compare(r.hi);
          const maxlo = lo_vs_rlo > 0 ? lo : r.lo;
          const minhi = hi_vs_rhi < 0 ? hi : r.hi;
          // delete the current undefined region
          regions.splice(i, 1);
          // add the upper remainder of the undefined region, if it exists
          if (hi_vs_rhi < 0) {
            regions.splice(i, 0, {lo: minhi, hi: r.hi, fifthShift: undefined });
          }
          // add the new intersection region
          regions.splice(i, 0, {lo: maxlo, hi: minhi, fifthShift: fifthShift, index: index });
          index++;
          // add the lower remainder of the undefined region, if it exists
          if (lo_vs_rlo > 0) {
            regions.splice(i, 0, {lo: r.lo, hi: maxlo, fifthShift: undefined });
          }
          // add the upper remainder of the region we're adding, if it exists
          if (hi_vs_rhi > 0) {
            addRegion(r.hi, hi, fifthShift);
          }
          return;
        }
      }
    }
  }

  for (const g of spec.fifthSeq()) {
    const f = Interval(3,2).pow(g).red();
    addRegion(f.div(spec.RoT), f.mul(spec.RoT), g);
    // if every part of interval space it accounted for, we're done
    if (regions.every(r => r.fifthShift != undefined)) { break; }
  }
  return regions;
}

/**
  * Returns the FJS fifth shift associated to any interval.
  *
  * @param {Interval} i
  * @param {FJSLike} [spec=fjsSpec]
  * @returns {Fraction}
  */
function fjsFifthShift(a,b, spec) {
  // if only two arguments are given, the second one may be `spec`!
  if (!spec) {
    if (typeof b == 'object' && b != null) {
      spec = b;
      b = undefined;
    } else {
      spec = fjsSpec;
    }
  }
  const intv = Interval(a,b);
  for (const g of spec.fifthSeq()) {
    let c = intv.div(Interval(3,2).pow(g)).reb();
    if (c.compare(spec.RoT) < 0 && spec.RoT.recip().compare(c) < 0) {
      return g;
    }
  }
}

/**
  * Returns the FJS comma associated to a prime interval greater than 3
  * (i.e. 5, 7, 11, etc.)
  *
  * @param {integer} p
  * @param {FJSLike} [spec=fjsSpec]
  * @returns {Interval}
  */
function fjsComma(p, spec) {
  if (!spec) { spec = fjsSpec; }
  p = parseInt(p);
  if (!pf.isPrime(p) || p <= 3) {
    throw new Error ("input is not a prime interval greater than 3");
  }
  for (const g of spec.fifthSeq()) {
    let c = Interval(p).div(Interval(3,2).pow(g)).reb();
    if (c.compare(spec.RoT) < 0 && spec.RoT.recip().compare(c) < 0) {
      return c;
    }
  }
}

/**
  * Given an interval, returns the product of the FJS commas associated to each
  * of its prime factors raised to the exponents of those prime factors
  *
  * @param {Interval} k
  * @param {FJSLike} [spec=fjsSpec]
  * @returns {Interval}
  */
function fjsFactor(a,b, spec) {
  // if only two arguments are given, the second one may be `spec`!
  if (!spec) {
    if (typeof b == 'object' && b != null) {
      spec = b;
      b = undefined;
    } else {
      spec = fjsSpec;
    }
  }
  const k = Interval(a,b);
  let ret = Interval(1);
  for (const [p,e] of k.factors()) {
    ret = ret.mul(fjsComma(p,spec).pow(e));
  }
  return ret;
}

/**
  * Returns the string of FJS accidentals for the given interval, as well as
  * the pythagorean interval which when applied to these accidentals
  * results in the given interval.
  *
  * @param {Interval} i
  * @param {FJSLike} [spec=fjsSpec]
  * @returns {{ accStr: string, pyi: Interval }}
  */
function fjsAccidentals(a,b, spec) {
  // if only two arguments are given, the second one may be `spec`!
  if (!spec) {
    if (typeof b == 'object' && b != null) {
      spec = b;
      b = undefined;
    } else {
      spec = fjsSpec;
    }
  }
  const i = Interval(a,b);
  let pyi = i;
  let otos = [];
  let utos = [];
  for (let [p,e] of i.factors()) {
    if (p != 2 && p != 3) {
      pyi = pyi.div(fjsComma(p,spec).pow(e));
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
  const modulus = spec.hasSemiNeutrals ? 1 : spec.hasNeutrals ? 2 : 4;
  if (py.isPythagorean(pyi) && py.pyGenerator(pyi) % modulus == 0) {
    const otoStr = otos.length == 0 ? "" : "^" + otos.join(",");
    const utoStr = utos.length == 0 ? "" : "_" + utos.join(",");
    return { otos: otos, utos: utos, accStr: otoStr + utoStr, pyi: pyi };
  }
}

/**
  * Returns the FJS symbol of the given interval, or undefined if no such symbol
  * exists
  *
  * @param {Interval} i
  * @param {FJSLike} [spec=fjsSpec]
  * @returns {string}
  */
function fjsSymb(a,b, spec) {
  const res = fjsAccidentals(a,b, spec);
  if (res) {
    return py.pySymb(res.pyi) + res.accStr;
  }
}

/**
  * Returns the FJS note name of the given interval to A4, or undefined if no
  * such name exists
  *
  * @param {Interval} i
  * @param {FJSLike} [spec=fjsSpec]
  * @returns {string}
  */
function fjsNote(a,b, spec) {
  const res = fjsAccidentals(a,b, spec);
  if (res) {
    return py.pyNote(res.pyi) + res.accStr;
  }
}

module['exports'].FJSLike = FJSLike;
module['exports'].fjsRoT = fjsRoT;
module['exports'].fjsFifthSeq = fjsFifthSeq;
module['exports'].fjsSpec = fjsSpec;
module['exports'].nfjsRoT = nfjsRoT;
module['exports'].nfjsFifthSeq = nfjsFifthSeq;
module['exports'].nfjsSpec = nfjsSpec;
module['exports'].fjsRegions = fjsRegions;
module['exports'].fjsFifthShift = fjsFifthShift;
module['exports'].fjsComma = fjsComma;
module['exports'].fjsFactor = fjsFactor;
module['exports'].fjsAccidentals = fjsAccidentals;
module['exports'].fjsSymb = fjsSymb;
module['exports'].fjsNote = fjsNote;
