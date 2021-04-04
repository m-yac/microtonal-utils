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
  for (let g = 1; g <= 6; g++) {
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
    throw new Error ("input is not a prime interval greater than 3");
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
