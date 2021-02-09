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
  * The radius of tolerance of the FJS + Neutrals, a pythagorean
  * semi-diminished second ("sd2", the interval exactly halfway between a
  * pythagorean "d2" and "m2", or about `33.38c`)
  *
  * @constant {Interval}
  */
const fjsnRoT = py.pyInterval(2,-1); // "sd2" ~= 33.38c

/**
  * The (finite) fifths sequence of the FJS + Neutrals,
  * `0, 1, -1, 2, -2, ..., 6, -6, 1/2, -1/2, 3/2, -3/2, ..., 11/2, -11/2`
  *
  * @yields {Fraction}
  */
function* fjsnFifthsSeq() {
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
  * The parameters of the FJS + Neutrals, `fjsnRoT`, `fjsnFifthsSeq`, and
  * `hasNeutrals = true`
  *
  * @constant {{RoT: Fraction, fifthSeq: Fraction, hasNeutrals: boolean}}
  */
const fjsnParams = { RoT: fjsnRoT, fifthsSeq: fjsnFifthsSeq, hasNeutrals: true };

/**
  * Returns the FJS comma associated to a prime interval greater than 3
  * (i.e. 5, 7, 11, etc.)
  *
  * @param {integer} p
  * @param {{RoT: Fraction, fifthSeq: Fraction}} [params=fjsParams]
  * @returns {boolean}
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
  * @returns {boolean}
  */
function fjsFactor(a,b, params) {
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
  const otoStr = otos.length == 0 ? "" : "^" + otos.join(",");
  const utoStr = utos.length == 0 ? "" : "_" + utos.join(",");
  return { accStr: otoStr + utoStr, pyi: pyi };
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
  if (!params) {
    if (typeof b == 'object' && b != null) {
      params = b;
      b = undefined;
    } else {
      params = fjsParams;
    }
  }
  const {accStr, pyi} = fjsAccidentals(a,b, params);
  const modulus = params.hasNeutrals ? 2 : 4;
  // If, after applying all the accidentals, the result is a permitted
  // pythagorean interval then an FJS symbol exists for this interval
  if (py.isPythagorean(pyi) && py.generator(pyi) % modulus == 0) {
    return py.pySymb(pyi) + accStr;
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
  if (!params) {
    if (typeof b == 'object' && b != null) {
      params = b;
      b = undefined;
    } else {
      params = fjsParams;
    }
  }
  const {accStr, pyi} = fjsAccidentals(a,b, params);
  const modulus = params.hasNeutrals ? 2 : 4;
  // If, after applying all the accidentals, the result is a permitted
  // pythagorean interval then an FJS symbol exists for this interval
  if (py.isPythagorean(pyi) && py.generator(pyi) % modulus == 0) {
    return py.pyNote(pyi) + accStr;
  }
}

module['exports'].fjsRoT = fjsRoT;
module['exports'].fjsFifthsSeq = fjsFifthsSeq;
module['exports'].fjsParams = fjsParams;
module['exports'].fjsnRoT = fjsnRoT;
module['exports'].fjsnFifthsSeq = fjsnFifthsSeq;
module['exports'].fjsnParams = fjsnParams;
module['exports'].fjsComma = fjsComma;
module['exports'].fjsFactor = fjsFactor;
module['exports'].fjsSymb = fjsSymb;
module['exports'].fjsNote = fjsNote;
