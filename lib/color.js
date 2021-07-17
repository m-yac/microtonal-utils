/**
 * Color notation for intervals
 * Based on: https://en.xen.wiki/w/Color_notation
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module color
 **/

const {mod} = require('./utils.js')
const {gcd} = require('mathutils');
const pf = require('primes-and-factors');
const Fraction = require('fraction.js');
const Interval = require('./interval.js');
const {pyDegreeString, pyNote} = require('./pythagorean.js');

/**
  * The regions of the octave used when determining the degree of a prime in
  * color notation
  *
  * @constant {Array.<Interval>}
  */
const colorRegions = [ Interval([Fraction( 1,24)]) /*    0c -  50c */
                     , Interval([Fraction( 5,24)]) /*   50c - 250c */
                     , Interval([Fraction( 9,24)]) /*  250c - 450c */
                     , Interval([Fraction(12,24)]) /*  450c - 600c */
                     , Interval([Fraction(15,24)]) /*  600c - 750c */
                     , Interval([Fraction(19,24)]) /*  750c - 950c */
                     , Interval([Fraction(23,24)]) /*  950c -1150c */
                     , Interval([Fraction(24,24)]) /* 1150c -1200c */ ];

let colorCache_var = {};

/**
  * Returns the "zeroed" degree of the given prime in color notation
  *
  * @param {integer} p a prime
  * @returns {integer}
  */
function colorPrimeZDegree(p) {
  if (colorCache_var[p]) {
    return colorCache_var[p];
  }
  if (!pf.isPrime(p)) {
    throw new Error("colorPrimeZDegree not given a prime");
  }
  const i = Interval(p).red();
  const dNo2 = colorRegions.findIndex(hi => i.compare(hi) < 0);
  const d = dNo2 - 7 * i.div(p).valueOf_log()
  colorCache_var[p] = d;
  return d;
}

/**
  * Returns the "zeroed" degree of a the given interval in color notation
  *
  * @param {Interval} i
  * @returns {integer}
  */
function colorZDegree(a,b) {
  const i = Interval(a,b);
  let zd = 0;
  for (const [p,e] of i.factors()) {
    zd += colorPrimeZDegree(p) * e.valueOf();
  }
  return zd;
}

/**
  * Returns the degree of a the given interval in color notation.
  *
  * @param {Interval} i
  * @returns {integer}
  */
function colorDegree(a,b) {
  const zd = colorZDegree(a,b);
  return zd == 0 ? 1 : zd + Math.sign(zd);
}

/**
  * Returns the magnitude of a the given interval in color notation
  *
  * @param {Interval} i
  * @returns {integer}
  */
function colorMagnitude(a,b) {
  const i = Interval(a,b);
  let sum = Fraction(0);
  for (const [p,e] of i.factors()) {
    if (p > 2) {
      sum = sum.add(e);
    }
  }
  return Math.round(sum.valueOf() / 7);
}

/**
  * Returns the prefix of the given prime in color notation. If v = "o", return
  * the otonal prefix (e.g. yo). If v = "u", return the utonal prefix (e.g. gu).
  * If v = "e", return the multi prefix (e.g. quin);
  *
  * @param {integer} p a prime
  * @param {string} v either "e", "o", or "u"
  * @param {boolean} [abbreviate=false]
  * @returns {string}
  */
function colorPrimePrefix(p, v, abbreviate) {
  if (v != "e" && v != "o" && v != "u") {
    throw new Error("Invalid vowel passed to colorPrimePrefix");
  }
  if (p == 2) {
    if (v == "e") { return "bi"; }
    return "";
  }
  if (p == 3) {
    if (v == "e") { return "tri"; }
    return "w" + (abbreviate ? "" : "a");
  }
  if (p == 5) {
    if (v == "e") { return "quin"; }
    if (v == "o") { return "y" + (abbreviate ? "" : v); }
    if (v == "u") { return "g" + (abbreviate ? "" : v); }
  }
  if (p == 7) {
    if (v == "e") { return "sep"; }
    if (v == "o") { return "z" + (abbreviate ? "" : v); }
    if (v == "u") { return "r" + (abbreviate ? "" : v); }
  }
  if (abbreviate && v != "e") {
    if (p == 11) { return "1" + v; }
    if (p == 13) { return "3" + v; }
    return p + v;
  }
  else {
    if (p == 11) { return "l" + v; }
    if (p > 67) {
      throw new Error("Prime larger than 67 passed to colorPrimePrefix");
    }
    const [tens, ones] = [Math.floor(p / 10), p % 10];
    return { 1: "", 2: "twe", 3: "thi", 4: "fo", 5: "fi", 6: "si"}[tens] +
           { 1: "w", 3: "th", 7: "s", 9: "n" }[ones] + v;
  }
}

/**
  * Returns the multi prefix of the positive integer in color notation (e.g.
  * "quinbi" for 10).
  *
  * @param {integer} n
  * @returns {string}
  */
function colorMultiPrefix(n) {
  const fs = pf.getPrimeExponentObject(n);
  let fs_arr = [];
  if (fs[2] % 2) { fs_arr.push([2, fs[2] % 2]); }
  if (fs[3])     { fs_arr.push([3, fs[3]]); }
  if (fs[2] > 1) { fs_arr.push([4, Math.floor(fs[2]/2)]); }
  delete fs[2];
  delete fs[3];
  fs_arr = fs_arr.concat(Object.entries(fs));
  let res = "";
  for (const [p,e] of fs_arr) {
    const prefix = p == 4 ? "quad" : colorPrimePrefix(p, "e");
    res = prefix.repeat(e.valueOf()) + res;
  }
  return res;
}

/**
  * Returns the otonal/utonal prefix of the given prime power in color notation
  *
  * @param {integer} p a prime
  * @param {integer} e the prime exponent
  * @param {boolean} [abbreviate=false]
  * @returns {string}
  */
function colorFactorPrefix(p, e, verbosity) {
  if (e == 0) { return ""; }
  const base = colorPrimePrefix(p, e > 0 ? "o" : "u", verbosity == 0);
  const eAbs = Math.abs(e);
  if (verbosity == 0) {
    return base + base[base.length-1].repeat(eAbs-1);
  }
  else {
    if (eAbs == 1) { return base; }
    if (eAbs == 2) { return base + base; }
    return colorMultiPrefix(eAbs) + base + "-a";
  }
}

function findRuns(factors) {
  let [runs, current_run, current_run_val] = [[], [], undefined];
  for (let i = 0; i < factors.length; i++) {
    if (current_run_val != undefined) {
      if (Math.abs(factors[i][1]) == current_run_val) {
        current_run.push([factors[i][0], Math.sign(factors[i][1])]);
      }
      else {
        runs.push([current_run, current_run_val]);
        [current_run, current_run_val] = [[], undefined];
      }
    }
    // the below can't be an "else" because `current_run_val` may be different
    if (current_run_val == undefined) {
      current_run.push([factors[i][0], Math.sign(factors[i][1])]);
      current_run_val = Math.abs(factors[i][1]);
    }
  }
  if (current_run_val != undefined) {
    runs.push([current_run, current_run_val]);
  }
  return runs;
}

/**
  * Returns the magnitude + color prefix of the given interval in color notation
  *
  * @param {Interval} i
  * @param {Object=} opts
  * @param {integer=} opts.verbosity verbosity can be the default 0
  *                                  (e.g. "17og"), 1 (e.g. "sogu"), or 2
  *                                  (the same as 1 for this function)
  * @param {boolean=} opts.hideMagnitude defaults to false
  * @param {boolean=} opts.useFullMagnitude defaults to false
  * @param {boolean=} opts.keepTrailingHyphen defaults to false
  * @param {boolean=} opts.hasCoPrefix defaults to false, used in colorTempermentName
  * @returns {string}
  */
function colorPrefix(a,b, opts) {
  // if only two arguments are given, the second one may be `opts`!
  if (!opts && typeof b == 'object' && b != null) {
    opts = b;
    b = undefined;
  }
  let {verbosity, hideMagnitude, useFullMagnitude, keepTrailingHyphen, hasCoPrefix} = opts || {};
  if (verbosity == undefined) { verbosity = 0; }

  const i = Interval(a,b);
  if (!i.isFrac()) {
    throw new Error("Non-ratio has no color name");
  }
  let iNo23 = i.factorOut(2)[1].factorOut(3)[1];
  let factors = iNo23.factors();

  // the magnitude string
  let mStr = "";
  const m = colorMagnitude(i);
  if (m != 0 && !hideMagnitude) {
    const mAbs = Math.abs(m);
    if (verbosity == 0) {
      mStr = (m > 0 ? "L" : "s").repeat(mAbs);
    }
    else if (!useFullMagnitude) {
      mStr = m > 0 ? "la" : "sa";
      if (mAbs == 2) { mStr = mStr + mStr; }
      if (mAbs >= 3) { mStr = colorMultiPrefix(mAbs) + mStr; }
      // we add a hyphen in all but the three cases in which we would have a
      //  single syllable on either the left or right side
      if (!(mAbs == 1 || factors.length == 0
                      || (factors.length == 1 && Math.abs(factors[0][1]) == 1
                                              && !hasCoPrefix))) {
        mStr = mStr + "-";
      }
    }
    else {
      mStr = m > 0 ? "large " : "small ";
      if (mAbs > 1) { mStr = mAbs + "-" + mStr; }
    }
  }

  // the wa case
  if (factors.length == 0) {
    return mStr + colorFactorPrefix(3, 1, verbosity);
  }

  // the prefix string
  let pStr = "";
  // if verbosity != 0, pull out runs of prime exponents
  if (verbosity != 0) {
    for (const [ps,e] of findRuns(factors)) {
      const psStrs = ps.map(([p,si]) => colorFactorPrefix(p, si, verbosity));
      if (e == 1) { pStr = psStrs.reverse().join("") + pStr }
      else if (e == 2 && ps.length == 1) { pStr = psStrs[0] + psStrs[0] + pStr; }
      else { pStr = colorMultiPrefix(e) + psStrs.reverse().join("") + "-a" + pStr; }
    }
  }
  else {
    for (const [p,e] of factors) {
      pStr = colorFactorPrefix(p, e.valueOf(), verbosity) + pStr;
    }
  }
  // get rid of a trailing "-a"
  if (!keepTrailingHyphen) {
    pStr = pStr.replace(/-a$/, "");
  }

  // put the final string together
  let res = mStr + pStr;

  if (res == "lo") { return "ilo"; }
  if (res == "so") { return "iso"; }
  if (res == "no") { return "ino"; }
  if (res == "nu") { return "inu"; }
  return res;
}

/**
  * Returns the symbol of given interval in color notation
  *
  * @param {Interval} i
  * @param {Object=} opts
  * @param {integer=} opts.verbosity verbosity can be the default 0
  *                                  (e.g. "17og3"), 1 (e.g. "sogu 3rd"), or
  *                                  2 (e.g. "sogu third")
  * @param {boolean=} opts.useFullMagnitude defaults to false
  * @param {boolean=} opts.useWordNegative defaults to false
  * @returns {string}
  */
function colorSymb(a,b, opts) {
  // if only two arguments are given, the second one may be `opts`!
  if (!opts && typeof b == 'object' && b != null) {
    opts = b;
    b = undefined;
  }
  let {verbosity, useFullMagnitude, useWordNegative} = opts || {};
  if (verbosity == undefined) { verbosity = 0; }
  const optsToPass = { verbosity: verbosity
                     , useFullMagnitude: useFullMagnitude };

  const i = Interval(a,b);

  const d = colorDegree(i);
  const spacer_str = verbosity > 0 ? " " : "";
  const neg_str = verbosity > 0 && d < 0 ? (useWordNegative ? "negative " : "-") : "";
  const d_str = pyDegreeString(d, verbosity);
  return colorPrefix(i, optsToPass) + spacer_str + neg_str + d_str;
}

/**
  * Given a number of "co"s, a magnitude, an interval made up of only primes
  * > 3, and a degree, returns the corresponding interval in color notation
  *
  * @param {integer} m the number of "co"s
  * @param {integer} m the magnitude
  * @param {Interval} iNo23 cannot contain factors of 2 or 3
  * @param {integer} d the degree
  * @returns {Interval}
  */
function colorFromSymb(cos, m, iNo23, d) {
  iNo23 = Interval(iNo23);
  if (iNo23.hasExp(2) || iNo23.hasExp(3)) {
    throw new Error("Second argument to colorFromSymb has a factor of 2 or 3")
  }
  const zd = d - Math.sign(d); // AKA: S, or "stepspan" (here zd stands for "zeroed degree")
  const zdNo23 = colorZDegree(iNo23); // AKA: X
  const zd_diff = zd - zdNo23;
  const mNo2 = colorMagnitude(iNo23.mul(Interval(3).pow(2*zd_diff))); // AKA: Y

  // By the definition of degree we have:
  // (1) zd = 7*e2 + 11*e3 + zdNo23
  //  => zd_diff = 7*e2 + 11*e3

  // All solutions to the linear diophantine equation above have the form:
  //  e2 = -3*zd_diff - 11*k,
  //  e3 =  2*zd_diff +  7*k  for some k
  // (Source: https://en.wikipedia.org/wiki/Diophantine_equation#One_equation)

  // By the definition of magnitude we have:
  // (2) m = round((e3 + e5 + e7 + ...) / 7)
  //  => m = round((2*zd_diff + 7*k + e5 + e7 + ...) / 7)
  //  => m = round((2*zd_diff + e5 + e7 + ...) / 7) + k
  //  => m = mNo2 + k
  //  => k = m - mNo2

  const k = m - mNo2;
  const e2 = -3*zd_diff - 11*k;
  const e3 = 2*zd_diff + 7*k;

  // The above is sometimes different from what's given on:
  // https://en.xen.wiki/w/Color_notation
  const e3_xenWiki = mod(2*zd - 2*zdNo23 + 3, 7) + 7*m - 3;
  const e2_xenWiki = (zd - zdNo23 - 11*e3_xenWiki) / 7;
  if (e2 != e2_xenWiki || e3 != e3_xenWiki) {
    const ab1 = "a=" + e2_xenWiki + ",b=" + e3_xenWiki;
    const ab2 = "a=" + e2 + ",b=" + e3;
    console.log("Corrected ratio-from-color formula: " + ab1 + " ~> " + ab2);
  }

  return iNo23.mul(Interval([cos+e2,e3]));
}

/**
  * Returns the note name of the given interval in color notation. The returned
  * string uses ASCII instead of uniode wherever possible iff the `useASCII`
  * field of `opts` is given and is true
  *
  * @param {Interval} intvToA4
  * @param {Object=} opts
  * @param {integer=} opts.verbosity verbosity can be the default 0
  *                                  (e.g. "17ogC5"), 1 (e.g. "sogu C5"), or 2
  *                                  (the same as 1 for this function)
  * @param {boolean=} opts.useASCII defaults to false
  * @returns {string}
  */
function colorNote(a,b, opts) {
  // if only two arguments are given, the second one may be `opts`!
  if (!opts && typeof b == 'object' && b != null) {
    opts = b;
    b = undefined;
  }
  let {verbosity, useASCII} = opts || {};
  if (verbosity == undefined) { verbosity = 0; }
  const optsToPass = { verbosity: verbosity
                     , hideMagnitude: true };

  const i = Interval(a,b);
  let [e2,iNo2] = i.factorOut(2);
  let [e3,iNo23] = iNo2.factorOut(3);
  if (iNo23.equals(1)) {
    return pyNote(pyi, useASCII);
  }
  for (const [p,e] of iNo23.factors()) {
    const j = colorFromSymb(0, 0, Interval(p), 1).pow(e);
    e2 = e2.sub(j.expOf(2));
    e3 = e3.sub(j.expOf(3));
  }
  const pyi = Interval([e2,e3]);
  return colorPrefix(iNo23, optsToPass)
         + (verbosity > 0 ? " " : "")
         + pyNote(pyi, useASCII);
}

/**
  * Given an interval made up of only primes > 3 and a pythagorean interval to
  * A4, returns the corresponding interval to A4 in color notation
  *
  * @param {Interval} iNo23 cannot contain factors of 2 or 3
  * @param {Interval} pyi the base Pythagorean note
  * @returns {Interval}
  */
function colorFromNote(iNo23, pyi) {
  let [e2,e3] = [pyi.expOf(2), pyi.expOf(3)];
  for (const [p,e] of iNo23.factors()) {
    const j = colorFromSymb(0, 0, Interval(p), 1).pow(e);
    e2 = e2.add(j.expOf(2));
    e3 = e3.add(j.expOf(3));
  }
  return Interval([e2,e3]).mul(iNo23);
}

/**
  * Return the temperament name associated with the given comma, based on:
  * https://en.xen.wiki/w/Color_notation/Temperament_Names
  *
  * @param {Interval} i
  * @returns {string}
  */
function colorTemperament(a,b) {
  const iUnRed = Interval(a,b);
  if (iUnRed.compare(1) < 0) {
    throw new Error("Comma passed to `colorTempermentName` must be > 1");
  }

  const i = iUnRed.red();
  const cos = iUnRed.div(i).expOf(2).valueOf();
  let coStr = "";
  if (cos == 1) { coStr = "co"; }
  if (cos == 2) { coStr = "coco-"; }
  if (cos >= 3) { coStr = colorMultiPrefix(cos) + "co-"; }

  const [m, d] = [colorMagnitude(i), colorDegree(i)]
  const iNo23 = i.factorOut(2)[1].factorOut(3)[1];
  let segOffset = 1;
  let last_diff = i.distance();
  let curr_diff = colorFromSymb(0, m, iNo23, d-1).red().distance();
  while (curr_diff.compare(last_diff) < 0) {
    segOffset++;
    last_diff = curr_diff;
    curr_diff = colorFromSymb(0, m, iNo23, d-segOffset).red().distance();
  }
  const segOffsetStr = colorMultiPrefix(segOffset);

  const colorStr = colorPrefix(i, { verbosity: 1
                                  , useFullMagnitude: false
                                  , hasCoPrefix: cos == 1 });
  return coStr + colorStr + segOffsetStr;
}

module['exports'].colorPrimeZDegree = colorPrimeZDegree;
module['exports'].colorZDegree = colorZDegree;
module['exports'].colorDegree = colorDegree;
module['exports'].colorMagnitude = colorMagnitude;
module['exports'].colorPrimePrefix = colorPrimePrefix;
module['exports'].colorMultiPrefix = colorMultiPrefix;
module['exports'].colorFactorPrefix = colorFactorPrefix;
module['exports'].colorPrefix = colorPrefix;
module['exports'].colorSymb = colorSymb;
module['exports'].colorFromSymb = colorFromSymb;
module['exports'].colorNote = colorNote;
module['exports'].colorFromNote = colorFromNote;
module['exports'].colorTemperament = colorTemperament;
