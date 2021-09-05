/**
 * Color notation for intervals
 * Based on: https://en.xen.wiki/w/Color_notation
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module color
 **/

const {sign1, mod} = require('./utils.js')
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
  return zd + sign1(zd);
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
  * @param {Object=} opts
  * @param {integer=} opts.verbosity verbosity can be the default 0
  *                                  (e.g. "17ooo"), 1 (e.g. "triso-a"), or 2
  *                                  (the same as 1 for this function)
  * @param {boolean=} opts.useExps defaults to false
  * @param {boolean=} opts.useHTMLExps defaults to false
  * @returns {string}
  */
function colorFactorPrefix(p, e, opts) {
  if (e == 0) { return ""; }
  if (opts == undefined) { opts = {}; }
  let {verbosity, useExps, useHTMLExps} = opts;
  if (verbosity == undefined) { verbosity = 0; }

  const base = colorPrimePrefix(p, e > 0 ? "o" : "u", verbosity == 0);
  const eAbs = Math.abs(e);
  if (verbosity == 0) {
    if (eAbs == 1) { return base; }
    if (eAbs == 2) { return base + base[base.length-1]; }
    if (useHTMLExps) { return base + "<sup>" + eAbs + "</sup>"; }
    if (useExps) {
      if ((''+eAbs).length == 1) { return base + "^" + eAbs; }
      else { return base + "^(" + eAbs + ")"; }
    }
    return base + base[base.length-1].repeat(eAbs-1);
  }
  else {
    if (eAbs == 1) { return base; }
    if (eAbs == 2) { return base + base; }
    return colorMultiPrefix(eAbs) + base + "-a";
  }
}

// An algorithm to find all the groupings possible in a color prefix

// Classes to represent the way a color prefix may be grouped
// e.g. [ Group { d: 2,
//                gs: [ End { p: 11, e: Fraction(1) },
//                      Group { d: 3,
//                              gs: [ End { p: 7, e: Fraction(-1) } ] } ] },
//        Group { d: 3,
//                gs: [ End { p: 5, e: Fraction(1) } ] } ]
//      corresponds to bilotriru-atriyo
class End {
  constructor(p,e) {
    this.p = p;
    this.e = e;
  }
}
class Group {
  constructor(d, gs) {
    this.d = d;
    this.gs = gs;
  }
}

// Checks whether two class representations of color prefixes are the same
function eqPrefixes(p1, p2) {
  if (p1.length != p2.length) { return false; }
  for (let i = 0; i < p1.length; i++) {
    if (p1[i].constructor != p2[i].constructor) { return false; }
    if (p1[i] instanceof End) {
      return p1[i].p == p2[i].p && p1[i].e.equals(p2[i].e);
    }
    if (p1[i] instanceof Group) {
      return p1[i].d == p2[i].d && eqPrefixes(p1[i].gs, p2[i].gs);
    }
  }
}

// Converts a class representation of a color prefix to a string
function groupingToString(gs, opts) {
  if (opts.verbosity <= 0) { opts = Object.assign({}, opts, {verbosity: 1}); }
  let ret = "";
  for (const g of gs) {
    if (g instanceof End) {
      ret += colorFactorPrefix(g.p, g.e, opts);
    }
    if (g instanceof Group) {
      ret += colorMultiPrefix(g.d) + groupingToString(g.gs, opts) + "-a";
    }
  }
  return ret;
}

// Finds all possible groupings for a prime prefix using precomputed GCD values
// (see allGroupings for an example)
function allGroupings_withGCDs(xs, gcds) {
  let ret = [ xs.map(([p,e]) => e.n <= 2 && (p <= 19 || e.n == 1)
                                  ? new End(p,e)
                                  : new Group(e.n, [new End(p,e.div(e.n))])) ];
  if (xs.length == 1) {
    return ret;
  }
  const d = gcds[0][xs.length-1];
  let fn = (xs => xs);
  if (d > 1 && xs[0][1].n/d <= 2 && (xs[0][0] <= 19 || xs[0][1].n/d == 1)) {
    fn = (xs => [new Group(d, xs)]);
    xs = xs.map(([p,e]) => [p, e.div(d)]);
    gcds = gcds.map(ds => ds.map(x => x/d));
  }
  for (let i = 1; i < xs.length; i++) {
    const ls = allGroupings_withGCDs(xs.slice(0,i), gcds);
    const rs = allGroupings_withGCDs(xs.slice(i), gcds.slice(i));
    for (let j = 0; j < ls.length; j++) {
      for (let k = 0; k < rs.length; k++) {
        const new_gs = fn(ls[j].concat(rs[k]));
        if (ret.some(gs => eqPrefixes(gs, new_gs))) { continue; }
        ret.push(new_gs);
      }
    }
  }
  return ret;
}

// Precomputes the GCD values for allGroupings_withGCDs
function allAdjGCDs(xs) {
  let ret = xs.map(([p,e]) => [e.n]);
  for (let len = 1; len < xs.length; len++) {
    for (let i = 0; i < xs.length - len; i++) {
      const [a,b] = [ret[i], ret[i+1]];
      ret[i].push(gcd(a[a.length-1], b[b.length-1]));
    }
  }
  return ret;
}

// Finds all possible groupings for a prime prefix
// e.g. allGroupings(Interval([0,0,3,-6,2]).factors())
// [
//   [
//     End { p: 11, e: Fraction(2) },
//     Group { d: 6,
//             gs: [ End { p: 7, e: Fraction(-1) } ] },
//     Group { d: 3,
//             gs: [ End { p: 5, e: Fraction(1) } ] }
//   ],
//   [
//     End { p: 11, e: Fraction(2) },
//     Group { d: 3,
//             gs: [ End { p: 7, e: Fraction(-2) },
//                   End { p: 5, e: Fraction(1) } ] }
//   ],
//   [
//     Group { d: 2,
//             gs: [ End { p: 11, e: Fraction(1) },
//                   Group { d: 3,
//                           gs: [ End { p: 7, e: Fraction(-1) } ] } ] },
//     Group { d: 3,
//             gs: [ End { p: 5, e: Fraction(1) } ] }
//   ]
// ]
function allGroupings(factors) {
  const xs = factors.slice().reverse();
  const gcds = allAdjGCDs(xs);
  return allGroupings_withGCDs(xs, gcds);
}

// Counts the number of hyphens which will be in the resulting color prefix
function groupingHyphens(gs) {
  let hyphs = 0;
  for (let i = 0; i < gs.length; i++) {
    if (gs[i] instanceof Group) {
      hyphs += groupingHyphens(gs[i].gs);
      if (i < gs.length - 1) { hyphs++; }
    }
  }
  return hyphs;
}

// Checks all possible ways to group the factors of the color prefix, and
//  returns a string with the minimum number of hyphens, and among those
//  strings with the same number of hyphens, the maximum number of groupings.
//  If there are multiple strings which satisfy the above, strings which group
//  on the left are preferred.
function minColorPrefixStr(factors, opts) {
  const gss = allGroupings(factors);
  let [minHyphs, idxOfMin] = [Infinity, 0];
  for (let i = 0; i < gss.length; i++) {
    const hyphs = groupingHyphens(gss[i]);
    if (hyphs <= minHyphs) {
      [minHyphs, idxOfMin] = [hyphs, i];
    }
  }
  return groupingToString(gss[idxOfMin], opts);
}

/**
  * Returns the magnitude + color prefix of the given interval in color notation
  *
  * @param {Interval} i
  * @param {Object=} opts
  * @param {integer=} opts.verbosity verbosity can be the default 0
  *                                  (e.g. "17og"), 1 (e.g. "sogu"), or 2
  *                                  (the same as 1 for this function)
  * @param {boolean=} opts.addCosAfterDeg defaults to 13, if set to `Infinity`,
  *                                       a "co" prefix is never added
  * @param {boolean=} opts.hideMagnitude defaults to false
  * @param {boolean=} opts.useFullMagnitude defaults to false
  * @param {boolean=} opts.keepTrailingHyphen defaults to false
  * @param {boolean=} opts.useExps defaults to false
  * @param {boolean=} opts.useHTMLExps defaults to false
  * @returns {string}
  */
function colorPrefix(a,b, opts) {
  // if only two arguments are given, the second one may be `opts`!
  if (!opts && typeof b == 'object' && b != null) {
    opts = b;
    b = undefined;
  }
  let { verbosity, addCosAfterDeg, hideMagnitude,
        useFullMagnitude, keepTrailingHyphen, useExps, useHTMLExps} = opts || {};
  if (verbosity == undefined) { verbosity = 0; }
  if (addCosAfterDeg == undefined) { addCosAfterDeg = 13; }

  let i = Interval(a,b);
  if (!i.isFrac()) {
    throw new Error("Non-ratio has no color name");
  }

  // the cos string
  let [cos, coStr] = [0, ""];
  const [i_logval, d] = [i.valueOf_log(), colorDegree(i)];
  if (i_logval >= 1 && (d > addCosAfterDeg || d < -addCosAfterDeg)) {
    cos = Math.floor(i_logval);
    i = i.div(Interval(2).pow(cos));
  }
  if (verbosity == 0) {
    if (cos == 0) { coStr = "" }
    else if (cos == 1) { coStr = "c"; }
    else if (cos == 2) { coStr = "cc"; }
    else if (useHTMLExps) { coStr = "c<sup>" + cos + "</sup>"; }
    else if (useExps) {
      if ((''+cos).length == 1) { coStr = "c^" + cos; }
      else { coStr = "c^(" + cos + ")"; }
    }
    else { coStr = "c".repeat(cos); }
  }
  else {
    if (cos == 1) { coStr = "co"; }
    if (cos == 2) { coStr = "coco"; }
    if (cos >= 3) { coStr = colorMultiPrefix(cos) + "co"; }
  }

  let iNo23 = i.factorOut(2)[1].factorOut(3)[1];
  let factors = iNo23.factors();

  // the magnitude string
  let [mAbs, mStr] = [0, ""];
  const m = colorMagnitude(i);
  if (m != 0 && !hideMagnitude) {
    mAbs = Math.abs(m);
    if (verbosity == 0) {
      const c = m > 0 ? "L" : "s";
      if (mAbs == 0) { mStr = ""; }
      else if (mAbs == 1) { mStr = c; }
      else if (mAbs == 2) { mStr = c + c; }
      else if (useHTMLExps) { mStr = c + "<sup>" + mAbs + "</sup>"; }
      else if (useExps) {
        if ((''+mAbs).length == 1) { mStr = c + "^" + mAbs; }
        else { mStr = c + "^(" + mAbs + ")"; }
      }
      else { mStr = c.repeat(mAbs); }
    }
    else if (!useFullMagnitude) {
      mStr = m > 0 ? "la" : "sa";
      if (mAbs == 2) { mStr = mStr + mStr; }
      if (mAbs >= 3) { mStr = colorMultiPrefix(mAbs) + mStr; }
    }
    else {
      mStr = m > 0 ? "large " : "small ";
      if (mAbs > 1) { mStr = mAbs + "-" + mStr; }
    }
  }

  // the prefix string
  let pStr = "";
  // the wa case
  if (factors.length == 0) {
    pStr = colorFactorPrefix(3, 1, opts);
  }
  // if verbosity != 0, pull out groups of exponents
  else if (verbosity != 0) {
    pStr = minColorPrefixStr(factors, opts);
  }
  else {
    for (const [p,e] of factors) {
      pStr = colorFactorPrefix(p, e.valueOf(), opts) + pStr;
    }
  }
  // get rid of a trailing "-a"
  if (!keepTrailingHyphen) {
    pStr = pStr.replace(/-a$/, "");
  }

  let [hy1, hy2] = ["", ""];
  if (verbosity != 0) {
    let coNoHyphen = cos <= 1; // when coStr is empty or a single syllable
    let mNoHyphen = mAbs <= 1; // when mStr is empty or a single syllable
    let pNoHyphen = factors.length == 0 ||
                    (factors.length == 1 && Math.abs(factors[0][1]) == 1);
                    // ^ when pStr is a single syllable
    // we add a hyphen between sections in all but the cases in which we would
    //  have a single syllable on either the left or right side
    if (mAbs == 0) {
      if (!(coNoHyphen || pNoHyphen)) { hy2 = "-"; }
    }
    else {
      if (!(coNoHyphen || mNoHyphen)) { hy1 = "-"; }
      if (!( mNoHyphen || pNoHyphen)) { hy2 = "-"; }
    }
  }

  // put the final string together
  let res = coStr + hy1 + mStr + hy2 + pStr;

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
  * @param {boolean=} opts.addCosAfterDeg defaults to 13, if set to `Infinity`,
  *                                       a "co" prefix is never added
  * @param {boolean=} opts.useFullMagnitude defaults to false
  * @param {boolean=} opts.useWordNegative defaults to false
  * @param {boolean=} opts.useExps defaults to false
  * @param {boolean=} opts.useHTMLExps defaults to false
  * @returns {string}
  */
function colorSymb(a,b, opts) {
  // if only two arguments are given, the second one may be `opts`!
  if (!opts && typeof b == 'object' && b != null) {
    opts = b;
    b = undefined;
  }
  let {verbosity, addCosAfterDeg, useFullMagnitude, useWordNegative, useExps, useHTMLExps} = opts || {};
  if (verbosity == undefined) { verbosity = 0; }
  if (addCosAfterDeg == undefined) { addCosAfterDeg = 13; }
  const optsToPass = { verbosity: verbosity
                     , addCosAfterDeg: addCosAfterDeg
                     , useFullMagnitude: useFullMagnitude
                     , useExps: useExps
                     , useHTMLExps: useHTMLExps };

  const i = Interval(a,b);
  const i_logval = i.valueOf_log();
  if (i.compare(1) < 0) {
    return (verbosity == 0 ? "desc. " : "descending ")
           + colorSymb(i.recip(), opts);
  }

  let d = colorDegree(i);
  if (i_logval >= 1 && (d > addCosAfterDeg || d < -addCosAfterDeg)) {
    const cos = Math.floor(i_logval);
    d = colorDegree(i.div(Interval(2).pow(cos)));
  }

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
  * @param {boolean} [logCorrections] defaults to false
  * @returns {Interval}
  */
function colorFromSymb(cos, m, iNo23, d, logCorrections) {
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

  if (logCorrections) {
    // The above is sometimes different from what's given on:
    // https://en.xen.wiki/w/Color_notation
    const e3_xenWiki = mod(2*zd - 2*zdNo23 + 3, 7) + 7*m - 3;
    const e2_xenWiki = (zd - zdNo23 - 11*e3_xenWiki) / 7;
    if (e2 != e2_xenWiki || e3 != e3_xenWiki) {
      const ab1 = "a=" + e2_xenWiki + ",b=" + e3_xenWiki;
      const ab2 = "a=" + e2 + ",b=" + e3;
      console.log("Corrected ratio-from-color formula: " + ab1 + " ~> " + ab2);
    }
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
  * @param {boolean=} opts.useExps defaults to false
  * @param {boolean=} opts.useHTMLExps defaults to false
  * @returns {string}
  */
function colorNote(a,b, opts) {
  // if only two arguments are given, the second one may be `opts`!
  if (!opts && typeof b == 'object' && b != null) {
    opts = b;
    b = undefined;
  }
  let {verbosity, useASCII, useExps, useHTMLExps} = opts || {};
  if (verbosity == undefined) { verbosity = 0; }
  const optsToPass = { verbosity: verbosity
                     , addCosAfterDeg: Infinity
                     , hideMagnitude: true
                     , useExps: useExps
                     , useHTMLExps: useHTMLExps };

  const i = Interval(a,b);
  let [e2,iNo2] = i.factorOut(2);
  let [e3,iNo23] = iNo2.factorOut(3);
  if (iNo23.equals(1)) {
    return pyNote(i, useASCII);
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
  const i = Interval(a,b);
  if (i.compare(1) < 0) {
    throw new Error("Comma passed to `colorTempermentName` must be > 1");
  }

  const iRed = i.red();
  const [m, zd] = [colorMagnitude(iRed), colorZDegree(iRed)]
  const iNo23 = i.factorOut(2)[1].factorOut(3)[1];
  let segOffset = 1;
  let last_diff = iRed.distance();
  let curr_diff = colorFromSymb(0, m, iNo23, (zd-1)+sign1(zd-1)).red().distance();
  while (curr_diff.compare(last_diff) < 0) {
    segOffset++;
    last_diff = curr_diff;
    curr_diff = colorFromSymb(0, m, iNo23, (zd-segOffset)+sign1(zd-segOffset)).red().distance();
  }
  const segOffsetStr = colorMultiPrefix(segOffset);

  const colorStr = colorPrefix(i, { verbosity: 1
                                  , addCosAfterDeg: 0 // i.e. always
                                  , useFullMagnitude: false });
  return colorStr + segOffsetStr;
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
