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
  const inv_str = verbosity && d < 0 ? " (inverted)" : "";
  return pyQuality(a,b, opts) + d_str + inv_str;
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