/**
 * @module pythagorean.js
 * Copyright (c) 2020, Matthew Yacavone (matthew [at] yacavone [dot] net)
 **/

(function(root) {

const pf = require('primes-and-factors');
const Fraction = require('fraction.js');
const Interval = require('./interval.js');

function mod(a,n) {
  return ((a % n) + n) % n;
}

/**
  * Constructs an interval from a pythagorean degree and offset
  *
  * @param {Integer} d
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
  * @param {Interval} o
  * @returns {boolean}
  */
function isPythagorean(a,b) {
  const i = new Interval(a,b);
  const e2 = (i['2'] || Fraction(0));
  const e3 = (i['3'] || Fraction(0));
  return e3.mul(4).d == 1 && e2.add(e3).d == 1;
}

/**
  * For a given pythagorean interval `(3/2)^(g/4) * 2^v`, returns the `g`.
  *
  * @param {Interval} o
  * @returns {Integer}
  */
function generator(a,b) {
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
  * @param {Interval} o
  * @returns {Integer}
  */
function octaves(a,b) {
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
  * @returns {Integer}
  */
function degree(a,b) {
  const i = new Interval(a,b);
  const g = generator(i);
  const v = octaves(i);
  const zd = g + v * 7;
  return zd == 0 ? 1 : zd + Math.sign(zd);
}

/**
  * Returns the offset of the given pythagorean interval
  *
  * @param {Interval} i
  * @returns {Fraction}
  */
function offset(a,b) {
  const i = new Interval(a,b);
  const g = generator(i);
  const v = octaves(i);
  const zd = g + v * 7;
  return Fraction(Math.sign(zd) * (2 * Math.floor((4 * g + 3) / 7) - g), 4);
}

/**
  * Reduces a pythagorean degree so it lies between 1 and 7
  *
  * @param {Integer} d
  * @returns {Integer}
  */
function redDeg(d) {
  return mod(d - Math.sign(d), 7) + 1;
}

/**
  * Checks whether a given degree (of a pythagorean interval) is a unison,
  * fourth, or fifth
  *
  * @param {Integer} d
  * @returns {boolean}
  */
function isPerfectDeg(d) {
  return redDeg(d) == 1 || redDeg(d) == 4 || redDeg(d) == 5;
}

/**
  * Returns the quality of the given pythagorean interval
  *
  * @param {Interval} i
  * @returns {String}
  */
function pyQuality(a,b) {
  var o = offset(a,b);
  if (isPerfectDeg(degree(a,b))) {
    if (o == 0    ) { return "P" }
  }
  else {
    if (o == 0    ) { return "n" }
    if (o == 0.25 ) { return "sM" }
    if (o == 0.5  ) { return "M" }
    if (o == -0.25) { return "sm" }
    if (o == -0.5 ) { return "m" }
    o = o.sub(o.s * Fraction(1,2));
  }
  if (o == 0.5 ) { return "sA" }
  if (o == 1   ) { return "A" }
  if (o == -0.5) { return "sA" }
  if (o == -1  ) { return "d" }
  if (o > 1  && o.d == 1) { return o.n + "A" }
  if (o > 1  && o.d != 1) { return o.toFraction() + "-A" }
  if (o < -1 && o.d == 1) { return o.n + "d" }
  if (o < -1 && o.d != 1) { return o.neg().toFraction() + "-d" }
}

/**
  * Returns the symbol of the given pythagorean interval
  *
  * @param {Interval} i
  * @returns {String}
  */
function pySymb(a,b) {
  const d = degree(a,b);
  return pyQuality(a,b) + d;
}

module['exports'].pyInterval = pyInterval;
module['exports'].isPythagorean = isPythagorean;
module['exports'].generator = generator;
module['exports'].octaves = octaves;
module['exports'].degree = degree;
module['exports'].offset = offset;
module['exports'].redDeg = redDeg;
module['exports'].isPerfectDeg = isPerfectDeg;
module['exports'].pyQuality = pyQuality;
module['exports'].pySymb = pySymb;

})(this);
