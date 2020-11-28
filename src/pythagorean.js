/**
 * @module pythagorean.js
 * Copyright (c) 2020, Matthew Yacavone (matthew [at] yacavone [dot] net)
 **/

(function(root) {

const pf = require('primes-and-factors');
const Fraction = require('fraction.js');
const Interval = require('./interval.js');

/**
  * Constructs an interval from a pythagorean degree and offset
  *
  * @param {Integer} d
  * @param {Fraction} o
  * @returns {Interval}
  */
function pyInterval(d,o) {
  const ox4 = Fraction(o).mul(4 * Math.sign(d));
  if (ox4.d != 1) {
    throw "offset does not have denominator 1, 2, or 4"
  }
  const zd = d - Math.sign(d);
  const ng = ((zd * 4 + 3) % 7) - 3;
  const g = ng * 2 + ox4.d * ox4.n * 7;
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

function generator(a,b) {
  const i = new Interval(a,b);
  const g = (i['3'] || Fraction(0)).mul(4);
  if (g.d != 1) {
    throw "interval is not pythagorean";
  }
  return g.s * g.n;
}

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

module['exports'].pyInterval = pyInterval;
module['exports'].isPythagorean = isPythagorean;
module['exports'].degree = degree;
module['exports'].offset = offset;

})(this);
