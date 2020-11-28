/**
 * @module fjs.js
 * Copyright (c) 2020, Matthew Yacavone (matthew [at] yacavone [dot] net)
 **/

(function(root) {

const pf = require('primes-and-factors');
const Fraction = require('fraction.js');
const Interval = require('./interval.js');
const py = require('./pythagorean.js');

/**
  * The radius of tolerance of the FJS
  *
  * @constant {Interval}
  */
const fjsRoT = Interval(65,63);

/**
  * Returns the FJS comma associated to a prime interval greater than 3
  * (i.e. 5, 7, 11, etc.)
  *
  * @param {Integer} p
  * @returns {boolean}
  */
function fjsComma(pin) {
  const p = parseInt(pin);
  if (!pf.isPrime(p) || p <= 3) {
    throw "input is not a prime interval greater than 3";
  }
  var g = 0;
  while (true) {
    let c = Interval(p).div(Interval(3).pow(g)).reb();
    if (c.compare(fjsRoT) < 0 && fjsRoT.recip().compare(c) < 0) {
      return c;
    }
    g = g > 0 ? -g : -g+1;
  }
}

/**
  * Given an interval, returns the product of the FJS commas associated to each
  * of its prime factors raised to the exponents of those prime factors
  *
  * @param {Interval} k
  * @returns {boolean}
  */
function fjsFactor(a,b) {
  var ret = Interval(1);
  for (const [p,e] of Object.entries(Interval(a,b))) {
    ret = ret.mul(fjsComma(p).pow(e));
  }
  return ret;
}

module['exports'].fjsRoT = fjsRoT;
module['exports'].fjsComma = fjsComma;
module['exports'].fjsFactor = fjsFactor;

})(this);
