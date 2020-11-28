/**
 * @module interval.js
 * Copyright (c) 2020, Matthew Yacavone (matthew [at] yacavone [dot] net)
 **/

(function(root) {

const pf = require('primes-and-factors');
const Fraction = require('fraction.js');

const keys = function(a, b) {
  var ret = {};
  for (const [k,v] of Object.entries(a)) {
    ret[k] = 1;
  }
  if (b) {
    for (const [k,v] of Object.entries(b)) {
      ret[k] = 1;
    }
  }
  return ret;
}

const parse = function(a, b) {
  if (a === undefined || a === null) {
    return {};
  }
  else if (b !== undefined) {
    if (a == 0 || a < 0 || b == 0 || b < 0) {
      throw "non-positive number cannot be converted into an interval"
    }
    const afs = pf.getPrimeExponentObject(a);
    const bfs = pf.getPrimeExponentObject(b);
    var ret = keys(afs,bfs);
    for (const i in ret) {
      ret[i] = Fraction((afs[i] || 0) - (bfs[i] || 0));
    }
    return ret;
  }
  else if (typeof a == "object") {
    if ("d" in a && "n" in a) {
      var sn = a["n"];
      if ("s" in a) {
        sn *= a["s"];
      }
      return parse(sn, a["d"]);
    }
    else {
      var allPrimes = true
      var ret = {};
      for (const i in keys(a)) {
        allPrimes &= pf.isPrime(Number(i));
        if (Fraction(a[i]) != 0) {
          ret[i] = Fraction(a[i]);
        }
      }
      if (allPrimes) {
        return ret;
      } else {
        throw "argument is not a number, fraction, or interval"
      }
    }
  }
  else {
    return parse(Fraction(a));
  }
}

/**
  * Module constructor
  *
  * @constructor
  * @param {number|Fraction|Object=} a
  * @param {number=} b
  */
function Interval(a, b) {

  if (!(this instanceof Interval)) {
    return new Interval(a, b);
  }

  const p = parse(a,b);
  for (const i in keys(p)) {
    this[i] = p[i]
  }

}

Interval.prototype = {

  /**
   * Multiplies two intervals
   */
  "mul": function(a,b) {
    const rhs = parse(a,b);
    var ret = keys(this,rhs);
    for (const i in ret) {
      ret[i] = (this[i] || Fraction(0)).add(rhs[i] || Fraction(0));
    }
    return new Interval(ret);
  },

  /**
   * Divides two intervals
   */
  "div": function(a,b) {
    const rhs = parse(a,b);
    var ret = keys(this,rhs);
    for (const i in ret) {
      ret[i] = (this[i] || Fraction(0)).sub(rhs[i] || Fraction(0));
    }
    return new Interval(ret);
  },

  /**
   * Takes the reciprocal/inverse of an interval
   */
  "recip": function() {
    var ret = keys(this);
    for (const i in ret) {
      ret[i] = this[i].neg();
    }
    return new Interval(ret);
  },

  /**
   * Raises an interval to a fractional power
   */
  "pow": function(a,b) {
    var ret = keys(this);
    for (const i in ret) {
      ret[i] = this[i].mul(Fraction(a,b));
    }
    return new Interval(ret);
  },

  /**
   * Takes the nth root of an interval
   */
  "root": function(a) {
    return this.pow(Fraction(1,a));
  },

  /**
   * The square root of an interval
   */
  "sqrt": function() {
    return this.pow(1/2);
  },

  /**
   * Converts an interval with integer prime exponents to a fraction
   */
  "toFrac": function() {
    var ret = Fraction(1);
    for (const i in keys(this)) {
      if (this[i].d == 1) {
        ret = ret.mul(Fraction(i).pow(this[i].s * this[i].n));
      } else {
        throw "interval does not have integer exponents";
      }
    }
    return ret;
  },

  /**
   * Converts an interval with fractional prime exponents to the nth root of a
   * fraction. Specifically, this function returns an `Object` of type
   * `{{k: Fraction, n:Integer}}`.
   */
  "toNthRoot": function() {
    var n_fr = Fraction(1);
    for (const i in keys(this)) {
      n_fr = n_fr.gcd(this[i]);
    }
    return { k: this.pow(n_fr.inverse()).toFrac(), n: n_fr.d };
  },

  /**
   * Compares two intervals. Specifically, returns 0 if the intervals are equal,
   * 1 if the first interval is greater than the second, and -1 if the second
   * interval is greater than the first.
   */
  "compare": function(a,b) {
    const rhs = parse(a,b);
    return this.div(rhs).toNthRoot().k.compare(Fraction(1));
  },

  /**
   * Checks if the two intervals are the same.
   */
  "equals": function(a,b) {
    return this.compare(a,b) == 0;
  },

  /**
   * Converts an interval to its decimal value
   */
  "valueOf": function() {
    var ret = 1;
    for (const i in keys(this)) {
      ret *= Math.pow(i,this[i].valueOf())
    }
    return ret;
  },

  /**
   * Reduces an interval w.r.t. another interval. If the second argument is not
   * given or is undefined, it is taken to be 2 (an octave).
   */
  "red": function(a,b) {
    const rhs = Interval(2);
    if (a || b) {
      rhs = parse(a,b);
    }
    // the exponent of the closest power of `rhs` to `this`
    const logval = Math.log(this.valueOf()) / Math.log(rhs.valueOf());
    const e = Math.round(logval);
    // if `this` is greater than or equal to this power, divide by it
    if (this.compare(rhs.pow(e)) >= 0) {
      return this.div(rhs.pow(e));
    }
    // otherwise divide by the power one below it
    else {
      return this.div(rhs.pow(e-1));
    }
  },

  /**
   * Balanced reduces an interval w.r.t. another interval. If the second
   * argument is not given or is undefined, it is taken to be 2 (an octave).
   */
  "reb": function(a,b) {
    const rhs = Interval(2);
    if (a || b) {
      rhs = parse(a,b);
    }
    // the exponent of the closest half-power of `rhs` to `this`
    const logval = Math.log(this.valueOf()) / Math.log(rhs.valueOf());
    const e = Math.round(logval+0.5)-0.5;
    // if `this` is greater than this power, divide by it rounded up
    if (this.compare(rhs.pow(e)) > 0) {
      return this.div(rhs.pow(e+0.5));
    }
    // otherwise divide by it rounded down
    else {
      return this.div(rhs.pow(e-0.5));
    }
  },

  /**
   * Converts an interval to its value in cents
   */
  "toCents": function() {
    return Math.log(this.valueOf()) / Math.log(2) * 1200
  }

}

if (typeof define === "function" && define["amd"]) {
  define([], function() {
    return Interval;
  });
} else if (typeof exports === "object") {
  Object.defineProperty(Interval, "__esModule", { 'value': true });
  module['exports'] = Interval;
} else {
  root['Interval'] = Interval;
}

})(this);
