/**
 * The interval datatype, based on `Fraction` from `fraction.js` on npm
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module interval
 **/

const pf = require('primes-and-factors');
const Fraction = require('fraction.js');

const keys = function(a, b) {
  let ret = {};
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
    let ret = keys(afs,bfs);
    for (const i in ret) {
      ret[i] = Fraction((afs[i] || 0) - (bfs[i] || 0));
    }
    return ret;
  }
  else if (typeof a == "object") {
    if ("d" in a && "n" in a) {
      let sn = a["n"];
      if ("s" in a) {
        sn *= a["s"];
      }
      return parse(sn, a["d"]);
    }
    else {
      let allPrimes = true
      let ret = {};
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
  * Constructs an `Interval`. Valid argument combinations are two integers
  * (e.g. `Interval(3,2)`), a single number (e.g. `Interval(3/2)`), an array
  * containing 0-2 integers (e.g. `Interval([3,2])`), a factorization
  * (e.g. `Interval({2: -1, 3: 1})`), a `Fraction`, or an `Interval`.
  *
  * As a convention, all functions which have a JSDoc parameter of type
  * `Interval` should be able to accept any of these argument combinations in
  * place of that parameter. For example, `mul` in this file, or
  * `bestRationalApproxs` in `approx.js`.
  *
  * If both arguments are omitted, the result is `Interval(1)`.
  *
  * @constructor
  * @param {(number|Fraction|Object)=} a
  * @param {integer=} b
  */
function Interval(a,b) {

  if (!(this instanceof Interval)) {
    return new Interval(a,b);
  }

  const p = parse(a,b);
  for (const i in keys(p)) {
    this[i] = p[i]
  }

}

Interval.prototype = {

  /**
   * Multiplies (i.e. composes) two intervals.
   *
   * e.g. `Interval(3,2).mul(5,4)` is the composition of `3/2` and `5/4`, the
   * interval `15/8`, or `Interval(15,8)`
   *
   * @param {Interval} i
   * @returns {Interval}
   */
  "mul": function(a,b) {
    const rhs = parse(a,b);
    let ret = keys(this,rhs);
    for (const i in ret) {
      ret[i] = (this[i] || Fraction(0)).add(rhs[i] || Fraction(0));
    }
    return new Interval(ret);
  },

  /**
   * Divides two intervals.
   *
   * e.g. `Interval(2).div(3,2)` is exactly `Interval(4,3)`
   *
   * @param {Interval} i
   * @returns {Interval}
   */
  "div": function(a,b) {
    const rhs = parse(a,b);
    let ret = keys(this,rhs);
    for (const i in ret) {
      ret[i] = (this[i] || Fraction(0)).sub(rhs[i] || Fraction(0));
    }
    return new Interval(ret);
  },

  /**
   * Takes the reciprocal/inverse of an interval.
   *
   * e.g. `Interval(3,2).recip()` is exactly `Interval(2,3)`
   *
   * @returns {Interval}
   */
  "recip": function() {
    let ret = keys(this);
    for (const i in ret) {
      ret[i] = this[i].neg();
    }
    return new Interval(ret);
  },

  /**
   * Raises an interval to a fractional power.
   *
   * e.g. `Interval(4,3).pow(2)` is the composition of `4/3` with itself, the
   * interval `16/9`
   *
   * e.g. `Interval(2).pow(4,12)` is the interval `4\12`, four steps of 12-EDO
   *
   * e.g. `Interval(5).pow(1,4)` is the fourth root of `5`, the fifth in
   * quarter-comma meantone
   *
   * @param {Fraction} k
   * @returns {Interval}
   */
  "pow": function(a,b) {
    let ret = keys(this);
    for (const i in ret) {
      ret[i] = this[i].mul(Fraction(a,b));
    }
    return new Interval(ret);
  },

  /**
   * The nth root of an interval, i.e. `pow(1,n)`.
   *
   * @param {integer} n
   * @returns {Interval}
   */
  "root": function(a) {
    return this.pow(Fraction(1,a));
  },

  /**
   * The square root of an interval, i.e. `pow(1,2)`.
   *
   * @returns {Interval}
   */
  "sqrt": function() {
    return this.pow(1/2);
  },

  /**
   * Converts an interval with integer prime exponents, i.e. an interval which
   * can be expressed as a fraction, to a `Fraction`.
   *
   * e.g. `Interval(3,2).toFrac()` is exactly `Fraction(3,2)`
   *
   * e.g. `Interval(2).sqrt().toFrac()` throws an error
   *
   * @returns {Fraction}
   */
  "toFrac": function() {
    let ret = Fraction(1);
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
   * Takes the mediant of two intervals with integer prime exponents, i.e. two
   * intervals which can be expressed as fractions.
   *
   * e.g. `Interval(5,4).med(9,7)` is the mediant of the intervals `5/4` and
   * `9/7`, the interval `14/11`
   *
   * @returns {Interval}
   */
  "med": function(a,b) {
    let [f1, f2] = [this.toFrac(), Interval(a,b).toFrac()];
    return new Interval(f1.n + f2.n, f1.d + f2.d);
  },

  /**
   * Converts any interval to its representation as the nth root of a fraction.
   *
   * e.g. `Interval(3,2).toNthRoot()` is exactly `{k: Interval(3,2), n: 1}`
   *
   * e.g. `Interval({2: 1/2, 5: 1/3})` is exactly `{k: Interval({2: 3, 5: 2}), n: 6}`
   *
   * @returns {{k: Fraction, n: Integer}}
   */
  "toNthRoot": function() {
    let n_fr = Fraction(1);
    for (const i in keys(this)) {
      n_fr = n_fr.gcd(this[i]);
    }
    return { k: this.pow(n_fr.inverse()).toFrac(), n: n_fr.d };
  },

  /**
   * Converts an interval to its decimal value, for automatic use by javascript.
   * Note that this function should not be used to compare relative sizes of
   * intervals, use `valueOf_log` or `toCents` instead.
   *
   * e.g. `Interval(3,2).valueOf()` is exactly `1.5`
   *
   * @returns {number}
   */
  "valueOf": function() {
    let ret = 1;
    for (const i in keys(this)) {
      ret *= Math.pow(i,this[i].valueOf())
    }
    return ret;
  },

  /**
   * Performs an exact comparison of two intervals. Specifically, returns 0 if
   * the intervals are equal, 1 if the first interval is greater than the
   * second, and -1 if the second interval is greater than the first.
   *
   * In general, `i1 ineq i2`, where `i1`, `i2` are intervals and `ineq` is an
   * inequality (e.g. `>=`), can be incorrect since javascript uses `valueOf`
   * to convert both sides to finite precision floating point numbers before
   * doing the comparision. To perform an exact version of the same check,
   * use `i1.compare(i2) ineq 0`.
   *
   * e.g. `Interval(2).sqrt().compare(3,2) < 0` is true since `sqrt(2) < 3/2`
   *
   * @param {Interval} i
   * @returns {integer}
   */
  "compare": function(a,b) {
    const diff = this.div(a,b);
    const {k,n} = diff.toNthRoot();
    if (isFinite(k.n) && isFinite(k.d)) {
      // do an exact comparison if k is finite
      return k.compare(Fraction(1));
    }
    else {
      // otherwise the numbers are just too big - for now we approximate
      return (1 < diff.valueOf()) - (diff.valueOf() < 1);
    }
  },

  /**
   * Checks if the two intervals are the same. In general, `i1 == i2`, where
   * `i1`, `i2` are intervals, can give false positives since javascript uses
   * `valueOf` to convert both sides to finite precision floating point numbers
   * before doing the comparision.
   *
   * e.g. `Interval(4,3).pow(2).equals(16,9)` is true
   *
   * @param {Interval} i
   * @returns {boolean}
   */
  "equals": function(a,b) {
    return this.compare(a,b) == 0;
  },

  /**
   * If the given argument is a prime, returns a pair whose first element is the
   * exponent of that prime in this interval, and whose second element is the
   * interval without that prime (i.e. the rest of the factorization).
   *
   * e.g. `Interval(8*5,7).factorOut(2)` is exactly `[3, Interval(5,7)]`.
   *
   * More generally, if the given argument is an interval `i` with factorization
   * `p1^e1 ... pm^em` (where the `pk`s are prime and in ascending order, and
   * each `ek > 0`), returns a pair `[g, this.div(i.pow(g))]` where `g` is the
   * smallest fraction such that `this.div(i.pow(g))` contains no factors of
   * `pm` (the largest prime in the factorization of `i`).
   *
   * e.g. `Interval(9,8).factorOut(3,2)` is exactly `[2, Interval(1,2)]`.
   *
   * @param {Interval} i
   * @returns {Pair.<Fraction,Interval>}
   */
  "factorOut": function(a,b) {
    const base = new Interval(a,b);
    const gp = Math.max(...Object.keys(base));
    if (isFinite(gp)) {
      const g = (this[gp] || Fraction(0)).div(base[gp]);
      let res = keys(this, base);
      for (const i in res) {
        res[i] = (this[i] || Fraction(0)).sub((base[i] || Fraction(0)).mul(g));
      }
      return [g, new Interval(res)];
    }
    else {
      return [Fraction(0), this];
    }
  },

  /**
   * Converts an interval to its decimal value log the given base. If no
   * argument is given, the base is taken to be 2 (an octave).
   *
   * e.g. `Interval(3,2).valueOf_log()` gives `0.5849625007211561`
   *
   * Note that this function uses `factorOut` to preserve as much precision as
   * possible - for example, for any interval `i` and fraction `k`, then
   * `i.pow(k).valueOf_log(i) == k` *exactly*.
   *
   * e.g. `Interval(3,2).pow(1,2).valueOf_log(3,2)` gives `0.5`
   *
   * @param {Interval} [i=Interval(2)]
   * @returns {number}
   */
  "valueOf_log": function(a,b) {
    let base = new Interval(2);
    if (a != undefined || b != undefined) {
      base = new Interval(a,b);
    }
    const [g, res] = this.factorOut(base);
    return g + Math.log(res.valueOf()) / Math.log(base.valueOf());
  },

  /**
   * Reduces an interval w.r.t. another interval. If no argument is given, it
   * is taken to be 2 (an octave).
   *
   * e.g. `Interval(3,2).pow(2).red()` is exactly `Interval(9,8)`
   *
   * For all intervals `i`, `j` this function satisfies the equality:
   * `i.div(i.red(j)).equals(j.pow(Math.floor(i.valueOf_log(j))))`
   *
   * @param {Interval} [i=Interval(2)]
   * @returns {Interval}
   */
  "red": function(a,b) {
    let base = new Interval(2);
    if (a != undefined || b != undefined) {
      base = new Interval(a,b);
    }
    const e = Math.floor(this.valueOf_log(base));
    return this.div(base.pow(e));
  },

  /**
   * Balanced reduces an interval w.r.t. another interval. If no argument is
   * given, the it is taken to be 2 (an octave).
   *
   * e.g. `Interval(3,2).reb()` is exactly `Interval(2,3)`
   *
   * For all intervals `i`, `j` this function satisfies the equality:
   * `i.div(i.reb(j)).equals(j.pow(Math.round(i.valueOf_log(j))))`
   *
   * @param {Interval} [i=Interval(2)]
   * @returns {Interval}
   */
  "reb": function(a,b) {
    let base = new Interval(2);
    if (a != undefined || b != undefined) {
      base = new Interval(a,b);
    }
    const e = Math.round(this.valueOf_log(base));
    return this.div(base.pow(e));
  },

  /**
   * Converts an interval to its value in cents.
   *
   * e.g. `Interval(3,2).toCents()` gives `701.9550008653873`
   *
   * Note that this function uses `factorOut` to preserve as much precision as
   * possible - for example, for any fraction `k`,
   * `Interval(2).pow(k).toCents() == k.mul(1200)` *exactly*.
   *
   * e.g. `Interval(2).pow(4,12).toCents()` is exactly `400`
   *
   * @returns {number}
   */
  "toCents": function() {
    const [e2, res] = this.factorOut(2);
    return e2.mul(1200) + Math.log(res.valueOf()) / Math.log(2) * 1200;
  },

  /**
   * Converts an interval to its Tenney harmonic distance, or Tenney height.
   *
   * e.g. `Interval(3,2).tenneyHD()` gives `2.584962500721156`
   *
   * @returns {number}
   */
  "tenneyHD": function() {
    let ret = Interval(1);
    for (const i in keys(this)) {
      ret[i] = this[i].abs();
    }
    return ret.valueOf_log();
  }

}

module.exports = Interval;
