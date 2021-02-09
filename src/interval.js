/**
 * The interval datatype
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
  * Module constructor
  *
  * @constructor
  * @param {(number|Fraction|Object)=} a
  * @param {number=} b
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
   * Multiplies two intervals
   *
   * @param {Interval} i
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
   * Divides two intervals
   *
   * @param {Interval} i
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
   * Takes the reciprocal/inverse of an interval
   */
  "recip": function() {
    let ret = keys(this);
    for (const i in ret) {
      ret[i] = this[i].neg();
    }
    return new Interval(ret);
  },

  /**
   * Raises an interval to a fractional power
   *
   * @param {Fraction} k
   */
  "pow": function(a,b) {
    let ret = keys(this);
    for (const i in ret) {
      ret[i] = this[i].mul(Fraction(a,b));
    }
    return new Interval(ret);
  },

  /**
   * Takes the nth root of an interval
   *
   * @param {integer} n
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
   * Converts an interval with fractional prime exponents to the nth root of a
   * fraction. Specifically, this function returns an `Object` of type
   * `{{k: Fraction, n:Integer}}`.
   */
  "toNthRoot": function() {
    let n_fr = Fraction(1);
    for (const i in keys(this)) {
      n_fr = n_fr.gcd(this[i]);
    }
    return { k: this.pow(n_fr.inverse()).toFrac(), n: n_fr.d };
  },

  /**
   * Converts an interval to its decimal value
   */
  "valueOf": function() {
    let ret = 1;
    for (const i in keys(this)) {
      ret *= Math.pow(i,this[i].valueOf())
    }
    return ret;
  },

  /**
   * Compares two intervals. Specifically, returns 0 if the intervals are equal,
   * 1 if the first interval is greater than the second, and -1 if the second
   * interval is greater than the first.
   *
   * @param {Interval} i
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
   * Checks if the two intervals are the same.
   *
   * @param {Interval} i
   */
  "equals": function(a,b) {
    return this.compare(a,b) == 0;
  },

  /**
   * If the given argument is a prime, returns a pair whose first element is the
   * exponent of that prime in this interval, and whose second element is the
   * interval without that prime (i.e. the rest of the factorization).
   *
   * For example, `Interval(8*5,7).factorOut(2)` returns `[3, Interval(5,7)]`.
   *
   * More generally, if the given argument is an interval `i` with factorization
   * `p1^e1 ... pm^em` (where the `pk`s are prime and in ascending order, and
   * each `ek > 0`), returns a pair `[g, this.div(i.pow(g))]` where `g` is the
   * smallest fraction such that `this.div(i.pow(g))` contains no factors of
   * `pm` (the largest prime in the factorization of `i`).
   *
   * For example, `Interval(9,8).factorOut(3,2)` returns `[2, Interval(1,2)]`.
   *
   * @param {Interval} i
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
   * Note that this function uses `factorOut` to preserve as much precision as
   * possible - for example, for any interval `i` and fraction `k`, then
   * `i.pow(k).valueOf_log(i) == k` *exactly*.
   *
   * @param {Interval} [i=Interval(2)]
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
   * For all intervals `i`, `j` this function satisfies the equality:
   * `i.div(i.red(j)).equals(j.pow(Math.floor(i.valueOf_log(j))))`
   *
   * @param {Interval} [i=Interval(2)]
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
   * For all intervals `i`, `j` this function satisfies the equality:
   * `i.div(i.reb(j)).equals(j.pow(Math.round(i.valueOf_log(j))))`
   *
   * @param {Interval} [i=Interval(2)]
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
   * Note that this function uses `factorOut` to preserve as much precision as
   * possible - for example, for any fraction `k`,
   * `Interval(2).pow(k).toCents() == k.mul(1200)` *exactly*.
   */
  "toCents": function() {
    const [e2, res] = this.factorOut(2);
    return e2.mul(1200) + Math.log(res.valueOf()) / Math.log(2) * 1200;
  },

  /**
   * Converts an interval to its Tenney harmonic distance, or Tenney height.
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
