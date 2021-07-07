/**
 * The interval datatype, based on `Fraction` from `fraction.js` on npm
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module interval
 **/

const pf = require('primes-and-factors');
const bigInt = require('big-integer');
const Fraction = require('fraction.js');
const BigFraction = require('fraction.js/bigfraction.js');

function unBigFraction(fr) {
  return Fraction(Number(fr.s * fr.n), Number(fr.d));
}

let cached_logs = {}
function cachedLog2(i) {
  let entry = cached_logs[i];
  if (entry == undefined) {
    const logi = Math.log2(i);
    entry = [logi, 1200*logi];
    cached_logs[i] = entry;
  }
  return entry;
}

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
      throw new Error("non-positive number cannot be converted into an interval");
    }
    const afs = pf.getPrimeExponentObject(Number(a));
    const bfs = pf.getPrimeExponentObject(Number(b));
    let ret = keys(afs,bfs);
    for (const i in ret) {
      ret[i] = BigFraction((afs[i] || 0) - (bfs[i] || 0));
    }
    return ret;
  }
  // if the input is a monzo
  else if (Array.isArray(a)) {
    let ret = {};
    if (a.length > 0) {
      if (!BigFraction(a[0]).equals(0)) {
        ret[2] = BigFraction(a[0]);
      }
      let i = 1;
      for (let p = 3; i < a.length; p += 2) {
        if (pf.isPrime(p)) {
          if (!BigFraction(a[i]).equals(0)) {
            ret[p] = BigFraction(a[i]);
          }
          i++;
        }
      }
    }
    return ret;
  }
  else if (typeof a == "object") {
    // if the input is a Fraction object
    if ("d" in a && "n" in a) {
      let sn = a["n"];
      if ("s" in a) {
        sn *= a["s"];
      }
      return parse(sn, a["d"]);
    }
    // if the input is an Interval object
    else {
      let allPrimes = true
      let ret = {};
      for (const i of Object.keys(a)) {
        allPrimes &= pf.isPrime(Number(i));
        if (!BigFraction(a[i]).equals(0)) {
          ret[i] = BigFraction(a[i]);
        }
      }
      if (allPrimes) {
        return ret;
      } else {
        throw new Error("invalid arguments to Interval: " + a + ", " + b);
      }
    }
  }
  else {
    return parse(BigFraction(a));
  }
}

/**
  * Constructs an `Interval`. Valid argument combinations are two integers
  * (e.g. `Interval(3,2)`), a single number (e.g. `Interval(3/2)`), a
  * factorization (e.g. `Interval({2: -1, 3: 1})`), an array of numbers or
  * `Fraction`s representing a monzo (e.g. `Interval([-1,1])`), a `Fraction`,
  * or an `Interval`.
  *
  * As a convention, all functions which have a JSDoc parameter of type
  * `Interval` should be able to accept any of these argument combinations in
  * place of that parameter. For example, `mul` in this file, or
  * `bestRationalApproxsByHeight` in `approx.js`.
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
  for (const i of Object.keys(p)) {
    this[i] = p[i]
  }

}

Interval.prototype = {

  /**
   * Returns true iff the exponent of the given prime in an interval's prime
   * factorization is nonzero.
   *
   * e.g. `Interval(24).hasExp(2)` is `true`, `Interval(8).hasExp(5)` is `false`
   *
   * @param {integer} p a prime number
   * @returns {boolean}
   */
  "hasExp": function(p) {
    if (!pf.isPrime(Number(p))) {
      throw new Error(p + " is not prime");
    }
    return !!this[p];
  },

  /**
   * Returns the exponent of the given prime in an interval's prime
   * factorization.
   *
   * e.g. `Interval(24).expOf(2)` is `3`, `Interval(8).expOf(5)` is `0`
   *
   * @param {integer} p a prime number
   * @returns {Fraction}
   */
  "expOf": function(p) {
    return unBigFraction(this.expOfBig(p));
  },

  /**
   * Like `expOf`, but returns a BigFraction.
   *
   * @param {integer} p a prime number
   * @returns {BigFraction}
   */
  "expOfBig": function(p) {
    return this.hasExp(p) ? this[p] : BigFraction(0);
  },

  /**
   * Returns the prime factorization of an interval as a list of pairs
   * `[[p1,e1], [p2,e2], ..., [pn,en]]` where each pn is prime such that
   * the interval is equal to `p1^e1 * p2^e2 * ... * pn^en`.
   *
   * @returns {Array.<Pair.<integer,BigFraction>>}
   */
  "factors": function() {
    let ret = [];
    for (const [p,e] of Object.entries(this)) {
      if (!e.equals(0n)) {
        ret.push([parseInt(p), unBigFraction(e)]);
      }
    }
    return ret;
  },

  /**
   * Like `factors`, but returns an array of BigFraction.
   *
   * @returns {Array.<Pair.<integer,BigFraction>>}
   */
  "factorsBig": function() {
    let ret = [];
    for (const [p,e] of Object.entries(this)) {
      if (!e.equals(0n)) {
        ret.push([parseInt(p), e]);
      }
    }
    return ret;
  },

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
      ret[i] = (this[i] || BigFraction(0)).add(rhs[i] || BigFraction(0));
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
      ret[i] = (this[i] || BigFraction(0)).sub(rhs[i] || BigFraction(0));
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
      ret[i] = this[i].mul(a,b);
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
    return this.pow(1,a);
  },

  /**
   * The square root of an interval, i.e. `pow(1,2)`. or `root(2)`
   *
   * @returns {Interval}
   */
  "sqrt": function() {
    return this.root(2);
  },

  /**
   * Checks whether an interval has integer prime exponents, i.e. whether the
   * interval can be expressed as a fraction
   *
   * e.g. `Interval(3,2).isFrac()` returns `true`
   *
   * e.g. `Interval(2).sqrt().isFrac()` returns `false`
   *
   * @returns {bool}
   */
  "isFrac": function() {
    for (const i of Object.keys(this)) {
      if (this[i].d != 1n) {
        return false;
      }
    }
    return true;
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
    return unBigFraction(this.toFracBig());
  },

  /**
   * Like `toFrac`, but returns a `BigFraction`
   *
   * @returns {BigFraction}
   */
  "toFracBig": function() {
    return BigFraction(this.toFracRaw());
  },

  /**
   * Like `toFrac`, but returns an unreduced `BigFraction`
   *
   * @param {bool} allowUnbounded if true, supresses errors related to the
   *                              exponents being too big
   * @returns {{s: BigInt, n:BigInt, d:BigInt}}
   */
  "toFracRaw": function(allowUnbounded) {
    let [n, d] = [1n, 1n];
    for (const i of Object.keys(this)) {
      if (this[i].d == 1) {
        let m = this[i].s * this[i].n;
        if ((m >= 4096 || m <= -4096) && !allowUnbounded) {
          throw new Error("exponent of " + i + " too big: " + m)
        }
        if (m > 0) {
          n *= BigInt(i) ** m;
        }
        if (m < 0) {
          d *= BigInt(i) ** (-1n * m);
        }
      } else {
        throw new Error("interval does not have integer exponents");
      }
    }
    return {s: 1n, n: n, d: d};
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
    let [f1, f2] = [this.toFracRaw(), Interval(a,b).toFracRaw()];
    return new Interval(f1.n + f2.n, f1.d + f2.d);
  },

  /**
   * Converts any interval to its representation as the nth root of a fraction.
   *
   * e.g. `Interval(3,2).toNthRoot()` is exactly `{k: Interval(3,2).toFrac(), n: 1}`
   *
   * e.g. `Interval({2: 1/2, 5: 1/3})` is exactly `{k: Interval({2: 3, 5: 2}).toFrac(), n: 6}`
   *
   * @returns {{k: Fraction, n: Integer}}
   */
  "toNthRoot": function() {
    const {k, n} = this.toNthRootBig();
    return { k: unBigFraction(k), n: Number(n) };
  },

  /**
   * Like `toNthRoot`, but returns a `BigFraction`.
   *
   * @returns {{k: BigFraction, n: Integer}}
   */
  "toNthRootBig": function() {
    const {k, n} = this.toNthRootRaw();
    return { k: BigFraction(k), n: n };
  },

  /**
   * Like `toNthRoot`, but returns an unreduced `BigFraction`.
   *
   * @returns {{k: {s: BigInt, n:BigInt, d:BigInt}, n: Integer}}
   */
   "toNthRootRaw": function() {
     const n = this.minPowFracBig();
     return { k: this.pow(n).toFracRaw(), n: n };
   },

   /**
    * Returns the smallest integer such that `this.pow(n).isFrac()` is true, i.e.
    * the `n` in `this.toNthRoot()`.
    *
    * @returns {Integer}
    */
   "minPowFrac": function() {
     return Number(this.minPowFracBig());
   },

   /**
    * Like `minPowFrac`, but returns a `BigInt`.
    *
    * @returns {BigInt}
    */
   "minPowFracBig": function() {
     let ret = 1n;
     for (const i of Object.keys(this)) {
       ret = BigInt(bigInt.lcm(ret, this[i].d));
     }
     return ret;
   },

  /**
   * Converts any interval to a string of its representation as the nth root of
   * a fraction.
   *
   * e.g. `Interval(3,2).toNthRoot()` gives `"3/2"`
   *
   * e.g. `Interval({2: 1/2, 5: 1/3})` gives `"root6(200)"`
   *
   * @returns {{k: Fraction, n: Integer}}
   */
  "toNthRootString": function() {
    const {k,n} = this.toNthRootBig();
    if (n == 1) { return k.toFraction(); }
    if (n == 2) { return "sqrt(" + k.toFraction() + ")" }
    return "root" + n + "(" + k.toFraction() + ")"
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
    for (const i of Object.keys(this)) {
      ret *= i ** this[i].valueOf()
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
    try {
      // this uses the facts that:
      // - x < y iff x/y < 1
      // - k^(1/n) < 1 iff k < 1
      // - n/d < 1 iff n - d < 0
      const {n, d} = this.div(a,b).toNthRootRaw().k;
      const t = n - d;
      return (0 < t) - (t < 0);
    }
    catch(e) {
      // if something goes wrong, just do an approximate comparison
      const x = this.valueOf_log();
      const y = Interval(a,b).valueOf_log();
      if (x == y) { return 0; }
      else { return (y < x) - (x < y); }
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
    const rhs = parse(a,b);
    let ret = keys(this,rhs);
    for (const i in ret) {
      if (!(this[i] || BigFraction(0)).equals(rhs[i] || BigFraction(0))) {
        return false;
      }
    }
    return true;
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
      const g = (this[gp] || BigFraction(0)).div(base[gp]);
      let res = keys(this, base);
      for (const i in res) {
        res[i] = (this[i] || BigFraction(0)).sub((base[i] || BigFraction(0)).mul(g));
      }
      return [unBigFraction(g), new Interval(res)];
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
    // The below is just an unfolded version of:
    // const [g, res] = this.factorOut(base);
    // return g.valueOf() + Math.log(res.valueOf()) / Math.log(base.valueOf());
    const gp = Math.max(...Object.keys(base));
    if (isFinite(gp)) {
      const g = (this[gp] || BigFraction(0)).div(base[gp]);
      let ret = g.valueOf();
      for (const i in keys(this, base)) {
        const e = (this[i] || BigFraction(0)).sub((base[i] || BigFraction(0)).mul(g));
        ret += e.valueOf() * cachedLog2(i)[0];
      }
      return ret;
    }
    else {
      return NaN;
    }
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
   * Returns the "distance" between two intervals, or if no second interval is
   * given, between the first interval and `1`. By "distance" here, we mean
   * whichever of `i1.div(i2)` and `i2.div(i1)` is greater than `1`.
   *
   * e.g. `Interval(5,4).distance(3,2)` is exactly `Interval(6,5)`
   *      `Interval(2,3).distance()` is exactly `Interval(3,2)`
   *
   * @param {Interval} [i=Interval(1)]
   * @returns {Interval}
   */
   "distance": function(a,b) {
     const q = this.div(a,b);
     return q.compare(1) < 0 ? q.recip() : q;
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
    // The below is just an unfolded version of:
    // const [e2, res] = this.factorOut(2);
    // return e2.mul(1200).valueOf() + Math.log(res.valueOf()) / Math.log(2) * 1200;
    let ret = (this[2] || BigFraction(0)).mul(1200).valueOf();
    for (const i of Object.keys(this)) {
      if (i != 2) {
        ret += (this[i] || BigFraction(0)).valueOf() * cachedLog2(i)[1];
      }
    }
    return ret;
  },

  /**
   * Converts an interval to its Benedetti height
   *
   * e.g. `Interval(3,2).benedettiHeight()` gives `6`
   *
   * @returns {number}
   */
  "benedettiHeight": function() {
    let ret = Interval(1);
    for (const i of Object.keys(this)) {
      ret[i] = this[i].abs();
    }
    return ret.valueOf();
  },

  /**
   * Converts an interval to its Tenney harmonic distance, or Tenney height,
   * the log base 2 of its Benedetti height
   *
   * e.g. `Interval(3,2).tenneyHD()` gives `2.584962500721156`
   *
   * @returns {number}
   */
  "tenneyHD": function() {
    let ret = Interval(1);
    for (const i of Object.keys(this)) {
      ret[i] = this[i].abs();
    }
    return ret.valueOf_log();
  },

  /**
   * Converts an interval to a monzo.
   *
   * e.g. `Interval(11,9).toMonzo()` gives `[0,-2,0,0,1]`
   *
   * @returns {number}
   */
  "toMonzo": function() {
    let max_p = 0;
    for (const i of Object.keys(this)) {
      max_p = Math.max(max_p, i);
    }
    let [ret, isFrac] = [[], true];
    if (2 <= max_p) {
      ret.push(this[2] || BigFraction(0));
      isFrac &= !this[2] || this[2].d == 1;
    }
    for (let p = 3; p <= max_p; p += 2) {
      if (pf.isPrime(p)) {
        ret.push(this[p] || BigFraction(0));
        isFrac &= !this[p] || this[p].d == 1;
      }
    }
    return ret.map(r => isFrac ? Number(r.s * r.n) : unBigFraction(r));
  },

  /**
   * Returns true iff the interval is in the given prime limit, i.e. has a
   * factorization only containing primes smaller than the given prime number.
   *
   * e.g. `Interval(11,9).inPrimeLimit(11)` is true but
   *      `Interval(11,9).inPrimeLimit(7)` is false
   *
   * @returns {boolean}
   */
  "inPrimeLimit": function (p) {
    return 2 <= p && Object.keys(this).every(i => i <= p);
  },

  /**
   * Returns the smallest prime limit which contains this interval, i.e. the
   * smallest prime for which `inPrimeLimit` returns true.
   *
   * @returns {integer}
   */
  "primeLimit": function () {
    return Math.max(2, ...Object.keys(this).map(i => parseInt(i)));
  },

  /**
   * Returns true iff the interval is a fraction in the given odd limit, i.e.
   * has numerator and denominor which are less than the given number if they're
   * odd, respectively.
   *
   * e.g. `Interval(16,9).inOddLimit(9)` is true but
   *      `Interval(16,9).inOddLimit(7)` is false
   *
   * @returns {boolean}
   */
  "inOddLimit": function (o) {
    if (!this.isFrac()) { return false; }
    const {n,d} = this.toFracBig();
    return (n % 2n == 0n || n <= BigInt(o)) && (d % 2n == 0n || d <= BigInt(o));
  },

  /**
   * Returns the smallest odd limit which contains this interval, i.e. the
   * smallest odd number for which `inOddLimit` returns true.
   *
   * @returns {integer}
   */
  "oddLimit": function () {
    const {n,d} = this.toFrac();
    return n % 2 == 0 ? d : d % 2 == 0 ? n : Math.max(n,d);
  }

}

module.exports = Interval;
