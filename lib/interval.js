/**
 * The interval datatype, based on `Fraction` from `fraction.js` on npm
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module interval
 **/

const {unBigFraction, cachedLog2, maxKey, keys, primes} = require('./utils.js');
const pf = require('primes-and-factors');
const bigInt = require('big-integer');
const Fraction = require('fraction.js');
const BigFraction = require('fraction.js/bigfraction.js');

// helper functions used when constructing `Interval`s in this file

function parse(a,b) {
  // no arguments given
  if (a == undefined) {
    return { _fact: {} };
  }
  // two arguments given
  else if (b != undefined) {
    const [n,d] = [Number(a), Number(b)];
    if (n % 1 != 0 || !isFinite(n) || d % 1 != 0 || !isFinite(d)) {
      throw new Error(`Invalid integer parameters to Interval: ${a}, ${b}`);
    }
    return parseRatio(n,d);
  }
  // a single array argument given
  else if (Array.isArray(a)) {
    return parseMonzo(a);
  }
  // a single BigInt argument given
  else if (typeof a == "bigint") {
    return parseRatio(Number(a), 1);
  }
  else if (typeof a == "object") {
    // a single Fraction/BigFraction-like argument given
    if ("d" in a && "n" in a) {
      const n = Number("s" in a ? a["s"] * a["n"] : a["n"]);
      const d = Number(a["d"]);
      if (n % 1 != 0 || !isFinite(n) || d % 1 != 0 || !isFinite(d)) {
        throw new Error(`Invalid Fraction-like parameter to Interval: ${a}`);
      }
      return parseRatio(n,d);
    }
    // a single Interval-like argument given
    else if ("_fact" in a) {
      return parseFactorization(a._fact, a._isApprox);
    }
    // otherwise we assume a single object argument is a factorization object
    else {
      return parseFactorization(a, true);
    }
  }
  // a single number argument given
  else if (isFinite(Number(a))) {
    if (!(Number(a) > 0)) {
      throw new Error(`${a} <= 0 cannot be represented as an Interval`);
    }
    return parseNumber(Number(a));
  }
  // a single string argument given (which is not a valid Number string)
  else if (typeof a == "string") {
    const fr = BigFraction(a);
    const [n, d] = [Number(fr.s * fr.n), Number(fr.d)];
    if (n % 1 != 0 || !isFinite(n) || d % 1 != 0 || !isFinite(d)) {
      throw new Error(`Invalid string parameter to Interval: ${a}`);
    }
    return parseRatio(n, d);
  }
  // otherwise we error
  else {
    throw new Error(`Invalid parameter to Interval: ${a}`)
  }
}

function parseRatio(n,d) {
  if (!(n > 0) || !(d > 0)) {
    throw new Error(`${n}/${d} <= 0 cannot be represented as an Interval`);
  }
  const n_fact = pf.getPrimeExponentObject(n);
  const d_fact = pf.getPrimeExponentObject(d);
  let fact = {};
  for (const p of keys(n_fact, d_fact)) {
    fact[p] = BigFraction((n_fact[p] || 0) - (d_fact[p] || 0));
  }
  return { _fact: fact };
}

function parseMonzo(a) {
  let fact = {};
  let ps = primes();
  for (const ai of a) {
    const e = BigFraction(ai);
    const p = ps.next().value;
    if (!e.equals(0)) {
      fact[p] = e;
    }
  }
  return { _fact: fact };
}

function parseFactorization(obj, allowApproxs) {
  let [fact, isApprox] = [{}, false];
  for (const i of Object.keys(obj)) {
    const e = BigFraction(obj[i]);
    if (!e.equals(0)) {
      if (pf.isPrime(Number(i))) {
        fact[i] = e;
      }
      else if (allowApproxs && parseNumber(i)._isApprox) {
        fact[i] = e;
        isApprox = true;
      }
      else {
        throw new Error(`non-prime in factorization parameter to Interval: ${i}`);
      }
    }
  }
  if (isApprox) { return { _isApprox: true, _fact: fact }; }
  else          { return { _fact: fact }; }
}

function parseNumber(x) {
  if (x % 1 == 0) {
    return parseRatio(x, 1);
  }
  try {
    // covers nth roots between 1 and 12
    for (let [xpk, k] = [x, 1]; k <= 12; [xpk, k] = [xpk*x, k+1]) {
      let fact = numberToSmallFact(xpk);
      if (fact) {
        for (const p in fact) {
          fact[p] = fact[p].div(k);
        }
        return { _fact: fact };
      }
    }
    // covers fractional powers of 2, 3, and 5
    for (const base of [2,3,5]) {
      const e = Fraction(Math.log2(x) / cachedLog2(base));
      if (e.d < 200) {
        return { _fact: { 2: e } };
      }
    }
  }
  catch (e) {
  }
  // otherwise we return an approximation
  if (x >= 1) {
    return { _isApprox: true, _fact: {[x]: BigFraction(1)} };
  }
  else {
    return { _isApprox: true, _fact: {[1/x]: BigFraction(-1)} };
  }
}

function numberToSmallFact(x) {
  const fr = Fraction(x);
  if (isFinite(fr.n) && isFinite(fr.d)) {
    const fact = parseRatio(fr.n, fr.d)._fact;
    let sum_of_prime_digits = 0;
    for (const p in fact) {
      sum_of_prime_digits += p.toString().length;
    }
    if (sum_of_prime_digits < 10) {
      return fact;
    }
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
  if (p._isApprox) { this._isApprox = p._isApprox; }
  this._fact = p._fact;
}

Interval.prototype = {

  /**
   * Returns true iff the interval has a known prime factorization.
   *
   * @returns {boolean}
   */
  "hasFactors": function() {
    return !this._isApprox;
  },

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
    if (this._isApprox) {
      throw new Error("interval does not have a prime factorization");
    }
    if (!pf.isPrime(Number(p))) {
      throw new Error(p + " is not prime");
    }
    return !!this._fact[p];
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
    return this.hasExp(p) ? this._fact[p] : BigFraction(0);
  },

  /**
   * Returns the prime factorization of an interval as a list of pairs
   * `[[p1,e1], [p2,e2], ..., [pn,en]]` where each pn is prime such that
   * the interval is equal to `p1^e1 * p2^e2 * ... * pn^en`.
   *
   * @returns {Array.<Pair.<integer,BigFraction>>}
   */
  "factors": function() {
    if (this._isApprox) {
      throw new Error("interval does not have a prime factorization");
    }
    let fs = [];
    for (const p in this._fact) {
      if (!this._fact[p].equals(0n)) {
        fs.push([parseInt(p), unBigFraction(this._fact[p])]);
      }
    }
    return fs;
  },

  /**
   * Like `factors`, but returns an array of BigFraction.
   *
   * @returns {Array.<Pair.<integer,BigFraction>>}
   */
  "factorsBig": function() {
    if (this._isApprox) {
      throw new Error("interval does not have a prime factorization");
    }
    let fs = [];
    for (const p in this._fact) {
      if (!this._fact[p].equals(0n)) {
        fs.push([parseInt(p), this._fact[p]]);
      }
    }
    return fs;
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
    let ret_fact = {};
    for (const p of keys(this._fact, rhs._fact)) {
      ret_fact[p] = (this._fact[p] || BigFraction(0)).add(rhs._fact[p] || BigFraction(0));
    }
    return new Interval(ret_fact);
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
    let ret_fact = {};
    for (const p of keys(this._fact, rhs._fact)) {
      ret_fact[p] = (this._fact[p] || BigFraction(0)).sub(rhs._fact[p] || BigFraction(0));
    }
    return new Interval(ret_fact);
  },

  /**
   * Takes the reciprocal/inverse of an interval.
   *
   * e.g. `Interval(3,2).recip()` is exactly `Interval(2,3)`
   *
   * @returns {Interval}
   */
  "recip": function() {
    let ret_fact = {};
    for (const p in this._fact) {
      ret_fact[p] = this._fact[p].neg();
    }
    return new Interval(ret_fact);
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
    const k = BigFraction(a,b);
    let ret_fact = {};
    for (const p in this._fact) {
      ret_fact[p] = this._fact[p].mul(k);
    }
    return new Interval(ret_fact);
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
    if (this._isApprox) {
      return false;
    }
    for (const p in this._fact) {
      if (this._fact[p].d != 1n) {
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
    if (this._isApprox) {
      throw new Error("interval does not have a prime factorization");
    }
    let [n, d] = [1n, 1n];
    for (const p in this._fact) {
      if (this._fact[p].d == 1) {
        let m = this._fact[p].s * this._fact[p].n;
        if ((m >= 4096 || m <= -4096) && !allowUnbounded) {
          throw new Error("exponent of " + p + " too big: " + m)
        }
        if (m > 0) {
          n *= BigInt(p) ** m;
        }
        if (m < 0) {
          d *= BigInt(p) ** (-1n * m);
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
   * @param {Interval} i
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
    if (this._isApprox) {
      throw new Error("interval does not have a prime factorization");
    }
    let ret = 1n;
    for (const p in this._fact) {
      ret = BigInt(bigInt.lcm(ret, this._fact[p].d));
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
    if (this._isApprox) {
      throw new Error("interval does not have a prime factorization");
    }
    const {k,n} = this.toNthRootBig();
    if (n == 1) { return k.toFraction(); }
    if (n == 2) { return "sqrt(" + k.toFraction() + ")"; }
    return "root" + n + "(" + k.toFraction() + ")";
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
    for (const p in this._fact) {
      ret *= p ** this._fact[p].valueOf();
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
    const rhs = Interval(a,b);
    try {
      // this uses the facts that:
      // - x < y iff x/y < 1
      // - k^(1/n) < 1 iff k < 1
      // - n/d < 1 iff n - d < 0
      const {n, d} = this.div(rhs).toNthRootRaw().k;
      const t = n - d;
      return (0 < t) - (t < 0);
    }
    catch(e) {
      // if something goes wrong, just do an approximate comparison
      const [x, y] =
        this._isApprox || rhs._isApprox ? [this.valueOf(), rhs.valueOf()]
                                        : [this.valueOf_log(), rhs.valueOf_log()];
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
    const rhs = Interval(a,b);
    if (this._isApprox || rhs._isApprox) {
      return this.valueOf() == rhs.valueOf();
    }
    for (const p of keys(this._fact, rhs._fact)) {
      if (!(this._fact[p] || BigFraction(0)).equals(rhs._fact[p] || BigFraction(0))) {
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
    const gp = maxKey(base._fact);
    if (isFinite(gp)) {
      const g = (this._fact[gp] || BigFraction(0)).div(base._fact[gp]);
      let ret_fact = {};
      for (const p of keys(this._fact, base._fact)) {
        ret_fact[p] = (this._fact[p] || BigFraction(0)).sub((base._fact[p] || BigFraction(0)).mul(g));
      }
      return [unBigFraction(g), new Interval(ret_fact)];
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
   * possible - for example, for any interval `i` != 1 and fraction `k`, then
   * `i.pow(k).valueOf_log(i) == k.valueOf()` *exactly*.
   *
   * e.g. `Interval(3,2).pow(1,2).valueOf_log(3,2)` gives `0.5`
   *
   * @param {Interval} [i=Interval(2)]
   * @returns {number}
   */
  "valueOf_log": function(a,b) {
    const i = new Interval(a,b);
    // If no base is given, default to 2. We also have a specical case for 2:
    if ((a == undefined && b == undefined) || i.equals(2)) {
      let ret = 0;
      for (const p in this._fact) {
        ret += (this._fact[p] || BigFraction(0)).valueOf() * cachedLog2(p);
      }
      return ret;
    }
    // We also have a special case for base 1:
    if (i.equals(1)) {
      return Math.log2(this.valueOf()) / 0;
    }
    // Otherwise we just have an unfolded version of:
    // const [g, j] = this.factorOut(base);
    // return g.valueOf() + Math.log2(j.valueOf()) / Math.log2(base.valueOf());
    const [base, base_log2] = [i, i.valueOf_log()];
    const gp = maxKey(i._fact);
    const g = (this._fact[gp] || BigFraction(0)).div(base._fact[gp]);
    let ret = g.valueOf();
    for (const p of keys(this._fact, base._fact)) {
      const e = (this._fact[p] || BigFraction(0)).sub((base._fact[p] || BigFraction(0)).mul(g));
      ret += e.valueOf() * cachedLog2(p) / base_log2;
    }
    return ret;
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
    let ret = 0;
    for (const p in this._fact) {
      ret += (this._fact[p] || BigFraction(0)).mul(1200).valueOf() * cachedLog2(p);
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
    if (this._isApprox) {
      throw new Error("interval does not have a prime factorization");
    }
    let ret = 1;
    for (const p in this._fact) {
      ret *= p ** this._fact[p].abs().valueOf();
    }
    return ret;
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
    if (this._isApprox) {
      throw new Error("interval does not have a prime factorization");
    }
    let ret = 0;
    for (const p in this._fact) {
      ret += this._fact[p].abs().valueOf() * cachedLog2(p);
    }
    return ret;
  },

  /**
   * Converts an interval to a monzo.
   *
   * e.g. `Interval(11,9).toMonzo()` gives `[0,-2,0,0,1]`
   *
   * @returns {number}
   */
  "toMonzo": function() {
    if (this._isApprox) {
      throw new Error("interval does not have a prime factorization");
    }
    const gp = maxKey(this._fact);
    let [ret, isFrac] = [[], true];
    for (const p of primes()) {
      if (p > gp) { break; }
      ret.push(this._fact[p] || BigFraction(0));
      isFrac &= !this._fact[p] || this._fact[p].d == 1;
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
    return !this._isApprox && 2 <= p && Object.keys(this._fact).every(pi => pi <= p);
  },

  /**
   * Returns the smallest prime limit which contains this interval, i.e. the
   * smallest prime for which `inPrimeLimit` returns true.
   *
   * @returns {integer}
   */
  "primeLimit": function () {
    if (this._isApprox) {
      throw new Error("interval does not have a prime factorization");
    }
    return Math.max(2, ...Object.keys(this._fact).map(pi => parseInt(pi)));
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
  },

  // ~~~~~~~~~~~~~~~~~~~~~~~
  //  Approximate functions

  /**
   * Adds two intervals together, approximately if either interval is not a
   * fraction. NB: If either interval is not a fraction, the result may not
   * have a prime factorization, so functions like `factors`, `hasExp`, etc.
   * may not work.
   *
   * @param {Interval} i
   * @returns {Interval}
   */
  "add": function (a,b) {
    const rhs = new Interval(a,b);
    try {
      const [this_fr, rhs_fr] = [this.toFracRaw(), rhs.toFracRaw()];
      return new Interval(this_fr.n * rhs_fr.d + rhs_fr.n * this_fr.d,
                          this_fr.d * rhs_fr.d)
    }
    catch (e) {
      return new Interval(this.valueOf() + rhs.valueOf());
    }
  },

  /**
   * Subtracts two intervals, approximately if either interval is not a
   * fraction. Errors if the result would not be a valid interval (i.e. <= 0).
   * NB: If either interval is not a fraction, the result may not have a prime
   * factorization, so functions like `factors`, `hasExp`, etc. may not work.
   *
   * @param {Interval} i
   * @returns {Interval}
   */
  "sub": function (a,b) {
    const rhs = new Interval(a,b);
    const x = this.valueOf() - rhs.valueOf();
    if (x <= 0) {
      throw new Error("result of `sub` would be " + x + " <= 0")
    }
    try {
      const [this_fr, rhs_fr] = [this.toFracRaw(), rhs.toFracRaw()];
      return new Interval(this_fr.n * rhs_fr.d - rhs_fr.n * this_fr.d,
                          this_fr.d * rhs_fr.d)
    }
    catch (e) {
      return new Interval(x);
    }
  },

  /**
   * If "a : b : c" is an isoharmonic triple (e.g. 4 : 5 : 6), then
   * `c.isoMid(a)` is `b = (c + a)/2`. NB: This function uses `add`, be aware
   * of the caveats of using that method.
   *
   * e.g. `Interval(5).sqrt(2).isoMid()` is the golden ratio: (sqrt(5) + 1)/2
   *
   * @param {Interval} [i=Interval(1)]
   * @returns {Interval}
   */
  "isoMid": function(a,b) {
    return this.add(a,b).div(2);
  },

  /**
   * If "a : b : c" is an isoharmonic triple (e.g. 4 : 5 : 6), then
   * `b.isoMid(a)` is `c = 2*b - a`. NB: This method uses `sub`, be aware of
   * the caveats of using that method.
   *
   * @param {Interval} [i=Interval(1)]
   * @returns {Interval}
   */
  "isoUp": function(a,b) {
    return this.mul(2).sub(a,b);
  },

  /**
   * If "a : b : c" is an isoharmonic triple (e.g. 4 : 5 : 6), then
   * `c.isoMid(b)` is `a = 2*b - c`. NB: This method uses `sub`, be aware of
   * the caveats of using that method.
   *
   * @param {Interval} [i=Interval(1)]
   * @returns {Interval}
   */
  "isoDown": function(a,b) {
    return Interval(a,b).mul(2).sub(this);
  },

  /**
   * `i.iso1(n)` is the nth entry in the isoharmonic chord starting "1 : i".
   *
   * e.g. `Interval(5,4).iso1(2)` is `Interval(6,4)`,
   *      `Interval(5,4).iso1(3)` is `Interval(7,4)`,
   *      `Interval(5,4).iso1(4)` is `Interval(8,4)`, etc.
   *
   * You can also give negative or fractional values to this function.
   *
   * e.g. `Interval(5,4).iso1(-1)` is `Interval(3,4)`,
   *      `Interval(6,4).iso1(1/2)` is `Interval(5,4)`
   *
   * NB: This method uses `sub` and `add`, be aware of the caveats of using
   * these methods.
   *
   * @param {Fraction}
   * @returns {Interval}
   */
  "iso1": function(a,b) {
    const k = Fraction(a,b);
    const kcmp1 = k.compare(1);
    if (kcmp1 > 0) {
      return this.mul(k).sub(k.sub(1));
    }
    if (kcmp1 == 0) {
      return this.mul(1);
    }
    const kcmp0 = k.compare(0);
    if (kcmp0 > 0) {
      return this.mul(k).add(Fraction(1).sub(k));
    }
    if (kcmp0 == 0) {
      return Fraction(1);
    }
    return Interval(Fraction(1).sub(k)).sub(this.mul(k.neg()));
  },

  /**
   * Approximates the noble mediant of two intervals with integer prime
   * exponents, i.e. two intervals which can be expressed as fractions. NB: The
   * result of using this method will not have a prime factorization, so
   * functions like `factors`, `hasExp`, etc. will not work.
   *
   * e.g. `Interval(5,4).nobleMed(9,7).toCents()` is `422.4873963821663`
   *
   * @param {Interval} i
   * @returns {Interval}
   */
  "nobleMed": function(a,b) {
    const phi = 1.618033988749895; // Interval.phi.valueOf()
    let [f1, f2] = [this.toFracRaw(), Interval(a,b).toFracRaw()];
    return new Interval((Number(f1.n) + Number(f2.n) * phi) /
                        (Number(f1.d) + Number(f2.d) * phi));
  }

}

/**
 * The golden ratio: (sqrt(5) + 1)/2.
 *
 * @constant {Interval}
 */
Interval.phi = Interval(5).sqrt().isoMid();

module.exports = Interval;
