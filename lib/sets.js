/**
 * Generators for sets of intervals
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module sets
 **/

const {cachedLog2} = require('./utils.js');
const Fraction = require('fraction.js');
const Interval = require('./interval.js');

// For a given interval with factorization `p1^e1 ... pm^em` (where `pk` is
// prime and `ek > 0` for all `k`), `signPerms(intv)` is the array of all
// intervals with factorizations `p1^(+/- e1) ... pm^(+/- em)`. For example, if
// `i = 45` then `i = 3^2 * 5^1` and
// `signPerms(i) = [ 3^2 * 5^1, 3^(-2) * 5^1, 3^2 * 5^(-1), 3^(-2) * 5(-1) ]`.
// Note that we also include the log2 values of each interval as well.
function* signPerms(intv) {
  const intv_fact = intv.factors();
  for (let bits = 0; bits < (1 << intv_fact.length); bits++) {
    let [i, fact, logval] = [0, {}, 0];
    for (const [p,e] of intv_fact) {
      fact[p] = e.mul((bits & (1 << i)) == 0 ? 1 : -1);
      logval += fact[p].valueOf() * cachedLog2(p);
      i++;
    }
    yield [Interval(fact), logval];
  }
}

/**
  * A helper function for generating ratios by no-2s Benedetti/Tenney height.
  *
  * @param {integer} startOdd the no-2s Benedetti height to start with (inclusive)
  * @param {integer} endOdd the no-2s Benedetti height to end with (exclusive)
  * @param {Object} [opts]
  * @param {integer} [opts.primeLimit]
  * @yields {Pair.<Interval, Iterable.<Pair.<Interval, integer>>>}
  */
function* ratioPermsByNo2sHeight(startOdd, endOdd, opts) {
  if (startOdd == undefined || startOdd < 1 || endOdd < -1) {
    throw new Error("Invalid arguments to ratiosByNo2sHeight: startOdd = " + start + ", endOdd = " + end);
  }
  const start = Math.ceil((startOdd-1)/2);
  const end = Math.floor((endOdd-1)/2);
  if (opts == undefined) { opts = {}; }
  const {primeLimit} = opts;
  for (let h = start; !isFinite(end) || h < end; h++) {
    const i = Interval(2*h + 1);
    if (primeLimit && !i.inPrimeLimit(primeLimit)) {
      continue;
    }
    yield [i, signPerms(i)];
  }
}

/**
  * Generates ratios between 1 and 2 sorted by the Benedetti height of the
  * ratio with all factors of 2 removed (or equivalently, the Tenney height of
  * the ratio with all factors of 2 removed). To specify `start` and `end`
  * using Tenney height, use `2 ** tenneyStart` and `2 ** tenneyEnd`.
  *
  * @param {integer} start the no-2s Benedetti height to start with (inclusive)
  * @param {integer} end the no-2s Benedetti height to end with (exclusive)
  * @param {Object} [opts]
  * @param {integer} [opts.primeLimit]
  * @yields {Fraction}
  */
function* redRatiosByNo2sHeight(start, end, opts) {
  const {oddLimit} = opts == undefined ? {} : opts;
  for (const [i, perms] of ratioPermsByNo2sHeight(start, end, opts)) {
    for (const [j_no2s, j_no2s_logval] of perms) {
      const j = j_no2s.red();
      if (oddLimit && !j.inOddLimit(oddLimit)) {
        continue;
      }
      yield j.toFrac();
    }
  }
}

/**
  * Generates ratios between 1/sqrt(2) and sqrt(2) sorted by the Benedetti
  * height of the ratio with all factors of 2 removed (or equivalently, the
  * Tenney height of the ratio with all factors of 2 removed). To specify
  * `start` and `end` using Tenney height, use `2 ** tenneyStart` and
  * `2 ** tenneyEnd`.
  *
  * @param {integer} start the no-2s Benedetti height to start with (inclusive)
  * @param {integer} end the no-2s Benedetti height to end with (exclusive)
  * @param {Object} [opts]
  * @param {integer} [opts.primeLimit]
  * @yields {Fraction}
  */
function* rebRatiosByNo2sHeight(start, end, opts) {
  const {oddLimit} = opts == undefined ? {} : opts;
  for (const [i, perms] of ratioPermsByNo2sHeight(start, end, opts)) {
    for (const [j_no2s, j_no2s_logval] of perms) {
      const j = j_no2s.reb();
      if (oddLimit && !j.inOddLimit(oddLimit)) {
        continue;
      }
      yield j.toFrac();
    }
  }
}

/**
  * A helper function for generating ratios by Benedetti/Tenney height.
  *
  * @param {integer} start the Benedetti height to start with (inclusive)
  * @param {integer} end the Benedetti height to end with (exclusive)
  * @param {Object} [opts]
  * @param {integer} [opts.primeLimit]
  * @param {integer} [opts.oddLimit]
  * @yields {Pair.<Interval, Iterable.<Pair.<Interval, integer>>>}
  */
function* ratioPermsByHeight(start, end, opts) {
  if (start == undefined || start < 1 || end < -1) {
    throw new Error("Invalid arguments to ratiosByHeight: start = " + start + ", end = " + end);
  }
  if (opts == undefined) { opts = {}; }
  let {primeLimit, oddLimit} = opts;
  for (let h = Math.ceil(start); !isFinite(end) || h < end; h++) {
    const i = Interval(h);
    if (primeLimit && !i.inPrimeLimit(primeLimit)) {
      continue;
    }
    function* perms() {
      for (const [j, j_logval] of signPerms(i)) {
        if (oddLimit && !j.inOddLimit(oddLimit)) {
          continue;
        }
        yield [j, j_logval];
      }
    }
    yield [i, perms()];
  }
}

/**
  * Generates ratios sorted by the Benedetti height of the ratio (or
  * equivalently, the Tenney height of the ratio). To specify `start` and `end`
  * using Tenney height, use `2 ** tenneyStart` and `2 ** tenneyEnd`.
  *
  * @param {integer} start the Benedetti height to start with (inclusive)
  * @param {integer} end the Benedetti height to end with (exclusive)
  * @param {Object} [opts]
  * @param {integer} [opts.primeLimit]
  * @yields {Fraction}
  */
function* ratiosByHeight(start, end, opts) {
  for (const [i, perms] of ratioPermsByHeight(start, end, opts)) {
    for (const [j, j_logval] of perms) {
      yield j.toFrac();
    }
  }
}

/**
  * Generates all ratios with the given denominator in the given range [lo, hi],
  * or between 1 and 2 if no range is given, sorted by value.
  *
  * @param {integer} d
  * @param {Object} [opts]
  * @param {integer} [opts.primeLimit]
  * @param {integer} [opts.oddLimit]
  * @param {Interval} [opts.lo] defaults to 1
  * @param {Interval} [opts.hi] defaults to 2
  * @yields {Fraction}
  */
function* ratiosWithDenom(d, opts) {
  if (!isFinite(d) || d < 1) {
    throw new Error("Invalid argument to ratiosWithDenom: d = " + d);
  }
  if (opts == undefined) { opts = {}; }
  let {primeLimit, oddLimit, lo, hi} = opts;
  lo = Interval(lo == undefined ? 1 : lo);
  hi = Interval(hi == undefined ? 2 : hi);
  if (oddLimit && d % 2 != 0 && d > oddLimit) {
    return;
  }
  const nLo = Math.ceil(lo.mul(d).valueOf());
  const nHi = Math.floor(hi.mul(d).valueOf());
  for (let n = nLo; n <= nHi; n++) {
    const r = Fraction(n,d);
    // if n/d reduces, we've seen it already - so we can safely skip
    if (r.d != d) {
      continue;
    }
    if (oddLimit && r.n % 2 != 0 && r.n > oddLimit) {
      continue;
    }
    if (primeLimit && !Interval(r).inPrimeLimit(primeLimit)) {
      continue;
    }
    yield r;
  }
}

/**
  * Generates all ratios with the given numerator in the given range [lo, hi],
  * or between 1 and 2 if no range is given, sorted by value.
  *
  * @param {integer} n
  * @param {Object} [opts]
  * @param {integer} [opts.primeLimit]
  * @param {integer} [opts.oddLimit]
  * @param {Interval} [opts.lo] defaults to 1
  * @param {Interval} [opts.hi] defaults to 2
  * @yields {Fraction}
  */
function* ratiosWithNumer(n, opts) {
  if (!isFinite(n) || n < 1) {
    throw new Error("Invalid argument to ratiosWithNumer: n = " + n);
  }
  if (opts == undefined) { opts = {}; }
  let {primeLimit, oddLimit, lo, hi} = opts;
  lo = Interval(lo == undefined ? 1 : lo);
  hi = Interval(hi == undefined ? 2 : hi);
  if (oddLimit && n % 2 != 0 && n > oddLimit) {
    return;
  }
  const dLo = Math.ceil(hi.recip().mul(n).valueOf());
  const dHi = Math.floor(lo.recip().mul(n).valueOf());
  for (let d = dHi; d >= dLo; d--) {
    const r = Fraction(n,d);
    // if n/d reduces, we've seen it already - so we can safely skip
    if (r.n != n) {
      continue;
    }
    if (oddLimit && r.d % 2 != 0 && r.d > oddLimit) {
      continue;
    }
    if (primeLimit && !Interval(r).inPrimeLimit(primeLimit)) {
      continue;
    }
    yield r;
  }
}

/**
  * Generates all ratios with denominators in the given range [start, end] with
  * values in the given range [lo, hi], or between 1 and 2 if no range is given,
  * sorted by denominator.
  *
  * @param {integer} start the lowest denominator to include (inclusive)
  * @param {integer} end the highest denominator to include (inclusive)
  * @param {Object} [opts]
  * @param {integer} [opts.primeLimit]
  * @param {integer} [opts.oddLimit]
  * @param {Interval} [opts.lo] defaults to 1
  * @param {Interval} [opts.hi] defaults to 2
  * @yields {Fraction}
  */
function* ratiosByDenom(start, end, opts) {
  if (start == undefined || start < 1 || end < -1) {
    throw new Error("Invalid arguments to ratiosByDenom: start = " + start + ", end = " + end);
  }
  if (opts == undefined) { opts = {}; }
  let {primeLimit, oddLimit, lo, hi} = opts;
  lo = Interval(lo == undefined ? 1 : lo);
  hi = Interval(hi == undefined ? 2 : hi);
  for (let d = start; !isFinite(end) || d < end; d++) {
    for (const r of ratiosWithDenom(d, opts)) {
      yield r;
    }
  }
}

/**
  * Generates all ratios with numerators in the given range [start, end] with
  * values in the given range [lo, hi], or between 1 and 2 if no range is given,
  * sorted by numerator.
  *
  * @param {integer} start the lowest numerator to include (inclusive)
  * @param {integer} end the highest numerator to include (inclusive)
  * @param {Object} [opts]
  * @param {integer} [opts.primeLimit]
  * @param {integer} [opts.oddLimit]
  * @param {Interval} [opts.lo] defaults to 1
  * @param {Interval} [opts.hi] defaults to 2
  * @yields {Fraction}
  */
function* ratiosByNumer(start, end, opts) {
  if (start == undefined || start < 1 || end < -1) {
    throw new Error("Invalid arguments to ratiosByNumer: start = " + start + ", end = " + end);
  }
  if (opts == undefined) { opts = {}; }
  let {primeLimit, oddLimit, lo, hi} = opts;
  lo = Interval(lo == undefined ? 1 : lo);
  hi = Interval(hi == undefined ? 2 : hi);
  for (let n = start; !isFinite(end) || n < end; n++) {
    for (const r of ratiosWithNumer(n, opts)) {
      yield r;
    }
  }
}

/**
  * Generates all ratios with denominators in the given range [start, end] with
  * values in the given range [lo, hi], or between 1 and 2 if no range is given,
  * sorted by value.
  *
  * @param {integer} start the lowest denominator to include (inclusive)
  * @param {integer} end the highest denominator to include (inclusive)
  * @param {Object} [opts]
  * @param {integer} [opts.primeLimit]
  * @param {integer} [opts.oddLimit]
  * @param {Interval} [opts.lo] defaults to 1
  * @param {Interval} [opts.hi] defaults to 2
  * @yields {Fraction}
  */
function* ratiosByDenomSorted(start, end, opts) {
  if (start == undefined || start < 1 || end < -1) {
    throw new Error("Invalid arguments to ratiosByDenom: start = " + start + ", end = " + end);
  }
  if (opts == undefined) { opts = {}; }
  let {primeLimit, oddLimit, lo, hi} = opts;
  lo = Interval(lo == undefined ? 1 : lo);
  hi = Interval(hi == undefined ? 2 : hi);
  let ret = [];
  for (let d = start; !isFinite(end) || d < end; d++) {
    let i = 0;
    for (const r of ratiosWithDenom(d, opts)) {
      let added = false;
      for (; !added && i < ret.length; i++) {
        if (r.compare(ret[i]) < 0) {
          ret.splice(i, 0, r);
          added = true;
        }
      }
      if (!added) {
        ret.push(r);
      }
    }
  }
  for (const r of ret) {
    yield r;
  }
}

/**
  * Generates all ratios with numerators in the given range [start, end] with
  * values in the given range [lo, hi], or between 1 and 2 if no range is given,
  * sorted by numerator.
  *
  * @param {integer} start the lowest numerator to include (inclusive)
  * @param {integer} end the highest numerator to include (inclusive)
  * @param {Object} [opts]
  * @param {integer} [opts.primeLimit]
  * @param {integer} [opts.oddLimit]
  * @param {Interval} [opts.lo] defaults to 1
  * @param {Interval} [opts.hi] defaults to 2
  * @yields {Fraction}
  */
function* ratiosByNumerSorted(start, end, opts) {
  if (start == undefined || start < 1 || end < -1) {
    throw new Error("Invalid arguments to ratiosByDenom: start = " + start + ", end = " + end);
  }
  if (opts == undefined) { opts = {}; }
  let {primeLimit, oddLimit, lo, hi} = opts;
  lo = Interval(lo == undefined ? 1 : lo);
  hi = Interval(hi == undefined ? 2 : hi);
  let ret = [];
  for (let n = start; !isFinite(end) || n < end; n++) {
    let i = 0;
    for (const r of ratiosWithNumer(n, opts)) {
      let added = false;
      for (; !added && i < ret.length; i++) {
        if (r.compare(ret[i]) < 0) {
          ret.splice(i, 0, r);
          added = true;
        }
      }
      if (!added) {
        ret.push(r);
      }
    }
  }
  for (const r of ret) {
    yield r;
  }
}

/**
  * Generates all ratios in the given odd limit which are not in the previous
  * odd limit with values in the given range [lo, hi], or between 1 and 2 if no
  * range is given, sorted by value unless `opts.unsorted` is given and true.
  *
  * @param {integer} oddLimit
  * @param {Object} [opts]
  * @param {integer} [opts.primeLimit]
  * @param {Interval} [opts.lo] defaults to 1
  * @param {Interval} [opts.hi] defaults to 2
  * @param {boolean} [opts.unsorted] defaults to false
  * @yields {Fraction}
  */
function* newRatiosInOddLimit(oddLimit, opts) {
  if (oddLimit == undefined || oddLimit % 2 == 0 || oddLimit < 1) {
    throw new Error("Invalid argument to newRatiosInOddLimit: oddLimit = " + oddLimit);
  }
  if (opts == undefined) { opts = {}; }
  let {unsorted} = opts;
  const ropts = Object.assign({}, opts, {oddLimit: oddLimit, unsorted: undefined});
  let ret = [];
  // ratios with denominator = oddLimit
  for (const r of ratiosWithDenom(oddLimit, ropts)) {
    if (unsorted) {
      yield r;
    }
    else {
      ret.push(r);
    }
  }
  // ratios with numerator = oddLimit
  let i = 0;
  for (const r of ratiosWithNumer(oddLimit, ropts)) {
    // if oddLimit == 1, don't add 1/1 twice!
    if (r.equals(1)) {
      continue;
    }
    if (unsorted) {
      yield r;
    }
    else {
      let added = false;
      for (; !added && i < ret.length; i++) {
        if (r.compare(ret[i]) < 0) {
          ret.splice(i, 0, r);
          added = true;
        }
      }
      if (!added) {
        ret.push(r);
      }
    }
  }
  if (!unsorted) {
    for (const r of ret) {
      yield r;
    }
  }
}

/**
  * Generates all ratios in the given odd limit with values in the given range
  * [lo, hi], or between 1 and 2 if no range is given, sorted by minimum odd
  * limit.
  *
  * @param {integer} oddLimit
  * @param {Object} [opts]
  * @param {integer} [opts.primeLimit]
  * @param {Interval} [opts.lo] defaults to 1
  * @param {Interval} [opts.hi] defaults to 2
  * @param {boolean} [opts.unsorted] whether or not to sort the intervals
                                     within each minimum odd limit (see
                                     `newRatiosInOddLimit`), defaults to false
  * @yields {Fraction}
  */
function* ratiosInOddLimit(oddLimit, opts) {
  if (opts == undefined) { opts = {}; }
  let {primeLimit, lo, hi} = opts;
  lo = Interval(lo == undefined ? 1 : lo);
  hi = Interval(hi == undefined ? 2 : hi);
  for (let o = 1; o <= oddLimit; o += 2) {
    for (const r of newRatiosInOddLimit(o, opts)) {
      yield r;
    }
  }
}

/**
  * Generates all ratios in the given odd limit with values in the given range
  * [lo, hi], or between 1 and 2 if no range is given, sorted by value.
  *
  * @param {integer} oddLimit
  * @param {Object} [opts]
  * @param {integer} [opts.primeLimit]
  * @param {Interval} [opts.lo] defaults to 1
  * @param {Interval} [opts.hi] defaults to 2
  * @yields {Fraction}
  */
function* ratiosInOddLimitSorted(oddLimit, opts) {
  if (opts == undefined) { opts = {}; }
  let {primeLimit, lo, hi} = opts;
  lo = Interval(lo == undefined ? 1 : lo);
  hi = Interval(hi == undefined ? 2 : hi);
  let ret = []
  for (let o = 1; o <= oddLimit; o += 2) {
    let i = 0;
    for (const r of newRatiosInOddLimit(o, Object.assign({}, opts, {unsorted: false}))) {
      let added = false;
      for (; !added && i < ret.length; i++) {
        if (r.compare(ret[i]) < 0) {
          ret.splice(i, 0, r);
          added = true;
        }
      }
      if (!added) {
        ret.push(r);
      }
    }
  }
  for (const r of ret) {
    yield r;
  }
}

module.exports.ratioPermsByNo2sHeight = ratioPermsByNo2sHeight;
module.exports.redRatiosByNo2sHeight = redRatiosByNo2sHeight;
module.exports.rebRatiosByNo2sHeight = rebRatiosByNo2sHeight;
module.exports.ratioPermsByHeight = ratioPermsByHeight;
module.exports.ratiosByHeight = ratiosByHeight;
module.exports.ratiosWithDenom = ratiosWithDenom;
module.exports.ratiosWithNumer = ratiosWithNumer;
module.exports.ratiosByDenom = ratiosByDenom;
module.exports.ratiosByNumer = ratiosByNumer;
module.exports.ratiosByDenomSorted = ratiosByDenomSorted;
module.exports.ratiosByNumerSorted = ratiosByNumerSorted;
module.exports.newRatiosInOddLimit = newRatiosInOddLimit;
module.exports.ratiosInOddLimit = ratiosInOddLimit;
module.exports.ratiosInOddLimitSorted = ratiosInOddLimitSorted;
