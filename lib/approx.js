/**
 * Best rational and EDO approximations of intervals
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module approx
 **/

const pf = require('primes-and-factors');
const Fraction = require('fraction.js');
const Interval = require('./interval.js');
const {edoApprox} = require('./edo.js');

function signPerms(intv) {
  const keys = Object.keys(intv);
  let ret = [];
  for (let bits = 0; bits < (1 << keys.length); bits++) {
    ret.push(keys.map((_,i) => (bits & (1 << i)) == 0 ? 1 : -1));
  }
  return ret;
}

function applySignPerm(sp, intv) {
  let [i, ret] = [0, {}];
  for (const [p,e] of intv.factors()) {
    ret[p] = e.mul(sp[i]);
    i++;
  }
  return Interval(ret);
}

/**
  * Determines the iteration size of `bestRationalApproxsByHeight` using
  * heuristics based on the primeLimit and oddLimit given.
  *
  * @param {Interval} i
  * @param {Object} [opts]
  * @param {integer} [opts.primeLimit]
  * @param {integer} [opts.oddLimit]
  * @returns {integer}
  */
function bestRationalApproxsByHeightIterationSize(opts) {
  let {primeLimit, oddLimit} = opts;
  let iterationSize = 100;
  if (primeLimit) {
    // for large prime limits, this iteration size is approximately half the
    // prime limit itself, but for small prime limits (roughly less than 47)
    // this is larger, to account for the fact that valid intervals are sparser
    iterationSize = Math.ceil(2000/primeLimit + (primeLimit+1)/2);
    // a prime limit of 2 means we also have an odd limit of 1!
    if (primeLimit <= 2) { oddLimit = 1; }
  }
  // the size of the largest odd number which would generate a valid interval
  // in our odd limit
  const oddLimit_max = oddLimit * Math.abs(oddLimit-2);
  if (oddLimit) {
    iterationSize = Math.min(iterationSize, Math.ceil((oddLimit_max+1)/2));
  }
  return iterationSize;
}

/**
  * Finds best rational approximations of the given interval, sorted by Tenney
  * height, or equivalently, Tenney harmonic distance. Returns a pair whose
  * first element is true iff no better approximaions can be found - i.e. if
  * either an exact approximation is found or there are no more intervals in
  * the given odd-limit to check.
  *
  * @param {Interval} i
  * @param {Object} [opts]
  * @param {integer} [opts.cutoff] defaults to 50 cents
  * @param {integer} [opts.primeLimit]
  * @param {integer} [opts.oddLimit]
  * @param {integer} [opts.startIteration] defaults to 0
  * @param {integer} [opts.numIterations] defaults to 1
  * @param {boolean} [opts.useExactDiffs] defaults to false, controls the type
  *                                       of each 'diff' property
  * @param {boolean} [opts.debug] defaults to false
  * @returns {Pair.<boolean, Array.<{ratio: Fraction, diff: (number|Interval)}>>}
  */
function bestRationalApproxsByHeight(a,b, opts) {
  // if only two arguments are given, the second one may be `opts`!
  if (!opts) {
    if (typeof b == 'object' && b != null) {
      opts = b;
      b = undefined;
    } else {
      opts = {};
    }
  }
  const intv = Interval(a,b);
  let {cutoff, primeLimit, oddLimit, startIteration, numIterations, useExactDiffs, debug} = opts;
  let [hitOddLimitMax, foundExact] = [false, false];
  if (debug) { console.time("bestRationalApproxsByHeight"); }

  // some heuristics for the iteration size, i.e. the number of odd numbers
  // to check in a given iteration
  const iterationSize = bestRationalApproxsByHeightIterationSize(opts);

  // a prime limit of 2 means we also have an odd limit of 1!
  if (primeLimit && primeLimit <= 2) { oddLimit = 1; }

  if (cutoff == undefined) { cutoff = Interval(2).pow(1,12).sqrt(); }
  if (primeLimit == undefined && oddLimit) { primeLimit = oddLimit; }
  if (startIteration == undefined) { startIteration = 0; }
  if (numIterations == undefined) { numIterations = 1; }
  let n_max = (startIteration + numIterations) * iterationSize;

  // if our n_max is greater than the largest odd number which would generate a
  // valid interval in our odd limit, we don't have to check any more than that!
  if (oddLimit) {
    const oddLimit_max = oddLimit * Math.abs(oddLimit-2);
    if (n_max >= (oddLimit_max+1)/2) {
      n_max = (oddLimit_max+1)/2;
      hitOddLimitMax = true;
    }
  }

  const intv_red = intv.red();
  const vs = intv.div(intv_red);
  let [last_diff, ret] = [Interval(2), []];
  // this loop iterates through all odd numbers `2*n + 1` for `n` in the range
  // `[startIteration * iterationSize + 1, numIterations * iterationSize)`
  for (let n = startIteration * iterationSize; !foundExact && n < n_max; n++) {
    const i = Interval(2*n + 1);
    if (primeLimit && !i.inPrimeLimit(primeLimit)) {
      continue;
    }
    // For a given odd `i` with factorization `p1^e1 ... pm^em` (where `pk` is
    // prime and `ek > 0` for all `k`), `i_perms` is the array of all intervals
    // with factorizations `p1^(+/- e1) ... pm^(+/- em)`. For example, if
    // `i = 45` then `i = 3^2 * 5^1` and
    // `i_perms = [ 3^2 * 5^1, 3^(-2) * 5^1, 3^2 * 5^(-1), 3^(-2) * 5(-1) ]`.
    const i_perms = signPerms(i).map(sp => applySignPerm(sp, i));
    // For each of these factorizations, we then add in the power of 2 which
    // gets it closest to `intv`, then package the result up with its difference
    // to `intv`. We do the former by finding the balanced octave-reduced
    // difference to `intv` then adding this difference back to `intv`; the
    // result will always be our original factorization times the power of 2
    // which minimizes its difference to `intv`
    const to_check = i_perms.map(function (j) {
                       const diff = j.div(intv).reb();
                       return [intv.mul(diff), diff];
                     }).sort((a,b) => a[1].compare(b[1]));
    for (const [j, diff] of to_check) {
      if (oddLimit && !j.inOddLimit(oddLimit)) {
        continue;
      }
      const abs_diff = diff.distance();
      if (abs_diff.compare(cutoff) < 0 && abs_diff.compare(last_diff) <= 0) {
        ret.push({ ratio: j.toFrac(), diff: useExactDiffs ? diff : diff.toCents() });
        last_diff = abs_diff;
        if (last_diff.equals(1)) { foundExact = true };
      }
    }
  }
  if (debug) {
    console.timeEnd("bestRationalApproxsByHeight");
    if (hitOddLimitMax || foundExact) {
      console.log("bestRationalApproxsByHeight: exhausted")
    }
  }
  return [hitOddLimitMax || foundExact, ret];
}

/**
  * Finds best rational approximations of the given interval, sorted by
  * denominator. Returns a pair whose first element is true iff no better
  * approximaions can be found - i.e. if an exact approximation is found.
  *
  * @param {Interval} i
  * @param {Object} [opts]
  * @param {integer} [opts.cutoff] defaults to 50 cents
  * @param {integer} [opts.primeLimit]
  * @param {integer} [opts.oddLimit]
  * @param {integer} [opts.startIteration] defaults to 0
  * @param {integer} [opts.numIterations] defaults to 1
  * @param {boolean} [opts.useExactDiffs] defaults to false, controls the type
  *                                       of each 'diff' property
  * @param {boolean} [opts.debug] defaults to false
  * @returns {Pair.<boolean, Array.<{ratio: Fraction, diff: (number|Interval)}>>}
  */
function bestRationalApproxsByDenom(a,b, opts) {
  // if only two arguments are given, the second one may be `opts`!
  if (!opts) {
    if (typeof b == 'object' && b != null) {
      opts = b;
      b = undefined;
    } else {
      opts = {};
    }
  }
  const intv = Interval(a,b);
  let {cutoff, primeLimit, oddLimit, startIteration, numIterations, useExactDiffs, debug} = opts;
  let [hitOddLimitMax, foundExact] = [false, false];
  if (debug) { console.time("bestRationalApproxsByDenom"); }

  // for now we always go in iterations of 100
  let iterationSize = 100;

  if (cutoff == undefined) { cutoff = Interval(2).pow(1,12).sqrt(); }
  if (primeLimit == undefined && oddLimit) { primeLimit = oddLimit; }
  if (startIteration == undefined) { startIteration = 0; }
  if (numIterations == undefined) { numIterations = 1; }
  let d_max = (startIteration + numIterations) * iterationSize + 1;

  let [last_diff, ret] = [Interval(2), []];
  for (let d = startIteration * iterationSize + 1; !foundExact && d < d_max; d++) {
    if (oddLimit && d % 2 != 0 && d > oddLimit) {
      continue;
    }
    const nBest = Math.round(intv.mul(d).valueOf());
    // If nBest/d is not in our odd limit, can there exist some i such that
    // (nBest+i)/d is in our odd limit but also satisfies abs_diff <= last_diff?
    // I have no idea! So for now, we check all of n, n+1, n+2, ... and
    // n-1, n-2, n-3, ... until we've cleared last_diff.
    for (let n = nBest; !foundExact; n++) {
      // NB: If you make any changes to this, make sure to update the below -
      // the bodies of these two loops should be identical.
      const r = Fraction(n,d);
      const diff = Interval(r).div(intv);
      const abs_diff = diff.distance();
      if (abs_diff.compare(cutoff) < 0 && abs_diff.compare(last_diff) <= 0) {
        // if n/d reduces, we've seen it already - so we can safely skip
        if (r.d != d) {
          continue;
        }
        if (oddLimit && r.n % 2 != 0 && r.n > oddLimit) {
          continue;
        }
        ret.push({ ratio: r, diff: useExactDiffs ? diff : diff.toCents() });
        last_diff = abs_diff;
        if (last_diff.equals(1)) { foundExact = true };
      }
      else {
        break;
      }
    }
    for (let n = nBest-1; !foundExact && n > 0; n--) {
      // NB: If you make any changes to this, make sure to update the above -
      // the bodies of these two loops should be identical.
      const r = Fraction(n,d);
      const diff = Interval(r).div(intv);
      const abs_diff = diff.distance();
      if (abs_diff.compare(cutoff) < 0 && abs_diff.compare(last_diff) <= 0) {
        // if n/d reduces, we've seen it already - so we can safely skip
        if (r.d != d) {
          continue;
        }
        if (oddLimit && r.n % 2 != 0 && r.n > oddLimit) {
          continue;
        }
        ret.push({ ratio: r, diff: useExactDiffs ? diff : diff.toCents() });
        last_diff = abs_diff;
        if (last_diff.equals(1)) { foundExact = true };
      }
      else {
        break;
      }
    }
  }
  if (debug) { console.timeEnd("bestRationalApproxsByDenom"); }
  return [foundExact, ret];
}

/**
  * Finds best rational approximations in the given odd limit of the given
  * interval, sorted by error.
  *
  * @param {Interval} i
  * @param {Object} opts
  * @param {integer} [opts.primeLimit]
  * @param {integer} opts.oddLimit
  * @param {boolean} [opts.useExactDiffs] defaults to false, controls the type
  *                                       of each 'diff' property
  * @param {boolean} [opts.debug] defaults to false
  * @returns {Array.<{ratio: Fraction, diff: (number|Interval)}>}
  */
function bestRationalApproxsByDiff(a,b, opts) {
  // if only two arguments are given, the second one may be `opts`!
  if (!opts) {
    if (typeof b == 'object' && b != null) {
      opts = b;
      b = undefined;
    } else {
      opts = {};
    }
  }
  const intv = Interval(a,b);
  let {primeLimit, oddLimit, useExactDiffs, debug} = opts;
  if (!isFinite(oddLimit) || oddLimit <= 0) {
    throw new Error("no valid odd limit given to bestRationalApproxsByDiff!");
  }
  if (debug) { console.time("bestRationalApproxsByDiff"); }

  let ret = [];
  const diff_to_1 = intv.recip().reb();
  const abs_diff_to_1 = diff_to_1.distance();
  ret.push({ ratio: intv.mul(diff_to_1).toFrac(), diff: diff_to_1, abs_diff: abs_diff_to_1 })
  for (let a = 1; a <= oddLimit; a += 2) {
    for (let b = 1; b < a; b += 2) {
      const r = Fraction(a,b);
      // skip all cases where a/b is not reduced
      if (r.n != a || r.d != b) {
        continue;
      }
      for (const j of [Interval(r), Interval(r).recip()]) {
        if (primeLimit && !j.inPrimeLimit(primeLimit)) {
          continue;
        }
        const diff = j.div(intv).reb();
        const abs_diff = diff.distance();
        const to_add = { ratio: intv.mul(diff).toFrac(), diff: diff, abs_diff: abs_diff };
        let added = false;
        for (let i = 0; !added && i < ret.length; i++) {
          const cmp_abs_diffs = abs_diff.compare(ret[i].abs_diff)
          if ((cmp_abs_diffs == 0 && diff.compare(ret[i].diff) < 0)
              || cmp_abs_diffs < 0) {
            ret.splice(i, 0, to_add);
            added = true;
          }
        }
        if (!added) {
          ret.push(to_add);
        }
      }
    }
  }
  if (debug) { console.timeEnd("bestRationalApproxsByDiff"); }
  return ret.map(x => ({ ratio: x.ratio, diff: useExactDiffs ? x.diff : x.diff.toCents() }));
}

/**
  * Finds best EDO step approximations of the given interval, sorted by EDO
  * size.
  *
  * @param {Interval} i
  * @param {Object} [opts]
  * @param {integer} [opts.cutoff] defaults to 50 cents
  * @param {integer} [opts.startEDO] defaults to 5
  * @param {integer} [opts.endEDO] defaults to 60
  * @param {boolean} [opts.useExactDiffs] defaults to false, controls the type
  *                                       of each 'diff' property
  * @returns {Array.<{steps: Array, diff: (number|Interval)}>}
  */
function bestEDOApproxsByEDO(a,b, opts) {
  // if only two arguments are given, the second one may be `opts`!
  if (!opts) {
    if (typeof b == 'object' && b != null) {
      opts = b;
      b = undefined;
    } else {
      opts = {};
    }
  }
  const intv = Interval(a,b);
  if (opts == undefined) { opts = {}; }
  let {cutoff, startEDO, endEDO, useExactDiffs} = opts;
  if (cutoff == undefined) { cutoff = Interval(2).pow(1,12).sqrt(); }
  if (startEDO == undefined) { startEDO = 5; }
  if (endEDO == undefined) { endEDO = 60; }

  let foundExact = false;
  let [last_diff, ret] = [Interval(2), []];
  for (let edo = startEDO; edo <= endEDO; edo++) {
    const steps = edoApprox(edo, intv);
    const diff = intv.div(Interval(2).pow(steps,edo));
    const abs_diff = diff.distance();
    const diff_to_last = abs_diff.compare(last_diff);
    if (abs_diff.compare(cutoff) < 0 && diff_to_last <= 0) {
      if (diff_to_last == 0) {
        ret[ret.length - 1].steps.push([steps, edo]);
      }
      else if (!foundExact) {
        ret.push({ steps: [[steps,edo]], diff: useExactDiffs ? diff : diff.toCents() });
        last_diff = abs_diff;
        if (last_diff.equals(1)) { foundExact = true };
      }
    }
  }

  return ret;
}

/**
  * Finds best EDO step approximations of the given interval, sorted by error.
  *
  * @param {Interval} i
  * @param {Object} [opts]
  * @param {integer} [opts.startEDO] defaults to 5
  * @param {integer} [opts.endEDO] defaults to 60
  * @param {boolean} [opts.useExactDiffs] defaults to false, controls the type
  *                                       of each 'diff' property
  * @returns {Array.<{steps: Array, diff: (number|Interval)}>}
  */
function bestEDOApproxsByDiff(a,b, opts) {
  // if only two arguments are given, the second one may be `opts`!
  if (!opts) {
    if (typeof b == 'object' && b != null) {
      opts = b;
      b = undefined;
    } else {
      opts = {};
    }
  }
  const intv = Interval(a,b);
  if (opts == undefined) { opts = {}; }
  let {startEDO, endEDO, useExactDiffs} = opts;
  if (startEDO == undefined) { startEDO = 5; }
  if (endEDO == undefined) { endEDO = 60; }

  let ret = [];
  for (let edo = startEDO; edo <= endEDO; edo++) {
    const steps = edoApprox(edo, intv);
    const diff = intv.div(Interval(2).pow(steps,edo));
    const abs_diff = diff.distance();
    const to_add = { steps: [[steps, edo]], diff: diff, abs_diff: abs_diff };
    let added = false;
    for (let i = 0; !added && i < ret.length; i++) {
      if (diff.equals(ret[i].diff)) {
        ret[i].steps.push([steps,edo]);
        added = true;
      }
      else if (abs_diff.compare(ret[i].abs_diff) < 0) {
        ret.splice(i, 0, to_add);
        added = true;
      }
    }
    if (!added) {
      ret.push(to_add);
    }
  }

  return ret.map(x => ({ steps: x.steps, diff: useExactDiffs ? x.diff : x.diff.toCents() }));
}

module.exports.bestRationalApproxsByHeightIterationSize = bestRationalApproxsByHeightIterationSize;
module.exports.bestRationalApproxsByHeight = bestRationalApproxsByHeight;
module.exports.bestRationalApproxsByDenom  = bestRationalApproxsByDenom;
module.exports.bestRationalApproxsByDiff   = bestRationalApproxsByDiff;
module.exports.bestEDOApproxsByEDO  = bestEDOApproxsByEDO;
module.exports.bestEDOApproxsByDiff = bestEDOApproxsByDiff;
