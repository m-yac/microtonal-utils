/**
 * Best rational and EDO approximations of intervals
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module approx
 **/

const {fractionalPart, cachedLog2} = require('./utils.js');
const pf = require('primes-and-factors');
const Fraction = require('fraction.js');
const Interval = require('./interval.js');
const {edoApprox} = require('./edo.js');

// For a given interval with factorization `p1^e1 ... pm^em` (where `pk` is
// prime and `ek > 0` for all `k`), `signPerms(intv)` is the array of all
// intervals with factorizations `p1^(+/- e1) ... pm^(+/- em)`. For example, if
// `i = 45` then `i = 3^2 * 5^1` and
// `signPerms(i) = [ 3^2 * 5^1, 3^(-2) * 5^1, 3^2 * 5^(-1), 3^(-2) * 5(-1) ]`.
// Note that we also include the log2 values of each interval as well.
function signPerms(intv) {
  const intv_fact = intv.factors();
  let ret = [];
  for (let bits = 0; bits < (1 << intv_fact.length); bits++) {
    let [i, fact, logval] = [0, {}, 0];
    for (const [p,e] of intv_fact) {
      fact[p] = e.mul((bits & (1 << i)) == 0 ? 1 : -1);
      logval += fact[p].valueOf() * cachedLog2(p);
      i++;
    }
    ret.push([Interval(fact), logval]);
  }
  return ret;
}

// The epsilon to use when comparing approximate distances
const epsilon = 1e-5;

/**
  * Determines the iteration size of `bestRationalApproxsByNo2sHeight` using
  * heuristics based on the primeLimit given.
  *
  * @param {integer} [primeLimit]
  * @returns {integer}
  */
function bestRationalApproxsByNo2sHeightIterationSize(primeLimit) {
  if (primeLimit) {
    // for large prime limits, this iteration size is approximately half the
    // prime limit itself, but for small prime limits (roughly less than 47)
    // this is larger, to account for the fact that valid intervals are sparser
    return Math.ceil(2000/primeLimit + (primeLimit+1)/2);
  }
  return 100;
}

/**
  * Finds best rational approximations of the given interval, sorted by the
  * Tenney height (or equivalently, Tenney harmonic distance) of the interval
  * with all factors of 2 removed. Returns a pair whose first element is true
  * iff an exact approximaion has been be found.
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
function bestRationalApproxsByNo2sHeight(a,b, opts) {
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
  const intv_logval = intv.valueOf_log();
  let {cutoff, primeLimit, oddLimit, startIteration, numIterations, useExactDiffs, debug} = opts;
  if (debug) { console.time("bestRationalApproxsByNo2sHeight"); }

  // some heuristics for the iteration size, i.e. the number of odd numbers
  // to check in a given iteration
  const iterationSize = bestRationalApproxsByNo2sHeightIterationSize(primeLimit);

  // a prime limit of 2 means we also have an odd limit of 1!
  if (primeLimit && primeLimit <= 2) { oddLimit = 1; }

  if (cutoff == undefined) { cutoff = Interval(2).pow(1,12).sqrt(); }
  if (startIteration == undefined) { startIteration = 0; }
  if (numIterations == undefined) { numIterations = 1; }
  let n_max = (startIteration + numIterations) * iterationSize;

  let [foundExact, ret] = [false, []];
  let [dist_bound, approx_dist_bound] = [cutoff, cutoff.valueOf_log() + epsilon];
  for (let n = startIteration * iterationSize; !foundExact && n < n_max; n++) {
    const i = Interval(2*n + 1);
    if (primeLimit && !i.inPrimeLimit(primeLimit)) {
      continue;
    }
    let to_add = [];
    let [new_dist_bound, new_approx_dist_bound] = [dist_bound, approx_dist_bound];
    for (const [j_no2s, j_no2s_logval] of signPerms(i)) {
      const j_approx_dist = Math.abs(fractionalPart(j_no2s_logval - intv_logval));
      if (j_approx_dist < approx_dist_bound) {
        const j_diff = j_no2s.div(intv).reb();
        const j = j_no2s.mul(Interval(2).pow(intv.expOf(2).add(j_diff.expOf(2))));
        if (!oddLimit || j.inOddLimit(oddLimit)) {
          const j_dist = j_diff.distance();
          if (j_dist.compare(dist_bound) <= 0) {
            new_dist_bound = j_dist;
            new_approx_dist_bound = j_approx_dist + epsilon;
            to_add.push([j, j_diff, j_dist]);
          }
        }
      }
    }
    to_add.sort(function([a, a_diff, a_dist], [b, b_diff, b_dist]) {
      if (b_dist.equals(a_dist)) { return a_diff.compare(b_diff); }
      return b_dist.compare(a_dist);
    });
    for (const [j, j_diff, j_dist] of to_add) {
      ret.push({ ratio: j.toFrac(), diff: useExactDiffs ? j_diff : j_diff.toCents() });
    }
    [dist_bound, approx_dist_bound] = [new_dist_bound, new_approx_dist_bound];
    if (dist_bound.equals(1)) { foundExact = true };
  }
  if (debug) {
    console.timeEnd("bestRationalApproxsByNo2sHeight");
    if (foundExact) {
      console.log("bestRationalApproxsByNo2sHeight: exhausted")
    }
  }
  return [foundExact, ret];
}

/**
  * Determines the iteration size of `bestRationalApproxsByHeight` using
  * heuristics based on the primeLimit given.
  *
  * @param {integer} [primeLimit]
  * @returns {integer}
  */
function bestRationalApproxsByHeightIterationSize(primeLimit) {
  return 16*bestRationalApproxsByNo2sHeightIterationSize(primeLimit);
}

/**
  * Finds best rational approximations of the given interval, sorted by Tenney
  * height, or equivalently, Tenney harmonic distance. Returns a pair whose
  * first element is true iff an exact approximaion has been be found.
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
  const intv_logval = intv.valueOf_log();
  let {cutoff, primeLimit, oddLimit, startIteration, numIterations, useExactDiffs, debug} = opts;
  if (debug) { console.time("bestRationalApproxsByHeight"); }

  // some heuristics for the iteration size
  const iterationSize = bestRationalApproxsByHeightIterationSize(primeLimit);

  // a prime limit of 2 means we also have an odd limit of 1!
  if (primeLimit && primeLimit <= 2) { oddLimit = 1; }

  if (cutoff == undefined) { cutoff = Interval(2).pow(1,12).sqrt(); }
  if (startIteration == undefined) { startIteration = 0; }
  if (numIterations == undefined) { numIterations = 1; }
  let n_max = (startIteration + numIterations) * iterationSize + 1;

  let [foundExact, ret] = [false, []];
  let [dist_bound, approx_dist_bound] = [cutoff, cutoff.valueOf_log() + epsilon];
  for (let n = startIteration * iterationSize + 1; !foundExact && n < n_max; n++) {
    const i = Interval(n);
    if (primeLimit && !i.inPrimeLimit(primeLimit)) {
      continue;
    }
    let to_add = [];
    let [new_dist_bound, new_approx_dist_bound] = [dist_bound, approx_dist_bound];
    for (const [j, j_logval] of signPerms(i)) {
      const j_approx_dist = Math.abs(j_logval - intv_logval);
      if (j_approx_dist < approx_dist_bound && (!oddLimit || j.inOddLimit(oddLimit))) {
        const j_diff = j.div(intv);
        const j_dist = j_diff.distance();
        if (j_dist.compare(dist_bound) <= 0) {
          new_dist_bound = j_dist;
          new_approx_dist_bound = j_approx_dist + epsilon;
          to_add.push([j, j_diff, j_dist]);
        }
      }
    }
    to_add.sort(function([a, a_diff, a_dist], [b, b_diff, b_dist]) {
      if (b_dist.equals(a_dist)) { return a_diff.compare(b_diff); }
      return b_dist.compare(a_dist);
    });
    for (const [j, j_diff, j_dist] of to_add) {
      ret.push({ ratio: j.toFrac(), diff: useExactDiffs ? j_diff : j_diff.toCents() });
    }
    [dist_bound, approx_dist_bound] = [new_dist_bound, new_approx_dist_bound];
    if (dist_bound.equals(1)) { foundExact = true };
  }
  if (debug) {
    console.timeEnd("bestRationalApproxsByHeight");
    if (foundExact) {
      console.log("bestRationalApproxsByHeight: exhausted")
    }
  }
  return [foundExact, ret];
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
  * @param {integer} [opts.iterationSize] defaults to 100
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
  const intv_logval = intv.valueOf_log();
  let {cutoff, primeLimit, oddLimit, startIteration, numIterations, iterationSize, useExactDiffs, debug} = opts;
  if (debug) { console.time("bestRationalApproxsByDenom"); }

  if (cutoff == undefined) { cutoff = Interval(2).pow(1,12).sqrt(); }
  if (primeLimit == undefined && oddLimit) { primeLimit = oddLimit; }
  if (startIteration == undefined) { startIteration = 0; }
  if (numIterations == undefined) { numIterations = 1; }
  if (iterationSize == undefined) { iterationSize = 100; }
  let d_max = (startIteration + numIterations) * iterationSize + 1;

  let [foundExact, ret] = [false, []];
  let [dist_bound, approx_dist_bound] = [cutoff, cutoff.valueOf_log() + epsilon];
  for (let d = startIteration * iterationSize + 1; !foundExact && d < d_max; d++) {
    if (oddLimit && d % 2 != 0 && d > oddLimit) {
      continue;
    }
    const nBest = Math.round(intv.mul(d).valueOf());
    // If nBest/d is not in our odd limit, can there exist some i such that
    // (nBest+i)/d is in our odd limit but also satisfies dist <= dist_bound?
    // I have no idea! So for now, we check all of n, n+1, n+2, ... and
    // n-1, n-2, n-3, ... until we've cleared dist_bound.
    for (let n = nBest; !foundExact; n++) {
      // NB: If you make any changes to this, make sure to update the below -
      // the bodies of these two loops should be identical.
      const r = Fraction(n,d);
      const i = Interval(r);
      const approx_dist = Math.abs(i.valueOf_log() - intv_logval);
      if (approx_dist < approx_dist_bound) {
        const diff = i.div(intv);
        const dist = diff.distance();
        if (dist.compare(dist_bound) <= 0) {
          // if n/d reduces, we've seen it already - so we can safely skip
          if (r.d != d) {
            continue;
          }
          if (oddLimit && r.n % 2 != 0 && r.n > oddLimit) {
            continue;
          }
          dist_bound = dist;
          approx_dist_bound = approx_dist + epsilon;
          ret.push({ ratio: r, diff: useExactDiffs ? diff : diff.toCents() });
          if (dist_bound.equals(1)) { foundExact = true };
        }
        else {
          break;
        }
      }
      else {
        break;
      }
    }
    for (let n = nBest-1; !foundExact && n > 0; n--) {
      // NB: If you make any changes to this, make sure to update the above -
      // the bodies of these two loops should be identical.
      const r = Fraction(n,d);
      const i = Interval(r);
      const approx_dist = Math.abs(i.valueOf_log() - intv_logval);
      if (approx_dist < approx_dist_bound) {
        const diff = i.div(intv);
        const dist = diff.distance();
        if (dist.compare(dist_bound) <= 0) {
          // if n/d reduces, we've seen it already - so we can safely skip
          if (r.d != d) {
            continue;
          }
          if (oddLimit && r.n % 2 != 0 && r.n > oddLimit) {
            continue;
          }
          dist_bound = dist;
          approx_dist_bound = approx_dist + epsilon;
          ret.push({ ratio: r, diff: useExactDiffs ? diff : diff.toCents() });
          if (dist_bound.equals(1)) { foundExact = true };
        }
        else {
          break;
        }
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
  const dist_to_1 = diff_to_1.distance();
  ret.push({ ratio: intv.mul(diff_to_1).toFrac(), diff: diff_to_1,
             dist: dist_to_1, dist_bound: dist_to_1.valueOf_log() + epsilon });
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
        const dist = diff.distance();
        const approx_dist = dist.valueOf_log();
        const to_add = { ratio: intv.mul(diff).toFrac(), diff: diff,
                         dist: dist, dist_bound: approx_dist + epsilon };
        let added = false;
        for (let i = 0; !added && i < ret.length; i++) {
          if (approx_dist < ret[i].dist_bound
              && ((dist.equals(ret[i].dist) && diff.compare(ret[i].diff) < 0)
                  || dist.compare(ret[i].dist) < 0)) {
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
  const intv_logval = intv.valueOf_log();
  if (opts == undefined) { opts = {}; }
  let {cutoff, startEDO, endEDO, useExactDiffs} = opts;
  if (cutoff == undefined) { cutoff = Interval(2).pow(1,12).sqrt(); }
  if (startEDO == undefined) { startEDO = 5; }
  if (endEDO == undefined) { endEDO = 60; }

  let [foundExact, ret] = [false, []];
  let [dist_bound, approx_dist_bound] = [cutoff, cutoff.valueOf_log() + epsilon];
  for (let edo = startEDO; edo <= endEDO; edo++) {
    const steps = edoApprox(edo, intv);
    const approx_dist = Math.abs(steps/edo - intv_logval);
    if (approx_dist < approx_dist_bound) {
      const diff = Interval(2).pow(steps,edo).div(intv);
      const dist = diff.distance();
      if (dist.equals(dist_bound) && ret.length > 0) {
        ret[ret.length - 1].steps.push([steps, edo]);
      }
      else if (dist.compare(dist_bound) <= 0 && !foundExact) {
        dist_bound = dist;
        approx_dist_bound = approx_dist + epsilon;
        ret.push({ steps: [[steps,edo]], diff: useExactDiffs ? diff : diff.toCents() });
        if (dist_bound.equals(1)) { foundExact = true };
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
    const diff = Interval(2).pow(steps,edo).div(intv);
    const dist = diff.distance();
    const approx_dist = dist.valueOf_log();
    const to_add = { steps: [[steps, edo]], diff: diff,
                     dist: dist, dist_bound: approx_dist + epsilon };
    let added = false;
    for (let i = 0; !added && i < ret.length; i++) {
      if (approx_dist < ret[i].dist_bound) {
        if (diff.equals(ret[i].diff)) {
          ret[i].steps.push([steps,edo]);
          added = true;
        }
        else if (dist.compare(ret[i].dist) < 0) {
          ret.splice(i, 0, to_add);
          added = true;
        }
      }
    }
    if (!added) {
      ret.push(to_add);
    }
  }

  return ret.map(x => ({ steps: x.steps, diff: useExactDiffs ? x.diff : x.diff.toCents() }));
}

module.exports.bestRationalApproxsByNo2sHeightIterationSize = bestRationalApproxsByNo2sHeightIterationSize;
module.exports.bestRationalApproxsByNo2sHeight = bestRationalApproxsByNo2sHeight;
module.exports.bestRationalApproxsByHeightIterationSize = bestRationalApproxsByHeightIterationSize;
module.exports.bestRationalApproxsByHeight = bestRationalApproxsByHeight;
module.exports.bestRationalApproxsByDenom  = bestRationalApproxsByDenom;
module.exports.bestRationalApproxsByDiff   = bestRationalApproxsByDiff;
module.exports.bestEDOApproxsByEDO  = bestEDOApproxsByEDO;
module.exports.bestEDOApproxsByDiff = bestEDOApproxsByDiff;
