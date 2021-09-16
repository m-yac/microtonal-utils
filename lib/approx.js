/**
 * Best rational and EDO approximations of intervals
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module approx
 **/

const {fractionalPart, cachedLog2} = require('./utils.js');
const pf = require('primes-and-factors');
const Fraction = require('fraction.js');
const Interval = require('./interval.js');
const {edoApprox, edoApproxConsistentWithErr} = require('./edo.js');
const {ratioPermsByNo2sHeight, ratioPermsByHeight, ratiosWithDenom, ratiosInOddLimit} = require('./sets.js');

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
  const startOdd = 2 * startIteration * iterationSize + 1;
  const endOdd = 2 * (startIteration + numIterations) * iterationSize + 1;

  let [foundExact, ret] = [false, []];
  let [dist_bound, approx_dist_bound] = [cutoff, cutoff.valueOf_log() + epsilon];
  for (const [i, perms] of ratioPermsByNo2sHeight(startOdd, endOdd, {primeLimit: primeLimit})) {
    let to_add = [];
    let [new_dist_bound, new_approx_dist_bound] = [dist_bound, approx_dist_bound];
    for (const [j_no2s, j_no2s_logval] of perms) {
      const j_approx_dist = Math.abs(fractionalPart(j_no2s_logval - intv_logval));
      if (j_approx_dist < approx_dist_bound) {
        const j_diff = j_no2s.div(intv).reb();
        const j = j_diff.mul(intv);
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
    if (dist_bound.equals(1)) { foundExact = true; break; };
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
  const start = startIteration * iterationSize + 1
  const end = (startIteration + numIterations) * iterationSize + 1;

  let [foundExact, ret] = [false, []];
  let [dist_bound, approx_dist_bound] = [cutoff, cutoff.valueOf_log() + epsilon];
  for (const [i, perms] of ratioPermsByHeight(start, end, {primeLimit: primeLimit, oddLimit: oddLimit})) {
    let to_add = [];
    let [new_dist_bound, new_approx_dist_bound] = [dist_bound, approx_dist_bound];
    for (const [j, j_logval] of perms) {
      const j_approx_dist = Math.abs(j_logval - intv_logval);
      if (j_approx_dist < approx_dist_bound) {
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
    if (dist_bound.equals(1)) { foundExact = true; break; };
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
  if (startIteration == undefined) { startIteration = 0; }
  if (numIterations == undefined) { numIterations = 1; }
  if (iterationSize == undefined) { iterationSize = 100; }
  let d_max = (startIteration + numIterations) * iterationSize + 1;

  let [foundExact, ret] = [false, []];
  let dist_bound = cutoff;
  for (let d = startIteration * iterationSize + 1; !foundExact && d < d_max; d++) {
    const ropts = {lo: intv.div(dist_bound), hi: intv.mul(dist_bound),
                   primeLimit: primeLimit, oddLimit: oddLimit};
    for (const r of ratiosWithDenom(d, ropts)) {
      const i = Interval(r);
      const diff = i.div(intv);
      const dist = diff.distance();
      if (dist.compare(dist_bound) <= 0) {
        dist_bound = dist;
        ret.push({ ratio: r, diff: useExactDiffs ? diff : diff.toCents() });
        if (dist_bound.equals(1)) { foundExact = true; break; };
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
  let {cutoff, primeLimit, oddLimit, useExactDiffs, debug} = opts;
  if (!isFinite(oddLimit) || oddLimit <= 0) {
    throw new Error("no valid odd limit given to bestRationalApproxsByDiff!");
  }
  if (debug) { console.time("bestRationalApproxsByDiff"); }

  if (cutoff == undefined) { cutoff = Interval(2).pow(1,12).sqrt(); }
  const ropts = {lo: intv.div(cutoff), hi: intv.mul(cutoff), primeLimit: primeLimit};

  let ret = [];
  for (const r of ratiosInOddLimit(oddLimit, ropts)) {
    const j = Interval(r);
    const diff = j.div(intv);
    const dist = diff.distance();
    const approx_dist = dist.valueOf_log();
    const to_add = { ratio: r, diff: diff, dist: dist, dist_bound: approx_dist + epsilon };
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

  if (debug) { console.timeEnd("bestRationalApproxsByDiff"); }
  return ret.map(x => ({ ratio: x.ratio, diff: useExactDiffs ? x.diff : x.diff.toCents() }));
}

/**
  * Finds best rational approximations of each of the intervals in an octave of
  * the given EDO.
  *
  * @param {integer} edo
  * @param {Object} [opts]
  * @param {integer} [opts.relCutoff] defaults to 2/3
  * @param {integer} [opts.primeRelErrCutoff] defaults to 2/3
  * @param {boolean} [opts.ensureSameDeg] defaults to false
  * @param {integer} [opts.primeLimit]
  * @param {integer} [opts.oddLimit]
  * @param {integer} [opts.startIteration] defaults to 0
  * @param {integer} [opts.numIterations] defaults to 1
  * @param {integer} [opts.iterationSizeMultiplier] defaults to 4
  * @param {boolean} [opts.useExactDiffs] defaults to false, controls the type
  *                                       of each 'diff' property
  * @param {boolean} [opts.debug] defaults to false
  * @returns {Array.<Array.<{ratio: Fraction, diff: (number|Interval)}>>}
  */
function bestRationalApproxsOfEDOByStep(edo, opts) {
  let {relCutoff, primeRelErrCutoff, ensureSameDeg, primeLimit, oddLimit, startIteration, numIterations, iterationSizeMultiplier, useExactDiffs, debug} = opts || {};
  if (debug) { console.time("bestRationalApproxsOfEDOByStep"); }

  if (relCutoff == undefined) { relCutoff = 2/3; }
  if (primeRelErrCutoff == undefined) { primeRelErrCutoff = 2/3; }
  const cutoff = relCutoff / (2 * edo);
  const primeErrCutoff = primeRelErrCutoff / (2 * edo);

  // a prime limit of 2 means we also have an odd limit of 1!
  if (primeLimit && primeLimit <= 2) { oddLimit = 1; }

  if (startIteration == undefined) { startIteration = 0; }
  if (numIterations == undefined) { numIterations = 1; }
  if (iterationSizeMultiplier == undefined) { iterationSizeMultiplier = 4; }
  const iterationSize = iterationSizeMultiplier * edo;
  const startOdd = 2 * startIteration * iterationSize + 1;
  const endOdd = 2 * (startIteration + numIterations) * iterationSize + 1;

  let ret = [];
  for (let i = 0; i < edo; i++) {
    ret.push([]);
  }
  for (const [i, perms] of ratioPermsByNo2sHeight(1, endOdd, {primeLimit: primeLimit})) {
    for (const [j_no2s, j_no2s_logval] of perms) {

      let {n, max_prime_err} = edoApproxConsistentWithErr(edo, j_no2s);
      if (max_prime_err > primeErrCutoff) { continue; }

      const e2 = - Math.floor(n / edo);
      const j = j_no2s.mul(Interval(2).pow(e2));

      if (oddLimit && !j.inOddLimit(oddLimit)) { continue; }
      if (ensureSameDeg) {
        const d = colorZDegree(j);
        if (updnsSymbCache(edo)[n].every(([uds,pyi]) => pyZDegree(pyi) != d)) {
          continue;
        }
      }

      const j_logval = j_no2s_logval + e2;
      n += edo * e2;
      const approx_dist = Math.abs(j_logval - n / edo);

      if (approx_dist < cutoff + epsilon) {
        const diff = j.div(Interval(2).pow(n,edo));
        const dist = diff.distance();
        const to_add = { ratio: j.toFrac(), diff: diff, dist: dist, dist_bound: approx_dist + epsilon };
        let added = false;
        for (let i = 0; !added && i < ret[n].length; i++) {
          if (approx_dist < ret[n][i].dist_bound
              && ((dist.equals(ret[n][i].dist) && diff.compare(ret[n][i].diff) < 0)
                  || dist.compare(ret[n][i].dist) < 0)) {
            ret[n].splice(i, 0, to_add);
            added = true;
          }
        }
        if (!added) {
          ret[n].push(to_add);
        }
      }
    }
  }
  if (debug) { console.timeEnd("bestRationalApproxsOfEDOByStep"); }
  return ret.map(xs => xs.map(x => ({ ratio: x.ratio, diff: useExactDiffs ? x.diff : x.diff.toCents() })));
}

/**
  * Finds best rational approximations in the given odd limit in the given EDO,
  * sorted by error
  *
  * @param {integer} edo
  * @param {Object} opts
  * @param {integer} [opts.primeRelErrCutoff] defaults to 2/3
  * @param {integer} [opts.primeLimit]
  * @param {integer} opts.oddLimit
  * @param {boolean} [opts.useExactDiffs] defaults to false, controls the type
  *                                       of each 'diff' property
  * @param {boolean} [opts.debug] defaults to false
  * @returns {Array.<{ratios:Array.<Fraction>, dist: (number|Interval)}>}
  */
function bestRationalApproxsOfEDOByDist(edo, opts) {
  let {primeRelErrCutoff, primeLimit, oddLimit, useExactDiffs, debug} = opts;
  if (!isFinite(oddLimit) || oddLimit <= 0) {
    throw new Error("no valid odd limit given to bestRationalApproxsOfEDOByDist!");
  }
  if (debug) { console.time("bestRationalApproxsOfEDOByDist"); }

  if (primeRelErrCutoff == undefined) { primeRelErrCutoff = 2/3; }
  const primeErrCutoff = primeRelErrCutoff / (2 * edo);

  const ropts = { lo: Interval(1), hi: Interval(2).sqrt(), primeLimit: primeLimit };

  let ret = [];
  for (const r of ratiosInOddLimit(oddLimit, ropts)) {
    const j = Interval(r);
    if (j.inPrimeLimit(2)) { continue; }
    const m = edoApprox(edo, j);
    const dist = j.div(Interval(2).pow(m,edo)).distance();
    const approx_dist = dist.valueOf_log();
    let to_add = { ratios: [r, r.inverse().mul(2)], dist: dist, dist_bound: approx_dist + epsilon };
    const {n, max_prime_err} = edoApproxConsistentWithErr(edo, j);
    if (n != m) { to_add.inconsistent = true; }
    if (max_prime_err > primeErrCutoff) { to_add.overErr = true; }
    let added = false;
    for (let i = 0; !added && i < ret.length; i++) {
      if (approx_dist < ret[i].dist_bound && dist.compare(ret[i].dist) < 0) {
        ret.splice(i, 0, to_add);
        added = true;
      }
    }
    if (!added) {
      ret.push(to_add);
    }
  }

  if (debug) { console.timeEnd("bestRationalApproxsOfEDOByDist"); }
  return ret.map(function (x) {
    let x0 = { ratios: x.ratios, dist: useExactDiffs ? x.dist : x.dist.toCents() };
    if (x.inconsistent) { x0.inconsistent = true; }
    if (x.overErr) { x0.overErr = true; }
    return x0;
  });
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
  let {cutoff, startEDO, endEDO, useExactDiffs, debug} = opts;
  if (cutoff == undefined) { cutoff = Interval(2).pow(1,12).sqrt(); }
  if (startEDO == undefined) { startEDO = 5; }
  if (endEDO == undefined) { endEDO = 60; }
  if (debug) { console.time("bestEDOApproxsByEDO"); }

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

  if (debug) { console.timeEnd("bestEDOApproxsByEDO"); }
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
  let {startEDO, endEDO, useExactDiffs, debug} = opts;
  if (startEDO == undefined) { startEDO = 5; }
  if (endEDO == undefined) { endEDO = 60; }
  if (debug) { console.time("bestEDOApproxsByDiff"); }

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

  if (debug) { console.timeEnd("bestEDOApproxsByDiff"); }
  return ret.map(x => ({ steps: x.steps, diff: useExactDiffs ? x.diff : x.diff.toCents() }));
}

module.exports.bestRationalApproxsByNo2sHeightIterationSize = bestRationalApproxsByNo2sHeightIterationSize;
module.exports.bestRationalApproxsByNo2sHeight = bestRationalApproxsByNo2sHeight;
module.exports.bestRationalApproxsByHeightIterationSize = bestRationalApproxsByHeightIterationSize;
module.exports.bestRationalApproxsByHeight = bestRationalApproxsByHeight;
module.exports.bestRationalApproxsByDenom  = bestRationalApproxsByDenom;
module.exports.bestRationalApproxsByDiff   = bestRationalApproxsByDiff;
module.exports.bestRationalApproxsOfEDOByStep = bestRationalApproxsOfEDOByStep;
module.exports.bestRationalApproxsOfEDOByDist = bestRationalApproxsOfEDOByDist;
module.exports.bestEDOApproxsByEDO  = bestEDOApproxsByEDO;
module.exports.bestEDOApproxsByDiff = bestEDOApproxsByDiff;
