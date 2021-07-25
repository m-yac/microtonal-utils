
const _ = require("lodash");
const assert = require('assert');
const Fraction = require('fraction.js');
const Interval = require('../lib/interval.js');
const apx = require('../lib/approx.js');

require('./interval.test.js'); // only run these tests after interval.test.js

const regression_tests = [{
    fn: "bestRationalApproxsByNo2sHeight",
    input: Interval(2).pow(400,1200),
    expect: [ Fraction(5,4), Fraction(64,51), Fraction(24,19), Fraction(161,128) ]
  }, {
    fn: "bestRationalApproxsByNo2sHeight",
    input: Interval(2).pow(400,1200),
    opts: { primeLimit: 13 },
    expect: [ Fraction(5,4), Fraction(81,64) ]
  }, {
    fn: "bestRationalApproxsByNo2sHeight",
    input: Interval(2).pow(300,1200),
    opts: { numIterations: 3 },
    expect: [ Fraction(6,5), Fraction(19,16), Fraction(256,215), Fraction(44,37) ]
  }, {
    fn: "bestRationalApproxsByNo2sHeight",
    input: Interval(2).pow(300,1200),
    opts: { primeLimit: 13, numIterations: 2 },
    expect: [ Fraction(6,5), Fraction(32,27), Fraction(25,21) ]
  }, {
    fn: "bestRationalApproxsByNo2sHeight",
    input: Interval(2).sqrt(),
    expect: [ Fraction(11,8), Fraction(16,11), Fraction(32,23), Fraction(23,16), Fraction(7,5), Fraction(10,7), Fraction(45,32), Fraction(64,45), Fraction(24,17), Fraction(17,12), Fraction(181,128), Fraction(256,181) ]
  }, {
    fn: "bestRationalApproxsByNo2sHeight",
    input: Interval(2).sqrt(),
    opts: { primeLimit: 13, oddLimit: 81 },
    expect: [ Fraction(11,8), Fraction(16,11), Fraction(7,5), Fraction(10,7), Fraction(45,32), Fraction(64,45) ]
  }, {
    fn: "bestRationalApproxsByNo2sHeight",
    input_ratio: Fraction(81,64),
    opts: { primeLimit: 13, oddLimit: 81 },
    expect: [ Fraction(5,4), Fraction(32,25), Fraction(14,11), Fraction(81,64) ]
  }, {
    fn: "bestRationalApproxsByHeight",
    input: Interval(2).pow(400,1200),
    opts: { primeLimit: 19 },
    expect: [ Fraction(5,4), Fraction(19,15), Fraction(24,19), Fraction(34,27) ]
  }, {
    fn: "bestRationalApproxsByHeight",
    input: Interval(2).pow(300,1200),
    opts: { primeLimit: 13 },
    expect: [ Fraction(6,5), Fraction(13,11), Fraction(25,21) ]
  }, {
    fn: "bestRationalApproxsByHeight",
    input: Interval(2).sqrt(),
    opts: { primeLimit: 13, oddLimit: 81 },
    expect: [ Fraction(7,5), Fraction(10,7), Fraction(45,32), Fraction(55,39) ]
  }, {
    fn: "bestRationalApproxsByHeight",
    input_ratio: Fraction(81,64),
    opts: { primeLimit: 19 },
    expect: [ Fraction(5,4), Fraction(14,11), Fraction(19,15) ]
  }, {
    fn: "bestRationalApproxsByDiff",
    input: Interval(2).pow(350,1200),
    opts: { oddLimit: 9 },
    expect: [ Fraction(6,5), Fraction(5,4), Fraction(7,6), Fraction(9,7), Fraction(8,7), Fraction(9,8), Fraction(4,3), Fraction(10,9), Fraction(7,5), Fraction(10,7), Fraction(1,1), Fraction(3,2), Fraction(14,9), Fraction(8,5), Fraction(9,10), Fraction(5,3), Fraction(8,9), Fraction(7,8), Fraction(12,7) ]
  }, {
    fn: "bestEDOApproxsByEDO",
    input_ratio: Fraction(5,4),
    expect: [ [ [ 2, 6 ], [ 3, 9 ], [ 4, 12 ], [ 5, 15 ] ], [ [ 5, 16 ] ], [ [ 6, 19 ] ], [ [ 7, 22 ] ], [ [ 8, 25 ] ], [ [ 9, 28 ], [ 18, 56 ] ], [ [ 19, 59 ] ] ]
  }, {
    fn: "bestEDOApproxsByEDO",
    input: Interval(2).pow(1,12),
    expect: [ [ [ 1, 8 ] ], [ [ 1, 9 ] ], [ [ 1, 10 ] ], [ [ 1, 11 ] ], [ [ 1, 12 ], [ 2, 24 ], [ 3, 36 ], [ 4, 48 ], [ 5, 60 ] ] ]
  }, {
    fn: "bestEDOApproxsByDiff",
    input_ratio: Fraction(5,4),
    expect: [ [ [ 19, 59 ] ], [ [ 9, 28 ], [ 18, 56 ] ], [ [ 10, 31 ] ], [ [ 17, 53 ] ], [ [ 11, 34 ] ], [ [ 8, 25 ], [ 16, 50 ] ], [ [ 12, 37 ] ], [ [ 15, 47 ] ], [ [ 13, 40 ] ], [ [ 14, 43 ] ], [ [ 7, 22 ], [ 14, 44 ] ], [ [ 15, 46 ] ], [ [ 16, 49 ] ], [ [ 13, 41 ] ], [ [ 17, 52 ] ], [ [ 19, 60 ] ], [ [ 18, 55 ] ], [ [ 19, 58 ] ], [ [ 6, 19 ], [ 12, 38 ], [ 18, 57 ] ], [ [ 17, 54 ] ], [ [ 11, 35 ] ], [ [ 16, 51 ] ], [ [ 5, 16 ], [ 10, 32 ], [ 15, 48 ] ], [ [ 14, 45 ] ], [ [ 2, 6 ], [ 3, 9 ], [ 4, 12 ], [ 5, 15 ], [ 6, 18 ], [ 7, 21 ], [ 8, 24 ], [ 9, 27 ], [ 10, 30 ], [ 11, 33 ], [ 12, 36 ], [ 13, 39 ], [ 14, 42 ] ], [ [ 9, 29 ] ], [ [ 4, 13 ], [ 8, 26 ] ], [ [ 7, 23 ] ], [ [ 3, 10 ], [ 6, 20 ] ], [ [ 5, 17 ] ], [ [ 5, 14 ] ], [ [ 2, 7 ] ], [ [ 4, 11 ] ], [ [ 3, 8 ] ], [ [ 2, 5 ] ] ]
  }
];

describe("Best approximations", function () {

  for (const test of regression_tests) {

    let input; let input_str;
    if (test.input_cents != undefined) {
      input = Interval(2).pow(test.input_cents,1200);
      input_str = "Interval(2).pow(" + test.input_cents + ",1200)";
    }
    else if (test.input_ratio != undefined) {
      input = Interval(test.input_ratio);
      input_str = test.input_ratio.n + "," + test.input_ratio.d;
    }
    else {
      input = Interval(test.input);
      // special case for cents values
      if (input.factors().length == 1 && input.hasExp(2)) {
        input_str = "{2: " + input.expOf(2).mul(1200).valueOf() + "/1200}"
      }
      else {
        let factor_strs = [];
        for (const [p,e] of input.factors()) {
          factor_strs.push(p + ": " + e.toFraction());
        }
        input_str = "{" + factor_strs.join(", ") + "}";
      }
    }

    let [opts, opts_str] = [{useExactDiffs: true}, ""];
    if (test.opts != undefined) {
      let opts_strs = [];
      for (const k in test.opts) {
        opts[k] = test.opts[k];
        opts_strs.push(k + ": " + JSON.stringify(test.opts[k]));
      }
      opts_str = ", {" + opts_strs.join(", ") + "}"
    }

    it("regression: " + test.fn + "(" + input_str + opts_str + ")", function() {
      let res = apx[test.fn](input, opts);
      if (typeof res[0] == "boolean") {
        res = res[1];
      }
      assert.equal(res.length, test.expect.length,
                   "actual length !== expected length (" + res.length + " !== " + test.expect.length + ")");
      for (let i = 0; i < res.length; i++) {
        let expected_diff;
        if (res[i].ratio) {
          assert.equal(res[i].ratio.equals(test.expect[i]), true,
                       "actual ratio !== expected ratio (" + res[i].ratio.toFraction() + " !== " + test.expect[i].toFraction() + ")");
          expected_diff = Interval(res[i].ratio).div(input);
        }
        else if (res[i].steps) {
          assert.equal(_.isEqual(res[i].steps, test.expect[i]), true,
                       "actual steps !== expected steps (" + res[i].steps + " !== " + test.expect[i] + ")");
          expected_diff = Interval(2).pow(res[i].steps[0][0], res[i].steps[0][1]).div(input);
        }
        else {
          assert.ok(false, "malformed entry: " + res[i]);
        }
        assert.equal(res[i].diff.equals(expected_diff), true,
                     "actual diff !== expected diff (" + res[i].diff.toCents() + " !== " + expected_diff.toCents() + ")");
      }
    });
  }
});
