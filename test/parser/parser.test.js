
const _ = require('lodash');
const jsc = require("jsverify");
const Fraction = require('fraction.js');
const Interval = require('../../lib/interval.js');
const {pySymb} = require('../../lib/pythagorean.js');
const {parseCvt} = require('../../lib/parser.js');

const {nzPosFrac, intvFromNthRoot, pyIntv} = require('../arbitrary.js')

// How many tests to run in this file
const TESTS_TO_RUN = 20;

// Versions of `jsc.property` that only run `TESTS_TO_RUN` times (instead of 100!)
function jscSlowProperty1(name, arg, fn) {
  it(name, () => jsc.assert(jsc.forall(arg, fn), {tests: TESTS_TO_RUN}));
}
function jscSlowProperty2(name, arg1, arg2, fn) {
  it(name, () => jsc.assert(jsc.forall(arg1, arg2, fn), {tests: TESTS_TO_RUN}));
}

describe("Intervals and the parser", function () {

  jscSlowProperty1("toNthRootString: i == parseCvt(i.toNthRootString()).intv", intvFromNthRoot, function (i) {
    return i.equals(parseCvt(i.toNthRootString()).intv);
  });

  jscSlowProperty2("fr1.mul(fr2) == parseCvt(`${fr1} * ${fr2}`).ratio", nzPosFrac, nzPosFrac, function (fr1, fr2) {
    return fr1.mul(fr2).equals(parseCvt(`${fr1.toFraction()} * ${fr2.toFraction()}`).ratio);
  });

});

describe("Pythagorean intervals and the parser", function () {

  jscSlowProperty1("pyi == parseCvt(pySymb(pyi)).ratio", pyIntv, function (pyi) {
    return pyi.equals(parseCvt(pySymb(pyi)).intv);
  });

});
