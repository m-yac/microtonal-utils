
const _ = require('lodash');
const jsc = require("jsverify");
const Fraction = require('fraction.js');
const Interval = require('../../lib/interval.js');
const {pyInterval, pyDegree, pyOffset, pySymb} = require('../../lib/pythagorean.js');
const {colorSymb} = require('../../lib/color.js');
const {updnsSymb} = require('../../lib/edo.js');
const {parseCvt} = require('../../lib/parser.js');

const {nzPosFrac, intvFromNthRoot, degree, offset} = require('../arbitrary.js')

const nonSemiNeutPyIntv =
  jsc.pair(degree, offset).smap(([d,o]) => pyInterval(d,o/2),
                                pyi => [pyDegree(pyi), pyOffset(pyi)]);

// How many tests to run in this file
const VSLOW_TESTS_TO_RUN = 20;
const SLOW_TESTS_TO_RUN = 100;

// Versions of `jsc.property` that run less than 100 times
function jscVerySlowProperty1(name, arg, fn) {
  it(name, () => jsc.assert(jsc.forall(arg, fn), {tests: VSLOW_TESTS_TO_RUN}));
}
function jscVerySlowProperty2(name, arg1, arg2, fn) {
  it(name, () => jsc.assert(jsc.forall(arg1, arg2, fn), {tests: VSLOW_TESTS_TO_RUN}));
}
function jscSlowProperty1(name, arg, fn) {
  it(name, () => jsc.assert(jsc.forall(arg, fn), {tests: SLOW_TESTS_TO_RUN}));
}
function jscSlowProperty2(name, arg1, arg2, fn) {
  it(name, () => jsc.assert(jsc.forall(arg1, arg2, fn), {tests: SLOW_TESTS_TO_RUN}));
}

describe("Intervals and the parser", function () {

  jscVerySlowProperty1("toNthRootString: i == parseCvt(i.toNthRootString()).intv", intvFromNthRoot, function (i) {
    return i.equals(parseCvt(i.toNthRootString()).intv);
  });

  jscVerySlowProperty2("fr1.mul(fr2) == parseCvt(`${fr1} * ${fr2}`).ratio", nzPosFrac, nzPosFrac, function (fr1, fr2) {
    return fr1.mul(fr2).equals(parseCvt(`${fr1.toFraction()} * ${fr2.toFraction()}`).ratio);
  });

});

describe("Pythagorean intervals and the parser", function () {

  jscSlowProperty1("pyi == parseCvt(pySymb(pyi)).intv", nonSemiNeutPyIntv, function (pyi) {
    return pyi.equals(parseCvt(pySymb(pyi)).intv);
  });

});

describe("Color notation intervals and the parser", function () {

  jscSlowProperty1("fr == parseCvt(colorSymb(fr)).ratio", nzPosFrac, function (fr) {
    return fr.equals(parseCvt(colorSymb(fr)).ratio);
  });

  jscSlowProperty1("fr == parseCvt(colorSymb(fr, {useExps:1})).ratio", nzPosFrac, function (fr) {
    console.log(colorSymb(fr, {useExps:1}));
    return fr.equals(parseCvt(colorSymb(fr, {useExps:1})).ratio);
  });

  jscSlowProperty1("fr == parseCvt(colorSymb(fr, {verbosity:1})).ratio", nzPosFrac, function (fr) {
    let i = Interval(fr);
    // remove all multiples of primes higher than 67
    for (const [p,e] of i.factors()) {
      if (p > 67) { i = i.mul(Interval(p).pow(e.neg())); }
    }
    console.log("Input w/out primes > 67: " + i.toNthRootString());
    return i.equals(parseCvt(colorSymb(i, {verbosity:1})).intv);
  });

});


describe("EDO-step intervals and the parser", function () {

  jscSlowProperty2("[n,edo] == parseCvt(n + '\\\\' + edo).edoSteps", jsc.integer(-300,300), jsc.integer(2, 300), function (n,edo) {
    const [n0,edo0] = parseCvt(n + "\\" + edo).edoSteps;
    return n0 == n && edo0 == edo;
  });

});

describe("Ups-and-downs intervals and the parser", function () {

  jscSlowProperty2("[n,edo] == parseCvt(updnsSymb(edo,n)[0]+'\\\\'+edo).edoSteps", jsc.integer(-300,300), jsc.integer(2, 300), function (n,edo) {
    for (const s of updnsSymb(edo,n)) {
      const [n0,edo0] = parseCvt(s + "\\" + edo).edoSteps;
      if (n0 != n || edo0 != edo) { return false; }
    }
    return true;
  });

  jscSlowProperty2("[n,edo] == parseCvt(updnsSymb(edo,n,{verbosity:1})[0]+'\\\\'+edo).edoSteps", jsc.integer(-300,300), jsc.integer(2, 300), function (n,edo) {
    for (const s of updnsSymb(edo,n, {verbosity:1})) {
      const [n0,edo0] = parseCvt(s + "\\" + edo).edoSteps;
      if (n0 != n || edo0 != edo) { return false; }
    }
    return true;
  });

});
