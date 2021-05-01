
const _ = require('lodash');
const jsc = require("jsverify");
const pf = require('primes-and-factors');
const Fraction = require('fraction.js');
const BigFraction = require('fraction.js/bigfraction.js');
const Interval = require('../lib/interval.js');
const {pyInterval, pyZDegree, pyDegree, pyOffset, pyA1} = require('../lib/pythagorean.js');

const {degree, offset, pyIntv} = require('./arbitrary.js')

describe("Pythagorean intervals", function() {

  jsc.property("pyDegree(pyInterval(d,o/4)) == d (if d != -1)", degree, offset, function (d,o) {
    if (d != -1) {
      return pyDegree(pyInterval(d,o/4)) == d;
    }
    else {
      return pyDegree(pyInterval(d,o/4)) == 1;
    }
  });

  jsc.property("pyOffset(pyInterval(d,o/4)) == o/4 (if d != -1)", degree, offset, function (d,o) {
    if (d != -1) {
      return pyOffset(pyInterval(d,o/4)).equals(Fraction(o,4));
    }
    else {
      return pyOffset(pyInterval(d,o/4)).equals(Fraction(-o,4));
    }
  });

  jsc.property("pyInterval(d,o/4).recip() == pyInterval(-d,o/4)", degree, offset, function (d,o) {
    return pyInterval(d,o/4).recip().equals(pyInterval(-d,o/4));
  });

  jsc.property("pyZDegree(pyi1.mul(pyi2)) == pyZDegree(pyi1) + pyZDegree(pyi2)", pyIntv, pyIntv, function (pyi1,pyi2) {
    return pyZDegree(pyi1.mul(pyi2)) == pyZDegree(pyi1) + pyZDegree(pyi2);
  });

  jsc.property("pyInterval(±d,o) == pyInterval(±d,0).mul(pyA1.pow(±o))", degree, offset, function(d,o) {
    return pyInterval(d,o).equals(pyInterval(d,0).mul(pyA1.pow(Math.sign(d) * o)));
  });

});
