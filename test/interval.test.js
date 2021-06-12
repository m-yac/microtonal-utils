
const _ = require('lodash');
const jsc = require("jsverify");
const pf = require('primes-and-factors');
const Fraction = require('fraction.js');
const BigFraction = require('fraction.js/bigfraction.js');
const Interval = require('../lib/interval.js');

const {posInt, nzPosInt, frac, nzPosFrac,
       monzo, intv, intvFromFrac, intvFromNthRoot} = require('./arbitrary.js')

function removeTrailingZeros(arr) {
  while (arr[arr.length - 1] == 0) {
    arr.pop();
  }
  return arr;
}

// From: https://floating-point-gui.de/errors/comparison/
function nearly_equal(a, b, epsilon = 1e-15) {
  const MIN_NORMAL = 2.225e-308;
  const diff = Math.abs(a - b);

  if (a == b) {
    return true;
  }
  else if (a == 0 || b == 0 || diff < MIN_NORMAL) {
    return diff < (epsilon * MIN_NORMAL);
  }
  else {
    const denom = Math.min(Math.abs(a) + Math.abs(b), Number.MAX_VALUE);
    return diff / denom < epsilon;
  }
}

describe("Interval constructors and conversions", function() {

  jsc.property("Interval(n).factors() is the prime factorization of n", nzPosInt, function(n) {
    const fact = pf.getPrimeExponentObject(n);
    const fracFact = _.mapValues(fact, k => Fraction(k));
    return _.isEqual(Interval(n).factors(), Object.entries(fracFact));
  });

  jsc.property("Interval(a/b).factors() is the prime factorization of a/b", nzPosFrac, function(fr) {
    const fact = _.mergeWith(pf.getPrimeExponentObject(fr.n),
                             pf.getPrimeExponentObject(fr.d),
                             (nk, dk) => (nk || 0) - (dk || 0));
    const fracFact = _.mapValues(fact, k => Fraction(k));
    return _.isEqual(Interval(fr).factors(), Object.entries(fracFact));
  });

  jsc.property("Interval(fr).toFrac() == fr", nzPosFrac, function(fr) {
    return Interval(fr).toFrac().equals(fr);
  });

  jsc.property("Interval(monzo).toMonzo() == monzo", monzo, function(mz) {
    return _.isEqual(_.mapValues(Interval(mz).toMonzo(), k => Fraction(k)),
                     _.mapValues(removeTrailingZeros(mz), k => Fraction(k)));
  });

  jsc.property("Interval(2).pow(fr).toCents() == fr.mul(1200)", frac, function(fr) {
    return Interval(2).pow(fr).toCents() == fr.mul(1200).valueOf();
  });

});

describe("Interval and Fraction operations", function() {

  jsc.property("mul: Interval(fr1).mul(fr2) == fr1.mul(fr2)", nzPosFrac, nzPosFrac, function(fr1,fr2) {
    return Interval(fr1).mul(fr2).toFrac().equals(Fraction(fr1).mul(fr2));
  });

  jsc.property("div: Interval(fr1).div(fr2) == fr1.div(fr2)", nzPosFrac, nzPosFrac, function(fr1,fr2) {
    return Interval(fr1).div(fr2).toFrac().equals(Fraction(fr1).div(fr2));
  });

  jsc.property("recip: Interval(fr).recip() == fr.inverse()", nzPosFrac, function(fr) {
    return Interval(fr).recip().toFrac().equals(Fraction(fr).inverse());
  });

  jsc.property("pow: Interval(fr).pow(n) == fr.pow(n)", nzPosFrac, jsc.integer(-60,60), function(fr, n) {
    return Interval(fr).pow(n).toFracBig().equals(BigFraction(fr).pow(n));
  });

  jsc.property("equals: Interval(fr1).equals(f2) iff fr1.equals(fr2)", nzPosFrac, nzPosFrac, function(fr1,fr2) {
    return Interval(fr1).equals(fr2) == fr1.equals(fr2);
  });

  jsc.property("compare: Interval(fr1).compare(f2) iff fr1.compare(fr2)", nzPosFrac, nzPosFrac, function(fr1,fr2) {
    return Interval(fr1).compare(fr2) == fr1.compare(fr2);
  });

  jsc.property("valueOf: Interval(fr).valueOf() ~= fr.valueOf()", nzPosFrac, function(fr) {
    return nearly_equal(Interval(fr).valueOf(), fr.valueOf());
  });

});

describe("Other Interval operations", function() {

  jsc.property("pow/mul: i.pow(n) == i.mul(i).mul(i) ... .mul(i)", intv, jsc.integer(0,15), function(i,n) {
    let ip = Interval(1);
    for (let m = 0; m < n; m++) {
      ip = ip.mul(i);
    }
    return i.pow(n).equals(ip);
  });

  jsc.property("pow/div: i.pow(-n) == i.div(i).div(i) ... .div(i)", intv, jsc.integer(0,15), function(i,n) {
    let ip = Interval(1);
    for (let m = 0; m < n; m++) {
      ip = ip.div(i);
    }
    return i.pow(-n).equals(ip);
  });

  jsc.property("pow: i.pow(fr).pow(fr.inverse()) == i", intv, jsc.suchthat(frac, fr => !fr.equals(0)), function(i,fr) {
    return i.pow(fr).pow(fr.inverse()).equals(i);
  });

  jsc.property("root/toNthRoot: Interval(fr).root(n).toNthRoot() == {k: fr, n: n}", nzPosFrac, nzPosInt, function(fr,m) {
    const {k,n} = Interval(fr).root(m).toNthRootBig();
    return m % Number(n) == 0 && k.pow(m/Number(n)).equals(fr);
  });

  jsc.property("root/valueOf: Interval(fr).root(n).valueOf() ~= Math.pow(fr, 1/n)", nzPosFrac, nzPosInt, function(fr,m) {
    return nearly_equal(Interval(fr).root(m).valueOf(), Math.pow(fr.valueOf(), 1/m));
  });

  jsc.property("factorOut: i1 == i2.pow(i1.factorOut(i2)[0]).mul(i1.factorOut(i2)[1])", intv, intv, function(i1,i2) {
    const [n, j] = i1.factorOut(i2);
    return i1.equals(i2.pow(n).mul(j));
  });

  jsc.property("isPrimeLimit: i.inPrimeLimit(k) for all k >= i.primeLimit()", intv, posInt, function(i,k) {
    return i.inPrimeLimit(i.primeLimit() + k);
  });

  jsc.property("isPrimeLimit: !i.inPrimeLimit(k) for all k < i.primeLimit()", intv, nzPosInt, function(i,k) {
    return !i.inPrimeLimit(i.primeLimit() - k);
  });

  jsc.property("isOddLimit: Interval(fr).inOddLimit(k) for all k >= Interval(fr).oddLimit()", intvFromFrac, posInt, function(i,k) {
    return i.inOddLimit(i.oddLimit() + k);
  });

  jsc.property("isOddLimit: !Interval(fr).inOddLimit(k) for k < Interval(fr).oddLimit()", intvFromFrac, nzPosInt, function(i,k) {
    return !i.inOddLimit(i.oddLimit() - k);
  });

  jsc.property("oddLimit: Interval(odd,even).oddLimit() == Interval(even,odd).oddLimit() == odd", posInt, nzPosInt, function(a,b) {
    // `o,e` are a relatively prime pair where `o` is odd and `e` is even
    const r = Fraction(2*a+1,2*b);
    const [o,e] = [r.n,r.d];
    return Interval(o,e).oddLimit() == o && Interval(e,o).oddLimit() == o;
  });

});
