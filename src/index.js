
const pf = require('primes-and-factors');
var ne = require('nearley');
var Fraction = require('fraction.js');
const Interval = require('./interval.js');
var grammar = require('./grammar.js');
var {isPythagorean, pySymb} = require('./pythagorean.js');
var {fjsSymb} = require('./fjs.js');

function parse(str) {
  const parser = new ne.Parser(ne.Grammar.fromCompiled(grammar));
  parser.feed(str);
  let results = {};
  parser.results.forEach(r => results[r[0]] = r[1]);
  // extract the preferred EDO
  let edo = null;
  if (results["cents"]) {
    edo = results["cents"][0];
    results["cents"] = results["cents"][1];
  }
  // extract the interval
  let itv;
  if (results["symb"]) {
    itv = results["symb"];
  }
  else {
    if (parser.results.length > 1) {
      console.log("Parse was ambiguous! Full results:")
      console.dir(parser.results, { depth: null });
    }
    itv = Object.entries(results)[0][1];
  }
  // update the preferred EDO if it doesn't already exist
  if (!edo && Object.entries(itv).length == 1 && itv['2']) {
    if (itv['2'].d == 3 || itv['2'].d == 4) {
      edo = 12;
    }
    else if (4 < itv['2'].d && itv['2'].d < 50) {
      edo = itv['2'].d;
    }
  }
  // generate some chord symbols
  let fjs = fjsSymb(itv);
  let symb;
  if (!fjs && isPythagorean(itv)) { symb = pySymb(itv); }
  if (itv.equals(Interval(2).sqrt())) { symb = "TT"; }
  // package everything up nicely
  let ret = {cents: itv.toCents(), itv: itv};
  if (fjs) { ret.fjs = fjs; }
  if (symb) { ret.symb = symb; }
  if (edo) { ret.edoSteps = (itv['2'].mul(edo).n) + "\\" + edo; }
  return ret;
}
