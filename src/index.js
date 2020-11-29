
const pf = require('primes-and-factors');
var ne = require('nearley');
var Fraction = require('fraction.js');
const Interval = require('./interval.js');
var grammar = require('./grammar.js');
var py = require('./pythagorean.js');
var {fjsSymb} = require('./fjs.js');

function parse(str) {
  const parser = new ne.Parser(ne.Grammar.fromCompiled(grammar));
  parser.feed(str);
  let results = {};
  parser.results.forEach(r => results[r[0]] = r[1]);
  let itv;
  if (results["symb"]) {
    itv = results["symb"];
  } else {
    if (parser.results.length > 1) {
      console.log("Parse was ambiguous! Full results:")
      console.dir(parser.results, { depth: null });
    }
    itv = parser.results[0][1];
  }
  let fjs = fjsSymb(itv);
  return {cents: itv.toCents(), itv: itv, fjs: fjs};
}
