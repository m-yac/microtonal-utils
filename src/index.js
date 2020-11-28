
const pf = require('primes-and-factors');
var ne = require('nearley');
var Fraction = require('fraction.js');
const Interval = require('./interval.js');
var grammar = require('./grammar.js');
var py = require('./pythagorean.js');
var fjs = require('./fjs.js');

function test(str) {
  const parser = new ne.Parser(ne.Grammar.fromCompiled(grammar));
  parser.feed(str);
  console.dir(parser.results, { depth: null });
}
