/**
 * @module parser.js
 * Copyright (c) 2020, Matthew Yacavone (matthew [at] yacavone [dot] net)
 **/

(function(root) {

const ne = require('nearley');
const Interval = require('./interval.js');
var grammar = require('./parser/grammar.js');
var {isPythagorean, pySymb} = require('./pythagorean.js');
var {fjsSymb} = require('./fjs.js');
var {updnsSymb} = require('./edo.js');

function parse(str) {

  const parser = new ne.Parser(ne.Grammar.fromCompiled(grammar));
  parser.feed(str);
  var results = parser.results;

  if (results.length == 0) {
    throw "No parse"
  }
  if (results.some(d => d[0] == "interval" && d[1] == true)) {
    results = results.filter(d => !(d[0] == "interval" && d[1] == false));
  }
  if (results.some(d => d[0] == "note" && d[1] == true)) {
    results = results.filter(d => !(d[0] == "note" && d[1] == false));
  }
  if (results.length > 1) {
    console.log("Parse was ambiguous! Full results:");
    console.dir(parser.results, { depth: null });
  }
  let   result  = { type: results[0][0] };
  const intv    = results[0][2];
  let   prefEDO = parseInt(results[0][3]);
  const refNote = results[0][4];

  // If `intv` is an EDO step (i.e. a fractional power of two),
  if (Object.entries(intv).length == (intv['2'] != null)) {
    let e2 = intv['2'] || Fraction(0);
    // forget `prefEDO` if `intv` is not `2^(k/prefEDO)` (sanity check)
    if (prefEDO && e2.mul(prefEDO).d != 1) {
      prefEDO = null;
    }
    // set `prefEDO` if `intv` is a simple enough power of two
    if (!prefEDO && (e2.d == 3 || e2.d == 4)) {
      prefEDO = 12;
    }
    if (!prefEDO && 4 < e2.d && e2.d < 50) {
      prefEDO = e2.d;
    }
  }
  // Otherwise, forget `prefEDO` (sanity check)
  else {
    prefEDO = null;
  }

  // package everything up nicely
  if (result.type == "interval") {
    result.cents = intv.toCents();
    result.intv = intv;
    result.symb = {};
    let fjs = fjsSymb(intv);
    if (fjs) {
      result.symb['FJS'] = fjs;
    }
    if (prefEDO) {
      let e2 = intv['2'] || Fraction(0);
      result.symb['EDO-steps'] = [e2.mul(prefEDO).n, prefEDO];
      result.symb['ups-and-downs'] = [updnsSymb(prefEDO,e2.mul(prefEDO).n), prefEDO];
    }
    if (!fjs && isPythagorean(intv)) {
      result.symb['other'] = pySymb(intv);
    }
    if (intv.equals(Interval(2).sqrt())) {
      result.symb['other'] = "TT";
    }
  }
  if (result.type == "note") {
    result.tuning = refNote.hertz.mul(intv).valueOf();
    result.intv = intv;
    result.symb = {};
  }
  return result;
}

module['exports'].parse = parse;

})(this);
