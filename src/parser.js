/**
 * Interface for parsing interval/note expressions
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module parser
 **/

const ne = require('nearley');
const Fraction = require('fraction.js');
const Interval = require('./interval.js');
const grammar = require('./parser/grammar.js');
const {evalExpr} = require('./parser/eval.js');
const {isPythagorean, pySymb, pyNote} = require('./pythagorean.js');
const {fjsSymb, fjsNote, nfjsParams} = require('./fjs.js');
const {edoApprox, edoPy, updnsSymb, updnsNote} = require('./edo.js');
const {enNames} = require('./english.js');

function mod(a,n) {
  return ((a % n) + n) % n;
}

function expectedSymbols(parser) {
  let symbs = [];
  const lastColumnIndex = parser.table.length - 2;
  const lastColumn = parser.table[lastColumnIndex];
  const expectantStates = lastColumn.states
      .filter(function(state) {
          var nextSymbol = state.rule.symbols[state.dot];
          return nextSymbol && typeof nextSymbol !== "string";
      });

  const stateStacks = expectantStates
      .map(function(state) {
          return parser.buildFirstStateStack(state, []) || [state];
      }, parser);
  // Display each state that is expecting a terminal symbol next.
  stateStacks.forEach(function(stateStack) {
      var state = stateStack[0];
      var nextSymbol = state.rule.symbols[state.dot];
      var symbolDisplay = parser.getSymbolDisplay(nextSymbol);
      symbs.push(symbolDisplay);
  }, parser);

  // remove duplicates
  symbs = [...new Set(symbs)];
  symbs.sort((a,b) => a.length - b.length);
  if (symbs.length > 1) {
    symbs[symbs.length-1] = "or " + symbs[symbs.length-1];
  }
  return "expected a " + symbs.join(", ");
}

/**
 * @typedef {Object} RawParseResult
 * @property {string} type either "interval" or "note"
 * @property {Interval} intv the resulting interval (to the reference, if
 *                           type is "note")
 * @property {{hertz: Interval, intvToA4: Interval}} refNote the reference note
 * @property {integer=} prefEDO the preferred EDO, if any, of the interval
 */

/**
  * Parses the given string
  *
  * @param {string} str
  * @returns {RawParseResult}
  */
function parse(str) {

  const parser = new ne.Parser(ne.Grammar.fromCompiled(grammar));
  try { parser.feed(str); }
  catch (err) {
    throw new Error ("Parse error at col " + err.offset + ", " + expectedSymbols(parser));
  }
  let results = parser.results;

  for (let i = 0; i < results.length; i++) {
    const res = evalExpr(results[i].expr, results[i].refNote);
    results[i].val = res.val;
    results[i].prefEDO = res.prefEDO;
  }

  if (results.length == 0) {
    try { parser.feed("$"); }
    catch (err) {
      throw new Error ("Parse error at col " + err.offset + ", " + expectedSymbols(parser));
    }
  }
  if (results.some(d => d.type[0] == "interval" && d.type[1] == "symbol")) {
    results = results.filter(d => !(d.type[0] == "interval" && d.type[1] != "symbol"));
  }
  if (results.some(d => d.type[0] == "note" && d.type[1] == "symbol")) {
    results = results.filter(d => !(d.type[0] == "note" && d.type[1] != "symbol"));
  }
  if (results.length > 1) {
    console.log("Parse was ambiguous! Full results:");
    console.dir(parser.results, { depth: null });
  }
  let ret = { type: results[0].type[0]
            , intv: results[0].val
            , refNote: results[0].refNote
            , prefEDO: results[0].prefEDO };

  // If `intv` is an EDO step (i.e. a fractional power of two),
  if (Object.entries(ret.intv).length == (ret.intv['2'] != null)) {
    let e2 = ret.intv['2'] || Fraction(0);
    // forget `ret.prefEDO` if `ret.intv` is not `2^(k/prefEDO)` (sanity check)
    if (ret.prefEDO && e2.mul(ret.prefEDO).d != 1) {
      delete ret.prefEDO;
    }
    // set `ret.prefEDO` if `ret.intv` is a simple enough power of two
    if (!ret.prefEDO && (e2.d == 2 || e2.d == 3 || e2.d == 4)) {
      ret.prefEDO = 12;
    }
    if (!ret.prefEDO && 4 < e2.d && e2.d <= 60) {
      ret.prefEDO = e2.d;
    }
  }
  // Otherwise, forget `ret.prefEDO` (sanity check)
  else {
    delete ret.prefEDO;
  }

  return ret;
}

/**
 * @typedef {Object} IntvParseResult
 * @property {string} type always "interval"
 * @property {number} cents the resulting interval converted to cents
 * @property {Interval} intv the resulting interval object
 * @property {Fraction=} ratio the resulting interval as a JI ratio
 * @property {number=} tenneyHD the Tenney harmonic distance of the resulting
 *                              interval as a JI ratio
 * @property {Pair.<integer,integer>=} edoSteps the resulting interval as some
 *                                              number of EDO steps
 * @property {Object.<string,string>} symb various symbols for the resulting
 *                                         interval, including FJS,
 *                                         Neutral FJS, and ups-and-downs
 *                                         notations
 * @property {Array.<string>} english (experimental) english name for the
 *                                    resulting interval, based on
 *                                    Neutral FJS and ups-and-downs notations
 */

/**
 * @typedef {Object} NoteParseResult
 * @property {string} type always "note"
 * @property {number} freq the resulting interval converted to hertz
 * @property {Interval} intvToRef the resulting interval to the reference
 * @property {Pair.<integer,integer>=} edoStepsToRef the resulting interval as
 *                                                   some number of EDO steps
 *                                                   to the reference
 * @property {ReferenceNote} ref the reference note
 * @property {Object.<string,string>} symb various symbols for the resulting
 *                                         interval, including FJS and
 *                                         ups-and-downs notations
 */

/**
 * @typedef {Object} ReferenceNote
 * @property {Interval} hertz
 * @property {Interval} intvToA4
 * @property {Pair.<integer,integer>=} edoStepsToA4
 */

/**
  * Parses the given string and converts it to a few other convenient forms
  *
  * @param {string} str
  * @returns {IntvParseResult|NoteParseResult}
  */
function parseCvt(str) {
  let {type, intv, refNote, prefEDO} = parse(str);
  let ret = { type: type };
  if (type == "interval") {
    ret.cents = intv.toCents();
    ret.intv = intv;
    try {
      ret.ratio = intv.toFrac();
      ret.tenneyHD = intv.tenneyHD();
    } catch (_) {}
    if (prefEDO) {
      let e2 = (intv['2'] || Fraction(0)).mul(prefEDO);
      ret.edoSteps = [e2.s*e2.n, prefEDO];
    }
    ret.symb = {};
    let fjs = fjsSymb(intv);
    let nfjs = fjsSymb(intv, nfjsParams);
    if (fjs) {
      ret.symb['FJS'] = fjs;
    }
    if (nfjs && nfjs != fjs) {
      ret.symb['Neutral FJS'] = nfjs;
    }
    if (prefEDO) {
      let e2 = (intv['2'] || Fraction(0)).mul(prefEDO);
      ret.symb['ups-and-downs'] = updnsSymb(prefEDO,e2.s*e2.n).map(s => s + "\\" + prefEDO);
    }
    if (!nfjs && isPythagorean(intv)) {
      ret.symb['other'] = pySymb(intv);
    }
    if (intv.equals(Interval(2).sqrt())) {
      ret.symb['other'] = "TT";
    }
    const nms = enNames(intv, {prefEDO: prefEDO});
    if (nms.length > 0) {
      ret.english = nms;
    }
  }
  if (type == "note") {
    ret.hertz = refNote.hertz.mul(intv).valueOf();
    const intvToA4 = intv.mul(refNote.intvToA4);
    const closest12EDO = edoApprox(12, intvToA4);
    const diffTo12EDO = intvToA4.div(Interval(2).pow(closest12EDO,12)).toCents();
    ret.tuningMeter = updnsNote(12, mod(closest12EDO+9,12)-9).join("/") + " "
                      + (diffTo12EDO == 0 ? "±" : diffTo12EDO > 0 ? "+" : "-")
                      + Math.abs(diffTo12EDO).toFixed(1) + "c";
    ret.intvToRef = intv;
    if (prefEDO) {
      let e2 = (intv['2'] || Fraction(0)).mul(prefEDO);
      ret.edoStepsToRef = [e2.s*e2.n, prefEDO];
    }
    ret.ref = { hertz: refNote.hertz.valueOf()
              , intvToA4: refNote.intvToA4 };
    ret.symb = {};

    let fjs = fjsNote(intvToA4);
    if (fjs) {
      ret.symb['FJS'] = fjs;
    }
    if (prefEDO) {
      const refEDOStepsToA4 = edoPy(prefEDO, refNote.intvToA4);
      ret.ref.edoStepsToA4 = [refEDOStepsToA4, prefEDO];
      let e2 = (intv['2'] || Fraction(0)).mul(prefEDO).add(refEDOStepsToA4);
      ret.symb['ups-and-downs'] = updnsNote(prefEDO,e2.s*e2.n).map(s => s + "\\" + prefEDO);
    }
  }
  return ret;
}

module['exports'].parse = parse;
module['exports'].parseCvt = parseCvt;
