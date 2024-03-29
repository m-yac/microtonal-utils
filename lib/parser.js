/**
 * Interface for parsing interval/note expressions
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module parser
 **/

const {mod} = require('./utils.js');
const ne = require('nearley');
const Fraction = require('fraction.js');
const Interval = require('./interval.js');
const grammar = require('./parser/grammar.js');
const {ParseError, OtherError, evalExpr} = require('./parser/eval.js');
const {isPythagorean, pySymb, pyNote} = require('./pythagorean.js');
const {fjsSymb, fjsNote, fjsSpec, nfjsSpec} = require('./fjs.js');
const {edoApprox, edoPy, updnsSymb, updnsNote} = require('./edo.js');
const {colorSymb, colorNote} = require('./color.js');
const {enNames} = require('./english.js');

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
  return "expected " + (symbs.length > 0 ? "a " + symbs.join(", ") : "nothing");
}

/**
  * Returns the raw output of calling the parser on the given string, with no
  * evaluation or post-processing. Use `parse` to get a nicely-formatted version
  * of calling this function with start = "top1".
  *
  * @param {string} str
  * @param {string} [start="top1"] the grammar rule to start parsing from
  * @returns {Array}
  */
function parseFromRule(str, start) {
  if (start === undefined) {
    start = grammar.ParserStart;
  }
  else if (!grammar.ParserRules.some(r => r.name == start)) {
    throw new Error("Invalid start rule: " + start);
  }
  // These first two lines are adapated from the nearley source code of
  //  `ne.Grammar.fromCompiled`, since I can't figure out how to start from a
  //  specific rule using the given API
  const rules = grammar.ParserRules.map(r => new ne.Rule(r.name, r.symbols, r.postprocess));
  const parser = new ne.Parser(new ne.Grammar(rules, start));
  try {
    parser.feed(str);
    // the below will ensure an error is thrown if the input has no parses
    if (parser.results.length == 0) {
      parser.feed("$");
    }
    return parser.results;
  }
  catch (err) {
    if (err.offset != undefined) {
      if (err.name != "LocatedError") {
        err = new ParseError(expectedSymbols(parser), err.offset);
      }
      throw err.toError(str);
    }
    else {
      throw err;
    }
  }
}

/**
  * Parse a monzo.
  *
  * @param {string} str
  * @returns {Interval}
  */
function parseMonzo(str) {
  return parseFromRule(str, "monzo")[0];
}

/**
  * Parse a Pythagorean interval symbol, the inverse of `pySymb`.
  *
  * @param {string} str
  * @returns {Interval}
  */
function parsePySymb(str) {
  return evalExpr(parseFromRule(str, "anyPyIntv")[0]).val;
}

/**
  * Parse a Pythagorean note symbol and return its interval to A4, the inverse
  * of `pyNote`.
  *
  * @param {string} str
  * @returns {Interval}
  */
function parsePyNote(str) {
  return evalExpr(parseFromRule(str, "anyPyNote")[0]).val;
}

/**
  * Parse an FJS interval symbol, the inverse of `fjsSymb`.
  *
  * @param {string} str
  * @param {FJSLike} [spec=fjsSpec]
  * @returns {Interval}
  */
function parseFJSSymb(str, spec) {
  if (!spec) { spec = fjsSpec; }
  const result = parseFromRule(str, "fjsLikeIntv")[0];
  return evalExpr(result, undefined, {fjsLikeSpecs: [spec]}).val;
}

/**
  * Parse an FJS note symbol and return its interval to A4, the inverse of
  * `fjsNote`.
  *
  * @param {string} str
  * @param {FJSLike} [spec=fjsSpec]
  * @returns {Interval}
  */
function parseFJSNote(str, spec) {
  if (!spec) { spec = fjsSpec; }
  const result = parseFromRule(str, "fjsLikeNote")[0];
  return evalExpr(result, undefined, {fjsLikeSpecs: [spec]}).val;
}

/**
  * Parse an ups-and-downs notation symbol and return the number of steps it
  * corresponds to in the given EDO, the inverse of `updnsSymb`.
  *
  * @param {integer} edo
  * @param {string} str
  * @returns {integer}
  */
function parseUpdnsSymb(edo, str) {
  const result = parseFromRule(str, "upsDnsIntv")[0];
  return evalExpr(result, undefined, {}, {edo: edo}).val;
}

/**
  * Parse an ups-and-downs notation note and return the number of steps to A4 it
  * corresponds to in the given EDO, the inverse of `updnsNote`.
  *
  * @param {integer} edo
  * @param {string} str
  * @returns {integer}
  */
function parseUpdnsNote(edo, str) {
  const result = parseFromRule(str, "upsDnsNote")[0];
  return evalExpr(result, undefined, {}, {edo: edo}).val;
}

/**
  * Parse a color notation interval symbol, the inverse of `colorSymb`.
  *
  * @param {string} str
  * @returns {Interval}
  */
function parseColorSymb(str) {
  return evalExpr(parseFromRule(str, "anyClrIntv")[0]).val;
}

/**
  * Parse a color notation note symbol and return its interval to A4, the
  * inverse of `colorNote`.
  *
  * @param {string} str
  * @returns {Interval}
  */
function parseColorNote(str) {
  return evalExpr(parseFromRule(str, "anyClrNote")[0]).val;
}

/**
 * @typedef {Object} ParseResult
 * @property {string} type either "interval", "note", or "edo"
 * @property {string} queryType either "multiplicative", "additive", or "symbol",
 *                              if `type` is not "edo", or "name" otherwise
 * @property {string=} symbolType only defined if queryType is "symbol"
 * @property {Interval} intv the resulting interval (to the reference, if
 *                           type is "note"), if `type` is not "edo"
 * @property {integer} edo the value of the parsed EDO, if `type` is "edo"
 * @property {{hertz: Interval, intvToA4: Interval}} refNote the reference note
 * @property {integer=} prefEDO the preferred EDO, if any, of the interval
 */

/**
  * Parses the given string using the entire grammar and evaluates the result
  *
  * @param {string} str
  * @param {EvalOpts} [opts] options to pass to `evalExpr` from `parser/eval.js`
  * @returns {ParseResult}
  */
function parse(str, opts) {

  let results = parseFromRule(str, "top1");

  try {
    for (let i = 0; i < results.length; i++) {
      try {
        const res = evalExpr(results[i].expr, results[i].refNote, opts);
        results[i].val = res.val;
        results[i].prefEDO = res.prefEDO;
      }
      catch (err) {
        results[i].err = err;
      }
    }
    const resultsNoErrors = results.filter(d => d.err == undefined);
    if (resultsNoErrors.length == 0) {
      throw results[0].err;
    }
    else {
      results = resultsNoErrors;
    }
  }
  catch (err) {
    if (err.offset != undefined) {
      if (err.name != "LocatedError") {
        err = new OtherError(err.message, err.offset);
      }
      throw err.toError(str);
    }
    else {
      throw err;
    }
  }

  if (results.length == 1 && results[0].type[0] == "EDO") {
    return { type: results[0].type[0]
           , queryType: results[0].type[1]
           , edo: parseInt(results[0].val)
           , refNote: results[0].refNote };
  }

  if (results.some(d => d.type[0] == "interval" && d.type[1] == "symbol")) {
    results = results.filter(d => !(d.type[0] == "interval" && d.type[1] != "symbol"));
  }
  if (results.some(d => d.type[0] == "note" && d.type[1] == "symbol")) {
    results = results.filter(d => !(d.type[0] == "note" && d.type[1] != "symbol"));
  }
  if (results.length > 1) {
    console.dir("Parse was ambiguous on: \'" + str + "\' - full results:");
    console.dir(results, { depth: null });
  }
  let ret = { type: results[0].type[0]
            , queryType: results[0].type[1]
            , symbolType: results[0].symbolType
            , intv: results[0].val
            , refNote: results[0].refNote
            , prefEDO: results[0].prefEDO };

  // If `intv` is an EDO step (i.e. a fractional power of two),
  if (ret.intv.inPrimeLimit(2)) {
    let e2 = ret.intv.expOf(2);
    // forget `ret.prefEDO` if `ret.intv` is not `2^(k/prefEDO)` (sanity check)
    if (ret.prefEDO && e2.mul(ret.prefEDO).d != 1) {
      delete ret.prefEDO;
    }
    // forget `ret.prefEDO` if it is less than 2
    if (ret.prefEDO < 2) {
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
 * @typedef {Object} IntvParseCvtResult
 * @property {string} type always "interval"
 * @property {string} queryType either "multiplicative", "additive", or "symbol"
 * @property {string=} symbolType only defined if queryType is "symbol"
 * @property {number} cents the resulting interval converted to cents
 * @property {Interval} intv the resulting interval object
 * @property {ReferenceNote} ref the reference note
 * @property {Fraction=} ratio the resulting interval as a JI ratio
 * @property {Pair.<integer,integer>=} edoSteps the resulting interval as some
 *                                              number of EDO steps
 * @property {Object.<string,number>=} height various heights for the resulting
                                              interval as a JI ratio
 * @property {Object.<string,string>} symb various symbols for the resulting
 *                                         interval, including FJS,
 *                                         Neutral FJS, and ups-and-downs
 *                                         notations
 * @property {Array.<string>} english (experimental) english name for the
 *                                    resulting interval, based on
 *                                    Neutral FJS and ups-and-downs notations
 */

/**
 * @typedef {Object} NoteParseCvtResult
 * @property {string} type always "note"
 * @property {string} queryType either "multiplicative", "additive", or "symbol"
 * @property {string=} symbolType only defined if queryType is "symbol"
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
 * @typedef {Object} EDOParseCvtResult
 * @property {string} type always "edo"
 * @property {string} queryType always "name"
 * @property {integer} edo the EDO parsed
 * @property {ReferenceNote} ref the reference note
 * @property {Array.<EDOStepParseCvtResult>} intvs the intervals of the EDO
 */

/**
 * @typedef {Object} EDOStepParseCvtResult
 * @property {integer} steps the number of EDO steps of the interval
 * @property {number} cents the EDO step interval interval converted to cents
 * @property {string} ups_and_downs the EDO step as an ups-and-downs symbol
 * @property {string} ups_and_downs_verbose
 */

/**
 * @typedef {Object} ReferenceNote
 * @property {Interval} hertz
 * @property {Interval} intvToA4
 * @property {Pair.<integer,integer>=} edoStepsToA4
 */

/**
  * Parses the given string using the entire grammar and converts the result to
  * some convenient forms
  *
  * @param {string} str
  * @param {EvalOpts} [opts] options to pass to `evalExpr` from `parser/eval.js`
  * @returns {IntvParseCvtResult|NoteParseCvtResult}
  */
function parseCvt(str, opts) {
  let {type, queryType, symbolType, intv, edo, refNote, prefEDO} = parse(str, opts);
  let ret = { type: type, queryType: queryType };
  if (symbolType != undefined) { ret.symbolType = symbolType; }
  if (type == "EDO") {
    ret.edo = edo;
    ret.ref = { hertz: refNote.hertz.valueOf()
              , intvToA4: refNote.intvToA4 };
    ret.intvs = [];
    for (let i = 0; i < edo; i++) {
      let entry = { steps: i, cents: 1200 * i / edo };
      entry.ups_and_downs = updnsSymb(edo,i, {useExps:1});
      entry.ups_and_downs_verbose = updnsSymb(edo,i, {verbosity:1, useExps:1});
      ret.intvs.push(entry);
    }
  }
  if (type == "interval") {
    ret.cents = intv.toCents();
    ret.intv = intv;
    ret.ref = { hertz: refNote.hertz.valueOf()
              , intvToA4: refNote.intvToA4 };
    try {
      ret.ratio = intv.toFrac();
      let heights = { benedetti: intv.benedettiHeight()
                    , tenney:    intv.tenneyHD() }
      ret.height = heights;
    } catch (_) {}
    if (prefEDO) {
      let e2 = intv.expOf(2).mul(prefEDO);
      ret.edoSteps = [e2.s*e2.n, prefEDO];
    }
    if (!intv.isFrac()) {
      try {
        const isos = intv.invIso1(6);
        if (isos.length > 0) { ret.iso = isos[0]; };
      } catch (_) { }
    }
    ret.symb = {};
    if (isPythagorean(intv)) {
      ret.symb.py = pySymb(intv);
      ret.symb.py_verbose = pySymb(intv, {verbosity: 1});
    }
    else if (intv.hasFactors()) {
      let fjs = fjsSymb(intv);
      let nfjs = fjsSymb(intv, nfjsSpec);
      if (fjs) {
        ret.symb.FJS = fjs;
      }
      if (nfjs && nfjs != fjs) {
        ret.symb.NFJS = nfjs;
      }
    }
    if (prefEDO) {
      let e2 = intv.expOf(2).mul(prefEDO);
      ret.symb.ups_and_downs =
        updnsSymb(prefEDO,e2.s*e2.n, {useWordDesc:1, useExps:1})
          .map(s => s + "\\" + prefEDO);
      ret.symb.ups_and_downs_verbose =
        updnsSymb(prefEDO,e2.s*e2.n, {verbosity:1, useWordDesc:1, useExps:1})
          .map(s => s + " \\ " + prefEDO);
    }
    if (ret.ratio) {
      try {
        ret.symb.color = colorSymb(intv, {verbosity: 0});
      } catch (_) {}
      try {
        ret.symb.color_verbose = colorSymb(intv, {verbosity: 1});
      } catch (_) {}
    }
    if (intv.equals(Interval(2).sqrt())) {
      ret.symb.other = "TT";
    }
    if (intv.equals(Interval.phi)) {
      ret.symb.other = "phi";
    }
    try {
      const nms = enNames(intv, {prefEDO: prefEDO});
      if (nms.length > 0) {
        ret.english = nms;
      }
    } catch (_) {}
  }
  if (type == "note") {
    ret.hertz = refNote.hertz.mul(intv).valueOf();
    const intvToA4 = intv.mul(refNote.intvToA4);
    const closest12EDO = edoApprox(12, intvToA4);
    const diffTo12EDO = intvToA4.div(Interval(2).pow(closest12EDO,12)).toCents();
    ret.tuningMeter = updnsNote(12, mod(closest12EDO+9,12)-9, {useExps:1}).join("/") + " "
                      + (diffTo12EDO == 0 ? "±" : diffTo12EDO > 0 ? "+" : "-")
                      + Math.abs(diffTo12EDO).toFixed(1) + "c";
    ret.intvToRef = intv;
    if (prefEDO) {
      let e2 = intv.expOf(2).mul(prefEDO);
      ret.edoStepsToRef = [e2.s*e2.n, prefEDO];
    }
    ret.ref = { hertz: refNote.hertz.valueOf()
              , intvToA4: refNote.intvToA4 };
    ret.symb = {};

    if (isPythagorean(intv)) {
      ret.symb.py = pyNote(intv);
    }
    else if (intv.hasFactors()) {
      let fjs = fjsNote(intvToA4);
      if (fjs) {
        ret.symb.FJS = fjs;
      }
    }
    if (prefEDO) {
      const refEDOStepsToA4 = edoPy(prefEDO, refNote.intvToA4);
      ret.ref.edoStepsToA4 = [refEDOStepsToA4, prefEDO];
      let e2 = intv.expOf(2).mul(prefEDO).add(refEDOStepsToA4);
      ret.symb.ups_and_downs = updnsNote(prefEDO,e2.s*e2.n, {useExps:1}).map(s => s + "\\" + prefEDO);
    }
    if (intv.isFrac()) {
      try {
        ret.symb.color = colorNote(intvToA4, {verbosity: 0});
      } catch (_) {}
      try {
        ret.symb.color_verbose = colorNote(intvToA4, {verbosity: 1});
      } catch (_) {}
    }
  }
  return ret;
}

module['exports'].parseFromRule = parseFromRule;
module['exports'].parseMonzo = parseMonzo;
module['exports'].parsePySymb = parsePySymb;
module['exports'].parsePyNote = parsePyNote;
module['exports'].parseFJSSymb = parseFJSSymb;
module['exports'].parseFJSNote = parseFJSNote;
module['exports'].parseUpdnsSymb = parseUpdnsSymb;
module['exports'].parseUpdnsNote = parseUpdnsNote;
module['exports'].parseColorSymb = parseColorSymb;
module['exports'].parseColorNote = parseColorNote;
module['exports'].parse = parse;
module['exports'].parseCvt = parseCvt;
