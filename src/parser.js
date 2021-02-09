/**
 * Interface for parsing interval/note expressions
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module parser
 **/

const ne = require('nearley');
const Fraction = require('fraction.js');
const Interval = require('./interval.js');
const grammar = require('./parser/grammar.js');
const {isPythagorean, pySymb, pyNote} = require('./pythagorean.js');
const {fjsSymb, fjsNote, fjsnParams} = require('./fjs.js');
const {edoPy, updnsSymb, updnsNote} = require('./edo.js');

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
  parser.feed(str);
  let results = parser.results;

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
  let ret = { type: results[0][0]
            , intv: results[0][2]
            , refNote: results[0][4]
            , prefEDO: parseInt(results[0][3]) };

  // If `intv` is an EDO step (i.e. a fractional power of two),
  if (Object.entries(ret.intv).length == (ret.intv['2'] != null)) {
    let e2 = ret.intv['2'] || Fraction(0);
    // forget `ret.prefEDO` if `ret.intv` is not `2^(k/prefEDO)` (sanity check)
    if (ret.prefEDO && e2.mul(ret.prefEDO).d != 1) {
      delete ret.prefEDO;
    }
    // set `ret.prefEDO` if `ret.intv` is a simple enough power of two
    if (!ret.prefEDO && (e2.d == 3 || e2.d == 4)) {
      ret.prefEDO = 12;
    }
    if (!ret.prefEDO && 4 < e2.d && e2.d < 50) {
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
 * @property {Pair.<integer,integer>=} edoSteps the resulting interval as some
 *                                              number of EDO steps
 * @property {Object.<string,string>} symb various symbols for the resulting
 *                                         interval, including FJS,
 *                                         FJS + Neutrals, and ups-and-downs
 *                                         notations
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
    if (prefEDO) {
      let e2 = (intv['2'] || Fraction(0)).mul(prefEDO);
      ret.edoSteps = [e2.s*e2.n, prefEDO];
    }
    ret.symb = {};
    let fjs = fjsSymb(intv);
    let fjsn = fjsSymb(intv, fjsnParams);
    if (fjs) {
      ret.symb['FJS'] = fjs;
    }
    if (fjsn && fjsn != fjs) {
      ret.symb['FJS+Neutrals'] = fjsn;
    }
    if (prefEDO) {
      let e2 = (intv['2'] || Fraction(0)).mul(prefEDO);
      ret.symb['ups-and-downs'] = updnsSymb(prefEDO,e2.s*e2.n).map(s => s + "\\" + prefEDO);
    }
    if (!fjsn && isPythagorean(intv)) {
      ret.symb['other'] = pySymb(intv);
    }
    if (intv.equals(Interval(2).sqrt())) {
      ret.symb['other'] = "TT";
    }
  }
  if (type == "note") {
    ret.hertz = refNote.hertz.mul(intv).valueOf();
    ret.intvToRef = intv;
    if (prefEDO) {
      let e2 = (intv['2'] || Fraction(0)).mul(prefEDO);
      ret.edoStepsToRef = [e2.s*e2.n, prefEDO];
    }
    ret.ref = { hertz: refNote.hertz.valueOf()
              , intvToA4: refNote.intvToA4 };
    ret.symb = {};
    const intvToA4 = intv.mul(refNote.intvToA4);
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
