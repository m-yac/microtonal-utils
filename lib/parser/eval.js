/**
 * A function for evaluating the results of running `grammar.ne`
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module eval
 **/

const pf = require('primes-and-factors');
const Fraction = require('fraction.js');
const Interval = require('../interval.js');
const {pyInterval, pyRedDeg, isPerfectDeg} = require('../pythagorean.js');
const {fjsFactor, fjsSpec, nfjsSpec} = require('../fjs.js');
const {edoApprox, edoHasNeutrals, edoHasSemiNeutrals, updnsFromSymb} = require('../edo.js');
const {colorFromSymb, colorFromNote} = require('../color.js');

/**
 * Class representing an error with a location in a string
 * @extends Error
 */
class LocatedError extends Error {
  /**
   * Create a `LocatedError`
   * @param {string} kind e.g. "Parse error" or "Error"
   * @param {string} message
   * @param {integer} loc the index of where the error occurs in a string
   */
  constructor(kind, message, loc) {
    super(message);
    this.name = "LocatedError";
    this.kind = kind;
    this.offset = loc;
  }
  /**
   * Format a `LocatedError` using the string the error is about, producing a
   * regular `Error` with a nicely formatted `message`
   * @param {string} str
   */
  toError(str) {
    const errStr = "\n" + str + "\n" + " ".repeat(this.offset) + "^\n"
                   + this.kind + ": " + this.message;
    let err = new Error(errStr);
    err.kind = this.kind;
    err.offset = this.offset;
    err.srcStr = str;
    return err;
  }
}

/**
 * A `LocatedError` which is a parse error, i.e. has `kind` "Parse error"
 * @extends LocatedError
 */
class ParseError extends LocatedError {
  /**
   * Create a `ParseError`
   * @param {string} message
   * @param {integer} loc the index of where the error occurs in a string
   */
  constructor(message, loc) {
    super("Parse error", message, loc);
  }
}

/**
 * A `LocatedError` which is some other error, i.e. has `kind` "Error"
 * @extends LocatedError
 */
class OtherError extends LocatedError {
  /**
   * Create an `OtherError`
   * @param {string} message
   * @param {integer} loc the index of where the error occurs in a string
   */
  constructor(message, loc) {
    super("Error", message, loc);
  }
}

/**
  * The default reference note for the evaluator: A4 = 440Hz
  *
  * @constant {Interval}
  */
const defaultRefNote = { intvToA4: Interval(1), hertz: Interval(440) };

/**
 * @typedef {Object} EvalOpts
 * @param {Array.<FJSLike>} [fjsLikeSpecs=[fjsSpec,nfjsSpec]]
 *                          specs to use for FJS-like intervals, tried in the
 *                          order given based on whether they apply to the given
 *                          non-neutral/neutral/semi-neutral base Pythagorean
 *                          interval
 */

function cbnEDOs(a,b) {
  return a && b ? Fraction(1,a).gcd(1,b).d : undefined
}

/**
  * Evaluates the result of running `grammar.ne`
  *
  * @param {Array} e the expression to evaluate
  * @param {{hertz: Interval, intvToA4: Interval}} [refNote=defaultRefNote] the reference note
  * @param {EvalOpts} [opts] various options
  * @returns {{val: Interval, prefEDO: integer}}
  */
function evalExpr(e, r, opts, state) {
  if (!state) { state = {}; }
  let fjsLikeSpecs = [fjsSpec, nfjsSpec];
  if (opts && Array.isArray(opts.fjsLikeSpecs) && opts.fjsLikeSpecs.length > 0) {
    fjsLikeSpecs = opts.fjsLikeSpecs;
  }

  if (Array.isArray(e)) {
    // don't fail in the case of a nested array
    if (Array.isArray(e[0])) {
      console.log("evalExpr: nested arrays")
      return evalExpr(e[0], r, opts, state);
    }

    // 1 | Special cases:
    if (e[0] == "!isoExpr") {
      const args = e[1].map(([e1i,loc]) => [evalExpr(e1i, r, opts, state).val, loc]);
      const loc = e[2];
      // figure out the shape of the isoharmonic expression
      let [fstIntv, sndIntv, qs] = [undefined, undefined, 0];
      for (let i = 0; i < args.length; i++) {
        const [ai,loc] = args[i];
        if (ai === "?") {
          if (qs == 1) {
            throw new LocatedError("Isoharmonic expression cannot contain more than one `?`", loc);
          }
          qs++;
        }
        else if (ai !== "-") {
          if (fstIntv == undefined) {
            fstIntv = i;
          }
          else if (sndIntv == undefined) {
            sndIntv = i;
          }
        }
      }
      if (fstIntv == undefined || sndIntv == undefined) {
        throw new OtherError("Isoharmonic expression must contain at least two intervals", loc);
      }
      // check to make sure the whole expression is coherent
      let res = undefined;
      for (let i = 0; i < args.length; i++) {
        const [ai,loc] = args[i];
        const n = Fraction(i - fstIntv, sndIntv - fstIntv);
        let expected = undefined;
        try {
          expected = args[sndIntv][0].iso(args[fstIntv][0], n);
        }
        catch (e) {
          throw new OtherError("Invalid/impossible position in isoharmonic expression", loc);
        }
        if (ai === "?") {
          res = expected;
        }
        else if (ai !== "-" && !expected.equals(ai)) {
          const str = expected.hasFactors() ? ", should be: " + expected.toNthRootString() : "";
          throw new OtherError("Incorrect interval in isoharmonic expression" + str, loc);
        }
      }
      // return the result
      if (res) {
        return { val: res };
      }
      else if (args.length == 2) {
        return { val: args[1][0].div(args[0][0]) };
      }
      else {
        throw new OtherError("This is a valid isoharmonic chord, but it must contain one `?` to give a result", loc);
      }
    }
    else if (e[0] == "!refIntvToA4") {
      return { val: (r || defaultRefNote).intvToA4 };
    }
    else if (e[0] == "!refHertz") {
      return { val: (r || defaultRefNote).hertz };
    }
    else if (e[0] == "!med") {
      const arg0 = evalExpr(e[1], r, opts, state).val;
      const arg1 = evalExpr(e[2], r, opts, state).val;
      const loc = e[3];
      if (arg0.isFrac() && arg1.isFrac()) {
        return { val: arg0.med(arg1) };
      }
      else {
        throw new OtherError("One of the arguments to `med` is not a fraction", loc);
      }
    }
    else if (e[0] == "!nobleMed") {
      const arg0 = evalExpr(e[1], r, opts, state).val;
      const arg1 = evalExpr(e[2], r, opts, state).val;
      const [nm, loc] = [e[3], e[4]];
      if (arg0.isFrac() && arg1.isFrac()) {
        return { val: arg0.nobleMed(arg1) };
      }
      else {
        throw new OtherError("One of the arguments to `" + nm + "` is not a fraction", loc);
      }
    }
    else if (e[0] == "!isoUp1" || e[0] == "!isoDown1") {
      const arg0 = evalExpr(e[1], r, opts, state).val;
      const [nm, loc] = [e[2], e[3]];
      try {
        return { val: e[0] == "!isoUp1" ? arg0.isoUp()
                                        : arg0.isoDown() };
      }
      catch (err) {
        const str = e[0] == "!isoUp1" ? "greater than 1/2" : "less than 2";
        throw new OtherError("Argument to `" + nm + "` must be " + str, loc);
      }
    }
    else if (e[0] == "!isoUp2" || e[0] == "!isoDown2") {
      const arg0 = evalExpr(e[1], r, opts, state).val;
      const arg1 = evalExpr(e[2], r, opts, state).val;
      const [nm, loc] = [e[3], e[4]];
      try {
        return { val: e[0] == "!isoUp2" ? arg0.isoUp(arg1)
                                        : arg0.isoDown(arg1) };
      }
      catch (err) {
        const bound = e[0] == "!isoUp2" ? arg1.div(2) : arg1.mul(2);
        let str;
        // if arg1 is a 1, 2, 3, or 4th root
        if (bound.pow(3).isFrac() || bound.pow(4).isFrac()) {
          str = bound.toNthRootString();
        }
        else {
          str = bound.toCents().toFixed(5) + "c";
        }
        str = (e[0] == "!isoUp2" ? "greater than 1/2 * the first, i.e. > "
                                 : "less than 2 * the first, i.e. < ") + str;
        throw new OtherError("Second argument to `" + nm + "` must be " + str, loc);
      }
    }
    else if (e[0] == "!iso") {
      const arg0 = evalExpr(e[1], r, opts, state).val;
      const arg1 = evalExpr(e[2], r, opts, state).val;
      const arg2 = evalExpr(e[3], r, opts, state).val;
      const loc = e[4];
      try {
        return { val: arg0.iso(arg1, arg2) };
      }
      catch (err) {
        throw new OtherError("Result of `iso` out of range", loc);
      }
    }
    else if (e[0] == "!cents") {
      const arg0 = evalExpr(e[1], r, opts, state).val.div(1200);
      if (arg0.d < 100) {
        return { val: Interval(2).pow(arg0), prefEDO: arg0.d };
      }
      if (arg0.d < 1000) {
        return { val: Interval(2).pow(arg0) };
      }
      return { val: Interval(2 ** arg0.valueOf()) };
    }
    else if (e[0] == "!hertz") {
      const arg0 = evalExpr(e[1], r, opts, state).val;
      const arg1 = evalExpr(e[2], r, opts, state).val;
      const loc = e[3];
      if (arg0.compare(0) <= 0) {
        throw new OtherError("Hertz value cannot be negative or zero", loc);
      }
      return { val: Interval(arg0.valueOf()).div(arg1) };
    }
    else if (e[0] == "!edoApprox") {
      const arg0 = evalExpr(e[1], r, opts, state).val;
      const arg1 = evalExpr(e[2], r, opts, state).val;
      return { val: Interval(2).pow(edoApprox(arg1, arg0)).pow(1,arg1), prefEDO: arg1 };
    }
    else if (e[0] == "!inEDO") {
      const arg1 = evalExpr(e[2], r, opts, state).val;
      state.edo = arg1;
      const arg0 = evalExpr(e[1], r, opts, state).val;
      return { val: Interval(2).pow(arg0).pow(1,arg1), prefEDO: arg1 };
    }
    else if (e[0] == "!edoTT") { // `state.edo` should be set from "!inEDO"
      const loc = e[1];
      if (state.edo % 2 == 0) {
        return { val: state.edo / 2 };
      }
      else {
        throw new OtherError(state.edo + "-EDO does not have a tritone", loc);
      }
    }
    else if (e[0] == "!updnsSymb") { // `state.edo` should be set from "!inEDO"
      if (e[2][0] == "!perfPyIntv" && ![1,4,5].includes(pyRedDeg(e[2][1]))) {
        const [uds, deg, loc] = [e[1], e[2][1], e[3]];
        if (state.edo % 7 != 0) {
          throw new OtherError((uds > 0 ? "^" : "v").repeat(uds) + "P"
                                 + deg + " is not a valid interval ("
                                 + deg + " is not a perfect scale degree and "
                                 + state.edo + " is not a \"perfect\" EDO"
                                 + " in ups-and-downs notation)", loc);
        }
        try { return { val: updnsFromSymb(state.edo, uds, pyInterval(deg, 0)) }; }
        catch (err) {
          throw new OtherError(err.message, loc);
        }
      }
      const [uds, pyi, loc] = [e[1], evalExpr(e[2], r, opts, state).val, e[3]];
      try { return { val: updnsFromSymb(state.edo, uds, pyi) }; }
      catch (err) {
        throw new OtherError(err.message, loc);
      }
    }
    else if (e[0] == "!updnsPerfSymb") { // `state.edo` should be set from "!inEDO"
      const [uds, deg, loc] = [e[1], evalExpr(e[2], r, opts, state).val, e[3]];
      if (![1,4,5].includes(pyRedDeg(deg)) && state.edo % 7 != 0) {
        throw new OtherError((uds > 0 ? "^" : "v").repeat(uds)
                               + deg + " is not a valid interval ("
                               + deg + " is not a perfect scale degree and "
                               + state.edo + " is not a \"perfect\" EDO"
                               + " in ups-and-downs notation)", loc);
      }
      try { return { val: updnsFromSymb(state.edo, uds, pyInterval(deg, 0)) }; }
      catch (err) {
        throw new OtherError(err.message, loc);
      }
    }
    else if (e[0] == "!updnsNeutSymb") { // `state.edo` should be set from "!inEDO"
      const [uds, deg, loc] = [e[1], evalExpr(e[2], r, opts, state).val, e[3]];
      if (pyRedDeg(deg) == 1) {
        throw new OtherError((uds > 0 ? "^" : "v").repeat(uds) + "~" +
                               + deg + " is not a valid interval", loc);
      }
      const pyi = pyRedDeg(deg) == 4 ? pyInterval(deg, Math.sign(deg), 2) :
                  pyRedDeg(deg) == 5 ? pyInterval(deg, -Math.sign(deg), 2) :
                                       pyInterval(deg, 0);
      try { return { val: updnsFromSymb(state.edo, uds, pyi) }; }
      catch (err) {
        throw new OtherError(err.message, loc);
      }
    }
    else if (e[0] == "!updnsNote") { // `state.edo` should be set from "!inEDO"
      const [uds, pyi, loc] = [e[1], evalExpr(e[2], r, opts, state).val, e[3]];
      try { return { val: updnsFromSymb(state.edo, uds, pyi) }; }
      catch (err) {
        throw new OtherError(err.message, loc);
      }
    }
    else if (e[0] == "!perfPyIntv") {
      const [d, loc] = [e[1], e[2]];
      if (isPerfectDeg(d)) { return { val: pyInterval(d,0) }; }
      else { throw new OtherError("P" + d + " is not a valid interval ("
                                      + d + " is not a perfect scale degree)", loc); }
    }
    else if (e[0] == "!nonPerfPyIntv") {
      const [d, o, q, loc] = [e[1], e[2], e[3], e[4]];
      if (!isPerfectDeg(d)) { return { val: pyInterval(d,o) }; }
      else { throw new OtherError(q + d + " is not a valid interval ("
                                    + d + " is a perfect scale degree)", loc); }
    }
    else if (e[0] == "!augOrDimPyIntv") {
      const [d, a, b, loc] = [e[1], e[2], e[3], e[4]];
      const o = Fraction(a,b);
      const o_np = o.add(o.s,2);
      return { val: isPerfectDeg(d) ? pyInterval(d,o) : pyInterval(d,o_np) };
    }
    else if (e[0] == "!ensureNo2Or3") {
      const [k, loc] = [e[1], e[2]];
      if (k.equals(1) || k.hasExp(2) || k.hasExp(3)) {
        throw new OtherError("FJS accidental must be > 1 and cannot contain a factor or 2 or 3", loc);
      }
      return { val: k };
    }
    else if (e[0] == "!fjsFactor") {
      const arg0 = evalExpr(e[1], r, opts, state).val;
      const spec = e[2];
      return { val: fjsFactor(arg0, spec) };
    }
    else if (e[0] == "!fjsPy") {
      const [f, loc] = [e[1], e[2]];
      const spec = fjsLikeSpecs[0];
      return evalExpr(f(spec), r, opts, state);
    }
    else if (e[0] == "!fjsNPy") {
      const [f, loc] = [e[1], e[2]];
      const spec = fjsLikeSpecs.find(spec => spec.hasNeutrals);
      if (spec) {
        return evalExpr(f(spec), r, opts, state);
      }
      else {
        throw new OtherError("Neutral FJS-like interval not supported", loc);
      }
    }
    else if (e[0] == "!fjsSNPy") {
      const [f, loc] = [e[1], e[2]];
      const spec = fjsLikeSpecs.find(spec => spec.hasSemiNeutrals);
      if (spec) {
        return evalExpr(f(spec), r, opts, state);
      }
      else {
        throw new OtherError("Semi-neutral FJS-like interval not supported", loc);
      }
    }
    else if (e[0] == "!clrIntv") {
      const cs = evalExpr(e[1], r, opts, state).val;
      const m  = evalExpr(e[2], r, opts, state).val;
      const pps = e[3].map(ei => evalExpr(ei, r, opts, state).val); // prime powers
      const ps = pps.map(pp => pp.factors()[0][0]); // the primes in pps
      const d = evalExpr(e[4], r, opts, state).val;
      const loc = e[5];
      // ensure the list is decreasing
      if (pps.length > 0 && !ps.every((p,i) => i == 0 || p <= ps[i-1])) {
        throw new OtherError("Invalid color prefix (non-decreasing)", loc);
      }
      // ensure the list has only exact repetition
      if (pps.length > 0 && !pps.every((pp,i) => i == 0 || ps[i] != ps[i-1] || pp.equals(pps[i-1]))) {
        throw new OtherError("Invalid color prefix (bad repetition)", loc);
      }
      const i = pps.reduce(((a,b) => a.mul(b)), Interval(1));
      return { val: colorFromSymb(cs, m, i, d) };
    }
    else if (e[0] == "!clrNote") {
      const pps = e[1].map(ei => evalExpr(ei, r, opts, state).val); // prime powers
      const ps = pps.map(pp => pp.factors()[0][0]); // the primes in pps
      const [pyi, loc] = [evalExpr(e[2], r, opts, state).val, e[3]];
      // ensure the list is decreasing
      if (pps.length > 0 && !ps.every((p,i) => i == 0 || p <= ps[i-1])) {
        throw new OtherError("Invalid color prefix (non-decreasing)", loc);
      }
      // ensure the list has only exact repetition
      if (pps.length > 0 && !pps.every((pp,i) => i == 0 || ps[i] != ps[i-1] || pp.equals(pps[i-1]))) {
        throw new OtherError("Invalid color prefix (bad repetition)", loc);
      }
      const i = pps.reduce(((a,b) => a.mul(b)), Interval(1));
      return { val: colorFromNote(i, pyi) };
    }
    else if (e[0] == "!aclrPP") {
      const [p, x, loc] = [e[1], e[2], e[3]];
      if (p == 1) { return { val: Interval(11).pow(x) }; }
      if (p == 3) { return { val: Interval(13).pow(x) }; }
      if (!pf.isPrime(p)) {
        throw new OtherError("Expected a prime number", loc);
      }
      return { val: Interval(p).pow(x) };
    }
    else if (e[0] == "!clrMPs") {
      const [ps, loc] = [e[1].map(ei => evalExpr(ei, r, opts, state).val), e[2]];
      // ensure the list is decreasing
      if (ps.length > 0 && !ps.every((x,i) => i == 0 || x <= ps[i-1])) {
        throw new OtherError("Invalid color multi prefix (non-decreasing)", loc);
      }
      return { val: ps.reduce((a,b) => a * b, 1) };
    }
    else if (e[0] == "!clrGenPP") {
      const [p, loc] = [e[1], e[2]];
      if (!pf.isPrime(p)) {
        throw new OtherError(p + " is not a prime number", loc);
      }
      return { val: Interval(p) };
    }
    else if (e[0][0] == "!") {
      throw new LocatedError("Panic", "command " + e[0] + " not defined!", 0);
    }

    // for the remaining cases, we evaluate every argument
    const args = e.slice(1).map(ei => evalExpr(ei, r, opts, state));

    // 2 | Operators:
    if (e[0] == "+") { return { val: args[0].val + args[1].val }; }
    if (e[0] == "-") { return { val: args[0].val - args[1].val }; }
    if (e[0] == "*") { return { val: args[0].val * args[1].val }; }
    if (e[0] == "/") { return { val: args[0].val / args[1].val }; }

    // 3 | Otherwise, we assume `e[0]` is a method of `args[0]`
    let ret = { val: args[0].val[e[0]](...args.slice(1).map(argi => argi.val))
              , prefEDO: args.map(argi => argi.prefEDO).reduce(cbnEDOs) };
    // for some operations on intervals we don't want to look at every
    //  argument's perferred EDO
    if (e[0] == "pow") {
      ret.prefEDO = args[0].prefEDO;
    }
    if (e[0] == "red" || e[0] == "reb") {
      if (args[1] && args[1].equals && args[1].equals(2)) {
        ret.prefEDO = args[0].prefEDO;
      }
      else {
        ret.prefEDO = undefined;
      }
    }
    return ret;
  }
  if (e instanceof Interval && e.inPrimeLimit(2) && e.expOf(2) > 2) {
    return { val: e, prefEDO: e.expOf(2).d };
  }
  return { val: e };
}

module['exports'].LocatedError = LocatedError;
module['exports'].ParseError = ParseError;
module['exports'].OtherError = OtherError;
module['exports'].defaultRefNote = defaultRefNote;
module['exports'].evalExpr = evalExpr;
