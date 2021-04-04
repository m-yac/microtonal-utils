/**
 * A function for evaluating the results of running `grammar.ne`
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module eval
 **/

const Fraction = require('fraction.js');
const Interval = require('../interval.js');
const {pyInterval, isPerfectDeg} = require('../pythagorean.js');
const {fjsFactor} = require('../fjs.js');
const {edoApprox, edoPy, edoHasNeutrals, edoHasSemiNeutrals} = require('../edo.js');

/**
 * Class representing an error with a location in a string
 * @extends Error
 */
class LocatedError extends Error {
  /*
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
  /*
   * Format a `LocatedError` using the string the error is about, producing a
   * regular `Error` with a nicely formatted `message`
   * @param {string} str
   */
  toError(str) {
    const errStr = "\n" + str + "\n" + " ".repeat(this.offset) + "^\n"
                   + this.kind + ": " + this.message;
    return new Error(errStr);
  }
}

/**
 * A `LocatedError` which is a parse error, i.e. has `kind` "Parse error"
 * @extends LocatedError
 */
class ParseError extends LocatedError {
  /*
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
  /*
   * Create an `OtherError`
   * @param {string} message
   * @param {integer} loc the index of where the error occurs in a string
   */
  constructor(message, loc) {
    super("Error", message, loc);
  }
}

function cbnEDOs(a,b) {
  return a && b ? Fraction(1,a).gcd(1,b).d : undefined
}

/**
  * Evaluates the result of running `grammar.ne`
  *
  * @param {Array} e the expression to evaluate
  * @param {{hertz: Interval, intvToA4: Interval}} refNote the reference note
  * @returns {{val: Interval, prefEDO: integer}}
  */
function evalExpr(e, r, edo) {
  if (Array.isArray(e)) {
    // don't fail in the case of a nested array
    if (Array.isArray(e[0])) {
      console.log("evalExpr: nested arrays")
      return evalExpr(e[0], r, edo);
    }

    // 1 | Special cases:
    if (e[0] == "!refIntvToA4") {
      return { val: r.intvToA4 };
    }
    else if (e[0] == "!refHertz") {
      return { val: r.hertz };
    }
    else if (e[0] == "!med") { // `edo` should not be defined
      const arg0 = evalExpr(e[1],r).val;
      const arg1 = evalExpr(e[2],r).val;
      if (arg0.isFrac() && arg1.isFrac()) {
        return { val: arg0.med(arg1) };
      }
      else {
        throw new OtherError("One of the arguments to `med` is not a fraction", loc);
      }
    }
    else if (e[0] == "!cents") { // `edo` should not be defined
      const arg0 = Fraction(evalExpr(e[1],r).val).div(1200);
      return { val: Interval(2).pow(arg0)
             , prefEDO: 48 % arg0.d == 0 ? 24 % arg0.d == 0 ? 12 % arg0.d == 0 ? 12 : 24 : 48 : undefined };
    }
    else if (e[0] == "!edoApprox") { // `edo` should not be defined
      const arg0 = evalExpr(e[1],r).val;
      const arg1 = evalExpr(e[2],r).val;
      return { val: Interval(2).pow(edoApprox(arg1, arg0)).pow(1,arg1), prefEDO: arg1 };
    }
    else if (e[0] == "!inEDO") { // `edo` should not be defined
      const arg1 = evalExpr(e[2],r).val;
      const arg0 = evalExpr(e[1],r,arg1).val;
      return { val: Interval(2).pow(arg0).pow(1,arg1), prefEDO: arg1 };
    }
    else if (e[0] == "!edoTT") { // `edo` should be defined
      const loc = e[1];
      if (edo % 2 == 0) {
        return { val: edo / 2 };
      }
      else {
        throw new OtherError(edo + "-EDO does not have a tritone", loc);
      }
    }
    else if (e[0] == "!edoPy") { // `edo` should be defined
      const arg0 = evalExpr(e[1],r,edo).val;
      const loc = e[2];
      try { return { val: edoPy(edo, arg0) }; }
      catch (err) {
        throw new OtherError(err.message, loc);
      }
    }
    else if (e[0] == "!perfPyIntv") { // `edo` may be defined
      const [d, loc] = [e[1], e[2]];
      if (isPerfectDeg(d)) { return { val: pyInterval(d,0) }; }
      else { throw new OtherError("P" + d + " is not a valid interval ("
                                      + d + " is not a perfect scale degree)", loc); }
    }
    else if (e[0] == "!nonPerfPyIntv") { // `edo` may be defined
      const [d, o, q, loc] = [e[1], e[2], e[3], e[4]];
      if (!isPerfectDeg(d)) { return { val: pyInterval(d,o) }; }
      else { throw new OtherError(q + d + " is not a valid interval ("
                                    + d + " is a perfect scale degree)", loc); }
    }
    else if (e[0] == "!augOrDimPyIntv") { // `edo` may be defined
      const [d, a, b, loc] = [e[1], e[2], e[3], e[4]];
      const o = Fraction(a,b);
      const o_np = o.add(o.s,2);
      return { val: isPerfectDeg(d) ? pyInterval(d,o) : pyInterval(d,o_np) };
    }
    else if (e[0] == "!ensureNo2Or3") { // `edo` should not be defined
      const [k, loc] = [e[1], e[2]];
      if ((k['2'] && !k['2'].equals(0)) || (k['3'] && !k['3'].equals(0))) {
        throw new OtherError("FJS accidental cannot contain a factor or 2 or 3", loc);
      }
      return { val: k };
    }
    else if (e[0] == "!fjsFactor") { // `edo` should not be defined
      const arg0 = evalExpr(e[1],r).val;
      return { val: fjsFactor(arg0, e[2]) };
    }
    else if (e[0][0] == "!") {
      throw new LocatedError("Panic", "command " + e[0] + " not defined!", 0);
    }

    // for the remaining cases, we evaluate every argument
    const args = e.slice(1).map(ei => evalExpr(ei,r,edo));

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
  return { val: e, prefEDO: e == 2 ? 1 : undefined };
}

module['exports'].LocatedError = LocatedError;
module['exports'].ParseError = ParseError;
module['exports'].OtherError = OtherError;
module['exports'].evalExpr = evalExpr;
