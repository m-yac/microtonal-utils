
const Fraction = require('fraction.js');
const Interval = require('../interval.js');
const {pyInterval, isPerfectDeg, baseNoteIntvToA} = require('../pythagorean.js');
const {fjsFactor} = require('../fjs.js');
const {edoPy, edoHasNeutrals, edoHasSemiNeutrals} = require('../edo.js');

function cbnEDOs(a,b) {
  return a && b ? Fraction(1,a).gcd(1,b).d : undefined
}

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
    if (e[0] == "!refHertz") {
      return { val: r.hertz };
    }
    if (e[0] == "!med") { // `edo` should be undefined
      const arg0 = evalExpr(e[1],r).val;
      const arg1 = evalExpr(e[2],r).val;
      if (arg0.isFrac() && arg1.isFrac()) {
        return { val: arg0.med(arg1) };
      }
      else {
        throw "One of the arguments to `med` is not a fraction"
      }
    }
    if (e[0] == "!cents") { // `edo` should be undefined
      const arg0 = Fraction(evalExpr(e[1],r).val).div(1200);
      return { val: Interval(2).pow(arg0)
             , prefEDO: 48 % arg0.d == 0 ? 24 % arg0.d == 0 ? 12 % arg0.d == 0 ? 12 : 24 : 48 : undefined };
    }
    if (e[0] == "!inEDO") { // `edo` should be undefined
      const arg1 = evalExpr(e[2],r).val;
      const arg0 = evalExpr(e[1],r,arg1).val;
      return { val: Interval(2).pow(arg0).pow(1,arg1), prefEDO: arg1 };
    }
    if (e[0] == "!edoTT") { // `edo` should be defined
      if (edo % 2 == 0) {
        return { val: edo / 2 };
      }
      else {
        throw edo + "-EDO does not have a tritone!";
      }
    }
    if (e[0] == "!edoPy") { // `edo` should be defined
      const arg0 = evalExpr(e[1],r,edo).val;
      return { val: edoPy(edo, arg0) };
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


module['exports'].evalExpr = evalExpr;
