// import everything from `lib/` as well as `Fraction` from fraction.js
const Fraction = require('fraction.js');
const Interval = require('./lib/interval.js');
const {pyInterval, pyA1, isPythagorean, pyGenerator, pyOctaves, pyDegree, pyZDegree, pyOffset, pyRedDeg, isPerfectDeg, pyQuality, pyDegreeString, pySymb, baseNoteIntvToA, octaveOfIntvToA4, pyNote} = require('./lib/pythagorean.js');
const {FJSLike, fjsRoT, fjsFifthSeq, fjsSpec, nfjsRoT, nfjsFifthSeq, nfjsSpec, fjsRegions, fjsFifthShift, fjsComma, fjsFactor, fjsAccidentals, fjsSymb, fjsNote} = require('./lib/fjs.js');
const {edoApprox, edoPrimeApprox, edoApproxConsistentWithErr, edoApproxConsistent, edoIntvIsConsistent, edoPy, edoPyInv, edoPyComma, edoHasNeutrals, edoHasSemiNeutrals, updnsSymbCache, fmtUpdnsSymb, updnsSymb, updnsFromSymb, updnsNoteCache, updnsNote} = require('./lib/edo.js');
const {colorPrimeZDegree, colorZDegree, colorDegree, colorMagnitude, colorPrimePrefix, colorMultiPrefix, colorFactorPrefix, colorPrefix, colorSymb, colorFromSymb, colorNote, colorFromNote, colorTemperament} = require('./lib/color.js');
const {ratioPermsByNo2sHeight, redRatiosByNo2sHeight, rebRatiosByNo2sHeight, ratioPermsByHeight, ratiosByHeight, ratiosWithDenom, ratiosWithNumer, ratiosByDenom, ratiosByNumer, ratiosByDenomSorted, ratiosByNumerSorted, newRatiosInOddLimit, ratiosInOddLimit, ratiosInOddLimitSorted} = require('./lib/sets.js');
const {bestRationalApproxsByNo2sHeightIterationSize, bestRationalApproxsByNo2sHeight, bestRationalApproxsByHeightIterationSize, bestRationalApproxsByHeight, bestRationalApproxsByDenom, bestRationalApproxsByDiff, bestRationalApproxsOfEDOByStep, bestRationalApproxsOfEDOByDist, bestEDOApproxsByEDO, bestEDOApproxsByDiff} = require('./lib/approx.js');
const {enNames} = require('./lib/english.js');
const {LocatedError, ParseError, OtherError, defaultRefNote, evalExpr} = require('./lib/parser/eval.js');
const {parseFromRule, parseMonzo, parsePySymb, parsePyNote, parseFJSSymb, parseFJSNote, parseUpdnsSymb, parseUpdnsNote, parseColorSymb, parseColorNote, parse, parseCvt} = require('./lib/parser.js');
// increase the implicit printing depth
const util = require('util');
Object.prototype[util.inspect.custom] = function(depth, options) {
  const newOptions = Object.assign({}, options, {depth: null, customInspect: false});
  return util.inspect(this, newOptions);
};
