
(function(root) {

const Fraction = require('fraction.js');
const Interval = require('../interval.js');
const {pyInterval, redDeg, isPerfectDeg} = require('../pythagorean.js');
const {fjsFactor} = require('../fjs.js');
const {edoPy, edoHasNeutrals, edoHasSemiNeutrals} = require('../edo.js');

function perfPyInterval(d,o,reject) {
  return isPerfectDeg(d) ? pyInterval(d,o) : reject;
}
function nonPerfPyInterval(d,o,reject) {
  return isPerfectDeg(d) ? reject : pyInterval(d,o);
}
function augOrDimPyInterval(d,a,b,reject) {
  const o = Fraction(a,b);
  if (o.d != b) {
    return reject;
  }
  const o_np = o.add(o.s,2);
  return isPerfectDeg(d) ? pyInterval(d,o) : pyInterval(d,o_np);
}

function ensureNo2Or3(i,reject) {
  return (i['2'] && i['2'] != 0) || (i['3'] && i['3'] != 0) ? reject : i;
}

function cbnEDOs(a,b) {
  if (a && b) { return Fraction(1,a).gcd(1,b).d; }
  else { return null; }
}

function baseNoteIntvToA4(x) {
  if (x == "C") { return pyInterval(-6, 0.5) /* M6 down */ }
  if (x == "D") { return pyInterval(-5, 0)   /* P5 down */ }
  if (x == "E") { return pyInterval(-4, 0)   /* P4 down */ }
  if (x == "F") { return pyInterval(-3, 0.5) /* M3 down */ }
  if (x == "G") { return pyInterval(-2, 0.5) /* M2 down */ }
  if (x == "A") { return Interval(1)         /* P1 */      }
  if (x == "B") { return pyInterval(2, 0.5)  /* M2 */      }
}

function baseNoteIntvToReference(x,referenceNoteIntvToA4) {
  return baseNoteIntvToA4(x).div(referenceNoteIntvToA4);
}

function octaveOfIntvToA4(x) {
  const intvToC4 = x.div(baseNoteIntvToA4("C"));
  return 4 + Math.floor(Math.log(intvToC4.valueOf()) / Math.log(2));
}

module['exports'].perfPyInterval = perfPyInterval;
module['exports'].nonPerfPyInterval = nonPerfPyInterval;
module['exports'].augOrDimPyInterval = augOrDimPyInterval;
module['exports'].ensureNo2Or3 = ensureNo2Or3;
module['exports'].cbnEDOs = cbnEDOs;
module['exports'].baseNoteIntvToReference = baseNoteIntvToReference;
module['exports'].octaveOfIntvToA4 = octaveOfIntvToA4;

})(this);
