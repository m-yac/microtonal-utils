
const Fraction = require('fraction.js');
const Interval = require('../interval.js');
const {pyInterval, isPerfectDeg, baseNoteIntvToA} = require('../pythagorean.js');
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

function baseNoteIntvToReference(x,referenceNoteIntvToA4) {
  return baseNoteIntvToA(x).div(referenceNoteIntvToA4);
}

module['exports'].perfPyInterval = perfPyInterval;
module['exports'].nonPerfPyInterval = nonPerfPyInterval;
module['exports'].augOrDimPyInterval = augOrDimPyInterval;
module['exports'].ensureNo2Or3 = ensureNo2Or3;
module['exports'].cbnEDOs = cbnEDOs;
module['exports'].baseNoteIntvToReference = baseNoteIntvToReference;
