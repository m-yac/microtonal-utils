
const Fraction = require('fraction.js');
const Interval = require('../interval.js');
const {pyInterval, isPerfectDeg, baseNoteIntvToA} = require('../pythagorean.js');
const {fjsFactor} = require('../fjs.js');
const {edoPy, edoHasNeutrals, edoHasSemiNeutrals} = require('../edo.js');

class ParseError extends Error {
  constructor(message, loc) {
    super(message);
    this.name = "ParseError";
    this.offset = loc;
  }
  toError(str) {
    const errStr = "\n" + str + "\n" + " ".repeat(this.offset) + "^\n"
                   + "Parse error, " + this.message;
    return new Error(errStr);
  }
}

const defaultRefNote = { intvToA4: Interval(1), hertz: Interval(440) };

function perfPyInterval(d,o,loc) {
  if (isPerfectDeg(d)) { return pyInterval(d,o); }
  else { throw new ParseError("P" + d + " is not a valid interval ("
                                  + d + " is not a perfect scale degree)", loc); }
}
function nonPerfPyInterval(d,o,q,loc) {
  if (!isPerfectDeg(d)) { return pyInterval(d,o); }
  else { throw new ParseError(q + d + " is not a valid interval ("
                                + d + " is a perfect scale degree)", loc); }
}
function augOrDimPyInterval(d,a,b,reject) {
  const o = Fraction(a,b);
  if (o.d != b) {
    return reject;
  }
  const o_np = o.add(o.s,2);
  return isPerfectDeg(d) ? pyInterval(d,o) : pyInterval(d,o_np);
}

function ensureNo2Or3(i,loc) {
  if ((!i['2'] || i['2'] == 0) && (!i['3'] || i['3'] == 0)) { return i; }
  else { throw new ParseError("FJS accidental cannot contain a factor or 2 or 3", loc); }
}

function cbnEDOs(a,b) {
  if (a && b) { return Fraction(1,a).gcd(1,b).d; }
  else { return null; }
}

function baseNoteIntvToReference(x,referenceNoteIntvToA4) {
  return baseNoteIntvToA(x).div(referenceNoteIntvToA4);
}

module['exports'].ParseError = ParseError;
module['exports'].defaultRefNote = defaultRefNote;
module['exports'].perfPyInterval = perfPyInterval;
module['exports'].nonPerfPyInterval = nonPerfPyInterval;
module['exports'].augOrDimPyInterval = augOrDimPyInterval;
module['exports'].ensureNo2Or3 = ensureNo2Or3;
module['exports'].cbnEDOs = cbnEDOs;
module['exports'].baseNoteIntvToReference = baseNoteIntvToReference;
