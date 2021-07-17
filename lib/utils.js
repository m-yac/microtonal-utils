
const Fraction = require('fraction.js');

// a version of mod where the result is always between 1 and n
function mod(a,n) {
  return ((a % n) + n) % n;
}

let cached_logs = {}
function cachedLog2(i) {
  let entry = cached_logs[i];
  if (entry == undefined) {
    const logi = Math.log2(i);
    entry = [logi, 1200*logi];
    cached_logs[i] = entry;
  }
  return entry;
}

function unBigFraction(fr) {
  return Fraction(Number(fr.s * fr.n), Number(fr.d));
}

function keys(a, b) {
  let ret = {};
  for (const [k,v] of Object.entries(a)) {
    ret[k] = 1;
  }
  if (b) {
    for (const [k,v] of Object.entries(b)) {
      ret[k] = 1;
    }
  }
  return ret;
}

module.exports.mod = mod;
module.exports.cachedLog2 = cachedLog2;
module.exports.unBigFraction = unBigFraction;
module.exports.keys = keys;
