
const pf = require('primes-and-factors');
const Fraction = require('fraction.js');

// a version of mod where the result is always between 1 and n
function mod(a,n) {
  return ((a % n) + n) % n;
}

let cached_logs = {}
function cachedLog2(i) {
  let entry = cached_logs[i];
  if (entry == undefined) {
    entry = Math.log2(i);
    cached_logs[i] = entry;
  }
  return entry;
}

function unBigFraction(fr) {
  return Fraction(Number(fr.s * fr.n), Number(fr.d));
}

function maxKey(a) {
  let max = -Infinity;
  for (const i in a) {
    max = Math.max(i, max);
  }
  return max;
}

function* keys(a, b) {
  for (const i in a) {
    yield i;
  }
  for (const i in b) {
    if (a[i] == undefined) {
      yield i;
    }
  }
}

function* primes() {
  yield* [2,3,5,7,11,13,17,19,23];
  for (let i = 29; true; i += 6) {
    if (pf.isPrime(i)) { yield i; }
    if (pf.isPrime(i + 2)) { yield (i + 2); }
  }
}

module.exports.mod = mod;
module.exports.cachedLog2 = cachedLog2;
module.exports.unBigFraction = unBigFraction;
module.exports.maxKey = maxKey;
module.exports.keys = keys;
module.exports.primes = primes;
