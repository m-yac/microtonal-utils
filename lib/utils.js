
const pf = require('primes-and-factors');
const Fraction = require('fraction.js');

// a version of Math.sign which returns 1 for the input 0
function sign1(x) {
  return x == 0 ? 1 : Math.sign(x);
}

// a version of % where the result is always between 1 and n
function mod(a,n) {
  return ((a % n) + n) % n;
}

// get the fractional part (between -0.5 and 0.5) of a number
function fractionalPart(n) {
  const nSplit = (n+"").split(".");
  const decimalPlaces = nSplit.length > 1 ? nSplit[1].length : 0;
  return (n - Math.round(n)).toFixed(decimalPlaces);
}

let cached_logs = {}
function cachedLog2(i) {
  if (i % 1 != 0) {
    return Math.log2(i)
  }
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

module.exports.sign1 = sign1;
module.exports.mod = mod;
module.exports.fractionalPart = fractionalPart;
module.exports.cachedLog2 = cachedLog2;
module.exports.unBigFraction = unBigFraction;
module.exports.maxKey = maxKey;
module.exports.keys = keys;
module.exports.primes = primes;
