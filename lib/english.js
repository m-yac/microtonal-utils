/**
 * English names for intervals based on the Neutral FJS and ups-and-downs
 * notations (very much incomplete!)
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module english
 **/

const pf = require('primes-and-factors');
const ntw = require('number-to-words');
const Fraction = require('fraction.js');
const Interval = require('./interval.js');
const {pyInterval, pySymb, pyGenerator} = require('./pythagorean.js');
const {fjsComma, fjsFifthShift, fjsAccidentals, nfjsSpec} = require('./fjs.js');
const {updnsSymbCache} = require('./edo.js');

const primeNames = { '5':  ["classic", "cls."]
                   , '7':  ["septimal", "sep."]
                   , '11': ["undecimal", "und."]
                   , '13': ["tridecimal", "trid."]
                   , '17': ["septendecimal", "sepd."]
                   , '19': ["undevicesimal", "undv."] };

/**
  * Attempts to give english names to the given interval based on the
  * Neutral FJS and ups-and-downs notations.
  *
  * @param {Interval} i
  * @param {{abbreviate: boolean, prefEDO: integer}=} opts
  * @returns {Array.<string>}
  */
function enNames(a,b, opts) {
  // if only two arguments are given, the second one may be `opts`!
  if (!opts && typeof b == 'object' && b != null) {
      opts = b;
      b = undefined;
  }
  const intv = Interval(a,b);
  const abbreviate = (opts || {}).abbreviate ? 1 : 0;
  const verbosity  = abbreviate ? 1 : 2;
  const prefEDO    = (opts || {}).prefEDO;

  let nms = [];

  // handle approximate intervals separately
  if (!intv.hasFactors()) {
    const k = Math.floor(intv.valueOf_log());
    if (intv.div(Interval(2).pow(k)).equals(Interval.phi)) {
      const pyi = pyInterval(6,0).mul(Interval(2).pow(k));
      nms.push("phi " + pySymb(pyi, {verbosity: verbosity}));
    }
    return nms;
  }

  // special case for the Pythagorean comma
  if (intv.equals(pyInterval(-2,-1.5))) {
    nms.push("Pythagorean comma");
  }
  // special cases for FJS formal commas
  for (const p in primeNames) {
    const comma = fjsComma(p);
    const inv_str = intv.compare(1) < 0 ? " (descending)" : "";
    if (intv.equals(comma) || intv.equals(comma.recip())) {
      nms.push((p == 5 ? "syntonic" : primeNames[p][0]) + " comma" + inv_str);
    }
  }
  // special case for multiple octaves
  if (intv.inPrimeLimit(2) && intv.expOf(2).d == 1 && intv.expOf(2).n > 1
      /* ^ is a non-zero integer power of 2 */) {
    const invStr = intv.expOf(2) < 0 ? " (descending)" : "";
    nms.push(ntw.toWords(intv.expOf(2).n) + " octaves" + invStr);
  }

  // Neutral FJS intervals
  const fjs = fjsAccidentals(a,b, nfjsSpec);
  if (fjs) {
    let pyi_symb = pySymb(fjs.pyi, {verbosity: verbosity});
    const resFact = intv.factors().filter(([p,_]) => p > 3);
    // FJS intervals with no accidentials and a factor of 3 are Pythagorean
    if (resFact.length == 0) {
      if (intv.hasExp(3)) {
        nms.push((abbreviate ? "py. " : "Pythagorean ") + pyi_symb);
      }
      else {
        nms.push(pyi_symb);
      }
    }
    // FJS intervals with a single prime (>3) factor might be able to be named
    else if (resFact.length == 1) {
      const [p,e] = resFact[0];
      // We don't consider cases where the prime doesn't have the name, the FJS
      //  accidental is not an integer, or the pythagorean interval is an
      //  octave
      if (primeNames[p] && e.d == 1 && pyGenerator(fjs.pyi) != 0) {
        const fifthShift = fjsFifthShift(p, nfjsSpec);
        const g = fjs.pyi.expOf(3);
        // Ensure otonality matches (e.g. let through "M3^5" but not "M3_5")
        //  and neutral-ness matches (e.g. let through "M3^5" but not "n3^5")
        if (e.s == fifthShift.s * g.s && g.d == fifthShift.d) {
          // Ensure multiplicity matches, i.e. n-aug/dim have (n+1) primes
          //  (e.g. let through M3^5 and A4^5,5 but not M3^5,5 or A4^5)
          let multiplicityMatches = false;
          // Well, for primes with non-neutral fifth shifts, we do exactly
          //  what's stated above...
          if (fifthShift.d == 1) {
            if (g.n == 6) {
              multiplicityMatches = (e.n == 2);
            }
            else {
              multiplicityMatches = (e.n == 2 + Math.floor((g.n - 6)/7))
            }
          }
          // ...but for primes with neutral fifth shifts, we just handle cases
          //  where the neutral interval is small, since it's not clear to me
          //  what to do in the general case
          if (fifthShift.d == 2) {
            multiplicityMatches = (g.n <= 11 && e.n == 1);
          }
          if (multiplicityMatches) {
            // make sure we don't have "perfect" in the name for a 4th or 5th
            if (Math.abs(pyGenerator(fjs.pyi)) == 4) {
              let typ = intv.compare(fjs.pyi) > 0 ? "super" : "sub";
              if (abbreviate) { pyi_symb = pyi_symb.replace("perfect", typ); }
              else {
                typ = p == 5 ? (typ == "super" ? "acute " : "grave ") : typ + "-";
                pyi_symb = pyi_symb.replace("perfect ", typ);
              }
            }
            nms.push(primeNames[p][abbreviate] + " " + pyi_symb.replace("perfect ", ""));
          }
        }
      }
    }
  }

  // ups-and-downs intervals
  else if (intv.inPrimeLimit(2)) {
    const e2 = intv.expOf(2);
    const edo = prefEDO ? prefEDO : e2.d;
    const edo_str = edo + "-EDO ";
    let intv_strs = [];
    if (e2.mul(edo).d == 1 && (prefEDO || edo <= 60)) {
      const n = e2.s * e2.mul(edo).n;
      const n_mod = ((n % edo) + edo) % edo;
      for (const [uds, pyi_red] of updnsSymbCache(edo)[n_mod]) {
        const pyi = pyi_red.mul(Interval(2).pow((n-n_mod)/edo));
        let uds_str = "";
        if      (uds ==  1) { uds_str = "up"; }
        else if (uds == -1) { uds_str = "down"; }
        else if (uds ==  2 && !abbreviate) { uds_str = "double-up "; }
        else if (uds == -2 && !abbreviate) { uds_str = "double-down "; }
        else if (uds >=  2) { uds_str = uds + "-up "; }
        else if (uds <= -2) { uds_str = uds + "-down "; }
        let pyi_symb = pySymb(pyi, {verbosity: verbosity});
        let neut_str = undefined;
        if (pyi.minPowFrac() == 2) {
          if (uds == 0) { neut_str = uds_str + pyi_symb; }
          pyi_symb = pyi_symb.replace("neutral", "mid")
                             .replace("semi-augmented", "mid")
                             .replace("semi-diminished", "mid");
        }
        if ((uds == 1 || uds == -1) && !abbreviate) {
          pyi_symb = pyi_symb.replace("perfect ", "-");
        }
        else if (uds != 0) {
          pyi_symb = pyi_symb.replace("perfect", "");
        }
        intv_strs.push(uds_str + pyi_symb);
        if (neut_str != undefined) { intv_strs.push(neut_str); }
      }
      nms.push(edo_str + intv_strs.join(" / "));
      if (n == edo / 2) {
        nms.push(edo_str + "tritone");
      }
    }
  }

  // special case for the inverse of the Pythagorean comma
  if (intv.equals(pyInterval(2,-1.5))) {
    nms.push("Pythagorean comma (descending)");
  }
  // special case for harmonics > 1
  if (intv.isFrac()) {
    const {n,d} = intv.toFrac();
    if (d == 1 && n > 1) {
      nms.push(ntw.toOrdinal(n) + " harmonic");
    }
    if (n == 1 && d > 1) {
      nms.push(ntw.toOrdinal(d) + " harmonic (descending)");
    }
  }

  return nms;
}

module.exports.enNames = enNames;
