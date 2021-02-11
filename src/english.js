/**
 * English names for intervals based on the FJS + Neutrals and ups-and-downs
 * notations (very much incomplete!)
 * @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
 * @module english
 **/

const pf = require('primes-and-factors');
const Fraction = require('fraction.js');
const Interval = require('./interval.js');
const {pySymb, generator} = require('./pythagorean.js');
const {fjsFifthShift, fjsAccidentals, fjsnParams} = require('./fjs.js');
const {updnsSymbCache} = require('./edo.js');

const primeNames = { '5':  ["pental", "ptl."]
                   , '7':  ["septimal", "sep."]
                   , '11': ["undecimal", "und."]
                   , '13': ["tridecimal", "trid."]
                   , '17': ["septendecimal", "sepd."]
                   , '19': ["undevicesimal", "undv."] };

/**
  * Attempts to give english names to the given interval based on the
  * FJS + Neutrals and ups-and-downs notations.
  *
  * @param {Interval} i
  * @param {{abbreviate: boolean, prefEDO: }=} opts
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

  // random special cases
  if (intv.equals(Interval(2).sqrt())) {
    nms.push("tritone");
  }

  // FJS + Neutrals intervals
  const fjs = fjsAccidentals(a,b, fjsnParams);
  if (fjs) {
    let pyi_symb = pySymb(fjs.pyi, {verbosity: verbosity});
    const resFact = Object.entries(intv).filter(pe => pe[0] > 3);
    // FJS intervals with no accidentials and a factor of 3 are Pythagorean
    if (resFact.length == 0) {
      if (intv['3'] && (intv['3'].d != 1 || intv['3'].n > 1)) {
        nms.push((abbreviate ? "py. " : "pythagorean ") + pyi_symb);
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
      if (primeNames[p] && e.d == 1 && generator(fjs.pyi) != 0) {
        const fifthShift = fjsFifthShift(p, fjsnParams);
        const g = fjs.pyi['3'] || Fraction(0);
        // Ensure otonality matches (e.g. let through "M3^5" but not "M3_5")
        if (e.s == fifthShift.s * g.s) {
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
          // ...but for primes with neutral fifth shifts, we just handle the
          //  non-aug/dim case, since it's not clear to me what to do elsewhere
          if (fifthShift.d == 2) {
            multiplicityMatches = (g.n < 6 && e.n == 1);
          }
          if (multiplicityMatches) {
            // make sure we don't have "perfect" in the name for a 4th or 5th
            if (Math.abs(generator(fjs.pyi)) == 4) {
              const typ = intv.compare(fjs.pyi) > 0 ? "super" : "sub";
              if (abbreviate) { pyi_symb = pyi_symb.replace("perfect", typ); }
              else { pyi_symb = pyi_symb.replace("perfect ", typ + "-"); }
            }
            nms.push(primeNames[p][abbreviate] + " " + pyi_symb.replace("perfect ", ""));
          }
        }
      }
    }
  }

  // ups-and-downs intervals
  else if (Object.entries(intv).length == (intv['2'] != null)) {
    const e2 = intv['2'] || Fraction(0);
    const edo = prefEDO ? prefEDO : e2.d;
    const edo_str = edo + "-EDO ";
    let intv_strs = [];
    if (e2.mul(edo).d == 1 && (prefEDO || edo <= 60)) {
      const n = e2.s * e2.mul(edo).n;
      const n_mod = ((n % edo) + edo) % edo;
      for (const [uds, pyi] of updnsSymbCache(edo)[n_mod]) {
        let uds_str = "";
        if      (uds ==  1) { uds_str = "up"; }
        else if (uds == -1) { uds_str = "down"; }
        else if (uds ==  2 && !abbreviate) { uds_str = "double-up "; }
        else if (uds == -2 && !abbreviate) { uds_str = "double-down "; }
        else if (uds >=  2) { uds_str = uds + "-up "; }
        else if (uds <= -2) { uds_str = uds + "-down "; }
        let pyi_symb = pySymb(pyi, {verbosity: verbosity});
        if (abbreviate) { pyi_symb = pyi_symb.replace("perfect", ""); }
        else { pyi_symb = pyi_symb.replace("perfect ", "-"); }
        intv_strs.push(uds_str + pyi_symb);
      }
      nms.push(edo_str + intv_strs.join(" / "));
    }
  }

  return nms;
}

module.exports.enNames = enNames;
