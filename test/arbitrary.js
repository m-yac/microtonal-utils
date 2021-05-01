
const jsc = require("jsverify");
const Fraction = require('fraction.js');
const Interval = require('../lib/interval.js');
const {pyInterval, pyDegree, pyOffset} = require('../lib/pythagorean.js');

BigInt.prototype.toJSON = function() { return this.toString(); };

const int      = jsc.oneof(jsc.integer(-65535, 65535), jsc.integer(-63,63));
const posInt   = jsc.oneof(jsc.integer(0, 65535), jsc.integer(0,63));
const nzPosInt = jsc.suchthat(posInt, n => n != 0);

// An aribitrary Fraction is made up of an arbitrary integer and an arbitrary
// non-zero and positive integer
const frac = jsc.pair(int, nzPosInt).smap(Fraction, fr => [fr.s*fr.n, fr.d]);

// An arbitrary non-zero and positive Fraction is made up of two arbitrary
// non-zero and positive integers
const nzPosFrac = jsc.pair(nzPosInt, nzPosInt).smap(Fraction, fr => [fr.s*fr.n, fr.d]);

// An arbitrary monzo is an arbitrary array of integers or fractions which is
// on average half empty (to at least somewhat mirror commonly used monzos)
const intMonzo = jsc.array(jsc.oneof(int, jsc.constant(0)));
const fracMonzo = jsc.array(jsc.oneof(frac, jsc.constant(Fraction(0))));
const monzo = jsc.oneof(intMonzo, fracMonzo);

// An arbitrary Interval is either made up of an arbitrary positive Fraction,
// an arbitrary integer monzo, or an arbitrary fractional monzo
const intvFromFrac = nzPosFrac.smap(Interval, i => i.toFrac());
const intvFromIntMonzo = intMonzo.smap(Interval, i => i.toMonzo());
const intvFromFracMonzo = fracMonzo.smap(Interval, i => i.toMonzo());
const intv = jsc.oneof(jsc.oneof(intvFromFrac, intvFromIntMonzo), intvFromFracMonzo);

// An arbitrary Interval from an nth root is made up of an arbitrary positive
// Fraction and an arbitrary non-zero and positive integer
const intvFromNthRoot = jsc.pair(nzPosFrac, jsc.integer(1,15))
                             .smap(([k,n]) => Interval(k).root(n),
                                   i => Object.values(i.toNthRoot()));

// An arbitrary Pythagorean interval is made up of an arbitrary degree (an
// arbitrary small and non-zero integer) and an arbitrary offset (an arbitrary
// small integer)
const pyInt = jsc.oneof(jsc.integer(-64,63), jsc.integer(-16,15));
const degree = jsc.suchthat(pyInt, d => d != 0);
const offset = pyInt;
const pyIntv = jsc.pair(degree, offset).smap(([d,o]) => pyInterval(d,o/4),
                                             pyi => [pyDegree(pyi), pyOffset(pyi)]);

module.exports.int               = int;
module.exports.posInt            = posInt;
module.exports.nzPosInt          = nzPosInt;
module.exports.frac              = frac;
module.exports.nzPosFrac         = nzPosFrac;
module.exports.intMonzo          = intMonzo;
module.exports.fracMonzo         = fracMonzo;
module.exports.monzo             = monzo;
module.exports.intvFromFrac      = intvFromFrac;
module.exports.intvFromIntMonzo  = intvFromIntMonzo;
module.exports.intvFromFracMonzo = intvFromFracMonzo;
module.exports.intv              = intv;
module.exports.intvFromNthRoot   = intvFromNthRoot;
module.exports.pyInt = pyInt;
module.exports.degree = degree;
module.exports.offset = offset;
module.exports.pyIntv = pyIntv;
