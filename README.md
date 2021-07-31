# microtonal-utils

[![Run on repl.it](https://repl.it/badge/github/m-yac/microtonal-utils)](https://repl.it/@m_yac/microtonal-utils)

A javascript library for doing precise calculations involving microtonal intervals and notes, inspired by [`fraction.js`](https://github.com/infusion/Fraction.js/).

Interact with this code live at: [yacavone.net/microtonal-utils](https://www.yacavone.net/microtonal-utils).

The main datatype of this library is `Interval`, located in `lib/interval.js`. Internally, intervals are stored as prime factorizations with fractional exponents, which allows for both justly tuned intervals (e.g. `5/4` which can be written as `2^(-2) * 5^1`) and intervals in equal temperaments (e.g. `4\12` which can be written as `2^(4/12)`) to be represented by the same datatype.

Like [`fraction.js`](https://github.com/infusion/Fraction.js/), you can perform calculations using `Intervals` without losing any precision. For example, notice that the representations of `5/4 * 4\12 / 1\12` and `5/4 * 3\12` are identical:
```javascript
> Interval(5,4).mul(Interval(2).pow(4,12)).div(Interval(2).pow(1,12))
{ _fact: { '2': { s: -1n, n: 7n, d: 4n }, '5': { s: 1n, n: 1n, d: 1n } } }
> Interval(5,4).mul(Interval(2).pow(3,12))
{ _fact: { '2': { s: -1n, n: 7n, d: 4n }, '5': { s: 1n, n: 1n, d: 1n } } }
```
This is because both have the factorization `2^(-7/4) * 5^1`. Trying to do this same calculation with javascript's number type, we get imprecision:
```javascript
> 5/4 * Math.pow(2, 4/12) / Math.pow(2, 1/12)
1.4865088937534015
> 5/4 * Math.pow(2, 3/12)
1.4865088937534012
```
Another example of where using `Interval` is more precise, and more convenient, is when computing cents:
```javascript
> Interval(2).pow(4,12).toCents()
400
> Math.log(Math.pow(2, 4/12)) / Math.log(2) * 1200
400.00000000000006
```

See the JSDoc comments above the functions in `interval.js` for more examples.

This repository also contains:
- `pythagorean.js`: functions for constructing and getting properties of Pythagorean intervals, as well as functions for formatting Pythagorean interval and note symbols
- `edo.js`: functions for approximating intervals in equal temperaments, as well as functions for formatting EDO-step and ups-and-downs notation interval and note symbols
- `fjs.js`: functions related to the Functional Just System (FJS) and systems like it, in particular, functions for formatting FJS interval and note symbols
- `color.js`: functions for formatting color notation for intervals, notes, and temperaments
- `parser.js`: a parser for arbitrary expressions involving notes, intervals, and the all note/interval symbols mentioned above (used in [`xen-calc`](https://github.com/m-yac/xen-calc)) as well as inverses of all the formatting functions mentioned above
- `approx.js`: functions for getting best rational and best EDO approximations of an interval
- `english.js`: an experiment with programmatically assigning English names to arbitrary intervals

## Building

To build this library, run:
```
$ npm install
```

If you've changed `grammar.ne`, run the following to update `grammar.js`:
```
$ npm run nearley
```

## Using

After building on your local machine, to use this library in an interactive REPL run:
```
$ npm run repl
```
You can also visit [yacavone.net/microtonal-utils](https://www.yacavone.net/microtonal-utils) or [repl.it/@m_yac/microtonal-utils](https://repl.it/@m_yac/microtonal-utils) instead to run the REPL directly in your web browser.

Everything exported in the `lib` directory is made accessible. Here's an
example of an interactive session:
```javascript
> Interval([-2,0,1]).toCents()
386.3137138648344
> colorTemperament(32805,32768)
'layo'
> updnsSymb(22, edoApprox(22, parsePySymb("M3")))
[ 'vM3' ]
> parseCvt("sqrt(3/2)")
{
  type: 'interval',
  cents: 350.97750043269366,
  intv: { _fact: { '2': [Object], '3': [Object] } },
  ref: { hertz: 440, intvToA4: { _fact: {} } },
  symb: { NFJS: 'n3' },
  english: [ 'Pythagorean neutral third' ]
}
```

To use this library as part of a website, run:
```
$ npm run build:all
```
A single-file ("browserified") version of the entire library can then be found
at `dist/microtonal-utils.js`. Copy that as well as `microtonal-utils.js.map`
into your project and simply include the former as you would any other
javascript file. All the functions of the library are included under
`microtonal_utils` (e.g. `microtonal_utils.parseCvt`). A minified version of
this file can also be found at `dist/microtonal-utils.min.js`.


## Testing

This library includes a test suite of property-based tests, located in `/test`. The term "property-based" means that each test consists of some property (e.g. `Interval(2).pow(fr).toCents() == fr.mul(1200)`) which is then checked using many (usually 100) randomly generated values (e.g. a randomly generated value for `fr` would be `Fraction(99/38)`). Since each run of the test suite checks these properties with a totally new set of random values, the fact that the test suite consistently passes should give high confidence that these properties do hold in general. There are also a few regression tests, which are not property-based.

To run the test suite, use the command `npm run test` or `npm run test:all`. The latter also includes tests of the parser, which are often fairly slow. The output of `npm run test` should look like:
```
$ npm run test

  Interval constructors and conversions
    ✓ Interval(n).factors() is the prime factorization of n
    ✓ Interval(a/b).factors() is the prime factorization of a/b
    ✓ Interval(fr).toFrac() == fr
    ✓ Interval(monzo).toMonzo() == monzo
    ✓ Interval(2).pow(fr).toCents() == fr.mul(1200)

  Interval and Fraction operations
    ✓ mul: Interval(fr1).mul(fr2) == fr1.mul(fr2)
    ✓ div: Interval(fr1).div(fr2) == fr1.div(fr2)
    ✓ recip: Interval(fr).recip() == fr.inverse()
    ✓ pow: Interval(fr).pow(n) == fr.pow(n)
    ✓ equals: Interval(fr1).equals(f2) iff fr1.equals(fr2)
    ✓ compare: Interval(fr1).compare(f2) iff fr1.compare(fr2)
    ✓ valueOf: Interval(fr).valueOf() ~= fr.valueOf()

  Other Interval operations
    ✓ pow/mul: i.pow(n) == i.mul(i).mul(i) ... .mul(i)
    ✓ pow/div: i.pow(-n) == i.div(i).div(i) ... .div(i)
    ✓ pow: i.pow(fr).pow(fr.inverse()) == i
    ✓ root/toNthRoot: Interval(fr).root(n).toNthRoot() == {k: fr, n: n}
    ✓ root/valueOf: Interval(fr).root(n).valueOf() ~= Math.pow(fr, 1/n)
    ✓ factorOut: i1 == i2.pow(i1.factorOut(i2)[0]).mul(i1.factorOut(i2)[1])
    ✓ pow/valueOf_log: i.pow(fr).valueOf_log(i) == fr.valueOf()
    ✓ valueOf_log: i1.valueOf_log(i2) ~= Math.log(i1) / Math.log(i2)
    ✓ toCents: i.toCents() ~= 1200 * Math.log2(i)
    ✓ isPrimeLimit: i.inPrimeLimit(k) for all k >= i.primeLimit()
    ✓ isPrimeLimit: !i.inPrimeLimit(k) for all k < i.primeLimit()
    ✓ isOddLimit: Interval(fr).inOddLimit(k) for all k >= Interval(fr).oddLimit()
    ✓ isOddLimit: !Interval(fr).inOddLimit(k) for k < Interval(fr).oddLimit()
    ✓ oddLimit: Interval(odd,even).oddLimit() == Interval(even,odd).oddLimit() == odd

  Best approximations
    ✓ regression: bestRationalApproxsByNo2sHeight({2: 400/1200})
    ✓ regression: bestRationalApproxsByNo2sHeight({2: 400/1200}, {primeLimit: 13})
    ✓ regression: bestRationalApproxsByNo2sHeight({2: 300/1200}, {numIterations: 3})
    ✓ regression: bestRationalApproxsByNo2sHeight({2: 300/1200}, {primeLimit: 13, numIterations: 2})
    ✓ regression: bestRationalApproxsByNo2sHeight({2: 600/1200})
    ✓ regression: bestRationalApproxsByNo2sHeight({2: 600/1200}, {primeLimit: 13, oddLimit: 81})
    ✓ regression: bestRationalApproxsByNo2sHeight(81,64, {primeLimit: 13, oddLimit: 81})
    ✓ regression: bestRationalApproxsByHeight({2: 400/1200}, {primeLimit: 19})
    ✓ regression: bestRationalApproxsByHeight({2: 300/1200}, {primeLimit: 13})
    ✓ regression: bestRationalApproxsByHeight({2: 600/1200}, {primeLimit: 13, oddLimit: 81})
    ✓ regression: bestRationalApproxsByHeight(81,64, {primeLimit: 19})
    ✓ regression: bestRationalApproxsByDiff({2: 350/1200}, {oddLimit: 9})
    ✓ regression: bestEDOApproxsByEDO(5,4)
    ✓ regression: bestEDOApproxsByEDO({2: 100/1200})
    ✓ regression: bestEDOApproxsByDiff(5,4)

  Pythagorean intervals
    ✓ pyDegree(pyInterval(d,o/4)) == d (if d != -1)
    ✓ pyOffset(pyInterval(d,o/4)) == o/4 (if d != -1)
    ✓ pyInterval(d,o/4).recip() == pyInterval(-d,o/4)
    ✓ pyZDegree(pyi1.mul(pyi2)) == pyZDegree(pyi1) + pyZDegree(pyi2)
    ✓ pyInterval(±d,o) == pyInterval(±d,0).mul(pyA1.pow(±o))

  Color notation
    ✓ colorSymb(63,40) == zg6
    ✓ colorSymb(63,40,{verbosity:1}) == zogu 6th
    ✓ colorFromSymb(0, -1, [0,0,-2], 2) == 2048/2025
    ✓ colorFromSymb(0, 0, 5, 1) == 80/81
    ✓ colorTemperament(135,128) == layobi
    ✓ colorTemperament([24,-21,4]) == sasa-quadyo

  52 passing (328ms)

```
