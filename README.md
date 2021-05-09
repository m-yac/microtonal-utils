# microtonal-utils

A javascript library for doing precise calculations involving microtonal intervals and notes, inspired by [`fraction.js`](https://github.com/infusion/Fraction.js/).

The main datatype of this library is `Interval`, located in `lib/interval.js`. Internally, intervals are stored as prime factorizations with fractional exponents, which allows for both justly tuned intervals (e.g. `5/4` which can be written as `2^(-2) * 5^1`) and intervals in equal temperaments (e.g. `4\12` which can be written as `2^(4/12)`) to be represented by the same datatype.

Like [`fraction.js`](https://github.com/infusion/Fraction.js/), you can perform calculations using `Intervals` without losing any precision. For example, notice that the representations of `5/4 * 4\12 / 1\12` and `5/4 * 3\12` are identical:
```javascript
> Interval(5,4).mul(Interval(2).pow(4,12)).div(Interval(2).pow(1,12))
{ '2': { s: -1n, n: 7n, d: 4n }, '5': { s: 1n, n: 1n, d: 1n } }
> Interval(5,4).mul(Interval(2).pow(3,12))
{ '2': { s: -1n, n: 7n, d: 4n }, '5': { s: 1n, n: 1n, d: 1n } }
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

See the JSDoc comments above the functions in `fraction.js` for more examples.

This repository also contains:
- `pythagorean.js`: functions for constructing and getting properties of Pythagorean intervals, as well as functions for formatting Pythagorean interval and note symbols
- `edo.js`: functions for approximating intervals in equal temperaments, as well as functions for formatting EDO-step and ups-and-downs notation interval and note symbols
- `fjs.js`: functions related to the Functional Just System (FJS) and systems like it, in particular, containing functions for formatting FJS interval and note symbols
- `parser.js`: a parser for arbitrary expressions involving notes, intervals, and the note/interval symbols mentioned above - used in [`xen-calc`](https://github.com/m-yac/xen-calc)
- `approx.js`: functions for getting best rational and best EDO approximations of an interval
- `english.js`: an experiment with programmatically assigning English names to arbitrary intervals

## Testing

This library includes an in-progress test suite of property-based tests, located in `/test`. The term "property-based" means that each test consists of some property (e.g. `Interval(2).pow(fr).toCents() == fr.mul(1200)`) which is then checked using many (usually 100) randomly generated values (e.g. random values for `fr`, such as `Fraction(99/38)`). Since each run of the test suite checks these properties with a totally new set of random values, the fact that the test suite consistently passes should give high confidence that these properties do hold in general.

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

  Pythagorean intervals
    ✓ pyDegree(pyInterval(d,o/4)) == d (if d != -1)
    ✓ pyOffset(pyInterval(d,o/4)) == o/4 (if d != -1)
    ✓ pyInterval(d,o/4).recip() == pyInterval(-d,o/4)
    ✓ pyZDegree(pyi1.mul(pyi2)) == pyZDegree(pyi1) + pyZDegree(pyi2)
    ✓ pyInterval(±d,o) == pyInterval(±d,0).mul(pyA1.pow(±o))


  23 passing (210ms)

```
