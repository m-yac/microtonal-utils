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
