# microtonal-utils

A javascript library for doing calculations involving microtonal intervals and
notes.

The main datatype of this library is `Interval`, located in `lib/interval.js`.
Internally, intervals are stored as prime factorizations with fractional
exponents, which allows for both justly tuned intervals (e.g. `5/4` which can be
written as `2^(-2) * 5^1`) and intervals in equal temperaments (e.g. `4\\12`
which can be written as `2^(4/12)`) to be represented by the same datatype. See
the documentation in `lib/interval.js` for examples of using this datatype.
