# microtonal-utils

A javascript library for doing calculations involving microtonal intervals and
notes.

The main datatype of this library is `Interval`, located in `src/interval.js`.
Internally, intervals are stored as prime factorizations with fractional
exponents, which allows for both justly tuned intervals (e.g. `5/4` which can be
written as `2^(-2) * 5`) and intervals in equal temperments (e.g. `4\\12` which
can be written as `2^(4/12)`) to be represented by the same datatype.
