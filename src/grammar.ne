@{%

const Fraction = require('fraction.js');
const Interval = require('./interval.js');
const py = require('./pythagorean.js');

function isPerfectDeg(d) {
  const redDeg = (d-1) % 7 + 1;
  return redDeg == 1 || redDeg == 4 || redDeg == 5;
}

function perfPyInterval(d,o,reject) {
  return isPerfectDeg(d) ? py.pyInterval(d,o) : reject;
}
function nonPerfPyInterval(d,o,reject) {
  return isPerfectDeg(d) ? reject : py.pyInterval(d,o);
}
function augOrDimPyInterval(d,a,b,reject) {
  const o = Fraction(a,b);
  if (o.d != b) {
    return reject;
  }
  const o_np = o.add(o.s,2);
  return isPerfectDeg(d) ? py.pyInterval(d,o) : py.pyInterval(d,o_np);
}

%}

@builtin "whitespace.ne"

top ->
    _ itvExpr1 _ {% d => ["interval", d[1]] %}
  | _ ctsExpr1 _ {% d => ["cents", d[1]] %}

# Pythagorean intervals

pyItv ->
  # perfect intervals
    "P"  posInt {% (d,_,reject) => perfPyInterval(d[1],0,reject) %}
  # neutral, major, and minor intervals
  | "n"i posInt {% (d,_,reject) => nonPerfPyInterval(d[1],0,reject) %}
  | "sM" posInt {% (d,_,reject) => nonPerfPyInterval(d[1],Fraction(1,4),reject) %}
  | "M"  posInt {% (d,_,reject) => nonPerfPyInterval(d[1],Fraction(1,2),reject) %}
  | "sm" posInt {% (d,_,reject) => nonPerfPyInterval(d[1],Fraction(-1,4),reject) %}
  | "m"  posInt {% (d,_,reject) => nonPerfPyInterval(d[1],Fraction(-1,2),reject) %}
  # augmented and diminished intervals
  | "sA" posInt {% (d,_,reject) => augOrDimPyInterval(d[1],1,2,reject) %}
  | "sd" posInt {% (d,_,reject) => augOrDimPyInterval(d[1],-1,2,reject) %}
  | "A":+ posInt {% (d,_,reject) => augOrDimPyInterval(d[1],d[0].length,1,reject) %}
  | "d":+ posInt {% (d,_,reject) => augOrDimPyInterval(d[1],-d[0].length,1,reject) %}
  | posInt "A" posInt {% (d,_,reject) => augOrDimPyInterval(d[2],d[0],1,reject) %}
  | posInt "d" posInt {% (d,_,reject) => augOrDimPyInterval(d[2],d[0],1,reject) %}
  | posInt "/2-A" posInt {% (d,_,reject) => augOrDimPyInterval(d[2],d[0],2,reject) %}
  | posInt "/2-d" posInt {% (d,_,reject) => augOrDimPyInterval(d[2],d[0],2,reject) %}
  | posInt "/4-A" posInt {% (d,_,reject) => augOrDimPyInterval(d[2],d[0],4,reject) %}
  | posInt "/4-d" posInt {% (d,_,reject) => augOrDimPyInterval(d[2],d[0],4,reject) %}

# Interval expressions

itvExpr1 ->
    itvExpr1 _ "*" _ itvExpr2                       {% d => d[0].mul(d[4]) %}
  | itvExpr1 _ "/" _ itvExpr2                       {% d => d[0].div(d[4]) %}
  | itvExpr2                                        {% id %}
itvExpr2 ->
    itvExpr4 _ "^" _ frcExpr3                       {% d => d[0].pow(d[4]) %}
  | "sqrt" _ "(" _ itvExpr1 _ ")"                   {% d => d[4].sqrt() %}
  | "red"  _ "(" _ itvExpr1 _ ")"                   {% d => d[4].red() %}
  | "red"  _ "(" _ itvExpr1 _ "," _ itvExpr1 _ ")"  {% d => d[4].red(d[8]) %}
  | "reb"  _ "(" _ itvExpr1 _ ")"                   {% d => d[4].reb() %}
  | "reb"  _ "(" _ itvExpr1 _ "," _ itvExpr1 _ ")"  {% d => d[4].reb(d[8]) %}
  | itvExpr3                                        {% id %}
itvExpr3 ->
    pyItv                                           {% id %}
  | itvExpr4                                        {% id %}
itvExpr4 ->
    posInt                                          {% d => Interval(d[0]) %}
  | "(" _ itvExpr1 _ ")"                            {% d => d[2] %}

# Cents expressions

ctsExpr1 ->
    ctsExpr1 _ "+" _ ctsExpr2                        {% d => d[0].mul(d[4]) %}
  | ctsExpr1 _ "-" _ ctsExpr2                        {% d => d[0].div(d[4]) %}
  | ctsExpr2                                         {% id %}
ctsExpr2 ->
    ctsExpr3 _ "x" _ frcExpr3                        {% d => d[0].pow(d[4]) %}
  | frcExpr3 _ "x" _ ctsExpr3                        {% d => d[4].pow(d[0]) %}
  | ctsExpr3                                         {% id %}
ctsExpr3 ->
    "cents" _ "(" _ itvExpr1 _ ")"                   {% d => d[4] %}
  | "red"   _ "(" _ ctsExpr1 _ ")"                   {% d => d[4].red() %}
  | "red"   _ "(" _ ctsExpr1 _ "," _ itvExpr1 _ ")"  {% d => d[4].red(d[8]) %}
  | "reb"   _ "(" _ ctsExpr1 _ ")"                   {% d => d[4].reb() %}
  | "reb"   _ "(" _ ctsExpr1 _ "," _ itvExpr1 _ ")"  {% d => d[4].reb(d[8]) %}
  | pyItv                                            {% id %}
  | decimal "c"
    {% d => Interval(2).pow(Fraction(d[0]).div(1200)) %}
  | intExpr3 _ "\\" _ posInt
    {% d => Interval(2).pow(Fraction(d[0]).div(Fraction(d[4]))) %}
  | "(" _ ctsExpr1 _ ")"                             {% d => d[2] %}

# Fractional expressions (positive, negative, or zero!)

frcExpr1 ->
    frcExpr1 _ "+" _ frcExpr2  {% d => d[0].add(d[4]) %}
  | frcExpr1 _ "-" _ frcExpr2  {% d => d[0].sub(d[4]) %}
  | frcExpr2                   {% id %}
frcExpr2 ->
    frcExpr2 _ "*" _ frcExpr3  {% d => d[0].mul(d[4]) %}
  | frcExpr2 _ "/" _ frcExpr3  {% d => d[0].div(d[4])%}
  | frcExpr3                   {% id %}
frcExpr3 ->
    "-" _ frcExpr4             {% d => d[2].neg() %}
  | frcExpr4                   {% id %}
frcExpr4 ->
    frcExpr5 _ "^" _ intExpr3  {% d => d[0].pow(d[4]) %}
  | frcExpr5                   {% id %}
frcExpr5 ->
    posInt                     {% d => Fraction(d[0]) %}
  | "(" _ frcExpr1 _ ")"       {% d => d[2] %}

# Integer expressions (positive, negative, or zero!)

intExpr1 ->
    intExpr1 _ "+" _ intExpr2  {% d => d[0] + d[4] %}
  | intExpr1 _ "-" _ intExpr2  {% d => d[0] - d[4] %}
  | intExpr2                   {% id %}
intExpr2 ->
    intExpr2 _ "*" _ intExpr3  {% d => d[0] * d[4] %}
  | intExpr3                   {% id %}
intExpr3 ->
    "-" _ intExpr4             {% d => - d[2] %}
  | intExpr4                   {% id %}
intExpr4 ->
    intExpr5 _ "^" _ posInt    {% d => Math.pow(d[0],d[4]) %}
  | intExpr5                   {% id %}
intExpr5 ->
    posInt                     {% d => parseInt(d[0]) %}
  | "(" _ intExpr1 _ ")"       {% d => d[2] %}

# Terminals

posInt -> [1-9] [0-9]:* {% d => d[0] + d[1].join("") %}

decimal -> "-":? [0-9]:+ ("." [0-9]:* ("(" [0-9]:+ ")"):?):?
  {% d => (d[0] || "") + d[1].join("")
                       + (d[2] ? "." + d[2][1].join("")
                                     + (d[2][2] ? "("+d[2][2][1].join("")+")"
                                                : "")
                               : "") %}
