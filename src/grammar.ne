@{%

const Fraction = require('fraction.js');
const Interval = require('./interval.js');
const {pyInterval} = require('./pythagorean.js');
const {fjsFactor} = require('./fjs.js');
const {edoPy, edoHasNeutrals, edoHasSemiNeutrals} = require('./upsdowns.js');

function isPerfectDeg(d) {
  const redDeg = (d-1) % 7 + 1;
  return redDeg == 1 || redDeg == 4 || redDeg == 5;
}

function perfPyInterval(d,o,reject) {
  return isPerfectDeg(d) ? pyInterval(d,o) : reject;
}
function nonPerfPyInterval(d,o,reject) {
  return isPerfectDeg(d) ? reject : pyInterval(d,o);
}
function augOrDimPyInterval(d,a,b,reject) {
  const o = Fraction(a,b);
  if (o.d != b) {
    return reject;
  }
  const o_np = o.add(o.s,2);
  return isPerfectDeg(d) ? pyInterval(d,o) : pyInterval(d,o_np);
}

function ensureNo2Or3(i,reject) {
  return (i['2'] && i['2'] != 0) || (i['3'] && i['3'] != 0) ? reject : i;
}

%}

@builtin "whitespace.ne"

top1 -> _ top2 _ {% d => d[1] %}
top2 ->
    fjsItv    {% d => ["FJS", d[0]] %}
  | itvExpr1  {% d => ["interval", d[0]] %}
  | ctsExpr1  {% d => ["cents", d[0]] %}

# ---------------
# FJS intervals

fjsItv ->
    pyItv               {% id %}
  | fjsItv "^" fjsAccs  {% d => d[0].mul(d[2]) %}
  | fjsItv "_" fjsAccs  {% d => d[0].div(d[2]) %}

fjsAccs ->
    fjsAcc              {% d => fjsFactor(d[0]) %}
  | fjsAccs "," fjsAcc  {% d => d[0].mul(fjsFactor(d[2])) %}

fjsAcc ->
    posInt                        {% (d,_,reject) => ensureNo2Or3(Interval(d[0]),reject) %}
  | "sqrt(" fjsAcc ")"            {% d => d[3].sqrt() %}
  | "root" posInt "(" fjsAcc ")"  {% d => d[3].root(d[1]) %}
  | "(" fjsAcc "^" frcExpr3 ")"   {% d => d[1].pow(d[3]) %}

# -----------------------
# Pythagorean intervals

pyItv ->
  # perfect intervals
    "P"  posInt {% (d,_,reject) => perfPyInterval(d[1],0,reject) %}
  # major and minor intervals
  | "M"  posInt {% (d,_,reject) => nonPerfPyInterval(d[1],Fraction(1,2),reject) %}
  | "m"  posInt {% (d,_,reject) => nonPerfPyInterval(d[1],Fraction(-1,2),reject) %}
  # augmented and diminished intervals
  | "A":+ posInt {% (d,_,reject) => augOrDimPyInterval(d[1],d[0].length,1,reject) %}
  | "d":+ posInt {% (d,_,reject) => augOrDimPyInterval(d[1],-d[0].length,1,reject) %}
  | posInt "A" posInt {% (d,_,reject) => augOrDimPyInterval(d[2],d[0],1,reject) %}
  | posInt "d" posInt {% (d,_,reject) => augOrDimPyInterval(d[2],d[0],1,reject) %}

npyItv ->
  # neutral intervals
    "n"i posInt {% (d,_,reject) => nonPerfPyInterval(d[1],0,reject) %}
  # semi-augmented and semi-diminished intervals
  | "sA" posInt {% (d,_,reject) => augOrDimPyInterval(d[1],1,2,reject) %}
  | "sd" posInt {% (d,_,reject) => augOrDimPyInterval(d[1],-1,2,reject) %}
  | posInt "/2-A" posInt {% (d,_,reject) => augOrDimPyInterval(d[2],d[0],2,reject) %}
  | posInt "/2-d" posInt {% (d,_,reject) => augOrDimPyInterval(d[2],d[0],2,reject) %}

snpyItv ->
  # semi-neutral intervals
    "sM" posInt {% (d,_,reject) => nonPerfPyInterval(d[1],Fraction(1,4),reject) %}
  | "sm" posInt {% (d,_,reject) => nonPerfPyInterval(d[1],Fraction(-1,4),reject) %}
  | posInt "/4-A" posInt {% (d,_,reject) => augOrDimPyInterval(d[2],d[0],4,reject) %}
  | posInt "/4-d" posInt {% (d,_,reject) => augOrDimPyInterval(d[2],d[0],4,reject) %}

# ----------------------
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
    fjsItv                                          {% id %}
  | itvExpr4                                        {% id %}
itvExpr4 ->
    posInt                                          {% d => Interval(d[0]) %}
  | "(" _ itvExpr1 _ ")"                            {% d => d[2] %}

# -------------------
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
  | fjsItv                                           {% id %}
  | decimal "c"
    {% d => Interval(2).pow(Fraction(d[0]).div(1200)) %}
  | edoExpr3 _ "\\" _ posInt
    {% (d,_,reject) => d[0](d[4]) == reject ? reject :
         Interval(2).pow(Fraction(d[0](d[4])).div(Fraction(d[4]))) %}
  | "(" _ ctsExpr1 _ ")"                             {% d => d[2] %}

# ----------------------
# EDO-step expressions

edoExpr1 ->
    edoExpr1 _ "+" _ edoExpr2  {% d => edo => d[0](edo) + d[4](edo) %}
  | edoExpr1 _ "-" _ edoExpr2  {% d => edo => d[0](edo) - d[4](edo) %}
  | edoExpr2                   {% id %}
edoExpr2 ->
    edoExpr3 _ "x" _ intExpr1  {% d => edo => d[0](edo) * d[4](edo) %}
  | intExpr1 _ "x" _ edoExpr3  {% d => edo => d[4](edo) * d[0](edo) %}
  | edoExpr3                   {% id %}
edoExpr3 ->
    posInt                     {% d => _ => parseInt(d[0]) %}
  | upsDns pyItv               {% d => edo => d[0] + edoPy(edo,d[1]) %}
  | upsDns npyItv
    {% (d,_,reject) => edo =>
         !edoHasNeutrals(edo) ? reject : d[0] + edoPy(edo,d[1]) %}
  | upsDns snpyItv
    {% (d,_,reject) => edo =>
         !edoHasSemiNeutrals(edo) ? reject : d[0] + edoPy(edo,d[1]) %}
  # alternate notation for neutal intervals, semi-augmented fourths, and
  # semi-diminished fifths
  | upsDns "~" posInt
    {% (d,_,reject) => edo =>
         !edoHasNeutrals(edo) || (d[2]-1)%7 == 0 ? reject :
           (d[2]-1)%7+1 == 4 ? d[0] + edoPy(edo,pyInterval(d[2],1,2)) :
           (d[2]-1)%7+1 == 5 ? d[0] + edoPy(edo,pyInterval(d[2],-1,2)) :
                               d[0] + edoPy(edo,pyInterval(d[2],0)) %}
  | "(" _ edoExpr1 _ ")"       {% d => d[2] %}

upsDns ->
    null   {% d => 0 %}
  | "^":+  {% d => d[0].length %}
  | "v":+  {% d => d[0].length %}

# -------------------------------------------------------
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

# ----------------------------------------------------
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

# -----------
# Terminals

posInt -> [1-9] [0-9]:* {% d => d[0] + d[1].join("") %}

decimal -> "-":? [0-9]:+ ("." [0-9]:* ("(" [0-9]:+ ")"):?):?
  {% d => (d[0] || "") + d[1].join("")
                       + (d[2] ? "." + d[2][1].join("")
                                     + (d[2][2] ? "("+d[2][2][1].join("")+")"
                                                : "")
                               : "") %}
