@{%

const Fraction = require('fraction.js');
const Interval = require('./interval.js');
const {pyInterval, redDeg} = require('./pythagorean.js');
const {fjsFactor} = require('./fjs.js');
const {edoPy, edoHasNeutrals, edoHasSemiNeutrals} = require('./edo.js');
const helpers = require('./grammar-helpers.js');

%}

@builtin "whitespace.ne"

top1 -> _ top2 _ {% d => d[1] %}
top2 ->
    symbExpr1  {% d => ["symb", d[0]] %}
  | itvExpr1   {% d => ["interval", d[0]] %}
  | ctsExpr1   {% d => ["cents", d[0]] %}

# ------------------
# Interval symbols

symbExpr1 ->
    "red"  _ "(" _ symbExpr1 _ ")"                   {% d => d[4].red() %}
  | "reb"  _ "(" _ symbExpr1 _ ")"                   {% d => d[4].reb() %}
  | "red"  _ "(" _ symbExpr1 _ "," _ itvExpr1 _ ")"  {% d => d[4].red(d[8]) %}
  | "reb"  _ "(" _ symbExpr1 _ "," _ itvExpr1 _ ")"  {% d => d[4].reb(d[8]) %}
  | symbExpr2                                        {% id %}
symbExpr2 ->
    symb                                             {% id %}
  | "(" _ symbExpr1 _ ")"                            {% d => d[2] %}

symb ->
    fjsItv   {% id %}
  | npyItv   {% id %}
  | snpyItv  {% id %}
  | "TT"     {% _ => Interval(2).sqrt() %}

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
    posInt                        {% (d,_,reject) => helpers.ensureNo2Or3(Interval(d[0]),reject) %}
  | "sqrt(" fjsAcc ")"            {% d => d[1].sqrt() %}
  | "root" posInt "(" fjsAcc ")"  {% d => d[3].root(d[1]) %}
  | "(" fjsAcc "^" frcExpr3 ")"   {% d => d[1].pow(d[3]) %}

# -----------------------
# Pythagorean intervals

pyItv ->
  # perfect intervals
    "P"  pyDeg {% (d,_,reject) => helpers.perfPyInterval(d[1],0,reject) %}
  # major and minor intervals
  | "M"  pyDeg {% (d,_,reject) => helpers.nonPerfPyInterval(d[1],Fraction(1,2),reject) %}
  | "m"  pyDeg {% (d,_,reject) => helpers.nonPerfPyInterval(d[1],Fraction(-1,2),reject) %}
  # augmented and diminished intervals
  | "A":+ pyDeg {% (d,_,reject) => helpers.augOrDimPyInterval(d[1],d[0].length,1,reject) %}
  | "d":+ pyDeg {% (d,_,reject) => helpers.augOrDimPyInterval(d[1],-d[0].length,1,reject) %}
  | posInt "A" pyDeg {% (d,_,reject) => helpers.augOrDimPyInterval(d[2],d[0],1,reject) %}
  | posInt "d" pyDeg {% (d,_,reject) => helpers.augOrDimPyInterval(d[2],d[0],1,reject) %}

npyItv ->
  # neutral intervals
    "n"i pyDeg {% (d,_,reject) => helpers.nonPerfPyInterval(d[1],0,reject) %}
  # semi-augmented and semi-diminished intervals
  | "sA" pyDeg {% (d,_,reject) => helpers.augOrDimPyInterval(d[1],1,2,reject) %}
  | "sd" pyDeg {% (d,_,reject) => helpers.augOrDimPyInterval(d[1],-1,2,reject) %}
  | posInt "/2-A" pyDeg {% (d,_,reject) => helpers.augOrDimPyInterval(d[2],d[0],2,reject) %}
  | posInt "/2-d" pyDeg {% (d,_,reject) => helpers.augOrDimPyInterval(d[2],d[0],2,reject) %}

snpyItv ->
  # semi-neutral intervals
    "sM" pyDeg {% (d,_,reject) => helpers.nonPerfPyInterval(d[1],Fraction(1,4),reject) %}
  | "sm" pyDeg {% (d,_,reject) => helpers.nonPerfPyInterval(d[1],Fraction(-1,4),reject) %}
  | posInt "/4-A" pyDeg {% (d,_,reject) => helpers.augOrDimPyInterval(d[2],d[0],4,reject) %}
  | posInt "/4-d" pyDeg {% (d,_,reject) => helpers.augOrDimPyInterval(d[2],d[0],4,reject) %}

pyDeg ->
    posInt      {% d => parseInt(d[0]) %}
  | "-" posInt  {% d => - parseInt(d[1]) %}

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
  | "reb"  _ "(" _ itvExpr1 _ ")"                   {% d => d[4].reb() %}
  | "red"  _ "(" _ itvExpr1 _ "," _ itvExpr1 _ ")"  {% d => d[4].red(d[8]) %}
  | "reb"  _ "(" _ itvExpr1 _ "," _ itvExpr1 _ ")"  {% d => d[4].reb(d[8]) %}
  | itvExpr3                                        {% id %}
itvExpr3 ->
    symb                                            {% id %}
  | itvExpr4                                        {% id %}
itvExpr4 ->
    posInt                                          {% d => Interval(d[0]) %}
  | "(" _ itvExpr1 _ ")"                            {% d => d[2] %}

# -------------------
# Cents expressions

ctsExpr1 ->
    ctsExpr1 _ "+" _ ctsExpr2                        {% d => [helpers.cbnEDOs(d[0][0],d[4][0]), d[0][1].mul(d[4][1])] %}
  | ctsExpr1 _ "-" _ ctsExpr2                        {% d => [helpers.cbnEDOs(d[0][0],d[4][0]), d[0][1].div(d[4][1])] %}
  | ctsExpr2                                         {% id %}
ctsExpr2 ->
    ctsExpr3 _ "x" _ frcExpr3                        {% d => [d[0][0], d[0][1].pow(d[4])] %}
  | frcExpr3 _ "x" _ ctsExpr3                        {% d => [d[4][0], d[4][1].pow(d[0])] %}
  | ctsExpr3                                         {% id %}
ctsExpr3 ->
    "cents" _ "(" _ itvExpr1 _ ")"                   {% d => [null, d[4]] %}
  | "red"   _ "(" _ ctsExpr1 _ ")"                   {% d => [d[4][0], d[4][1].red()] %}
  | "reb"   _ "(" _ ctsExpr1 _ ")"                   {% d => [d[4][0], d[4][1].reb()] %}
  | "red"   _ "(" _ ctsExpr1 _ "," _ itvExpr1 _ ")"  {% d => [d[8].equals(2) ? d[4][0] : null, d[4][1].red(d[8])] %}
  | "reb"   _ "(" _ ctsExpr1 _ "," _ itvExpr1 _ ")"  {% d => [d[8].equals(2) ? d[4][0] : null, d[4][1].reb(d[8])] %}
  | symb                                             {% d => [null, d[0]] %}
  | decimal "c"                                      {% d => [null, Interval(2).pow(Fraction(d[0]).div(1200))] %}
  | edoExpr3 _ "\\" _ posInt
      {% (d,_,reject) => d[0](d[4]) == reject ? reject :
           [d[4], Interval(2).pow(Fraction(d[0](d[4])).div(Fraction(d[4])))] %}
  | "(" _ ctsExpr1 _ ")"                             {% d => d[2] %}

# ----------------------
# EDO-step expressions

edoExpr1 ->
    edoExpr1 _ "+" _ edoExpr2  {% d => edo => d[0](edo) + d[4](edo) %}
  | edoExpr1 _ "-" _ edoExpr2  {% d => edo => d[0](edo) - d[4](edo) %}
  | edoExpr2                   {% id %}
edoExpr2 ->
    edoExpr3 _ "x" _ intExpr1  {% d => edo => d[0](edo) * d[4] %}
  | intExpr1 _ "x" _ edoExpr3  {% d => edo => d[0] * d[4](edo) %}
  | edoExpr3                   {% id %}
edoExpr3 ->
    "-" _ edoExpr4             {% d => edo => - d[2](edo) %}
  | edoExpr4                   {% id %}
edoExpr4 ->
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
         !edoHasNeutrals(edo) || redDeg(d[2]) == 1 ? reject :
           redDeg(d[2]) == 4 ? d[0] + edoPy(edo,pyInterval(d[2],1,2)) :
           redDeg(d[2]) == 5 ? d[0] + edoPy(edo,pyInterval(d[2],-1,2)) :
                               d[0] + edoPy(edo,pyInterval(d[2],0)) %}
  # special notation for the tritone, if it exists
  | "TT"
      {% (d,_,reject) => edo => edo % 2 == 0 ? edo/2 : reject %}
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
