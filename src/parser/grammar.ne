@{%

const Fraction = require('fraction.js');
const Interval = require('../interval.js');
const {pySymb, pyInterval, redDeg, octaveOfIntvToA4} = require('../pythagorean.js');
const {fjsFactor} = require('../fjs.js');
const {edoPy, edoHasNeutrals, edoHasSemiNeutrals} = require('../edo.js');
const helpers = require('./grammar-helpers.js');

%}

@builtin "whitespace.ne"

top1 ->
    _ top2 _
    {% function (d) { const r = { intvToA4: Interval(1)
                                , hertz: Interval(440) };
                      return d[1](r).concat(r); } %}
  | _ top2 __ "where" __ pyNote _ "=" _ decimal hertz:? _
    {% function (d) { const r = { intvToA4: d[5](Interval(1))
                                , hertz: Interval(d[9]) };
                      return d[1](r).concat(r); } %}
  | _ top2 __ "where" __ pyNote _ "=" _ pyNote _ "\\" _ posInt _
    {% function (d,_,reject) {
         if (!d[5](Interval(1)).equals(d[9](Interval(1)))) { return reject; }
         const s = edoPy(parseInt(d[13]),d[9](Interval(1)));
         const r = { intvToA4: d[5](Interval(1))
                   , hertz: Interval(2).pow(s,d[13]).mul(440) };
         return d[1](r).concat(r); } %}
top2 ->
    topIntv  {% id %}
  | topNote  {% id %}

topIntv ->
    intvSExpr1  {% d => _ => ["interval", true, d[0], null] %}
  | intvMExpr1  {% d => r => ["interval", false, d[0](r), null] %}
  | intvAExpr1  {% d => r => ["interval", false, d[0](r)[0], d[0](r)[1]] %}

topNote ->
    noteSExpr1  {% d => r => ["note", true, d[0](r.intvToA4), null] %}
  | noteMExpr1  {% d => r => ["note", false, d[0](r), null] %}
  | noteAExpr1  {% d => r => ["note", false, d[0](r)[0], d[0](r)[1]] %}


# -------------------------------------
# Multiplicative interval expressions
# type: {intvToA4: Interval, hertz: Interval} => Interval

intvMExpr1 ->
    intvMExpr1 _ "*" _ intvMExpr2                       {% d => r => d[0](r).mul(d[4](r)) %}
  | intvMExpr1 _ "/" _ intvMExpr2                       {% d => r => d[0](r).div(d[4](r)) %}
  | noteMExpr1 _ "/" _ noteMExpr2                       {% d => r => d[0](r).div(d[4](r)) %}
  | intvMExpr2                                          {% id %}
intvMExpr2 ->
    intvMExpr3 _ "^" _ frcExpr3                         {% d => r => d[0](r).pow(d[4]) %}
  | "sqrt" _ "(" _ intvMExpr1 _ ")"                     {% d => r => d[4](r).sqrt() %}
  | "red"  _ "(" _ intvMExpr1 _ ")"                     {% d => r => d[4](r).red() %}
  | "reb"  _ "(" _ intvMExpr1 _ ")"                     {% d => r => d[4](r).reb() %}
  | "red"  _ "(" _ intvMExpr1 _ "," _ intvMExpr1 _ ")"  {% d => r => d[4](r).red(d[8](r)) %}
  | "reb"  _ "(" _ intvMExpr1 _ "," _ intvMExpr1 _ ")"  {% d => r => d[4](r).reb(d[8](r)) %}
  | intvSymbol                                          {% d => _ => d[0] %}
  | intvMExpr3                                          {% id %}
intvMExpr3 ->
    posInt                                              {% d => _ => Interval(d[0]) %}
  | "(" _ intvMExpr1 _ ")"                              {% d => d[2] %}

# ---------------------------------
# Multiplicative note expressions
# type: {intvToA4: Interval, hertz: Interval} => Interval

noteMExpr1 ->
    noteMExpr1 _ "*" _ intvMExpr2  {% d => r => d[0](r).mul(d[4](r)) %}
  | intvMExpr1 _ "*" _ noteMExpr2  {% d => r => d[0](r).mul(d[4](r)) %}
  | noteMExpr1 _ "/" _ intvMExpr2  {% d => r => d[0](r).div(d[4](r)) %}
  | noteMExpr2                     {% id %}
noteMExpr2 ->
    noteSymbol                     {% d => r => d[0](r.intvToA4) %}
  | decimal hertz                  {% d => r => Interval(d[0]).div(r.hertz) %}
  | "(" _ noteMExpr1 _ ")"         {% d => d[2] %}

# -------------------------------
# Additive interval expressions
# type: {intvToA4: Interval, hertz: Interval} => Interval

intvAExpr1 ->
    intvAExpr1 _ "+" _ intvAExpr2                        {% d => r => [d[0](r)[0].mul(d[4](r)[0]), helpers.cbnEDOs(d[0](r)[1],d[4](r)[1])] %}
  | intvAExpr1 _ "-" _ intvAExpr2                        {% d => r => [d[0](r)[0].div(d[4](r)[0]), helpers.cbnEDOs(d[0](r)[1],d[4](r)[1])] %}
  | noteAExpr1 _ "-" _ noteAExpr2                        {% d => r => [d[0](r)[0].div(d[4](r)[0]), helpers.cbnEDOs(d[0](r)[1],d[4](r)[1])] %}
  | intvAExpr2                                           {% id %}
intvAExpr2 ->
    intvAExpr3 _ "x" _ frcExpr3                          {% d => r => [d[0](r)[0].pow(d[4]), d[0](r)[1]] %}
  | frcExpr3 _ "x" _ intvAExpr3                          {% d => r => [d[4](r)[0].pow(d[0]), d[4](r)[1]] %}
  | intvAExpr3                                           {% id %}
intvAExpr3 ->
    "cents" _ "(" _ intvMExpr1 _ ")"                     {% d => r => [d[4](r), null] %}
  | "red"   _ "(" _ intvAExpr1 _ ")"                     {% d => r => [d[4](r)[0].red(), d[4](r)[1]] %}
  | "reb"   _ "(" _ intvAExpr1 _ ")"                     {% d => r => [d[4](r)[0].reb(), d[4](r)[1]] %}
  | "red"   _ "(" _ intvAExpr1 _ "," _ intvMExpr1 _ ")"  {% d => r => [d[4](r)[0].red(d[8](r)), d[8](r).equals(2) ? d[4](r)[1] : null] %}
  | "reb"   _ "(" _ intvAExpr1 _ "," _ intvMExpr1 _ ")"  {% d => r => [d[4](r)[0].reb(d[8](r)), d[8](r).equals(2) ? d[4](r)[1] : null] %}
  | intvSymbol                                           {% d => _ => [d[0], null] %}
  | decimal "c"
    {% d => function () {
         const d0 = Fraction(d[0]).div(1200);
         return [Interval(2).pow(d0), d0.mul(48).d == 1 ? d0.d : null] } %}
  | intvEDOExpr3 _ "\\" _ posInt
    {% (d,_,reject) => function (r) {
         const d0 = d[0]({intvToA4: r.intvToA4, edo: d[4]});
         if (d0 == reject) { return reject; }
         else { return [Interval(2).pow(d0).pow(1,d[4]), d[4]] } } %}
  | "(" _ intvAExpr1 _ ")"                               {% d => d[2] %}

# ---------------------------
# Additive note expressions
# type: {intvToA4: Interval, hertz: Interval} => Interval

noteAExpr1 ->
    noteAExpr1 _ "+" _ intvAExpr2  {% d => r => [d[0](r)[0].mul(d[4](r)[0]), helpers.cbnEDOs(d[0](r)[1],d[4](r)[1])] %}
  | intvAExpr1 _ "+" _ noteAExpr2  {% d => r => [d[0](r)[0].mul(d[4](r)[0]), helpers.cbnEDOs(d[0](r)[1],d[4](r)[1])] %}
  | noteAExpr1 _ "-" _ intvAExpr2  {% d => r => [d[0](r)[0].div(d[4](r)[0]), helpers.cbnEDOs(d[0](r)[1],d[4](r)[1])] %}
  | noteAExpr2                     {% id %}
noteAExpr2 ->
    noteSymbol                     {% d => r => [d[0](r.intvToA4), null] %}
  | noteEDOExpr2 _ "\\" _ posInt
    {% (d,_,reject) => function (r) {
         const d0 = d[0]({intvToA4: r.intvToA4, edo: d[4]});
         if (d0 == reject) { return reject; }
         else { return [Interval(2).pow(d0).pow(1,d[4]), d[4]] } } %}
  | "(" _ noteAExpr1 _ ")"         {% d => d[2] %}

# -------------------------------
# EDO-step interval expressions
# type: {intvToA4: Interval, edo: ?Integer} => Interval

intvEDOExpr1 ->
    intvEDOExpr1 _ "+" _ intvEDOExpr2  {% d => r => d[0](r) + d[4](r) %}
  | intvEDOExpr1 _ "-" _ intvEDOExpr2  {% d => r => d[0](r) - d[4](r) %}
  | noteEDOExpr1 _ "-" _ noteEDOExpr2  {% d => r => d[0](r) - d[4](r) %}
  | intvEDOExpr2                       {% id %}
intvEDOExpr2 ->
    intvEDOExpr3 _ "x" _ intExpr1      {% d => r => d[0](r) * d[4] %}
  | intExpr1 _ "x" _ intvEDOExpr3      {% d => r => d[0] * d[4](r) %}
  | intvEDOExpr3                       {% id %}
intvEDOExpr3 ->
    "-" _ intvEDOExpr4                 {% d => r => - d[2](r) %}
  | intvEDOExpr4                       {% id %}
intvEDOExpr4 ->
    posInt                             {% d => _ => parseInt(d[0]) %}
  | upsDns pyIntv                      {% d => r => d[0] + edoPy(r.edo,d[1]) %}
  | upsDns npyIntv
    {% (d,_,reject) => r =>
         !edoHasNeutrals(r.edo) ? reject : d[0] + edoPy(r.edo,d[1]) %}
  | upsDns snpyIntv
    {% (d,_,reject) => r =>
         !edoHasSemiNeutrals(r.edo) ? reject : d[0] + edoPy(r.edo,d[1]) %}
  # alternate notation for neutal intervals, semi-augmented fourths, and
  # semi-diminished fifths
  | upsDns "~" posInt
    {% (d,_,reject) => r =>
         !edoHasNeutrals(r.edo) || redDeg(d[2]) == 1 ? reject :
           redDeg(d[2]) == 4 ? d[0] + edoPy(r.edo,pyInterval(d[2],1,2)) :
           redDeg(d[2]) == 5 ? d[0] + edoPy(r.edo,pyInterval(d[2],-1,2)) :
                               d[0] + edoPy(r.edo,pyInterval(d[2],0)) %}
  # special notation for the tritone, if it exists
  | "TT"
    {% (d,_,reject) => r => r.edo % 2 == 0 ? r.edo/2 : reject %}
  | "(" _ intvEDOExpr1 _ ")"           {% d => d[2] %}

upsDns ->
    null   {% d => 0 %}
  | "^":+  {% d => d[0].length %}
  | "v":+  {% d => - d[0].length %}

# ---------------------------
# EDO-step note expressions
# type: {intvToA4: Interval, edo: ?Integer} => Interval

noteEDOExpr1 ->
    noteEDOExpr1 _ "+" _ intvEDOExpr2  {% d => r => d[0](r) + d[4](r) %}
  | intvEDOExpr1 _ "+" _ noteEDOExpr2  {% d => r => d[0](r) + d[4](r) %}
  | noteEDOExpr1 _ "-" _ intvEDOExpr2  {% d => r => d[0](r) - d[4](r) %}
  | noteEDOExpr2                       {% id %}
noteEDOExpr2 ->
    upsDns pyNote                      {% d => r => d[0] + edoPy(r.edo,d[1](r.intvToA4)) %}
  | upsDns npyNote
    {% (d,_,reject) => r =>
         !edoHasNeutrals(r.edo) ? reject : d[0] + edoPy(r.edo,d[1](r.intvToA4)) %}
  | "(" _ noteEDOExpr1 _ ")"           {% d => d[2] %}

# -----------------------------
# Interval symbol expressions
# type: Interval

intvSExpr1 ->
    "red"  _ "(" _ intvSExpr1 _ ")"                     {% d => d[4].red() %}
  | "reb"  _ "(" _ intvSExpr1 _ ")"                     {% d => d[4].reb() %}
  | "red"  _ "(" _ intvSExpr1 _ "," _ intvMExpr1 _ ")"  {% d => d[4].red(d[8]) %}
  | "reb"  _ "(" _ intvSExpr1 _ "," _ intvMExpr1 _ ")"  {% d => d[4].reb(d[8]) %}
  | intvSExpr2                                          {% id %}
intvSExpr2 ->
    intvSymbol                                          {% id %}
  | "(" _ intvSExpr1 _ ")"                              {% d => d[2] %}

intvSymbol ->
    fjsIntv   {% id %}
  | npyIntv   {% id %}
  | snpyIntv  {% id %}
  | "TT"      {% _ => Interval(2).sqrt() %}

# ------------------------
# Note symbol expresions
# type: Interval => Interval

noteSExpr1 ->
    noteSymbol              {% id %}
  | "(" _ noteSExpr1 _ ")"  {% d => d[2] %}

noteSymbol ->
    fjsNote  {% id %}
  | npyNote  {% id %}

# -------------
# FJS interval and note symbols
# type: Interval and Interval => Interval

fjsIntv ->
    pyIntv               {% id %}
  | fjsIntv "^" fjsAccs  {% d => d[0].mul(d[2]) %}
  | fjsIntv "_" fjsAccs  {% d => d[0].div(d[2]) %}

fjsNote ->
    pyNote               {% id %}
  | fjsNote "^" fjsAccs  {% d => refIntvToA4 => d[0](refIntvToA4).mul(d[2]) %}
  | fjsNote "_" fjsAccs  {% d => refIntvToA4 => d[0](refIntvToA4).div(d[2]) %}

fjsAccs ->
    fjsAcc              {% d => fjsFactor(d[0]) %}
  | fjsAccs "," fjsAcc  {% d => d[0].mul(fjsFactor(d[2])) %}

fjsAcc ->
    posInt                        {% (d,_,reject) => helpers.ensureNo2Or3(Interval(d[0]),reject) %}
  | "sqrt(" fjsAcc ")"            {% d => d[1].sqrt() %}
  | "root" posInt "(" fjsAcc ")"  {% d => d[3].root(d[1]) %}
  | "(" fjsAcc "^" frcExpr3 ")"   {% d => d[1].pow(d[3]) %}

# ------------------------------
# Pythagorean interval symbols
# type: Interval

pyIntv ->
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

npyIntv ->
  # neutral intervals
    "n"i pyDeg {% (d,_,reject) => helpers.nonPerfPyInterval(d[1],0,reject) %}
  # semi-augmented and semi-diminished intervals
  | "sA" pyDeg {% (d,_,reject) => helpers.augOrDimPyInterval(d[1],1,2,reject) %}
  | "sd" pyDeg {% (d,_,reject) => helpers.augOrDimPyInterval(d[1],-1,2,reject) %}
  | posInt "/2-A" pyDeg {% (d,_,reject) => helpers.augOrDimPyInterval(d[2],d[0],2,reject) %}
  | posInt "/2-d" pyDeg {% (d,_,reject) => helpers.augOrDimPyInterval(d[2],d[0],2,reject) %}

snpyIntv ->
  # semi-neutral intervals
    "sM" pyDeg {% (d,_,reject) => helpers.nonPerfPyInterval(d[1],Fraction(1,4),reject) %}
  | "sm" pyDeg {% (d,_,reject) => helpers.nonPerfPyInterval(d[1],Fraction(-1,4),reject) %}
  | posInt "/4-A" pyDeg {% (d,_,reject) => helpers.augOrDimPyInterval(d[2],d[0],4,reject) %}
  | posInt "/4-d" pyDeg {% (d,_,reject) => helpers.augOrDimPyInterval(d[2],d[0],4,reject) %}

pyDeg ->
    posInt      {% d => parseInt(d[0]) %}
  | "-" posInt  {% d => - parseInt(d[1]) %}

# --------------------------
# Pythagorean note symbols
# type: Interval => Interval

genPyNote[NOTE,ACCS] ->
    $NOTE $ACCS int:?
    {% d => function(refIntvToA4) {
         const d2 = d[2] ? parseInt(d[2]) : 4;
         return helpers.baseNoteIntvToReference(d[0], refIntvToA4)
                         .mul(d[1][0])
                         .mul(Interval(2).pow(d2 - 4)); } %}

pyNote ->
    "A"                             {% _ => refIntvToA4 => refIntvToA4.recip() %}
  | genPyNote[[B-G], pyNoteNoAccs]  {% id %}
  | genPyNote[[A-G], pyNoteAccs]    {% id %}

pyNoteNoAccs -> null  {% _ => Interval(1) %}

pyNoteAccs ->
    "♮"                          {% _ => Interval(1) %}
  | ("♯" | "#"):+                {% d => pyInterval(1, d[0].length) %}
  | ("𝄪" | "X"):+ ("♯" | "#"):*  {% d => pyInterval(1, 2*d[0].length + d[1].length) %}
  | ("♭" | "b"):+                {% d => pyInterval(-1, d[0].length) %}
  | ("♭" | "b"):* "𝄫":+          {% d => pyInterval(-1, 2*d[0].length + d[1].length) %}

npyNote ->
    genPyNote[[A-G], npyNoteAccs]   {% id %}

npyNoteAccs ->
    ("𝄪" | "X"):* ("♯" | "#"):* ("𝄲" | "t"):+
    {% d => pyInterval(1, 2*d[0].length + d[1].length + 0.5*d[2].length) %}
  | ("𝄳" | "d"):+ ("♭" | "b"):* "𝄫":*
    {% d => pyInterval(-1, 2*d[0].length + d[1].length + 0.5*d[2].length) %}

# ------------------------------------------------------
# Fractional expressions (positive, negative, or zero)
# type: Fraction

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
    nonNegInt                  {% d => Fraction(d[0]) %}
  | "(" _ frcExpr1 _ ")"       {% d => d[2] %}

# ---------------------------------------------------
# Integer expressions (positive, negative, or zero)
# type: Integer

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
    nonNegInt                  {% d => parseInt(d[0]) %}
  | "(" _ intExpr1 _ ")"       {% d => d[2] %}

# -----------
# Terminals
# type: String

posInt -> [1-9] [0-9]:* {% d => d[0] + d[1].join("") %}

nonNegInt -> "0" {% _ => "0" %} | posInt {% id %}

int -> "-":? nonNegInt {% d => (d[0] || "") + d[1] %}

decimal -> "-":? [0-9]:+ ("." [0-9]:* ("(" [0-9]:+ ")"):?):?
  {% d => (d[0] || "") + d[1].join("")
                       + (d[2] ? "." + d[2][1].join("")
                                     + (d[2][2] ? "("+d[2][2][1].join("")+")"
                                                : "")
                               : "") %}

hertz -> "hz" | "Hz"
