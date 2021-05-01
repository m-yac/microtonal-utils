#
# Grammar for interval and note expressions
# @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
#
# This file generates a parser by running:
# ```
# nearleyc lib/parser/grammar.ne -o lib/parser/grammar.js
# ```
#
# You can then get nicely formatted output by passing your string to the `parse`
#  or `parseCvt` functions from `parser.js`.
#
# You can also get the raw output of this parser by doing the following:
# ```
# const ne = resquire('nearley');
# const grammar = require('./parser/grammar.js');
# parser.feed(str);
# const result = parser.results[0];
# ```
# Then to evaluate these results (i.e. convert the raw output into the Interval
#  it represents), do the following:
# ```
# const {evalExpr} = require('./parser/eval.js');
# const resultIntv = evalExpr(result[0].expr, result[0].refNote).val;
# ```

@{%

const Fraction = require('fraction.js');
const Interval = require('../interval.js');
const {pyInterval, pyNote, pyRedDeg, baseNoteIntvToA} = require('../pythagorean.js');
const {fjsFactor, fjsParams, nfjsParams} = require('../fjs.js');
const {edoPy} = require('../edo.js');
const {ParseError, evalExpr} = require('./eval.js');

const defaultRefNote = { intvToA4: Interval(1), hertz: Interval(440) };

%}

@builtin "whitespace.ne"

# ------------------------------------------------------
# Interval and note expressions
# ------------------------------------------------------

top1 ->
    _ top2 _
    {% function (d) { let d1 = Object.assign({},d[1]); // copy this!
                      d1.refNote = defaultRefNote;
                      return d1; } %}
  | _ top2 __ "where" __ pyNote _ "=" _ decimal hertz:? _
    {% function (d) { let d1 = Object.assign({},d[1]); // copy this!
                      d1.refNote = {};
                      d1.refNote.intvToA4 = evalExpr(d[5], defaultRefNote).val;
                      d1.refNote.hertz    = Interval(d[9]);
                      return d1; } %}
  | _ top2 __ "where" __ pyNote _ "=" _ eqPyNote _ "\\" _ posInt _
    {% function (d) { let d1 = Object.assign({},d[1]); // copy this!
                      const d5 = evalExpr(d[5], defaultRefNote).val;
                      const d9 = d[9](d5);
                      const d13 = parseInt(d[13]);
                      d1.refNote = {};
                      d1.refNote.intvToA4 = d9;
                      d1.refNote.hertz    = Interval(2).pow(edoPy(d13,d9),d13).mul(440);
                      return d1; } %}

top2 ->
    intvSExpr1  {% d => ({type: ["interval", "symbol"], expr: d[0]}) %}
  | intvMExpr1  {% d => ({type: ["interval", "multiplicative"], expr: d[0]}) %}
  | intvAExpr1  {% d => ({type: ["interval", "additive"], expr: d[0]}) %}
  | noteSExpr1  {% d => ({type: ["note", "symbol"], expr: d[0]}) %}
  | noteMExpr1  {% d => ({type: ["note", "multiplicative"], expr: d[0]}) %}
  | noteAExpr1  {% d => ({type: ["note", "additive"], expr: d[0]}) %}

eqPyNote -> pyNote {% (d,loc,_) => function(ref) {
    let d0 = evalExpr(d[0], defaultRefNote).val;
    if (!ref || !ref.equals(d0)) {
      throw new ParseError("expected " + pyNote(ref), loc);
    }
    return d0;
  } %}


# ------------------------------------------------------
# Interval and note expressions
# ------------------------------------------------------

# When reading the rest of this file, it's useful to keep in mind that:
# `["ident", a0, a1, ..., an]` gets evaluated to `a0.ident(a1,...,an)`
# `["operator", a0, a1]` gets evaluated to `a0 operator a1` (e.g. "+")
# `["!ident", a0, ..., an]` has some special handling - see eval.js

# -------------------------------------
# Multiplicative interval expressions
# @returns {(Interval|Array)}

intvMExpr1 ->
    intvMExpr1 _ "*" _ intvMExpr2                      {% d => ["mul", d[0], d[4]] %}
  | intvMExpr1 _ "/" _ intvMExpr2                      {% d => ["div", d[0], d[4]] %}
  | noteMExpr1 _ "/" _ noteMExpr2                      {% d => ["div", d[0], d[4]] %}
  | intvMExpr2                                         {% id %}
intvMExpr2 ->
    intvMExpr3 _ "^" _ frcExpr3                        {% d => ["pow", d[0], d[4]] %}
  | "sqrt" _ "(" _ intvMExpr1 _ ")"                    {% d => ["sqrt", d[4]] %}
  | "root" posInt _ "(" _ intvMExpr1 _ ")"             {% d => ["root", d[5], d[1]] %}
  | "red" _ "(" _ intvMExpr1 _ ")"                     {% d => ["red", d[4]] %}
  | "reb" _ "(" _ intvMExpr1 _ ")"                     {% d => ["reb", d[4]] %}
  | "red" _ "(" _ intvMExpr1 _ "," _ intvMExpr1 _ ")"  {% d => ["red", d[4], d[8]] %}
  | "reb" _ "(" _ intvMExpr1 _ "," _ intvMExpr1 _ ")"  {% d => ["reb", d[4], d[8]] %}
  | "med" _ "(" _ intvMExpr1 _ "," _ intvMExpr1 _ ")"  {% (d,loc,_) => ["!med", d[4], d[8], loc] %}
  | "approx"  _ "(" _ intvMExpr1 _ "," _ posInt _ ")"  {% d => ["!edoApprox", d[4], parseInt(d[8])] %}
  | intvSymbol                                         {% id %}
  | intvMExpr3                                         {% id %}
intvMExpr3 ->
    posInt                                             {% d => Interval(d[0]) %}
  | int           _ "\\" _ posInt                      {% d => ["!inEDO", parseInt(d[0]), parseInt(d[4])] %}
  | intvMEDOExpr3 _ "\\" _ posInt                      {% d => ["!inEDO", d[0], parseInt(d[4])] %}
  | "(" _ intvMExpr1 _ ")"                             {% d => d[2] %}

# ---------------------------------
# Multiplicative note expressions
# @returns {(Interval|Array)}

noteMExpr1 ->
    noteMExpr1 _ "*" _ intvMExpr2                     {% d => ["mul", d[0], d[4]] %}
  | intvMExpr1 _ "*" _ noteMExpr2                     {% d => ["mul", d[0], d[4]] %}
  | noteMExpr1 _ "/" _ intvMExpr2                     {% d => ["div", d[0], d[4]] %}
  | noteMExpr2                                        {% id %}
noteMExpr2 ->
    "approx"  _ "(" _ noteMExpr1 _ "," _ posInt _ ")" {% d => ["!edoApprox", d[4], parseInt(d[8])] %}
  | noteSymbol                                        {% id %}
  | noteMEDOExpr2 _ "\\" _ posInt                     {% d => ["!inEDO", d[0], parseInt(d[4])] %}
  | decimal hertz                                     {% d => ["div", Interval(d[0]), ["!refHertz"]] %}
  | "(" _ noteMExpr1 _ ")"                            {% d => d[2] %}

# -------------------------------
# Additive interval expressions
# @returns {(Interval|Array)}

intvAExpr1 ->
    intvAExpr1 _ "+" _ intvAExpr2                      {% d => ["mul", d[0], d[4]] %}
  | intvAExpr1 _ "-" _ intvAExpr2                      {% d => ["div", d[0], d[4]] %}
  | noteAExpr1 _ "-" _ noteAExpr2                      {% d => ["div", d[0], d[4]] %}
  | intvAExpr2                                         {% id %}
intvAExpr2 ->
    intvAExpr3 _ "x" _ frcExpr2                        {% d => ["pow", d[0], d[4]] %}
  | frcExpr2 _ "x" _ intvAExpr3                        {% d => ["pow", d[4], d[0]] %}
  | intvAExpr3                                         {% id %}
intvAExpr3 ->
    "cents" _ "(" _ intvMExpr1 _ ")"                   {% d => d[4] %}
  | "red" _ "(" _ intvAExpr1 _ ")"                     {% d => ["red", d[4]] %}
  | "reb" _ "(" _ intvAExpr1 _ ")"                     {% d => ["reb", d[4]] %}
  | "red" _ "(" _ intvAExpr1 _ "," _ intvMExpr1 _ ")"  {% d => ["red", d[4], d[8]] %}
  | "reb" _ "(" _ intvAExpr1 _ "," _ intvMExpr1 _ ")"  {% d => ["reb", d[4], d[8]] %}
  | "approx"  _ "(" _ intvAExpr1 _ "," _ posInt _ ")"  {% d => ["!edoApprox", d[4], parseInt(d[8])] %}
  | intvSymbol                                         {% id %}
  | intvAExpr4                                         {% id %}
intvAExpr4 ->
    decimal "c"                                        {% d => ["!cents", d[0]] %}
  | intvAEDOExpr3 _ "\\" _ posInt                      {% d => ["!inEDO", d[0], parseInt(d[4])] %}
  | "(" _ intvAExpr1 _ ")"                             {% d => d[2] %}

# ---------------------------
# Additive note expressions
# @returns {(Interval|Array)}

noteAExpr1 ->
    noteAExpr1 _ "+" _ intvAExpr2                     {% d => ["mul", d[0], d[4]] %}
  | intvAExpr1 _ "+" _ noteAExpr2                     {% d => ["mul", d[0], d[4]] %}
  | noteAExpr1 _ "-" _ intvAExpr2                     {% d => ["div", d[0], d[4]] %}
  | noteAExpr2                                        {% id %}
noteAExpr2 ->
    "approx"  _ "(" _ noteAExpr1 _ "," _ posInt _ ")" {% d => ["!edoApprox", d[4], parseInt(d[8])] %}
  | noteSymbol                                        {% id %}
  | noteAEDOExpr2 _ "\\" _ posInt                     {% d => ["!inEDO", d[0], parseInt(d[4])] %}
  | "(" _ noteAExpr1 _ ")"                            {% d => d[2] %}

# ----------------------------------------------
# Multiplicative EDO-step interval expressions
# @returns {(integer|Array)}

intvMEDOExpr1 ->
    intvMEDOExpr1 _ "*" _ intvMEDOExpr2  {% d => ["+", d[0], d[4]] %}
  | intvMEDOExpr1 _ "/" _ intvMEDOExpr2  {% d => ["-", d[0], d[4]] %}
  | noteMEDOExpr1 _ "/" _ noteMEDOExpr2  {% d => ["-", d[0], d[4]] %}
  | intvMEDOExpr2                        {% id %}
intvMEDOExpr2 ->
    intvMEDOExpr3 _ "^" _ intExpr1       {% d => ["*", d[0], d[4]] %}
  | intvMEDOExpr3                        {% id %}
intvMEDOExpr3 ->
    upsDnsIntv                           {% id %}
  | "TT"                                 {% d => ["!edoTT", loc] %}
  | "(" _ intvMEDOExpr1 _ ")"            {% d => d[2] %}

# ------------------------------------------
# Multiplicative EDO-step note expressions
# @returns {(integer|Array)}

noteMEDOExpr1 ->
    noteMEDOExpr1 _ "*" _ intvMEDOExpr2  {% d => ["+", d[0], d[4]] %}
  | intvMEDOExpr1 _ "*" _ noteMEDOExpr2  {% d => ["+", d[0], d[4]] %}
  | noteMEDOExpr1 _ "/" _ intvMEDOExpr2  {% d => ["-", d[0], d[4]] %}
  | noteMEDOExpr2                        {% id %}
noteMEDOExpr2 ->
    upsDnsNote                           {% id %}
  | "(" _ noteMEDOExpr1 _ ")"            {% d => d[2] %}

# ----------------------------------------
# Additive EDO-step interval expressions
# @returns {(integer|Array)}

intvAEDOExpr1 ->
    intvAEDOExpr1 _ "+" _ intvAEDOExpr2  {% d => ["+", d[0], d[4]] %}
  | intvAEDOExpr1 _ "-" _ intvAEDOExpr2  {% d => ["-", d[0], d[4]] %}
  | noteAEDOExpr1 _ "-" _ noteAEDOExpr2  {% d => ["-", d[0], d[4]] %}
  | intvAEDOExpr2                        {% id %}
intvAEDOExpr2 ->
    intvAEDOExpr3 _ "x" _ intExpr1       {% d => ["*", d[0], d[4]] %}
  | intExpr1 _ "x" _ intvAEDOExpr3       {% d => ["*", d[0], d[4]] %}
  | intvAEDOExpr3                        {% id %}
intvAEDOExpr3 ->
    "-" _ intvAEDOExpr4                  {% d => ["-", 0, d[2]] %}
  | intvAEDOExpr4                        {% id %}
intvAEDOExpr4 ->
    nonNegInt                            {% d => parseInt(d[0]) %}
  | upsDnsIntv                           {% id %}
  | "TT"                                 {% d => ["!edoTT"] %}
  | "(" _ intvAEDOExpr1 _ ")"            {% d => d[2] %}

# ------------------------------------
# Additive EDO-step note expressions
# @returns {(integer|Array)}

noteAEDOExpr1 ->
    noteAEDOExpr1 _ "+" _ intvAEDOExpr2  {% d => ["+", d[0], d[4]] %}
  | intvAEDOExpr1 _ "+" _ noteAEDOExpr2  {% d => ["+", d[0], d[4]] %}
  | noteAEDOExpr1 _ "-" _ intvAEDOExpr2  {% d => ["-", d[0], d[4]] %}
  | noteAEDOExpr2                        {% id %}
noteAEDOExpr2 ->
    upsDnsNote                           {% id %}
  | "(" _ noteAEDOExpr1 _ ")"            {% d => d[2] %}

# -----------------------------
# Interval symbol expressions
# @returns {(Interval|Array)}

intvSExpr1 ->
    "red"  _ "(" _ intvSExpr1 _ ")"                     {% d => ["red", d[4]] %}
  | "reb"  _ "(" _ intvSExpr1 _ ")"                     {% d => ["reb", d[4]] %}
  | "red"  _ "(" _ intvSExpr1 _ "," _ intvMExpr1 _ ")"  {% d => ["red", d[4], d[8]] %}
  | "reb"  _ "(" _ intvSExpr1 _ "," _ intvMExpr1 _ ")"  {% d => ["reb", d[4], d[8]] %}
  | "approx"  _ "(" _ intvSExpr1 _ "," _ posInt _ ")"   {% d => ["!edoApprox", d[4], parseInt(d[8])] %}
  | intvSExpr2                                          {% id %}
intvSExpr2 ->
    intvSymbol                                          {% id %}
  | int        _ "\\" _ posInt                          {% d => ["!inEDO", parseInt(d[0]), parseInt(d[4])] %}
  | upsDnsIntv _ "\\" _ posInt                          {% d => ["!inEDO", d[0], parseInt(d[4])] %}
  | "TT"       _ "\\" _ posInt                          {% d => ["!inEDO", ["!edoTT"], parseInt(d[4])] %}
  | "(" _ intvSExpr1 _ ")"                              {% d => d[2] %}

# ------------------------
# Note symbol expresions
# @returns {(Interval|Array)}

noteSExpr1 ->
    "approx"  _ "(" _ noteSExpr1 _ "," _ posInt _ ")"  {% d => ["!edoApprox", d[4], parseInt(d[8])] %}
  | noteSymbol                                         {% id %}
  | upsDnsNote _ "\\" _ posInt                         {% d => ["!inEDO", d[0], parseInt(d[4])] %}
  | "(" _ noteSExpr1 _ ")"                             {% d => d[2] %}

# ------------------------------------------------------
# Interval and note symbols
# ------------------------------------------------------

# ------------------------------
# Interval and note symbols
# @returns {(Interval|Array)}

intvSymbol ->
    fjsIntv                        {% id %}
  | nfjsNeutIntv                   {% id %}
  | "NFJS" _ "(" _ nfjsIntv _ ")"  {% d => d[4] %}
  | snpyIntv                       {% id %}
  | "TT"                           {% _ => Interval(2).sqrt() %}

noteSymbol ->
    fjsNote                        {% id %}
  | nfjsNeutNote                   {% id %}
  | "NFJS" _ "(" _ nfjsNote _ ")"  {% d => d[4] %}
  | npyNote                        {% id %}

# ------------------------------
# Pythagorean interval symbols
# @returns {(Interval|Array)}

pyIntv ->
  # perfect intervals
    "P"  pyDeg {% (d,loc,_) => ["!perfPyIntv", d[1], loc] %}
  # major and minor intervals
  | "M"  pyDeg {% (d,loc,_) => ["!nonPerfPyIntv", d[1], Fraction(1,2), "M", loc] %}
  | "m"  pyDeg {% (d,loc,_) => ["!nonPerfPyIntv", d[1], Fraction(-1,2), "m", loc] %}
  # augmented and diminished intervals
  | "A":+ pyDeg {% (d,loc,_) => ["!augOrDimPyIntv", d[1], d[0].length, 1, loc] %}
  | "d":+ pyDeg {% (d,loc,_) => ["!augOrDimPyIntv", d[1], -d[0].length, 1, loc] %}
  | posInt "A" pyDeg {% (d,loc,_) => ["!augOrDimPyIntv", d[2], d[0], 1, loc] %}
  | posInt "d" pyDeg {% (d,loc,_) => ["!augOrDimPyIntv", d[2], -d[0], 1, loc] %}

npyIntv ->
  # neutral intervals
    "n"i pyDeg {% (d,loc,_) => ["!nonPerfPyIntv", d[1], 0, "n", loc] %}
  # semi-augmented and semi-diminished intervals
  | "sA" pyDeg {% (d,loc,_) => ["!augOrDimPyIntv", d[1], 1, 2, loc] %}
  | "sd" pyDeg {% (d,loc,_) => ["!augOrDimPyIntv", d[1], -1, 2, loc] %}
  | posInt "/2-A" pyDeg {% (d,loc,_) => ["!augOrDimPyIntv", d[2], d[0], 2, loc] %}
  | posInt "/2-d" pyDeg {% (d,loc,_) => ["!augOrDimPyIntv", d[2], -d[0], 2, loc] %}

snpyIntv ->
  # semi-neutral intervals
    "sM" pyDeg {% (d,loc,_) => ["!nonPerfPyIntv", d[1], Fraction(1,4), "sM", loc] %}
  | "sm" pyDeg {% (d,loc,_) => ["!nonPerfPyIntv", d[1], Fraction(-1,4), "sm", loc] %}
  | posInt "/4-A" pyDeg {% (d,loc,_) => ["!augOrDimPyIntv", d[2], d[0], 4, loc] %}
  | posInt "/4-d" pyDeg {% (d,loc,_) => ["!augOrDimPyIntv", d[2], -d[0], 4, loc] %}

pyDeg ->
    posInt      {% d => parseInt(d[0]) %}
  | "-" posInt  {% d => - parseInt(d[1]) %}

# --------------------------
# Pythagorean note symbols
# @returns {(Interval|Array)}

genPyNote[NOTE,ACCS] ->
    $NOTE $ACCS int:?
    {% function(d) {
         const d2 = d[2] ? parseInt(d[2]) : 4;
         return ["mul", ["div", baseNoteIntvToA(d[0][0]), ["!refIntvToA4"]]
                      , d[1][0].mul(Interval(2).pow(d2 - 4))]; } %}

pyNote ->
    "A"                             {% _ => ["recip", ["!refIntvToA4"]] %}
  | genPyNote[[B-G], pyNoteNoAccs]  {% id %}
  | genPyNote[[A-G], pyNoteAccs]    {% id %}

pyNoteNoAccs -> null  {% _ => Interval(1) %}

pyNoteAccs ->
    ("♮" | "nat")                {% _ => Interval(1) %}
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

# -----------------------------------------------
# FJS and Neutral FJS interval and note symbols
# @returns {(Interval|Array)}

genFJSSymb[BASE,REC] ->
    $BASE             {% d => _ => d[0][0] %}
  | $REC "^" fjsAccs  {% d => params => ["mul", d[0][0], d[2](params)] %}
  | $REC "_" fjsAccs  {% d => params => ["div", d[0][0], d[2](params)] %}

fjsIntv -> genFJSSymb[pyIntv,fjsIntv]  {% d => d[0](fjsParams) %}
fjsNote -> genFJSSymb[pyNote,fjsNote]  {% d => d[0](fjsParams) %}

# be warned that these (specifically nfjsNonNeutIntv and nfjsNonNeutNote) will
#  give different answers than the above on the same input!
nfjsIntv -> nfjsNeutIntv {% id %} | nfjsNonNeutIntv {% id %}
nfjsNote -> nfjsNeutNote {% id %} | nfjsNonNeutNote {% id %}

nfjsNeutIntv ->
  genFJSSymb[npyIntv,nfjsNeutIntv]    {% d => d[0](nfjsParams) %}
nfjsNonNeutIntv ->
  genFJSSymb[pyIntv,nfjsNonNeutIntv]  {% d => d[0](nfjsParams) %}
nfjsNeutNote ->
  genFJSSymb[npyNote,nfjsNeutNote]    {% d => d[0](nfjsParams) %}
nfjsNonNeutNote ->
  genFJSSymb[pyNote,nfjsNonNeutNote]  {% d => d[0](nfjsParams) %}

fjsAccs ->
    fjsAccOk              {% d => params => ["!fjsFactor", d[0], params] %}
  | fjsAccs "," fjsAccOk  {% d => params => ["mul", d[0](params), ["!fjsFactor", d[2], params]] %}

fjsAccOk -> fjsAcc {% (d,loc,_) => ["!ensureNo2Or3", d[0], loc] %}

fjsAcc ->
    posInt                        {% d => Interval(d[0]) %}
  | "sqrt(" fjsAcc ")"            {% d => d[1].sqrt() %}
  | "root" posInt "(" fjsAcc ")"  {% d => d[3].root(d[1]) %}
  | "(" fjsAcc "^" frcExpr3 ")"   {% d => d[1].pow(d[3]) %}

# --------------------------------------------------
# Ups-and-downs notation interval and note symbols
# @returns {(Interval|Array)}

upsDnsIntv ->
    upsDns pyIntv    {% (d,loc,_) => ["+", d[0], ["!edoPy", d[1], loc]] %}
  | upsDns npyIntv   {% (d,loc,_) => ["+", d[0], ["!edoPy", d[1], loc]] %}
  | upsDns snpyIntv  {% (d,loc,_) => ["+", d[0], ["!edoPy", d[1], loc]] %}
  # alternate notation for up/down perfect intervals
  | upsDns posInt
    {% (d,loc,reject) => (pyRedDeg(d[1]) == 4 || pyRedDeg(d[1]) == 5) && d[0] != 0
                         ? ["+", d[0], ["!edoPy", parseInt(d[1]), loc]] : reject %}
  # alternate notation for neutal intervals, semi-augmented fourths, and
  # semi-diminished fifths
  | upsDns "~" posInt
    {% (d,loc,reject) => pyRedDeg(d[2]) == 1 ? reject :
                         pyRedDeg(d[2]) == 4 ? ["+", d[0], ["!edoPy", pyInterval(d[2],1,2), loc]] :
                         pyRedDeg(d[2]) == 5 ? ["+", d[0], ["!edoPy", pyInterval(d[2],-1,2), loc]] :
                                               ["+", d[0], ["!edoPy", pyInterval(d[2],0), loc]] %}

upsDnsNote ->
  upsDns pyNote      {% (d,loc,_) => ["+", d[0], ["!edoPy", d[1], loc]] %}
| upsDns npyNote     {% (d,loc,_) => ["+", d[0], ["!edoPy", d[1], loc]] %}

upsDns ->
  null   {% d => 0 %}
| "^":+  {% d => d[0].length %}
| "v":+  {% d => - d[0].length %}

# ------------------------------------------------------
# Terminals
# ------------------------------------------------------

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
