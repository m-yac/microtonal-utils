#
# Grammar for interval and note expressions
# @copyright 2021 Matthew Yacavone (matthew [at] yacavone [dot] net)
#
# This file generates a parser by running `npm run nearley`, or:
# ```
# nearleyc lib/parser/grammar.ne -o lib/parser/grammar.js
# ```
#
# You can then get nicely formatted output by passing your string to the `parse`
#  or `parseCvt` functions from `parser.js`.
#
# You can also get the raw output of the parser by passing your string and the
#  rule to start from (e.g. "top1") to the `parseFromRule` function from
#  `parser.js`. Then to evaluate these results (i.e. convert the raw output
#  into the Interval it represents), use `evalExpr` from `parser/eval.js`. For
#  example, if you started from "top1":
# ```
# const {parseFromRule} = require('./parser.js');
# const {evalExpr} = require('./parser/eval.js');
# const result = parseFromRule(str, "top1")[0];
# const resultIntv = evalExpr(result[0].expr, result[0].refNote).val;
# ```

@{%

const Fraction = require('fraction.js');
const Interval = require('../interval.js');
const {pyInterval, pyNote, pyRedDeg, baseNoteIntvToA} = require('../pythagorean.js');
const {fjsFactor, fjsSpec, nfjsSpec} = require('../fjs.js');
const {edoPy} = require('../edo.js');
const {ParseError, OtherError, defaultRefNote, evalExpr} = require('./eval.js');

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
    intvSExpr   {% d => ({type: ["interval", "symbol"], expr: d[0][0], symbolType: d[0][1]}) %}
  | intvMExpr0  {% d => ({type: ["interval", "multiplicative"], expr: d[0]}) %}
  | intvAExpr1  {% d => ({type: ["interval", "additive"], expr: d[0]}) %}
  | noteSExpr   {% d => ({type: ["note", "symbol"], expr: d[0][0], symbolType: d[0][1]}) %}
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

intvFns[REC] ->
    "red" _ "(" _ $REC _ ")"                                {% d => ["red", d[4][0]] %}
  | "reb" _ "(" _ $REC _ ")"                                {% d => ["reb", d[4][0]] %}
  | "red" _ "(" _ $REC _ "," _ intvMExpr0 _ ")"             {% d => ["red", d[4][0], d[8]] %}
  | "reb" _ "(" _ $REC _ "," _ intvMExpr0 _ ")"             {% d => ["reb", d[4][0], d[8]] %}
  | "approx"  _ "(" _ $REC _ "," _ posInt _ ")"             {% d => ["!edoApprox", d[4][0], parseInt(d[8])] %}
  | isoUp  _ "(" _ $REC _ ")"                               {% (d,loc,_) => ["!isoUp1", d[4][0], d[0], loc] %}
  | isoDn  _ "(" _ $REC _ ")"                               {% (d,loc,_) => ["!isoDown1", d[4][0], d[0], loc] %}
  | isoMid _ "(" _ $REC _ ")"                               {% d => ["isoMid", d[4][0], Interval(1)] %}
  | isoUp  _ "(" _ $REC _ "," _ $REC _ ")"                  {% (d,loc,_) => ["!isoUp2", d[8][0], d[4][0], d[0], loc] %}
  | isoDn  _ "(" _ $REC _ "," _ $REC _ ")"                  {% (d,loc,_) => ["!isoDown2", d[8][0], d[4][0], d[0], loc] %}
  | isoMid _ "(" _ $REC _ "," _ $REC _ ")"                  {% d => ["isoMid", d[8][0], d[4][0]] %}
  | "iso" _ "(" _ $REC _ "," _ frcExpr1 _ ")"               {% (d,loc,_) => ["!iso", d[4][0], Interval(1), d[8], loc] %}
  | "iso" _ "(" _ $REC _ "," _ $REC _ "," _ frcExpr1 _ ")"  {% (d,loc,_) => ["!iso", d[8][0], d[4][0], d[12], loc] %}

intvMExpr0 ->
    intvMExprIsoExpr                                   {% (d,loc,_) => ["!isoExpr", d[0], loc] %}
  | intvMExpr1                                         {% id %}
intvMExprIsoExpr ->
    intvMExprIsoElt _ ":" _ intvMExprIsoElt            {% d => [d[0], d[4]] %}
  | intvMExprIsoElt _ ":" _ intvMExprIsoExpr           {% d => [d[0]].concat(d[4]) %}
intvMExprIsoElt ->
    intvMExpr1                                         {% (d,loc,_) => [d[0], loc] %}
  | "?"                                                {% (d,loc,_) => [d[0], loc] %}
  | "-"                                                {% (d,loc,_) => [d[0], loc] %}
intvMExpr1 ->
    intvMExpr1 _ "*" _ intvMExpr2                      {% d => ["mul", d[0], d[4]] %}
  | intvMExpr1 _ "/" _ intvMExpr2                      {% d => ["div", d[0], d[4]] %}
  | noteMExpr1 _ "/" _ noteMExpr2                      {% d => ["div", d[0], d[4]] %}
  | intvMExpr2                                         {% id %}
intvMExpr2 ->
    intvMExpr3 _ "^" _ frcExpr3                        {% d => ["pow", d[0], d[4]] %}
  | "sqrt" _ "(" _ intvMExpr0 _ ")"                    {% d => ["sqrt", d[4]] %}
  | "root" posInt _ "(" _ intvMExpr0 _ ")"             {% d => ["root", d[5], parseInt(d[1])] %}
  | "med" _ "(" _ intvMExpr0 _ "," _ intvMExpr0 _ ")"  {% (d,loc,_) => ["!med", d[4], d[8], loc] %}
  | nmed _ "(" _ intvMExpr0 _ "," _ intvMExpr0 _ ")"   {% (d,loc,_) => ["!nobleMed", d[4], d[8], d[0], loc] %}
  | intvFns[intvMExpr0]                                {% id %}
  | intvSymbol                                         {% d => d[0][0] %}
  | intvMExpr3                                         {% id %}
intvMExpr3 ->
    posInt                                             {% d => Interval(d[0]) %}
  | int           _ "\\" _ posInt                      {% d => ["!inEDO", parseInt(d[0]), parseInt(d[4])] %}
  | intvMEDOExpr3 _ "\\" _ posInt                      {% d => ["!inEDO", d[0], parseInt(d[4])] %}
  | "(" _ intvMExpr0 _ ")"                             {% d => d[2] %}

isoUp -> "isoup" | "isoUp"
isoDn -> "isodn" | "isoDn" | "isoDown"
isoMid -> "isomid" | "isoMid"
nmed -> "nmed" | "nMed" | "nobleMed" | "NobleMed"

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
  | noteSymbol                                        {% d => d[0][0] %}
  | noteMEDOExpr2 _ "\\" _ posInt                     {% d => ["!inEDO", d[0], parseInt(d[4])] %}
  | decExpr3 hertz                                    {% (d,loc,_) => ["!hertz", d[0], ["!refHertz"], loc] %}
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
    "cents" _ "(" _ intvMExpr0 _ ")"                   {% d => d[4] %}
  | intvFns[intvAExpr1]                                {% id %}
  | intvSymbol                                         {% d => d[0][0] %}
  | intvAExpr4                                         {% id %}
intvAExpr4 ->
    decExpr3 "c"                                       {% d => ["!cents", d[0]] %}
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
  | noteSymbol                                        {% d => d[0][0] %}
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
  | "TT"                                 {% (d,loc,_) => ["!edoTT", loc] %}
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

intvSExpr ->
    intvFns[intvSExpr0]         {% d => [d[0], "function call"] %}
  | intvSymbol                  {% id %}
  | intvEDOSymb                 {% id %}
  | posInt _ "/" _ posInt       {% d => [Interval(d[0],d[4]), "ratio"] %}
  | decExpr3 "c"                {% d => [["!cents", d[0]], "cents"] %}
  | "(" _ intvSExpr _ ")"       {% d => d[2] %}

intvSExpr0 -> intvSExpr {% d => d[0][0] %}

intvEDOSymb ->
    int           _ "\\" _ posInt  {% d => [["!inEDO", parseInt(d[0]), parseInt(d[4])], "EDO step"] %}
  | upsDnsIntvAbD _ "\\" _ posInt  {% d => [["!inEDO", d[0], parseInt(d[4])], "ups-and-downs"] %}
  | upsDnsIntvVbD _ "\\" _ posInt  {% d => [["!inEDO", d[0], parseInt(d[4])], "ups-and-downs (verbose)"] %}
  | "TT"          _ "\\" _ posInt  {% d => [["!inEDO", ["!edoTT"], parseInt(d[4])], "EDO TT"] %}

# ------------------------
# Note symbol expresions
# @returns {(Interval|Array)}

noteSExpr ->
    "approx"  _ "(" _ noteSExpr _ "," _ posInt _ ")"  {% d => [["!edoApprox", d[4][0], parseInt(d[8])], "function call"] %}
  | noteSymbol                                        {% id %}
  | upsDnsNote _ "\\" _ posInt                        {% d => [["!inEDO", d[0], parseInt(d[4])], "ups-and-downs"] %}
  | decExpr3 hertz                                    {% (d,loc,_) => [["!hertz", d[0], ["!refHertz"], loc], "hertz"] %}
  | "(" _ noteSExpr _ ")"                             {% d => d[2] %}

# ------------------------------------------------------
# Interval and note symbols
# ------------------------------------------------------

# ------------------------------
# Interval and note symbols
# @returns {(Interval|Array)}

intvSymbol ->
    pyIntvD                        {% d => [d[0], "Pythagorean"] %}
  | npyIntvD                       {% d => [d[0], "neutral Pythagorean"] %}
  | snpyIntvD                      {% d => [d[0], "semi-neutral Pythagorean"] %}
  | pyIntvVbD                      {% d => [d[0], "Pythagorean (verbose)"] %}
  | npyIntvVbD                     {% d => [d[0], "neutral Pythagorean (verbose)"] %}
  | snpyIntvVbD                    {% d => [d[0], "semi-neutral Pythagorean (verbose)"] %}
  | strictFJSLikeIntv              {% d => [d[0], "FJS-like"] %}
  | "FJS" _ "(" _ fjsIntv _ ")"    {% d => [d[4], "NFJS"] %}
  | "NFJS" _ "(" _ nfjsIntv _ ")"  {% d => [d[4], "FJS"] %}
  | aclrIntv                       {% d => [d[0], "color"] %}
  | clrIntv                        {% d => [d[0], "color (verbose)"] %}
  | monzo                          {% d => [d[0], "monzo"] %}
  | "TT"                           {% _ => [Interval(2).sqrt(), "TT"] %}
  | phi                            {% _ => [Interval.phi, "phi"] %}

phi -> "phi" | "φ" | "ϕ"

noteSymbol ->
    pyNote                         {% d => [d[0], "Pythagorean"] %}
  | npyNote                        {% d => [d[0], "neutral Pythagorean"] %}
  | strictFJSLikeNote              {% d => [d[0], "FJS-like"] %}
  | "FJS" _ "(" _ fjsNote _ ")"    {% d => [d[4], "NFJS"] %}
  | "NFJS" _ "(" _ nfjsNote _ ")"  {% d => [d[4], "FJS"] %}
  | aclrNote                       {% d => [d[0], "color"] %}
  | clrNote                        {% d => [d[0], "color (verbose)"] %}

monzo ->
    [\[\|] _ monzoElts [\]>⟩]   {% d => Interval(d[2]) %}
monzoElts ->
    null                        {% d => [] %}
  | frcExpr2                    {% d => [d[0]] %}
  | frcExpr2 _ "," _ monzoElts  {% d => [d[0]].concat(d[4]) %}
  | frcExpr2 __ monzoElts       {% d => [d[0]].concat(d[2]) %}

# ------------------------------
# Pythagorean interval symbols
# @returns {(Interval|Array)}

anyPyIntv ->
    pyIntvD      {% id %}
  | npyIntvD     {% id %}
  | snpyIntvD    {% id %}
  | pyIntvVbD    {% id %}
  | npyIntvVbD   {% id %}
  | snpyIntvVbD  {% id %}

pyIntvD ->
    pyIntv       {% id %}
  | desc pyIntv  {% d => ["recip", d[1]] %}

npyIntvD ->
    npyIntv       {% id %}
  | desc npyIntv  {% d => ["recip", d[1]] %}

snpyIntvD ->
    snpyIntv       {% id %}
  | desc snpyIntv  {% d => ["recip", d[1]] %}

pyIntv ->
  # perfect intervals
    "P" degV0 {% (d,loc,_) => ["!perfPyIntv", d[1], loc] %}
  # major and minor intervals
  | "M" degV0 {% (d,loc,_) => ["!nonPerfPyIntv", d[1], Fraction(1,2), "M", loc] %}
  | "m" degV0 {% (d,loc,_) => ["!nonPerfPyIntv", d[1], Fraction(-1,2), "m", loc] %}
  # augmented and diminished intervals
  | "A":+ degV0 {% (d,loc,_) => ["!augOrDimPyIntv", d[1], d[0].length, 1, loc] %}
  | "d":+ degV0 {% (d,loc,_) => ["!augOrDimPyIntv", d[1], -d[0].length, 1, loc] %}
  | posInt "A" degV0 {% (d,loc,_) => ["!augOrDimPyIntv", d[2], parseInt(d[0]), 1, loc] %}
  | posInt "d" degV0 {% (d,loc,_) => ["!augOrDimPyIntv", d[2], -parseInt(d[0]), 1, loc] %}

npyIntv ->
  # neutral intervals
    "n"i degV0 {% (d,loc,_) => ["!nonPerfPyIntv", d[1], 0, "n", loc] %}
  # semi-augmented and semi-diminished intervals
  | "sA" degV0 {% (d,loc,_) => ["!augOrDimPyIntv", d[1], 1, 2, loc] %}
  | "sd" degV0 {% (d,loc,_) => ["!augOrDimPyIntv", d[1], -1, 2, loc] %}
  | posInt "/2-A" degV0 {% (d,loc,_) => ["!augOrDimPyIntv", d[2], parseInt(d[0]), 2, loc] %}
  | posInt "/2-d" degV0 {% (d,loc,_) => ["!augOrDimPyIntv", d[2], -parseInt(d[0]), 2, loc] %}

snpyIntv ->
  # semi-neutral intervals
    "sM" degV0 {% (d,loc,_) => ["!nonPerfPyIntv", d[1], Fraction(1,4), "sM", loc] %}
  | "sm" degV0 {% (d,loc,_) => ["!nonPerfPyIntv", d[1], Fraction(-1,4), "sm", loc] %}
  | posInt "/4-A" degV0 {% (d,loc,_) => ["!augOrDimPyIntv", d[2], parseInt(d[0]), 4, loc] %}
  | posInt "/4-d" degV0 {% (d,loc,_) => ["!augOrDimPyIntv", d[2], -parseInt(d[0]), 4, loc] %}

# verbose Pythagorean intervals

pyIntvVbD ->
    pyIntvVb       {% id %}
  | desc pyIntvVb  {% d => ["recip", d[1]] %}

npyIntvVbD ->
    npyIntvVb       {% id %}
  | desc npyIntvVb  {% d => ["recip", d[1]] %}

snpyIntvVbD ->
    snpyIntvVb       {% id %}
  | desc snpyIntvVb  {% d => ["recip", d[1]] %}

pyIntvVb ->
  # perfect intervals
    pyPrf degV1 {% (d,loc,_) => ["!perfPyIntv", d[1], loc] %}
  # major and minor intervals
  | pyMaj degV1 {% (d,loc,_) => ["!nonPerfPyIntv", d[1], Fraction(1,2), "M", loc] %}
  | pyMin degV1 {% (d,loc,_) => ["!nonPerfPyIntv", d[1], Fraction(-1,2), "m", loc] %}
  # augmented and diminished intervals
  | pyAug degV1 {% (d,loc,_) => ["!augOrDimPyIntv", d[1], 1, 1, loc] %}
  | pyDim degV1 {% (d,loc,_) => ["!augOrDimPyIntv", d[1], -1, 1, loc] %}
  | posInt "-" pyAug degV1 {% (d,loc,_) => ["!augOrDimPyIntv", d[3], parseInt(d[0]), 1, loc] %}
  | posInt "-" pyDim degV1 {% (d,loc,_) => ["!augOrDimPyIntv", d[3], -parseInt(d[0]), 1, loc] %}

npyIntvVb ->
  # neutral intervals
    pyNeut degV1 {% (d,loc,_) => ["!nonPerfPyIntv", d[1], 0, "n", loc] %}
  # semi-augmented and semi-diminished intervals
  | pySemi pyAug degV1 {% (d,loc,_) => ["!augOrDimPyIntv", d[2], 1, 2, loc] %}
  | pySemi pyDim degV1 {% (d,loc,_) => ["!augOrDimPyIntv", d[2], -1, 2, loc] %}
  | posInt "/2-" pyAug degV1 {% (d,loc,_) => ["!augOrDimPyIntv", d[3], parseInt(d[0]), 2, loc] %}
  | posInt "/2-" pyDim degV1 {% (d,loc,_) => ["!augOrDimPyIntv", d[3], -parseInt(d[0]), 2, loc] %}

snpyIntvVb ->
  # semi-neutral intervals
    pySemi pyMaj degV1 {% (d,loc,_) => ["!nonPerfPyIntv", d[2], Fraction(1,4), "sM", loc] %}
  | pySemi pyMin degV1 {% (d,loc,_) => ["!nonPerfPyIntv", d[2], Fraction(-1,4), "sm", loc] %}
  | posInt "/4-" pyAug degV1 {% (d,loc,_) => ["!augOrDimPyIntv", d[2], parseInt(d[0]), 4, loc] %}
  | posInt "/4-" pyDim degV1 {% (d,loc,_) => ["!augOrDimPyIntv", d[2], -parseInt(d[0]), 4, loc] %}

pyPrf -> "p"i "erfect"
pyMaj -> "m"i "aj" "or":?
pyMin -> "m"i "in" "or":?
pyAug -> "a"i "ug" "mented":?
pyDim -> "d"i "im" "inished":?
pyNeut -> "n"i "eutral"
pySemi -> "s"i "emi-"

# --------------------------
# Pythagorean note symbols
# @returns {(Interval|Array)}

anyPyNote ->
    pyNote   {% id %}
  | npyNote  {% id %}

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
# FJS-like interval and note symbols
# @returns {(Interval|Array)}

fjsAccs[REC] ->
    $REC "^" fjsAccList  {% d => spec => ["mul", d[0][0](spec), d[2](spec)] %}
  | $REC "_" fjsAccList  {% d => spec => ["div", d[0][0](spec), d[2](spec)] %}

# FJS interval and note symbols
fjsIntv -> fjsNonNeutIntv  {% d => d[0](fjsSpec) %}
fjsNote -> fjsNonNeutNote  {% d => d[0](fjsSpec) %}

fjsNonNeutIntv ->
    pyIntv                   {% d => _ => d[0] %}
  | fjsAccs[fjsNonNeutIntv]  {% id %}
fjsNonNeutNote ->
    pyNote                   {% d => _ => d[0] %}
  | fjsAccs[fjsNonNeutNote]  {% id %}

# Neutral FJS interval and note symbols
# (be warned that these, specifically nfjsNonNeutIntv and nfjsNonNeutNote, will
#  give different answers than the above on the same input!)
nfjsIntv ->
    nfjsNeutIntv     {% d => d[0](nfjsSpec) %}
  | nfjsNonNeutIntv  {% d => d[0](nfjsSpec) %}
nfjsNote ->
    nfjsNeutNote     {% d => d[0](nfjsSpec) %}
  | nfjsNonNeutNote  {% d => d[0](nfjsSpec) %}

nfjsNeutIntv ->
    npyIntv                   {% d => _ => d[0] %}
  | fjsAccs[nfjsNeutIntv]     {% id %}
nfjsNonNeutIntv ->
    pyIntv                    {% d => _ => d[0] %}
  | fjsAccs[nfjsNonNeutIntv]  {% id %}
nfjsNeutNote ->
    npyNote                   {% d => _ => d[0] %}
  | fjsAccs[nfjsNeutNote]     {% id %}
nfjsNonNeutNote ->
    pyNote                    {% d => _ => d[0] %}
  | fjsAccs[nfjsNonNeutNote]  {% id %}

# General FJS-like interval and note symbols
# - these use whatever FJS-like specs are passed to `eval`, which may be or may
#   not be the same as the above)
# - these may error in the case of un-accented neutral/semi-neutral Pythagorean
#   intervals depending on whether the specs passed to `eval` support them
fjsLikeIntv ->
    fjsLikeSemiNeutIntv  {% (d,loc,_) => ["!fjsSNPy", d[0], loc] %}
  | fjsLikeNeutIntv      {% (d,loc,_) => ["!fjsNPy", d[0], loc] %}
  | fjsLikeNonNeutIntv   {% (d,loc,_) => ["!fjsPy", d[0], loc] %}
fjsLikeNote ->
    fjsLikeNeutNote      {% (d,loc,_) => ["!fjsNPy", d[0], loc] %}
  | fjsLikeNonNeutNote   {% (d,loc,_) => ["!fjsPy", d[0], loc] %}

# General FJS-like symbols with at least one accidental
strictFJSLikeIntv ->
    fjsAccs[fjsLikeSemiNeutIntv]  {% (d,loc,_) => ["!fjsSNPy", d[0], loc] %}
  | fjsAccs[fjsLikeNeutIntv]      {% (d,loc,_) => ["!fjsNPy", d[0], loc] %}
  | fjsAccs[fjsLikeNonNeutIntv]   {% (d,loc,_) => ["!fjsPy", d[0], loc] %}
strictFJSLikeNote ->
    fjsAccs[fjsLikeNeutNote]      {% (d,loc,_) => ["!fjsNPy", d[0], loc] %}
  | fjsAccs[fjsLikeNonNeutNote]   {% (d,loc,_) => ["!fjsPy", d[0], loc] %}

fjsLikeSemiNeutIntv ->
    snpyIntv                      {% d => _ => d[0] %}
  | fjsAccs[fjsLikeSemiNeutIntv]  {% id %}
fjsLikeNeutIntv ->
    npyIntv                       {% d => _ => d[0] %}
  | fjsAccs[fjsLikeNeutIntv]      {% id %}
fjsLikeNonNeutIntv ->
    pyIntv                        {% d => _ => d[0] %}
  | fjsAccs[fjsLikeNonNeutIntv]   {% id %}
fjsLikeNeutNote ->
    npyNote                       {% d => _ => d[0] %}
  | fjsAccs[fjsLikeNeutNote]      {% id %}
fjsLikeNonNeutNote ->
    pyNote                        {% d => _ => d[0] %}
  | fjsAccs[fjsLikeNonNeutNote]   {% id %}

# FJS accidentals

fjsAccList ->
    fjsAcc                 {% d => spec => ["!fjsFactor", d[0], spec] %}
  | fjsAccList "," fjsAcc  {% d => spec => ["mul", d[0](spec), ["!fjsFactor", d[2], spec]] %}

fjsAcc -> fjsAccExpr {% (d,loc,_) => ["!ensureNo2Or3", d[0], loc] %}

fjsAccExpr ->
    posInt                            {% d => Interval(d[0]) %}
  | "sqrt(" fjsAccExpr ")"            {% d => d[1].sqrt() %}
  | "root" posInt "(" fjsAccExpr ")"  {% d => d[3].root(parseInt(d[1])) %}
  | "(" fjsAccExpr "^" frcExpr3 ")"   {% d => d[1].pow(d[3]) %}

# --------------------------------------------------
# Ups-and-downs notation interval and note symbols
# @returns {(Interval|Array)}

upsDnsIntv ->
    upsDnsIntvAbD  {% id %}
  | upsDnsIntvVbD  {% id %}

upsDnsIntvAbD ->
    upsDnsIntvAb       {% id %}
  | desc upsDnsIntvAb  {% d => ["-", 0, d[1]] %}

upsDnsIntvAb ->
    upsDns pyIntv     {% (d,loc,_) => ["!updnsSymb", d[0], d[1], loc] %}
  | upsDns npyIntv    {% (d,loc,_) => ["!updnsSymb", d[0], d[1], loc] %}
  | upsDns snpyIntv   {% (d,loc,_) => ["!updnsSymb", d[0], d[1], loc] %}
  # alternate notation for up/down perfect intervals
  | upsDnsNz degV0    {% (d,loc,_) => ["!updnsPerfSymb", d[0], d[1], loc] %}
  # alternate notation for neutal intervals, semi-augmented fourths, and
  # semi-diminished fifths
  | upsDns "~" degV0  {% (d,loc,) => ["!updnsNeutSymb", d[0], d[2], loc] %}

upsDnsIntvVbD ->
    upsDnsIntvVb       {% id %}
  | desc upsDnsIntvVb  {% d => ["-", 0, d[1]] %}

upsDnsIntvVb ->
    upsDnsVb upDnsVbSep pyIntvVb     {% (d,loc,_) => ["!updnsSymb", d[0], d[2], loc] %}
  | upsDnsVb upDnsVbSep npyIntvVb    {% (d,loc,_) => ["!updnsSymb", d[0], d[2], loc] %}
  | upsDnsVb upDnsVbSep snpyIntvVb   {% (d,loc,_) => ["!updnsSymb", d[0], d[2], loc] %}
  # alternate notation for up/down perfect intervals
  | upsDnsVb degV1                   {% (d,loc,_) => ["!updnsPerfSymb", d[0], d[1], loc] %}
  # alternate notation for neutal intervals, semi-augmented fourths, and
  # semi-diminished fifths
  | upsDnsVb upDnsVbSep "mid" degV1  {% (d,loc,) => ["!updnsNeutSymb", d[0], d[3], loc] %}

upsDnsNote ->
    upsDns pyNote   {% (d,loc,_) => ["!updnsNote", d[0], d[1], loc] %}
  | upsDns npyNote  {% (d,loc,_) => ["!updnsNote", d[0], d[1], loc] %}

upsDns ->
    null   {% d => 0 %}
  | "^":+  {% d => d[0].length %}
  | "v":+  {% d => -d[0].length %}

upsDnsNz ->
    "^":+  {% d => d[0].length %}
  | "v":+  {% d => -d[0].length %}

upsDnsVb ->
    null            {% d => 0 %}
  | "up"            {% d => 1 %}
  | "down"          {% d => -1 %}
  | "double-up"     {% d => 2 %}
  | "double-down"   {% d => -2 %}
  | posInt "-up"    {% d => parseInt(d[0]) %}
  | posInt "-down"  {% d => -parseInt(d[0]) %}

upDnsVbSep -> _ | _ "-" _

# --------------------------------------------------
# Color notation interval and note symbols
# @returns {(Interval|Array)}

anyClrIntv ->
    aclrIntv  {% id %}
  | clrIntv   {% id %}

anyClrNote ->
    aclrNote  {% id %}
  | clrNote   {% id %}

# abbreviated color notation intervals and notes
aclrIntv ->
    aclrCos aclrM aclrP degV0
    {% (d,loc,_) => ["!clrIntv", d[0], d[1], d[2], d[3], loc] %}
  | desc aclrCos aclrM aclrP degV0
    {% (d,loc,_) => ["recip", ["!clrIntv", d[1], d[2], d[3], d[4], loc]] %}
aclrNote ->
    aclrP pyNote
    {% (d,loc,_) => ["!clrNote", d[0], d[1], loc] %}

# abbreviated color notation "co"s
aclrCos ->
    null              {% d => 0 %}
  | "c":+             {% d => d[0].length %}
  | "c^" [1-9]        {% d => parseInt(d[1]) %}
  | "c^(" posInt ")"  {% d => parseInt(d[1]) %}

# abbreviated color notation magnitudes
aclrM ->
    null              {% d => 0 %}
  | "L":+             {% d => d[0].length %}
  | "L^" [1-9]        {% d => parseInt(d[1]) %}
  | "L^(" posInt ")"  {% d => parseInt(d[1]) %}
  | "s":+             {% d => -d[0].length %}
  | "s^" [1-9]        {% d => -parseInt(d[1]) %}
  | "s^(" posInt ")"  {% d => -parseInt(d[1]) %}

# abbreviated color prefixes
aclrP ->
    "w"        {% d => [] %}
  | aclrPP1:+  {% d => d[0].flat(1) %}
aclrPP1 ->
    aclrPP                  {% d => [d[0]] %}
  | aclrPP "^" [1-9]        {% d => Array(parseInt(d[2])).fill(d[0]) %}
  | aclrPP "^(" posInt ")"  {% d => Array(parseInt(d[2])).fill(d[0]) %}

# abbreviated color prime prefixes
aclrPP ->
    "y"           {% d => Interval(5) %}
  | "g"           {% d => Interval(1,5) %}
  | "z"           {% d => Interval(7) %}
  | "r"           {% d => Interval(1,7) %}
  | posInt "o":+  {% (d,loc,_) => ["!aclrPP", parseInt(d[0]), d[1].length, loc] %}
  | posInt "u":+  {% (d,loc,_) => ["!aclrPP", parseInt(d[0]), -d[1].length, loc] %}

# color notation intervals and notes
# N.B. we are much more permissive with hyphens than the grammar in:
# https://en.xen.wiki/w/Color_notation/Temperament_Names
clrIntv ->
    clrCos clrM clrP degV1
    {% (d,loc,_) => ["!clrIntv", d[0], d[1], d[2], d[3], loc] %}
  | desc clrCos clrM clrP degV1
    {% (d,loc,_) => ["recip", ["!clrIntv", d[1], d[2], d[3], d[4], loc]] %}
clrNote ->
    clrP _ pyNote
    {% (d,loc,_) => ["!clrNote", d[0], d[2], loc] %}

# color notation "co"s
clrCos ->
    null                          {% d => 0 %}
  | "c"i "o" "-":? clrCos         {% d => ["+", d[3], 1] %}
  | clrMPs "c"i "o" "-":? clrCos  {% d => ["+", d[4], d[0]] %}

# color notation magnitudes
clrM ->
    null                        {% d => 0 %}
  | "l"i "a" "-":? clrM         {% d => ["+", d[3], 1] %}
  | clrMPs "l"i "a" "-":? clrM  {% d => ["+", d[4], d[0]] %}
  | "s"i "a" "-":? clrM         {% d => ["-", d[3], 1] %}
  | clrMPs "s"i "a" "-":? clrM  {% d => ["-", d[4], d[0]] %}

# color prefixes
clrP ->
    "w"i "a"   {% d => [] %}
  | "i"i "lo"  {% d => [Interval(11)] %}
  | "i"i "so"  {% d => [Interval(17)] %}
  | "i"i "no"  {% d => [Interval(19)] %}
  | "i"i "nu"  {% d => [Interval(1,19)] %}
  | clrPPs     {% id %}

# color prime prefixes
clrPPs ->
    clrPPsMid1:? clrPPsMid3 clrPPsEnd  {% d => (d[0] || []).concat(d[1]).concat(d[2]) %}
  | clrPPsEnd                          {% id %}
clrPPsEnd ->
    clrPP                              {% d => [d[0]] %}
  | clrPP clrPPsEnd                    {% d => [d[0]].concat(d[1]) %}
  | clrMPs clrPPsEnd "-a":?            {% d => d[1].map(i => ["pow", i, d[0]]) %}
clrPPsMid1 ->
    clrPPsMid1 clrPPsMid2              {% d => d[0].concat(d[1]) %}
  | clrPPsMid2                         {% id %}
clrPPsMid2 ->
    clrPP                              {% d => [d[0]] %}
  | clrPPsMid3                         {% id %}
clrPPsMid3 ->
    clrMPs clrPPsMid1 "-" "a":?        {% d => d[1].map(i => ["pow", i, d[0]]) %}
clrPP ->
    "y"i "o"                           {% d => Interval(5) %}
  | "g"i "u"                           {% d => Interval(1,5) %}
  | "z"i "o"                           {% d => Interval(7) %}
  | "r"i "u"                           {% d => Interval(1,7) %}
  | "l"i "o"                           {% d => Interval(11) %}
  | "l"i "u"                           {% d => Interval(1,11) %}
  | clrGenPP "o"                       {% d => d[0] %}
  | clrGenPP "u"                       {% d => ["recip", d[0]] %}

# color notation multi prefixes
clrMPs ->
    clrMP:+       {% (d,loc,_) => ["!clrMPs", d[0], loc] %}
clrMP ->
    "b"i "i"      {% d => 2 %}
  | "t"i "ri"     {% d => 3 %}
  | "q"i "uad"    {% d => 4 %}
  | "q"i "uin"    {% d => 5 %}
  | "s"i "ep"     {% d => 7 %}
  | "l"i "e"      {% d => 11 %}
  | clrGenPP "e"  {% d => ["valueOf", d[0]] %}

# color notation general prime prefixes (13 <= p <= 67)
clrGenPP ->
    clrTens clrOnes  {% (d,loc,_) => ["!clrGenPP", d[0] + d[1], loc] %}

clrTens ->
    null       {% d => 10 %}
  | "t"i "we"  {% d => 20 %}
  | "t"i "hi"  {% d => 30 %}
  | "f"i "o"   {% d => 40 %}
  | "f"i "i"   {% d => 50 %}
  | "s"i "i"   {% d => 60 %}
clrOnes ->
    "w"i      {% d => 1 %}
  | "t"i "h"  {% d => 3 %}
  | "s"i      {% d => 7 %}
  | "n"i      {% d => 9 %}

# ------------------------------------------------------
# Terminals
# ------------------------------------------------------

# ------------------------------------------------------
# Degrees for Pythagorean, color, ups-and-downs intvs
# type: integer

degV0 ->
    posInt      {% d => parseInt(d[0]) %}
  | "-" posInt  {% d => - parseInt(d[1]) %}

degV1 ->
    __ "negative" __ degV1Pos  {% d => -d[3] %}
  | _ "-" _ degOrdinal         {% d => -d[3] %}
  | __ degWord                 {% d => d[1] %}
  | _ degOrdinal               {% d => d[1] %}
  | _ posInt                   {% d => parseInt(d[1]) %}
  | _ "-" _ posInt             {% d => - parseInt(d[3]) %}
degV1Pos ->
    degWord                    {% id %}
  | degOrdinal                 {% id %}
degWord ->
    "unison"                   {% d => 1 %}
  | "octave"                   {% d => 8 %}
degOrdinal ->
    "1sn"                      {% d => 1 %}
  | "8ve"                      {% d => 8 %}
  | ordinal                    {% d => parseInt(d[0]) %}

desc -> "desc." __ | "descending" __

# ------------------------------------------------------
# Fractional expressions (positive, negative, or zero)
# type: Fraction

frcExpr1 ->
    frcExpr1 _ "+" _ frcExpr2  {% d => d[0].add(d[4]) %}
  | frcExpr1 _ "-" _ frcExpr2  {% d => d[0].sub(d[4]) %}
  | frcExpr2                   {% id %}
frcExpr2 ->
    frcExpr2 _ "*" _ frcExpr3  {% d => d[0].mul(d[4]) %}
  | frcExpr2 _ "/" _ locFrcExpr3
    {% function(d) { try { return d[0].div(d[4][0]); }
                     catch(err) {
                       throw new OtherError("Division by zero", d[4][1])
                     } } %}
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

locFrcExpr3 -> frcExpr3        {% (d,loc,_) => [d[0],loc] %}

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

# ------------------------------------------------------
# Decimal expressions
# type: Fraction

decExpr1 ->
    decExpr1 _ "+" _ decExpr2  {% d => d[0].add(d[4]) %}
  | decExpr1 _ "-" _ decExpr2  {% d => d[0].sub(d[4]) %}
  | decExpr2                   {% id %}
decExpr2 ->
    decExpr2 _ "*" _ decExpr3  {% d => d[0].mul(d[4]) %}
  | decExpr2 _ "/" _ locDecExpr3
    {% function(d) { try { return d[0].div(d[4][0]); }
                     catch(err) {
                       throw new OtherError("Division by zero", d[4][1])
                     } } %}
  | decExpr3                   {% id %}
decExpr3 ->
    "-" _ decExpr4             {% d => d[2].neg() %}
  | decExpr4                   {% id %}
decExpr4 ->
    decimal                    {% d => Fraction(d[0]) %}
  | "(" _ decExpr1 _ ")"       {% d => d[2] %}

locDecExpr3 -> decExpr3 {% (d,loc,_) => [d[0],loc] %}

# -----------
# Terminals
# type: String

posInt -> [1-9] [0-9]:* {% d => d[0] + d[1].join("") %}

nonNegInt -> "0" {% _ => "0" %} | posInt {% id %}

int -> "-":? nonNegInt {% d => (d[0] || "") + d[1] %}

decimal -> [0-9]:+ ("." [0-9]:* ("(" [0-9]:+ ")"):?):?
  {% d => d[0].join("") + (d[1] ? "." + d[1][1].join("")
                                      + (d[1][2] ? "("+d[1][2][1].join("")+")"
                                                 : "")
                                : "") %}

hertz -> "hz" | "Hz"

# From: https://en.wikipedia.org/wiki/English_numerals#Ordinal_numbers
# - If the tens digit of a number is 1, then "th" is written after the number
# - If the tens digit is not equal to 1, then the following table could be used:
#   | If the units digit is:            | 0  | 1  | 2  | 3  | 4-9 |
#   | This is written after the number: | th | st | nd | rd | th  |
ordinal ->
    "1st"                            {% d => "1" %}
  | "2nd"                            {% d => "2" %}
  | "3rd"                            {% d => "3" %}
  | [4-9] "th"                       {% d => d[0] %}
  | posInt:? "1" [0-9] "th"          {% d => (d[0] || "") + "1" + d[2] %}
  | posInt:? [2-9] ordinalOnesDigit  {% d => (d[0] || "") + d[1] + d[2] %}
  | posInt "0" ordinalOnesDigit      {% d => d[0] + "0" + d[2] %}
ordinalOnesDigit ->
    "0th"       {% d => "0" %}
  | "1st"       {% d => "1" %}
  | "2nd"       {% d => "2" %}
  | "3rd"       {% d => "3" %}
  | [4-9] "th"  {% d => d[0] %}
