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
  | decExpr3 hertz                                    {% d => ["div", Interval(d[0]), ["!refHertz"]] %}
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
    anyPyIntv                      {% id %}
  | strictFJSLikeIntv              {% id %}
  | "FJS" _ "(" _ fjsIntv _ ")"    {% d => d[4] %}
  | "NFJS" _ "(" _ nfjsIntv _ ")"  {% d => d[4] %}
  | colorIntv                      {% id %}
  | monzo                          {% id %}
  | "TT"                           {% _ => Interval(2).sqrt() %}

noteSymbol ->
    anyPyNote                      {% id %}
  | strictFJSLikeNote              {% id %}
  | "FJS" _ "(" _ fjsNote _ ")"    {% d => d[4] %}
  | "NFJS" _ "(" _ nfjsNote _ ")"  {% d => d[4] %}
  | colorNote                      {% id %}

monzo ->
    [\[\|] monzoElts [\]>⟩]           {% d => Interval(d[1]) %}
monzoElts ->
    _                                 {% d => [] %}
  | _ frcExpr2 _                      {% d => [d[1]] %}
  | _ monzoEltsCommas _               {% d => d[1] %}
  | _ monzoEltsSpaces _               {% d => d[1] %}
monzoEltsCommas ->
    frcExpr2 _ "," _ frcExpr2         {% d => [d[0], d[4]] %}
  | frcExpr2 _ "," _ monzoEltsCommas  {% d => [d[0]].concat(d[4]) %}
monzoEltsSpaces ->
    frcExpr2 __ frcExpr2              {% d => [d[0], d[2]] %}
  | frcExpr2 __ monzoEltsSpaces       {% d => [d[0]].concat(d[2]) %}

# ------------------------------
# Pythagorean interval symbols
# @returns {(Interval|Array)}

anyPyIntv -> pyIntv {% id %} | npyIntv {% id %} | snpyIntv {% id %}

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

anyPyNote -> pyNote {% id %} | npyNote {% id %}

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
  | "root" posInt "(" fjsAccExpr ")"  {% d => d[3].root(d[1]) %}
  | "(" fjsAccExpr "^" frcExpr3 ")"   {% d => d[1].pow(d[3]) %}

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
  | "v":+  {% d => -d[0].length %}

# --------------------------------------------------
# Color notation interval and note symbols
# @returns {(Interval|Array)}

colorIntv -> aclrIntv {% id %} | clrIntv {% id %}
colorNote -> aclrNote {% id %} | clrNote {% id %}

# abbreviated color notation intervals and notes
aclrIntv ->
    "c":* aclrM aclrP aclrDeg
    {% (d,loc,_) => ["!clrIntv", d[0].length, d[1], d[2], d[3], loc] %}
aclrNote ->
    aclrP pyNote
    {% (d,loc,_) => ["!clrNote", d[0], d[1], loc] %}

# abbreviated color notation magnitudes
aclrM ->
    null   {% d => 0 %}
  | "L":+  {% d => d[0].length %}
  | "s":+  {% d => -d[0].length %}

# abbreviated color prefixes
aclrP ->
    "w"       {% d => [] %}
  | aclrPP:+  {% d => d[0] %}

# abbreviated color prime prefixes
aclrPP ->
    "y"           {% d => Interval(5) %}
  | "g"           {% d => Interval(1,5) %}
  | "z"           {% d => Interval(7) %}
  | "r"           {% d => Interval(1,7) %}
  | posInt "o":+  {% (d,loc,_) => ["!aclrPP", parseInt(d[0]), d[1].length, loc] %}
  | posInt "u":+  {% (d,loc,_) => ["!aclrPP", parseInt(d[0]), -d[1].length, loc] %}

# color notation numeral degrees
aclrDeg ->
    posInt      {% d => parseInt(d[0]) %}
  | "-" posInt  {% d => - parseInt(d[1]) %}

# color notation intervals and notes
clrIntv ->
    clrCos clrM clrP aclrDeg
    {% (d,loc,_) => ["!clrIntv", d[0], d[1], d[2], d[3], loc] %}
  | clrCos clrM clrP clrDeg
    {% (d,loc,_) => ["!clrIntv", d[0], d[1], d[2], d[3], loc] %}
clrNote ->
    clrP pyNote
    {% (d,loc,_) => ["!clrNote", d[0], d[1], loc] %}

# color notation "co"s
clrCos ->
    null               {% d => 0 %}
  | "co":+ "-":?       {% d => d[0].length %}
  | clrMPs "co" "-":?  {% d => d[0] %}

# color notation magnitudes
# N.B. this is more permissive than the hyphen grammar in:
# https://en.xen.wiki/w/Color_notation/Temperament_Names
clrM ->
    null               {% d => 0 %}
  | "la":+ "-":?       {% d => d[0].length %}
  | clrMPs "la" "-":?  {% d => d[0] %}
  | "sa":+ "-":?       {% d => -d[0].length %}
  | clrMPs "sa" "-":?  {% d => ["-", 0, d[0]] %}

# color prefixes
clrP ->
    "wa"    {% d => [] %}
  | "ilo"   {% d => [Interval(11)] %}
  | "iso"   {% d => [Interval(17)] %}
  | "ino"   {% d => [Interval(19)] %}
  | "inu"   {% d => [Interval(1,19)] %}
  | clrPPs  {% id %}

# color prime prefixes
clrPPs ->
    clrPPsMid1:? clrPPsMid3 clrPPsEnd {% d => (d[0] || []).concat(d[1]).concat(d[2]) %}
  | clrPPsEnd               {% id %}
clrPPsEnd ->
    clrPP                   {% d => [d[0]] %}
  | clrPP clrPPsEnd         {% d => [d[0]].concat(d[1]) %}
  | clrMPs clrPPsEnd        {% d => d[1].map(i => ["pow", i, d[0]]) %}
clrPPsMid1 ->
    clrPPsMid1 clrPPsMid2   {% d => d[0].concat(d[1]) %}
  | clrPPsMid2              {% id %}
clrPPsMid2 ->
    clrPP                   {% d => [d[0]] %}
  | clrPPsMid3              {% id %}
clrPPsMid3 ->
    clrMPs clrPPsMid1 "-a"  {% d => d[1].map(i => ["pow", i, d[0]]) %}
clrPP ->
    "yo"                     {% d => Interval(5) %}
  | "gu"                     {% d => Interval(1,5) %}
  | "zo"                     {% d => Interval(7) %}
  | "ru"                     {% d => Interval(1,7) %}
  | "lo"                     {% d => Interval(11) %}
  | "lu"                     {% d => Interval(1,11) %}
  | clrGenPP "o"             {% d => d[0] %}
  | clrGenPP "u"             {% d => ["recip", d[0]] %}

# color notation non-abbreviated degrees
clrDeg ->
    __ "negative" clrPosDeg  {% d => ["*", -1, d[2]] %}
  | __ "-" clrOrdinalDeg     {% d => ["*", -1, d[2]] %}
  | clrPosDeg                {% id %}
clrPosDeg ->
    __ "unison"              {% d => 1 %}
  | __ "octave"              {% d => 8 %}
  | __ clrOrdinalDeg         {% d => d[1] %}
clrOrdinalDeg ->
    "1sn"                    {% d => 1 %}
  | posInt "ve"              {% (d,loc,_) => ["!ordinalOctave", d[0], loc] %}
  | ordinal                  {% d => parseInt(d[0]) %}

# color notation multi prefixes
clrMPs ->
    clrMP:+       {% (d,loc,_) => ["!clrMPs", d[0], loc] %}
clrMP ->
    "bi"          {% d => 2 %}
  | "tri"         {% d => 3 %}
  | "quad"        {% d => 4 %}
  | "quin"        {% d => 5 %}
  | "sep"         {% d => 7 %}
  | "le"          {% d => 11 %}
  | clrGenPP "e"  {% d => ["valueOf", d[0]] %}

# color notation general prime prefixes (13 <= p <= 67)
clrGenPP ->
    clrTens clrOnes  {% (d,loc,_) => ["!clrGenPP", d[0] + d[1], loc] %}

clrTens ->
    null   {% d => 10 %}
  | "twe"  {% d => 20 %}
  | "thi"  {% d => 30 %}
  | "fo"   {% d => 40 %}
  | "fi"   {% d => 50 %}
  | "si"   {% d => 60 %}
clrOnes ->
    "w"   {% d => 1 %}
  | "th"  {% d => 3 %}
  | "s"   {% d => 7 %}
  | "n"   {% d => 9 %}

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
    decimal                    {% d => Fraction(d[0]) %}
  | "(" _ decExpr1 _ ")"       {% d => d[2] %}

locDecExpr3 -> decExpr3 {% (d,loc,_) => [d[0],loc] %}

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
