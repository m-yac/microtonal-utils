@{%
const Fraction = require('fraction.js');
const Interval = require('./interval.js');
%}

@builtin "whitespace.ne"

top ->
    _ itvExpr1 _ {% d => ["interval", d[1]] %}
  | _ ctsExpr1 _ {% d => ["cents", d[1]] %}

# Interval expressions

itvExpr1 ->
    itvExpr1 _ "*" _ itvExpr2                       {% d => d[0].mul(d[4]) %}
  | itvExpr1 _ "/" _ itvExpr2                       {% d => d[0].div(d[4]) %}
  | itvExpr2                                        {% id %}
itvExpr2 ->
    itvExpr3 _ "^" _ frcExpr5                       {% d => d[0].pow(d[4]) %}
  | itvExpr3                                        {% id %}
itvExpr3 ->
    "sqrt" _ "(" _ itvExpr1 _ ")"                   {% d => d[4].sqrt() %}
  | "red"  _ "(" _ itvExpr1 _ ")"                   {% d => d[4].red() %}
  | "red"  _ "(" _ itvExpr1 _ "," _ itvExpr1 _ ")"  {% d => d[4].red(d[8]) %}
  | "reb"  _ "(" _ itvExpr1 _ ")"                   {% d => d[4].reb() %}
  | "reb"  _ "(" _ itvExpr1 _ "," _ itvExpr1 _ ")"  {% d => d[4].reb(d[8]) %}
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
    ctsExpr3 _ "x" _ frcExpr5                        {% d => d[0].pow(d[4]) %}
  | frcExpr5 _ "x" _ ctsExpr3                        {% d => d[4].pow(d[0]) %}
  | ctsExpr3                                         {% id %}
ctsExpr3 ->
    "cents" _ "(" _ itvExpr1 _ ")"                   {% d => d[4] %}
  | "red"   _ "(" _ ctsExpr1 _ ")"                   {% d => d[4].red() %}
  | "red"   _ "(" _ ctsExpr1 _ "," _ itvExpr1 _ ")"  {% d => d[4].red(d[8]) %}
  | "reb"   _ "(" _ ctsExpr1 _ ")"                   {% d => d[4].reb() %}
  | "reb"   _ "(" _ ctsExpr1 _ "," _ itvExpr1 _ ")"  {% d => d[4].reb(d[8]) %}
  | ctsExpr4                                         {% id %}
ctsExpr4 ->
    decimal "c"             {% d => Interval(2).pow(Fraction(d[0]).div(1200)) %}
  | "(" _ ctsExpr1 _ ")"                             {% d => d[2] %}

# Fractional expressions

frcExpr1 ->
    frcExpr1 _ "+" _ frcExpr2      {% d => d[0].add(d[4]) %}
  | frcExpr1 _ "-" _ frcExpr2      {% d => d[0].sub(d[4]) %}
  | frcExpr2                       {% id %}
frcExpr2 ->
    frcExpr2 _ "*" _ frcExpr3      {% d => d[0].mul(d[4]) %}
  | frcExpr2 _ "/" _ frcExpr3      {% d => d[0].div(d[4])%}
  | frcExpr3                       {% id %}
frcExpr3 ->
    "-" _ frcExpr4                 {% d => d[2].neg() %}
  | frcExpr4                       {% id %}
frcExpr4 ->
    frcExpr5 _ "^" _ posInt        {% d => d[0].pow(d[4]) %}
  | frcExpr5 _ "^" _ "-" _ posInt  {% d => d[0].pow(-d[6]) %}
  | frcExpr5                       {% id %}
frcExpr5 ->
    posInt                         {% d => Fraction(d[0]) %}
  | "(" _ frcExpr1 _ ")"           {% d => d[2] %}

# Terminals

posInt -> [1-9] [0-9]:* {% d => d[0] + d[1].join("") %}

decimal -> "-":? [0-9]:+ ("." [0-9]:* ("(" [0-9]:+ ")"):?):?
  {% d => (d[0] || "") + d[1].join("")
                       + (d[2] ? "." + d[2][1].join("")
                                     + (d[2][2] ? "("+d[2][2][1].join("")+")"
                                                : "")
                               : "") %}
