// Generated automatically by nearley, version 2.19.8
// http://github.com/Hardmath123/nearley
(function () {
function id(x) { return x[0]; }


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

var grammar = {
    Lexer: undefined,
    ParserRules: [
    {"name": "_$ebnf$1", "symbols": []},
    {"name": "_$ebnf$1", "symbols": ["_$ebnf$1", "wschar"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "_", "symbols": ["_$ebnf$1"], "postprocess": function(d) {return null;}},
    {"name": "__$ebnf$1", "symbols": ["wschar"]},
    {"name": "__$ebnf$1", "symbols": ["__$ebnf$1", "wschar"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "__", "symbols": ["__$ebnf$1"], "postprocess": function(d) {return null;}},
    {"name": "wschar", "symbols": [/[ \t\n\v\f]/], "postprocess": id},
    {"name": "top", "symbols": ["_", "itvExpr1", "_"], "postprocess": d => ["interval", d[1]]},
    {"name": "top", "symbols": ["_", "ctsExpr1", "_"], "postprocess": d => ["cents", d[1]]},
    {"name": "pyItv", "symbols": [{"literal":"P"}, "posInt"], "postprocess": (d,_,reject) => perfPyInterval(d[1],0,reject)},
    {"name": "pyItv$subexpression$1", "symbols": [/[nN]/], "postprocess": function(d) {return d.join(""); }},
    {"name": "pyItv", "symbols": ["pyItv$subexpression$1", "posInt"], "postprocess": (d,_,reject) => nonPerfPyInterval(d[1],0,reject)},
    {"name": "pyItv$string$1", "symbols": [{"literal":"s"}, {"literal":"M"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "pyItv", "symbols": ["pyItv$string$1", "posInt"], "postprocess": (d,_,reject) => nonPerfPyInterval(d[1],Fraction(1,4),reject)},
    {"name": "pyItv", "symbols": [{"literal":"M"}, "posInt"], "postprocess": (d,_,reject) => nonPerfPyInterval(d[1],Fraction(1,2),reject)},
    {"name": "pyItv$string$2", "symbols": [{"literal":"s"}, {"literal":"m"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "pyItv", "symbols": ["pyItv$string$2", "posInt"], "postprocess": (d,_,reject) => nonPerfPyInterval(d[1],Fraction(-1,4),reject)},
    {"name": "pyItv", "symbols": [{"literal":"m"}, "posInt"], "postprocess": (d,_,reject) => nonPerfPyInterval(d[1],Fraction(-1,2),reject)},
    {"name": "pyItv$string$3", "symbols": [{"literal":"s"}, {"literal":"A"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "pyItv", "symbols": ["pyItv$string$3", "posInt"], "postprocess": (d,_,reject) => augOrDimPyInterval(d[1],1,2,reject)},
    {"name": "pyItv$string$4", "symbols": [{"literal":"s"}, {"literal":"d"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "pyItv", "symbols": ["pyItv$string$4", "posInt"], "postprocess": (d,_,reject) => augOrDimPyInterval(d[1],-1,2,reject)},
    {"name": "pyItv$ebnf$1", "symbols": [{"literal":"A"}]},
    {"name": "pyItv$ebnf$1", "symbols": ["pyItv$ebnf$1", {"literal":"A"}], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "pyItv", "symbols": ["pyItv$ebnf$1", "posInt"], "postprocess": (d,_,reject) => augOrDimPyInterval(d[1],d[0].length,1,reject)},
    {"name": "pyItv$ebnf$2", "symbols": [{"literal":"d"}]},
    {"name": "pyItv$ebnf$2", "symbols": ["pyItv$ebnf$2", {"literal":"d"}], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "pyItv", "symbols": ["pyItv$ebnf$2", "posInt"], "postprocess": (d,_,reject) => augOrDimPyInterval(d[1],-d[0].length,1,reject)},
    {"name": "pyItv", "symbols": ["posInt", {"literal":"A"}, "posInt"], "postprocess": (d,_,reject) => augOrDimPyInterval(d[2],d[0],1,reject)},
    {"name": "pyItv", "symbols": ["posInt", {"literal":"d"}, "posInt"], "postprocess": (d,_,reject) => augOrDimPyInterval(d[2],d[0],1,reject)},
    {"name": "pyItv$string$5", "symbols": [{"literal":"/"}, {"literal":"2"}, {"literal":"-"}, {"literal":"A"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "pyItv", "symbols": ["posInt", "pyItv$string$5", "posInt"], "postprocess": (d,_,reject) => augOrDimPyInterval(d[2],d[0],2,reject)},
    {"name": "pyItv$string$6", "symbols": [{"literal":"/"}, {"literal":"2"}, {"literal":"-"}, {"literal":"d"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "pyItv", "symbols": ["posInt", "pyItv$string$6", "posInt"], "postprocess": (d,_,reject) => augOrDimPyInterval(d[2],d[0],2,reject)},
    {"name": "pyItv$string$7", "symbols": [{"literal":"/"}, {"literal":"4"}, {"literal":"-"}, {"literal":"A"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "pyItv", "symbols": ["posInt", "pyItv$string$7", "posInt"], "postprocess": (d,_,reject) => augOrDimPyInterval(d[2],d[0],4,reject)},
    {"name": "pyItv$string$8", "symbols": [{"literal":"/"}, {"literal":"4"}, {"literal":"-"}, {"literal":"d"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "pyItv", "symbols": ["posInt", "pyItv$string$8", "posInt"], "postprocess": (d,_,reject) => augOrDimPyInterval(d[2],d[0],4,reject)},
    {"name": "itvExpr1", "symbols": ["itvExpr1", "_", {"literal":"*"}, "_", "itvExpr2"], "postprocess": d => d[0].mul(d[4])},
    {"name": "itvExpr1", "symbols": ["itvExpr1", "_", {"literal":"/"}, "_", "itvExpr2"], "postprocess": d => d[0].div(d[4])},
    {"name": "itvExpr1", "symbols": ["itvExpr2"], "postprocess": id},
    {"name": "itvExpr2", "symbols": ["itvExpr4", "_", {"literal":"^"}, "_", "frcExpr5"], "postprocess": d => d[0].pow(d[4])},
    {"name": "itvExpr2$string$1", "symbols": [{"literal":"s"}, {"literal":"q"}, {"literal":"r"}, {"literal":"t"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "itvExpr2", "symbols": ["itvExpr2$string$1", "_", {"literal":"("}, "_", "itvExpr1", "_", {"literal":")"}], "postprocess": d => d[4].sqrt()},
    {"name": "itvExpr2$string$2", "symbols": [{"literal":"r"}, {"literal":"e"}, {"literal":"d"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "itvExpr2", "symbols": ["itvExpr2$string$2", "_", {"literal":"("}, "_", "itvExpr1", "_", {"literal":")"}], "postprocess": d => d[4].red()},
    {"name": "itvExpr2$string$3", "symbols": [{"literal":"r"}, {"literal":"e"}, {"literal":"d"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "itvExpr2", "symbols": ["itvExpr2$string$3", "_", {"literal":"("}, "_", "itvExpr1", "_", {"literal":","}, "_", "itvExpr1", "_", {"literal":")"}], "postprocess": d => d[4].red(d[8])},
    {"name": "itvExpr2$string$4", "symbols": [{"literal":"r"}, {"literal":"e"}, {"literal":"b"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "itvExpr2", "symbols": ["itvExpr2$string$4", "_", {"literal":"("}, "_", "itvExpr1", "_", {"literal":")"}], "postprocess": d => d[4].reb()},
    {"name": "itvExpr2$string$5", "symbols": [{"literal":"r"}, {"literal":"e"}, {"literal":"b"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "itvExpr2", "symbols": ["itvExpr2$string$5", "_", {"literal":"("}, "_", "itvExpr1", "_", {"literal":","}, "_", "itvExpr1", "_", {"literal":")"}], "postprocess": d => d[4].reb(d[8])},
    {"name": "itvExpr2", "symbols": ["itvExpr3"], "postprocess": id},
    {"name": "itvExpr3", "symbols": ["pyItv"], "postprocess": id},
    {"name": "itvExpr3", "symbols": ["itvExpr4"], "postprocess": id},
    {"name": "itvExpr4", "symbols": ["posInt"], "postprocess": d => Interval(d[0])},
    {"name": "itvExpr4", "symbols": [{"literal":"("}, "_", "itvExpr1", "_", {"literal":")"}], "postprocess": d => d[2]},
    {"name": "ctsExpr1", "symbols": ["ctsExpr1", "_", {"literal":"+"}, "_", "ctsExpr2"], "postprocess": d => d[0].mul(d[4])},
    {"name": "ctsExpr1", "symbols": ["ctsExpr1", "_", {"literal":"-"}, "_", "ctsExpr2"], "postprocess": d => d[0].div(d[4])},
    {"name": "ctsExpr1", "symbols": ["ctsExpr2"], "postprocess": id},
    {"name": "ctsExpr2", "symbols": ["ctsExpr3", "_", {"literal":"x"}, "_", "frcExpr5"], "postprocess": d => d[0].pow(d[4])},
    {"name": "ctsExpr2", "symbols": ["frcExpr5", "_", {"literal":"x"}, "_", "ctsExpr3"], "postprocess": d => d[4].pow(d[0])},
    {"name": "ctsExpr2", "symbols": ["ctsExpr3"], "postprocess": id},
    {"name": "ctsExpr3$string$1", "symbols": [{"literal":"c"}, {"literal":"e"}, {"literal":"n"}, {"literal":"t"}, {"literal":"s"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "ctsExpr3", "symbols": ["ctsExpr3$string$1", "_", {"literal":"("}, "_", "itvExpr1", "_", {"literal":")"}], "postprocess": d => d[4]},
    {"name": "ctsExpr3$string$2", "symbols": [{"literal":"r"}, {"literal":"e"}, {"literal":"d"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "ctsExpr3", "symbols": ["ctsExpr3$string$2", "_", {"literal":"("}, "_", "ctsExpr1", "_", {"literal":")"}], "postprocess": d => d[4].red()},
    {"name": "ctsExpr3$string$3", "symbols": [{"literal":"r"}, {"literal":"e"}, {"literal":"d"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "ctsExpr3", "symbols": ["ctsExpr3$string$3", "_", {"literal":"("}, "_", "ctsExpr1", "_", {"literal":","}, "_", "itvExpr1", "_", {"literal":")"}], "postprocess": d => d[4].red(d[8])},
    {"name": "ctsExpr3$string$4", "symbols": [{"literal":"r"}, {"literal":"e"}, {"literal":"b"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "ctsExpr3", "symbols": ["ctsExpr3$string$4", "_", {"literal":"("}, "_", "ctsExpr1", "_", {"literal":")"}], "postprocess": d => d[4].reb()},
    {"name": "ctsExpr3$string$5", "symbols": [{"literal":"r"}, {"literal":"e"}, {"literal":"b"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "ctsExpr3", "symbols": ["ctsExpr3$string$5", "_", {"literal":"("}, "_", "ctsExpr1", "_", {"literal":","}, "_", "itvExpr1", "_", {"literal":")"}], "postprocess": d => d[4].reb(d[8])},
    {"name": "ctsExpr3", "symbols": ["ctsExpr4"], "postprocess": id},
    {"name": "ctsExpr4", "symbols": ["decimal", {"literal":"c"}], "postprocess": d => Interval(2).pow(Fraction(d[0]).div(1200))},
    {"name": "ctsExpr4", "symbols": ["pyItv"], "postprocess": id},
    {"name": "ctsExpr4", "symbols": [{"literal":"("}, "_", "ctsExpr1", "_", {"literal":")"}], "postprocess": d => d[2]},
    {"name": "frcExpr1", "symbols": ["frcExpr1", "_", {"literal":"+"}, "_", "frcExpr2"], "postprocess": d => d[0].add(d[4])},
    {"name": "frcExpr1", "symbols": ["frcExpr1", "_", {"literal":"-"}, "_", "frcExpr2"], "postprocess": d => d[0].sub(d[4])},
    {"name": "frcExpr1", "symbols": ["frcExpr2"], "postprocess": id},
    {"name": "frcExpr2", "symbols": ["frcExpr2", "_", {"literal":"*"}, "_", "frcExpr3"], "postprocess": d => d[0].mul(d[4])},
    {"name": "frcExpr2", "symbols": ["frcExpr2", "_", {"literal":"/"}, "_", "frcExpr3"], "postprocess": d => d[0].div(d[4])},
    {"name": "frcExpr2", "symbols": ["frcExpr3"], "postprocess": id},
    {"name": "frcExpr3", "symbols": [{"literal":"-"}, "_", "frcExpr4"], "postprocess": d => d[2].neg()},
    {"name": "frcExpr3", "symbols": ["frcExpr4"], "postprocess": id},
    {"name": "frcExpr4", "symbols": ["frcExpr5", "_", {"literal":"^"}, "_", "posInt"], "postprocess": d => d[0].pow(d[4])},
    {"name": "frcExpr4", "symbols": ["frcExpr5", "_", {"literal":"^"}, "_", {"literal":"-"}, "_", "posInt"], "postprocess": d => d[0].pow(-d[6])},
    {"name": "frcExpr4", "symbols": ["frcExpr5"], "postprocess": id},
    {"name": "frcExpr5", "symbols": ["posInt"], "postprocess": d => Fraction(d[0])},
    {"name": "frcExpr5", "symbols": [{"literal":"("}, "_", "frcExpr1", "_", {"literal":")"}], "postprocess": d => d[2]},
    {"name": "posInt$ebnf$1", "symbols": []},
    {"name": "posInt$ebnf$1", "symbols": ["posInt$ebnf$1", /[0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "posInt", "symbols": [/[1-9]/, "posInt$ebnf$1"], "postprocess": d => d[0] + d[1].join("")},
    {"name": "decimal$ebnf$1", "symbols": [{"literal":"-"}], "postprocess": id},
    {"name": "decimal$ebnf$1", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "decimal$ebnf$2", "symbols": [/[0-9]/]},
    {"name": "decimal$ebnf$2", "symbols": ["decimal$ebnf$2", /[0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "decimal$ebnf$3$subexpression$1$ebnf$1", "symbols": []},
    {"name": "decimal$ebnf$3$subexpression$1$ebnf$1", "symbols": ["decimal$ebnf$3$subexpression$1$ebnf$1", /[0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "decimal$ebnf$3$subexpression$1$ebnf$2$subexpression$1$ebnf$1", "symbols": [/[0-9]/]},
    {"name": "decimal$ebnf$3$subexpression$1$ebnf$2$subexpression$1$ebnf$1", "symbols": ["decimal$ebnf$3$subexpression$1$ebnf$2$subexpression$1$ebnf$1", /[0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "decimal$ebnf$3$subexpression$1$ebnf$2$subexpression$1", "symbols": [{"literal":"("}, "decimal$ebnf$3$subexpression$1$ebnf$2$subexpression$1$ebnf$1", {"literal":")"}]},
    {"name": "decimal$ebnf$3$subexpression$1$ebnf$2", "symbols": ["decimal$ebnf$3$subexpression$1$ebnf$2$subexpression$1"], "postprocess": id},
    {"name": "decimal$ebnf$3$subexpression$1$ebnf$2", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "decimal$ebnf$3$subexpression$1", "symbols": [{"literal":"."}, "decimal$ebnf$3$subexpression$1$ebnf$1", "decimal$ebnf$3$subexpression$1$ebnf$2"]},
    {"name": "decimal$ebnf$3", "symbols": ["decimal$ebnf$3$subexpression$1"], "postprocess": id},
    {"name": "decimal$ebnf$3", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "decimal", "symbols": ["decimal$ebnf$1", "decimal$ebnf$2", "decimal$ebnf$3"], "postprocess":  d => (d[0] || "") + d[1].join("")
        + (d[2] ? "." + d[2][1].join("")
                      + (d[2][2] ? "("+d[2][2][1].join("")+")"
                                 : "")
                : "") }
]
  , ParserStart: "top"
}
if (typeof module !== 'undefined'&& typeof module.exports !== 'undefined') {
   module.exports = grammar;
} else {
   window.grammar = grammar;
}
})();
