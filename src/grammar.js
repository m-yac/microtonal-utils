// Generated automatically by nearley, version 2.19.8
// http://github.com/Hardmath123/nearley
(function () {
function id(x) { return x[0]; }


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
    {"name": "top1", "symbols": ["_", "top2", "_"], "postprocess": d => d[1]},
    {"name": "top2", "symbols": ["fjsItv"], "postprocess": d => ["FJS", d[0]]},
    {"name": "top2", "symbols": ["itvExpr1"], "postprocess": d => ["interval", d[0]]},
    {"name": "top2", "symbols": ["ctsExpr1"], "postprocess": d => ["cents", d[0]]},
    {"name": "fjsItv", "symbols": ["pyItv"], "postprocess": id},
    {"name": "fjsItv", "symbols": ["fjsItv", {"literal":"^"}, "fjsAccs"], "postprocess": d => d[0].mul(d[2])},
    {"name": "fjsItv", "symbols": ["fjsItv", {"literal":"_"}, "fjsAccs"], "postprocess": d => d[0].div(d[2])},
    {"name": "fjsAccs", "symbols": ["fjsAcc"], "postprocess": d => fjsFactor(d[0])},
    {"name": "fjsAccs", "symbols": ["fjsAccs", {"literal":","}, "fjsAcc"], "postprocess": d => d[0].mul(fjsFactor(d[2]))},
    {"name": "fjsAcc", "symbols": ["posInt"], "postprocess": (d,_,reject) => ensureNo2Or3(Interval(d[0]),reject)},
    {"name": "fjsAcc$string$1", "symbols": [{"literal":"s"}, {"literal":"q"}, {"literal":"r"}, {"literal":"t"}, {"literal":"("}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "fjsAcc", "symbols": ["fjsAcc$string$1", "fjsAcc", {"literal":")"}], "postprocess": d => d[3].sqrt()},
    {"name": "fjsAcc$string$2", "symbols": [{"literal":"r"}, {"literal":"o"}, {"literal":"o"}, {"literal":"t"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "fjsAcc", "symbols": ["fjsAcc$string$2", "posInt", {"literal":"("}, "fjsAcc", {"literal":")"}], "postprocess": d => d[3].root(d[1])},
    {"name": "fjsAcc", "symbols": [{"literal":"("}, "fjsAcc", {"literal":"^"}, "frcExpr3", {"literal":")"}], "postprocess": d => d[1].pow(d[3])},
    {"name": "pyItv", "symbols": [{"literal":"P"}, "posInt"], "postprocess": (d,_,reject) => perfPyInterval(d[1],0,reject)},
    {"name": "pyItv", "symbols": [{"literal":"M"}, "posInt"], "postprocess": (d,_,reject) => nonPerfPyInterval(d[1],Fraction(1,2),reject)},
    {"name": "pyItv", "symbols": [{"literal":"m"}, "posInt"], "postprocess": (d,_,reject) => nonPerfPyInterval(d[1],Fraction(-1,2),reject)},
    {"name": "pyItv$ebnf$1", "symbols": [{"literal":"A"}]},
    {"name": "pyItv$ebnf$1", "symbols": ["pyItv$ebnf$1", {"literal":"A"}], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "pyItv", "symbols": ["pyItv$ebnf$1", "posInt"], "postprocess": (d,_,reject) => augOrDimPyInterval(d[1],d[0].length,1,reject)},
    {"name": "pyItv$ebnf$2", "symbols": [{"literal":"d"}]},
    {"name": "pyItv$ebnf$2", "symbols": ["pyItv$ebnf$2", {"literal":"d"}], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "pyItv", "symbols": ["pyItv$ebnf$2", "posInt"], "postprocess": (d,_,reject) => augOrDimPyInterval(d[1],-d[0].length,1,reject)},
    {"name": "pyItv", "symbols": ["posInt", {"literal":"A"}, "posInt"], "postprocess": (d,_,reject) => augOrDimPyInterval(d[2],d[0],1,reject)},
    {"name": "pyItv", "symbols": ["posInt", {"literal":"d"}, "posInt"], "postprocess": (d,_,reject) => augOrDimPyInterval(d[2],d[0],1,reject)},
    {"name": "npyItv$subexpression$1", "symbols": [/[nN]/], "postprocess": function(d) {return d.join(""); }},
    {"name": "npyItv", "symbols": ["npyItv$subexpression$1", "posInt"], "postprocess": (d,_,reject) => nonPerfPyInterval(d[1],0,reject)},
    {"name": "npyItv$string$1", "symbols": [{"literal":"s"}, {"literal":"A"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "npyItv", "symbols": ["npyItv$string$1", "posInt"], "postprocess": (d,_,reject) => augOrDimPyInterval(d[1],1,2,reject)},
    {"name": "npyItv$string$2", "symbols": [{"literal":"s"}, {"literal":"d"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "npyItv", "symbols": ["npyItv$string$2", "posInt"], "postprocess": (d,_,reject) => augOrDimPyInterval(d[1],-1,2,reject)},
    {"name": "npyItv$string$3", "symbols": [{"literal":"/"}, {"literal":"2"}, {"literal":"-"}, {"literal":"A"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "npyItv", "symbols": ["posInt", "npyItv$string$3", "posInt"], "postprocess": (d,_,reject) => augOrDimPyInterval(d[2],d[0],2,reject)},
    {"name": "npyItv$string$4", "symbols": [{"literal":"/"}, {"literal":"2"}, {"literal":"-"}, {"literal":"d"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "npyItv", "symbols": ["posInt", "npyItv$string$4", "posInt"], "postprocess": (d,_,reject) => augOrDimPyInterval(d[2],d[0],2,reject)},
    {"name": "snpyItv$string$1", "symbols": [{"literal":"s"}, {"literal":"M"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "snpyItv", "symbols": ["snpyItv$string$1", "posInt"], "postprocess": (d,_,reject) => nonPerfPyInterval(d[1],Fraction(1,4),reject)},
    {"name": "snpyItv$string$2", "symbols": [{"literal":"s"}, {"literal":"m"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "snpyItv", "symbols": ["snpyItv$string$2", "posInt"], "postprocess": (d,_,reject) => nonPerfPyInterval(d[1],Fraction(-1,4),reject)},
    {"name": "snpyItv$string$3", "symbols": [{"literal":"/"}, {"literal":"4"}, {"literal":"-"}, {"literal":"A"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "snpyItv", "symbols": ["posInt", "snpyItv$string$3", "posInt"], "postprocess": (d,_,reject) => augOrDimPyInterval(d[2],d[0],4,reject)},
    {"name": "snpyItv$string$4", "symbols": [{"literal":"/"}, {"literal":"4"}, {"literal":"-"}, {"literal":"d"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "snpyItv", "symbols": ["posInt", "snpyItv$string$4", "posInt"], "postprocess": (d,_,reject) => augOrDimPyInterval(d[2],d[0],4,reject)},
    {"name": "itvExpr1", "symbols": ["itvExpr1", "_", {"literal":"*"}, "_", "itvExpr2"], "postprocess": d => d[0].mul(d[4])},
    {"name": "itvExpr1", "symbols": ["itvExpr1", "_", {"literal":"/"}, "_", "itvExpr2"], "postprocess": d => d[0].div(d[4])},
    {"name": "itvExpr1", "symbols": ["itvExpr2"], "postprocess": id},
    {"name": "itvExpr2", "symbols": ["itvExpr4", "_", {"literal":"^"}, "_", "frcExpr3"], "postprocess": d => d[0].pow(d[4])},
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
    {"name": "itvExpr3", "symbols": ["fjsItv"], "postprocess": id},
    {"name": "itvExpr3", "symbols": ["itvExpr4"], "postprocess": id},
    {"name": "itvExpr4", "symbols": ["posInt"], "postprocess": d => Interval(d[0])},
    {"name": "itvExpr4", "symbols": [{"literal":"("}, "_", "itvExpr1", "_", {"literal":")"}], "postprocess": d => d[2]},
    {"name": "ctsExpr1", "symbols": ["ctsExpr1", "_", {"literal":"+"}, "_", "ctsExpr2"], "postprocess": d => d[0].mul(d[4])},
    {"name": "ctsExpr1", "symbols": ["ctsExpr1", "_", {"literal":"-"}, "_", "ctsExpr2"], "postprocess": d => d[0].div(d[4])},
    {"name": "ctsExpr1", "symbols": ["ctsExpr2"], "postprocess": id},
    {"name": "ctsExpr2", "symbols": ["ctsExpr3", "_", {"literal":"x"}, "_", "frcExpr3"], "postprocess": d => d[0].pow(d[4])},
    {"name": "ctsExpr2", "symbols": ["frcExpr3", "_", {"literal":"x"}, "_", "ctsExpr3"], "postprocess": d => d[4].pow(d[0])},
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
    {"name": "ctsExpr3", "symbols": ["fjsItv"], "postprocess": id},
    {"name": "ctsExpr3", "symbols": ["decimal", {"literal":"c"}], "postprocess": d => Interval(2).pow(Fraction(d[0]).div(1200))},
    {"name": "ctsExpr3", "symbols": ["edoExpr3", "_", {"literal":"\\"}, "_", "posInt"], "postprocess":  (d,_,reject) => d[0](d[4]) == reject ? reject :
        Interval(2).pow(Fraction(d[0](d[4])).div(Fraction(d[4]))) },
    {"name": "ctsExpr3", "symbols": [{"literal":"("}, "_", "ctsExpr1", "_", {"literal":")"}], "postprocess": d => d[2]},
    {"name": "edoExpr1", "symbols": ["edoExpr1", "_", {"literal":"+"}, "_", "edoExpr2"], "postprocess": d => edo => d[0](edo) + d[4](edo)},
    {"name": "edoExpr1", "symbols": ["edoExpr1", "_", {"literal":"-"}, "_", "edoExpr2"], "postprocess": d => edo => d[0](edo) - d[4](edo)},
    {"name": "edoExpr1", "symbols": ["edoExpr2"], "postprocess": id},
    {"name": "edoExpr2", "symbols": ["edoExpr3", "_", {"literal":"x"}, "_", "intExpr1"], "postprocess": d => edo => d[0](edo) * d[4](edo)},
    {"name": "edoExpr2", "symbols": ["intExpr1", "_", {"literal":"x"}, "_", "edoExpr3"], "postprocess": d => edo => d[4](edo) * d[0](edo)},
    {"name": "edoExpr2", "symbols": ["edoExpr3"], "postprocess": id},
    {"name": "edoExpr3", "symbols": ["posInt"], "postprocess": d => _ => parseInt(d[0])},
    {"name": "edoExpr3", "symbols": ["upsDns", "pyItv"], "postprocess": d => edo => d[0] + edoPy(edo,d[1])},
    {"name": "edoExpr3", "symbols": ["upsDns", "npyItv"], "postprocess":  (d,_,reject) => edo =>
        !edoHasNeutrals(edo) ? reject : d[0] + edoPy(edo,d[1]) },
    {"name": "edoExpr3", "symbols": ["upsDns", "snpyItv"], "postprocess":  (d,_,reject) => edo =>
        !edoHasSemiNeutrals(edo) ? reject : d[0] + edoPy(edo,d[1]) },
    {"name": "edoExpr3", "symbols": ["upsDns", {"literal":"~"}, "posInt"], "postprocess":  (d,_,reject) => edo =>
        !edoHasNeutrals(edo) || (d[2]-1)%7 == 0 ? reject :
          (d[2]-1)%7+1 == 4 ? d[0] + edoPy(edo,pyInterval(d[2],1,2)) :
          (d[2]-1)%7+1 == 5 ? d[0] + edoPy(edo,pyInterval(d[2],-1,2)) :
                              d[0] + edoPy(edo,pyInterval(d[2],0)) },
    {"name": "edoExpr3", "symbols": [{"literal":"("}, "_", "edoExpr1", "_", {"literal":")"}], "postprocess": d => d[2]},
    {"name": "upsDns", "symbols": [], "postprocess": d => 0},
    {"name": "upsDns$ebnf$1", "symbols": [{"literal":"^"}]},
    {"name": "upsDns$ebnf$1", "symbols": ["upsDns$ebnf$1", {"literal":"^"}], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "upsDns", "symbols": ["upsDns$ebnf$1"], "postprocess": d => d[0].length},
    {"name": "upsDns$ebnf$2", "symbols": [{"literal":"v"}]},
    {"name": "upsDns$ebnf$2", "symbols": ["upsDns$ebnf$2", {"literal":"v"}], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "upsDns", "symbols": ["upsDns$ebnf$2"], "postprocess": d => d[0].length},
    {"name": "frcExpr1", "symbols": ["frcExpr1", "_", {"literal":"+"}, "_", "frcExpr2"], "postprocess": d => d[0].add(d[4])},
    {"name": "frcExpr1", "symbols": ["frcExpr1", "_", {"literal":"-"}, "_", "frcExpr2"], "postprocess": d => d[0].sub(d[4])},
    {"name": "frcExpr1", "symbols": ["frcExpr2"], "postprocess": id},
    {"name": "frcExpr2", "symbols": ["frcExpr2", "_", {"literal":"*"}, "_", "frcExpr3"], "postprocess": d => d[0].mul(d[4])},
    {"name": "frcExpr2", "symbols": ["frcExpr2", "_", {"literal":"/"}, "_", "frcExpr3"], "postprocess": d => d[0].div(d[4])},
    {"name": "frcExpr2", "symbols": ["frcExpr3"], "postprocess": id},
    {"name": "frcExpr3", "symbols": [{"literal":"-"}, "_", "frcExpr4"], "postprocess": d => d[2].neg()},
    {"name": "frcExpr3", "symbols": ["frcExpr4"], "postprocess": id},
    {"name": "frcExpr4", "symbols": ["frcExpr5", "_", {"literal":"^"}, "_", "intExpr3"], "postprocess": d => d[0].pow(d[4])},
    {"name": "frcExpr4", "symbols": ["frcExpr5"], "postprocess": id},
    {"name": "frcExpr5", "symbols": ["posInt"], "postprocess": d => Fraction(d[0])},
    {"name": "frcExpr5", "symbols": [{"literal":"("}, "_", "frcExpr1", "_", {"literal":")"}], "postprocess": d => d[2]},
    {"name": "intExpr1", "symbols": ["intExpr1", "_", {"literal":"+"}, "_", "intExpr2"], "postprocess": d => d[0] + d[4]},
    {"name": "intExpr1", "symbols": ["intExpr1", "_", {"literal":"-"}, "_", "intExpr2"], "postprocess": d => d[0] - d[4]},
    {"name": "intExpr1", "symbols": ["intExpr2"], "postprocess": id},
    {"name": "intExpr2", "symbols": ["intExpr2", "_", {"literal":"*"}, "_", "intExpr3"], "postprocess": d => d[0] * d[4]},
    {"name": "intExpr2", "symbols": ["intExpr3"], "postprocess": id},
    {"name": "intExpr3", "symbols": [{"literal":"-"}, "_", "intExpr4"], "postprocess": d => - d[2]},
    {"name": "intExpr3", "symbols": ["intExpr4"], "postprocess": id},
    {"name": "intExpr4", "symbols": ["intExpr5", "_", {"literal":"^"}, "_", "posInt"], "postprocess": d => Math.pow(d[0],d[4])},
    {"name": "intExpr4", "symbols": ["intExpr5"], "postprocess": id},
    {"name": "intExpr5", "symbols": ["posInt"], "postprocess": d => parseInt(d[0])},
    {"name": "intExpr5", "symbols": [{"literal":"("}, "_", "intExpr1", "_", {"literal":")"}], "postprocess": d => d[2]},
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
  , ParserStart: "top1"
}
if (typeof module !== 'undefined'&& typeof module.exports !== 'undefined') {
   module.exports = grammar;
} else {
   window.grammar = grammar;
}
})();
