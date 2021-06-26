
const _ = require('lodash');
const jsc = require("jsverify");
const Fraction = require('fraction.js');
const Interval = require('../lib/interval.js');
const {colorSymb, colorFromSymb, colorTemperament} = require('../lib/color.js');

const {degree, offset, pyIntv} = require('./arbitrary.js');
require('./interval.test.js'); // only run these tests after interval.test.js
require('./pythagorean.test.js'); // only run these tests after pythagorean.test.js

describe("Color notation", function() {

  // Examples from:
  // https://en.xen.wiki/w/Color_notation#Converting_a_Ratio_to%2Ffrom_a_Color_Name
  it("colorSymb(63,40) == zg6", function () {
    return colorSymb(63,40) == "zg6";
  });
  it("colorSymb(63,40,{verbosity:1}) == zogu 6th", function () {
    return colorSymb(63,40) == "zogu 6th";
  });
  it("colorFromSymb(0, -1, [0,0,-2], 2) == 2048/2025", function () {
    return colorFromSymb(0, -1, [0,0,-2], 2).equals(2048,2025);
  });

  // Example that breaks the second formula of:
  // https://en.xen.wiki/w/Color_notation#Converting_a_Ratio_to%2Ffrom_a_Color_Name
  it("colorFromSymb(0, 0, 5, 1) == 80/81", function () {
    return colorFromSymb(0, 0, 5, 1).equals(80,81);
  });

  // Two examples from:
  // https://en.xen.wiki/w/Color_notation/Temperament_Names
  it("colorTemperament(135,128) == layobi", function () {
    return colorTemperament(135,128) == "layobi";
  });
  it("colorTemperament([24,-21,4]) == sasa-quadyo", function () {
    return colorTemperament([24,-21,4]) == "sasa-quadyo";
  });

});
