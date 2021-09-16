// export everything from `lib/` as well as `Fraction` from fraction.js
Object.assign(module['exports'], require('./utils.js'));
module['exports']['Fraction'] = require('fraction.js');
module['exports']['Interval'] = require('./interval.js');
Object.assign(module['exports'], require('./pythagorean.js'));
Object.assign(module['exports'], require('./fjs.js'));
Object.assign(module['exports'], require('./edo.js'));
Object.assign(module['exports'], require('./color.js'));
Object.assign(module['exports'], require('./sets.js'));
Object.assign(module['exports'], require('./approx.js'));
Object.assign(module['exports'], require('./english.js'));
Object.assign(module['exports'], require('./parser/eval.js'));
Object.assign(module['exports'], require('./parser.js'));
