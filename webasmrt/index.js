const funs = require('./pkg/webasmtest');
const fs = require('fs');
fs.writeFileSync('webasm.ppm', funs.render());
