const fs = require('fs');
const WIDTH = 1280;
const HEIGHT = 720;

function writeppm (data) {
  var ppm = fs.openSync('jsrb.ppm', 'w');
  
  ppm.write(`P3\n${WIDTH} ${HEIGHT}\n255\n`);

  for(
}

(() => {
})()
