'use strict';
const fs = require('fs');
const WIDTH = 1280;
const HEIGHT = 720;

/*
var va;
var vb;

va.sum(vb)

vec.sum(va,vb)
*/

class Vector3 {
  constructor (x = 0,y = 0,z = 0) {
    this.x = x;
    this.y = y;
    this.z = z;
  }

  add (v) {
    return new Vector3 (
      this.x + v.x,
      this.y + v.y,
      this.z + v.x
    );
  }

  sub (v) {
    return new Vector3 (
      this.x - v.x,
      this.y - v.y,
      this.z - v.z
    );
  }

  
}


function writeppm (data) {
  var ppm = fs.openSync('jsrb.ppm', 'w');
  
  fs.write(ppm, `P3\n${WIDTH} ${HEIGHT}\n255\n`);

  for (let y = 0; y < HEIGHT; ++y) {
    for (let x = 0; x < WIDTH; ++x) {
      fs.writeSync(ppm, '255 0 0 ');
    }
    fs.writeSync(ppm, '\n');
  }
  fs.closeSync(ppm);
}

(() => {
  writeppm();
})()
