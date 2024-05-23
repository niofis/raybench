'use strict';
const fs = require('fs');
const WIDTH = 1280;
const HEIGHT = 720;
const SAMPLES = 50;
const MAX_DEPTH = 5;

const range = (count) => [...Array(count).keys()];

class Vector3 {
  constructor (x, y, z) {
    this.x = x || 0;
    this.y = y || 0;
    this.z = z || 0;
  }

  add (v) {
    return new Vector3(
      this.x + v.x,
      this.y + v.y,
      this.z + v.z
    );
  }

  sub (v) {
    return new Vector3(
      this.x - v.x,
      this.y - v.y,
      this.z - v.z
    );
  }

  mul (v) {
    let r = new Vector3();
   
    if (typeof(v) == "object") {
        r.x = this.x * v.x;
        r.y = this.y * v.y;
        r.z = this.z * v.z;
    } else if (typeof(v) == "number") {
        r.x = this.x * v;
        r.y = this.y * v;
        r.z = this.z * v;
    }
    
    return r;
  }

  div (v) {
    let r = new Vector3();
   
    if (typeof(v) == "object") {
        r.x = this.x / v.x;
        r.y = this.y / v.y;
        r.z = this.z / v.z;
    } else if (typeof(v) == "number") {
        r.x = this.x / v;
        r.y = this.y / v;
        r.z = this.z / v;
    }
    
    return r;
  }

  dot (v) {
    return  this.x * v.x +
            this.y * v.y +
            this.z * v.z;
  }

  norm () {
    return Math.sqrt(this.dot(this));
  }

  unit () {
    return this.div(this.norm());
  }
}

const nocolor = new Vector3();

class Ray {
  constructor (origin, direction) {
    this.origin = origin || new Vector3();
    this.direction = direction || new Vector3();
  }

  point (dist) {
    return this.origin.add(this.direction.mul(dist));
  }
}

class Hit {
  constructor (dist, point, normal) {
    this.dist = dist || 0;
    this.point = point || new Vector3();
    this.normal = normal || new Vector3();
  }
}
const nohit = new Hit(1e15);

class Camera {
  constructor (eye, lt, rt, lb) {
    this.eye = eye || new Vector3(0, 4.5, 75);
    this.lt = lt || new Vector3(-8, 9, 50);
    this.rt = rt || new Vector3(8, 9, 50);
    this.lb = lb || new Vector3(-8, 0, 50);
  }
}

class Sphere {
  constructor (center, radius, color, is_light) {
    this.center = center || new Vector3();
    this.radius = radius || 1;
    this.color = color || new Vector3(1.0, 0, 0);
    this.is_light = is_light || false;
  }

  hit (ray) {
    let oc = ray.origin.sub(this.center);
    let a = ray.direction.dot(ray.direction);
    let b = oc.dot(ray.direction);
    let c = oc.dot(oc) - this.radius * this.radius;
    let dis = b * b - a * c;

    if (dis > 0) {
      let e = Math.sqrt(dis);

      let t = (-b - e) / a;
      if (t > 0.007) {
        let hit = new Hit();

        hit.dist = t;
        hit.point = ray.point(t);
        hit.normal = hit.point.sub(this.center).unit();

        return hit;
      }

      t = (-b + e) / a;
      if (t > 0.007) {
        let hit = new Hit();

        hit.dist = t;
        hit.point = ray.point(t);
        hit.normal = hit.point.sub(this.center).unit();

        return hit;
      }

      return null;
    }
    
    return null;
  }
}

const spheresHits = (spheres, ray) => {
  const indexes = range(spheres.count);
  const a = ray.direction.dot(ray.direction);

  return indexes.map(i => {
    const ocx = ray.origin.x - spheres.center.x[i];
    const ocy = ray.origin.y - spheres.center.y[i];
    const ocz = ray.origin.z - spheres.center.z[i];

    const b = ocx * ray.direction.x +
              ocy * ray.direction.y + 
              ocz * ray.direction.z;
    const c = (ocx * ocx + ocy * ocy + ocz * ocz) - spheres.radius[i] * spheres.radius[i];
    const dis = b * b - a * c;

    if (dis <= 0) return nohit;

    const e = Math.sqrt(dis);

    const t = (-b - e) / a;
    if (t > 0.007) {
      let hit = new Hit();
      hit.dist = t;
      hit.point = ray.point(t);
      hit.normal = (new Vector3(hit.point.x - spheres.center.x[i],
        hit.point.y - spheres.center.y[i],
        hit.point.z - spheres.center.z[i])).unit();
      hit.index = i;

      return hit;
    }

    const t2 = (-b + e) / a;
    if (t2 > 0.007) {
      let hit = new Hit();
      hit.dist = t2;
      hit.point = ray.point(t2);
      hit.normal = (new Vector3(hit.point.x - spheres.center.x[i],
        hit.point.y - spheres.center.y[i],
        hit.point.z - spheres.center.z[i])).unit();
      hit.index = i;

      return hit;
    }

    return nohit;
  });
}



class World {
  constructor() {
    this.camera = new Camera();
    
    this.spheres = {
      count: 8,
      center: {
        x: [     0,  -10012, 10012,      0,     0, -5,  0,  8],
        y: [-10002,       0,     0,      0, 10012,  0,  5,  5],
        z: [     0,       0,     0, -10002,     0,  2, -1, -1]
      },
      radius: [9999, 9999, 9999, 9999, 9999, 2, 4, 2],
      color: [
        new Vector3(1,1,1),
        new Vector3(1,0,0),
        new Vector3(0,1,0),
        new Vector3(1,1,1),
        new Vector3(1,1,1),
        new Vector3(1,1,0),
        new Vector3(1,0,0),
        new Vector3(0,0,1)
      ],
      is_light: [false, false, false, false, true, false, false, false]
    };
  }
}


function rnd_dome (normal) {
  let p = new Vector3();
  let d;

  do {
    p.x = 2 * Math.random() - 1;
    p.y = 2 * Math.random() - 1;
    p.z = 2 * Math.random() - 1;

    p = p.unit();
    d = p.dot(normal);
  } while (d<0);

  return p;
}

function trace (world, ray, depth) {
  if (depth >= MAX_DEPTH) return nocolor;

  const hits = spheresHits(world.spheres, ray);
  const hit = hits.reduce((prev, next) => prev.dist < next.dist ? prev : next, nohit);

  if (hit === nohit) return nocolor;
  if (world.spheres.is_light[hit.index]) return world.spheres.color[hit.index];
  let nray = new Ray(
    hit.point,
    rnd_dome(hit.normal));

  let ncolor = trace(world, nray, depth + 1);
  let at = nray.direction.dot(hit.normal);

  return world.spheres.color[hit.index].mul(ncolor.mul(at));
}


function writeppm (data) {
  var ppm = fs.openSync('jsrb_dod.ppm', 'w');
  
  fs.writeSync(ppm, `P3\n${WIDTH} ${HEIGHT}\n255\n`);

  for (let y = 0; y < HEIGHT; ++y) {
    for (let x = 0; x < WIDTH; ++x) {
      const pixel = data[x + y*WIDTH];
      let r = Math.floor(pixel.x * 255.99);
      let g = Math.floor(pixel.y * 255.99);
      let b = Math.floor(pixel.z * 255.99);
      fs.writeSync(ppm, `${r} ${g} ${b} `);
    }
    fs.writeSync(ppm, '\n');
  }
  fs.closeSync(ppm);
}

(() => {
  
  let world = new World();
  const vdu = (world.camera.rt.sub(world.camera.lt)).div(WIDTH);
  const vdv = (world.camera.lb.sub(world.camera.lt)).div(HEIGHT);

  const samples = range(SAMPLES);

  const data = range(WIDTH*HEIGHT)
    .map(pixel => {
      const x = pixel % WIDTH;
      const y = Math.floor(pixel / WIDTH);

      const color = samples
        .map(() => {
          const ray = new Ray();
          ray.origin = world.camera.eye;
          ray.direction = world.camera.lt.add(
          vdu.mul(x + Math.random()).add(
            vdv.mul(y + Math.random())));

          ray.direction = ray.direction.sub(ray.origin).unit();

          return trace(world, ray, 0);
        }).reduce((acc, c) => acc.add(c), nocolor);
      
      return color.div(SAMPLES);
    });

  writeppm(data);
})()
