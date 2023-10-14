'use strict';

const WIDTH = 1280;
const HEIGHT = 720;
const SAMPLES = 50;
const MAX_DEPTH = 5;

class Vector3 {
  constructor(x, y, z) {
    this.x = x || 0;
    this.y = y || 0;
    this.z = z || 0;
  }

  add(v) {
    return new Vector3(this.x + v.x, this.y + v.y, this.z + v.z);
  }

  sub(v) {
    return new Vector3(this.x - v.x, this.y - v.y, this.z - v.z);
  }

  mulv(v) {
    return new Vector3(this.x * v.x, this.y * v.y, this.z * v.z);
  }

  muls(s) {
    return new Vector3(this.x * s, this.y * s, this.z * s);
  }

  div(s) {
    return new Vector3(this.x / s, this.y / s, this.z / s);
  }

  dot(v) {
    return this.x * v.x + this.y * v.y + this.z * v.z;
  }

  norm() {
    return Math.sqrt(this.dot(this));
  }

  unit() {
    return this.div(this.norm());
  }
}

class Ray {
  constructor(origin, direction) {
    this.origin = origin || new Vector3();
    this.direction = direction || new Vector3();
  }

  point(dist) {
    return this.origin.add(this.direction.muls(dist));
  }
}

class Hit {
  constructor(dist, point, normal) {
    this.dist = dist || 0;
    this.point = point || new Vector3();
    this.normal = normal || new Vector3();
  }
}

class Camera {
  constructor(eye, lt, rt, lb) {
    this.eye = eye || new Vector3(0, 4.5, 75);
    this.lt = lt || new Vector3(-8, 9, 50);
    this.rt = rt || new Vector3(8, 9, 50);
    this.lb = lb || new Vector3(-8, 0, 50);
  }
}

class Sphere {
  constructor(center, radius, color, is_light) {
    this.center = center || new Vector3();
    this.radius = radius || 1;
    this.color = color || new Vector3(1.0, 0, 0);
    this.is_light = is_light || false;
  }

  hit(ray) {
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

class World {
  constructor() {
    this.camera = new Camera();
    this.spheres = [
      new Sphere(new Vector3(0, -10002, 0), 9999, new Vector3(1, 1, 1), false),
      new Sphere(new Vector3(-10012, 0, 0), 9999, new Vector3(1, 0, 0), false),
      new Sphere(new Vector3(10012, 0, 0), 9999, new Vector3(0, 1, 0), false),
      new Sphere(new Vector3(0, 0, -10012), 9999, new Vector3(1, 1, 1), false),
      new Sphere(new Vector3(0, 10012, 0), 9999, new Vector3(1, 1, 1), true),
      new Sphere(new Vector3(-5, 0, 2), 2, new Vector3(1, 1, 0), false),
      new Sphere(new Vector3(0, 5, -1), 4, new Vector3(1, 0, 0), false),
    ];

    this.spheres.push(
      new Sphere(new Vector3(8, 5, -1), 2, new Vector3(0, 0, 1), false)
    );
  }
}

function rand_new() {
  let x = 123456789;
  let y = 362436069;
  let z = 521288629;
  let w = 88675123;
  let t = 0;
  let max = 4294967295;

  return function () {
    t = (x ^ (x << 11)) >>> 0;
    x = y;
    y = z;
    z = w;
    w = (w ^ (w >>> 19) ^ (t ^ (t >>> 8))) >>> 0;
    return w / max;
  };
}

function rnd_dome(randf, normal) {
  let p = new Vector3();
  let d;

  do {
    p.x = 2 * randf() - 1;
    p.y = 2 * randf() - 1;
    p.z = 2 * randf() - 1;

    p = p.unit();
    d = p.dot(normal);
  } while (d < 0);

  return p;
}

function trace(randf, world, ray, depth) {
  let color = new Vector3();
  let did_hit = false;
  let hit = new Hit(1e15);
  let sp;

  world.spheres.forEach((s) => {
    let lh = s.hit(ray);

    if (lh && lh.dist > 0.0001 && lh.dist < hit.dist) {
      sp = s;
      did_hit = true;
      color = s.color;
      hit = lh;
    }
  });

  if (did_hit && depth < MAX_DEPTH) {
    if (sp.is_light != true) {
      let nray = new Ray(hit.point, rnd_dome(randf, hit.normal));

      let ncolor = trace(randf, world, nray, depth + 1);
      let at = nray.direction.dot(hit.normal);

      color = color.mulv(ncolor.muls(at));
    }
  }

  if (did_hit == false || depth >= MAX_DEPTH) {
    color = new Vector3();
  }

  return color;
}

function writeppm(data) {
  process.stdout.write(`P3\n${WIDTH} ${HEIGHT}\n255\n`);

  for (let y = 0; y < HEIGHT; ++y) {
    for (let x = 0; x < WIDTH; ++x) {
      const pixel = data[x + y * WIDTH];
      let r = Math.floor(pixel.x * 255.99);
      let g = Math.floor(pixel.y * 255.99);
      let b = Math.floor(pixel.z * 255.99);
      process.stdout.write(`${r} ${g} ${b} `);
    }
    process.stdout.write('\n');
  }
}

(() => {
  const world = new World();
  const vdu = world.camera.rt.sub(world.camera.lt).div(WIDTH);
  const vdv = world.camera.lb.sub(world.camera.lt).div(HEIGHT);
  const randf = rand_new();
  console.log(randf());
  console.log(randf());
  console.log(randf());
  console.log(randf());
  return;
  const data = [...Array(WIDTH * HEIGHT).keys()].map((pixel) => {
    const x = pixel % WIDTH;
    const y = Math.floor(pixel / WIDTH);

    const color = [...Array(SAMPLES).keys()]
      .map(() => {
        let ray = new Ray();
        ray.origin = world.camera.eye;
        ray.direction = world.camera.lt.add(
          vdu.muls(x + randf()).add(vdv.muls(y + randf()))
        );

        ray.direction = ray.direction.sub(ray.origin).unit();

        return trace(randf, world, ray, 0);
      })
      .reduce((acc, c) => acc.add(c), new Vector3());

    return color.div(SAMPLES);
  });

  writeppm(data);
})();
