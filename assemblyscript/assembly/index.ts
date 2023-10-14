@external("env", "process.stdout.write")
declare function stdOutWrite(s: string): void

const WIDTH = 1280;
const HEIGHT = 720;
const SAMPLES = 50;
const MAX_DEPTH = 5;

class Vector3 {
  x: f32;
  y: f32;
  z: f32;
  constructor(x: f32 = 0, y: f32 = 0, z: f32 = 0) {
    this.x = x;
    this.y = y;
    this.z = z;
  }

  add(v: Vector3): Vector3 {
    return new Vector3(this.x + v.x, this.y + v.y, this.z + v.z);
  }

  sub(v: Vector3): Vector3 {
    return new Vector3(this.x - v.x, this.y - v.y, this.z - v.z);
  }

  mulv(v: Vector3): Vector3 {
    return new Vector3(this.x * v.x, this.y * v.y, this.z * v.z);
  }

  muls(s: f32): Vector3 {
    return new Vector3(this.x * s, this.y * s, this.z * s);
  }

  div(s: f32): Vector3 {
    return new Vector3(this.x / s, this.y / s, this.z / s);
  }

  dot(v: Vector3): f32 {
    return this.x * v.x + this.y * v.y + this.z * v.z;
  }

  norm(): f32 {
    return Math.sqrt(this.dot(this)) as f32;
  }

  unit(): Vector3 {
    return this.div(this.norm());
  }
}

class Ray {
  origin: Vector3;
  direction: Vector3;
  constructor(origin: Vector3 = new Vector3(), direction: Vector3 = new Vector3()) {
    this.origin = origin;
    this.direction = direction;
  }

  point(dist: f32): Vector3 {
    return this.origin.add(this.direction.muls(dist));
  }
}

class Hit {
  dist: f32;
  point: Vector3;
  normal: Vector3;
  constructor(
    dist: f32 = 0,
    point: Vector3 = new Vector3(),
    normal: Vector3 = new Vector3()
  ) {
    this.dist = dist;
    this.point = point;
    this.normal = normal;
  }
}

class Camera {
  eye: Vector3;
  lt: Vector3;
  rt: Vector3;
  lb: Vector3;
  constructor(
    eye: Vector3 = new Vector3(0, 4.5, 75),
    lt: Vector3 = new Vector3(-8, 9, 50),
    rt: Vector3 = new Vector3(8, 9, 50),
    lb: Vector3 = new Vector3(-8, 0, 50)
  ) {
    this.eye = eye;
    this.lt = lt;
    this.rt = rt;
    this.lb = lb;
  }
}

class Sphere {
  center: Vector3;
  radius: f32;
  color: Vector3;
  is_light: bool;
  constructor(
    center: Vector3 = new Vector3(),
    radius: f32 = 1,
    color: Vector3 = new Vector3(1.0, 0, 0),
    is_light: bool = false
  ) {
    this.center = center;
    this.radius = radius;
    this.color = color;
    this.is_light = is_light;
  }

  hit(ray: Ray): Hit | null {
    let oc = ray.origin.sub(this.center);
    let a = ray.direction.dot(ray.direction);
    let b = oc.dot(ray.direction);
    let c = oc.dot(oc) - this.radius * this.radius;
    let dis = b * b - a * c;

    if (dis > 0) {
      let e = Math.sqrt(dis);

      let t: f32 = ((-b - e) / a) as f32;
      if (t > 0.007) {
        let hit = new Hit();

        hit.dist = t;
        hit.point = ray.point(t);
        hit.normal = hit.point.sub(this.center).unit();

        return hit;
      }

      t = ((-b + e) / a) as f32;
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
  camera: Camera;
  spheres: Array<Sphere>;
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

type randFn = () => f32;
let rX:u32 = 123456789;
let rY:u32 = 362436069;
let rZ:u32 = 521288629;
let rW:u32 = 88675123;
let rT:u32 = 0;
let rMax:u32 = 4294967295;

function rand_new(): randFn {
  rX = 123456789;
  rY = 362436069;
  rZ = 521288629;
  rW = 88675123;
  rT = 0;
  rMax = 4294967295;

  return function () {
    rT = (rX ^ (rX << 11)) >>> 0;
    rX = rY;
    rY = rZ;
    rZ = rW;
    rW = (rW ^ (rW >>> 19) ^ (rT ^ (rT >>> 8))) >>> 0;
    return ((rW as f32) / (rMax as f32)) as f32;
  };
}

function rnd_dome(randf: randFn, normal: Vector3): Vector3 {
  let p = new Vector3();
  let d: f32;

  do {
    p.x = 2 * randf() - 1;
    p.y = 2 * randf() - 1;
    p.z = 2 * randf() - 1;

    p = p.unit();
    d = p.dot(normal);
  } while (d < 0);

  return p;
}

function trace(randf: randFn, world: World, ray: Ray, depth: i32): Vector3 {
  let color = new Vector3();
  let did_hit = false;
  let hit = new Hit(1e15);
  let sp: Sphere | null = null;

  for(let i = 0; i < world.spheres.length; i++) {
    const s = world.spheres[i];
    let lh = s.hit(ray);

    if (lh && lh.dist > 0.0001 && lh.dist < hit.dist) {
      sp = s;
      did_hit = true;
      color = s.color;
      hit = lh;
    }
  }

  if (did_hit && depth < MAX_DEPTH) {
    if (sp && sp.is_light != true) {
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

function writeppm(data: Array<Vector3>): void {
  stdOutWrite(`P3\n${WIDTH} ${HEIGHT}\n255\n`);

  for (let y = 0; y < HEIGHT; ++y) {
    for (let x = 0; x < WIDTH; ++x) {
      const pixel = data[x + y * WIDTH];
      let r = Math.floor(pixel.x * 255.99) as u8;
      let g = Math.floor(pixel.y * 255.99) as u8;
      let b = Math.floor(pixel.z * 255.99) as u8;
      stdOutWrite(`${r} ${g} ${b} `);
    }
    stdOutWrite('\n');
  }
}

export function render(): void {
  const world = new World();
  const vdu = world.camera.rt.sub(world.camera.lt).div(WIDTH as f32);
  const vdv = world.camera.lb.sub(world.camera.lt).div(HEIGHT as f32);
  const randf = rand_new();
  const data: Array<Vector3> = [];

  for(let pixel = 0; pixel < WIDTH * HEIGHT; pixel++) {
    const x = pixel % WIDTH;
    const y = Math.floor(pixel / WIDTH);

    let color = new Vector3();
    for(let sample = 0; sample < SAMPLES; sample++) {
      let ray = new Ray();
      ray.origin = world.camera.eye;
      ray.direction = world.camera.lt.add(
        vdu.muls((x as f32) + randf()).add(vdv.muls((y + randf()) as f32))
      );
      ray.direction = ray.direction.sub(ray.origin).unit();
      color = color.add(trace(randf, world, ray, 0));
    }

    data.push(color.div(SAMPLES as f32));
  };

  writeppm(data);
}
