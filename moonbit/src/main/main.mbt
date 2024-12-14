///|
const WIDTH = 1280

///|
const HEIGHT = 720

///|
const SAMPLES = 50

///|
const MAXDEPTH = 5

///|
struct Vector {
  x : Float
  y : Float
  z : Float
} derive(Show)

///|
fn op_add(self : Vector, b : Vector) -> Vector {
  { x: self.x + b.x, y: self.y + b.y, z: self.z + b.z }
}

///|
fn op_mul(self : Vector, b : Vector) -> Vector {
  { x: self.x * b.x, y: self.y * b.y, z: self.z * b.z }
}

///|
fn op_sub(self : Vector, b : Vector) -> Vector {
  { x: self.x - b.x, y: self.y - b.y, z: self.z - b.z }
}

///|
fn div(self : Vector, b : Float) -> Vector {
  { x: self.x / b, y: self.y / b, z: self.z / b }
}

///|
fn mul(self : Vector, b : Float) -> Vector {
  { x: self.x * b, y: self.y * b, z: self.z * b }
}

///|
fn dot(self : Vector, b : Vector) -> Float {
  self.x * b.x + self.y * b.y + self.z * b.z
}

///|
fn unit(self : Vector) -> Vector {
  let norm = self.dot(self).sqrt()
  self.div(norm)
}

///|
fn min(a : Int, b : Int) -> Int {
  if a < b {
    a
  } else {
    b
  }
}

///|
fn to_888(self : Vector) -> (Int, Int, Int) {
  (
    min((self.x * 255.99).to_double().to_int(), 255),
    min((self.y * 255.99).to_double().to_int(), 255),
    min((self.z * 255.99).to_double().to_int(), 255),
  )
}

///|
struct Ray {
  origin : Vector
  direction : Vector
} derive(Show)

///|
fn point(self : Ray, distance : Float) -> Vector {
  self.origin + self.direction.mul(distance)
}

///|
struct Rng {
  mut x : UInt
  mut y : UInt
  mut z : UInt
  mut w : UInt
} derive(Show)

///|
fn rand_new() -> Rng {
  { x: 123456789, y: 362436069, z: 521288629, w: 88675123 }
}

///|
fn next(self : Rng) -> Float {
  let max : Float = 4294967295.0
  let t = self.x ^ (self.x << 11)
  self.x = self.y
  self.y = self.z
  self.z = self.w
  self.w = self.w ^ (self.w >> 19) ^ (t ^ (t >> 8))
  self.w.to_float() / max
}

///|
fn dome(self : Rng, normal : Vector) -> Vector {
  for {
    let v : Vector = {
      x: self.next() * 2 - 1,
      y: self.next() * 2 - 1,
      z: self.next() * 2 - 1,
    }.unit()
    if v.dot(normal) >= 0 {
      return v
    }
  }
}

///|
struct Hit {
  distance : Float
  point : Vector
  normal : Vector
} derive(Show)

///|
struct Sphere {
  center : Vector
  radius : Float
  color : Vector
  is_light : Bool
} derive(Show)

///|
fn hit(self : Sphere, ray : Ray) -> Hit? {
  let oc = ray.origin - self.center
  let a = ray.direction.dot(ray.direction)
  let b = oc.dot(ray.direction)
  let c = oc.dot(oc) - self.radius * self.radius
  let dis = b * b - a * c
  if dis > 0 {
    let e = dis.sqrt()
    let distance = (-b - e) / a
    if distance > 0.007 {
      let point = ray.point(distance)
      let normal = (point - self.center).unit()
      return Some({ distance, point, normal })
    }
    let distance = (-b + e) / a
    if distance > 0.007 {
      let point = ray.point(distance)
      let normal = (point - self.center).unit()
      return Some({ distance, point, normal })
    }
  }
  None
}

///|
struct Camera {
  eye : Vector
  left_top : Vector
  right_top : Vector
  left_bottom : Vector
} derive(Show)

///|
struct World {
  camera : Camera
  spheres : Array[Sphere]
  rng : Rng
} derive(Show)

///|
fn world_new(rng : Rng) -> World {
  {
    camera: {
      eye: { x: 0.0, y: 4.5, z: 75.0 },
      left_top: { x: -8.0, y: 9.0, z: 50.0 },
      right_top: { x: 8.0, y: 9.0, z: 50.0 },
      left_bottom: { x: -8.0, y: 0.0, z: 50.0 },
    },
    spheres: [
      {
        center: { x: 0.0, y: -10002.0, z: 0.0 },
        radius: 9999.0,
        color: { x: 1.0, y: 1.0, z: 1.0 },
        is_light: false,
      },
      {
        center: { x: -10012.0, y: 0.0, z: 0.0 },
        radius: 9999.0,
        color: { x: 1.0, y: 0.0, z: 0.0 },
        is_light: false,
      },
      {
        center: { x: 10012.0, y: 0.0, z: 0.0 },
        radius: 9999.0,
        color: { x: 0.0, y: 1.0, z: 0.0 },
        is_light: false,
      },
      {
        center: { x: 0.0, y: 0.0, z: -10012.0 },
        radius: 9999.0,
        color: { x: 1.0, y: 1.0, z: 1.0 },
        is_light: false,
      },
      {
        center: { x: 0.0, y: 10012.0, z: 0.0 },
        radius: 9999.0,
        color: { x: 1.0, y: 1.0, z: 1.0 },
        is_light: true,
      },
      {
        center: { x: -5.0, y: 0.0, z: 2.0 },
        radius: 2.0,
        color: { x: 1.0, y: 1.0, z: 0.0 },
        is_light: false,
      },
      {
        center: { x: 0.0, y: 5.0, z: -1.0 },
        radius: 4.0,
        color: { x: 1.0, y: 0.0, z: 0.0 },
        is_light: false,
      },
      {
        center: { x: 8.0, y: 5.0, z: -1.0 },
        radius: 2.0,
        color: { x: 0.0, y: 0.0, z: 1.0 },
        is_light: false,
      },
    ],
    rng,
  }
}

///|
fn trace(self : World, rng : Rng, ray : Ray, depth : Int) -> Vector {
  if depth == MAXDEPTH {
    return { x: 0, y: 0, z: 0 }
  }
  let mut closest_hit : (Sphere, Hit)? = None
  let hits = self.spheres.map_option(
    fn(sphere) -> (Sphere, Hit)? {
      match sphere.hit(ray) {
        Some(hit) => Some((sphere, hit))
        _ => None
      }
    },
  )
  for candidate_hit in hits {
    match closest_hit {
      Some((_, hit)) =>
        if candidate_hit.1.distance < hit.distance {
          closest_hit = Some(candidate_hit)
        }
      None => closest_hit = Some(candidate_hit)
    }
  }
  match closest_hit {
    None => { x: 0, y: 0, z: 0 }
    Some((sphere, hit)) =>
      if sphere.is_light {
        sphere.color
      } else {
        let nray : Ray = { origin: hit.point, direction: rng.dome(hit.normal) }
        let ncolor = self.trace(rng, nray, depth + 1)
        let at = nray.direction.dot(hit.normal)
        sphere.color * ncolor.mul(at)
      }
  }
}

///|
fn write_ppm(data : Array[Vector]) -> Unit {
  println("P3\n\{WIDTH} \{HEIGHT}\n255")
  for y in 0..<HEIGHT {
    let builder = @builtin.StringBuilder::new()
    for x in 0..<WIDTH {
      let (r, g, b) = data[y * WIDTH + x].to_888()
      builder.write_string("\{r} \{g} \{b} ")
    }
    println(builder.to_string())
  }
}

///|
fn main {
  let rng = rand_new()
  let world = world_new(rng)
  let vdu = (world.camera.right_top - world.camera.left_top).div(
    WIDTH.to_float(),
  )
  let vdv = (world.camera.left_bottom - world.camera.left_top).div(
    HEIGHT.to_float(),
  )
  (0)
  .until(WIDTH * HEIGHT)
  .map(
    fn(pixel) -> Vector {
      let x = pixel % WIDTH
      let y = pixel / WIDTH
      let mut color_acc = { x: 0, y: 0, z: 0 }
      for
        color in (0)
        .until(SAMPLES)
        .map(
          fn(_i) {
            let ray : Ray = {
              origin: world.camera.eye,
              direction: (world.camera.left_top +
              (
                vdu.mul(x.to_float() + rng.next()) +
                vdv.mul(y.to_float() + rng.next())
              ) -
              world.camera.eye).unit(),
            }
            world.trace(rng, ray, 0)
          },
        ) {
        color_acc = color_acc + color
      }
      color_acc.div(SAMPLES.to_float())
    },
  )
  .collect()
  |> write_ppm
}
