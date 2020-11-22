let Width = 1280
let Height = 720
let Samples = 50
let MaxDepth = 5

struct V3 {
  let x: Float
  let y: Float
  let z: Float

  func dot(_ rhs: V3) -> Float {
    return self.x * rhs.x + self.y * rhs.y + self.z * rhs.z
  }

  func norm() -> Float {
    return self.dot(self).squareRoot()
  }

  func unit() -> V3 {
    return self / self.norm()
  }
}

func +(left: V3, right: V3) -> V3 {
  return V3(x: left.x + right.x, y: left.y + right.y, z: left.z + right.z)
}

func -(left: V3, right: V3) -> V3 {
  return V3(x: left.x - right.x, y: left.y - right.y, z: left.z - right.z)
}

func *(left: V3, right: V3) -> V3 {
  return V3(x: left.x * right.x, y: left.y * right.y, z: left.z * right.z)
}

func *(left: V3, right: Float) -> V3 {
  return V3(x: left.x * right, y: left.y * right, z: left.z * right)
}

func /(left: V3, right: Float) -> V3 {
  return V3(x: left.x / right, y: left.y / right, z: left.z / right)
}

struct Rng {
  var x: UInt32 = 123456789
  var y: UInt32 = 362436069
  var z: UInt32 = 521288629
  var w: UInt32 = 80675123

  mutating func next() -> Float {
    let max: Float = 4294967295.0
    let t = self.x ^ (self.x << 11)
    self.x = self.y
    self.y = self.z
    self.z = self.w
    self.w = self.w ^ (self.w >> 19) ^ (t ^ (t >> 8))
    return Float(self.w) / max
  }
}
var random = Rng();

func random_dome(_ normal: V3) -> V3 {
  repeat {
    let v = V3(x: random.next() * 2.0 - 1.0,
               y: random.next() * 2.0 - 1.0,
               z: random.next() * 2.0 - 1.0)
    .unit()
    if v.dot(normal) >= 0 {
      return v
    }
  } while true
}

struct Ray {
  let origin: V3
  let direction: V3

  func point(_ t: Float) -> V3 {
    return V3(x: self.origin.x + self.direction.x * t,
              y: self.origin.y + self.direction.y * t,
              z: self.origin.z + self.direction.z * t)
  }
}

class Camera {
  let eye: V3
  let lt: V3
  let rt: V3
  let lb: V3
  init(eye: V3, lt: V3, rt: V3, lb: V3) {
    self.eye = eye
    self.lt = lt
    self.rt = rt
    self.lb = lb
  }
}

struct Hit {
  let dist: Float
  let point: V3
  let normal: V3
}

struct Sphere {
  let center: V3
  let radius: Float
  let color: V3
  let isLight: Bool

  func hit(_ ray: Ray) -> Hit? {
    let oc = ray.origin - self.center
    let a = ray.direction.dot(ray.direction)
    let b = oc.dot(ray.direction)
    let c = oc.dot(oc) - self.radius * self.radius
    let dis = b * b - a * c

    if dis > 0 {
      let e = dis.squareRoot()

      var t = (-b - e) / a
      if t > 0.007 {
        let pt = ray.point(t)
        let n = (pt - self.center).unit()
        return Hit(dist: t, point: pt, normal: n )
      }
      
      t = (-b + e) / a
      if t > 0.007 {
        let pt = ray.point(t)
        let n = (pt - self.center).unit()
        return Hit(dist: t, point: pt, normal: n )
      }
      
    }
    return nil
  }
}

class World {
  let spheres: [Sphere]
  let camera: Camera
  init() {
    self.camera = Camera(
      eye: V3(x: 0, y: 4.5, z: 75),
      lt: V3(x: -8, y: 9, z: 50),
      rt: V3(x: 8, y: 9, z: 50),
      lb: V3(x: -8, y: 0, z: 50))
    self.spheres = [
      Sphere(
        center: V3(x: 0, y: -10002, z: 0),
        radius: 9999,
        color: V3(x: 1, y: 1, z: 1),
        isLight: false
      ),
      Sphere(
        center: V3(x: -10012, y: 0, z: 0),
        radius: 9999,
        color: V3(x: 1, y: 0, z: 0),
        isLight: false
      ),
      Sphere(
        center: V3(x: 10012, y: 0, z: 0),
        radius: 9999,
        color: V3(x: 0, y: 1, z: 0),
        isLight: false
      ),
      Sphere(
        center: V3(x: 0, y: 0, z: -10012),
        radius: 9999,
        color: V3(x: 1, y: 1, z: 1),
        isLight: false
      ),
      Sphere(
        center: V3(x: 0, y: 10012, z: 0),
        radius: 9999,
        color: V3(x: 1, y: 1, z: 1),
        isLight: true
      ),
      Sphere(
        center: V3(x: -5, y: 0, z: 2),
        radius: 2,
        color: V3(x: 1, y: 1, z: 0),
        isLight: false
      ),
      Sphere(
        center: V3(x: 0, y: 5, z: -1),
        radius: 4,
        color: V3(x: 1, y: 0, z: 0),
        isLight: false
      ),
      Sphere(
        center: V3(x: 8, y: 5, z: -1),
        radius: 2,
        color: V3(x: 0, y: 0, z: 1),
        isLight: false
      )
    ]
  }
  
  func trace(_ ray: Ray, _ depth: UInt32) -> V3 {
    if depth >= MaxDepth {
      return V3(x: 0, y: 0, z: 0)
    }
    var closestHit: Hit?
    var closestSphere: Sphere?
    for sphere in self.spheres {
      if let hit = sphere.hit(ray) {
        if let ch = closestHit {
          if hit.dist >= ch.dist {
            continue
          }
        }
        closestHit = hit
        closestSphere = sphere
      }
    }

    if let cs = closestSphere, let hit = closestHit { 
      if cs.isLight {
        return cs.color
      }

      let nray = Ray(origin: hit.point, direction: random_dome(hit.normal))
      let ncolor = self.trace(nray, depth + 1)
      let at = nray.direction.dot(hit.normal)
      return cs.color * (ncolor * at)
    }
    else {
      return V3(x: 0, y: 0, z: 0)
    }
  }
}

func writePpm(_ data: [V3]) {
  print("P3\n\(Width) \(Height)\n255")
  for y in 0..<Height {
    for x in 0..<Width {
      let pixel = data[y * Width + x]
      print("\(Int(pixel.x * 255.99)) \(Int(pixel.y * 255.99)) \(Int(pixel.z * 255.99))", terminator:" ")
    }
    print("");
  }
}

func main() {
  let world = World()
  let vdu = (world.camera.rt - world.camera.lt) / Float(Width)
  let vdv = (world.camera.lb - world.camera.lt) / Float(Height)

  var data = [V3]();
  data.reserveCapacity(Width * Height);

  for y in 0..<Height {
    for x in 0..<Width {
      var acc = V3(x: 0, y: 0, z: 0)
      for _ in 0..<Samples {
        let ray = Ray(
            origin: world.camera.eye,
            direction: ((world.camera.lt 
                         + (vdu * (Float(x) + random.next()) 
                            + vdv * (Float(y) + random.next()))) 
                         - world.camera.eye)
            .unit()
          )
        acc = acc + world.trace(ray, 0)
      }
      data.append(acc / Float(Samples))
    }
  }
  writePpm(data)
}

main()
