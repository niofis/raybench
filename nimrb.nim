import strutils, math, random

const 
  WIDTH = 1280
  HEIGHT = 720
  SAMPLES = 50
  MAXDEPTH = 5

type V3 = tuple[x: float32, y: float32, z: float32]
const zero = (0f, 0f, 0f)

proc `+`*(a, b: V3): V3 = (x: a.x + b.x, y: a.y + b.y, z: a.z + b.z)
proc `*`*(a, b: V3): V3 = (x: a.x * b.x, y: a.y * b.y, z: a.z * b.z)
proc `*`*(a: V3, s: float32): V3 = (x: a.x * s, y: a.y * s, z: a.z * s)
proc `-`*(a, b: V3): V3 = (x: a.x - b.x, y: a.y - b.y, z: a.z - b.z)
proc `/`*(a: V3, s: float32): V3 = (x: a.x / s, y: a.y / s, z: a.z / s)
proc dot(a, b: V3): float32 = a.x * b.x + a.y * b.y + a.z * b.z
proc norm(a: V3): float32 = sqrt(a.dot(a))
proc unit(a: V3): V3 = a / a.norm

type Ray = tuple[origin: V3, direction: V3]

proc point(r: Ray, d: float32): V3 = r.origin + (r.direction * d)

type Camera = tuple[eye: V3, lt: V3, rt: V3, lb: V3]

type Sphere = tuple[center: V3, radius: float32, color: V3, is_light: bool]

type World = tuple[camera: Camera, spheres: seq[Sphere]]

proc world_new(): World =
  result.camera = (eye: (0f, 4.5f, 75f),
                  lt:  (-8f, 9f, 50f),
                  rt:  (8f, 9f, 50f),
                  lb:  (-8f, 0f, 50f))

  result.spheres = newSeq[Sphere]()
  
  result.spheres.add((center: (0f, -10002f, 0f), radius: 9999f,
                      color: (1f, 1f, 1f), is_light: false))

  result.spheres.add((center: (-10012f, 0f, 0f), radius: 9999f,
                      color: (1f, 0f, 0f), is_light: false))

  result.spheres.add((center: (10012f, 0f, 0f), radius: 9999f,
                      color: (0f, 1f, 0f), is_light: false))

  result.spheres.add((center: (0f, 0f, -10012f), radius: 9999f,
                      color: (1f, 1f, 1f), is_light: false))

  result.spheres.add((center: (0f, 10012f, 0f), radius: 9999f,
                      color: (1f, 1f, 1f), is_light: true))

  result.spheres.add((center: (-5f, 0f, 2f), radius: 2f,
                      color: (1f, 1f, 0f), is_light: false))

  result.spheres.add((center: (0f, 5f, -1f), radius: 4f,
                      color: (1f, 0f, 0f), is_light: false))

  result.spheres.add((center: (8f, 5f, -1f), radius: 2f,
                      color: (0f, 0f, 1f), is_light: false))

type Hit = tuple[distance: float32, point: V3, normal: V3]

const nohit = (distance: 1e16f, point: zero, normal: zero)

proc sphit(sp: Sphere, ray: Ray): Hit =
  let oc = ray.origin - sp.center
  let a = dot(ray.direction, ray.direction)
  let b = oc.dot(ray.direction)
  let c = dot(oc, oc) - sp.radius * sp.radius
  let dis = b*b - a*c

  if dis > 0:
    var e = sqrt(dis)
    var t = (-b - e) / a

    if t > 0.007:
      let pt = ray.point(t)
      let n = (pt - sp.center).unit
      return (distance: t, point: pt, normal: n)

    t = (-b + e) / a

    if t > 0.007:
      let pt = ray.point(t)
      let n = (pt - sp.center).unit
      return (distance: t, point: pt, normal: n)

  nohit

proc rnd2(): float32 = (2.0 * rand(1.0)) - 1.0

proc rnd_dome(normal: V3): V3 =
  var d:float32
  var p:V3

  d = -1.0

  while d < 0:
    p = unit((rnd2(), rnd2(), rnd2()))
    d = p.dot(normal)
  p

proc trace(w: World, r: Ray, depth: int): V3 =
  var did_hit = false
  var hit = nohit
  var sp:Sphere

  result = zero

  for s in w.spheres:
    let lh = s.sphit(r)
    
    if lh.distance < hit.distance:
      sp = s
      did_hit = true
      result = s.color
      hit = lh

  if did_hit and depth < MAXDEPTH:
    if not sp.is_light:
      let nray = (origin: hit.point, direction: rnd_dome(hit.normal))
      let ncolor = trace(w, nray, depth + 1)
      let at = nray.direction.dot(hit.normal)
      result = result * (ncolor * at)

  if not did_hit or depth >= MAXDEPTH:
    result = zero

proc writeppm(data: seq[seq[V3]]) =
  write(stdout, format("P3\n$# $#\n255\n",WIDTH, HEIGHT))
  for row in data:
    for c in row:
      write(stdout, format("$# $# $# ",
        floor(c.x * 255.99).int,
        floor(c.y * 255.99).int,
        floor(c.z * 255.99).int))
    write(stdout, "\n")
  flushFile(stdout)

proc main() =
  var data = newSeq[seq[V3]]()
  let world = world_new()
  let vdu = (world.camera.rt - world.camera.lt) / WIDTH.float32
  let vdv = (world.camera.lb - world.camera.lt) / HEIGHT.float32

  randomize()
  
  for y in 0..<HEIGHT:
    var row = newSeq[V3]()
    for x in 0..<WIDTH:
      var color = zero
      var ray:Ray

      ray.origin = world.camera.eye

      for i in 1..SAMPLES:
        ray.direction = ((world.camera.lt + (vdu * (x.float32 + rand(1.0)) +
                        vdv * (y.float32 + rand(1.0)))) -
                        world.camera.eye).unit
        color = color + trace(world, ray, 0)

      color = color / SAMPLES.float32
      row.add(color)
    data.add(row)
  writeppm(data)

main()
