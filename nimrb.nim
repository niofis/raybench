import strutils
import math

const 
  WIDTH = 1280
  HEIGHT = 720
  SAMPLES = 50
  MAXDEPTH = 5

type V3 = tuple[x: float32, y: float32, z: float32]
let zero = (0'f32, 0'f32, 0'f32)

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
  var world: World
  world.camera = (eye: (0'f32, 4.5'f32, 75'f32),
                  lt:  (-8'f32, 9'f32, 50'f32),
                  rt:  (8'f32, 9'f32, 50'f32),
                  lb:  (-8'f32, 0'f32, 50'f32))

  world.spheres = newSeq[Sphere]()
  
  world.spheres.add((center: (0'f32, -10002'f32, 0'f32), radius: 9999'f32,
                      color: (1'f32, 1'f32, 1'f32), is_light: false))

  world.spheres.add((center: (-10012'f32, 0'f32, 0'f32), radius: 9999'f32,
                      color: (1'f32, 0'f32, 0'f32), is_light: false))

  world.spheres.add((center: (10012'f32, 0'f32, 0'f32), radius: 9999'f32,
                      color: (0'f32, 1'f32, 0'f32), is_light: false))

  world.spheres.add((center: (0'f32, 0'f32, -10012'f32), radius: 9999'f32,
                      color: (1'f32, 1'f32, 1'f32), is_light: false))

  world.spheres.add((center: (0'f32, 10012'f32, 0'f32), radius: 9999'f32,
                      color: (1'f32, 1'f32, 1'f32), is_light: true))

  world.spheres.add((center: (-5'f32, 0'f32, 2'f32), radius: 2'f32,
                      color: (1'f32, 1'f32, 0'f32), is_light: false))

  world.spheres.add((center: (0'f32, 5'f32, -1'f32), radius: 4'f32,
                      color: (1'f32, 0'f32, 0'f32), is_light: false))

  world.spheres.add((center: (8'f32, 5'f32, -1'f32), radius: 2'f32,
                      color: (0'f32, 0'f32, 1'f32), is_light: false))
  return world

type Hit = tuple[distance: float32, point: V3, normal: V3]

let nohit = (distance: 1e16'f32, point: zero, normal: zero)

proc sphit(sp: Sphere, ray: Ray): Hit =
  let oc = ray.origin - sp.center
  let a = dot(ray.direction, ray.direction)
  let b = oc.dot(ray.direction)
  let c = dot(oc, oc) - sp.radius * sp.radius
  let dis = b*b - a*c

  if dis > 0:
    var e = sqrt(dis)
    var t:float32 = (-b - e) / a

    if t > 0.007'f32:
      let pt = ray.point(t)
      let n = (pt - sp.center).unit
      return (distance: t, point: pt, normal: n)

    t = (-b + e) / a

    if t > 0.007'f32:
      let pt = ray.point(t)
      let n = (pt - sp.center).unit
      return (distance: t, point: pt, normal: n)

    return nohit
  
  return nohit

proc rnd2(): float32 = float32(2'f32 * random(1'f32)) - 1'f32

proc rnd_dome(normal: V3): V3 =
  var d:float32
  var p:V3

  d = -1'f32

  while d < 0:
    p = ((rnd2(), rnd2(), rnd2())).unit
    d = p.dot(normal)
  return p

proc trace(w: World, r: Ray, depth: int): V3 =
  var did_hit = false
  var hit = nohit
  var color = zero
  var sp:Sphere

  for s in w.spheres:
    let lh = s.sphit(r)
    
    if lh.distance < hit.distance:
      sp = s
      did_hit = true
      color = s.color
      hit = lh

  if did_hit == true and depth < MAXDEPTH:
    if sp.is_light == false:
      let nray = (origin: hit.point, direction: rnd_dome(hit.normal))
      let ncolor = trace(w, nray, depth + 1)
      let at = nray.direction.dot(hit.normal)
      color = color * (ncolor * at)

  if did_hit == false or depth >= MAXDEPTH:
    color = zero

  return color

proc writeppm(data: seq[seq[V3]]) =
  let ppm = open("nimrb.ppm", fmWrite)
  ppm.write(format("P3\n$# $#\n255\n",WIDTH, HEIGHT))
  for row in data:
    for c in row:
      ppm.write(format("$# $# $# ",
        int(floor(c.x * 255.99'f32)),
        int(floor(c.y * 255.99'f32)),
        int(floor(c.z * 255.99'f32))))
    ppm.write("\n")
  ppm.close()

proc main() =
  var data = newSeq[seq[V3]]()
  let world = world_new()
  let vdu = (world.camera.rt - world.camera.lt) / float32(WIDTH)
  let vdv = (world.camera.lb - world.camera.lt) / float32(HEIGHT)

  randomize()
  
  for y in 0..(HEIGHT-1):
    var row = newSeq[V3]()
    for x in 0..(WIDTH-1):
      var color = zero
      var ray:Ray

      ray.origin = world.camera.eye

      for i in 1..SAMPLES:
        ray.direction = ((world.camera.lt + (vdu * (float32(x) + float32(random(1'f32))) +
                        vdv * (float32(y) + float32(random(1'f32))))) -
                        world.camera.eye).unit
        color = color + trace(world, ray, 0)

      color = color / float32(SAMPLES)
      row.add(color)
    data.add(row)
  writeppm(data)

main()
