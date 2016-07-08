import strutils
import math

const 
  WIDTH = 1280
  HEIGHT = 720
  SAMPLES = 50
  MAXDEPTH = 5

#type
#  V3* = ref object of RootObj
#    x*: float32
#    y*: float32
#    z*: float32

type V3 = tuple[x: float32, y: float32, z: float32]

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

  world.spheres.add((center: (8'f32, 5'f32, -1'f32), radius: 9999'f32,
                      color: (0'f32, 0'f32, 1'f32), is_light: false))
  return world

type Hit = tuple[distance: float32, point: V3, normal: V3]

let nohit = (distance: 1e16)

proc sphit(sp: Sphere, ray: Ray): Hit =
  let oc = ray.origin - sp.center
  let a = dot(ray.direction, ray.direction)
  let b = oc.dot(ray.direction)
  let c = dot(oc, oc)
  let dis = b*b - a*c

  if dis > 0:
    var e = sqrt(dis)
    var t = (-b - e) / a

proc writeppm() =
  let ppm = open("nimrb.ppm", fmWrite)
  ppm.write(format("P3\n$# $#\n255\n",WIDTH, HEIGHT))
  ppm.close()

proc main() =
  let world = world_new()
  writeppm()


main()
var v1 = (x:1'f32, y:2'f32, z:3'f32)
var v2 = (x:1'f32, y:1'f32, z:1'f32)
echo v1.unit
