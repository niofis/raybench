import 
  strutils
  math

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
proc norm(a, b: V3): float32 = sqrt(a.dot(a))
proc unit(a: V3): V3 = a / norm(a,a)

proc writeppm() =
  let ppm = open("nimrb.ppm", fmWrite)
  ppm.writeln(format("P3\n$# $#\n255",WIDTH, HEIGHT))
  ppm.close()

proc main() =
  writeppm()


#main()
var v1 = (x:1'f32, y:2'f32, z:3'f32)
var v2 = (x:1'f32, y:1'f32, z:1'f32)
echo v* 3'f32
