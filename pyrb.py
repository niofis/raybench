import math
import random

WIDTH = 1280
HEIGHT = 720
MAX_DEPTH = 5
SAMPLES = 50

class Vector3:
  def __init__(self, x = 0, y = 0, z = 0):
    self.x = x
    self.y = y
    self.z = z

  def add(self, v):
    return Vector3(
            self.x + v.x,
            self.y + v.y,
            self.z + v.z)

  def sub(self, v):
    return Vector3(
            self.x - v.x,
            self.y - v.y,
            self.z - v.z)

  def mul(self, v):
    return Vector3(
            self.x * v.x,
            self.y * v.y,
            self.z * v.z)

  def muls(self, s):
    return Vector3(
            self.x * s,
            self.y * s,
            self.z * s)

  def div(self, v):
    return Vector3(
            self.x / v.x,
            self.y / v.y,
            self.z / v.z)

  def divs(self, s):
    return Vector3(
            self.x / s,
            self.y / s,
            self.z / s)

  def dot(self, v):
    return self.x * v.x + self.y * v.y + self.z * v.z
  
  def norm(self):
      return math.sqrt(self.dot(self))

  def unit(self):
      return self.divs(self.norm())


class Ray:
  def __init__(self, origin = Vector3(), direction = Vector3):
    self.origin = origin
    self.direction = direction

  def point(self, dist):
    return self.origin.add(self.direction.muls(dist))

class Sphere:
    def __init__(self, 
            center = Vector3(0, 0, 0),
            radius = 1,
            color = Vector3(1, 0, 0),
            is_light = False):
      self.center = center
      self.radius = radius
      self.color = color
      self.is_light = is_light

    def hit(self, ray):
      oc = ray.origin.sub(self.center)
      a = ray.direction.dot(ray.direction)
      b = oc.dot(ray.direction)
      c = oc.dot(oc) - self.radius * self.radius
      dis = b * b - a * c

      if dis > 0:
        e = math.sqrt(dis)

        t = (-b - e) / a
        if t > 0.007:
          hit = Hit()

          hit.dist = t
          hit.point = ray.point(t)
          hit.normal = hit.point.sub(self.center).unit()

          return hit

        t = (-b + e) / a
        if t > 0.007:
          hit = Hit()

          hit.dist = t
          hit.point = ray.point(t)
          hit.normal = hit.point.sub(self.center).unit()

          return hit

        return None
    
      return None

          

class Camera:
  def __init__(self, 
          eye = Vector3(0.0, 4.5, 75.0), 
          lt = Vector3(-8.0, 9.0, 50.0), 
          rt = Vector3(8.0, 9.0, 50.0), 
          lb = Vector3(-8.0, 0.0, 50.0)):
    self.eye = eye
    self.lt = lt
    self.rt = rt
    self.lb = lb

class Hit:
  def __init__(self,
          dist = 0,
          point = Vector3(),
          normal = Vector3()):
    self.dist = dist
    self.point = point
    self.normal = normal

class World:
  def __init__(self):
    self.camera = Camera()
    self.spheres = []
    #Floor
    self.spheres.append(Sphere(
        Vector3(0, -10002.0, 0),
        9999.0,
        Vector3(1.0, 1.0, 1.0),
        False));

    #Left
    self.spheres.append(Sphere(
        Vector3(-10012.0, 0, 0),
        9999.0,
        Vector3(1.0, 0, 0),
        False));

    #Right
    self.spheres.append(Sphere(
        Vector3(10012.0, 0, 0),
        9999.0,
        Vector3(0, 1.0, 0),
        False));

    #Back
    self.spheres.append(Sphere(
        Vector3(0, 0, -10012.0),
        9999.0,
        Vector3(1.0, 1.0, 1.0),
        False));

    #Ceiling
    self.spheres.append(Sphere(
        Vector3(0, 10012.0, 0),
        9999.0,
        Vector3(1.0, 1.0, 1.0),
        True));

    #Other
    self.spheres.append(Sphere(
        Vector3(-5.0, 0, 2.0),
        2.0,
        Vector3(1.0, 1.0, 0),
        False));

    self.spheres.append(Sphere(
        Vector3(0, 5.0, -1.0),
        4.0,
        Vector3(1.0, 0, 0),
        False));
    
    self.spheres.append(Sphere(
        Vector3(8.0, 5.0, -1.0),
        2.0,
        Vector3(0, 0, 1.0),
        False));


def rnd_dome(normal):
  p = Vector3()
  d = -1

  while d < 0:
    p.x = 2 * random.random() - 1
    p.y = 2 * random.random() - 1
    p.z = 2 * random.random() - 1
    p = p.unit()
    d = p.dot(normal)

  return p



def trace(world, ray, depth):
  color = Vector3()
  did_hit = False
  hit = Hit(1e15)
  sp = None


  for sphere in world.spheres:
    lh = sphere.hit(ray)
    if lh != None and lh.dist > 0.0001 and lh.dist < hit.dist:
      sp = sphere
      did_hit = True
      color = sphere.color
      hit = lh

  if did_hit == True and depth < MAX_DEPTH:
    if sp.is_light == False:
      nray = Ray(hit.point, rnd_dome(hit.normal))
      ncolor = trace(world, nray, depth + 1)
      at = nray.direction.dot(hit.normal)
      color = color.mul(ncolor.muls(at))

  if did_hit == False or depth >= MAX_DEPTH:
    color = Vector3()

  return color

def writeppm(data):
  ppm = open("pyrb.ppm", "w")
  ppm.write("P3\n{0} {1}\n255\n".format(WIDTH,HEIGHT))
  for y in range(0,HEIGHT):
    for x in range(0,WIDTH):
      ppm.write("{0} {1} {2} ".format(
        math.floor(data[y][x].x * 255.99),
        math.floor(data[y][x].y * 255.99),
        math.floor(data[y][x].z * 255.99),
        ))
    ppm.write("\n")
  ppm.close()

def main():
  data = []
  world = World()
  vdu = world.camera.rt.sub(world.camera.lt).divs(WIDTH)
  vdv = world.camera.lb.sub(world.camera.lt).divs(HEIGHT)
  
  for y in range(0, HEIGHT):
    line = []
    for x in range(0, WIDTH):
      color = Vector3()
      ray = Ray(world.camera.eye)

      for i in range(0,SAMPLES):
        
        ray.direction = world.camera.lt.add(vdu.muls(x + random.random()).add(
                        vdv.muls(y + random.random())))
        ray.direction = ray.direction.sub(ray.origin)
        ray.direction = ray.direction.unit()
        color = color.add(trace(world, ray, 0))
      
      color = color.divs(SAMPLES)
      line.append(color)
    data.append(line)
  writeppm(data)

main()
