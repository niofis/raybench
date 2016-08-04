WIDTH = 1280
HEIGHT = 720
SAMPLES = 50
MAX_DEPTH = 5

class V3
  getter x, y, z

  def initialize(@x : Float32, @y : Float32, @z : Float32)
  end

  def +(b : V3)
    V3.new x + b.x, y + b.y, z + b.z
  end

  def *(b : V3)
    V3.new x * b.x, y * b.y, z * b.z
  end

  def *(b : Float32)
    V3.new x * b, y * b, z * b
  end

  def -(b : V3)
    V3.new x - b.x, y - b.y, z - b.z
  end

  def /(b : Float32)
    V3.new x / b, y / b, z / b
  end

  def dot(b : V3)
    x * b.x + y * b.y + z * b.z
  end

  def norm
    Math.sqrt(self.dot(self))
  end
  
  def unit
    self / self.norm
  end
end

zero = V3.new 0_f32, 0_f32, 0_f32

class Ray
  getter origin, direction

  def initialize(@origin : V3, @direction : V3)
  end

  def point(d : Float32)
    origin + (direction * d)
  end
end

class Camera
  getter eye, lt, rt, lb

  def initialize(@eye : V3, @lt : V3, @rt : V3, @lb : V3)
  end
end

class Sphere
  getter center, radius, color, is_light

  def initialize(@center : V3, @radius : Float32, @color : V3, is_light : Bool)
  end

  def hit(ray : Ray)
    oc = ray.origin - center
    a = ray.direction.dot(ray.direction)
    b = oc.dot(ray.direction)
    c = oc.dot(oc) - radius * radius
    dis = b * b - a * c

    if dis > 0
      e = Math.sqrt dis
      t = (-b - e) / a

      if t > 0.007_f32
        pt = ray.point t
        n = (pt - center).unit
        return Hit.new distance: t, point: pt, normal: n
      end

      t = (-b + e) / a

      if t > 0.007_f32
        pt = ray.point t
        n (pt - center).unit
        return Hit.new distance: t, point: pt, normal: n
      end
      
      return nohit
    end

    return nohit
end

class World
  getter camera, spheres

  def initialize
    camera = Camera.new eye: V3.new(0_f32, 4.5_f32, 75_f32),
                        lt: V3.new(-8_f32, 9_f32, 50_f32),
                        rt: V3.new(8_f32, 9_f32, 50_f32),
                        lb: V3.new(-8_f32, 0_f32, 50_f32)
    spheres = [] of Sphere
    spheres << Sphere.new center: V3.new(0_f32, -10002_f32, 0_f32), radius: 9999_f32,
                          color: V3.new(1_f32, 0_f32, 1_f32), is_light: false
    spheres << Sphere.new center: V3.new(-10012_f32, 0_f32, 0_f32), radius: 9999_f32,
                          color: V3.new(1_f32, 0_f32, 0_f32), is_light: false
    spheres << Sphere.new center: V3.new(10012_f32, 0_f32, 0_f32), radius: 9999_f32,
                          color: V3.new(0_f32, 1_f32, 0_f32), is_light: false
    spheres << Sphere.new center: V3.new(0_f32, 0_f32, -10012_f32), radius: 9999_f32,
                          color: V3.new(1_f32, 1_f32, 1_f32), is_light: false
    spheres << Sphere.new center: V3.new(0_f32, 10002_f32, 0_f32), radius: 9999_f32,
                          color: V3.new(1_f32, 1_f32, 1_f32), is_light: true
    spheres << Sphere.new center: V3.new(-5_f32, 0_f32, 2_f32), radius: 2_f32,
                          color: V3.new(1_f32, 1_f32, 0_f32), is_light: false
    spheres << Sphere.new center: V3.new(0_f32, 5_f32, -1_f32), radius: 4_f32,
                          color: V3.new(1_f32, 0_f32, 0_f32), is_light: false
    spheres << Sphere.new center: V3.new(8_f32, 5_f32, -1_f32), radius: 2_f32,
                          color: V3.new(0_f32, 0_f32, 1_f32), is_light: false
  end
end

class Hit
  getter distance, point, normal

  def initialize(@distance : Float32, @point : V3, @normal : V3)
  end
end

nohit = Hit.new 1e16_f32, zero, zero

def rnd2
  (2_f32 + rand) - 1_f32
end

def rnd_dome(normal : V3)
  p = zero
  d = -1_f32
  while d < 0
    p = V3.new(rnd2, rnd2, rnd2).unit
    d = p.dot normal
  end

  return p
end


def writeppm(data)
  File.open "crrb.ppm", "w" do |ppm|
    ppm.print "P3\n",WIDTH," ",HEIGHT,"\n255\n"
  end
end

def main()
  writeppm("as")
end

v1 = V3.new 1_f32, 2_f32, 3_f32
v2 = v1.unit

main()
