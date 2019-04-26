WIDTH    = 1280
HEIGHT   =  720
SAMPLES  =   50
MAXDEPTH =    5

class V3
  attr_reader :x, :y, :z

  def initialize(x, y, z)
    @x, @y, @z = x, y, z
  end

  def +(b)
    V3.new(x + b.x, y + b.y, z + b.z)
  end

  def *(b)
    if V3 === b
      V3.new(x * b.x, y * b.y, z * b.z)
    else
      V3.new(x * b, y * b, z * b)
    end
  end

  def -(b)
    V3.new(x - b.x, y - b.y, z - b.z)
  end

  def /(b)
    V3.new(x / b, y / b, z / b)
  end

  def dot(b)
    x * b.x + y * b.y + z * b.z
  end

  def norm
    Math.sqrt(self.dot(self))
  end

  def unit
    self / self.norm
  end
end

ZERO = V3.new 0, 0, 0

class Ray
  attr_accessor :origin, :direction

  def initialize(origin:, direction:)
    @origin, @direction = origin, direction
  end

  def point(d)
    origin + (direction * d)
  end
end

class Camera
  attr_reader :eye, :lt, :rt, :lb

  def initialize(eye:, lt:, rt:, lb:)
    @eye, @lt, @rt, @lb = eye, lt, rt, lb
  end
end

class Sphere
  attr_reader :center, :radius, :color, :is_light

  def initialize(center:, radius:, color:, is_light:)
    @center, @radius, @color, @is_light = center, radius, color, is_light
  end

  def hit(ray)
    oc = ray.origin - center
    a = ray.direction.dot(ray.direction)
    b = oc.dot(ray.direction)
    c = oc.dot(oc) - radius * radius
    dis = b * b - a * c

    if dis > 0
      e = Math.sqrt dis
      t = (-b - e) / a

      if t > 0.007
        pt = ray.point t
        n = (pt - center).unit
        return Hit.new distance: t, point: pt, normal: n
      end

      t = (-b + e) / a

      if t > 0.007
        pt = ray.point t
        n = (pt - center).unit
        return Hit.new distance: t, point: pt, normal: n
      end

      return NOHIT
    end

    return NOHIT
  end
end

class World
  attr_reader :camera, :spheres

  def initialize
    @camera = Camera.new(eye: V3.new(0, 4.5, 75), lt: V3.new(-8, 9, 50), rt: V3.new(8, 9, 50), lb: V3.new(-8, 0, 50))
    @spheres = []
    @spheres << Sphere.new(center: V3.new(0, -10002, 0), radius: 9999, color: V3.new(1, 1, 1), is_light: false)
    @spheres << Sphere.new(center: V3.new(-10012, 0, 0), radius: 9999, color: V3.new(1, 0, 0), is_light: false)
    @spheres << Sphere.new(center: V3.new(10012, 0, 0), radius: 9999, color: V3.new(0, 1, 0), is_light: false)
    @spheres << Sphere.new(center: V3.new(0, 0, -10012), radius: 9999, color: V3.new(1, 1, 1), is_light: false)
    @spheres << Sphere.new(center: V3.new(0, 10012, 0), radius: 9999, color: V3.new(1, 1, 1), is_light: true)
    @spheres << Sphere.new(center: V3.new(-5, 0, 2), radius: 2, color: V3.new(1, 1, 0), is_light: false)
    @spheres << Sphere.new(center: V3.new(0, 5, -1), radius: 4, color: V3.new(1, 0, 0), is_light: false)
    @spheres << Sphere.new(center: V3.new(8, 5, -1), radius: 2, color: V3.new(0, 0, 1), is_light: false)
  end
end

class Hit
  attr_reader :distance, :point, :normal

  def initialize(distance:, point:, normal:)
    @distance, @point, @normal = distance, point, normal
  end
end

NOHIT = Hit.new distance: 1e16, point: ZERO, normal: ZERO

def rnd2
  (2 * rand).to_f - 1
end

def rnd_dome(normal)
  p = ZERO
  d = -1

  while d < 0
    p = V3.new(rnd2, rnd2, rnd2).unit
    d = p.dot normal
  end

  return p
end

def trace(w, r, depth)
  did_hit = false
  hit = NOHIT
  color = ZERO
  sp = Sphere.new center: ZERO, radius: 0, color: ZERO, is_light: false

  w.spheres.each do |s|
    lh = s.hit r

    if lh.distance < hit.distance
      sp = s
      did_hit = true
      color = s.color
      hit = lh
    end
  end

  if did_hit == true && depth < MAXDEPTH
    if sp.is_light == false
      nray = Ray.new origin: hit.point, direction: rnd_dome(hit.normal)
      ncolor = trace w, nray, depth + 1
      at = nray.direction.dot(hit.normal)
      color = color * (ncolor * at)
    end
  end

  if did_hit == false || depth >= MAXDEPTH
    color = ZERO
  end

  return color
end

def to255(v)
  (v * 255.99).floor
end

def writeppm(data)
  File.open "rbrb.ppm", "w" do |ppm|
    ppm.print "P3\n", WIDTH, " ", HEIGHT, "\n255\n"
    data.each do |row|
      row.each do |c|
        ppm.print to255(c.x), " ", to255(c.y), " ", to255(c.z), " "
      end
      ppm.print "\n"
    end
  end
end

def main
  data = [[]]
  world = World.new
  vdu = (world.camera.rt - world.camera.lt) / WIDTH.to_f
  vdv = (world.camera.lb - world.camera.lt) / HEIGHT.to_f

  (0...HEIGHT).each do |y|
    row = []
    (0...WIDTH).each do |x|
      color = ZERO
      ray = Ray.new origin: ZERO, direction: ZERO

      ray.origin = world.camera.eye

      (1..SAMPLES).each do
        ray.direction = ((world.camera.lt + (vdu * (x + rand).to_f +
                                             vdv * (y + rand).to_f)) -
                         world.camera.eye).unit
        color = color + trace(world, ray, 0)
      end

      color = color / SAMPLES.to_f
      row << color
    end
    data << row
  end
  writeppm data
end

main()
