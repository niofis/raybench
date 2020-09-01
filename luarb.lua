local math = require("math")

local WIDTH = 1280
local HEIGHT = 720
local SAMPLES = 50
local MAX_DEPTH = 5

local Vector3 = {}
Vector3.__index = Vector3

function Vector3.new (x, y, z)
  local v = {}
  setmetatable(v, Vector3)

  v.x = x or 0
  v.y = y or 0
  v.z = z or 0

  return v
end

function Vector3:__add (v)
  local r = Vector3.new()

  r.x = self.x + v.x
  r.y = self.y + v.y
  r.z = self.z + v.z

  return r
end

function Vector3:__sub (v)
  local r = Vector3.new()

  r.x = self.x - v.x
  r.y = self.y - v.y
  r.z = self.z - v.z

  return r
end

function Vector3:__mul (v)
  local r = Vector3.new()

  if type(v) == "table" then 
    r.x = self.x * v.x
    r.y = self.y * v.y
    r.z = self.z * v.z
  elseif type(v) == "number" then
    r.x = self.x * v
    r.y = self.y * v
    r.z = self.z * v
  end

  return r
end

function Vector3:__div (v)
  local r = Vector3.new()

  if type(v) == "table" then 
    r.x = self.x / v.x
    r.y = self.y / v.y
    r.z = self.z / v.z
  elseif type(v) == "number" then
    r.x = self.x / v
    r.y = self.y / v
    r.z = self.z / v
  end

  return r
end

function Vector3:dot (v)
  local r =
    self.x * v.x +
    self.y * v.y +
    self.z * v.z

  return r
end

function Vector3:norm ()
  local r = math.sqrt(
    self.x * self.x +
    self.y * self.y +
    self.z * self.z)

  return r
end

function Vector3:unit ()
  local n = self:norm()
  local r = self / n

  return r
end

local Ray = {}
Ray.__index = Ray

function Ray.new (ops)
  local r = {}

  setmetatable(r, Ray)

  ops = ops or {}

  r.origin = ops.origin or Vector3.new()
  r.direction = ops.direction or Vector3.new()

  return r
end

function Ray:point (dist)
  return self.origin + self.direction * dist
end

local Hit = {}
Hit.__index = Hit

function Hit.new (ops)
  local h = {}

  setmetatable(h, Hit)

  ops = ops or {}

  h.dist = ops.dist or 0
  h.point = ops.point or Vector3.new()
  h.normal = ops.normal or Vector3.new()

  return h
end

local Camera = {}
Camera.__index = Camera

function Camera.new (ops)
  local c = {}

  setmetatable(c, Camera)

  ops = ops or {}

  c.eye = ops.eye or Vector3.new(0, 4.5, 75)
  c.lt = ops.lt or Vector3.new(-8, 9, 50)
  c.rt = ops.rt or Vector3.new(8, 9, 50)
  c.lb = ops.lb or Vector3.new(-8, 0, 50)

  return c
end

local Sphere = {}
Sphere.__index = Sphere

function Sphere.new (ops)
  local s = {}

  setmetatable(s, Sphere)

  ops = ops or {}

  s.center = ops.center or Vector3.new()
  s.radius = ops.radius or 1
  s.color = ops.color or Vector3.new(1.0, 0, 0)
  s.is_light = ops.is_light

  return s
end

function Sphere:hit (ray)
  local oc = ray.origin - self.center
  local a = ray.direction:dot(ray.direction)
  local b = oc:dot(ray.direction)
  local c = oc:dot(oc) - self.radius * self.radius
  local dis = b * b - a * c

  if dis > 0 then
    local e = math.sqrt(dis)

    local t = (-b - e) / a
    if t > 0.007 then
      local hit = Hit.new()

      hit.dist = t
      hit.point = ray:point(t)
      hit.normal = (hit.point - self.center):unit()

      return hit
    end

    t = (-b + e) / a
    if t > 0.007 then
      local hit = Hit.new()

      hit.dist = t
      hit.point = ray:point(t)
      hit.normal = (hit.point - self.center):unit()

      return hit
    end

    return nil
  end

  return nil
end

local World = {}
World.__index = World

function World.new (ops)
  local s = {}

  setmetatable(s, World)

  ops = ops or {}

  s.camera = Camera.new()
  s.spheres = {}

  s.spheres[0] = Sphere.new{
    center = Vector3.new(0, -10002, 0),
    radius = 9999,
    color = Vector3.new(1,1,1)
  }
  s.spheres[1] = Sphere.new{
    center = Vector3.new(-10012, 0, 0),
    radius = 9999, 
    color = Vector3.new(1,0,0)
  }
  s.spheres[2] = Sphere.new{
    center = Vector3.new(10012, 0, 0),
    radius = 9999,
    color = Vector3.new(0,1,0)
  }
  s.spheres[3] = Sphere.new{
    center = Vector3.new(0, 0, -10020), 
    radius = 9999,
    color = Vector3.new(1,1,1)
  }
  s.spheres[4] = Sphere.new{
    center = Vector3.new(0, 10012, 0),
    radius = 9999,
    color = Vector3.new(1,1,1),
    is_light = true
  }
  s.spheres[5] = Sphere.new{
    center = Vector3.new(-5, 0, 2),
    radius = 2,
    color = Vector3.new(1,1,0)
  }
  s.spheres[6] = Sphere.new{
    center = Vector3.new(0, 5, -1),
    radius = 4,
    color = Vector3.new(1,0,0)
  }
  s.spheres[7] = Sphere.new{
    center = Vector3.new(8, 5, -1),
    radius = 2,
    color = Vector3.new(0,0,1)
  }

  return s
end


local function rnd_dome (nrml)
  local p = Vector3.new()
  local d

  repeat
    p.x = 2 * math.random() - 1
    p.y = 2 * math.random() - 1
    p.z = 2 * math.random() - 1

    p = p:unit()

    d = p:dot(nrml)
  until d >= 0

  return p
end

local function trace (world, ray, depth)
  local color = Vector3.new()
  local did_hit = false
  local hit = Hit.new({dist = 1E+15})
  local sp

  for _,s in pairs(world.spheres) do
    local lh = s:hit(ray)

    if lh ~= nil and lh.dist > 0.0001 and lh.dist < hit.dist then
      sp = s
      did_hit = true
      color = s.color
      hit = lh
    end
  end

  if did_hit == true and depth < MAX_DEPTH then
    if sp.is_light ~= true then
      local nray = Ray.new()
      
      nray.origin = hit.point
      nray.direction = rnd_dome(hit.normal)

      local ncolor = trace(world, nray, depth + 1)

      local at = nray.direction:dot(hit.normal)

      color = color * ncolor * at
    end
  end

  if did_hit == false or depth >= MAX_DEPTH then
    color = Vector3.new()
  end

  return color
end

local function writeppm (data)
  local ppm = string.format("P3\n%u %u\n255\n", WIDTH, HEIGHT)

  for y = 0, HEIGHT - 1 do
    for x = 0, WIDTH - 1 do
      ppm = ppm .. string.format("%u %u %u ",
        data[y][x].x * 255, 
        data[y][x].y * 255,
        data[y][x].z * 255)
    end
    ppm = ppm .. "\n"
  end

  return ppm
end

local function main ()



  local data = {}

  local world = World.new()

  local vdu = (world.camera.rt - world.camera.lt) / WIDTH
  local vdv = (world.camera.lb - world.camera.lt) / HEIGHT

  for y = 0, HEIGHT - 1 do
    data[y] = {}
    for x = 0, WIDTH - 1 do
      local color = Vector3.new()
      local ray = Ray.new()

      ray.origin = world.camera.eye

      for i = 0, SAMPLES-1 do

        ray.direction = world.camera.lt +
          (vdu * (x + math.random())) +
          (vdv * (y + math.random()))


        ray.direction = ray.direction - ray.origin

        ray.direction = ray.direction:unit()

        color = color + trace(world, ray, 0)
      end

      color = color / SAMPLES

      data[y][x] = color
    end
  end

  print(writeppm(data))
end

main()
