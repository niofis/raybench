local io = require("io")
local math = require("math")

local WIDTH = 128
local HEIGHT = 72
local SAMPLES = 15
local MAX_DEPTH = 5

--[[
baseline:
time lua luarb_opt.lua 

real	0m29.953s
user	0m29.776s
sys	0m0.160s
]]

local V3 = {}

V3.new = function (x, y, z)
  local v = {}

  v.x = x or 0
  v.y = y or 0
  v.z = z or 0

  return v
end

function V3.add (a, b)
  return V3.new(a.x + b.x, a.y + b.y, a.z + b.z)
end

function V3.sub (a, b)
  return V3.new(a.x - b.x, a.y - b.y, a.z - b.z)
end

function V3.mul (a, b)
  return V3.new(a.x * b.x, a.y * b.y, a.z * b.z)
end

function V3.muls (a, s)
  return V3.new(a.x * s, a.y * s, a.z * s)
end

function V3.div (a, b)
  return V3.new(a.x / b.x, a.y / b.y, a.z / b.z)
end

function V3.divs (a, s)
  return V3.new(a.x / s, a.y / s, a.z / s)
end

function V3.dot (a, b)
  return a.x * b.x + a.y * b.y + a.z * b.z
end

function V3.norm (a)
  return math.sqrt(V3.dot(a, a))
end

function V3.unit (a)
  return V3.divs(a, V3.norm(a))
end

local Ray = {}
Ray.__index = Ray

function Ray.new (ops)
  local r = {}

  setmetatable(r, Ray)

  ops = ops or {}

  r.origin = ops.origin
  r.direction = ops.direction

  return r
end

function Ray:point (dist)
  return V3.add(self.origin, V3.muls(self.direction, dist))
end

local Hit = {}
Hit.__index = Hit

function Hit.new (ops)
  local h = {}

  setmetatable(h, Hit)

  ops = ops or {}

  h.dist = ops.dist
  h.point = ops.point
  h.normal = ops.normal

  return h
end

local Camera = {}
Camera.__index = Camera

function Camera.new (ops)
  local c = {}

  setmetatable(c, Camera)

  ops = ops or {}

  c.eye = V3.new(0, 4.5, 75)
  c.lt = V3.new(-8, 9, 50)
  c.rt = V3.new(8, 9, 50)
  c.lb = V3.new(-8, 0, 50)

  return c
end

local Sphere = {}
Sphere.__index = Sphere

function Sphere.new (ops)
  local s = {}

  setmetatable(s, Sphere)

  ops = ops or {}

  s.center = ops.center
  s.radius = ops.radius
  s.color = ops.color
  s.is_light = ops.is_light

  return s
end

function Sphere:hit (ray)
  local oc = V3.sub(ray.origin, self.center)
  local a = V3.dot(ray.direction, ray.direction)
  local b = V3.dot(oc, ray.direction)
  local c = V3.dot(oc, oc) - self.radius * self.radius
  local dis = b * b - a * c

  if dis > 0 then
    local e = math.sqrt(dis)

    local t = (-b - e) / a
    if t > 0.007 then
      local hit = Hit.new()

      hit.dist = t
      hit.point = ray:point(t)
      hit.normal = V3.unit(V3.sub(hit.point, self.center))

      return hit
    end

    t = (-b + e) / a
    if t > 0.007 then
      local hit = Hit.new()

      hit.dist = t
      hit.point = ray:point(t)
      hit.normal = V3.unit(V3.sub(hit.point, self.center))

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
    center = V3.new(0, -10002, 0),
    radius = 9999,
    color = V3.new(1,1,1)
  }
  s.spheres[1] = Sphere.new{
    center = V3.new(-10012, 0, 0),
    radius = 9999, 
    color = V3.new(1,0,0)
  }
  s.spheres[2] = Sphere.new{
    center = V3.new(10012, 0, 0),
    radius = 9999,
    color = V3.new(0,1,0)
  }
  s.spheres[3] = Sphere.new{
    center = V3.new(0, 0, -10020), 
    radius = 9999,
    color = V3.new(1,1,1)
  }
  s.spheres[4] = Sphere.new{
    center = V3.new(0, 10012, 0),
    radius = 9999,
    color = V3.new(1,1,1),
    is_light = true
  }
  s.spheres[5] = Sphere.new{
    center = V3.new(-5, 0, 2),
    radius = 2,
    color = V3.new(1,1,0)
  }
  s.spheres[6] = Sphere.new{
    center = V3.new(0, 5, -1),
    radius = 4,
    color = V3.new(1,0,0)
  }
  s.spheres[7] = Sphere.new{
    center = V3.new(8, 5, -1),
    radius = 2,
    color = V3.new(0,0,1)
  }

  return s
end


local function rnd_dome (nrml)
  local p = V3.new()
  local d

  repeat
    p.x = 2 * math.random() - 1
    p.y = 2 * math.random() - 1
    p.z = 2 * math.random() - 1

    p = V3.unit(p)

    d = V3.dot(p, nrml)
  until d >= 0

  return p
end

local function trace (world, ray, depth)
  local color = V3.new()
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

      local at = V3.dot(nray.direction, hit.normal)

      color = V3.mul(color, V3.muls(ncolor, at))
    end
  end

  if did_hit == false or depth >= MAX_DEPTH then
    color = V3.new()
  end

  return color
end

local function writeppm (data)
  local ppm = io.open("luarb.ppm", "w")

  ppm:write(string.format("P3\n%u %u\n255\n", WIDTH, HEIGHT))

  for y = 0, HEIGHT - 1 do
    for x = 0, WIDTH - 1 do
      ppm:write(string.format("%u %u %u ",
        data[y][x].x * 255, 
        data[y][x].y * 255,
        data[y][x].z * 255))
    end
    ppm:write("\n")
  end

  ppm:close()
end

local function main ()



  local data = {}

  local world = World.new()

  local vdu = V3.divs(V3.sub(world.camera.rt, world.camera.lt), WIDTH)
  local vdv = V3.divs(V3.sub(world.camera.lb, world.camera.lt), HEIGHT)

  for y = 0, HEIGHT - 1 do
    data[y] = {}
    for x = 0, WIDTH - 1 do
      local color = V3.new()
      local ray = Ray.new()

      ray.origin = world.camera.eye

      for i = 0, SAMPLES-1 do

        ray.direction = V3.add(world.camera.lt,
          V3.add(V3.muls(vdu, (x + math.random())),
          V3.muls(vdv, (y + math.random()))))


        ray.direction = V3.sub(ray.direction, ray.origin)

        ray.direction = V3.unit(ray.direction)

        color = V3.add(color, trace(world, ray, 0))
      end

      color = V3.divs(color, SAMPLES)

      data[y][x] = color
    end
  end

  writeppm(data)
end

main()
