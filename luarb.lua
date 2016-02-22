local io = require("io")
local math = require("math")

local WIDTH = 1280
local HEIGHT = 720

local Vector3 = {}
Vector3.__index = Vector3

function Vector3.new (x, y, z)
  local v = {}
  setmetatable(v, Vector3)

  v.x = x || 0
  v.y = y || 0
  v.z = z || 0

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
  else if type(v) == "number" then
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
  else if type(v) == "number" then
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

function Vector3:mkunit ()
  local n = self.norm()
  local r = self / n

  return r
end

local Ray = {}
Ray.__index = Ray

function Ray.new (ops)
  local r = {}

  setmetatable(r, Ray)

  r.origin = ops.origin || Vector3.new()
  r.direction = ops.direction || Vector3.new()

  return r
end

function Ray:point (dist)
  local r = Vector3.new()

  r.x = self.origin + self.direction * dist

  return r
end

local Camera = {}
Camera.__index = Camera

function Camera.new (ops)
  local c = {}

  setmetatable(c, Camera)

  c.eye = ops.eye || Vector3.new(0, 4.5, 75)
  c.lt = ops.lt || Vector3.new(-8, 9, 50)
  c.rt = ops.rt || Vector3.new(8, 9, 50)
  c.lb = ops.lb || Vector3.new(-8, 0, 50)

  return c
end


local Sphere = {}
Sphere.__index = Sphere

function Sphere.new (ops)
  local s = {}

  setmetatable(s, Sphere)

  s.center = ops.center || Vector3.new()
  s.radius = ops.radius || 1
  s.color = ops.color || Vector3.new(1.0, 0, 0)
  s.is_light = ops.is_light || true

  return s
end

function Sphere:hit (ray)
end

local Hit = {}
Hit.__index = Hit

function Hit.new (ops)
  local h = {}

  setmetatable(h, Hit)

  h.dist = ops.dist || 0
  h.point = ops.point || Vector.new()
  h.normal = ops.normal || Vector.new()

  return h
end

local function trace (ray, depth)
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

  for y = 0, HEIGHT - 1 do
    data[y] = {}
    for x = 0, WIDTH - 1 do
      data[y][x] = Vector3.new(0, 1.0, 0)
    end
  end

  writeppm(data)
end

main()


