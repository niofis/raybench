local io = require("io")

local WIDTH = 1280
local HEIGHT = 720

local Vector3 = {}
Vector3.__index = Vector3

function Vector3.new (x, y, z)
  local v = {}
  setmetatable(v, Vector3)

  v.x = x
  v.y = y
  v.z = z

  return v
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


