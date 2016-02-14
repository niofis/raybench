local io = require("io")

local function writeppm (data)
  local ppm = io.open("luarb.ppm", "w")

  ppm:write("P3\n

