defmodule Raybench do
  @width 1280
  @height 720
  @samples 50
  @max_depth 5

  defmodule Vector, do: defstruct x: 0.0, y: 0.0, z: 0.0

  defp vadd(v1, v2), do: %Vector{x: v1.x + v2.x, y: v1.y + v2.y, z: v1.z + v2.z}
  defp vsub(v1, v2), do: %Vector{x: v1.x - v2.x, y: v1.y - v2.y, z: v1.z - v2.z}
  defp vmul(v1, v2), do: %Vector{x: v1.x * v2.x, y: v1.y * v2.y, z: v1.z * v2.z}
  defp vmuls(v1, s), do: %Vector{x: v1.x * s, y: v1.y * s, z: v1.z * s}
  defp vdiv(v1, v2), do: %Vector{x: v1.x / v2.x, y: v1.y / v2.y, z: v1.z / v2.z}
  defp vdivs(v1, s), do: %Vector{x: v1.x / s, y: v1.y / s, z: v1.z / s}
  defp vdot(v1, v2), do: (v1.x * v2.x) + (v1.y * v2.y) + (v1.z * v2.z)
  defp vnorm(v1), do: :math.sqrt(vdot v1, v1)
  defp vunit(v1), do: vdivs v1, vnorm v1

  defmodule Ray, do: defstruct origin: %Vector{}, direction: %Vector{}
  defp point(r, d), do: vadd r.origin, (vmuls r.direction, d)

  defmodule Camera  do 
    defstruct eye: %Vector{x: 0.0, y: 4.5, z: 75.0},
              lt: %Vector{x: -8.0, y: 9.0, z: 50.0},
              rt: %Vector{x: 8.0, y: 9.0, z: 50.0},
              lb: %Vector{x: -8.0, y: 0.0, z: 50.0}
  end

  defmodule Sphere, do: defstruct center: %Vector{}, radius: 0.0, color: %Vector{}, islight: false

  defmodule Hit, do: defstruct distance: 1.0e16, point: %Vector{}, normal: %Vector{}, sphere: %Sphere{}
  
  defp sphit(sp, ry) do
    oc = vsub ry.origin, sp.center
    a = vdot ry.direction, ry.direction
    b = vdot oc, ry.direction
    c = (vdot oc, oc) - (sp.radius * sp.radius)
    dis = (b * b) - (a * c)
    if dis > 0 do
      e = :math.sqrt dis
      t = (-b - e) / a
      if t > 0.007 do
        pt = point ry, t
        %Hit{distance: t, point: pt, normal: (vsub pt, sp.center) |> vunit, sphere: sp}
      else
        t2 = (-b + e) / a
        if t2 > 0.007 do
          pt2 = point ry, t2
          %Hit{distance: t2, point: pt2, normal: (vsub pt2, sp.center) |> vunit, sphere: sp}
        else
          %Hit{}
        end
      end
    else
      %Hit{}
    end
  end

  defmodule World, do: defstruct camera: %Camera{}, spheres: [
    %Sphere{center: %Vector{x: 0.0, y: -10002.0, z: 0.0}, radius: 9999.0, 
            color: %Vector{x: 1.0, y: 1.0, z: 1.0}, islight: false},
    %Sphere{center: %Vector{x: -10012.0, y: 0.0, z: 0.0}, radius: 9999.0, 
            color: %Vector{x: 1.0, y: 0.0, z: 0.0}, islight: false},
    %Sphere{center: %Vector{x: 10012.0, y: 0.0, z: 0.0}, radius: 9999.0, 
            color: %Vector{x: 0.0, y: 1.0, z: 0.0}, islight: false},
    %Sphere{center: %Vector{x: 0.0, y: 0.0, z: -10012.0}, radius: 9999.0, 
            color: %Vector{x: 1.0, y: 1.0, z: 1.0}, islight: false},
    %Sphere{center: %Vector{x: 0.0, y: 10012.0, z: 0.0}, radius: 9999.0, 
            color: %Vector{x: 1.0, y: 1.0, z: 1.0}, islight: true},
    %Sphere{center: %Vector{x: -5.0, y: 0.0, z: 2.0}, radius: 2.0, 
            color: %Vector{x: 1.0, y: 1.0, z: 0.0}, islight: false},
    %Sphere{center: %Vector{x: 0.0, y: 5.0, z: -1.0}, radius: 4.0, 
            color: %Vector{x: 1.0, y: 0.0, z: 0.0}, islight: false},
    %Sphere{center: %Vector{x: 8.0, y: 5.0, z: -1.0}, radius: 2.0, 
            color: %Vector{x: 0.0, y: 0.0, z: 1.0}, islight: false}
  ]
  
  defp rnd2(), do: 2.0 * :random.uniform() - 1.0

  defp rnddome(normal) do
    p = vunit %Vector{x: rnd2(), y: rnd2(), z: rnd2()}
    d = vdot(p, normal)
    if d < 0 do rnddome(normal) else p end
  end

  defp trace(world, ray, depth) do
    hits = Enum.map(world.spheres, &(sphit(&1, ray)))
    closest = Enum.reduce(hits, %Hit{}, &(if (&1).distance < (&2).distance do &1 else &2 end))
    cond do
      closest == %Hit{} -> %Vector{}
      closest.sphere.islight -> closest.sphere.color
      depth < @max_depth ->
        nray = %Ray{origin: closest.point, direction: rnddome(closest.normal)}
        ncolor = trace(world, nray, (depth + 1))
        at = vdot nray.direction, closest.normal
        vmul closest.sphere.color, (vmuls ncolor, at)
      true -> %Vector{}
    end
  end


  defp to255(v), do: trunc(v * 255.99)
  defp colorToStr(color), do: "#{to255(color.x)} #{to255(color.y)} #{to255(color.z)} "

  defp writeppm(data) do
    {:ok, ppm} = File.open "elixirrb-mp.ppm", [:write, {:encoding, :utf8}]
    IO.puts(ppm, "P3\n#{@width} #{@height}\n255")
    Enum.each(data, fn row -> 
      Enum.each(Task.await(row,1000*60*1000), fn c -> IO.write(ppm, colorToStr(c)) end)
      IO.write(ppm, "\n") end)
    File.close ppm
  end

  def main do
    world = %World{}
    vdu = vdivs(vsub(world.camera.rt, world.camera.lt), @width)
    vdv = vdivs(vsub(world.camera.lb, world.camera.lt), @height)
    pixel = fn(x, y) ->
      :random.seed(:os.timestamp)
      dir = vunit(
        vsub(
          vadd(
            world.camera.lt,
            vadd(vmuls(vdu,(x + :random.uniform())),vmuls(vdv,(y + :random.uniform())))),
          world.camera.eye))
        ray = %Ray{origin: world.camera.eye, direction: dir}
        trace(world, ray, 0)
    end
    samples = fn (x,y) ->
      Enum.map(1..@samples, fn(_) -> pixel.(x,y) end)
      |> Enum.reduce(%Vector{}, &vadd/2)
      |> vdivs(@samples)
    end
    cols = fn(y, x, cols) -> if x < @width do [samples.(x,y) | cols.(y, (x+1), cols)] else [] end end
    rows = fn(y, rows) -> if y < @height do [Task.async(fn -> cols.(y, 0, cols) end) | rows.(y + 1, rows)] else [] end end
    writeppm(rows.(0, rows))
  end
end

Raybench.main

