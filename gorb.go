package main

import (
  "fmt"
  "os"
  "math"
  "math/rand"
)

const (
  Width = 128
  Height = 72
  Samples = 50
  MaxDepth = 5
)

type v3 struct {
  x float32
  y float32
  z float32
}

var zero = v3{x: 0, y: 0, z: 0}

func v3_add (a, b v3) v3 {
  return v3{x: a.x + b.x, y: a.y + b.y, z: a.z + b.z}
}

func v3_mul (a, b v3) v3 {
  return v3{x: a.x * b.x, y: a.y * b.y, z: a.z * b.z}
}

func v3_sub (a, b v3) v3 {
  return v3{x: a.x - b.x, y: a.y - b.y, z: a.z - b.z}
}

func v3_dot (a, b v3) float32 {
  return a.x * b.x + a.y * b.y + a.z * b.z
}

func v3_muls (a v3, s float32) v3 {
  return v3{x: a.x * s, y: a.y * s, z: a.z * s}
}

func v3_divs (a v3, s float32) v3 {
  return v3{x: a.x / s, y: a.y / s, z: a.z / s}
}

func v3_norm (a v3) float32 {
  return float32(math.Sqrt(float64(v3_dot(a,a))))
}

func v3_unit (a v3) v3 {
  return v3_divs(a, v3_norm(a))
}

type ray struct {
  origin v3
  direction v3
}

func ray_point (r ray, d float32) v3 {
  return v3_add(r.origin, v3_muls(r.direction, d))
}

type camera struct {
  eye v3
  lt v3
  rt v3
  lb v3
}

type sphere struct {
  center v3
  radius float32
  color v3
  is_light bool
}

type world struct {
  camera camera
  spheres []sphere
}

func world_new () world {
  w := world{}

  w.camera = camera{
    eye: v3{x: 0, y: 4.5, z: 75},
    lt: v3{x: -8, y: 9, z: 50},
    rt: v3{x:8, y: 9, z: 50},
    lb: v3{x:-8, y: 0, z: 50}}

  w.spheres = make([]sphere, 8)

  w.spheres[0] = sphere{center: v3{x: 0, y: -10002, z: 0},
    radius: 9999, color: v3{x: 1, y: 1, z: 1}, is_light: false}

  w.spheres[1] = sphere{center: v3{x: -10012, y: 0, z: 0},
    radius: 9999, color: v3{x: 1, y: 0, z: 0}, is_light: false}

  w.spheres[2] = sphere{center: v3{x: 10012, y: 0, z: 0},
    radius: 9999, color: v3{x: 0, y: 1, z: 0}, is_light: false}

  w.spheres[3] = sphere{center: v3{x: 0, y: 0, z: -10012},
    radius: 9999, color: v3{x: 1, y: 1, z: 1}, is_light: false}

  w.spheres[4] = sphere{center: v3{x: 0, y: 10012, z: 0},
    radius: 9999, color: v3{x: 1, y: 1, z: 1}, is_light: true}

  w.spheres[5] = sphere{center: v3{x: -5, y: 0, z: 2},
    radius: 2, color: v3{x: 1, y: 1, z: 0}, is_light: false}

  w.spheres[6] = sphere{center: v3{x: 0, y: 5, z: -1},
    radius: 4, color: v3{x: 1, y: 0, z: 0}, is_light: false}

  w.spheres[7] = sphere{center: v3{x: 8, y: 5, z: -1},
    radius: 2, color: v3{x: 0, y: 0, z: 1}, is_light: false}


  return w
}

type hit struct {
  distance float32
  point v3
  normal v3
}

var nohit = hit{distance: 1e16}

func sphit (sp sphere, ray ray) hit {
  oc := v3_sub(ray.origin, sp.center)
  a := v3_dot(ray.direction, ray.direction)
  b := v3_dot(oc, ray.direction)
  c := v3_dot(oc, oc) - sp.radius * sp.radius
  dis := b*b - a*c

  if dis > 0 {
    e := float32(math.Sqrt(float64(dis)))
    t := (-b - e) / a

    if t > 0.007 {
      pt := ray_point(ray, t)
      n := v3_unit(v3_sub(pt, sp.center))
      return hit{distance: t, normal: n}
    }

    t = (-b + e) / a

    if t > 0.007 {
      pt := ray_point(ray, t)
      n := v3_unit(v3_sub(pt, sp.center))
      return hit{distance: t, normal: n}
    }

    return nohit
  }

  return nohit
}

func rnd_dome (normal v3) v3 {

  var d float32
  var p v3

  d = -1

  for d < 0 {
    p = v3_unit(v3{x: 2 * rand.Float32() - 1,
    y: 2 * rand.Float32() - 1,
    z: 2 * rand.Float32() - 1})

    d = v3_dot(p, normal)
  }

  return p;
}

func trace (w world, r ray, depth int) v3 {
  did_hit := false
  hit := nohit
  color := zero
  var sp sphere

  for _, s := range w.spheres {
    lh := sphit(s, r)

    if (lh.distance < hit.distance) {
      sp = s
      did_hit = true
      color = s.color
      hit = lh
    }
  }

  if did_hit && depth < MaxDepth {
    if sp.is_light == false {
      nray := ray{origin: hit.point, direction: rnd_dome(hit.normal)}
      ncolor := trace(w, nray, depth + 1)
      at := v3_dot(nray.direction, hit.normal)
      color = v3_mul(color, v3_muls(ncolor,at))
    }
  }

  if did_hit == false || depth >= MaxDepth {
    color = zero
  }

  return color
}

func writeppm (data [][]v3) {
  ppm, _ := os.Create("gorb.ppm")
  defer ppm.Close()

  ppm.WriteString(fmt.Sprintf("P3\n%d %d\n255\n", Width, Height))

  for _, row := range data {
    for _, c := range row {
      r := int(math.Floor(float64(c.x * 255.99)))
      g := int(math.Floor(float64(c.y * 255.99)))
      b := int(math.Floor(float64(c.z * 255.99)))
      ppm.WriteString(fmt.Sprintf("%d %d %d ", r, g, b))
    }
    ppm.WriteString("\n")
  }

  ppm.Sync()
}

func main() {
  data := [][]v3{}
  world := world_new()
  vdu := v3_divs(v3_sub(world.camera.rt, world.camera.lt), Width);
  vdv := v3_divs(v3_sub(world.camera.lb, world.camera.lt), Height);

  data = make([][]v3, Height)

  for y := 0; y < Height; y++ {
    data[y] = make([]v3, Width)

    for x := 0; x < Width; x++ {
      color := zero
      ray := ray{}

      ray.origin = world.camera.eye

      for i :=0; i < Samples; i++ {
        ray.direction =  v3_unit(
          v3_sub(
            v3_add(
              world.camera.lt,
              v3_add(v3_muls(vdu, float32(x) + rand.Float32()),
                     v3_muls(vdv, float32(y) + rand.Float32()))),
            world.camera.eye))
        color = v3_add(color, trace(world, ray, 0))
      }

      color = v3_divs(color, Samples)
      data[y][x] = color
    }
  }

  writeppm(data)

}
