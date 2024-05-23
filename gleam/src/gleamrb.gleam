import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/result

const width = 1280

const height = 720

const samples = 50

const max_depth = 5

pub type Vector {
  Vector(x: Float, y: Float, z: Float)
}

pub type Point {
  Point(x: Float, y: Float, z: Float)
}

pub type Color {
  Color(r: Float, g: Float, b: Float)
}

pub type Ray {
  Ray(origin: Point, direction: Vector)
}

pub type Hit {
  Hit(dist: Float, point: Point, normal: Vector)
}

pub type Camera {
  Camera(eye: Point, left_top: Point, right_top: Point, left_bottom: Point)
}

pub type Sphere {
  Sphere(center: Point, radius: Float, color: Color, is_light: Bool)
}

pub type World {
  World(camera: Camera, spheres: List(Sphere))
}

pub type Random {
  Random(x: Int, y: Int, z: Int, w: Int)
}

fn new_random() -> Random {
  Random(123_456_789, 362_436_069, 521_288_629, 88_675_123)
}

fn next_random(random: Random) -> #(Float, Random) {
  let t =
    int.bitwise_exclusive_or(
      random.x,
      int.bitwise_and(int.bitwise_shift_left(random.x, 11), 0xFFFFFFFF),
    )
  let x = random.y
  let y = random.z
  let z = random.w
  let w =
    int.bitwise_and(
      int.bitwise_exclusive_or(
        {
          int.bitwise_exclusive_or(
            random.w,
            int.bitwise_shift_right(random.w, 19),
          )
        },
        int.bitwise_exclusive_or(t, int.bitwise_shift_right(t, 8)),
      ),
      0xFFFFFFFF,
    )
  let r = int.to_float(w) /. 4_294_967_295.0

  #(r, Random(x, y, z, w))
}

fn add_point_vector(a: Point, b: Vector) -> Point {
  Point(a.x +. b.x, a.y +. b.y, a.z +. b.z)
}

fn add_vector_vector(a: Vector, b: Vector) -> Vector {
  Vector(a.x +. b.x, a.y +. b.y, a.z +. b.z)
}

fn add_color_color(a: Color, b: Color) -> Color {
  Color(a.r +. b.r, a.g +. b.g, a.b +. b.b)
}

fn sub(a: Point, b: Point) -> Vector {
  Vector(a.x -. b.x, a.y -. b.y, a.z -. b.z)
}

fn div(a: Vector, b: Float) -> Vector {
  Vector(a.x /. b, a.y /. b, a.z /. b)
}

fn mul(a: Vector, b: Float) -> Vector {
  Vector(a.x *. b, a.y *. b, a.z *. b)
}

fn mul_color_float(a: Color, b: Float) -> Color {
  Color(a.r *. b, a.g *. b, a.b *. b)
}

fn mul_color_color(a: Color, b: Color) -> Color {
  Color(a.r *. b.r, a.g *. b.g, a.b *. b.b)
}

fn div_color_int(a: Color, b: Int) -> Color {
  let b = int.to_float(b)
  Color(a.r /. b, a.g /. b, a.b /. b)
}

fn norm(a: Vector) -> Float {
  float.square_root(a.x *. a.x +. a.y *. a.y +. a.z *. a.z)
  |> result.unwrap(0.0)
}

fn dot(a: Vector, b: Vector) -> Float {
  { a.x *. b.x } +. { a.y *. b.y } +. { a.z *. b.z }
}

fn unit(a: Vector) -> Vector {
  a
  |> div(
    a
    |> norm,
  )
}

fn new_world() -> World {
  World(
    Camera(
      eye: Point(0.0, 4.5, 75.0),
      left_top: Point(-8.0, 9.0, 50.0),
      right_top: Point(8.0, 9.0, 50.0),
      left_bottom: Point(-8.0, 0.0, 50.0),
    ),
    [
      Sphere(Point(0.0, -10_002.0, 0.0), 9999.0, Color(1.0, 1.0, 1.0), False),
      Sphere(Point(-10_012.0, 0.0, 0.0), 9999.0, Color(1.0, 0.0, 0.0), False),
      Sphere(Point(10_012.0, 0.0, 0.0), 9999.0, Color(0.0, 1.0, 0.0), False),
      Sphere(Point(0.0, 0.0, -10_012.0), 9999.0, Color(1.0, 1.0, 1.0), False),
      Sphere(Point(0.0, 10_012.0, 0.0), 9999.0, Color(1.0, 1.0, 1.0), True),
      Sphere(Point(-5.0, 0.0, 2.0), 2.0, Color(1.0, 1.0, 0.0), False),
      Sphere(Point(0.0, 5.0, -1.0), 4.0, Color(1.0, 0.0, 0.0), False),
      Sphere(Point(8.0, 5.0, -1.0), 2.0, Color(0.0, 0.0, 1.0), False),
    ],
  )
}

fn sphere_intersect(sphere: Sphere, ray: Ray) -> #(Bool, Float) {
  let oc =
    ray.origin
    |> sub(sphere.center)
  let a =
    ray.direction
    |> dot(ray.direction)
  let b =
    {
      oc
      |> dot(ray.direction)
    }
    *. -1.0
  let c =
    {
      oc
      |> dot(oc)
    }
    -. { sphere.radius *. sphere.radius }
  let dis = { b *. b } -. { a *. c }

  case dis {
    d if d <=. 0.0 -> #(False, 0.0)
    _ -> {
      let e = result.unwrap(float.square_root(dis), 0.0)
      let t1 = { b -. e } /. a
      let t2 = { b +. e } /. a
      case t1, t2 {
        t, _ if t >. 0.007 -> {
          #(True, t)
        }
        _, t if t >. 0.007 -> {
          #(True, t)
        }
        _, _ -> #(False, 0.0)
      }
    }
  }
}

fn random_dome(normal: Vector, random: Random) -> #(Vector, Random) {
  let #(x, random) = next_random(random)
  let #(y, random) = next_random(random)
  let #(z, random) = next_random(random)
  let p =
    unit(Vector({ 2.0 *. x } -. 1.0, { 2.0 *. y } -. 1.0, { 2.0 *. z } -. 1.0))
  case dot(p, normal) {
    d if d >=. 0.0 -> #(p, random)
    _ -> random_dome(normal, random)
  }
}

fn trace(ray: Ray, depth: Int, world: World, random: Random) -> #(Color, Random) {
  case depth {
    d if d >= max_depth -> #(Color(0.0, 0.0, 0.0), random)
    _ -> {
      let closest_hit =
        list.map(world.spheres, fn(sphere) -> #(Bool, Float, Sphere) {
          let #(is_hit, distance) = sphere_intersect(sphere, ray)
          #(is_hit, distance, sphere)
        })
        |> list.filter(fn(res) { res.0 })
        |> list.fold(
          #(
            False,
            0.0,
            result.unwrap(
              list.first(world.spheres),
              Sphere(
                Point(0.0, -10_002.0, 0.0),
                9999.0,
                Color(1.0, 1.0, 1.0),
                False,
              ),
            ),
          ),
          fn(closest_hit, hit) {
            case closest_hit, hit {
              #(False, _, _), #(True, _, _) -> hit
              #(True, _, _), #(True, _, _) if closest_hit.1 >. hit.1 -> hit
              _, _ -> closest_hit
            }
          },
        )
      case closest_hit {
        #(True, _, sphere) if sphere.is_light -> #(sphere.color, random)
        #(True, _, _) -> {
          let hit_point =
            add_point_vector(ray.origin, mul(ray.direction, closest_hit.1))
          let normal = unit(sub(hit_point, { closest_hit.2 }.center))
          let #(new_direction, random) = random_dome(normal, random)
          let new_ray = Ray(hit_point, new_direction)
          let #(n_color, random) = trace(new_ray, depth + 1, world, random)
          let at = dot(new_direction, normal)
          let color =
            mul_color_color(
              { closest_hit.2 }.color,
              mul_color_float(n_color, at),
            )
          #(color, random)
        }
        _ -> #(Color(0.0, 0.0, 0.0), random)
      }
    }
  }
}

fn render_samples(
  x: Int,
  y: Int,
  vdu: Vector,
  vdv: Vector,
  sample: Int,
  sampled_pixels: List(Color),
  world: World,
  random: Random,
) -> #(List(Color), Random) {
  case sample {
    s if s >= samples -> #(sampled_pixels, random)
    _ -> {
      let #(value, random) = next_random(random)
      let rx =
        {
          x
          |> int.to_float
        }
        +. value
      let #(value, random) = next_random(random)
      let ry =
        {
          y
          |> int.to_float
        }
        +. value
      let origin = world.camera.eye
      let direction =
        world.camera.left_top
        |> add_point_vector({
          vdu
          |> mul(rx)
          |> add_vector_vector(
            vdv
            |> mul(ry),
          )
        })
        |> sub(origin)
        |> unit

      let ray = Ray(origin, direction)

      let #(color, random) = trace(ray, 0, world, random)

      render_samples(
        x,
        y,
        vdu,
        vdv,
        sample + 1,
        list.append(sampled_pixels, [color]),
        world,
        random,
      )
    }
  }
}

fn render(
  pixel: Int,
  max_pixels: Int,
  pixels: List(Color),
  vdu: Vector,
  vdv: Vector,
  world: World,
  random: Random,
) -> List(Color) {
  case pixel {
    p if p >= max_pixels -> pixels
    _ -> {
      let x = pixel % width
      let y = pixel / width
      let #(sampled_pixels, random) =
        render_samples(x, y, vdu, vdv, 0, [], world, random)

      let color =
        sampled_pixels
        |> list.fold(Color(0.0, 0.0, 0.0), add_color_color)
        |> div_color_int(samples)

      render(
        pixel + 1,
        max_pixels,
        list.append(pixels, [color]),
        vdu,
        vdv,
        world,
        random,
      )
    }
  }
}

fn print_ppm(pixels: List(Color)) {
  io.println(
    "P3\n" <> int.to_string(width) <> " " <> int.to_string(height) <> "\n255\n",
  )
  pixels
  |> list.map(fn(c) {
    c.r *. 255.99
    |> float.truncate
    |> int.to_string
    <> " "
    <> c.g *. 255.99
    |> float.truncate
    |> int.to_string
    <> " "
    <> c.b *. 255.99
    |> float.truncate
    |> int.to_string
    <> " "
  })
  |> list.each(io.print)
}

pub fn main() {
  let world = new_world()
  let vdu =
    world.camera.right_top
    |> sub(world.camera.left_top)
    |> div(int.to_float(width))
  let vdv =
    world.camera.left_bottom
    |> sub(world.camera.left_top)
    |> div(int.to_float(height))
  let random = new_random()
  let max_pixels = width * height
  let pixels = render(0, max_pixels, [], vdu, vdv, world, random)
  print_ppm(pixels)
}
