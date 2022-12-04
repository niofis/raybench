package main

import "core:fmt"
import "core:math"

WIDTH       : int : 1280
HEIGHT      : int : 720
SAMPLES     : int : 50
MAXDEPTH    : int : 5

V3 :: [3]f32

Rng :: struct {
    x, y, z, w: u32,
}

rand_state := Rng{ 123456789, 362436069, 521288629, 88675123}

rand_next :: proc() -> f32 {
    max: f32 = 4294967295.0
    t: u32 = rand_state.x ~ (rand_state.x << 11)
    rand_state.x = rand_state.y
    rand_state.y = rand_state.z
    rand_state.z = rand_state.w
    rand_state.w = rand_state.w ~ (rand_state.w >> 19) ~ (t ~ (t >> 8))
    return f32(rand_state.w) / max
}

random_dome :: proc(normal: ^V3) -> V3 {
    n := proc() -> f32 { return rand_next() * 2.0 - 1.0 }
    for {
        v := V3 { n(), n(), n() }
        v = unit(&v)
        if dot(&v, normal) >= 0.0 do return v
    }
}

Ray :: struct {
    origin: V3,
    direction: V3,
}

point :: proc(ray: ^Ray, t: f32) -> V3 {
    return ray.origin + (t * ray.direction)
}

Camera :: struct {
    eye: V3,
    lt: V3,
    rt: V3,
    lb: V3,
}

Sphere :: struct {
    center: V3,
    radius: f32,
    color: V3,
    is_light: bool,
}

Hit :: struct {
    dist: f32,
    point: V3,
    normal: V3,
}

dot :: proc(a, b: ^V3) -> f32 {
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

norm :: proc(a: ^V3) -> f32 {
    return math.sqrt(a[0] * a[0] + a[1] * a[1] + a[2] * a[2])
}

unit :: proc(a: ^V3) -> V3 {
    return a^ / norm(a)
}

hit :: proc(sphere: ^Sphere, ray: ^Ray) -> Maybe(Hit) {
    oc := ray.origin - sphere.center
    a := dot(&ray.direction, &ray.direction)
    b := dot(&oc, &ray.direction)
    c := dot(&oc, &oc) - sphere.radius * sphere.radius;
    dis := b * b - a * c

    if dis > 0 {
        e := math.sqrt(dis)
        t := (-b - e) / a

        if t > 0.007 {
            pt := point(ray, t)
            n := pt - sphere.center
            n = unit(&n)
            return Hit { t, pt, n}
        }

        t = (-b + e) / a
        if t > 0.007 {
            pt := point(ray, t)
            n := pt - sphere.center
            n = unit(&n)
            return Hit { t, pt, n}
        }
    }

    return nil
}

World :: struct {
    spheres: [8] Sphere,
    camera: Camera,
}

world := World {
    spheres = [8] Sphere {
        Sphere {
            center = V3 { 0.0, -10002.0,  0.0 },
            radius = 9999.0,
            color = V3 { 1.0, 1.0,  1.0 },
            is_light = false,
        },
        Sphere {
            center = V3 { -10012.0, 0.0,  0.0 },
            radius = 9999.0,
            color = V3 { 1.0, 0.0,  0.0 },
            is_light = false,
        },
        Sphere {
            center = V3 { 10012.0, 0.0,  0.0 },
            radius = 9999.0,
            color = V3 { 0.0, 1.0,  0.0 },
            is_light = false,
        },
        Sphere {
            center = V3 { 0.0, 0.0,  -10012.0 },
            radius = 9999.0,
            color = V3 { 1.0, 1.0,  1.0 },
            is_light = false,
        },
        Sphere {
            center = V3 { 0.0, 10012.0,  0.0 },
            radius = 9999.0,
            color = V3 { 1.0, 1.0,  1.0 },
            is_light = true,
        },
        Sphere {
            center = V3 { -5.0, 0.0,  2.0 },
            radius = 2.0,
            color = V3 { 1.0, 1.0,  0.0 },
            is_light = false,
        },
        Sphere {
            center = V3 { 0.0, 5.0,  -1.0 },
            radius = 4.0,
            color = V3 { 1.0, 0.0,  0.0 },
            is_light = false,
        },
        Sphere {
            center = V3 { 8.0, 5.0,  -1.0 },
            radius = 2.0,
            color = V3 { 0.0, 0.0,  1.0 },
            is_light = false,
        },
    },
    camera = Camera {
        eye = V3 { 0.0, 4.5, 75.0 },
        lt = V3 { -8.0, 9.0, 50.0 },
        rt = V3 { 8.0, 9.0, 50.0 },
        lb = V3 { -8.0, 0.0, 50.0 },
    },
}

trace :: proc(world: ^World, ray: ^Ray, depth: int) -> V3 {
    if depth >= MAXDEPTH do return V3 { 0, 0, 0 }

    closest_hit : Maybe(Hit) = nil
    closest_sphere_idx := -1

    for idx in 0..<len(world.spheres) {
        sphere := world.spheres[idx]
        h, ok := hit(&sphere, ray).?
        c, ok2: = closest_hit.?
        if ok && !ok2 {
            closest_hit = h
            closest_sphere_idx = idx
        } else if ok && ok2 && h.dist < c.dist {
            closest_hit = h
            closest_sphere_idx = idx
        }
    }

    if hit, ok := closest_hit.?; ok {
        sphere := world.spheres[closest_sphere_idx]
        if sphere.is_light {
            return sphere.color
        } else {
            nray := Ray { origin = hit.point, direction = random_dome(&hit.normal) }
            ncolor := trace(world, &nray, depth + 1)
            at := dot(&nray.direction, &hit.normal)
            return sphere.color * (ncolor * at)
        }
    }

    return V3 { 0, 0, 0 }
}

write_ppm :: proc(data: [dynamic]V3) {
    fmt.printf("P3\n%i %i\n255\n", WIDTH, HEIGHT)
    i := 0
    for y in 0..<HEIGHT {
        for x in 0..<WIDTH {
            px := data[i]
            fmt.printf("%i %i %i ",
                u8(px[0] * 255.99),
                u8(px[1] * 255.99),
                u8(px[2] * 255.99),
            )
            i += 1
        }
        
        fmt.println("")
    }
}

main :: proc() {
    vdu := (world.camera.rt - world.camera.lt) / f32(WIDTH)
    vdv := (world.camera.lb - world.camera.lt) / f32(HEIGHT)
    data := make([dynamic]V3, WIDTH * HEIGHT)
    defer delete(data)

    idx := 0
    for y in 0..<HEIGHT {
        for x in 0..<WIDTH {
            color := V3 {0, 0, 0}
            for _ in 0..<SAMPLES {
                dir := (world.camera.lt + (vdu * (f32(x) + rand_next()) + vdv * (f32(y) + rand_next()))) - world.camera.eye;
                ray := Ray {
                    origin = world.camera.eye,
                    direction = unit(&dir),
                }
                color += trace(&world, &ray, 0)
            }
            data[idx] = color / f32(SAMPLES)
            idx += 1
        }
    }
    write_ppm(data)
}