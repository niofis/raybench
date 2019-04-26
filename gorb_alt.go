package main

import (
	"fmt"
	"math"
	"os"
	"time"
)

const WIDTH uint16 = 1280
const HEIGHT uint16 = 720
const SAMPLES uint16 = 50
const MAX_DEPTH uint16 = 5

type v3 struct {
	x, y, z float64
}

func v3_add(dest, a, b *v3) {
	dest.x = a.x + b.x
	dest.y = a.y + b.y
	dest.z = a.z + b.z
}

func v3_mul(dest, a, b *v3) {
	dest.x = a.x * b.x
	dest.y = a.y * b.y
	dest.z = a.z * b.z
}

func v3_sub(dest, a, b *v3) {
	dest.x = a.x - b.x
	dest.y = a.y - b.y
	dest.z = a.z - b.z
}

func v3_dot(a, b *v3) float64 {
	return a.x*b.x + a.y*b.y + a.z*b.z
}

func v3_muls(dest, a *v3, u float64) {
	dest.x = a.x * u
	dest.y = a.y * u
	dest.z = a.z * u
}

func v3_divs(dest, a *v3, u float64) {
	dest.x = a.x / u
	dest.y = a.y / u
	dest.z = a.z / u
}

func v3_norm(v *v3) float64 {
	return float64(math.Sqrt(float64(v.x*v.x + v.y*v.y + v.z*v.z)))
}

func v3_mkunit(dest, v *v3) {
	n := v3_norm(v)

	dest.x = v.x / n
	dest.y = v.y / n
	dest.z = v.z / n
}

type ray struct {
	origin, direction v3
}

func ray_point(dest *v3, r *ray, t float64) {
	dest.x = r.origin.x + r.direction.x*t
	dest.y = r.origin.y + r.direction.y*t
	dest.z = r.origin.z + r.direction.z*t
}

type camera struct {
	eye, lt, rt, lb v3
}

type sphere struct {
	center   v3
	radius   float64
	color    v3
	is_light bool
}

type world struct {
	spheres_count uint16
	spheres       []sphere
	camera        camera
}

func world_new() world {
	var world world

	world.spheres_count = 8
	world.spheres = make([]sphere, int(world.spheres_count))

	world.spheres[0] = sphere{v3{0.0, -10002.0, 0.0}, 9999.0, v3{1.0, 1.0, 1.0}, false}
	world.spheres[1] = sphere{v3{-10012.0, 0.0, 0.0}, 9999.0, v3{1.0, 0.0, 0.0}, false}
	world.spheres[2] = sphere{v3{10012.0, 0.0, 0.0}, 9999.0, v3{0.0, 1.0, 0.0}, false}
	world.spheres[3] = sphere{v3{0.0, 0.0, -10012.0}, 9999.0, v3{1.0, 1.0, 1.0}, false}
	world.spheres[4] = sphere{v3{0.0, 10012.0, 0.0}, 9999.0, v3{1.0, 1.0, 1.0}, false}
	world.spheres[4].is_light = true

	world.spheres[5] = sphere{v3{-5.0, 0.0, 2.0}, 2.0, v3{1.0, 1.0, 0.0}, false}
	world.spheres[6] = sphere{v3{0.0, 5.0, -1.0}, 4.0, v3{1.0, 0.0, 0.0}, false}
	world.spheres[7] = sphere{v3{8.0, 5.0, -1.0}, 2.0, v3{0.0, 0.0, 1.0}, false}

	world.camera.eye.x = 0.0
	world.camera.eye.y = 4.5
	world.camera.eye.z = 75.0

	world.camera.lt.x = -8.0
	world.camera.lt.y = 9.0
	world.camera.lt.z = 50.0

	world.camera.rt.x = 8.0
	world.camera.rt.y = 9.0
	world.camera.rt.z = 50.0

	world.camera.lb.x = -8.0
	world.camera.lb.y = 0.0
	world.camera.lb.z = 50.0

	return world
}

type hit struct {
	dist          float64
	point, normal v3
}

func hit_sphere(sp *sphere, ray *ray, hit *hit) bool {
	var oc v3
	v3_sub(&oc, &ray.origin, &sp.center)

	a := v3_dot(&ray.direction, &ray.direction)
	b := v3_dot(&oc, &ray.direction)
	c := v3_dot(&oc, &oc) - (sp.radius * sp.radius)
	dis := b*b - a*c

	if dis > 0.0 {
		e := float64(math.Sqrt(float64(dis)))
		t := (-b - e) / a

		if t > 0.007 {
			hit.dist = t
			ray_point(&hit.point, ray, t)
			v3_sub(&hit.normal, &hit.point, &sp.center)
			v3_mkunit(&hit.normal, &hit.normal)
			return true
		}

		t = (-b + e) / a
		if t > 0.007 {
			hit.dist = t
			ray_point(&hit.point, ray, t)
			v3_sub(&hit.normal, &hit.point, &sp.center)
			v3_mkunit(&hit.normal, &hit.normal)
			return true
		}

		return false
	}

	return false
}

var _x uint32 = 123456789
var _y uint32 = 362436069
var _z uint32 = 521288629
var _w uint32 = 88675123

//https://codingforspeed.com/using-faster-psudo-random-generator-xorshift/
func xor128() uint32 {
	var t uint32
	t = _x ^ (_x << 11)
	_x = _y
	_y = _z
	_z = _w
	_w = _w ^ (_w >> 19) ^ (t ^ (t >> 8))
	return _w
}

func randf() float64 {
	const UINT32_MAX = float64(4294967295)
	return float64(xor128()) / UINT32_MAX
}

func rnd_dome(normal *v3) v3 {
	var p v3
	var d float64 = -1.0

	for d <= 0 {
		p.x = 2.0*randf() - 1.0
		p.y = 2.0*randf() - 1.0
		p.z = 2.0*randf() - 1.0

		v3_mkunit(&p, &p)

		d = v3_dot(&p, normal)
	}

	return p
}

func trace(world *world, r *ray, depth uint16) v3 {
	var color v3
	var did_hit bool
	h := hit{dist: 1e15}
	var sp *sphere
	for i := uint16(0); i < world.spheres_count; i++ {
		var res hit
		if hit_sphere(&world.spheres[i], r, &res) {
			if res.dist > 0.0001 && res.dist < h.dist {
				sp = &world.spheres[i]
				did_hit = true
				color = sp.color
				h = res
			}
		}
	}

	if did_hit == true && depth < MAX_DEPTH {
		if sp.is_light == false {
			var nray ray
			nray.origin = h.point
			nray.direction = rnd_dome(&h.normal)
			var ncolor v3
			ncolor = trace(world, &nray, depth+1)
			at := v3_dot(&nray.direction, &h.normal)
			v3_muls(&ncolor, &ncolor, at)
			v3_mul(&color, &color, &ncolor)
		} else {
			color = sp.color
		}
	}

	if did_hit == false || depth >= MAX_DEPTH {
		color.x = 0
		color.y = 0
		color.z = 0
	}

	return color
}

func writeppm(data []v3) {
	ppm, _ := os.Create("gorb.ppm")
	defer ppm.Close()

	ppm.WriteString(fmt.Sprintf("P3\n%d %d\n255\n", WIDTH, HEIGHT))

	for y := uint16(0); y < HEIGHT; y++ {
		for x := uint16(0); x < WIDTH; x++ {
			r := int(math.Floor(float64(data[y*WIDTH+x].x * 255.99)))
			g := int(math.Floor(float64(data[y*WIDTH+x].y * 255.99)))
			b := int(math.Floor(float64(data[y*WIDTH+x].z * 255.99)))
			ppm.WriteString(fmt.Sprintf("%d %d %d ", r, g, b))
		}
		ppm.WriteString("\n")
	}

	ppm.Sync()
}

func main() {
	start := time.Now()
	world := world_new()

	data := make([]v3, int(HEIGHT)*int(WIDTH))

	var vdu v3
	v3_sub(&vdu, &world.camera.rt, &world.camera.lt)
	v3_divs(&vdu, &vdu, float64(WIDTH))

	var vdv v3
	v3_sub(&vdv, &world.camera.lb, &world.camera.lt)
	v3_divs(&vdv, &vdv, float64(HEIGHT))

	for y := uint16(0); y < HEIGHT; y++ {
		for x := uint16(0); x < WIDTH; x++ {
			var r ray
			r.origin = world.camera.eye

			var u, v, c v3
			for s := uint16(0); s < SAMPLES; s++ {
				r.direction = world.camera.lt

				v3_muls(&u, &vdu, float64(x)+randf())
				v3_muls(&v, &vdv, float64(y)+randf())

				v3_add(&r.direction, &r.direction, &u)
				v3_add(&r.direction, &r.direction, &v)

				v3_sub(&r.direction, &r.direction, &r.origin)

				v3_mkunit(&r.direction, &r.direction)
				u = trace(&world, &r, 0)
				v3_add(&c, &c, &u)
			}

			v3_divs(&c, &c, float64(SAMPLES))

			data[y*WIDTH+x] = c
		}
	}
	fmt.Println(time.Since(start))

	writeppm(data)
}
