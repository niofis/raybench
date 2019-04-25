import Base.+, Base.-, Base.*, Base./
using Printf

const COEFF = 80
const WIDTH = 16 * COEFF
const HEIGHT = 9 * COEFF
const SAMPLES = 50
const MAX_DEPTH = 5

struct Vec3
	x::Float32
	y::Float32
	z::Float32
end

const ZERO3 = Vec3(0f0, 0f0, 0f0)

+(x::Vec3, y::Vec3) = Vec3(x.x + y.x, x.y + y.y, x.z + y.z)
-(x::Vec3, y::Vec3) = Vec3(x.x - y.x, x.y - y.y, x.z - y.z)
*(x::Vec3, y::Vec3) = Vec3(x.x * y.x, x.y * y.y, x.z * y.z)
*(u::Float32, x::Vec3) = Vec3(x.x * u, x.y * u, x.z * u)
/(x::Vec3, u::Float32) = Vec3(x.x / u, x.y / u, x.z / u)

dot(x::Vec3, y::Vec3) = x.x * y.x + x.y * y.y + x.z * y.z
norm(x::Vec3) = sqrt(dot(x, x))
mkunit(x::Vec3) = x / norm(x)

struct Ray
	origin::Vec3
	direction::Vec3
end

ray_point(r::Ray, t::Float32) = r.origin + t * r.direction

mutable struct Camera
	eye::Vec3
	lt::Vec3
	rt::Vec3
	lb::Vec3
end

mutable struct Sphere
	center::Vec3
	radius::Float32
	color::Vec3
	is_light::Bool
end

mutable struct World
	spheres::Array{Sphere, 1}
	camera::Camera
end

randf() = rand(Float32)

function rnd_dome(normal::Vec3)
	p = ZERO3
	d = -1f0

	while d < 0f0
		p = mkunit(Vec3(2f0 * randf() - 1f0, 2f0 * randf() - 1f0, 2f0 * randf() - 1f0))
		d = dot(p, normal)
	end

	return p
end

struct Hit
	dist::Float32
	point::Vec3
	normal::Vec3
end

const NO_HIT = Hit(-1f0, ZERO3, ZERO3)
const MAX_HIT = Hit(1f15, ZERO3, ZERO3)

function hit_sphere(sp::Sphere, ray::Ray)
	oc = ray.origin - sp.center
	a = dot(ray.direction, ray.direction)
	b = dot(oc, ray.direction)
	c = dot(oc, oc) - (sp.radius * sp.radius)
	dis = b * b - a * c

	if dis > 0f0
		e = sqrt(dis)

		t = (-b - e) / a
		if t > 0.007f0
			pt = ray_point(ray, t)
			return Hit(t, pt, mkunit(pt - sp.center))
		end

		t = (-b + e) / a
		if t > 0.007f0
			pt = ray_point(ray, t)
			return Hit(t, pt, mkunit(pt - sp.center))
		end
	end

	NO_HIT
end

function trace(world::World, r::Ray, depth)
	if depth >= MAX_DEPTH
		return ZERO3
	end

	fhit = MAX_HIT

	sp = world.spheres[1]
	for sphere in world.spheres
		res = hit_sphere(sphere, r)
		if res.dist > 0.0001f0 && res.dist < fhit.dist
			sp = sphere
			fhit = res
		end
	end

	if fhit.dist == MAX_HIT.dist
		return ZERO3
	end

	if sp.is_light
		return sp.color
	end

	nray = Ray(fhit.point, rnd_dome(fhit.normal))
	ncolor = trace(world, nray, depth + 1)
	at = dot(nray.direction, fhit.normal)
	return sp.color * (at * ncolor)
end

function init_world()
	c = Camera(
		Vec3( 0.0, 4.5, 75.0),
		Vec3(-8.0, 9.0, 50.0),
		Vec3( 8.0, 9.0, 50.0),
		Vec3(-8.0, 0.0, 50.0)
	)

	w = World([], c)

	push!(w.spheres, Sphere(Vec3(0.0, -10002.0, 0.0), 9999.0, Vec3(1.0, 1.0, 1.0), false))
	push!(w.spheres, Sphere(Vec3(-10012.0, 0.0, 0.0), 9999.0, Vec3(1.0, 0.0, 0.0), false))
	push!(w.spheres, Sphere(Vec3(10012.0, 0.0, 0.0),  9999.0, Vec3(0.0, 1.0, 0.0), false))
	push!(w.spheres, Sphere(Vec3(0.0, 0.0, -10012.0), 9999.0, Vec3(1.0, 1.0, 1.0), false))
	push!(w.spheres, Sphere(Vec3(0.0, 10012.0, 0.0),  9999.0, Vec3(1.0, 1.0, 1.0), true))
	push!(w.spheres, Sphere(Vec3(-5.0, 0.0, 2.0),     2.0,    Vec3(1.0, 1.0, 0.0), false))
	push!(w.spheres, Sphere(Vec3(0.0, 5.0, -1.0),     4.0,    Vec3(1.0, 0.0, 0.0), false))
	push!(w.spheres, Sphere(Vec3(8.0, 5.0, -1.0),     2.0,    Vec3(0.0, 0.0, 1.0), false))

	return w
end

function writeppm(data::Array{Vec3,1})
	ppm = open("jlrb.ppm","w+")
	@printf(ppm, "P3\n%u %u\n255\n", WIDTH, HEIGHT)
	for y = 0:HEIGHT-1
		for x = 1:WIDTH
			local c = data[y * WIDTH + x]
			r = convert(UInt16, floor(c.x * 255.99f0))
			g = convert(UInt16, floor(c.y * 255.99f0))
			b = convert(UInt16, floor(c.z * 255.99f0))
			@printf(ppm, "%u %u %u ", r, g, b)
		end
		@printf(ppm, "\n")
	end
	close(ppm)
end

function main()
	world = init_world()
	data = fill(ZERO3, (WIDTH * HEIGHT))

	vdu = (world.camera.rt - world.camera.lt) / convert(Float32, WIDTH)
	vdv = (world.camera.lb - world.camera.lt) / convert(Float32, HEIGHT)

	for y = 0:HEIGHT - 1
		for x = 0:WIDTH - 1
			c = ZERO3
			for s = 1:SAMPLES
				u = (convert(Float32, x) + randf()) * vdu
				v = (convert(Float32, y) + randf()) * vdv
				d = mkunit(world.camera.lt + u + v - world.camera.eye)
				r = Ray(world.camera.eye, d)
				c += trace(world, r, 0)
			end
			data[1 + y * WIDTH + x] = c / convert(Float32, SAMPLES)
		end
	end

	writeppm(data)
end

@time main()