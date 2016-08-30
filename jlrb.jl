

const COEFF = 80
const WIDTH = 16 * COEFF
const HEIGHT = 9 * COEFF
const SAMPLES = 50
const MAX_DEPTH = 5

immutable Vec3
	x::Float32
	y::Float32
	z::Float32
end

function add(x::Vec3, y::Vec3)
	Vec3(x.x + y.x, x.y + y.y, x.z + y.z)
end

function mul(x::Vec3, y::Vec3)
	Vec3(x.x * y.x, x.y * y.y, x.z * y.z)
end

function sub(x::Vec3, y::Vec3)
	Vec3(x.x - y.x, x.y - y.y, x.z - y.z)
end

function dot(x::Vec3, y::Vec3)
	x.x * y.x + x.y * y.y + x.z * y.z
end

function muls(x::Vec3, u::Float32)
	Vec3(x.x * u, x.y * u, x.z * u)
end

function divs(x::Vec3, u::Float32)
	Vec3(x.x / u, x.y / u, x.z / u)
end

function norm(x::Vec3)
	sqrt(dot(x, x))
end

function mkunit(x::Vec3)
	divs(x, norm(x))
end

immutable Ray
	origin::Vec3
	direction::Vec3
end

function ray_point(r::Ray, t::Float32)
	add(r.origin, muls(r.direction, t))
end

immutable Camera
	eye::Vec3
	lt::Vec3
	rt::Vec3
	lb::Vec3
end

immutable Sphere
	center::Vec3
	radius::Float32
	color::Vec3
	is_light::Bool
end

type World
	spheres::Array{Sphere, 1}
	camera::Camera
end

function randf()
	rand(Float32)
end

function rnd_dome(normal::Vec3)
	p = Vec3(0, 0, 0)

	while true
		p = mkunit(Vec3(2.0 * randf() - 1.0, 2.0 * randf() - 1.0, 2.0 * randf() - 1.0))
		d = dot(p, normal)
		if d > 0 then
			break
		end
	end

	return p
end

immutable Hit
	dist::Float32
	point::Vec3
	normal::Vec3
end

function hit_sphere(sp::Sphere, ray::Ray)
	oc = sub(ray.origin, sp.center)
	a = dot(ray.direction, ray.direction)
	b = dot(oc, ray.direction)
	c = dot(oc, oc) - (sp.radius * sp.radius)
	dis = b*b - a*c

	res = Hit(0, Vec3(0, 0, 0), Vec3(0, 0, 0))

	if dis > 0.0 then
		e = sqrt(dis)

		t = (-b - e) / a
		if t > 0.007 then
			pt = ray_point(ray, t)
			res = Hit(t, pt, mkunit(sub(pt, sp.center)))
			return (true, res)
		end

		t = (-b + e) / a
		if t > 0.007 then
			pt = ray_point(ray, t)
			res = Hit(t, pt, mkunit(sub(pt, sp.center)))
			return (true, res)
		end

		return (false, res)
	end

	return (false, res)
end

function trace(world::World, r::Ray, depth)
	color = Vec3(0, 0, 0)
	did_hit = false
	fhit = Hit(1e15, Vec3(0, 0, 0), Vec3(0, 0, 0))

	sp = world.spheres[1]
	for i = 1:size(world.spheres,1)
		(hit_ok, res) = hit_sphere(world.spheres[i], r)
		if hit_ok && res.dist > 0.0001 && res.dist < fhit.dist then
			sp = world.spheres[i]
			did_hit = true
			color = sp.color
			fhit = res
		end
	end

	if did_hit && depth < MAX_DEPTH then
		if !sp.is_light then
			nray = Ray(fhit.point, rnd_dome(fhit.normal))
			ncolor = trace(world, nray, depth + 1)
			at = dot(nray.direction, fhit.normal)
			color = mul(color, muls(ncolor, at))
		else
			color = sp.color
		end
	end

	if !did_hit || depth >= MAX_DEPTH then
		return Vec3(0, 0, 0)
	end

	return color
end

function init_world()
	c = Camera(
		Vec3( 0.0, 4.5, 75.0),
		Vec3(-8.0, 9.0, 50.0),
		Vec3( 8.0, 9.0, 50.0),
		Vec3(-8.0, 0.0, 50.0)
	)

	w = World([], c)

	push!(w.spheres, Sphere(Vec3(0.0, -10002.0, 0.0), 9999.0,	Vec3(1.0,1.0,1.0), false))
	push!(w.spheres, Sphere(Vec3(-10012.0, 0.0, 0.0), 9999.0,	Vec3(1.0,0.0,0.0), false))
	push!(w.spheres, Sphere(Vec3(10012.0, 0.0, 0.0),	9999.0,	Vec3(0.0,1.0,0.0), false))
	push!(w.spheres, Sphere(Vec3(0.0, 0.0, -10012.0), 9999.0,	Vec3(1.0,1.0,1.0), false))
	push!(w.spheres, Sphere(Vec3(0.0, 10012.0, 0.0),	9999.0,	Vec3(1.0,1.0,1.0), true))
	push!(w.spheres, Sphere(Vec3(-5.0, 0.0, 2.0),		2.0,	Vec3(1.0,1.0,0.0), false))
	push!(w.spheres, Sphere(Vec3(0.0, 5.0, -1.0),		4.0,	Vec3(1.0,0.0,0.0), false))
	push!(w.spheres, Sphere(Vec3(8.0, 5.0, -1.0),		2.0,	Vec3(0.0,0.0,1.0), false))

	return w
end

function writeppm(data::Array{Vec3,1})
	ppm = open("jlrb.ppm","w+")
	@printf(ppm, "P3\n%u %u\n255\n", WIDTH, HEIGHT)
	for y = 0:HEIGHT-1
		for x = 1:WIDTH
			local c = data[y * WIDTH + x]
			r = convert(UInt16, floor(c.x * 255.99))
			g = convert(UInt16, floor(c.y * 255.99))
			b = convert(UInt16, floor(c.z * 255.99))
			@printf(ppm, "%u %u %u ", r, g, b)
		end
		@printf(ppm, "\n")
	end
	close(ppm)
end

function main()
	world = init_world()
	data = fill(Vec3(0, 0, 0), (WIDTH * HEIGHT))

	vdu = divs(sub(world.camera.rt, world.camera.lt), convert(Float32, WIDTH))
	vdv = divs(sub(world.camera.lb, world.camera.lt), convert(Float32, HEIGHT))

	for y = 0:HEIGHT-1
		for x = 1:WIDTH
			c = Vec3(0, 0, 0)
			for s = 1:SAMPLES
				u = muls(vdu, convert(Float32, x) + randf())
				v = muls(vdv, convert(Float32, y) + randf())
				d = mkunit(sub(add(add(world.camera.lt, u), v), world.camera.eye))
				r = Ray(world.camera.eye, d)
				c = add(c, trace(world, r, 0))
			end
			data[y * WIDTH + x] = divs(c, convert(Float32, SAMPLES))
		end
	end

	writeppm(data)
end

main()

