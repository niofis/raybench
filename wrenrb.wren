class Random {
    static init() {
        __x = 123456789
        __y = 362436069
        __z = 521288629
        __w = 88675123
    }

    static next() {
        var t = __x ^ (__x << 11)
        __x = __y
        __y = __z
        __z = __w
        __w = __w ^ (__w >> 19) ^ (t ^ (t >> 8))
        return __w / 4294967295
    }
}

class V3 {
    construct new(x, y, z) {
        _x = x
        _y = y
        _z = z
    }

    x { _x }
    y { _y }
    z { _z }

    +(other) { V3.new(_x + other.x, _y + other.y, _z + other.z) }
    -(other) { V3.new(_x - other.x, _y - other.y, _z - other.z) }
    /(other) { 
        if (other is Num) {
            return V3.new(_x / other, _y / other, _z / other)
        } else {
            System.print("/ not implemented")   
        }
    }
    *(other) {
        if (other is Num) {
            return V3.new(_x * other, _y * other, _z * other)
        } else {
            return V3.new(_x * other.x, _y * other.y, _z * other.z)
        }
    }

    norm { (_x*_x + _y*_y + _z*_z).sqrt }

    unit { this / norm }

    string { "V3(%(_x),%(_y),%(_z))" }

    dot(other) { _x * other.x + _y * other.y + _z * other.z }
}

class Ray {
    construct new() { }
    construct new(origin, direction) {
        _origin = origin
        _direction = direction
    }

    origin { _origin }
    direction { _direction }

    origin=(value) { _origin = value }
    direction=(value) { _direction = value }

    point(distance) { _origin + (_direction * distance) }

    string { "Ray(%(_origin.string), %(_direction.string)" }
}

class Sphere {
    construct new(center, radius, color, isLight) {
        _center = center
        _radius = radius
        _color = color
        _isLight = isLight
    }

    center { _center }
    radius { _radius }
    color { _color }
    isLight { _isLight }

    intersect(ray) {
        var oc = ray.origin - _center
        var a = ray.direction.dot(ray.direction)
        var b = oc.dot(ray.direction)
        var c = oc.dot(oc) - (_radius * _radius)
        var dis = b*b - a*c
        
        if(dis <= 0) {
            return null
        }

        var hit = Hit.new()
        var e = dis.sqrt
        var t = (-b - e) / a

        if (t > 0.007) {
            hit.distance = t
            hit.point = ray.point(t)
            hit.normal = (hit.point - _center).unit
            return hit
        }

        t = (-b + e) / a
        if(t > 0.007) {
            hit.distance = t
            hit.point = ray.point(t)
            hit.normal = (hit.point - _center).unit
            return hit
        }

        return null
    }
}

class Camera {
    construct new(eye, lt, rt, lb) {
        _eye = eye
        _lt = lt
        _rt = rt
        _lb = lb
    }

    eye { _eye }
    lt { _lt }
    rt { _rt }
    lb { _lb }
}

class World {
    construct new() {
        _spheres = [
            Sphere.new(V3.new(0.0, -10002.0, 0.0), 9999.0, V3.new(1.0, 1.0, 1.0), false),
            Sphere.new(V3.new(-10012.0, 0.0, 0.0), 9999.0, V3.new(1.0, 0.0, 0.0), false),
            Sphere.new(V3.new(10012.0, 0.0, 0.0), 9999.0, V3.new(0.0, 1.0, 0.0), false),
            Sphere.new(V3.new(0.0, 0.0, -10012.0), 9999.0, V3.new(1.0, 1.0, 1.0), false),
            Sphere.new(V3.new(0.0, 10012.0, 0.0), 9999.0, V3.new(1.0, 1.0, 1.0), true),

            Sphere.new(V3.new(-5.0, 0.0, 2.0), 2.0, V3.new(1.0, 1.0, 0.0), false),
            Sphere.new(V3.new(0.0, 5.0, -1.0), 4.0, V3.new(1.0, 0.0, 0.0), false),
            Sphere.new(V3.new(8.0, 5.0, -1.0), 2.0, V3.new(0.0, 0.0, 1.0), false),
        ]

        _camera = Camera.new(V3.new(0.0, 4.5, 75.0), V3.new(-8.0, 9.0, 50.0), V3.new(8.0, 9.0, 50.0), V3.new(-8.0, 0.0, 50.0))
    }

    spheres { _spheres }
    camera { _camera }
}

class Hit {
    construct new() {
        _distance = Num.largest
    }

    distance { _distance }
    point { _point }
    normal { _normal }

    distance=(value) { _distance = value }
    point=(value) { _point = value }
    normal=(value) { _normal = value }
}

class RayBench {
    construct new() {
        _world = World.new()
        _width = 1280
        _height = 720
        _samples = 50
        _max_depth = 5
    }

    randomDome(normal) {
        while(true) {
            var p = V3.new(2.0 * Random.next() - 1.0, 2.0 * Random.next() - 1.0, 2.0 * Random.next() - 1.0)
            p = p.unit
            var d = p.dot(normal)
            if (d > 0) {
                return p
            }
        }
    }

    trace(ray, depth) {
        var color = V3.new(0,0,0)

        if (depth >= _max_depth) {
            return color
        }

        var sphere = null
        var hit = Hit.new()

        for (sp in _world.spheres) {
            var res = sp.intersect(ray)
            if (res && res.distance > 0.0001 && res.distance < hit.distance) {
                sphere = sp
                hit = res
            }
        }

        if (!sphere) {
            return color
        }

        if (sphere.isLight) {
            return sphere.color
        }

        var nray = Ray.new(hit.point, randomDome(hit.normal))
        var ncolor = trace(nray, depth + 1)
        var at = nray.direction.dot(hit.normal)
        ncolor = ncolor * at
        
        return sphere.color * ncolor
    }

    ppm() {
        var lines = ["P3\n%(_width) %(_height)\n255"]
        for(y in 0..._height) {
            var line = []
            for(x in 0..._width) {
                var pixel = _pixels[y * _width + x]
                line.add("%((pixel.x * 255.99).floor) %((pixel.y * 255.99).floor) %((pixel.z * 255.99).floor)")
            }
            lines.add(line.join(" "))
        }
        return lines.join("\n")
    }

    render() {
        var vdu = (_world.camera.rt - _world.camera.lt) / _width
        var vdv = (_world.camera.lb - _world.camera.lt) / _height
        _pixels = []

        Random.init()

        for(y in 0..._height) {
            for(x in 0..._width) {
                var ray = Ray.new()
                ray.origin = _world.camera.eye
                var color = V3.new(0, 0, 0)

                for(s in 1.._samples) {
                    var u = vdu * (x + Random.next())
                    var v = vdv * (y + Random.next())
                    ray.direction =_world.camera.lt + u + v
                    ray.direction = ray.direction - ray.origin
                    ray.direction = ray.direction.unit
                    var res = trace(ray, 0)
                    color = color + res
                }
                color = color / _samples
                _pixels.add(color)
            }
        }
    }
}

var bench = RayBench.new()
bench.render()
System.print(bench.ppm())