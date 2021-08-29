using Lambda;

final WIDTH = 1280;
final HEIGHT = 720;
final SAMPLES = 50;
final MAX_DEPTH = 5;

class Vector3 {
	public var x:Float;
	public var y:Float;
	public var z:Float;

	public function new(?x:Float = 0, ?y:Float = 0, ?z:Float = 0) {
		this.x = x;
		this.y = y;
		this.z = z;
	}

	public function add(v:Vector3):Vector3 {
		return new Vector3(this.x + v.x, this.y + v.y, this.z + v.z);
	}

	public function sub(v:Vector3):Vector3 {
		return new Vector3(this.x - v.x, this.y - v.y, this.z - v.z);
	}

	public function mul(s:Float):Vector3 {
		return new Vector3(this.x * s, this.y * s, this.z * s);
	}

	public function mulv(v:Vector3):Vector3 {
		return new Vector3(this.x * v.x, this.y * v.y, this.z * v.z);
	}

	public function div(s:Float):Vector3 {
		return new Vector3(this.x / s, this.y / s, this.z / s);
	}

	public function dot(v:Vector3):Float {
		return this.x * v.x + this.y * v.y + this.z * v.z;
	}

	public function norm():Float {
		return Math.sqrt(this.dot(this));
	}

	public function unit():Vector3 {
		return this.div(this.norm());
	}
}

class Ray {
	public var origin:Vector3;
	public var direction:Vector3;

	public function new(origin:Vector3, direction:Vector3) {
		this.origin = origin;
		this.direction = direction;
	}

	public function point(dist:Float):Vector3 {
		return this.origin.add(this.direction.mul(dist));
	}
}

class Hit {
	public var dist:Float;
	public var point:Vector3;
	public var normal:Vector3;

	public function new(?dist:Float = 1e15, ?point:Vector3 = null, ?normal:Vector3 = null) {
		this.dist = dist;
		this.point = if (point != null) point else new Vector3();
		this.normal = if (normal != null) normal else new Vector3();
	}
}

class Camera {
	public var eye:Vector3;
	public var lt:Vector3;
	public var rt:Vector3;
	public var lb:Vector3;

	public function new(eye:Vector3, lt:Vector3, rt:Vector3, lb:Vector3) {
		this.eye = eye;
		this.lt = lt;
		this.rt = rt;
		this.lb = lb;
	}
}

class Sphere {
	public var center:Vector3;
	public var radius:Float;
	public var color:Vector3;
	public var isLight:Bool;

	public function new(center:Vector3, radius:Float, color:Vector3, isLight:Bool) {
		this.center = center;
		this.radius = radius;
		this.color = color;
		this.isLight = isLight;
	}

	public function hit(ray:Ray):Hit {
		var oc = ray.origin.sub(this.center);
		var a = ray.direction.dot(ray.direction);
		var b = oc.dot(ray.direction);
		var c = oc.dot(oc) - this.radius * this.radius;
		var dis = b * b - a * c;

		if (dis > 0) {
			var e = Math.sqrt(dis);
			var t = (-b - e) / a;
			if (t > 0.007) {
				var point = ray.point(t);
				var normal = point.sub(this.center).unit();
				return new Hit(t, point, normal);
			}

			t = (-b + e) / a;
			if (t > 0.007) {
				var point = ray.point(t);
				var normal = point.sub(this.center).unit();
				return new Hit(t, point, normal);
			}
		}

		return null;
	}
}

class World {
	public var camera:Camera;
	public var spheres:Array<Sphere>;

	public function new() {
		this.camera = new Camera(new Vector3(0, 4.5, 75), new Vector3(-8, 9, 50), new Vector3(8, 9, 50), new Vector3(-8, 0, 50));
		this.spheres = [
			new Sphere(new Vector3(0, -10002, 0), 9999, new Vector3(1, 1, 1), false),
			new Sphere(new Vector3(-10012, 0, 0), 9999, new Vector3(1, 0, 0), false),
			new Sphere(new Vector3(10012, 0, 0), 9999, new Vector3(0, 1, 0), false),
			new Sphere(new Vector3(0, 0, -10012), 9999, new Vector3(1, 1, 1), false),
			new Sphere(new Vector3(0, 10012, 0), 9999, new Vector3(1, 1, 1), true),
			new Sphere(new Vector3(0, 10012, 0), 9999, new Vector3(1, 1, 1), true),
			new Sphere(new Vector3(-5, 0, 2), 2, new Vector3(1, 1, 0), false),
			new Sphere(new Vector3(0, 5, -1), 4, new Vector3(1, 0, 0), false),
			new Sphere(new Vector3(8, 5, -1), 2, new Vector3(0, 0, 1), false)
		];
	}

	public function trace(ray, depth):Vector3 {
		var noHit = new Hit();
		var closestHit = this.spheres.map(sphere -> ({
			sphere: sphere,
			hit: sphere.hit(ray)
		})).filter(res -> res.hit != null).fold((res, bestRes) -> if (res.hit.dist < bestRes.hit.dist) res else bestRes, {sphere: null, hit: noHit});
		var hit = closestHit.hit;
		var sp = closestHit.sphere;
		if (sp == null || depth >= MAX_DEPTH) {
			return new Vector3();
		} else if (sp.isLight) {
			return sp.color;
		} else {
			var nray = new Ray(hit.point, rndDome(hit.normal));
			var ncolor = this.trace(nray, depth + 1);
			var at = nray.direction.dot(hit.normal);
			return sp.color.mulv(ncolor.mul(at));
		}
	}
}

class Rand {
	static var x:UInt = 123456789;
	static var y:UInt = 362436069;
	static var z:UInt = 521288629;
	static var w:UInt = 88675123;
	static var t:UInt = 0;
	static var max:Float = 4294967295;

	public static function next():Float {
		Rand.t = (Rand.x ^ (Rand.x << 11));
		Rand.x = Rand.y;
		Rand.y = Rand.z;
		Rand.z = Rand.w;
		Rand.w = (Rand.w ^ (Rand.w >> 19) ^ (Rand.t ^ (Rand.t >> 8)));
		return Rand.w / Rand.max;
	}
}

function rndDome(normal:Vector3):Vector3 {
	var p = new Vector3();
	var d = -1.0;
	do {
		p.x = 2 * Rand.next() - 1;
		p.y = 2 * Rand.next() - 1;
		p.z = 2 * Rand.next() - 1;

		p = p.unit();
		d = p.dot(normal);
	} while (d < 0);

	return p;
}

function writePpm(data:Array<Vector3>) {
	Sys.println('P3\n$WIDTH $HEIGHT\n255');
	for (y in 0...HEIGHT) {
		for (x in 0...WIDTH) {
			var pixel = data[x + y * WIDTH];
			var r = Math.floor(pixel.x * 255.99);
			var g = Math.floor(pixel.y * 255.99);
			var b = Math.floor(pixel.z * 255.99);
			Sys.print('$r $g $b ');
		}
		Sys.print('\n');
	}
}

class Haxerb {
	static public function main():Void {
		var world = new World();
		var vdu = world.camera.rt.sub(world.camera.lt).div(WIDTH);
		var vdv = world.camera.lb.sub(world.camera.lt).div(HEIGHT);
		var data = [for (x in 0...(WIDTH * HEIGHT)) x].map(pixel -> {
			var x = pixel % WIDTH;
			var y = Math.floor(pixel / WIDTH);
			var color = [for (c in 0...SAMPLES) c].map(_ -> {
				var direction = world.camera.lt.add(vdu.mul(x + Rand.next()).add(vdv.mul(y + Rand.next())));
				var origin = world.camera.eye;
				direction = direction.sub(origin).unit();
				var ray = new Ray(origin, direction);
				var color = world.trace(ray, 0);

				return color;
			}).fold((c, acc) -> acc.add(c), new Vector3());
			return color.div(SAMPLES);
		});
		writePpm(data);
	}
}
