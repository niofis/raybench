const std = @import("std");

const WIDTH: usize = 1280;
const HEIGHT: usize = 720;
const SAMPLES: i32 = 50;
const MAXDEPTH: i32 = 5;

const V3 = struct {
    x: f32,
    y: f32,
    z: f32,

    pub fn new() V3 {
        return V3{
            .x = 0.0,
            .y = 0.0,
            .z = 0.0,
        };
    }

    pub fn add(a: V3, b: V3) V3 {
        return V3{
            .x = a.x + b.x,
            .y = a.y + b.y,
            .z = a.z + b.z,
        };
    }

    pub fn sub(a: V3, b: V3) V3 {
        return V3{
            .x = a.x - b.x,
            .y = a.y - b.y,
            .z = a.z - b.z,
        };
    }

    pub fn mul(a: V3, b: V3) V3 {
        return V3{
            .x = a.x * b.x,
            .y = a.y * b.y,
            .z = a.z * b.z,
        };
    }

    pub fn mulS(a: V3, b: f32) V3 {
        return V3{
            .x = a.x * b,
            .y = a.y * b,
            .z = a.z * b,
        };
    }

    pub fn divS(a: V3, b: f32) V3 {
        return V3{
            .x = a.x / b,
            .y = a.y / b,
            .z = a.z / b,
        };
    }

    pub fn dot(a: V3, b: *const V3) f32 {
        return a.x * b.x + a.y * b.y + a.z * b.z;
    }

    pub fn norm(a: *const V3) f32 {
        return @sqrt(a.dot(a));
    }

    pub fn unit(a: V3) V3 {
        return a.divS(a.norm());
    }
};

const RandState = struct {
    x: u32,
    y: u32,
    z: u32,
    w: u32,
};

var randState = RandState{
    .x = 123456789,
    .y = 362436069,
    .z = 521288629,
    .w = 88675123,
};

const U32_MAX: f32 = 4294967295.0;

pub fn rand() f32 {
    var t: u32 = randState.x ^ (randState.x << 11);
    randState.x = randState.y;
    randState.y = randState.z;
    randState.z = randState.w;
    randState.w = randState.w ^ (randState.w >> 19) ^ (t ^ (t >> 8));
    return @intToFloat(f32, randState.w) / U32_MAX;
}

pub fn rndDome(normal: *const V3) V3 {
    while (true) {
        const v = (V3{
            .x = rand() * 2.0 - 1.0,
            .y = rand() * 2.0 - 1.0,
            .z = rand() * 2.0 - 1.0,
        }).unit();
        if (v.dot(normal) >= 0.0) {
            return v;
        }
    }
}

const Ray = struct {
    origin: V3,
    direction: V3,

    pub fn point(self: Ray, t: f32) V3 {
        return V3{
            .x = self.origin.x + self.direction.x * t,
            .y = self.origin.y + self.direction.y * t,
            .z = self.origin.z + self.direction.z * t,
        };
    }
};

const Camera = struct {
    eye: V3,
    lt: V3,
    rt: V3,
    lb: V3,
};

const Hit = struct {
    dist: f32,
    point: V3,
    normal: V3,
};

const Sphere = struct {
    center: V3,
    radius: f32,
    color: V3,
    isLight: bool,

    pub fn hit(self: *const Sphere, ray: *const Ray) ?Hit {
        const oc = ray.origin.sub(self.center);
        const a = ray.direction.dot(&ray.direction);
        const b = oc.dot(&ray.direction);
        const c = oc.dot(&oc) - self.radius * self.radius;
        const dis = b * b - a * c;

        if (dis > 0.0) {
            const e = @sqrt(dis);

            var t = (-b - e) / a;
            if (t > 0.007) {
                const pt = ray.point(t);
                const n = pt.sub(self.center).unit();
                return Hit{
                    .dist = t,
                    .point = pt,
                    .normal = n,
                };
            }

            t = (-b + e) / a;
            if (t > 0.007) {
                const pt = ray.point(t);
                const n = pt.sub(self.center).unit();
                return Hit{
                    .dist = t,
                    .point = pt,
                    .normal = n,
                };
            }
        }

        return null;
    }
};

const World = struct {
    spheres: [8]Sphere,
    camera: Camera,

    pub fn new() World {
        return World{
            .camera = Camera{
                .eye = V3{
                    .x = 0.0,
                    .y = 4.5,
                    .z = 75.0,
                },
                .lt = V3{
                    .x = -8.0,
                    .y = 9.0,
                    .z = 50.0,
                },
                .rt = V3{
                    .x = 8.0,
                    .y = 9.0,
                    .z = 50.0,
                },
                .lb = V3{
                    .x = -8.0,
                    .y = 0.0,
                    .z = 50.0,
                },
            },
            .spheres = [_]Sphere{
                Sphere{
                    .center = V3{
                        .x = 0.0,
                        .y = -10002.0,
                        .z = 0.0,
                    },
                    .radius = 9999.0,
                    .color = V3{
                        .x = 1.0,
                        .y = 1.0,
                        .z = 1.0,
                    },
                    .isLight = false,
                },
                Sphere{
                    .center = V3{
                        .x = -10012.0,
                        .y = 0.0,
                        .z = 0.0,
                    },
                    .radius = 9999.0,
                    .color = V3{
                        .x = 1.0,
                        .y = 0.0,
                        .z = 0.0,
                    },
                    .isLight = false,
                },
                Sphere{
                    .center = V3{
                        .x = 10012.0,
                        .y = 0.0,
                        .z = 0.0,
                    },
                    .radius = 9999.0,
                    .color = V3{
                        .x = 0.0,
                        .y = 1.0,
                        .z = 0.0,
                    },
                    .isLight = false,
                },
                Sphere{
                    .center = V3{
                        .x = 0.0,
                        .y = 0.0,
                        .z = -10012.0,
                    },
                    .radius = 9999.0,
                    .color = V3{
                        .x = 1.0,
                        .y = 1.0,
                        .z = 1.0,
                    },
                    .isLight = false,
                },
                Sphere{
                    .center = V3{
                        .x = 0.0,
                        .y = 10012.0,
                        .z = 0.0,
                    },
                    .radius = 9999.0,
                    .color = V3{
                        .x = 1.0,
                        .y = 1.0,
                        .z = 1.0,
                    },
                    .isLight = true,
                },
                Sphere{
                    .center = V3{
                        .x = -5.0,
                        .y = 0.0,
                        .z = 2.0,
                    },
                    .radius = 2.0,
                    .color = V3{
                        .x = 1.0,
                        .y = 1.0,
                        .z = 0.0,
                    },
                    .isLight = false,
                },
                Sphere{
                    .center = V3{
                        .x = 0.0,
                        .y = 5.0,
                        .z = -1.0,
                    },
                    .radius = 4.0,
                    .color = V3{
                        .x = 1.0,
                        .y = 0.0,
                        .z = 0.0,
                    },
                    .isLight = false,
                },
                Sphere{
                    .center = V3{
                        .x = 8.0,
                        .y = 5.0,
                        .z = -1.0,
                    },
                    .radius = 2.0,
                    .color = V3{
                        .x = 0.0,
                        .y = 0.0,
                        .z = 1.0,
                    },
                    .isLight = false,
                },
            },
        };
    }

    pub fn trace(self: *World, ray: *const Ray, depth: u32) V3 {
        if (depth >= MAXDEPTH) {
            return V3{
                .x = 0.0,
                .y = 0.0,
                .z = 0.0,
            };
        }

        var closestHit: ?Hit = null;
        var closestSphereIndex: usize = 0;

        for (self.spheres) |sphere, i| {
            if (sphere.hit(ray)) |currentHit| {
                if (closestHit) |previousHit| {
                    if (currentHit.dist > 0.0001 and currentHit.dist < previousHit.dist) {
                        closestHit = currentHit;
                        closestSphereIndex = i;
                    }
                } else {
                    closestHit = currentHit;
                    closestSphereIndex = i;
                }
            }
        }

        if (closestHit) |hit| {
            const sphere = self.spheres[closestSphereIndex];
            if (sphere.isLight) {
                return sphere.color;
            }
            const nray = Ray{
                .origin = hit.point,
                .direction = rndDome(&hit.normal),
            };
            const ncolor = self.trace(&nray, depth + 1);
            const at = nray.direction.dot(&hit.normal);
            return ncolor.mulS(at).mul(sphere.color);
        }

        return V3{
            .x = 0.0,
            .y = 0.0,
            .z = 0.0,
        };
    }
};

pub fn writePPM() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("P3\n{} {}\n255\n", .{ WIDTH, HEIGHT });
    var y: usize = 0;
    while (y < HEIGHT) : (y += 1) {
        var x: usize = 0;
        while (x < WIDTH) : (x += 1) {
            const idx = y * WIDTH + x;
            const pixel = data[idx];
            const r = @floatToInt(u8, pixel.x * 255.99);
            const g = @floatToInt(u8, pixel.y * 255.99);
            const b = @floatToInt(u8, pixel.z * 255.99);
            try stdout.print("{} {} {} ", .{ r, g, b });
        }
        try stdout.print("\n", .{});
    }
}

var data = [_]V3{V3.new()} ** (HEIGHT * WIDTH);

pub fn main() !void {
    var world = World.new();
    const vdu = world.camera.rt.sub(world.camera.lt).divS(@intToFloat(f32, WIDTH));
    const vdv = world.camera.lb.sub(world.camera.lt).divS(@intToFloat(f32, HEIGHT));
    var y: usize = 0;
    while (y < HEIGHT) : (y += 1) {
        var x: usize = 0;
        while (x < WIDTH) : (x += 1) {
            var ray = Ray{
                .origin = world.camera.eye,
                .direction = V3{
                    .x = 0.0,
                    .y = 0.0,
                    .z = 0.0,
                },
            };
            var c = V3{
                .x = 0.0,
                .y = 0.0,
                .z = 0.0,
            };
            var s: i32 = 0;
            while (s < SAMPLES) : (s += 1) {
                const u = vdu.mulS(@intToFloat(f32, x) + rand());
                const v = vdv.mulS(@intToFloat(f32, y) + rand());
                ray.direction = world.camera.lt.add(u).add(v).sub(world.camera.eye).unit();
                c = c.add(world.trace(&ray, 0));
            }
            data[y * WIDTH + x] = c.divS(@intToFloat(f32, SAMPLES));
        }
    }
    try writePPM();
}
