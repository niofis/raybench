//cargo script rsrb_dod.rs
use std::ops;

const WIDTH: usize = 1280;
const HEIGHT: usize = 720;
const SAMPLES: u32 = 50;
const MAXDEPTH: u32 = 5;

#[derive(Copy, Clone, Debug)]
struct V3 {
    x: f32,
    y: f32,
    z: f32,
}

impl ops::Add<V3> for V3 {
    type Output = V3;

    fn add(self, rhs: V3) -> V3 {
        V3 {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
            z: self.z + rhs.z,
        }
    }
}

impl ops::Sub<V3> for V3 {
    type Output = V3;

    fn sub(self, rhs: V3) -> V3 {
        V3 {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
            z: self.z - rhs.z,
        }
    }
}

impl ops::Mul<V3> for V3 {
    type Output = V3;

    fn mul(self, rhs: V3) -> V3 {
        V3 {
            x: self.x * rhs.x,
            y: self.y * rhs.y,
            z: self.z * rhs.z,
        }
    }
}

impl ops::Mul<f32> for V3 {
    type Output = V3;

    fn mul(self, rhs: f32) -> V3 {
        V3 {
            x: self.x * rhs,
            y: self.y * rhs,
            z: self.z * rhs,
        }
    }
}

impl ops::Div<f32> for V3 {
    type Output = V3;

    fn div(self, rhs: f32) -> V3 {
        V3 {
            x: self.x / rhs,
            y: self.y / rhs,
            z: self.z / rhs,
        }
    }
}

impl V3 {
    fn dot(&self, rhs: &V3) -> f32 {
        self.x * rhs.x + self.y * rhs.y + self.z * rhs.z
    }
    fn norm(&self) -> f32 {
        self.dot(self).sqrt()
    }
    fn unit(self) -> V3 {
        self / self.norm()
    }
}

struct Rng {
    x: u32,
    y: u32,
    z: u32,
    w: u32,
}

impl Iterator for Rng {
    type Item = f32;

    fn next(&mut self) -> Option<f32> {
        let max: f32 = 4294967295.0;
        let t = self.x ^ (self.x << 11);
        self.x = self.y;
        self.y = self.z;
        self.z = self.w;
        self.w = self.w ^ (self.w >> 19) ^ (t ^ (t >> 8));
        Some((self.w as f32) / max)
    }
}

// Returns a Fibonacci sequence generator
fn rand_new() -> Rng {
    Rng {
        x: 123456789,
        y: 362436069,
        z: 521288629,
        w: 88675123,
    }
}

fn random_dome(rng: &mut Rng, normal: V3) -> V3 {
    loop {
        let v = (V3 {
            x: rng.next().unwrap() * 2. - 1.,
            y: rng.next().unwrap() * 2. - 1.,
            z: rng.next().unwrap() * 2. - 1.,
        })
        .unit();
        if v.dot(&normal) >= 0. {
            return v;
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct Ray {
    origin: V3,
    direction: V3,
}

impl Ray {
    fn point(self, t: f32) -> V3 {
        V3 {
            x: self.origin.x + self.direction.x * t,
            y: self.origin.y + self.direction.y * t,
            z: self.origin.z + self.direction.z * t,
        }
    }
}

#[derive(Debug)]
struct Camera {
    eye: V3,
    lt: V3,
    rt: V3,
    lb: V3,
}

#[derive(Debug)]
struct Sphere {
    center: V3,
    radius: f32,
    color: V3,
    is_light: bool,
}

struct Hit {
    dist: f32,
    point: V3,
    normal: V3,
}

impl Sphere {
    fn hit(&self, ray: &Ray) -> Option<Hit> {
        let oc = ray.origin - self.center;
        let a = ray.direction.dot(&ray.direction);
        let b = oc.dot(&ray.direction);
        let c = oc.dot(&oc) - self.radius * self.radius;
        let dis = b * b - a * c;

        if dis > 0. {
            let e = dis.sqrt();

            let t = (-b - e) / a;
            if t > 0.007 {
                let pt = ray.point(t);
                let n = (pt - self.center).unit();
                return Some(Hit {
                    dist: t,
                    point: pt,
                    normal: n,
                });
            }

            let t = (-b + e) / a;
            if t > 0.007 {
                let pt = ray.point(t);
                let n = (pt - self.center).unit();
                return Some(Hit {
                    dist: t,
                    point: pt,
                    normal: n,
                });
            }
        }
        None
    }
}
#[derive(Debug)]
struct Spheres {
    x: Vec<f32>,
    y: Vec<f32>,
    z: Vec<f32>,
    radius: Vec<f32>,
    color: Vec<V3>,
    is_light: Vec<bool>,
}

impl Spheres {
    fn hits(&self, ray: &Ray) -> Vec<f32> {
        let size = self.x.len();
        let mut distances: Vec<f32> = vec![0.; size];

        let a = ray.direction.dot(&ray.direction);

        for i in [0..size] {
            let oc = [
                ray.origin.x - self.x[i],
                ray.origin.y - self.y[i],
                ray.origin.z - self.z[i],
            ];
            let b = oc[0] * ray.direction.x + oc[1] * ray.direction.y + oc[2] * ray.direction.z;
            let cdot = oc[0] * oc[0] + oc[1] * oc[1] + oc[2] * oc[2];
            let c = cdot - self.radius[i] * self.radius[i];
            let dis = b * b - a * c;

            if dis > 0. {
                let e = dis.sqrt();

                let t = (-b - e) / a;
                if t > 0.007 {
                    distances[i] = t;
                    break;
                }

                let t = (-b + e) / a;
                if t > 0.007 {
                    distances[i] = t;
                }
            }
        }

        return distances;
    }
}

#[derive(Debug)]
struct World {
    spheres: Spheres,
    camera: Camera,
}

impl World {
    fn new() -> World {
        let spheresObjs = vec![
            Sphere {
                center: V3 {
                    x: 0.,
                    y: -10002.,
                    z: 0.,
                },
                radius: 9999.,
                color: V3 {
                    x: 1.,
                    y: 1.,
                    z: 1.,
                },
                is_light: false,
            },
            Sphere {
                center: V3 {
                    x: -10012.,
                    y: 0.,
                    z: 0.,
                },
                radius: 9999.,
                color: V3 {
                    x: 1.,
                    y: 0.,
                    z: 0.,
                },
                is_light: false,
            },
            Sphere {
                center: V3 {
                    x: 10012.,
                    y: 0.,
                    z: 0.,
                },
                radius: 9999.,
                color: V3 {
                    x: 0.,
                    y: 1.,
                    z: 0.,
                },
                is_light: false,
            },
            Sphere {
                center: V3 {
                    x: 0.,
                    y: 0.,
                    z: -10012.,
                },
                radius: 9999.,
                color: V3 {
                    x: 1.,
                    y: 1.,
                    z: 1.,
                },
                is_light: false,
            },
            Sphere {
                center: V3 {
                    x: 0.,
                    y: 10012.,
                    z: 0.,
                },
                radius: 9999.,
                color: V3 {
                    x: 1.,
                    y: 1.,
                    z: 1.,
                },
                is_light: true,
            },
            Sphere {
                center: V3 {
                    x: -5.,
                    y: 0.,
                    z: 2.,
                },
                radius: 2.,
                color: V3 {
                    x: 1.,
                    y: 1.,
                    z: 0.,
                },
                is_light: false,
            },
            Sphere {
                center: V3 {
                    x: 0.,
                    y: 5.,
                    z: -1.,
                },
                radius: 4.,
                color: V3 {
                    x: 1.,
                    y: 0.,
                    z: 0.,
                },
                is_light: false,
            },
            Sphere {
                center: V3 {
                    x: 8.,
                    y: 5.,
                    z: -1.,
                },
                radius: 2.,
                color: V3 {
                    x: 0.,
                    y: 0.,
                    z: 1.,
                },
                is_light: false,
            },
        ];
        let mut spheres = Spheres {
            x: Vec::new(),
            y: Vec::new(),
            z: Vec::new(),
            radius: Vec::new(),
            color: Vec::new(),
            is_light: Vec::new(),
        };

        for sp in spheresObjs {
            spheres.x.push(sp.center.x);
            spheres.y.push(sp.center.y);
            spheres.z.push(sp.center.z);
            spheres.radius.push(sp.radius);
            spheres.color.push(sp.color);
            spheres.is_light.push(sp.is_light);
        }

        World {
            camera: Camera {
                eye: V3 {
                    x: 0.,
                    y: 4.5,
                    z: 75.,
                },
                lt: V3 {
                    x: -8.,
                    y: 9.,
                    z: 50.,
                },
                rt: V3 {
                    x: 8.,
                    y: 9.,
                    z: 50.,
                },
                lb: V3 {
                    x: -8.,
                    y: 0.,
                    z: 50.,
                },
            },
            spheres,
        }
    }

    fn trace(&self, rng: &mut Rng, ray: &Ray, depth: u32) -> V3 {
        if depth >= MAXDEPTH {
            return V3 {
                x: 0.,
                y: 0.,
                z: 0.,
            };
        }
        let closest_hit = self
            .spheres
            .iter()
            .filter_map(|sphere| sphere.hit(ray).map(|hit| (sphere, hit)))
            .fold(None, |old, (sphere, hit)| match old {
                None => Some((sphere, hit)),
                Some((_, ref old_hit)) if hit.dist < old_hit.dist => Some((sphere, hit)),
                _ => old,
            });
        match closest_hit {
            Some((ref sphere, ref hit)) if !sphere.is_light => {
                let nray = Ray {
                    origin: hit.point,
                    direction: random_dome(rng, hit.normal),
                };
                let ncolor = self.trace(rng, &nray, depth + 1);
                let at = nray.direction.dot(&hit.normal);
                sphere.color * (ncolor * at)
            }
            Some((ref sphere, _)) => sphere.color,
            _ => V3 {
                x: 0.,
                y: 0.,
                z: 0.,
            },
        }
    }
}

fn write_ppm(data: &Vec<V3>) {
    println!("P3\n{} {}\n255", WIDTH, HEIGHT);

    for y in 0..HEIGHT {
        for x in 0..WIDTH {
            let pixel = data[y * WIDTH + x];
            print!(
                "{} {} {} ",
                (pixel.x * 255.99) as u8,
                (pixel.y * 255.99) as u8,
                (pixel.z * 255.99) as u8
            );
        }
        print!("\n");
    }
}

fn main() {
    let world = World::new();
    let vdu = (world.camera.rt - world.camera.lt) / WIDTH as f32;
    let vdv = (world.camera.lb - world.camera.lt) / HEIGHT as f32;

    let mut rng = rand_new();
    let data: Vec<V3> = (0..HEIGHT * WIDTH)
        .map(|pixel| {
            let x = pixel % WIDTH;
            let y = pixel / WIDTH;

            let color: V3 = (0..SAMPLES)
                .map(|_| {
                    let ray = Ray {
                        origin: world.camera.eye,
                        direction: ((world.camera.lt
                            + (vdu * (x as f32 + rng.next().unwrap())
                                + vdv * (y as f32 + rng.next().unwrap())))
                            - world.camera.eye)
                            .unit(),
                    };

                    world.trace(&mut rng, &ray, 0)
                })
                .fold(
                    V3 {
                        x: 0.,
                        y: 0.,
                        z: 0.,
                    },
                    |a, b| a + b,
                );

            color / SAMPLES as f32
        })
        .collect();
    write_ppm(&data);
}
