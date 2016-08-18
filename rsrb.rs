extern crate rand;

use std::ops::{Add, Div, Mul, Sub};
use std::fs::File;
use std::io::{self, BufWriter, Write};
use rand::Rng;

const WIDTH: usize = 1280;
const HEIGHT: usize = 720;
const SAMPLES: u32 = 50;
const MAXDEPTH: u32 = 5;

#[derive(Copy, Clone, Default, PartialEq, Debug)]
struct V3<T>(T, T, T);

macro_rules! naive_op {
    ($tr: path, $bound: tt, $method: ident, $_self: ident, $rhs: ident: $rhs_ty: ty => $([$op1: expr, $op2: expr]),+) => {
        impl<T: Copy + $bound<Output = T>> $tr for V3<T> {
            type Output = V3<T>;
            fn $method($_self, $rhs: $rhs_ty) -> V3<T> {
                V3($($bound::$method($op1, $op2)),+)
            }
        }
    };
    (elementwise $tr: path, $bound: tt, $method: ident) => {
        naive_op!($tr, $bound, $method, self, rhs: V3<T> => [self.0, rhs.0], [self.1, rhs.1], [self.2, rhs.2]); };
    (scalar $tr: path, $bound: tt, $method: ident) => {
        naive_op!($tr, $bound, $method, self, rhs: T => [self.0, rhs], [self.1, rhs], [self.2, rhs]); };
}

naive_op!(elementwise Add<V3<T>>, Add, add);
naive_op!(elementwise Sub<V3<T>>, Sub, sub);
naive_op!(elementwise Mul<V3<T>>, Mul, mul);
naive_op!(elementwise Div<V3<T>>, Div, div);
naive_op!(scalar Mul<T>, Mul, mul);
naive_op!(scalar Div<T>, Div, div);

impl<T: Copy + Add<Output = T> + Mul<Output = T>> V3<T> {
    fn dot(&self, rhs: &V3<T>) -> T { self.0 * rhs.0 + self.1 * rhs.1 + self.2 * rhs.2 }
}

impl V3<f32> {
    fn random_dome<R: Rng>(rng: &mut R, normal: V3<f32>) -> V3<f32> {
        rng.gen_iter::<(f32, f32, f32)>()
            .map(|(x, y, z)| V3(x * 2. - 1., y * 2. - 1., z * 2. - 1.).unit())
            .filter(|v| v.dot(&normal) >= 0.)
            .next()
            .unwrap()
    }

    fn norm(&self) -> f32 { self.dot(self).sqrt() }

    fn unit(self) -> V3<f32> {
        let n = self.norm();
        self / n
    }
}

type V3F = V3<f32>;

struct Ray {
    origin: V3F,
    direction: V3F,
}

impl Ray {
    fn new(origin: V3F, direction: V3F) -> Ray {
        Ray {
            origin: origin,
            direction: direction,
        }
    }

    fn point(&self, dist: f32) -> V3F { self.origin + (self.direction * dist) }
}

struct Camera {
    eye: V3F,
    lt: V3F,
    rt: V3F,
    lb: V3F,
}

struct Sphere {
    center: V3F,
    radius: f32,
    color: V3F,
    is_light: bool,
}

struct Hit {
    dist: f32,
    point: V3F,
    normal: V3F,
}

impl Sphere {
    fn new(center: V3F, radius: f32, color: V3F, is_light: bool) -> Sphere {
        Sphere {
            center: center,
            radius: radius,
            color: color,
            is_light: is_light,
        }
    }

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

struct World {
    camera: Camera,
    spheres: Vec<Sphere>,
}

impl World {
    fn new() -> World {
        World {
            camera: Camera {
                eye: V3(0., 4.5, 75.),
                lt: V3(-8., 9., 50.),
                rt: V3(8., 9., 50.),
                lb: V3(-8., 0., 50.),
            },
            spheres: vec![Sphere::new(V3(0., -10002., 0.), 9999., V3(1., 1., 1.), false),
                          Sphere::new(V3(-10012., 0., 0.), 9999., V3(1., 0., 0.), false),
                          Sphere::new(V3(10012., 0., 0.), 9999., V3(0., 1., 0.), false),
                          Sphere::new(V3(0., 0., -10012.), 9999., V3(1., 1., 1.), false),
                          Sphere::new(V3(0., 10012., 0.), 9999., V3(1., 1., 1.), true),
                          Sphere::new(V3(-5., 0., 2.), 2., V3(1., 1., 0.), false),
                          Sphere::new(V3(0., 5., -1.), 4., V3(1., 0., 0.), false),
                          Sphere::new(V3(8., 5., -1.), 2., V3(0., 0., 1.), false)],
        }
    }

    fn trace<R: Rng>(&self, rng: &mut R, ray: &Ray, depth: u32) -> V3F {
        if depth >= MAXDEPTH {
            return V3F::default();
        }
        let closest_hit = self.spheres
            .iter()
            .filter_map(|sphere| sphere.hit(ray).map(|hit| (sphere, hit)))
            .fold(None, |old, (sphere, hit)| match old {
                None => Some((sphere, hit)),
                Some((_, ref old_hit)) if hit.dist < old_hit.dist => Some((sphere, hit)),
                _ => old,
            });

        match closest_hit {
            Some((ref sphere, ref hit)) if !sphere.is_light => {
                let nray = Ray::new(hit.point, V3F::random_dome(rng, hit.normal));
                let ncolor = self.trace(rng, &nray, depth + 1);
                let at = nray.direction.dot(&hit.normal);
                sphere.color * (ncolor * at)
            }
            Some((ref sphere, _)) => sphere.color,
            _ => V3F::default(),
        }
    }
}

struct PPM {
    w: usize,
    h: usize,
    pixels: Vec<Vec<V3F>>,
}

impl PPM {
    fn new(w: usize, h: usize) -> PPM {
        PPM {
            w: w,
            h: h,
            pixels: vec![vec![V3F::default(); w]; h],
        }
    }

    fn pixel(&mut self, x: usize, y: usize, p: V3F) { self.pixels[y][x] = p; }

    fn write(&self, out: &mut io::Write) -> io::Result<()> {
        fn format(f: f32) -> u8 { (f * 255.99) as u8 }
        try!(write!(out, "P3\n{} {}\n255\n", self.w, self.h));
        for line in &self.pixels {
            for pixel in line {
                try!(write!(out, "{} {} {} ", format(pixel.0), format(pixel.1), format(pixel.2)));
            }
            try!(write!(out, "\n"));
        }
        Ok(())
    }
}

fn render() -> PPM {
    let world = World::new();
    let vdu = (world.camera.rt - world.camera.lt) / WIDTH as f32;
    let vdv = (world.camera.lb - world.camera.lt) / HEIGHT as f32;
    let mut rng = rand::XorShiftRng::new_unseeded();

    let mut out = PPM::new(WIDTH, HEIGHT);
    let total = WIDTH * HEIGHT;
    for (x, y) in (0..HEIGHT).flat_map(|y| (0..WIDTH).map(move |x| (x, y))) {
        let mut ray = Ray::new(world.camera.eye, V3F::default());
        let mut color = V3F::default();

        for _ in 0..SAMPLES {
            ray.direction = ((world.camera.lt +
                              (vdu * (x as f32 + rng.gen::<f32>()) +
                               vdv * (y as f32 + rng.gen::<f32>()))) -
                             world.camera.eye)
                .unit();
            color = color + world.trace(&mut rng, &ray, 0);
        }

        out.pixel(x, y, color / SAMPLES as f32);

        let i = y * WIDTH + x;
        if i % 10000 == 0 {
            let progress = i * 100 / total;
            print!("Progress: {}%\r", progress);
            let _ = io::stdout().flush();
        }
    }
    out
}

fn main() {
    let image = render();
    let mut out = BufWriter::new(File::create("rsrb.ppm").expect("Couldn't create output file"));
    image.write(&mut out).expect("Couldn't write to output file");
}
