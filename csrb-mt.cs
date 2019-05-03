using System;
using System.Threading;
using System.Threading.Tasks;

//https://codingforspeed.com/using-faster-psudo-random-generator-xorshift/
class Random {
  uint x = 123456789;
  uint y = 362436069;
  uint z = 521288629;
  uint w = 88675123;
  
  public float NextFloat() {
    uint t = x ^ (x << 11);
    x = y; y = z; z = w;
    return (w = w ^ (w >> 19) ^ (t ^ (t >> 8))) / (float)uint.MaxValue;
  }
}

struct Vector3 {

  public float x;
  public float y;
  public float z;

  public Vector3 (float x, float y, float z) {
    this.x = x;
    this.y = y;
    this.z = z;
  }

  public Vector3 Add (Vector3 v) {
    return new Vector3 (
        this.x + v.x,
        this.y + v.y,
        this.z + v.z
        );
  }

  public Vector3 Sub (Vector3 v) {
    return new Vector3 (
        this.x - v.x,
        this.y - v.y,
        this.z - v.z
        );
  }

  public Vector3 Mul (float v) {
    return new Vector3(
        this.x * v,
        this.y * v,
        this.z * v
        );
  }

  public Vector3 Mul (Vector3 v) {
    return new Vector3(
        this.x * v.x,
        this.y * v.y,
        this.z * v.z
        );
  }

  public Vector3 Div (float v) {
    return new Vector3(
        this.x / v,
        this.y / v,
        this.z / v
        );
  }

  public Vector3 Div (Vector3 v) {
    return new Vector3(
        this.x / v.x,
        this.y / v.y,
        this.z / v.z
        );
  }

  public float Dot (Vector3 v) {
    return this.x * v.x + this.y * v.y + this.z * v.z;
  }

  public float Norm () {
    return (float) Math.Sqrt(this.x * this.x + this.y * this.y + this.z * this.z);
  }

  public Vector3 Unit () {
    return this.Div(this.Norm());
  }
}

struct Ray {
  public Vector3 origin;
  public Vector3 direction;

  public Ray (Vector3 origin, Vector3 direction) {
    this.origin = origin;
    this.direction = direction;
  }

  public Vector3 Point (float dist) {
    return this.origin.Add(this.direction.Mul(dist));
  }
}

struct Hit {
  public float dist;
  public Vector3 point;
  public Vector3 normal;

  public Hit (float dist, Vector3 point, Vector3 normal) {
    this.dist = dist;
    this.point = point;
    this.normal = normal;
  }
}

struct Camera {
  public Vector3 eye;
  public Vector3 lt;
  public Vector3 rt;
  public Vector3 lb;
}

struct Sphere {
  public Vector3 center;
  public float radius;
  public Vector3 color;
  public bool is_light;

  public Sphere (Vector3 center, float radius, Vector3 color, bool is_light) {
    this.center = center;
    this.radius = radius;
    this.color = color;
    this.is_light = is_light;
  }

  public Hit? Hit (Ray ray) {
    var oc = ray.origin.Sub(this.center);
    float a = ray.direction.Dot(ray.direction);
    float b = oc.Dot(ray.direction);
    float c = oc.Dot(oc) - this.radius * this.radius;
    float dis = b * b - a * c;

    if (dis > 0) {
      float e = (float) Math.Sqrt(dis);

      float t = (-b - e) / a;
      if (t > 0.007f) {
        var hit = new Hit();

        hit.dist = t;
        hit.point = ray.Point(t);
        hit.normal = hit.point.Sub(this.center).Unit();

        return hit;
      }

      t = (-b + e) / a;
      if (t > 0.007f) {
        var hit = new Hit();

        hit.dist = t;
        hit.point = ray.Point(t);
        hit.normal = hit.point.Sub(this.center).Unit();

        return hit;
      }

      return null;
    }

    return null;
  }
}

class RayBench{

  public const int WIDTH = 1280;
  public const int HEIGHT = 720;
  public const int SAMPLES = 50;
  public const int MAX_DEPTH = 5;
  public static Sphere[] spheres = new Sphere[8];

  public static Vector3 rnd_dome (Vector3 normal, Random rnd) {
    var p = new Vector3();
    float d;

    do {
      p.x = 2.0f * rnd.NextFloat() - 1.0f;
      p.y = 2.0f * rnd.NextFloat() - 1.0f;
      p.z = 2.0f * rnd.NextFloat() - 1.0f;

      p = p.Unit();
      d = p.Dot(normal);
    } while (d < 0);

    return p;
  }

  public static Vector3 trace(Ray ray, int depth, Random rnd) {
    var color = new Vector3();
    bool did_hit = false;
    var hit = new Hit();
    var sp = new Sphere();

    hit.dist = 1e15f;

    foreach(var s in spheres) {
      var lh = s.Hit(ray);

      if (lh !=null && lh.Value.dist > 0.0001f && lh.Value.dist < hit.dist) {
        sp = s;
        did_hit = true;
        color = s.color;
        hit = lh.Value;
      }
    }

    if (did_hit && depth < MAX_DEPTH) {
      if (sp.is_light != true) {
        var nray = new Ray(
            hit.point,
            RayBench.rnd_dome(hit.normal, rnd)
            );
        var ncolor = RayBench.trace(nray, depth + 1, rnd);
        var at = nray.direction.Dot(hit.normal);

        color = color.Mul(ncolor.Mul(at));
      }
    }

    if (did_hit == false || depth >= MAX_DEPTH) {
      return new Vector3();
    }

    return color;
  }

  public static void WritePPM (Vector3[][] data) {
    using (var ppm = new System.IO.StreamWriter("csrb-mt.ppm")) {
      ppm.Write($"P3\n{RayBench.WIDTH} {RayBench.HEIGHT}\n255\n");

      for(int y = 0; y < HEIGHT; ++y) {
        for(int x = 0; x < WIDTH; ++x) {
          int r = (int) Math.Floor(data[y][x].x * 255.99f);
          int g = (int) Math.Floor(data[y][x].y * 255.99f);
          int b = (int) Math.Floor(data[y][x].z * 255.99f);
          ppm.Write($"{r} {g} {b} ");
        }
        ppm.Write("\n");
      }

      ppm.Close();
    }
  }

  static readonly ThreadLocal<Random> rnd = new ThreadLocal<Random>(() => new Random());

  public static void Main (String[] args) {

    spheres[0] = (new Sphere(
          new Vector3(0, -10002, 0),
          9999,
          new Vector3(1, 1, 1),
          false));

    spheres[1] = (new Sphere(
          new Vector3(-10012, 0, 0),
          9999,
          new Vector3(1, 0, 0),
          false));

    spheres[2] = (new Sphere(
          new Vector3(10012, 0, 0),
          9999,
          new Vector3(0, 1, 0),
          false));

    spheres[3] = (new Sphere(
          new Vector3(0, 0, -10012),
          9999,
          new Vector3(1, 1, 1),
          false));

    spheres[4] = (new Sphere(
          new Vector3(0, 10012, 0),
          9999,
          new Vector3(1, 1, 1),
          true));

    spheres[5] = (new Sphere(
          new Vector3(-5, 0, 2),
          2,
          new Vector3(1, 1, 0),
          false));

    spheres[6] = (new Sphere(
          new Vector3(0, 5, -1),
          4,
          new Vector3(1, 0, 0),
          false));

    spheres[7] = (new Sphere(
          new Vector3(8, 5, -1),
          2,
          new Vector3(0, 0, 1),
          false));

    var data = new Vector3[RayBench.HEIGHT][];
    var cam = new Camera{
      eye = new Vector3(0.0f, 4.5f, 75.0f),
      lt = new Vector3(-8, 9, 50),
      rt = new Vector3(8, 9, 50),
      lb = new Vector3(-8, 0, 50),
    };
    var vdu = cam.rt.Sub(cam.lt).Div(RayBench.WIDTH);
    var vdv = cam.lb.Sub(cam.lt).Div(RayBench.HEIGHT);

    var options = new ParallelOptions();
    options.MaxDegreeOfParallelism = Environment.ProcessorCount;

    Parallel.For(0, RayBench.HEIGHT, options, y => {
      var random = rnd.Value;
      data[y] = new Vector3[RayBench.WIDTH];
      for(int x = 0; x < RayBench.WIDTH; ++x) {
        var color = new Vector3();
        var ray = new Ray();

        ray.origin = cam.eye;

        for(int i = 0; i < RayBench.SAMPLES; ++i) {
          ray.direction = cam.lt.Add(
              vdu.Mul(x + random.NextFloat()).Add(
                vdv.Mul(y + random.NextFloat())));

          ray.direction = ray.direction.Sub(ray.origin);
          ray.direction = ray.direction.Unit();
          color = color.Add(RayBench.trace(ray, 0, random));
        }

        color = color.Div(RayBench.SAMPLES);

        data[y][x] = color;
      }
    });

    RayBench.WritePPM(data);
  }
}
