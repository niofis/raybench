using System;
using System.Numerics;

static class Vector3Extensions {
  public static Vector3 Unit(this Vector3 v) {
    return v / v.Length();
  }
}

//https://codingforspeed.com/using-faster-psudo-random-generator-xorshift/
static class Random {
  static uint x = 123456789;
  static uint y = 362436069;
  static uint z = 521288629;
  static uint w = 88675123;
  
  public static float NextFloat() {
    uint t = x ^ (x << 11);
    x = y; y = z; z = w;
    return (w = w ^ (w >> 19) ^ (t ^ (t >> 8))) / (float)uint.MaxValue;
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
    return origin + direction * dist;
  }
}

struct Hit {
  public Vector3 point;
  public Vector3 normal;
  public float dist;

  public Hit (float dist) {
    this.dist = dist;
    point = default;
    normal = default;
  }

  public Hit (float dist, Vector3 point, Vector3 normal) {
    this.dist = dist;
    this.point = point;
    this.normal = normal;
  }
}

class Camera {
  public Vector3 eye;
  public Vector3 lt;
  public Vector3 rt;
  public Vector3 lb;

  public Camera () {
    eye = new Vector3(0.0f, 4.5f, 75.0f);
    lt = new Vector3(-8, 9, 50);
    rt = new Vector3(8, 9, 50);
    lb = new Vector3(-8, 0, 50);
  }
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

  public Hit Hit (Ray ray) {
    var oc = ray.origin - center;
    float a = Vector3.Dot(ray.direction, ray.direction);
    float b = Vector3.Dot(oc, ray.direction);
    float c = Vector3.Dot(oc, oc) - radius * radius;
    float dis = b * b - a * c;

    if (dis > 0) {
      float e = (float) Math.Sqrt(dis);

      float t = (-b - e) / a;
      if (t > 0.007f) {
        var point = ray.Point(t);
        return new Hit(t, point, (point - center).Unit());
      }

      t = (-b + e) / a;
      if (t > 0.007f) {
        var point = ray.Point(t);
        return new Hit(t, point, (point - center).Unit());
      }

      return default;
    }

    return default;
  }
}

class RayBench{

  public const int WIDTH = 1280;
  public const int HEIGHT = 720;
  public const int SAMPLES = 50;
  public const int MAX_DEPTH = 5;
  public static Sphere[] spheres = new Sphere[8];

  public static Vector3 RandomDome(Vector3 normal) {
    Vector3 p;
    float d;

    do {
      p = new Vector3(
        (float)(2.0 * Random.NextFloat() - 1.0),
        (float)(2.0 * Random.NextFloat() - 1.0),
        (float)(2.0 * Random.NextFloat() - 1.0));

      p = p.Unit();
      d = Vector3.Dot(p, normal);
    } while (d < 0);

    return p;
  }

  public static Vector3 Trace(Ray ray, int depth) {
    Vector3 color = default;
    bool did_hit = false;
    var hit = new Hit(1e15f);
    Sphere sp = default;

    foreach(var s in spheres) {
      var lh = s.Hit(ray);

      if (lh.dist > 0.0001f && lh.dist < hit.dist) {
        sp = s;
        did_hit = true;
        color = s.color;
        hit = lh;  
      }
    }

    if (did_hit && depth < MAX_DEPTH) {
      if (sp.is_light != true) {
        var nray = new Ray(
            hit.point,
            RandomDome(hit.normal)
            );
        var ncolor = Trace(nray, depth + 1);
        var at = Vector3.Dot(nray.direction, hit.normal);

        color *= ncolor * at;
      }
    }

    if (did_hit == false || depth >= MAX_DEPTH) {
      color = default;
    }

    return color;
  }

  public static void WritePPM (Vector3[][] data) {
    using (var ppm = new System.IO.StreamWriter("csrb.ppm")) {
      ppm.Write($"P3\n{WIDTH} {HEIGHT}\n255\n");

      for(int y = 0; y < data.Length; y++) {
        var dataY = data[y];
        for (int x = 0; x < dataY.Length; x++) {
          var vec = dataY[x] * 255.99f;
          int r = (int) Math.Floor(vec.X);
          int g = (int) Math.Floor(vec.Y);
          int b = (int) Math.Floor(vec.Z);
          ppm.Write($"{r} {g} {b} ");
        }
        ppm.Write("\n");
      }
    }
  }

  public static void Main (string[] args) {

    spheres[0] = new Sphere(
          new Vector3(0, -10002, 0),
          9999,
          new Vector3(1, 1, 1),
          false);

    spheres[1] = new Sphere(
          new Vector3(-10012, 0, 0),
          9999,
          new Vector3(1, 0, 0),
          false);

    spheres[2] = new Sphere(
          new Vector3(10012, 0, 0),
          9999,
          new Vector3(0, 1, 0),
          false);

    spheres[3] = new Sphere(
          new Vector3(0, 0, -10012),
          9999,
          new Vector3(1, 1, 1),
          false);

    spheres[4] = new Sphere(
          new Vector3(0, 10012, 0),
          9999,
          new Vector3(1, 1, 1),
          true);

    spheres[5] = new Sphere(
          new Vector3(-5, 0, 2),
          2,
          new Vector3(1, 1, 0),
          false);

    spheres[6] = new Sphere(
          new Vector3(0, 5, -1),
          4,
          new Vector3(1, 0, 0),
          false);

    spheres[7] = new Sphere(
          new Vector3(8, 5, -1),
          2,
          new Vector3(0, 0, 1),
          false);

    var data = new Vector3[HEIGHT][];
    var cam = new Camera();
    var vdu = (cam.rt - cam.lt) / WIDTH;
    var vdv = (cam.lb - cam.lt) / HEIGHT;

    for(int y = 0; y < data.Length; y++) {
      var dataY = new Vector3[WIDTH];
      data[y] = dataY;
      for(int x = 0; x < dataY.Length; x++) {
        var color = new Vector3();
        var ray = new Ray() { origin = cam.eye };

        for(int i = 0; i < SAMPLES; ++i) {
          ray.direction = cam.lt +
              (vdu * (x + Random.NextFloat())) +
              (vdv * (y + Random.NextFloat()));

          ray.direction -= ray.origin;
          ray.direction = ray.direction.Unit();
          color += Trace(ray, 0);
        }

        color /= SAMPLES;

        dataY[x] = color;
      }
    }

    WritePPM(data);
  }
}
