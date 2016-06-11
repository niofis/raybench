using System;

class Vector3 {

  public float x;
  public float y;
  public float z;

  public Vector3 () {
    this.x = 0;
    this.y = 0;
    this.z = 0;
  }

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

class Ray {
  public Vector3 origin;
  public Vector3 direction;

  public Ray () {
  }

  public Ray (Vector3 origin, Vector3 direction) {
    this.origin = origin;
    this.direction = direction;
  }

  public Vector3 Point (float dist) {
    return this.origin.Add(this.direction.Mul(dist));
  }
}

class Hit {
  public float dist;
  public Vector3 point;
  public Vector3 normal;

  public Hit () {
    this.dist = 0;
    this.point = new Vector3();
    this.normal = new Vector3();
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
    this.eye = new Vector3(0.0f, 4.5f, 75.0f);
    this.lt = new Vector3(-8, 9, 50);
    this.rt = new Vector3(8, 9, 50);
    this.lb = new Vector3(-8, 0, 50);
  }
}

class Sphere {
  public Vector3 center;
  public float radius;
  public Vector3 color;
  public bool is_light;

  public Sphere () {
    this.center = new Vector3();
    this.radius = 1;
    this.color = new Vector3(1,0,0);
    this.is_light = false;
  }

  public Sphere (Vector3 center, float radius, Vector3 color, bool is_light) {
    this.center = center;
    this.radius = radius;
    this.color = color;
    this.is_light = is_light;
  }

  public Hit Hit (Ray ray) {
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
  public static Random rnd = new Random();

  public static Vector3 rnd_dome (Vector3 normal) {
    var p = new Vector3();
    float d;

    do {
      p.x = (float)(2.0 * rnd.NextDouble() - 1.0);
      p.y = (float)(2.0 * rnd.NextDouble() - 1.0);
      p.z = (float)(2.0 * rnd.NextDouble() - 1.0);

      p = p.Unit();
      d = p.Dot(normal);
    } while (d < 0);

    return p;
  }

  public static Vector3 trace(Ray ray, int depth) {
    var color = new Vector3();
    bool did_hit = false;
    var hit = new Hit();
    Sphere sp = null;

    hit.dist = 1e15f;

    foreach(var s in spheres) {
      var lh = s.Hit(ray);

      if (lh !=null && lh.dist > 0.0001f && lh.dist < hit.dist) {
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
            RayBench.rnd_dome(hit.normal)
            );
        var ncolor = RayBench.trace(nray, depth + 1);
        var at = nray.direction.Dot(hit.normal);

        color = color.Mul(ncolor.Mul(at));
      }
    }

    if (did_hit == false || depth >= MAX_DEPTH) {
      color = new Vector3();
    }

    return color;
  }

  public static void WritePPM (Vector3[][] data) {
    using (var ppm = new System.IO.StreamWriter("csrb.ppm")) {
      ppm.Write(String.Format("P3\n{0} {1}\n255\n", RayBench.WIDTH, RayBench.HEIGHT));

      for(int y = 0; y < HEIGHT; ++y) {
        for(int x = 0; x < WIDTH; ++x) {
          int r = (int) Math.Floor(data[y][x].x * 255.99);
          int g = (int) Math.Floor(data[y][x].y * 255.99);
          int b = (int) Math.Floor(data[y][x].z * 255.99);
          ppm.Write(String.Format("{0} {1} {2} ", r, g, b));
        }
        ppm.Write("\n");
      }

      ppm.Close();
    }
  }

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
    var cam = new Camera();
    var vdu = cam.rt.Sub(cam.lt).Div(RayBench.WIDTH);
    var vdv = cam.lb.Sub(cam.lt).Div(RayBench.HEIGHT);

    for(int y = 0; y < RayBench.HEIGHT; ++y) {
      data[y] = new Vector3[RayBench.WIDTH];
      for(int x = 0; x < RayBench.WIDTH; ++x) {
        var color = new Vector3();
        var ray = new Ray();

        ray.origin = cam.eye;

        for(int i = 0; i < RayBench.SAMPLES; ++i) {
          ray.direction = cam.lt.Add(
              vdu.Mul((float)(x + rnd.NextDouble())).Add(
                vdv.Mul((float)(y + rnd.NextDouble()))));

          ray.direction = ray.direction.Sub(ray.origin);
          ray.direction = ray.direction.Unit();
          color = color.Add(RayBench.trace(ray, 0));
        }

        color = color.Div(RayBench.SAMPLES);

        data[y][x] = color;
      }
    }

    RayBench.WritePPM(data);
  }
}
