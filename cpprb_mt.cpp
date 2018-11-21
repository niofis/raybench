#include <iostream>
#include <cmath>
#include <vector>
#include <numeric>
#include <parallel/algorithm>
#include <fstream>
#include <limits>

using namespace std;

const uint_fast16_t WIDTH = 1280;
const uint_fast16_t HEIGHT = 720;
const uint_fast16_t SAMPLES = 50;
const uint_fast16_t MAX_DEPTH = 5;

class V3 {
  public:
    float x;
    float y;
    float z;

    const V3 operator+(const V3 &b) const {
      return V3 {
        .x = this->x + b.x,
          .y = this->y + b.y,
          .z = this->z + b.z
      };
    }

    const V3 operator*(const V3 &b) const {
      return V3 {
        .x = this->x * b.x,
          .y = this->y * b.y,
          .z = this->z * b.z
      };
    }

    const V3 operator-(const V3 &b) const {
      return V3 {
        .x = this->x - b.x,
          .y = this->y - b.y,
          .z = this->z - b.z
      };
    }

    const V3 operator*(float s) const {
      return V3 {
        .x = this->x * s,
          .y = this->y * s,
          .z = this->z * s
      };
    }

    const V3 operator/(float s) const {
      return V3 {
        .x = this->x / s,
          .y = this->y / s,
          .z = this->z / s
      };
    }

    float dot(const V3 &b) const {
      return this->x * b.x +
        this->y * b.y +
        this->z * b.z;
    }

    float norm() const {
      return sqrtf(
          this->x * this->x +
          this->y * this->y +
          this->z * this->z
          );
    }

    const V3 unit() const {
      float n = this->norm();
      return (*this) / n;
    }
};

class Ray {
  public:
    V3 origin;
    V3 direction;

    const V3 point(float t) const {
      V3 tmp = this->direction * t;
      return this->origin + tmp;
    }
};

class Camera {
  public:
    V3 eye;
    V3 lt;
    V3 rt;
    V3 lb;
};

class Hit {
  public:
    float dist;
    V3 point;
    V3 normal;
};

class Sphere {
  public:
    V3 center;
    float radius;
    V3 color;
    bool is_light;

    const Hit hit(Ray &ray) const
    {
      const V3 oc = ray.origin - this->center;
      const float a = ray.direction.dot(ray.direction);
      const float b = oc.dot(ray.direction);
      const float c = oc.dot(oc) - (this->radius * this->radius);
      const float dis = b * b - a * c;

      if(dis > 0.0f)
      {
        const float e = sqrtf(dis);
        const float t1 = (-b - e) / a;

        if(t1 > 0.007f)
        {
          const V3 point = ray.point(t1);
          return Hit {
            .dist = t1,
              .point = point,
              .normal = (point - this->center).unit()
          };
        }

        const float t2 = (-b + e) / a;
        if(t2 > 0.007f)
        {
          const V3 point = ray.point(t2);
          return Hit {
            .dist = t2,
              .point = point,
              .normal = (point - this->center).unit()
          };
        }
      }

      return Hit {0};
    }
};

//https://codingforspeed.com/using-faster-psudo-random-generator-xorshift/
uint32_t xor128(void) {
  static uint32_t x = 123456789;
  static uint32_t y = 362436069;
  static uint32_t z = 521288629;
  static uint32_t w = 88675123;
  uint32_t t;
  t = x ^ (x << 11);   
  x = y; y = z; z = w;   
  return w = w ^ (w >> 19) ^ (t ^ (t >> 8));
}


float randf()
{
  return (float)xor128() / (float)UINT32_MAX;
}

const V3 rnd_dome(V3 &normal)
{
  V3 p;
  float d;
  do
  {
    p.x = 2.0f * randf() - 1.0f;
    p.y = 2.0f * randf() - 1.0f;
    p.z = 2.0f * randf() - 1.0f;

    d = p.unit().dot(normal);
  } while(d <= 0);

  return p;
}

class World {
  public:
    vector<Sphere> spheres;
    Camera camera;

    World() {
      this->spheres = vector<Sphere> {
        Sphere {
          .center = V3 {.x = 0.0f, .y = -10002.0f, .z = 0.0f},
            .radius = 9999.0f,
            .color = V3 {.x = 1.0f, .y = 1.0f, .z = 1.0f}
        },
               Sphere {
                 .center = V3 {.x = -10012.0f, .y = 0.0f, .z = 0.0f},
                 .radius = 9999.0f,
                 .color = V3 {.x = 1.0f, .y = 0.0f, .z = 0.0f}
               },
               Sphere {
                 .center = V3 {.x = 10012.0f, .y = 0.0f, .z = 0.0f},
                 .radius = 9999.0f,
                 .color = V3 {.x = 0.0f, .y = 1.0f, .z = 0.0f}
               },
               Sphere {
                 .center = V3 {.x = 0.0f, .y = 0.0f, .z = -10012.0f},
                 .radius = 9999.0f,
                 .color = V3 {.x = 1.0f, .y = 1.0f, .z = 1.0f}
               },
               Sphere {
                 .center = V3 {.x = 0.0f, .y = 10012.0f, .z = 0.0f},
                 .radius = 9999.0f,
                 .color = V3 {.x = 1.0f, .y = 1.0f, .z = 1.0f},
                 .is_light = true
               },
               Sphere {
                 .center = V3 {.x = -5.0f, .y = 0.0f, .z = 2.0f},
                 .radius = 2.0f,
                 .color = V3 {.x = 1.0f, .y = 1.0f, .z = 0.0f}
               },
               Sphere {
                 .center = V3 {.x = 0.0f, .y = 5.0f, .z = -1.0f},
                 .radius = 4.0f,
                 .color = V3 {.x = 1.0f, .y = 0.0f, .z = 0.0f}
               },
               Sphere {
                 .center = V3 {.x = 8.0f, .y = 5.0f, .z = -1.0f},
                 .radius = 2.0f,
                 .color = V3 {.x = 0.0f, .y = 0.0f, .z = 1.0f}
               }
      };

      this->camera = Camera {
        .eye = V3 {.x = 0.0f, .y = 4.5f, .z = 75.0f},
          .lt = V3 {.x = -8.0f, .y = 9.0f, .z = 50.f},
          .rt = V3 {.x = 8.0f, .y = 9.0f, .z = 50.0f},
          .lb = V3 {.x = -8.0f, .y = 0.0f, .z = 50.0f}
      };
    }

    V3 trace(Ray &ray, uint_fast16_t depth) const
    {
      if(depth >= MAX_DEPTH)
      {
        return V3 {0};
      }

      V3 color = {0};
      bool did_hit = false;
      Hit hit = {.dist = 1e15};
      Sphere sp;
      for(auto sphere : this->spheres)
      {
        Hit res = sphere.hit(ray);
        if(res.dist > 0.0001f && res.dist < hit.dist)
        {
          sp = sphere;
          did_hit = true;
          color = sp.color;
          hit = res;
        }
      }

      if(did_hit && depth < MAX_DEPTH)
      {
        if(!sp.is_light)
        {
          Ray nray = {
            .origin = hit.point,
            .direction = rnd_dome(hit.normal)
          };
          V3 ncolor = this->trace(nray, depth + 1);
          const float a = nray.direction.dot(hit.normal);
          ncolor = ncolor * a;
          color = color * ncolor;
        }
        else
        {
          color = sp.color;
        }
      }

      return color;
    }
};


void writeppm(vector<V3> data)
{
  ofstream ppm;
  ppm.open("cpprb_mt.ppm");
  ppm<<"P3\n"<<WIDTH<<" "<<HEIGHT<<"\n255"<<endl;

  for(uint_fast16_t y = 0; y < HEIGHT; ++y)
  {
    for(uint_fast16_t x = 0; x < WIDTH; ++x)
    {
      V3 p = data[y* WIDTH + x];
      ppm
        <<(uint_fast16_t)(p.x * 255.99f)<<" "
        <<(uint_fast16_t)(p.y * 255.99f)<<" "
        <<(uint_fast16_t)(p.z * 255.99f)<<" ";
    }
    ppm<<endl;
  }
  ppm.close();
}

int main()
{
  const World world;
  vector<V3> data(WIDTH * HEIGHT);
  const V3 vdu = (world.camera.rt - world.camera.lt) / (float) WIDTH;
  const V3 vdv = (world.camera.lb - world.camera.lt) / (float) HEIGHT;

  vector<int> pixels(WIDTH * HEIGHT);
  iota(pixels.begin(), pixels.end(), 0);

  __gnu_parallel::transform(pixels.begin(), pixels.end(), data.begin(),
      [&world, &vdu, &vdv](int pixel) -> V3 {
      const int x = pixel % WIDTH;
      const int y = pixel / WIDTH;
      Ray r = {
      .origin = world.camera.eye
      };

      V3 u;
      V3 v;
      V3 c = {0};

      for(uint_fast16_t s = 0; s < SAMPLES; ++s)
      {
      u = vdu * ((float)x + randf());
      v = vdv * ((float)y + randf());
      r.direction = ((world.camera.lt + u + v) - r.origin).unit();
      u = world.trace(r, 0);
      c = c + u;
      }
      //data[y * WIDTH + x] = c / (float)SAMPLES;
      return c / (float)SAMPLES;
      });

  writeppm(data);

  return 0;
}
