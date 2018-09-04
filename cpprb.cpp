#include <iostream>
#include <cmath>
#include <vector>

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

    V3 operator+(const V3 &b) {
      V3 res {
        .x = this->x + b.x,
          .y = this->y + b.y,
          .z = this->z + b.z
      };
      return res;
    }

    V3 operator*(const V3 &b) {
      V3 res {
        .x = this->x * b.x,
          .y = this->y * b.y,
          .z = this->z * b.z
      };
      return res;
    }

    V3 operator-(const V3 &b) {
      V3 res {
        .x = this->x - b.x,
          .y = this->y - b.y,
          .z = this->z - b.z
      };
      return res;
    }

    V3 operator*(float s) {
      V3 res {
        .x = this->x * s,
          .y = this->y * s,
          .z = this->z * s
      };
      return res;
    }

    V3 operator/(const V3 &b) {
      V3 res {
        .x = this->x / b.x,
          .y = this->y / b.y,
          .z = this->z / b.z
      };
      return res;
    }

    V3 operator/(float s) {
      V3 res {
        .x = this->x / s,
          .y = this->y / s,
          .z = this->z / s
      };
      return res;
    }

    float dot(const V3 &b) {
      return  this->x * b.x +
        this->y * b.y +
        this->z * b.z;
    }

    float norm() {
      return sqrtf(
          this->x * this->x +
          this->y * this->y +
          this->z * this->z
          );
    }

    V3 unit() {
      float n = this->norm();
      return (*this) / n;
    }
};

class Ray {
  public:
    V3 origin;
    V3 direction;

    V3 point(float t) {
      return this->origin + (this->direction * t);
    }
};

class Camera {
  public:
    V3 eye;
    V3 lt;
    V3 rt;
    V3 lb;
};

class Sphere {
  public:
    V3 center;
    V3 color;
    float radius;
    bool is_light;
};

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
        eye = V3 {.x = 0.0f, .y = 4.5f, .z = 75.0f},
        lt = V3 {.x = -8.0f, .y = 9.0f, .z = 50.f},
        rt = V3 {.x = 8.0f, .y = 9.0f, .z = 50.0f},
        lb = V3 {.x = -8.0f, .y = 0.0f, .z = 50.0f}
      };
    }
};

int main() {
  World world;
  V3 vdu = (world.camera.rt - world.camera.lt) / (float) WIDTH;
  V3 vdu = (world.camera.lb - world.camera.lt) / (float) HEIGHT;

  for(uint_fast16_t y = 0; y < HEIGHT; ++y)
  {
    for(uint_fast16_t x = 0; x < WIDTH; ++x)
    {
      Ray r = {
        .origin = world.camera.lt
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
    }
  }

  return 0;
}
