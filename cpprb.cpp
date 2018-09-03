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

    World base() {
      World res = {
        spheres = new vector<Sphere> {
          new Sphere {
            .center = new V3 {.x = 0.0f, .y = -10002.0f, .z = 0.0f},
            .radius = 9999.0f,
            .color = new V3 {.x = 1.0f, .y = 1.0f, .z = 1.0f}
          }
        }
      };
      return res;
    }
};

int main() {
  V3 a {.x=1,.y=2,.z=3};
  V3 b {.x=10,.y=20,.z=30};
  cout<<"Coso 10="<<(a+b).z<<endl;
  return 0;
}
