#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>

typedef float v3 __attribute__ ((vector_size (16)));

const uint_fast16_t WIDTH = 1280;
const uint_fast16_t HEIGHT = 720;
const uint_fast16_t SAMPLES = 50;
const uint_fast16_t MAX_DEPTH = 5;

inline float v3_dot(const v3 a, const v3 b)
{
  const v3 c = a * b;
  return c[0] + c[1] + c[2];
}

inline v3 v3_mkunit(const v3 v)
{
  v3 w = v * v;
  float len = sqrtf(w[0] + w[1] + w[2]);
  return v / len;
}

struct ray
{
  v3 origin;
  v3 direction;
};

inline v3 ray_point(const struct ray r, float t)
{
  return r.origin + r.direction * t;
}

struct camera
{
  v3 eye;
  v3 lt;
  v3 rt;
  v3 lb;
};

struct sphere
{
  v3 center;
  v3 color;
  float radius;
  bool is_light;
};

struct sphere sphere_new(v3 center, float radius, v3 color)
{
  struct sphere s = {.center = center, .radius = radius, .color = color};
  return s;
}

struct world
{
  int spheres_count;
  struct sphere* spheres;
  struct camera camera;
};

struct world world_new()
{
  struct world world;
  
  world.spheres_count = 8;
  world.spheres = (struct sphere*) malloc(sizeof(struct sphere) * world.spheres_count);

  world.spheres[0] = sphere_new((v3) { 0.0f, -10002.0f, 0.0f }, 9999.f, (v3) { 1.0f, 1.0f, 1.0f });
  world.spheres[1] = sphere_new((v3) { -10012.0f, 0.0f, 0.0f }, 9999.f, (v3) { 1.0f, 0.0f, 0.0f });
  world.spheres[2] = sphere_new((v3) { 10012.0f, 0.0f, 0.0f }, 9999.f, (v3) { 0.0f, 1.0f, 0.0f });
  world.spheres[3] = sphere_new((v3) { 0.0f, 0.0f, -10012.0f }, 9999.f, (v3) { 1.0f, 1.0f, 1.0f });
  world.spheres[4] = sphere_new((v3) { 0.0f, 10012.0f, 0.0f }, 9999.f, (v3) { 1.0f, 1.0f, 1.0f });
  world.spheres[4].is_light = true;

  world.spheres[5] = sphere_new((v3) { -5.0f, 0.0f, 2.0f }, 2.0f, (v3) { 1.0f, 1.0f, 0.0f });
  world.spheres[6] = sphere_new((v3) { 0.0f, 5.0f, -1.0f }, 4.0f, (v3) { 1.0f, 0.0f, 0.0f });
  world.spheres[7] = sphere_new((v3) { 8.0f, 5.0f, -1.0f }, 2.0f, (v3) { 0.0f, 0.0f, 1.0f });

  world.camera.eye = (v3) { 0.0f, 4.5f, 75.0f };
  world.camera.lt = (v3) { -8.0f, 9.0f, 50.0f };
  world.camera.rt = (v3) { 8.0f, 9.0f, 50.0f };
  world.camera.lb = (v3) { -8.0f, 0.0f, 50.0f };

  return world;
}

void world_del(struct world* world)
{
  free(world->spheres);
}

struct hit
{
  v3 point;
  v3 normal;
  float dist;
};

bool hit_sphere(const struct sphere* sp, const struct ray* ray, struct hit* hit)
{
  v3 oc = ray->origin - sp->center;

  float a = v3_dot(ray->direction, ray->direction);
  float b = v3_dot(oc, ray->direction);
  float c = v3_dot(oc, oc) - (sp->radius * sp->radius);
  float dis = b*b - a*c;

  if(dis > 0.0f)
  {
    float e = sqrt(dis);
    float t = (-b - e) / a;

    if(t > 0.007f)
    {
      hit->dist = t;
      hit->point = ray_point(*ray, t);
      hit->normal = v3_mkunit(hit->point - sp->center);
      return true;
    }

    t = (-b + e) / a;
    if(t > 0.007f)
    {
      hit->dist = t;
      hit->point = ray_point(*ray, t);
      hit->normal = v3_mkunit(hit->point - sp->center);
      return true;
    }

    return false;
  }

  return false;
}

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

v3 rnd_dome(const v3 normal)
{
  v3 p;
  float d;
  do
  {
    p = v3_mkunit((v3) {
      2.0f * randf() - 1.0f,
      2.0f * randf() - 1.0f,
      2.0f * randf() - 1.0f,
    });
    d = v3_dot(p, normal);
  } while(d <= 0);

  return p;
}

v3 trace(struct world* world, struct ray* ray, uint_fast16_t depth)
{
  v3 color = {0};
  bool did_hit = false;
  struct hit hit = {.dist = 1e15};
  struct sphere* sp;
  for(uint_fast16_t i = 0 ; i < world->spheres_count; ++i)
  {
    struct hit res;
    if (hit_sphere(&world->spheres[i], ray, &res))
    {
      if(res.dist > 0.0001f && res.dist < hit.dist) {
        sp = &world->spheres[i];
        did_hit = true;
        color = sp->color;
        hit = res;
      }
    }
  }

  if(did_hit == true && depth < MAX_DEPTH)
  {
    if(sp->is_light == false)
    {
      struct ray nray;
      nray.origin = hit.point;
      nray.direction = rnd_dome(hit.normal);
      v3 ncolor = trace(world, &nray, depth + 1);
      float at = v3_dot(nray.direction, hit.normal);
      ncolor *= at;
      color *= ncolor;
    }
    else
    {
      color = sp->color;
    }
  }

  if(did_hit == false || depth >= MAX_DEPTH) {
    color = (v3) {0};
  }

  return color;
}

void writeppm(v3 *data)
{
  FILE *ppm = fopen("crb-vec.ppm", "w+");
  fprintf(ppm,"P3\n%u %u\n255\n", WIDTH, HEIGHT);

  for(uint_fast16_t y = 0; y < HEIGHT; ++y)
  {
    for(uint_fast16_t x = 0; x < WIDTH; ++x)
    {
      
      fprintf(ppm, "%u %u %u ", 
          (uint_fast16_t)(data[y * WIDTH + x][0] * 255.99f), 
          (uint_fast16_t)(data[y * WIDTH + x][1] * 255.99f),
          (uint_fast16_t)(data[y * WIDTH + x][2] * 255.99f)
          );
    }
    
    fprintf(ppm, "\n");
  }

  fclose(ppm);

}

int main (int argc, char** argv)
{
  
  struct world world = world_new();
  
  v3 *data = malloc(HEIGHT * WIDTH * sizeof(v3));

  v3 vdu = (world.camera.rt - world.camera.lt) / (float) WIDTH;
  v3 vdv = (world.camera.lb - world.camera.lt) / (float) HEIGHT;
  v3 eye = world.camera.eye;

  for(uint_fast16_t y = 0; y < HEIGHT; ++y)
  {
    for(uint_fast16_t x = 0; x < WIDTH; ++x)
    {

      struct ray r;
      r.origin = eye;
      
      v3 c = {0};

      for(uint_fast16_t s = 0; s < SAMPLES; ++s)
      {        
        v3 u = vdu * ((float)x + randf());
        v3 v = vdv * ((float)y + randf());

        r.direction = v3_mkunit(world.camera.lt + u + v - eye);

        c += trace(&world, &r, 0);
      }

      c /= (float)SAMPLES;

      data[y * WIDTH + x] = c;
    }
  }

  writeppm(data);

  free(data);

  world_del(&world);

  return 0;
}
