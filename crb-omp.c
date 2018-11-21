#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <omp.h>

const uint_fast16_t WIDTH = 1280;
const uint_fast16_t HEIGHT = 720;
const uint_fast16_t SAMPLES = 50;
const uint_fast16_t MAX_DEPTH = 5;

struct v3
{
  float x;
  float y;
  float z;
};

struct v3 v3_new(float x, float y, float z)
{
  struct v3 r = {
    .x = x,
    .y = y,
    .z = z
  };
  
  return r;
}

void v3_add(struct v3* dest, const struct v3* a, const struct v3* b)
{
  dest->x = a->x + b->x;
  dest->y = a->y + b->y;
  dest->z = a->z + b->z;
}

void v3_mul(struct v3* dest, const struct v3* a, const struct v3* b)
{
  dest->x = a->x * b->x;
  dest->y = a->y * b->y;
  dest->z = a->z * b->z;
}

void v3_sub(struct v3* dest, const struct v3* a, const struct v3* b)
{
  dest->x = a->x - b->x;
  dest->y = a->y - b->y;
  dest->z = a->z - b->z;
}

float v3_dot(const struct v3* a, const struct v3* b)
{
  return a->x * b->x +
         a->y * b->y +
         a->z * b->z;
}

void v3_muls(struct v3* dest, const struct v3* a, float u)
{
  dest->x = a->x * u;
  dest->y = a->y * u;
  dest->z = a->z * u;
}

void v3_divs(struct v3* dest, const struct v3* a, float u)
{
  dest->x = a->x / u;
  dest->y = a->y / u;
  dest->z = a->z / u;
}

float v3_norm(const struct v3* v)
{
  return sqrtf(
      v->x * v->x +
      v->y * v->y +
      v->z * v->z
      );
}

void v3_mkunit(struct v3* dest, const struct v3* v)
{
  float n = v3_norm(v);

  dest->x = v->x / n;
  dest->y = v->y / n;
  dest->z = v->z / n;
}

struct ray
{
  struct v3 origin;
  struct v3 direction;
};

void ray_point(struct v3* dest, const struct ray* r, float t)
{
  dest->x = r->origin.x + r->direction.x * t;
  dest->y = r->origin.y + r->direction.y * t;
  dest->z = r->origin.z + r->direction.z * t;
}

struct camera
{
  struct v3 eye;
  struct v3 lt;
  struct v3 rt;
  struct v3 lb;
};

struct sphere
{
  struct v3 center;
  float radius;
  struct v3 color;
  bool is_light;
};

struct sphere sphere_new(struct v3 center, float radius, struct v3 color)
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

  world.spheres[0] = sphere_new(v3_new(0.0f, -10002.0f, 0.0f), 9999.f, v3_new(1.0f,1.0f,1.0f));
  world.spheres[1] = sphere_new(v3_new(-10012.0f, 0.0f, 0.0f), 9999.f, v3_new(1.0f,0.0f,0.0f));
  world.spheres[2] = sphere_new(v3_new(10012.0f, 0.0f, 0.0f), 9999.f, v3_new(0.0f,1.0f,0.0f));
  world.spheres[3] = sphere_new(v3_new(0.0f, 0.0f, -10012.0f), 9999.f, v3_new(1.0f,1.0f,1.0f));
  world.spheres[4] = sphere_new(v3_new(0.0f, 10012.0f, 0.0f), 9999.f, v3_new(1.0f,1.0f,1.0f));
  world.spheres[4].is_light = true;

  world.spheres[5] = sphere_new(v3_new(-5.0f, 0.0f, 2.0f), 2.0f, v3_new(1.0f,1.0f,0.0f));
  world.spheres[6] = sphere_new(v3_new(0.0f, 5.0f, -1.0f), 4.0f,v3_new(1.0f,0.0f,0.0f));
  world.spheres[7] = sphere_new(v3_new(8.0f, 5.0f, -1.0f), 2.0f,v3_new(0.0f,0.0f,1.0f));

  world.camera.eye.x = 0.0f;
  world.camera.eye.y = 4.5f;
  world.camera.eye.z = 75.0f;

  world.camera.lt.x = -8.0f;
  world.camera.lt.y =  9.0f;
  world.camera.lt.z =  50.0f;
  
  world.camera.rt.x = 8.0f;
  world.camera.rt.y = 9.0f;
  world.camera.rt.z = 50.0f;

  world.camera.lb.x = -8.0f;
  world.camera.lb.y =  0.0f;
  world.camera.lb.z =  50.0f;

  return world;
}

void world_del(struct world* world)
{
  free(world->spheres);
}

struct hit
{
  float dist;
  struct v3 point;
  struct v3 normal;
};

bool hit_sphere(const struct sphere* sp, const struct ray* ray, struct hit* hit)
{
  struct v3 oc;
  v3_sub(&oc, &ray->origin, &sp->center);

  float a = v3_dot(&ray->direction, &ray->direction);
  float b = v3_dot(&oc, &ray->direction);
  float c = v3_dot(&oc, &oc) - (sp->radius * sp->radius);
  float dis = b*b - a*c;

  if(dis > 0.0f)
  {
    float e = sqrt(dis);
    float t = (-b - e) / a;

    if(t > 0.007f)
    {
      hit->dist = t;
      ray_point(&hit->point, ray, t);
      v3_sub(&hit->normal, &hit->point, &sp->center);
      v3_mkunit(&hit->normal, &hit->normal);
      return true;
    }

    t = (-b + e) / a;
    if(t > 0.007f)
    {
      hit->dist = t;
      ray_point(&hit->point, ray, t);
      v3_sub(&hit->normal, &hit->point, &sp->center);
      v3_mkunit(&hit->normal, &hit->normal);
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

struct v3 rnd_dome(const struct v3* normal)
{
  struct v3 p;
  float d;
  do
  {
    p.x = 2.0f * randf() - 1.0f;
    p.y = 2.0f * randf() - 1.0f;
    p.z = 2.0f * randf() - 1.0f;

    v3_mkunit(&p, &p);
    
    d = v3_dot(&p, normal);
  } while(d <= 0);

  return p;
}

struct v3 trace(struct world* world, struct ray* ray, uint_fast16_t depth)
{
  struct v3 color = {0};
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
      nray.direction = rnd_dome(&hit.normal);
      struct v3 ncolor = {0};
      ncolor = trace(world, &nray, depth + 1);
      float at = v3_dot(&nray.direction, &hit.normal);
      v3_muls(&ncolor, &ncolor, at);
      v3_mul(&color, &color, &ncolor);
    }
    else
    {
      color = sp->color;
    }
  }

  if(did_hit == false || depth >= MAX_DEPTH) {
    color.x = 0;
    color.y = 0;
    color.z = 0;
  }

  return color;
}

void writeppm(struct v3 *data)
{
  FILE *ppm = fopen("crb-omp.ppm","w+");
  fprintf(ppm,"P3\n%u %u\n255\n", WIDTH, HEIGHT);

  for(uint_fast16_t y = 0; y < HEIGHT; ++y)
  {
    for(uint_fast16_t x = 0; x < WIDTH; ++x)
    {
      
      fprintf(ppm, "%u %u %u ", 
          (uint_fast16_t)(data[y * WIDTH + x].x * 255.99f), 
          (uint_fast16_t)(data[y * WIDTH + x].y * 255.99f),
          (uint_fast16_t)(data[y * WIDTH + x].z * 255.99f)
          );
    }
    
    fprintf(ppm, "\n");
  }

  fclose(ppm);

}

int main (int argc, char** argv)
{
  
  struct world world = world_new();
  
  struct v3 *data;
  data = malloc(HEIGHT * WIDTH * sizeof(struct v3));

  struct v3 vdu = {0};
  v3_sub(&vdu, &world.camera.rt, &world.camera.lt);
  v3_divs(&vdu, &vdu, (float) WIDTH);

  struct v3 vdv = {0};
  v3_sub(&vdv, &world.camera.lb, &world.camera.lt);
  v3_divs(&vdv, &vdv, (float) HEIGHT);
#pragma omp parallel
  {
#pragma omp for 
    for(uint_fast16_t y = 0; y < HEIGHT; ++y)
    {
      for(uint_fast16_t x = 0; x < WIDTH; ++x)
      {

        struct ray r;
        r.origin = world.camera.eye;

        struct v3 u;
        struct v3 v;
        struct v3 c = {0};

        for(uint_fast16_t s = 0; s < SAMPLES; ++s)
        {
          r.direction = world.camera.lt;

          v3_muls(&u, &vdu, (float)x + randf());
          v3_muls(&v, &vdv, (float)y + randf());

          v3_add(&r.direction, &r.direction, &u);
          v3_add(&r.direction, &r.direction, &v);

          v3_sub(&r.direction, &r.direction, &r.origin);

          v3_mkunit(&r.direction, &r.direction);
          u = trace(&world, &r, 0);
          v3_add(&c, &c, &u);
        }

        v3_divs(&c, &c, (float)SAMPLES);

        data[y * WIDTH + x] = c;
      }
    }
  }

  writeppm(data);

  free(data);

  world_del(&world);

  return 0;
}
