#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>

const uint32_t WIDTH = 1280;
const uint32_t HEIGHT = 720;

struct v3
{
  float x;
  float y;
  float z;
};

static const struct v3 v3zero = {0};

struct v3 v3new(float x, float y, float z)
{
  struct v3 r = {
    .x = x,
    .y = y,
    .z = z
  };
  
  return r;
}

void v3add(struct v3* dest, const struct v3* a, const struct v3* b)
{
  dest->x = a->x + b->x;
  dest->y = a->y + b->y;
  dest->z = a->z + b->z;
}

void v3sub(struct v3* dest, const struct v3* a, const struct v3* b)
{
  dest->x = a->x - b->x;
  dest->y = a->y - b->y;
  dest->z = a->z - b->z;
}

float v3dot(const struct v3* a, const struct v3* b)
{
  return a->x * b->x +
         a->y * b->y +
         a->z * b->z;
}

void v3muls(struct v3* dest, const struct v3* a, float u)
{
  dest->x = a->x * u;
  dest->y = a->y * u;
  dest->z = a->z * u;
}

void v3divs(struct v3* dest, const struct v3* a, float u)
{
  dest->x = a->x / u;
  dest->y = a->y / u;
  dest->z = a->z / u;
}

float v3norm(const struct v3* v)
{
  return sqrtf(
      v->x * v->x +
      v->y * v->y +
      v->z * v->z
      );
}

void v3mkunit(struct v3* dest, const struct v3* v)
{
  float n = v3norm(v);

  dest->x = v->x / n;
  dest->y = v->y / n;
  dest->z = v->z / n;
}

void v3print(const struct v3* v)
{
  printf("(%f, %f, %f)\n", v->x, v->y, v->z);
} 

struct ray
{
  struct v3 origin;
  struct v3 direction;
};

static const struct ray rayzero = {0};

struct camera
{
  struct v3 eye;
  struct v3 lt;
  struct v3 rt;
  struct v3 lb;
};

const struct camera cam = {
  .eye = {
    .x = 0.0f,
    .y = 4.5f,
    .z = 75.0f
  },
  .lt = {
    .x = -8.0f,
    .y =  9.0f,
    .z =  50.0f
  },
  .rt = {
    .x = 8.0f,
    .y = 9.0f,
    .z = 50.0f
  },
  .lb = {
    .x = -8.0f,
    .y =  0.0f,
    .z =  50.0f
  }
};

struct sphere
{
  struct v3 center;
  float radius;
};

const uint32_t spheres_count = 2;
struct sphere spheres[spheres_count];

struct sphere spherenew(struct v3 center, float radius)
{
  struct sphere s = {.center = center, .radius = radius};
  return s;
}

struct hit
{
  float dist;
};

float hit_sphere(const struct sphere* sp, const struct ray* ray)
{
  struct v3 oc;
  v3sub(&oc, &ray->origin, &sp->center);

  float a = v3dot(&ray->direction, &ray->direction);
  float b = -2.0f * v3dot(&oc, &ray->direction);
  float c = v3dot(&oc, &oc) - (sp->radius * sp->radius);
  float dis = b*b - 4.0f*a*c;

  if(dis > 0.0f)
  {
    float e = sqrt(dis);
    float denom = 2.0f * a;
    float t = (b - e) / denom;

    if(t > 0.007f)
    {
      return t;
    }

    t = (b + e) / denom;
    if(t > 0.007f)
    {
      return t;
    }

    return -1.0f;
  }

  return -1.0f;
}

struct v3 trace(struct ray* ray)
{
  struct v3 color = {0};
  bool hit = false;

  float min = 1e15;
  struct sphere *csp;
  for(uint32_t i = 0 ; i < spheres_count; ++i)
  {
    float t = hit_sphere(&spheres[i], ray);
    if (t > 0.0f && t < min)
    {
      csp = &spheres[i];
      min = t;
      hit = true;
      color.x = 1.0f;
    }
  }

  if(hit == false)
  {

    struct v3 dir = ray->direction;

    float t = 0.5f * (dir.y + 1.0f);


    struct v3 a = v3new(1.0f,1.0f,1.0f);
    v3muls(&a, &a, 1.0f - t);

    struct v3 b = v3new(0.5f, 0.7f, 1.0f);
    v3muls(&b, &b, t);


    v3add(&color, &a, &b);

  }

  return color;
}

void writeppm(struct v3 *data)
{
  FILE *ppm = fopen("crb.ppm","w+");
  fprintf(ppm,"P3\n%u %u\n255\n", WIDTH, HEIGHT);

  for(uint32_t y = 0; y < HEIGHT; ++y)
  {
    for(uint32_t x = 0; x < WIDTH; ++x)
    {
      
      fprintf(ppm, "%u %u %u ", 
          (uint32_t)(data[y * WIDTH + x].x * 255.99f), 
          (uint32_t)(data[y * WIDTH + x].y * 255.99f),
          (uint32_t)(data[y * WIDTH + x].z * 255.99f)
          );
    }
    
    fprintf(ppm, "\n");
  }

  fclose(ppm);

}

int main (int argc, char** argv)
{
  spheres[0] = spherenew(v3new(0.0f, 4.5f, -5.0f), 5.0f);
  spheres[1] = spherenew(v3new(0.0f, -999.0f, 0.0f), 1000.f);

  struct v3 *data;
  data = malloc(HEIGHT * WIDTH * sizeof(struct v3));

  struct v3 vdu = {0};
  v3sub(&vdu, &cam.rt, &cam.lt);
  v3divs(&vdu, &vdu, (float) WIDTH);

  struct v3 vdv = {0};
  v3sub(&vdv, &cam.lb, &cam.lt);
  v3divs(&vdv, &vdv, (float) HEIGHT);

  for(uint32_t y = 0; y < HEIGHT; ++y)
  {
    for(uint32_t x = 0; x < WIDTH; ++x)
    {
      struct ray r;
      r.origin = cam.eye;
      r.direction = cam.lt;
      
      struct v3 u = vdu;
      struct v3 v = vdv;

      v3muls(&u, &vdu, (float)x);
      v3muls(&v, &vdv, (float)y);

      v3add(&r.direction, &r.direction, &u);
      v3add(&r.direction, &r.direction, &v);

      v3sub(&r.direction, &r.direction, &r.origin);

      v3mkunit(&r.direction, &r.direction);

      data[y * WIDTH + x] = trace(&r);
    }
  }

  writeppm(data);

  free(data);

  return 0;
}
