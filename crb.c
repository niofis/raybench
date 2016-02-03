#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>

const uint_fast16_t WIDTH = 1280;
const uint_fast16_t HEIGHT = 720;
const uint_fast16_t SAMPLES = 100;
const uint_fast16_t MAX_DEPTH = 10;

struct v3
{
  float x;
  float y;
  float z;
};

static const struct v3 v3zero = {0};

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

void v3_dump(const struct v3* v)
{
  printf("(%f, %f, %f)\n", v->x, v->y, v->z);
} 

struct ray
{
  struct v3 origin;
  struct v3 direction;
};

static const struct ray rayzero = {0};

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
  struct v3 color;
};

const uint_fast16_t spheres_count = 2;
struct sphere spheres[spheres_count];

struct sphere sphere_new(struct v3 center, float radius, struct v3 color)
{
  struct sphere s = {.center = center, .radius = radius, .color = color};
  return s;
}

struct hit
{
  float dist;
  struct v3 point;
  struct v3 normal;
};

bool hit_sphere(const struct sphere* sp, const struct ray* ray, struct hit* res)
{
  struct v3 oc;
  v3_sub(&oc, &ray->origin, &sp->center);

  float a = v3_dot(&ray->direction, &ray->direction);
  float b = -2.0f * v3_dot(&oc, &ray->direction);
  float c = v3_dot(&oc, &oc) - (sp->radius * sp->radius);
  float dis = b*b - 4.0f*a*c;

  if(dis > 0.0f)
  {
    float e = sqrt(dis);
    float denom = 2.0f * a;
    float t = (b - e) / denom;

    if(t > 0.007f)
    {
      res->dist = t;
      ray_point(&res->point, ray, t);
      v3_sub(&res->normal, &res->point, &sp->center);
      v3_mkunit(&res->normal, &res->normal);
      return true;
    }

    t = (b + e) / denom;
    if(t > 0.007f)
    {
      res->dist = t;
      ray_point(&res->point, ray, t);
      v3_sub(&res->normal, &res->point, &sp->center);
      v3_mkunit(&res->normal, &res->normal);
      return true;
    }

    return false;
  }

  return false;
}

struct v3 rnd_dome(const struct v3* normal)
{
  struct v3 p;
  float d;
  do
  {
    p.x = 2.0f * drand48() - 1.0f;
    p.y = 2.0f * drand48() - 1.0f;
    p.z = 2.0f * drand48() - 1.0f;

    v3_mkunit(&p, &p);
    
    d = v3_dot(&p, normal);
  } while(d < 0);

  return p;
}

struct v3 trace(struct ray* ray, uint_fast16_t depth)
{
  struct v3 color = {0};
  bool did_hit = false;

  float min = 1e15;
  struct hit hit = {0};
  struct sphere *csp;
  for(uint_fast16_t i = 0 ; i < spheres_count; ++i)
  {
    if (hit_sphere(&spheres[i], ray, &hit))
    {
      if(hit.dist > 0.0001f && hit.dist < min) {
        csp = &spheres[i];
        min = hit.dist;
        did_hit = true;
        color = csp->color;
      }
    }
  }


  if(did_hit == true && depth < MAX_DEPTH)
  {
    struct ray nray;
    nray.origin = hit.point;
    nray.direction = rnd_dome(&hit.normal);
    struct v3 ncolor = {0};
    ncolor = trace(&nray, depth + 1);
    //v3_muls(&ncolor, &ncolor, 0.2f);
    //v3_muls(&color, &color, 0.8f);
    //v3_add(&color, &color, &ncolor);
    v3_mul(&color, &color, &ncolor);
  }

  if(did_hit == false)
  {

    struct v3 dir = ray->direction;

    float t = 0.5f * (dir.y + 1.0f);


    struct v3 a = v3_new(1.0f,1.0f,1.0f);
    v3_muls(&a, &a, 1.0f - t);

    struct v3 b = v3_new(0.5f, 0.7f, 1.0f);
    v3_muls(&b, &b, t);


    v3_add(&color, &a, &b);

  }

  return color;
}

void writeppm(struct v3 *data)
{
  FILE *ppm = fopen("crb.ppm","w+");
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
  spheres[0] = sphere_new(v3_new(0.0f, 5.0f, -1.0f), 4.0f,v3_new(1.0f,0.0f,0.0f));
  spheres[1] = sphere_new(v3_new(0.0f, -999.0f, 0.0f), 1000.f, v3_new(1.0f,1.0f,1.0f));

  struct v3 *data;
  data = malloc(HEIGHT * WIDTH * sizeof(struct v3));

  struct v3 vdu = {0};
  v3_sub(&vdu, &cam.rt, &cam.lt);
  v3_divs(&vdu, &vdu, (float) WIDTH);

  struct v3 vdv = {0};
  v3_sub(&vdv, &cam.lb, &cam.lt);
  v3_divs(&vdv, &vdv, (float) HEIGHT);

  for(uint_fast16_t y = 0; y < HEIGHT; ++y)
  {
    for(uint_fast16_t x = 0; x < WIDTH; ++x)
    {

      struct ray r;
      r.origin = cam.eye;
      
      struct v3 u;
      struct v3 v;
      struct v3 c = {0};

      for(uint_fast16_t s = 0; s < SAMPLES; ++s)
      {
        r.direction = cam.lt;
        
        v3_muls(&u, &vdu, (float)x + drand48());
        v3_muls(&v, &vdv, (float)y + drand48());

        v3_add(&r.direction, &r.direction, &u);
        v3_add(&r.direction, &r.direction, &v);

        v3_sub(&r.direction, &r.direction, &r.origin);

        v3_mkunit(&r.direction, &r.direction);
        u = trace(&r, 0);
        v3_add(&c, &c, &u);
      }

      v3_divs(&c, &c, (float)SAMPLES);

      data[y * WIDTH + x] = c;
    }
  }

  writeppm(data);

  free(data);

  return 0;
}
