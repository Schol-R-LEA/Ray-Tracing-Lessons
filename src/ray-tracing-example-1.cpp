#include <iostream>
#include "vec3.h"
#include "ray.h"


vec3 color(const ray& r) 
{
  vec3 unit_direction = unit_vector(r.direction());
  float t = 0.5*(unit_direction.y() + 1.0);
  return (1.0-t)*vec3(1.0, 1.0, 1.0) + t*vec3(0.5, 0.7, 1.0);
}


int main()
{
  int nx = 800;
  int ny = 400;

  vec3 lower_left_corner(-2.0, -1.0, -1.0);
  vec3 vertical(4.0, 0.0, 0.0);
  vec3 horizontal(0.0, -2.0, 0.0);
  vec3 origin(0.0, 0.0, 0.0);

  std::cout << "P3" << std::endl;
  std::cout << nx << " " << ny << std::endl;
  std::cout << "255" << std::endl;

  for (int j = ny - 1; j >= 0; j--)
    {
      for (int i = 0; i < nx; i++)
        {
          float u = float(i) / float(nx);
          float v = float(j) / float(ny);
          ray r(origin, lower_left_corner + v*vertical + u*horizontal);
          vec3 col = color(r);
          int ir = int(255.99 * col[0]);
          int ig = int(255.99 * col[1]);
          int ib = int(255.99 * col[2]);
         
          std::cout << ir << " " 
                    << ig << " "
                    << ib << std::endl;
        }
    }

  return 0;
}
