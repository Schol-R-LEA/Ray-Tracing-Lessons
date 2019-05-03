#include <iostream>
#include <iomanip>
#include "vec3.h"
#include "ray.h"

vec3 color(const ray& r) 
{
  vec3 unit_direction = unit_vector(r.direction());
  float t = 0.5*(unit_direction.y() + 1.0);
  std::cerr << std::setw(6) << std::setprecision(4)
            << "<" << r.direction().x() << "," << r.direction().y() << "," << r.direction().z() 
            << ">, -> <"
            << unit_direction.x() << "," << unit_direction.y() << "," << unit_direction.z() << ">: "
            << t 
            << ", " 
            << (1.0-t) 
            << std::endl;
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
          std::cerr << std::setw(6) << std::setprecision(4)
                    << "<" << lower_left_corner.x() << "," << lower_left_corner.y() 
                    << "," << lower_left_corner.z()
                    << "> + (<"
                    << vertical.x() << "," << vertical.y() << "," << vertical.z()
                    << "> * " << v << ") + (<"
                    << horizontal.x() << "," << horizontal.y() << "," << horizontal.z()
                    << "> * " << u << ")  == ";
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
