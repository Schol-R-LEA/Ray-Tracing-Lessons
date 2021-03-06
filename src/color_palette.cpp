#include <iostream>

int main()
{
   int nx = 800;
   int ny = 400;
   
   std::cout << "P3" << std::endl;
   std::cout << nx << " " << ny << std::endl;
   std::cout << "255" << std::endl;

   for (int j = ny - 1; j >= 0; j--)
   {
      for (int i = 0; i < nx; i++)
      {
         float r = float(i) / float(nx);
         float g = float(j) / float(ny);
         float b = 0.2;

         int ir = int(255.99 * r);
         int ig = int(255.99 * g);
         int ib = int(255.99 * b);
         
         std::cout << ir << " " 
                   << ig << " "
                   << ib << std::endl;
      }
   }

   return 0;
}
