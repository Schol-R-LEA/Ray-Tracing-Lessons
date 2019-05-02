guile -l src/average.sls tests/average-tests.scm 

echo


guile -l src/average.sls \
      -l src/colors/rgb-colors.sls \
      tests/colors-tests.scm 

echo


guile -l src/average.sls \
      -l src/colors/rgb-colors.sls \
      -l src/image-file-types/ppm.sls \
      tests/ppm-tests.scm 

echo

guile -l src/ray-tracing/point3D.sls tests/point3D-tests.scm

echo

guile -l src/ray-tracing/point3D.sls \
      -l src/ray-tracing/ray3D.sls \
       tests/ray3D-tests.scm 
