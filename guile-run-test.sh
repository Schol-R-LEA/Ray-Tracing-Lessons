guile -l src/average.sls tests/average-tests.scm 

echo

guile -l src/ray-tracing/vector3D.sls tests/vector3D-tests.scm 

echo

guile -l src/average.sls \
      -l src/colors/rgb-colors.sls \
      tests/colors-tests.scm 
