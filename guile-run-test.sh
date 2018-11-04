guile -l src/average/main.sls tests/average-tests.scm 

echo

guile -l src/average/main.sls \
      -l src/ray-tracing/vector3D.sls \
      tests/vector3D-tests.scm 

echo

guile -l src/average/main.ss \
      -l src/ray-tracing/vector3D.sls \
      -l src/ray-tracing/colors.sls \
      tests/colors-tests.scm 
