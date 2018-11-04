vicare --build-directory bin -l src/average/main.sls  tests/average-tests.scm

echo 

vicare --build-directory bin -l src/ray-tracing/vector3D.sls tests/vector3D-tests.scm

echo

vicare --build-directory bin -l src/average/main.sls -l src/ray-tracing/vector3D.sls -l src/ray-tracing/colors.sls  tests/colors-tests.scm
