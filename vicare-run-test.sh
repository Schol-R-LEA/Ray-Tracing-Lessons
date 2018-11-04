vicare --build-directory bin -l src/average/main.ss  tests/average-tests.scm

echo 

vicare --build-directory bin -l src/ray-tracing/vector3D.ss tests/vector3D-tests.scm

echo

vicare --build-directory bin -l src/average/main.ss -l src/ray-tracing/vector3D.ss -l src/ray-tracing/colors.ss  tests/colors-tests.scm
