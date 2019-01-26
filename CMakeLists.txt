cmake_minimum_required(VERSION 3.0.0 FATAL_ERROR)

project(lasm LANGUAGES Fortran)

find_package(MPI)
if (NOT MPI_Fortran_FOUND)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DNO_MPI")
else ()
  include_directories(${MPI_Fortran_INCLUDE_PATH})
endif ()

set(source_files
  src/constants_mod.F90
  src/geometry_mod.F90
  src/parcel_mod.F90
  src/domain_mod.F90
  src/cartesian_domain_mod.F90
  src/spherical_domain_mod.F90
  src/parallel_mod.F90
)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/.mods")

add_library(lasm ${source_files})

add_executable(test_driver.exe src/test_driver.F90)
target_link_libraries(test_driver.exe lasm ${MPI_Fortran_LIBRARIES})
