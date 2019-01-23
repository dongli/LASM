cmake_minimum_required(VERSION 3.0.0 FATAL_ERROR)

project(lasm LANGUAGES Fortran)

set(source_files
  src/constants_mod.F90
  src/geometry_mod.F90
  src/parcel_mod.F90
)

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/.mods")

add_library(lasm ${source_files})
