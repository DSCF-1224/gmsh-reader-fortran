#!/bin/bash

GEO_FILE_BASENAME=t1

GEO_FILE_PATH=./${GEO_FILE_BASENAME}.geo

gmsh ${GEO_FILE_PATH} -2 -format msh2 -o ./${GEO_FILE_BASENAME}-txt.msh2
gmsh ${GEO_FILE_PATH} -2 -format msh4 -o ./${GEO_FILE_BASENAME}-txt.msh4
gmsh ${GEO_FILE_PATH} -2 -format msh2 -o ./${GEO_FILE_BASENAME}-bin.msh2 -bin
gmsh ${GEO_FILE_PATH} -2 -format msh4 -o ./${GEO_FILE_BASENAME}-bin.msh4 -bin

# EOF
