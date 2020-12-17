#!/bin/bash

set -e

GFE_DIR=${HOME}/gfe
mkdir -p ${GFE_DIR}

# First install prerequisites
GFE_INSTALL_DIR=${HOME}/Software/GFE
mkdir -p ${GFE_INSTALL_DIR}

to_build=(gFTL gFTL-shared fArgParse yaFyaml pFUnit)
for repo in "${to_build[@]}"
do
   echo "Running build for ${repo}"
   cd ${GFE_DIR}
   git clone https://github.com/Goddard-Fortran-Ecosystem/${repo}.git
   cd ${GFE_DIR}/${repo}
   if [[ "${repo}" != "gFTL" ]]
   then
      git checkout bugfix/mathomp4/build_submodule_fix
   fi
   mkdir build && cd build
   cmake .. -DCMAKE_INSTALL_PREFIX=${GFE_INSTALL_DIR} -DCMAKE_PREFIX_PATH=${GFE_INSTALL_DIR}
   make -j$(nproc) install
   echo "Completed build for ${repo}"
done

