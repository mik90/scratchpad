#!/bin/bash

set -e

cd benchmark
mkdir -p build
cd build


cmake -DCMAKE_BUILD_TYPE=Release -DBENCHMARK_DOWNLOAD_DEPENDENCIES=ON \
      -DBENCHMARK_ENABLE_GTEST_TESTS=OFF ../

make -j6
