Bootstrap: docker
From: ubuntu:18.04

%post
    apt-get -y update
    apt-get -y install wget emacs zsh less libncurses-dev lmod curl
    apt-get install -y gcc-7 g++-7 gfortran-7 gcc-6 g++-6 gfortran-6 git \
                       cmake \
                       libopenblas-dev liblapack-dev libhdf5-dev \
                       libgsl0-dev \
                       clang-5.0 clang-format-5.0 clang-tidy-5.0 \
                       libclang-5.0-dev wget lcov libboost-all-dev \
                       libgtest-dev
    apt-get update -y
    apt-get install -y libc++-dev libc++1 libc++abi-dev \
            libjemalloc1 libjemalloc-dev \
            ccache doxygen python-pip python-numpy

    # In order to get locales working properly inside a Singularity container
    # we need to do the following:
    apt-get update && apt-get install -y \
            locales language-pack-fi language-pack-en
            export LANGUAGE=en_US.UTF-8 && \
            export LANG=en_US.UTF-8 && \
            export LC_ALL=en_US.UTF-8 && \
            locale-gen en_US.UTF-8 && \
            dpkg-reconfigure locales

    # Set up IWYU:
    mkdir -p /work && cd /work
    wget https://github.com/include-what-you-use/include-what-you-use/archive/clang_5.0.tar.gz
    tar -xzf clang_5.0.tar.gz
    rm clang_5.0.tar.gz
    mkdir /work/include-what-you-use-clang_5.0/build
    cd /work/include-what-you-use-clang_5.0/
    sed -i 's^\\\"third_party/^<boost/^' iwyu_include_picker.cc
    cd /work/include-what-you-use-clang_5.0/build
    cmake -D CMAKE_CXX_COMPILER=clang++-5.0 \
          -D CMAKE_C_COMPILER=clang-5.0 \
          -D IWYU_LLVM_ROOT_PATH=/usr/lib/llvm-5.0 .. \
          && make -j2 \
          && make install \
          && cd / \
          && rm -rf /work/include-what-you-use-clang_5.0

    # Set up Google Benchmark
    git clone https://github.com/google/benchmark.git && \
        cd benchmark && mkdir build && cd build && \
        cmake -D CMAKE_BUILD_TYPE=Release \
        -DBENCHMARK_ENABLE_GTEST_TESTS=OFF .. && \
        make -j4 && make install && \
        cd / && rm -rf /work/benchmark

    # Install Catch2
    cd /work
    wget https://github.com/catchorg/Catch2/releases/download/v2.2.2/catch.hpp
    mv catch.hpp /usr/local/include

    # Install Blaze
    wget https://bitbucket.org/blaze-lib/blaze/downloads/blaze-3.3.tar.gz
    tar -xzf blaze-3.3.tar.gz
    mv blaze-3.3/blaze /usr/local/include
    rm -rf /work/blaze*

    # Install brigand
    cd /work && git clone https://github.com/edouarda/brigand.git && \
        mv /work/brigand/include/brigand /usr/local/include && \
        rm -rf /work/brigand

    # Install LIBXSMM
    mkdir -p /work && cd /work && \
    wget https://github.com/hfp/libxsmm/archive/1.8.3.tar.gz && \
        tar -xzf 1.8.3.tar.gz && ls && cd /work/libxsmm-1.8.3 && \
        make PREFIX=/usr/local install -j8 && \
        cd /work && rm -rf /work/libxsmm-1.8.3 /work/1.8.3.tar.gz

%environment
    export IN_CONTAINER=true
