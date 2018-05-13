Bootstrap: docker
From: base/devel

%post
    # Use US english UTF-8 locale
    sed -i 's/#en_US.UTF-8/en_US.UTF-8/g' /etc/locale.gen
    locale-gen en_US.UTF-8
    pacman -Sy --noconfirm zsh

    # Set up compilers. python is needed for git-clang-format
    pacman -Sy --noconfirm gcc-fortran
    pacman -Sy --noconfirm python openmp clang llvm llvm-libs

    # Downgrade to older compilers. You can browse what's available at
    # https://archive.archlinux.org/packages/
    # pacman -U --noconfirm https://archive.archlinux.org/packages/g/gcc-libs/gcc-libs-7.3.1+20180406-1-x86_64.pkg.tar.xz https://archive.archlinux.org/packages/g/gcc/gcc-7.3.1+20180406-1-x86_64.pkg.tar.xz
    # pacman -U --noconfirm https://archive.archlinux.org/packages/c/clang/clang-5.0.1-2-x86_64.pkg.tar.xz https://archive.archlinux.org/packages/l/llvm/llvm-5.0.1-2-x86_64.pkg.tar.xz https://archive.archlinux.org/packages/l/llvm-libs/llvm-libs-5.0.1-2-x86_64.pkg.tar.xz

    # Set up yaourt. Unfortunately yaourt can't be run as root and I haven't
    # figured out how to properly get that working.
    echo "[archlinuxfr]" >> /etc/pacman.conf
    echo "SigLevel = Never" >> /etc/pacman.conf
    echo "Server = http://repo.archlinux.fr/\$arch" >> /etc/pacman.conf
    pacman -Sy --noconfirm yaourt

    # Set up CMake and other development software
    pacman -Sy --noconfirm cmake git python python-pip wget \
            curl gtest gmock doxygen openblas ccache
    pip install numpy h5py
    pacman -Sy --noconfirm yaml-cpp catch2 jemalloc boost hdf5

    # Install include-what-you-use
    # Need to link clang libraries to /usr/local
    ln -s /usr/lib/clang /usr/local/lib/
    wget https://github.com/include-what-you-use/include-what-you-use/archive/clang_6.0.tar.gz
    tar -xzf clang_6.0.tar.gz
    rm clang_6.0.tar.gz
    mkdir ./include-what-you-use-clang_6.0/build
    cd ./include-what-you-use-clang_6.0/
    sed -i 's^\\\"third_party/^<boost/^' iwyu_include_picker.cc
    sed -i 's^LLVMDemangle^LLVM^' CMakeLists.txt
    sed -i 's^LLVM[XCiSITAOMPBS].*^^g' CMakeLists.txt
    cd ./build
    cmake -D CMAKE_CXX_COMPILER=clang++ \
          -D CMAKE_C_COMPILER=clang \
          -D IWYU_LLVM_ROOT_PATH=/usr/lib/ .. \
          && make -j2 \
          && make install
    cd ../.. && rm -rf ./include-what-you-use-clang_6.0

    # Set up Google Benchmark
    wget https://github.com/google/benchmark/archive/v1.4.0.tar.gz
    tar -xzf v1.4.0.tar.gz
    cd benchmark-1.4.0 && mkdir build && cd build && \
        cmake -D CMAKE_BUILD_TYPE=Release .. && \
        make -j4 && make install
    cd ../.. && rm -rf ./benchmark-1.4.0* v1.4.0.tar.gz*

    # Install Blaze
    wget https://bitbucket.org/blaze-lib/blaze/downloads/blaze-3.3.tar.gz
    tar -xzf blaze-3.3.tar.gz
    mv blaze-3.3/blaze /usr/local/include
    rm -rf /work/blaze*

    # Install brigand
    git clone https://github.com/edouarda/brigand.git && \
        mv ./brigand/include/brigand /usr/local/include && \
        rm -rf ./brigand

    # Install LIBXSMM
    wget https://github.com/hfp/libxsmm/archive/1.8.3.tar.gz && \
        tar -xzf 1.8.3.tar.gz && ls && cd ./libxsmm-1.8.3 && \
        make PREFIX=/usr/local install -j8 && \
        cd .. && rm -rf ./libxsmm-1.8.3 ./1.8.3.tar.gz

    # Set up Ninja with Fortran support
    wget https://github.com/Kitware/ninja/releases/download/v1.8.2.g3bbbe.kitware.dyndep-1.jobserver-1/ninja-1.8.2.g3bbbe.kitware.dyndep-1.jobserver-1_x86_64-linux-gnu.tar.gz
    tar -xzf ninja-1.8.2.g3bbbe.kitware.dyndep-1.jobserver-1_x86_64-linux-gnu.tar.gz
    mv ninja-1.8.2.g3bbbe.kitware.dyndep-1.jobserver-1_x86_64-linux-gnu/ninja /usr/local/bin/
    rm -r ./ninja-*

    # Set up profiling and benchmarking code
    pacman -Sy --noconfirm perf

    # Remove the packages downloaded to image's Pacman cache dir.
    paccache -r -k0


%environment
    export IN_CONTAINER=true
