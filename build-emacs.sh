#!/bin/bash

# Build latest version of Emacs, version management with stow
# OS: Ubuntu 14.04 LTS and newer
# version: 24.5
# Toolkit: lucid
# Assumes that all dependencies are available.
# Dependencies can be installed with:
# sudo apt-get -qq install -y stow build-essential libx11-dev xaw3dg-dev \
#      libjpeg-dev libpng-dev libgif-dev libtiff5-dev libncurses5-dev \
#      libxft-dev librsvg2-dev libmagickcore-dev libmagick++-dev \
#      libxml2-dev libgpm-dev libotf-dev libm17n-dev \
#      libgnutls-dev wget

version="$1"
cpu_count=`grep -c ^processor /proc/cpuinfo`
build_threads=$(( $cpu_count * 2 ))

echo "CPU count is probably $cpu_count, setting build_threads to $build_threads"

if [ -z "$version" ]; then
    echo "Version not specified, defaulting to 24.5"
    version="24.5"
fi

# download source package
if [[ ! -d emacs-"$version" ]]; then
    wget http://ftp.gnu.org/gnu/emacs/emacs-"$version".tar.xz
    tar xf emacs-"$version".tar.xz
fi

cd emacs-"$version"

# build and install
if [ "$version" > "25" ] ; then
    ./configure \
        --with-xft \
        --with-x-toolkit=lucid \
        --with-modules
else
    ./configure \
        --with-xft \
        --with-x-toolkit=lucid
fi

make -j"$build_threads"

sudo mkdir -p /usr/local/stow
sudo make \
     install-arch-dep \
     install-arch-indep \
     prefix=/usr/local/stow/emacs-"$version"

cd /usr/local/stow
sudo stow emacs-"$version"
