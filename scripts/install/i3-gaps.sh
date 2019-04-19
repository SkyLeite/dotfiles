#!/usr/bin/env bash

i3Dependencies () {
    sudo apt $1
        gcc \
        make \
        dh-autoreconf \
        libxcb-keysyms1-dev \
        libpango1.0-dev \
        libxcb-util0-dev \
        xcb \
        libxcb1-dev \
        libxcb-icccm4-dev \
        libyajl-dev \
        libev-dev \
        libxcb-xkb-dev \
        libxcb-cursor-dev \
        libxkbcommon-dev \
        libxcb-xinerama0-dev \
        libxkbcommon-x11-dev \
        libstartup-notification0-dev \
        libxcb-randr0-dev \
        libxcb-xrm0 \
        libxcb-xrm-dev \
        libxcb-shape0-dev
}

i3Dependencies install

cd /tmp

# clone the repository
git clone https://www.github.com/Airblader/i3 i3-gaps
cd i3-gaps

# compile & install
autoreconf --force --install
rm -rf build/
mkdir -p build && cd build/

# Disabling sanitizers is important for release versions!
# The prefix and sysconfdir are, obviously, dependent on the distribution.
../configure --prefix=/usr --sysconfdir=/etc --disable-sanitizers
make

sudo make install

i3Dependencies remove
