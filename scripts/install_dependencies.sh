#!/usr/bin/env bash

SCRIPTS_DIR=~/.dotfiles/scripts

writeToConsole () {
    echo -e "\e[31m->\e[0m $1"
}

writeToConsole "Installing apt dependencies..."

dpkg -s  "apt" &> /dev/null
if [ "$?" != "0" ];
then
    echo "Unfortunately you don't have apt installed. This script only supports debian based distributions :("
    exit 1
fi

read -d . VERSION < /etc/debian_version

writeToConsole "Apt found. Beginning install. If you get prompted for sudo password, it's for apt."
sudo apt install \
    rofi \
    dunst \
    tint2 \
    pulseaudio-utils \
    network-manager-gnome \
    compton \
    flameshot \
    build-essential \
    python3 \
    python3-pip \
    mpv \
    git \
    ffmpeg \
    x11-xserver-utils \
    feh \
    xinput

writeToConsole "Apt dependencies installed. Installing pip dependencies."

if ! [ -x "$(command -v streamlink)" ]
then
    writeToConsole "Streamlink is not installed. Installing..."
    sudo pip3 install streamlink
else
    writeToConsole "Streamlink is installed. Skipping"
fi

if ! [ -x "$(command -v youtube-dl)" ]
then
    writeToConsole "Youtube-dl is not installed. Installing..."
    sudo pip3 install youtube-dl
else
    writeToConsole "Youtube-dl is installed. Skipping"
fi

writeToConsole "Pip dependencies installed. Compiling and installing native dependencies."

if ! [ -x "$(command -v i3)" ]
then
    writeToConsole "i3-gaps is not installed. Installing..."
    . $SCRIPTS_DIR/install/i3-gaps.sh
else
    writeToConsole "i3-gaps is installed. Skipping"
fi

if ! [ -x "$(command -v emacs)" ]
then
    writeToConsole "Emacs is not installed. Installing..."
    . $SCRIPTS_DIR/install/emacs.sh
else
    writeToConsole "Emacs is installed. Skipping"
fi
