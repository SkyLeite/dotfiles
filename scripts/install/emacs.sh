#!/usr/bin/env bash

# These are installed from http://emacs.secretsauce.net/
echo "deb     [arch=amd64] http://emacs.secretsauce.net unstable main" | sudo tee -a /etc/apt/sources.list
echo "deb-src [arch=amd64] http://emacs.secretsauce.net unstable main" | sudo tee -a /etc/apt/sources.list
wget -q -O - http://emacs.secretsauce.net/key.gpg | sudo apt-key add -
sudo apt install emacs-snapshot

git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom quickstart
