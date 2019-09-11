#!/bin/bash

MUSIC_DIR=/mnt/HDD/Music

find $MUSIC_DIR | entr -r -d rclone sync -v  $MUSIC_DIR drive:Music
