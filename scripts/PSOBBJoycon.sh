#!/usr/bin/env bash

xboxdrv \
    --evdev /dev/input/event22 \
    --dpad-as-button \
    --trigger-as-button \
    --evdev-keymap BTN_DPAD_LEFT=dl,BTN_DPAD_RIGHT=dr,BTN_DPAD_UP=du,BTN_DPAD_DOWN=dd,BTN_WEST=B,BTN_EAST=X,BTN_SOUTH=A,BTN_NORTH=Y,BTN_TL=LB,BTN_TR=RB,BTN_TL2=LT,BTN_TR2=RT,BTN_THUMBL=TL,BTN_THUMBR=TR \
    --evdev-absmap ABS_RZ=y2,ABS_RY=x2,ABS_RX=y1,ABS_Z=x1 \
    --axismap -X1=X1,-X2=X2 \
    --mimic-xpad
