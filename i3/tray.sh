#!/bin/bash

pkill -f nm-applet
#pkill -f lxqt-powermanagement
pkill -f qlipper
pkill -f dropbox
pkill -f volctl

volctl &
dropbox &
nm-applet &
#lxqt-powermanagement &
qlipper &

