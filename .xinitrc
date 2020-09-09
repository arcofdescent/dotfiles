#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# Automount
udisksctl mount -b /dev/sdb1

# CAPS LOCK as Ctrl
setxkbmap -option "ctrl:nocaps"
xcape

# Set the background color
xsetroot -solid midnightblue

# screensaver
xscreensaver -no-splash &

# Notifications
lxqt-notificationd &

# Set up an icon tray
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 10 --transparent true --tint 0x191970 --height 20 &
 
# System tray
sleep 2
nm-applet &
lxqt-powermanagement &
xfce4-clipman &

# Dropbox
#~/.dropbox-dist/dropboxd &

# Arrange the monitors
# HDMI-1 to left of laptop
sh /home/rohan/.screenlayout/hdmi-edp.sh

exec xmonad
