#! /bin/bash 
picom -b &
nitrogen --restore &
#~/.fehbg &
killall "xiccd"
xiccd &
killall "dunst"
dunst &
killall "pipewire"
killall "pipewire-media-session"
pipewire &
polkit-gnome-authentication-agent-1 &
xfce4-power-manager &
xsetroot -cursor_name left_ptr &
killall "trayer"
trayer-srg --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype request  --transparent true --alpha 0 --tint 0x1e222a  --height 28 --iconspacing 2 &
sleep 4 && pamixer --set-volume 70
