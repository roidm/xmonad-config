-- http://projects.haskell.org/xmobar/
-- install xmobar with these flags: --flags="with_alsa" --flags="with_mpd" --flags="with_xft"  OR --flags="all_extensions"
-- you can find weather location codes here: http://weather.noaa.gov/index.html


Config { font    = "xft:JetBrainsMono Nerd Font:weight=bold:pixelsize=17:antialias=true:hinting=true"
       , additionalFonts = [ "xft:UbuntuMono Nerd Font:pixelsize=20:antialias=true:hinting=true"
                           , "xft:UbuntuMono Nerd Font:size=22"
                           , "xft:UbuntuMono Nerd Font:size=17"
                           , "xft:Weather Icons:size=17"
                           , "xft:JetBrainsMono Nerd Font:weight=bold:pixelsize=18:antialias=true:hinting=true"
                           ]
       , bgColor = "#1E222A"
       , fgColor = "#ff6c6b"
       , position = Static { xpos = 20, ypos = 12, width = 3800, height = 33 }
       , lowerOnStart = True
       , hideOnStart = False
       , allDesktops = True
       , persistent = True
       , commands = [
                      Run Date "<fn=5> </fn> %a %b %d - %R " "date" 50
                    , Run Uptime ["-t", "<fn=5> </fn><hours>h <minutes>m"] 60
                    , Run Network "enp34s0" ["-t","<fn=5> </fn> <rx> kb  <fn=1> </fn> <tx> kb"] 20
                    , Run Cpu ["-t", "<fn=5>﬙ </fn> <total>%"] 20
                    , Run Com "~/.local/bin/memory2" ["--listen"] "" 40
                    , Run Com "~/.config/xmobar/scripts/dwmpulse" ["--listen"] "" 10
                    , Run Com "uname" ["-r"] "" 3600
                    , Run K10Temp "0000:00:18.3" ["--template", "<fn=5>﨎</fn> <Tdie>ºC"] 60
                    , Run UnsafeStdinReader
                    , Run Com "/home/roidm/.config/xmobar/scripts/padding" ["panel"] "trayerpad" 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template ="<fn=3><fc=#bc7ad9>  </fc></fn> <fn=5>%UnsafeStdinReader%</fn> }{<fn=2><fc=#282c34,#1e222a:0></fc></fn><fc=#e06c75,#282c34:0>%enp34s0% </fc><fn=2><fc=#1e222a,#282c34:0></fc></fn><fc=#c9866f>%uptime% </fc><fn=2><fc=#282c34,#1e222a:0></fc></fn><fc=#39D7E5,#282c34:0>  %uname% </fc><fn=2><fc=#1e222a,#282c34:0></fc></fn><fc=#6bb2c0,#1e222a:0>%k10temp% </fc><fn=2><fc=#282c34,#1e222a:0></fc></fn><fc=#ebcb8d,#282c34:0>%cpu% </fc><fn=2><fc=#1e222a,#282c34:0></fc></fn><fc=#bc7ad9,#1e222a:0><fn=5> </fn> %memory2%</fc><fn=2><fc=#282c34,#1e222a:0></fc></fn><fc=#71abeb,#282c34:0>%dwmpulse% </fc><fn=2><fc=#1e222a,#282c34:0></fc></fn><fc=#9ec07c,#1e222a:0>%date%</fc>%trayerpad%"
       }
