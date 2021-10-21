-- http://projects.haskell.org/xmobar/
-- install xmobar with these flags: --flags="with_alsa" --flags="with_mpd" --flags="with_xft"  OR --flags="all_extensions"
-- you can find weather location codes here: http://weather.noaa.gov/index.html


Config { font              = "xft:JetBrainsMono Nerd Font:weight=bold:pixelsize=17:antialias=true:hinting=true"
       , additionalFonts = [ "xft:UbuntuMono Nerd Font:pixelsize=20:antialias=true:hinting=true"
                           , "xft:JetBrainsMono Nerd Font:size=21"
                           , "xft:UbuntuMono Nerd Font:size=17"
                           , "xft:Weather Icons:size=17"
                           , "xft:JetBrainsMono Nerd Font:weight=bold:pixelsize=18:antialias=true:hinting=true"
                           ]
       , bgColor = "#1a1b26"
       , fgColor = "#ff6c6b"
     --  , position = Static { xpos = 20, ypos = 12, width = 3800, height = 32 }
       , position = Static { xpos = 0, ypos = 0, width = 3840, height = 30 }
       , overrideRedirect = False
       , lowerOnStart = False
       , hideOnStart = False
       , allDesktops = True
       , persistent = True
       , commands = [
                      Run Date "  %a %b %d - %R " "date" 50
                    , Run Uptime ["-t", " <hours>h <minutes>m"] 60
                    , Run Network "enp34s0" ["-t"," <rx> kb   <tx> kb"] 20
                    , Run Cpu ["-t", "﬙  <total>%"] 20
                    , Run Com "memory2" ["--listen"] "" 40
                    , Run Com "dwmpulse" ["--listen"] "" 10
                     --, Run Com "uname" ["-r"] "" 3600  
                    , Run Com "weather.sh" ["--listen"] "" 1800
                    , Run K10Temp "0000:00:18.3" ["--template", "﨎 <Tdie>ºC"] 60
                    , Run Com "padding" ["panel"] "trayerpad" 10
                    , Run UnsafeXMonadLog
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template ="<fn=5>%UnsafeXMonadLog%</fn>}{\
                   \<fn=2><fc=#2b2f40,#1a1b26:0></fc></fn><fc=#F7768E,#2b2f40:0>%enp34s0% </fc>\
                   \<fn=2><fc=#1a1b26,#2b2f40:0></fc></fn><fc=#7AA2F7,#1a1b26:0>%uptime% </fc>\
                   \<fn=2><fc=#2b2f40,#1a1b26:0></fc></fn><fc=#39D7E5,#2b2f40:0>%weather.sh% </fc>\
                   \<fn=2><fc=#1a1b26,#2b2f40:0></fc></fn><fc=#4ABAAF,#1a1b26:0>%k10temp% </fc>\
                   \<fn=2><fc=#2b2f40,#1a1b26:0></fc></fn><fc=#E0AF68,#2b2f40:0>%cpu% </fc>\
                   \<fn=2><fc=#1a1b26,#2b2f40:0></fc></fn><fc=#C574DD,#1a1b26:0>  %memory2% </fc>\
                   \<fn=2><fc=#2b2f40,#1a1b26:0></fc></fn><fc=#a9b1d6,#2b2f40:0>%dwmpulse% </fc>\
                   \<fn=2><fc=#1a1b26,#2b2f40:0></fc></fn><fc=#9ECE6A,#1a1b26:0>%date%</fc>%trayerpad%"
       }
