import qualified Data.Map                            as M
import           System.Directory
import           System.Exit
import           System.IO
import           XMonad
import           XMonad.Actions.CycleRecentWS        (toggleRecentWS)
import           XMonad.Actions.CycleWS
import           XMonad.Actions.GroupNavigation      (Direction (History),
                                                      historyHook, nextMatch)
import           XMonad.Actions.Minimize
import           XMonad.Actions.MouseResize
import           XMonad.Actions.Promote
import           XMonad.Actions.RotSlaves
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.UpdatePointer        (updatePointer)
import           XMonad.Actions.WithAll
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Minimize
import           XMonad.Hooks.RefocusLast            (refocusLastLogHook)
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.StatusBar              (StatusBarConfig,
                                                      statusBarProp, withSB)
import           XMonad.Hooks.StatusBar.PP           hiding (trim)
import qualified XMonad.Layout.Dwindle               as Dwindle
import           XMonad.Layout.Gaps
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.LimitWindows
import           XMonad.Layout.Maximize
import           XMonad.Layout.Minimize
import           XMonad.Layout.MouseResizableTile
import           XMonad.Layout.MultiToggle
import qualified XMonad.Layout.MultiToggle           as MT
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Spacing
import           XMonad.Layout.ThreeColumns
import qualified XMonad.Layout.ToggleLayouts         as T
import           XMonad.Layout.WindowArranger
import           XMonad.Layout.WindowNavigation
import           XMonad.Prelude                      (Endo, fromJust, isDigit,
                                                      isJust, isSpace, toUpper)
import qualified XMonad.StackSet                     as W
import           XMonad.Util.ClickableWorkspaces     (clickablePP)
import           XMonad.Util.EZConfig
import           XMonad.Util.Hacks
import           XMonad.Util.Loggers                 (logTitles)
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run

($.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
($.) = (.) . (.)

if2 :: (a -> b -> Bool) -> (a -> b -> c) -> (a -> b -> c) -> a -> b -> c
if2 p f g x y = if p x y then f x y else g x y

myModMask :: KeyMask
myModMask = mod4Mask        -- modkey to super/windows key

altMask :: KeyMask
altMask = mod1Mask

myTerminal :: String
myTerminal = "st"    -- default terminal

myEmacs :: String
myEmacs = "emacsclient -c -a 'emacs' "  -- Makes emacs keybindings easier to type

myBrowser :: String
myBrowser = "firefox"

centreRect :: W.RationalRect
centreRect =  W.RationalRect l t w h
  where
    h = 0.8
    w = 0.5
    t = 0.9 -h
    l = 0.75 -w

centerFloat :: X ()
centerFloat = withFocused $ \win -> do
    (_, W.RationalRect _ _ w h) <- floatLocation win
    windows $ W.float win $ W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h

isFloating :: Window -> WindowSet -> Bool
isFloating w s = M.member w (W.floating s)

enableFloat :: W.RationalRect -> Window -> (WindowSet -> WindowSet)
enableFloat = flip W.float

disableFloat :: Window -> (WindowSet -> WindowSet)
disableFloat = W.sink

toggleFloat :: W.RationalRect -> Window -> X ()
toggleFloat r = windows . if2 isFloating disableFloat (enableFloat r)

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
    spawn "$HOME/.xmonad/scripts/autostart.sh"
    setWMName "LG3D"

---------------------------------------------
----Scratchpads
---------------------------------------------

nearFullFloat = customFloating $ W.RationalRect l t w h
  where
    h = 0.5
    w = 0.5
    t = 0.75 -h
    l = 0.75 -w

nearFullFloat2 = customFloating $ W.RationalRect l t w h
  where
    h = 0.8
    w = 0.5
    t = 0.9 -h
    l = 0.75 -w

myScratchPads :: [NamedScratchpad]
myScratchPads =  [ NS "terminal" "st -c terminal"                    (className =? "terminal")        terminalHook
                 , NS "ranger" "alacritty -t ranger -e ranger"       (title     =? "ranger")          rangerHook
                 , NS "vlc" "vlc"                                    (className =? "vlc")             vlcHook
                 , NS "Telegram" "Telegram"                          (className =? "TelegramDesktop") telegramHook
                 , NS "obs" "obs"                                    (className =? "obs")             obsHook
                 , NS "Discord" "Discord"                            (className =? "discord")         discordHook
                 , NS "term" "alacritty --class term,term"           (className =? "term")            termHook
                 , NS "htop" "alacritty -t htop -e  htop"            (title     =? "htop")            htopHook
                 , NS "skypeforlinux" "skypeforlinux"                (className =? "Skype")           skypeforlinuxHook
                 , NS "mpv" "mpv --player-operation-mode=pseudo-gui" (className =? "mpv")             mpvHook
                ]

                  where terminalHook      = nearFullFloat
                        rangerHook        = nearFullFloat
                        vlcHook           = nearFullFloat
                        telegramHook      = nearFullFloat2
                        obsHook           = nearFullFloat2
                        htopHook          = nearFullFloat
                        termHook          = nearFullFloat
                        skypeforlinuxHook = nearFullFloat2
                        discordHook       = nearFullFloat2
                        mpvHook           = nearFullFloat

---------------------
-- The layout hook --
---------------------

gap :: Int
gap = 18

fi = fromIntegral

mrt = mouseResizableTile { draggerType = FixedDragger (fi gap) (fi gap) }
applyGaps = gaps $ zip [U, D, R, L] $ repeat gap

-- Gaps bewteen windows
myGaps gap  = gaps [(U, gap),(D, gap),(L, gap),(R, gap)]
gapSpaced g = spacing g . myGaps g

tall        = renamed [Replace "tall"]
            $ minimize
            $ maximizeWithPadding 0
            $ limitWindows 12
            $ gapSpaced 9
            $ ResizableTall 1 (1/100) (1/2) []
dwindle     = renamed [Replace "dwindle"]
            $ minimize
            $ maximizeWithPadding 0
            $ gapSpaced 9
            $ limitWindows 12
            $ Dwindle.Dwindle R Dwindle.CW (2/2) (11/10)
monocle     = renamed [Replace "monocle"]
            $ minimize
            $ maximizeWithPadding 0
            $ limitWindows 20 Full
threeColMid = renamed [Replace "|C|"]
            $ minimize
            $ maximizeWithPadding 0
            $ gapSpaced 9
            $ limitWindows 7
            $ ThreeColMid 1 (1/100) (1/2)
floats      = renamed [Replace "floats"]
            $ minimize
            $ maximizeWithPadding 0
            $ limitWindows 20 simplestFloat

myLayoutHook = smartBorders $ avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout = tall
                                 ||| dwindle
                                 ||| avoidStruts (applyGaps mrt)
                                 ||| threeColMid
                                 ||| noBorders monocle
                                 ||| floats

------------------
--- Workspaces ---
------------------

myWorkspaces :: [WorkspaceId]
myWorkspaces =  ["1", "2", "3", "4", "5", "6", "7", "8", "9"]


------------------
------ Keys ------
------------------

myKeys :: [(String, X ())]
myKeys =
    -- Xmonad
        [ ("M-C-r", spawn "xmonad --recompile")  -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")    -- Restarts xmonad
        , ("M-S-q", io exitSuccess)              -- Quits xmonad

    --  Terminal, Browser, Htop
        , ("M-<Return>", spawn (myTerminal ++ " -e zsh"))
        , ("M1-<Return>", spawn ("alacritty"))
        , ("M-b", spawn (myBrowser))

    -- Kill windows
        , ("M-q", kill)     -- Kill the currently focused client
        , ("M-S-a", killAll)   -- Kill all windows on current workspace

    -- Workspaces
        , ("M-.", nextScreen)  -- Switch focus to next monitor
        , ("M-,", prevScreen)  -- Switch focus to prev monitor

    -- Floating windows
        , ("M-f", sendMessage (T.Toggle "floats")) -- Toggles my 'floats' layout
        , ("M-t", withFocused $ windows . W.sink)  -- Push floating window back to tile
        , ("M-S-t", sinkAll)                       -- Push ALL floating windows to tile

    -- Increase/decrease spacing (gaps)
        , ("M-d", decWindowSpacing 4)           -- Decrease window spacing
        , ("M-i", incWindowSpacing 4)           -- Increase window spacing
        , ("M-S-d", decScreenSpacing 4)         -- Decrease screen spacing
        , ("M-S-i", incScreenSpacing 4)         -- Increase screen spacing

    -- Windows navigation
        , ("M-m", windows W.focusMaster)  -- Move focus to the master window
        , ("M-j", windows W.focusDown)    -- Move focus to the next window
        , ("M-k", windows W.focusUp)      -- Move focus to the prev window
        , ("M-S-m", windows W.swapMaster) -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)   -- Swap focused window with next window
        , ("M-S-k", windows W.swapUp)     -- Swap focused window with prev window
        , ("M-<Backspace>", promote)      -- Moves focused window to master, others maintain order
        , ("M-S-<Tab>", rotSlavesDown)    -- Rotate all windows except master and keep focus in place
        , ("M-C-<Tab>", rotAllDown)       -- Rotate all the windows in the current stack
        , ("M-z", withFocused minimizeWindow)
        , ("M-S-z",withLastMinimized maximizeWindowAndFocus)
        , ("M-r", toggleRecentWS)
        , ("M1-r", nextMatch History (return True))

    -- Layouts
        , ("M-<Tab>", sendMessage NextLayout)           -- Switch to next layout
        , ("M-C-M1-<Up>", sendMessage Arrange)
        , ("M-C-M1-<Down>", sendMessage DeArrange)
        , ("M-S-<Space>", sendMessage ToggleStruts)     -- Toggles struts
        , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)  -- Toggles noborder
        , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
        , ("M-a", withFocused $ toggleFloat centreRect)
        , ("M-c",           centerFloat)

    -- Increase/decrease windows in the master pane or the stack
        , ("M-S-<Up>", sendMessage (IncMasterN 1))      -- Increase # of clients master pane
        , ("M-S-<Down>", sendMessage (IncMasterN (-1))) -- Decrease # of clients master pane
        , ("M-C-<Up>", increaseLimit)                   -- Increase # of windows
        , ("M-C-<Down>", decreaseLimit)                 -- Decrease # of windows

    -- Window resizing
        , ("M-h", sendMessage Shrink)                   -- Shrink horiz window width
        , ("M-l", sendMessage Expand)                   -- Expand horiz window width
        , ("M-M1-j", sendMessage MirrorShrink)          -- Shrink vert window width
        , ("M-M1-k", sendMessage MirrorExpand)          -- Exoand vert window width

    ---- Rofi and Dmenu Scripts
        , ("C-<Space>", spawn "rofi -show drun")
        , ("M1-<Space>", spawn "dmenu_run -sb '#4d78cc' -p 'Run: '")

    -- Scratchpads
        , ("M1-t",   namedScratchpadAction myScratchPads "terminal")
        , ("M-S-<Return>", namedScratchpadAction myScratchPads "dropdown-term")
        , ("M1-S-t", namedScratchpadAction myScratchPads "term")
        , ("M1-S-r",   namedScratchpadAction myScratchPads "ranger")
        , ("M1-S-v", namedScratchpadAction myScratchPads "vlc")
        , ("M1-C-t", namedScratchpadAction myScratchPads "Telegram")
        , ("M1-o",   namedScratchpadAction myScratchPads "obs")
        , ("M1-s",   namedScratchpadAction myScratchPads "skypeforlinux")
        , ("M1-h",   namedScratchpadAction myScratchPads "htop")
        , ("M1-d",   namedScratchpadAction myScratchPads "Discord")
        , ("M1-m",   namedScratchpadAction myScratchPads "mpv")

     -- Emacs (ALT-e followed by a key)
        , ("M1-e e", spawn myEmacs)                 -- start emacs
        , ("M1-e b", spawn (myEmacs ++ ("--eval '(ibuffer)'")))   -- list buffers
        , ("M1-e s", spawn (myEmacs ++ ("--eval '(eshell)'")))    -- eshell
        , ("M1-e d", spawn (myEmacs ++ ("--eval '(dired nil)'"))) -- dired
        , ("M1-e v", spawn (myEmacs ++ ("--eval '(+vterm/here nil)'"))) -- vterm if on Doom Emacs

    --- My Applications
        , ("M1-S-d", spawn "gnome-disks")
        , ("M1-f", spawn "thunar")
        , ("M1-g", spawn "gthumb")
        , ("M1-S-m", spawn "gnome-system-monitor")
        , ("M1-v", spawn (myTerminal ++ (" -e lvim ")))

    -- Multimedia Keys
        , ("<XF86AudioPlay>", spawn "playerctl play-pause")
        , ("<XF86AudioPrev>", spawn "playerctl previous")
        , ("<XF86AudioNext>", spawn "playerctl next")
        , ("<XF86AudioMute>",   spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
        , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
        , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
        , ("<XF86HomePage>", spawn "firefox")
        , ("<Print>", spawn "screenshot")
        ]

myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                      >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                      >> windows W.shiftMaster)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

--------------------------
--- Window / App Rules ---
--------------------------

type AppName      = String
type AppTitle     = String
type AppClassName = String
type AppCommand   = String

data App
  = ClassApp AppClassName AppCommand
  | TitleApp AppTitle AppCommand
  | NameApp AppName AppCommand
  deriving Show

audacious = ClassApp "Audacious"            "audacious"
calendar  = ClassApp "Gnome-calendar"       "gnome-calendar"
eog       = NameApp  "eog"                  "eog"
evince    = ClassApp "Evince"               "evince"
gimp      = ClassApp "Gimp.bin"             "gimp.bin"
gimp2     = ClassApp "Gimp-2.99"            "gimp-2.99"
pavuctrl  = ClassApp "Pavucontrol"          "pavucontrol"
spotify   = ClassApp "Spotify"              "myspotify"
vlc       = ClassApp "Vlc"                  "vlc"
yad       = ClassApp "Yad"                  "yad --text-info --text 'XMonad'"
obs       = ClassApp "Obs"                  "obs"
about     = TitleApp "About Mozilla Firefox" "About Mozilla Firefox"
picture   = TitleApp "Picture-in-Picture"    "Picture-in-Picture"
gcolor    = TitleApp "gcolor2"               "gcolor2"
iwarp     = TitleApp "IWarp"                "IWarp"

myManageHook = manageApps <+> manageSpawn <+> namedScratchpadManageHook myScratchPads
 where
  isBrowserDialog     = isDialog <&&> className =? "Brave-browser"
  isFileChooserDialog = isRole =? "GtkFileChooserDialog"
  isPopup             = isRole =? "pop-up"
  isSplash            = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"
  isRole              = stringProperty "WM_WINDOW_ROLE"
  tileBelow           = insertPosition Below Newer
  doCalendarFloat   = customFloating (W.RationalRect (11 / 15) (1 / 48) (1 / 4) (1 / 4))
  anyOf :: [Query Bool] -> Query Bool
  anyOf = foldl (<||>) (pure False)
  match :: [App] -> Query Bool
  match = anyOf . fmap isInstance
  manageApps = composeOne
    [ isInstance calendar                      -?> doCalendarFloat
    , match [ gimp, gimp2 ]                    -?> doFloat
    , match [ audacious
            , eog
            , obs
            , pavuctrl
            , about
            , picture
            , gcolor
            , iwarp
            ]                                  -?> doCenterFloat
    , match [ evince, spotify, vlc, yad ]      -?> doFullFloat
    , resource =? "desktop_window"             -?> doIgnore
    , resource =? "kdesktop"                   -?> doIgnore
    , anyOf [ isBrowserDialog
            , isFileChooserDialog
            , isDialog
            , isPopup
            , isSplash
            ]                                  -?> doCenterFloat
    , isFullscreen                             -?> doFullFloat
    , pure True                                -?> tileBelow
    ]

isInstance (ClassApp c _) = className =? c
isInstance (TitleApp t _) = title =? t
isInstance (NameApp n _)  = appName =? n

getNameCommand (ClassApp n c) = (n, c)
getNameCommand (TitleApp n c) = (n, c)
getNameCommand (NameApp  n c) = (n, c)

getAppName    = fst . getNameCommand
getAppCommand = snd . getNameCommand

--------------------------
-- XMOBAR CONFIGURATION --
--------------------------

myXmobarPP :: X PP
myXmobarPP = clickablePP . filterOutWsPP [scratchpadWorkspaceTag] $ def
      { ppCurrent         = xmobarColor "#71abeb" "" . xmobarBorder "Bottom" "#71abeb" 3
      , ppVisible         = xmobarColor "#5AB1BB" ""
      , ppHidden          = xmobarColor "#e5c07b" "" . xmobarBorder "Bottom" "#e5c07b" 3
      , ppHiddenNoWindows = xmobarColor "#d6d5d5" ""
      , ppUrgent          = xmobarColor "#e06c75" "" . wrap "!" "!"
      , ppTitle           = xmobarColor "#9ec07c" "" . shorten 90
      , ppWsSep           = "  "
      , ppLayout          = xmobarColor "#c678dd" ""
      , ppSep             = "<fc=#4b5363> <fn=1>|</fn> </fc>"
      , ppOrder           = \(ws : l : t : extras) -> [ws,l]++extras++[t]
      , ppExtras          = [windowCount]
      }

----------------------
-- XMOBAR INSTANCES --
----------------------
xmobar0 :: StatusBarConfig
xmobar0 = statusBarProp "xmobar ~/.config/xmobar/xmobar.hs" myXmobarPP

---------------------
------- Main --------
---------------------

main :: IO ()
main = do

    xmonad . docks . withSB xmobar0 . javaHack . ewmhFullscreen . ewmh $ def
        { manageHook         = myManageHook
        , handleEventHook    = handleEventHook def <+> fullscreenEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , logHook            = updatePointer (0.5, 0.5) (0, 0) <> historyHook <> refocusLastLogHook
        , workspaces         = myWorkspaces
        , borderWidth        = 5
        , normalBorderColor  = "#282c34"
        , focusedBorderColor = "#4d78cc"
        } `additionalKeysP` myKeys
