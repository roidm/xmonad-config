  -- Base
import           System.Directory
import           System.Exit                         (exitSuccess)
import           System.IO                           (hPutStrLn)
import           XMonad
import           XMonad.Operations
import qualified XMonad.StackSet                     as W

    -- Actions
import           XMonad.Actions.CopyWindow           (copyToAll, kill1,
                                                      killAllOtherCopies)
import           XMonad.Actions.CycleWS              (Direction1D (..),
                                                      WSType (..), moveTo,
                                                      nextScreen, prevScreen,
                                                      shiftTo)
import           XMonad.Actions.MouseResize
import           XMonad.Actions.Promote
import           XMonad.Actions.RotSlaves            (rotAllDown, rotSlavesDown)
import           XMonad.Actions.WindowGo             (runOrRaise)
import           XMonad.Actions.WithAll              (killAll, sinkAll)

    -- Data
import qualified Data.Map                            as M
import           XMonad.Prelude                      (Endo, fromJust, isDigit,
                                                      isJust, isSpace, toUpper)
    -- Hooks
import           XMonad.Hooks.DynamicLog             (PP (..), dynamicLogWithPP,
                                                      filterOutWsPP, shorten,
                                                      wrap, xmobarBorder,
                                                      xmobarColor, xmobarPP)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.InsertPosition         (Focus (..), Position (..),
                                                      insertPosition)
import           XMonad.Hooks.ManageDocks            (ToggleStruts (..),
                                                      avoidStruts,
                                                      docksEventHook,
                                                      manageDocks)
import           XMonad.Hooks.ManageHelpers          (composeOne, doCenterFloat,
                                                      doFullFloat, isDialog,
                                                      isFullscreen,
                                                      isInProperty, (-?>))
import           XMonad.Hooks.ServerMode
    -- Layouts
import           XMonad.Layout.Gaps
import           XMonad.Layout.GridVariants          (Grid (Grid))
import           XMonad.Layout.MouseResizableTile
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Spiral
import           XMonad.Layout.ThreeColumns

    -- Layouts modifiers
import           XMonad.Actions.Minimize
import qualified XMonad.Layout.BoringWindows         as BW
import qualified XMonad.Layout.Dwindle               as Dwindle
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.LimitWindows          (decreaseLimit,
                                                      increaseLimit,
                                                      limitWindows)
import           XMonad.Layout.Minimize
import           XMonad.Layout.MultiToggle           (EOT (EOT),
                                                      Toggle (Toggle), mkToggle,
                                                      single, (??))
import qualified XMonad.Layout.MultiToggle           as MT (Toggle (..))
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.Spacing
import qualified XMonad.Layout.ToggleLayouts         as T (ToggleLayout (Toggle),
                                                           toggleLayouts)
import           XMonad.Layout.WindowArranger        (WindowArrangerMsg (..),
                                                      windowArrange)
import           XMonad.Layout.WindowNavigation


   -- Utilities
import           XMonad.Actions.SpawnOn              (manageSpawn, spawnOn)
import           XMonad.Util.Dmenu
import           XMonad.Util.EZConfig                (additionalKeysP)
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run                     (runProcessWithInput,
                                                      safeSpawn, spawnPipe)
import           XMonad.Util.WorkspaceCompare        (getSortByIndex)

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

myEditor :: String
myEditor = "emacsclient -c -a 'emacs' "

myBorderWidth :: Dimension
myBorderWidth = 5           -- window border

myNormColor :: String
myNormColor   = "#282c34"   -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#4d78cc"   -- Border color of focused windows
-- myFocusColor  = "#98C379"

myBorderColor :: String
myBorderColor  = "#71abeb"

centreRect :: W.RationalRect
centreRect = W.RationalRect (1 / 3) (1 / 3) (1 / 3) (1 / 3)

videoRect :: W.RationalRect
videoRect = W.RationalRect offset offset size size
  where
    size = 1 / 4
    offset = 1 - size - (size / 8)

isFloating :: Window -> WindowSet -> Bool
isFloating w s = M.member w (W.floating s)

enableFloat :: W.RationalRect -> Window -> (WindowSet -> WindowSet)
enableFloat = flip W.float

enableFloat' :: W.RationalRect -> Window -> X ()
enableFloat' = windows $. enableFloat

disableFloat :: Window -> (WindowSet -> WindowSet)
disableFloat = W.sink

disableFloat' :: Window -> X ()
disableFloat' = windows . disableFloat

toggleFloat :: W.RationalRect -> Window -> X ()
toggleFloat r = windows . if2 isFloating disableFloat (enableFloat r)

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
    spawn "$HOME/.xmonad/scripts/autostart.sh"


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

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

tall     = renamed [Replace "tall"]
           $ minimize . BW.boringWindows
           $ limitWindows 12
           $ mySpacing 9
           $ ResizableTall 1 (1/100) (1/2) []
dwindle  = renamed [Replace "dwindle"]
           $ minimize . BW.boringWindows
           $ mySpacing 9
           $ limitWindows 12
           $ Dwindle.Dwindle R Dwindle.CW (2/2) (11/10)
spirals  = renamed [Replace "spirals"]
           $ minimize . BW.boringWindows
           $ windowNavigation
           $ mySpacing' 9
           $ spiral (2/2)
monocle  = renamed [Replace "monocle"]
           $ limitWindows 20 Full
grid     = renamed [Replace "grid"]
           $ limitWindows 12
           $ mySpacing 8
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
threeCol = renamed [Replace "threeCol"]
           $ mySpacing' 9
           $ limitWindows 7
           $ ThreeCol 1 (1/100) (1/2)
threeColMid = renamed [Replace "|C|"]
           $ minimize . BW.boringWindows
           $ mySpacing' 9
           $ limitWindows 7
           $ ThreeColMid 1 (1/100) (1/2)
floats   = renamed [Replace "floats"]
           $ limitWindows 20 simplestFloat


gap :: Int
gap = 18

fi = fromIntegral

mrt = mouseResizableTile { draggerType = FixedDragger (fi gap) (fi gap) }
applyGaps = gaps $ zip [U, D, R, L] $ repeat gap

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =     withBorder myBorderWidth tall
                                 ||| dwindle
                                 ||| spirals
                                 ||| avoidStruts (applyGaps mrt)
                                 ||| grid
                                 ||| threeCol
                                 ||| threeColMid
                                 ||| noBorders monocle
                                 ||| floats

xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . (map xmobarEscape) $ ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

   where
         clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                             (i,ws) <- zip [1..9] l,
                            let n = i ]



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
        , ("M-M1-h", spawn (myTerminal ++ " -e htop"))

    -- Kill windows
        , ("M-q", kill1)     -- Kill the currently focused client
        , ("M-S-a", killAll)   -- Kill all windows on current workspace

    -- Workspaces
        , ("M-.", nextScreen)  -- Switch focus to next monitor
        , ("M-,", prevScreen)  -- Switch focus to prev monitor
    --   , ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next ws
     --   , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to prev ws

    -- Floating windows
     --   , ("M-f", sendMessage (T.Toggle "Full"))
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
        , ("M-S-z", withLastMinimized maximizeWindow)

    -- Layouts
        , ("M-<Tab>", sendMessage NextLayout)           -- Switch to next layout
        , ("M-C-M1-<Up>", sendMessage Arrange)
        , ("M-C-M1-<Down>", sendMessage DeArrange)
        , ("M-S-<Space>", sendMessage ToggleStruts)     -- Toggles struts
        , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)  -- Toggles noborder
        , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
        , ("M-a", withFocused $ toggleFloat centreRect)
        , ("M-c", windows copyToAll <> withFocused (enableFloat' videoRect))
        , ("M-S-a", killAllOtherCopies <> withFocused disableFloat')

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
        , ("M1-S-t", namedScratchpadAction myScratchPads "term")
        , ("M1-r",   namedScratchpadAction myScratchPads "ranger")
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
iwarp      = TitleApp "IWarp"                "IWarp"

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

--myEventHook = docksEventHook <+> fullscreenEventHook


main :: IO ()
main = do

    xmbar <- spawnPipe "xmobar $HOME/.config/xmobar/xmobar.hs"
    xmonad $ ewmh def
        { manageHook = manageDocks <+> myManageHook
        , handleEventHook    = docksEventHook <+> fullscreenEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook = dynamicLogWithPP $  filterOutWsPP [scratchpadWorkspaceTag] $ xmobarPP
              { ppOutput = \x -> hPutStrLn xmbar x                              -- xmobar
              , ppCurrent = xmobarColor "#71abeb" "" . xmobarBorder "Bottom"  myBorderColor 3 -- Current workspace
              , ppVisible = xmobarColor "#5AB1BB" ""                            -- Visible but not current workspace
              , ppHidden = xmobarColor "#e5c07b" "" . xmobarBorder "Bottom" "#e5c07b" 3-- . wrap "-" "-" . clickable  -- Hidden workspaces
              , ppHiddenNoWindows = xmobarColor "#d6d5d5" ""                    -- Hidden workspaces (no windows)
              , ppWsSep   = "  "                                                -- Workspaces separator
              , ppTitle = xmobarColor "#9ec07c" "" . shorten 90                 -- Title of active window
              , ppSep =  "<fc=#4b5363> <fn=1>|</fn> </fc>"                      -- Separator character
              , ppUrgent = xmobarColor "#e06c75" "" . wrap "!" "!"              -- Urgent workspace
              , ppExtras  = [windowCount]                                       -- # of windows current workspace
              , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]                      -- order of things in xmobar
              , ppLayout  = xmobarColor "#c678dd" "" .
                  ( \t -> case t of
                      "MouseResizableTile" -> "MRT"
                      _                    -> t
                  )
              }
        } `additionalKeysP` myKeys
