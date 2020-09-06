import XMonad
import XMonad.Layout.ResizableTile
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Util.EZConfig
import System.IO

myLayout = avoidStruts (tall ||| Mirror tall ||| Full)
  where
    tall = ResizableTall 1 (3/100) (1/2) []

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ docks $ ewmh def
    { modMask = mod4Mask     -- Rebind Mod to the Windows key
    , terminal = "qterminal"
    , layoutHook = myLayout
    , logHook = dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green" "" . shorten 50
                }
    , borderWidth = 3
    } `additionalKeys`
    [
      ((mod4Mask .|. shiftMask, xK_h), sendMessage MirrorExpand)
    , ((mod4Mask .|. shiftMask, xK_l), sendMessage MirrorShrink)
    , ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    , ((0                     , 0x1008ff11), spawn "amixer -q sset Master 2%- && /home/rohan/bin/notify_volume.sh")
    , ((0                     , 0x1008ff13), spawn "amixer -q sset Master 2%+ && /home/rohan/bin/notify_volume.sh")
    , ((0                     , 0x1008ff12), spawn "amixer -D pulse set Master toggle")
    ]
