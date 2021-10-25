import XMonad
import XMonad.Layout.ResizableTile
import XMonad.Layout.Grid
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Util.EZConfig
import System.IO
import XMonad.Actions.CycleWS
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO

myLayout = avoidStruts (tall ||| Mirror tall ||| layoutGrid ||| Full)
  where
    tall = ResizableTall 1 (3/100) (1/2) []
    layoutGrid = Grid

main = do
  xmproc0 <- spawnPipe "xmobar -x 0"
  xmproc1 <- spawnPipe "xmobar -x 1"
  xmproc2 <- spawnPipe "xmobar -x 2"

  xmonad $ docks $ ewmh def
    { modMask = mod4Mask     -- Rebind Mod to the Windows key
    , terminal = "qterminal"
    , layoutHook = myLayout
    , logHook = dynamicLogWithPP xmobarPP
                { ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x >> hPutStrLn xmproc2 x
                , ppTitle = xmobarColor "green" "" . shorten 50
                }
    , borderWidth = 4
    } `additionalKeys`
    [
      ((mod4Mask .|. shiftMask, xK_h), sendMessage MirrorExpand)
    , ((mod4Mask .|. shiftMask, xK_l), sendMessage MirrorShrink)
    , ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    , ((0                     , 0x1008ff11), spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
    , ((0                     , 0x1008ff13), spawn "pactl set-sink-volume @DEFAULT_SINK@  +2%")
    , ((0                     , 0x1008ff12), spawn "pactl set-sink-mute @DEFAULT_SINK@  toggle")
    , ((mod4Mask , xK_bracketleft), DO.moveTo Prev HiddenNonEmptyWS)
    , ((mod4Mask , xK_bracketright), DO.moveTo Next HiddenNonEmptyWS)
    ]
