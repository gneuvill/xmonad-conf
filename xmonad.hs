import XMonad
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Config.Azerty
import System.Posix.Env (putEnv)
import System.Process(spawnProcess)

main :: IO ()
main = do
  putEnv "_JAVA_AWT_WM_NONREPARENTING=1"
  setxkbmap
  xsetroot
  xsetbg
  emacs
  clipit
  redshift
  xmonad $ azertyConfig
    { modMask = mod4Mask --  Use Super instead of Alt
    , handleEventHook    = fullscreenEventHook
    , layoutHook         = myLayout
    , borderWidth        = 2
    , terminal           = "evilvte"
    , normalBorderColor  = "#cccccc"
    , focusedBorderColor = "#cd8b00"
    }

myLayout = tiled ||| Mirror tiled ||| smartBorders Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

emacs = spawnProcess "/home/neuville/bin/semacs" []

xsetroot = spawnProcess "xsetroot" ["cursor_name left_ptr"]

setxkbmap = spawnProcess "setxkbmap" ["layout fr"]

clipit = spawnProcess "clipit" []

xsetbg = spawnProcess "xsetbg" ["/home/neuville/archdesktop.jpg"]

redshift = spawnProcess "redshift-gtk" []
