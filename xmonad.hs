import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Config.Azerty (azertyKeys)
import XMonad.Util.CustomKeys (customKeys)
import XMonad.Prompt
import XMonad.Prompt.Shell
import qualified XMonad.StackSet as W

import Data.Char (isLetter, isDigit)
import Data.Monoid (Any(..), getAny, (<>))
import System.Posix.Env (putEnv, getEnv)
import System.Process (spawnProcess)

import qualified Data.Map as M

main :: IO ()
main = do
  putEnv "_JAVA_AWT_WM_NONREPARENTING=1"
  putEnv "OOO_FORCE_DESKTOP=gnome"
  gol
  setxkbmap
  xsetroot
  xsetbg
  emacs
  clipit
  redshift
  xmonad $ defaultConfig
    { modMask = mod4Mask --  Use Super instead of Alt
    , handleEventHook    = fullscreenEventHook
    , layoutHook         = myLayout
    , borderWidth        = 2
    , terminal           = "termonad"
    , normalBorderColor  = "#cccccc"
    , focusedBorderColor = "#cd8b00"
    , keys = myKeys <+> azertyKeys <+> keys def
    }

myKeys = customKeys delkeys inskeys
  where
    delkeys = const []
    inskeys (XConfig {modMask = modm}) =
      [ ((modm .|. shiftMask, xK_p), myPrompt =<< initMatches)
      , ((modm .|. shiftMask, xK_i), spawn "x-www-browser")
      , ((modm .|. shiftMask, xK_e), spawn "cemacs")
      ]

myPrompt ref = shellPrompt greenXPConfig
               { font = "-misc-fixed-*-*-*-*-18-*-*-*-*-*-*-*"
               , height = 25
               , promptKeymap = myPromptKeymap
               }
  where
    myPromptKeymap = (emacsLikeXPKeymap' $ not . isLetterOrDigit)
                     `M.union`
                     M.fromList
                     [ ((controlMask, xK_m), setSuccess True >> setDone True)
                     , ((controlMask, xK_r), historyUpMatching ref)
                     , ((controlMask, xK_s), historyDownMatching ref)
                     , ((controlMask, xK_n), moveHistory W.focusUp')
                     , ((controlMask, xK_p), moveHistory W.focusDown')
                     ]
    isLetterOrDigit = getAny . (Any . isLetter <> Any . isDigit)

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

emacs = do
  mbUser <- getEnv "USER"
  let user = maybe "neuville" id mbUser
  spawnProcess ("/home/" ++ user ++ "/bin/semacs") []

xsetroot = spawnProcess "xsetroot" ["cursor_name left_ptr"]

setxkbmap = spawnProcess "setxkbmap" ["layout fr"]

clipit = spawnProcess "parcellite" []

xsetbg = spawnProcess "xsetbg" ["~/Images/bg.png"]

redshift = do
  spawnProcess "/usr/lib/geoclue-2.0/demos/agent" []
  spawnProcess "redshift-gtk" []

-- Growl on linux
gol = spawnProcess "gol" []
