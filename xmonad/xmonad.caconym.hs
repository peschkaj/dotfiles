import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.Navigation2D
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog        ( dynamicLogWithPP
                                                , xmobarPP
                                                , xmobarColor
                                                , ppOutput
                                                , ppTitle
                                                , ppCurrent
                                                , ppVisible
                                                , ppHidden
                                                , ppUrgent
                                                , ppSort
                                                )
import           XMonad.Hooks.DynamicProperty
import           XMonad.Hooks.ManageHelpers     ( doCenterFloat
                                                , doFullFloat
                                                , isDialog
                                                , isFullscreen
                                                )
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.Fullscreen
import           XMonad.Util.EZConfig
import           XMonad.Util.Run

import qualified XMonad.StackSet               as W

import           Data.Char                      ( isPrint )
import           Data.List                      ( isPrefixOf )
import           Data.Monoid                    ( Endo
                                                , mconcat
                                                )

rofi = "rofi -show drun -modi drun"
rofiRunCommand = "rofi -show run -modi run"
rofiCalc = "rofi -show calc -modi calc -no-show-match -no-sort"
rofiClip
  = "rofi -modi \"clipboard:greenclip print\" -show clipboard -run-command '{cmd}'"
rofiPower =
  "/home/jeremiah/.local/bin/rofi-power \"/home/jeremiah/.local/bin/stop\""
rofiCharpicker = "/home/jeremiah/src/charpicker/charpicker.sh"

--------------------------------------------------------------------------------
-- Key configs
myModMask = mod3Mask -- changes the mod key to "right alt"

myKeys baseConfig@(XConfig { modMask = modKey }) =
  -- ctrl-[1..9]           %! Switch to workspace N
  -- ctrl-shift-[1..9]     %! Move client to workspace N
  -- ctrl-shift-win-[1..9] %! Move client and switch to workspace N
  [ ((m .|. controlMask, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces baseConfig) [xK_1 .. xK_9]
  , (f, m) <-
    [ (W.greedyView                    , 0)
    , (W.shift                         , shiftMask)
    , (\i -> W.greedyView i . W.shift i, shiftMask .|. mod4Mask)
    ]
  ]

--------------------------------------------------------------------------------
-- | Desktop layouts
myLayouts = emptyBSP



------------------------------------------------------------------------
-- | Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
  [ resource =? "desktop_window" --> doIgnore
  , className =? "Galculator" --> doFloat
  , className =? "Steam" --> doFloat
  , className
  =?  "steam"
  --> doFullFloat  -- bigpicture-mode
  , className =? "Gimp" --> doFloat
  , className =? "stalonetray" --> doIgnore
  , isDialog --> doCenterFloat
  , isFullscreen --> (doF W.focusDown <+> doFullFloat)
  ]

myDynNameHook =
  composeAll [(title =? "Downloading Files - Mozilla Firefox" --> doFloat)]

--------------------------------------------------------------------------------
-- | Log bar
mkConfig xmProc = desktopConfig
  { terminal        = "kitty"
  , modMask         = myModMask -- super
  , layoutHook      = desktopLayoutModifiers $ myLayouts
  , manageHook      = myManageHook
  , logHook         = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmProc
                        , ppTitle = xmobarColor "orange" "" . filter isPrint
                        , ppCurrent = \s -> xmobarColor "green" "" ("[" ++ s ++ "]")
                        }
  , handleEventHook = fullscreenEventHook
                      <+> dynamicPropertyChange "WM_NAME" myDynNameHook
                      <+> handleEventHook desktopConfig
  , workspaces      = map show [1 .. 9 :: Int]
  }

main = do
  xmobarProc <- spawnPipe "~/.local/bin/xmobar ~/.xmobarrc"
  let myConfig = mkConfig xmobarProc
  -- floatNextWindows <- newIORef 0
  xmonad
    $                 myConfig
    -- remove default modMask + [1 - 9] binding for switching workspaces
    `removeKeys`      [ (mod3Mask, n) | n <- [xK_1 .. xK_9] ]
    -- remove modMask + SHIFT + [1 - 9] binding for flinging crap around workspaces
    `removeKeys`      [ (mod3Mask .|. shiftMask, n) | n <- [xK_1 .. xK_9] ]
    -- Unset the quit xmonad command because we want a sane shutdown
    `removeKeys`      [(myModMask .|. shiftMask, xK_q)]
    `additionalKeys`  (myKeys myConfig)
    `additionalKeysP` [ ("M-S-q", spawn "/home/jeremiah/.local/bin/stop")
                      , ("M4-<Space>"             , spawn $ rofi)
                      , ("M4-r"                   , spawn $ rofiRunCommand)
                      , ("M4-h"                   , windowGo L False)
                      , ("M4-j"                   , windowGo D False)
                      , ("M4-k"                   , windowGo U False)
                      , ("M4-l"                   , windowGo R False)
                      , ("C-M4-h"                 , windowSwap L False)
                      , ("C-M4-j"                 , windowSwap D False)
                      , ("C-M4-k"                 , windowSwap U False)
                      , ("C-M4-l"                 , windowSwap R False)
                      , ("M4-M1-5", spawn $ "shutter -s --profile=default")
                      , ("M4-c"                   , spawn $ rofiCharpicker)
                      , ("M4-S-c"                 , spawn $ rofiClip)
                      , ("M-p"                    , spawn $ rofiPower)
                      , ("M-r"                    , sendMessage Rotate)
                      , ("M-s"                    , sendMessage Swap)
                      , ("M-l", spawn "mate-screensaver-command -l")
                      , ("M-n"                    , sendMessage FocusParent)
                      , ("M-C-n"                  , sendMessage SelectNode)
                      , ("M-S-n"                  , sendMessage MoveNode)
                      , ("<XF86MonBrightnessUp>"  , spawn "light -A 5")
                      , ("<XF86MonBrightnessDown>", spawn "light -U 5")
                      ]

