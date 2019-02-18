import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Navigation2D
import XMonad.Config.Desktop
import XMonad.Layout.BinarySpacePartition
import XMonad.Util.Run
import XMonad.Util.EZConfig

import qualified XMonad.StackSet as W

rofi = "rofi -show run"

--------------------------------------------------------------------------------
-- Possible mod keys
-- mod1 - Left Alt
-- mod2 - NUMLOCK
-- mod3 


myModMask = mod4Mask -- changes the mod key to "super"

myKeys baseConfig@(XConfig {modMask = modKey}) =
  -- ctrl-[1..9] %! Switch to workspace N
  -- ctrl-shift-[1..9] %! Move client to workspace N
  -- ctrl-shift-win-[1..9] %! Move client and switch to workspace N
  [((m .|. controlMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces baseConfig) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0)
                  ,(W.shift, shiftMask)
                  ,(\i -> W.greedyView i . W.shift i, shiftMask .|. mod4Mask)
                  ]]

-- TODO: Add dynamicLogWithPP to xmobar, take a look at https://github.com/davidbrewer/xmonad-ubuntu-conf/blob/master/xmonad.hs
myConfig = desktopConfig
  { terminal   = "kitty"
  , modMask    = myModMask -- super
  , layoutHook = desktopLayoutModifiers $ myLayouts
  , workspaces = map show [ 1 .. 9 :: Int ]
  }

main = do
  xmobarProc <- spawnPipe "~/.local/bin/xmobar ~/.xmobarrc"
  -- floatNextWindows <- newIORef 0
  xmonad $ myConfig
    -- remove default modMask + [1 - 9] binding for switching workspaces
    `removeKeys` [(mod4Mask, n) | n <- [xK_1 .. xK_9]]
    -- remove modMask + SHIFT + [1 - 9] binding for flinging crap around workspaces
    `removeKeys` [(mod4Mask .|. shiftMask, n) | n <- [xK_1 .. xK_9]]
    -- add CTRL + [1 - 9] for switching workspaces
    `additionalKeys` (myKeys myConfig)
    `additionalKeysP`
    [ ("M4-<Space>", spawn $ rofi)
    , ("M4-h", windowGo L False)
    , ("M4-j", windowGo D False)
    , ("M4-k", windowGo U False)
    , ("M4-l", windowGo R False)
    , ("C-M4-h", windowSwap L False)
    , ("C-M4-j", windowSwap D False)
    , ("C-M4-k", windowSwap U False)
    , ("C-M4-l", windowSwap R False)
    ]

--------------------------------------------------------------------------------
-- | Desktop layouts
myLayouts = emptyBSP

--------------------------------------------------------------------------------
-- | Keyboard layout
