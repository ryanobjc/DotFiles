import XMonad
import XMonad.Config.Gnome
import XMonad.Layout.Gaps
import XMonad.Hooks.ManageHelpers (isFullscreen,doFullFloat)
import XMonad.Hooks.ICCCMFocus
import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.ManageHook
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myManageHook = composeAll(
	     [ className =? "Unity-2d-panel"    --> doIgnore
	     , className =? "Unity-2d-launcher" --> doFloat
	     , className =? "Unity-2d-shell"    --> doIgnore
	     , className =? "panel-launcher"    --> doIgnore
	     ])

main = do
     xmproc <- spawnPipe "/usr/bin/xmobar /home/ryan/.xmobarrc"
     xmonad $ defaultConfig
     	    { terminal = "/usr/bin/gnome-terminal.wrapper"
	    , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
	    , layoutHook = avoidStruts  $  layoutHook defaultConfig
	    , logHook = takeTopFocus <+> dynamicLogWithPP xmobarPP
	      	      			 { ppOutput = hPutStrLn xmproc
					 , ppTitle = xmobarColor "green" "" . shorten 150
					 }
     	    , modMask = mod4Mask
	    , startupHook = setWMName "LG3D"
     	    } `additionalKeys`
     	    [ ((mod4Mask .|. shiftMask, xK_z), spawn "gnome-screensaver-command --lock")
     	    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
     	    , ((0, xK_Print), spawn "scrot")
	    , ((mod4Mask .|. shiftMask, xK_q), spawn "gnome-session-quit")
     	    ]