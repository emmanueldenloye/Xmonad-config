import           Control.Applicative
import           Data.List
import           Data.Monoid
import           System.IO
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.NoBorders
import qualified XMonad.StackSet as S
import           XMonad.Util.EZConfig (additionalKeys,additionalKeysP,removeKeys)
import           XMonad.Util.Run (spawnPipe)
-- import           XMonad.Prompt
-- import           XMonad.Prompt.XMonad

main :: IO ()
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/emmanuel/.xmobarrc"
    xmonad $ defaultConfig
        { terminal                  = myTerminal
          , modMask                   = myModMask
          , workspaces                = myWorkspaces
          , borderWidth               = myBorderWidth
          , focusFollowsMouse         = myFocusFollowsMouse
          , clickJustFocuses         = myClickJustFocuses
          , normalBorderColor         = myNormalBorderColor
          , focusedBorderColor        = myFocusedBorderColor
          , handleEventHook           = fullscreenEventHook
          , manageHook = manageDocks <+> myManageHook
                         <+> manageHook defaultConfig
          , layoutHook = avoidStruts $ layoutHook defaultConfig
          , logHook    = dynamicLogWithPP xmobarPP
                          { ppOutput = hPutStrLn xmproc
                          , ppTitle  = xmobarColor "red" "" . shorten 50
                          }
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), (windows . S.greedyView $ (!! 1) myWorkspaces) >> spawn "slock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        , ((mod4Mask, xK_Right), spawn "amixer -D pulse sset Master 4%+")
        , ((mod4Mask, xK_Left), spawn  "amixer -D pulse sset Master 4%-")
        , ((mod4Mask, xK_Down), spawn  "amixer -D pulse sset Master toggle")
        , ((0, xK_Print), spawn "scrot")
        -- , ((mod4Mask .|. controlMask, xK_x), xmonadPrompt defaultXPConfig)
        ] `additionalKeysP` myEmacsKeys
          `removeKeys` [(mod4Mask .|. shiftMask, n) | n <- [xK_1 .. xK_9]]

myEmacsKeys :: [(String, X ())]
myEmacsKeys = zip viewKeys (worker S.greedyView myWorkspaces) ++
              zip shiftKeys (worker S.shift myWorkspaces) ++
              miscKeys
  where
    modWorkSpaceKey       = "M4-c "
    (viewKeys,shiftKeys) = (,) <$> map (modWorkSpaceKey ++)
                               <*> map ((++) $ modWorkSpaceKey ++ "S-")
                                $ baseWorkSpaceKeys
    baseWorkSpaceKeys     = group "htnsgcrl"
    worker f              = map (windows . f)
    miscKeys              = [("M4-c e", spawn "emacsclient -c")
                            ,("<XF86AudioRaiseVolume>", spawn "amixer -D pulse sset Master 4%+")
                            ,("<XF86AudioLowerVolume>", spawn "amixer -D pulse sset Master 4%-")
                            ,("<XF86AudioMute>", spawn "amixer -D pulse sset Master toggle")]

myTerminal :: String
myTerminal = "gnome-terminal"

myWorkspaces :: [String]
myWorkspaces    = ["firefox"
                  ,"prog-mode"
                  ,"zathura"
                  ,"zsh"
                  ,"org-latex"
                  ,"erc/mail"
                  ,"chrome"
                  , "random"]

-- hooks to manage floating windows
myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
                [className =? "Gimp" --> doFloat
                 ,className =? "VNC Viewer" --> doFloat
                 ,className =? "Firefox-bin" --> doShift "firefox"
                 ,(className =? "Firefox" <&&> resource =? "Dialog") --> doFloat
                 ,className =? "Xmessage" --> doFloat
                 ,isFullscreen --> doFullFloat]

myModMask :: KeyMask
myModMask = mod4Mask

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Border colors for unfocused and focused windows, respectively.

myNormalBorderColor :: String
myNormalBorderColor  = "#DDDDDD"

myFocusedBorderColor :: String
myFocusedBorderColor = "#ff0000"

myClickJustFocuses :: Bool
myClickJustFocuses = True

myBorderWidth :: Dimension
myBorderWidth   = 0
