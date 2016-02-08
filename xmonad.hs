import           Data.List
import           Data.Monoid
import           System.IO
import           XMonad
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.Warp -- Banish the pointer!
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Roledex
import           XMonad.Layout.NoBorders
import qualified XMonad.StackSet as S
import           XMonad.Util.EZConfig (additionalKeys,additionalKeysP,removeKeys)
import           XMonad.Util.Run (spawnPipe)
import           XMonad.Prompt
import           XMonad.Prompt.XMonad

main :: IO ()
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/emmanuel/.xmobarrc"
    xmonad $ defaultConfig
        { terminal             = myTerminal
          , modMask            = myModMask
          , workspaces         = myWorkspaces
          , borderWidth        = myBorderWidth
          , focusFollowsMouse  = myFocusFollowsMouse
          , clickJustFocuses   = myClickJustFocuses
          , normalBorderColor  = myNormalBorderColor
          , focusedBorderColor = myFocusedBorderColor
          , handleEventHook    = handleEventHook defaultConfig <+> fullscreenEventHook
          , manageHook         = manageDocks <+> myManageHook
                         <+> manageHook defaultConfig
          , layoutHook         = avoidStruts myLayout
          , logHook            = dynamicLogWithPP xmobarPP
                          { ppOutput = hPutStrLn xmproc
                          , ppTitle  = xmobarColor "red" "" . shorten 50
                          }
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), (windows . S.greedyView $ (!! 0) myWorkspaces) >> spawn "slock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        , ((mod4Mask, xK_Right), spawn "amixer -D pulse sset Master 4%+")
        , ((mod4Mask, xK_Left), spawn  "amixer -D pulse sset Master 4%-")
        , ((mod4Mask, xK_Down), spawn  "amixer -D pulse sset Master toggle")
        , ((mod4Mask, xK_t), windows S.focusDown)
        , ((mod4Mask .|. shiftMask, xK_t), windows S.swapDown)
        , ((mod4Mask .|. controlMask, xK_t), withFocused $ windows . S.sink)
        , ((mod4Mask, xK_n), windows S.focusUp)
        , ((mod4Mask .|. shiftMask, xK_n), windows S.swapUp)
        , ((mod4Mask, xK_s), sendMessage Expand)
        , ((0, xK_Print), spawn "scrot")
        , ((mod4Mask .|. controlMask, xK_x), xmonadPrompt defaultXPConfig)
        ] `additionalKeysP` myEmacsKeys
          `removeKeys` ([(mod4Mask .|. shiftMask, n) | n <- [xK_1 .. xK_9]]
                       ++ [(mod4Mask, xK_j),(mod4Mask, xK_k),(mod4Mask, xK_l)]
                       ++ [(mod4Mask .|. shiftMask, xK_j),(mod4Mask .|. shiftMask, xK_k)])

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
                            ,("<XF86AudioMute>", spawn "amixer -D pulse sset Master toggle")
                            ,("M4-g g", spawnHere (myTerminal ++ " -e /home/emmanuel/Development/bin/ghc/bin/ghci"))
                            ,("M4-b", banish UpperLeft)
                            ,("M4-S-b", banishScreen UpperRight)]

myTerminal :: String
myTerminal = "gnome-terminal"

myWorkspaces :: [String]
myWorkspaces    = ["firefox"
                  ,"prog-mode"
                  ,"erc/mail"
                  ,"zsh"
                  ,"org-latex"
                  ,"zathura"
                  ,"chrome"
                  ,"random"]

-- hooks to manage floating windows
myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
                [className =? "Gimp" --> doFloat
                 ,className =? "VNC Viewer" --> doFloat
                 ,className =? "MPlayer" --> doFloat
                 ,className =? "mpv" --> doFloat
                 ,className =? "xzgv" --> doFloat
                 ,className =? "Firefox-bin" --> doShift "firefox"
                 ,(className =? "Firefox" <&&> resource =? "Dialog") --> doFloat
                 ,className =? "Xmessage" --> doFloat
                 ,isFullscreen --> doFullFloat]

myModMask :: KeyMask
myModMask = mod4Mask

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myNormalBorderColor :: String
myNormalBorderColor  = "#DDDDDD"

myFocusedBorderColor :: String
myFocusedBorderColor = "#ff0000"

myClickJustFocuses :: Bool
myClickJustFocuses = True

myLayout = smartBorders tiled ||| smartBorders Full ||| Roledex -- This is very silly!
    where
      tiled = Tall nmaster delta ratio
      nmaster = 1
      ratio = 1/2
      delta = 2 / 100


myBorderWidth :: Dimension
myBorderWidth   = 1
