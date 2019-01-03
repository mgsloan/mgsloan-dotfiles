module Scratchpads where

{-
scratchpads :: [NamedScratchpad]
scratchpads =
  [ urxvtPad "term" ["-title", "scratch_term"]
  , urxvtPad "ghci" ["-e", "ghci"]
  , urxvtPad "htop" ["-e", "htop"]
  , controlCenter "sound" "Sound"
  , controlCenter "display" "Displays"
  , emacsOpen "notes" "emacs-notes" ["~/notes.md"]
  , NS "power" "gnome-power-statistics" (className =? "Gnome-power-statistics") flt
  ]
 where
  urxvtPad n args = NS n
                       (unwords args')
                       ((intercalate "\NUL" args' `isInfixOf`) <$> stringProperty "WM_COMMAND")
                       flt
    where
      args' = [terminalCmd] ++ args ++ terminalArgs
  controlCenter n t = NS n
                         ("unity-control-center " ++ n)
                         (title =? t <&&> stringProperty "_GTK_APPLICATION_ID" =? "org.gnome.ControlCenter")
                         flt
  emacsOpen n t args = NS n
                          (unwords ("alacritty --title" : t : "-e" : "emacs" : args))
                          (title =? t)
                          flt
  flt = customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2)

openScratch :: String -> X ()
openScratch = namedScratchpadAction scratchpads

-- ...

    -- , namedScratchpadManageHook scratchpads

-- ...

  , ("M-a M-a", openScratch "term")
  , ("M-a M-s", openScratch "sound")
  , ("M-a M-d", openScratch "display")
  , ("M-a M-h", openScratch "htop")
  , ("M-a M-g", openScratch "ghci")
  , ("M-a M-p", openScratch "power")
  , ("M-a M-n", openScratch "notes")
-}
