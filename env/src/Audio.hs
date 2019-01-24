-- | Utilities using 'amixer' to set volume and mute
module Audio where

import Imports

muteAudio :: Xio ()
muteAudio = amixerSet ["mute"]

unmuteAudio :: Xio ()
unmuteAudio = amixerSet ["unmute"]

toggleAudio :: Xio ()
toggleAudio = amixerSet ["toggle"]

volumeMax :: Xio ()
volumeMax = amixerSet ["100%"]

volumeMin :: Xio ()
volumeMin = amixerSet ["0%"]

volumeUp :: Xio ()
volumeUp = amixerSet ["5%+"]

volumeDown :: Xio ()
volumeDown = amixerSet ["5%-"]

amixerSet :: [String] -> Xio ()
amixerSet args = amixer (["set", "Master"] ++ args) (fromMaybe "" . lastMay . lines)

toggleMicrophone :: Xio ()
toggleMicrophone = amixer ["set", "Capture", "toggle"] (unlines . reverse . take 2 . reverse . lines)

amixer :: [String] -> (String -> String) -> Xio ()
amixer args postprocess = do
  output <- syncSpawnAndRead "amixer" (["-D", "pulse"] ++ args)
  syncSpawn "notify-send" ["-t", "1000", "amixer", postprocess output]
