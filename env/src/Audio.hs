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
amixerSet cmd = syncSpawn "amixer" (["-D", "pulse", "sset", "Master"] ++ cmd)
