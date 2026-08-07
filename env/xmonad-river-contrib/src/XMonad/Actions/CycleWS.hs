-- | Moving between workspaces and screens.
module XMonad.Actions.CycleWS
  ( nextScreen
  , prevScreen
  , nextWS
  , prevWS
  , shiftToNext
  , shiftToPrev
  , toggleWS
  ) where

import XMonad.Core
import XMonad.Operations (windows)
import qualified XMonad.StackSet as W

-- | Move focus to the next screen, wrapping around.
nextScreen :: X ()
nextScreen = withWindowSet $ \ws -> case W.visible ws of
  [] -> pure ()
  (s:_) -> windows (W.view (W.tag (W.workspace s)))

prevScreen :: X ()
prevScreen = withWindowSet $ \ws -> case reverse (W.visible ws) of
  [] -> pure ()
  (s:_) -> windows (W.view (W.tag (W.workspace s)))

nextWS :: X ()
nextWS = moveTo 1

prevWS :: X ()
prevWS = moveTo (-1)

shiftToNext :: X ()
shiftToNext = shiftBy 1

shiftToPrev :: X ()
shiftToPrev = shiftBy (-1)

-- | Return to the previously visited workspace.
toggleWS :: X ()
toggleWS = withWindowSet $ \ws -> case W.hidden ws of
  (h:_) -> windows (W.view (W.tag h))
  []    -> pure ()

-- | Offset within the workspace list, wrapping.
neighbour :: Int -> WindowSet -> WorkspaceId
neighbour n ws = tags !! ((idx + n) `mod` length tags)
  where
    tags = map W.tag (W.workspaces ws)
    current = W.tag (W.workspace (W.current ws))
    idx = length (takeWhile (/= current) tags)

moveTo :: Int -> X ()
moveTo n = withWindowSet $ \ws -> windows (W.view (neighbour n ws))

shiftBy :: Int -> X ()
shiftBy n = withWindowSet $ \ws -> windows (W.shift (neighbour n ws))
