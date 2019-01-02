-- | Prompt for running byzanz
module Byzanz where

{- FIXME: reinstate

import XMonad
import XMonad.Prompt

import Constants

data ByzanzPrompt = ByzanzPrompt

instance XPrompt ByzanzPrompt where
  showXPrompt ByzanzPrompt = "Byzanz arguments: "

byzanzPrompt :: XPConfig -> X ()
byzanzPrompt c = mkXPrompt ByzanzPrompt c (const $ return []) $ \args ->
  let args' = if null args
                 then "10"
                 else args
  in spawn $ "~/env/byzanz-record-region.sh " ++ args' ++ " /tmp/recorded.gif; " ++ browser ++ " /tmp/recorded.gif"

-}
