-- | Could be made into an extension. Generalized from "XMonad.Util.SpawnOnce"
module DoOnce where

import XMonad
import XMonad.Actions.SpawnOn
import qualified Data.Set as S
import qualified XMonad.Util.ExtensibleState as State

import Misc

data DoOnce = DoOnce { unDoOnce :: S.Set String }
    deriving (Read, Show, Typeable)

instance ExtensionClass DoOnce where
    initialValue = DoOnce S.empty
    extensionType = PersistentExtension

-- | The first time 'spawnOnce' is executed on a particular command, that
-- command is executed.  Subsequent invocations for a command do nothing.
doOnce :: String -> X () -> X ()
doOnce k action = do
    whenX (not <$> State.gets (S.member k . unDoOnce)) $ do
        action
        State.modify (DoOnce . S.insert k . unDoOnce)

spawnOnce :: String -> X ()
spawnOnce cmd = doOnce (cmd ++ " # spawnOnce") $ spawn cmd

-- This command was the reason I needed to generalize the SpawnOnce module.
spawnOnceOn :: WorkspaceId -> String -> X ()
spawnOnceOn wid cmd = doOnce (cmd ++ " # spawnOnceOn") $ spawnOn wid cmd
