-- | Uses https://github.com/sachaos/todoist CLI application to
-- interact with Todoist.
--
-- Here's how I installed this on Ubuntu-18.04:
--
-- > cd ~/dl
-- >
-- > wget https://dl.google.com/go/go1.13.3.linux-amd64.tar.gz
-- >
-- > mkdir -p ~/oss/go/src/github.com/sachaos
-- >
-- > cd ~/oss/go/src/github.com/sachaos
-- >
-- > git clone https://github.com/sachaos/todoist
-- >
-- > cd todoist
-- >
-- > export GOROOT=~/dl/go
-- >
-- > export GOPATH=~/oss/go/src
-- >
-- > export PATH=$GOPATH/bin:$GOROOT/bin:$PATH
-- >
-- > make install
-- >
-- > mv ~/oss/go/src/bin/todoist ~/.local/bin
module Todoist where

import XMonad.Prompt
import Imports
import Prompt

addTodoistTask :: XX ()
addTodoistTask = do
  xpConfig <- getXpConfig
  env <- ask
  toXX $ mkXPrompt (GenericPrompt "Add task: ") xpConfig (const $ return []) $ \content ->
    withEnv env $ spawnAndNotifyFail "todoist" ["a", content]

todoistCliRaw :: XX ()
todoistCliRaw = do
  xpConfig <- getXpConfig
  let prompt = "Run todoist cli (-d DATE -N PROJECT -p PRIORITY \"text\"): "
  env <- ask
  toXX $ mkXPrompt (GenericPrompt prompt) xpConfig (const $ return []) $ \content ->
    withEnv env $ spawnAndNotifyFail "bash" ["-c", "todoist " ++ content]
