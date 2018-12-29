module Recompile where

-- The idea of this is to override a function in the config like this:
--
-- , handleRecompile = customRecompile
--
-- and then get output from compilation as it makes progress, and
-- present compilation results in a way that is more friendly to
-- hidpi.
--
-- Unfortunately, it doesn't work yet, so sticking it into this file
-- for now.

{-
customRecompile :: ForceRecompile -> IO RecompileStatus
customRecompile _ = do
    cfgdir  <- getXMonadDir
    datadir <- getXMonadDataDir
    let binn = "xmonad-"++arch++"-"++os
        bin = datadir </> binn
        statusFile = cfgdir </> "recompile_status"
    bracket uninstallSignalHandlers (\() -> installSignalHandlers) $ \() -> do
      removeFile statusFile `catch` \(_ :: IOError) -> return ()
      ph <- runProcess
        terminalCmd
        ("-title" : recompileTitle : "-e" : (cfgdir </> "build-in-terminal.sh") : [bin])
        -- FIXME: Figure out why it won't run in tmux.
        -- ("-e" : "tmux" : "-c" : (cfgdir </> "build-in-terminal.sh") : [bin])
        (Just cfgdir)
        Nothing
        Nothing
        Nothing
        Nothing
      -- FIXME: Better implementation of blocking on file existence
      let blockOnStatusFile = do
            whileM (not <$> doesFileExist statusFile) $
              -- Delay 50ms
              threadDelay $ 50 * 1000
            trace "Found status file"
            status <- readFile statusFile
            case lines status of
              ["success"] -> return RecompileSuccess
              ["failure"] -> return RecompileFailure
              _ -> do
                trace ("Unexpected status: " ++ show status)
                return RecompileFailure
      eres <- race (waitForProcess ph) blockOnStatusFile
      return $ case eres of
        Left ExitSuccess -> RecompileSuccess
        Left ExitFailure{} -> RecompileFailure
        Right res -> res

closeRecompileWindows :: X ()
closeRecompileWindows =
  ifWindows (title =? recompileTitle) (mapM_ killWindow) (return ())

recompileTitle :: String
recompileTitle = "XMonad recompilation terminal"
-}
