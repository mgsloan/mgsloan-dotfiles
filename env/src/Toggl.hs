module Toggl where

{-
globalManager :: IORef Manager
globalManager = unsafePerformIO $ newIORef =<< newManager tlsManagerSettings
{-# NOINLINE globalManager #-}

withToggl :: (Token -> ClientM a) -> X a
withToggl f = do
  mgr <- liftIO $ readIORef globalManager
  token <- readToken "/home/mgsloan/.xmonad/toggl-token"
  let clientEnv = ClientEnv mgr togglBaseUrl
  eres <- liftIO $ runClientM (f (Api token)) clientEnv
  case eres of
    Left err -> liftIO $ do
      print err
      throwIO err
    Right res -> return res

promptTogglTimer :: X ()
promptTogglTimer =
  mkXPrompt (GenericPrompt "FIXME") (xpconfig False) (const $ return []) $ \msg -> do
    timerInfo <- withToggl $ \token -> startTimer token TES
      { tesDescription = Just (T.pack msg)
      , tesTags = []
      , tesPid = Nothing
      , tesCreatedWith = "mgsloan's xmonad.hs + the hoggl library"
      }
    notify "toggl" (T.unpack (fromMaybe "" (teDescription timerInfo)))

stopTogglTimer :: X ()
stopTogglTimer = do
  currentTimer <- withToggl $ \token -> do
    currentTimer <- currentTimeEntry token
    case currentTimer of
      Just te -> void $ stopTimer token (teId te)
      Nothing -> return ()
    return currentTimer
  case currentTimer of
    Just te -> notify summary (maybe "" T.unpack (teDescription te))
      where
        summary =
          maybe (++ " - NO CLIENT") (\c -> (++ (" - " ++ T.unpack c))) (teClient te) $
          maybe "toggl stopped - NO PROJECT " (("toggl stopped - " ++) . T.unpack) (teProject te)
    Nothing -> notify "toggl" "No timer running"
-}

{-
listTogglProjects :: X ()
listTogglProjects = do
-}
