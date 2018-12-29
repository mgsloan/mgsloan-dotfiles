module Todoist where

import Data.Time (getCurrentTime)
import XMonad
import XMonad.Prompt

import Misc
import Prompt

data GenericPrompt = GenericPrompt String

instance XPrompt GenericPrompt where
  showXPrompt (GenericPrompt x) = x

promptTodoistTaskWithDate :: X ()
promptTodoistTaskWithDate =
  mkXPrompt (GenericPrompt "Date: ") (xpconfig False) (const $ return []) $ \time ->
    promptTodoistTask "TODO: " time

promptTodoistTask :: String -> String -> X ()
promptTodoistTask msg time =
  mkXPrompt (GenericPrompt msg) (xpconfig False) (const $ return []) $ \content ->
    addTodoistTask time content

addTodoistTask :: String -> String -> X ()
addTodoistTask time content = do
  uid <- liftIO $ fmap show getCurrentTime
  let commandsArg = "commands='[{\"type\": \"item_add\", " ++
        "\"temp_id\":\"" ++ uid ++ "\", " ++
        "\"uuid\":\"" ++ uid ++ "\", " ++
        "\"args\":{\"content\":" ++ show content ++ ", " ++
                  "\"date_string\":" ++ show time ++ "}}]'"
  token <- readToken "/home/mgsloan/.xmonad/todoist-token"
  liftIO $ putStrLn $ "Sending todoist request with " ++ commandsArg
  spawn $ unwords $ "curl" :
    [ "--show-error"
    , "'https://todoist.com/API/v7/sync'"
    , "-d", "token='" ++ token ++ "'"
    , "-d", commandsArg]
  --FIXME: error handling
  -- when ("{\"error" `isPrefixOf` output) $
  --   spawn ("xmessage 'Todoist failed with:\n\n" ++ output ++ "'")
  -- liftIO $ putStrLn $ "Todoist response: " ++ output
