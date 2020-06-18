module Roam where

import Imports

roamTemplates :: [(String, XX ())]
roamTemplates = map (second (forkXio . roamInsert))
  [ ( "book-template"
    , [ "Author::"
      , "Found via::"
      , "Tags:: #book #unread"
      ])
  , ( "paper-template"
    , [ "Author::"
      , "Source::"
      , "Found via::"
      , "Tags:: #paper #unread"
      ])
  , ( "article-template"
    , [ "Author::"
      , "Source::"
      , "Found via::"
      , "Tags:: #article #unread"
      ])
  , ( "project-template"
    , [ "Due Date::"
      , "Success Criteria::"
      , "Completed Date::"
      , "Related Goals::"
      , "Tags:: #project #waiting"
      ])
  , ( "goal-template"
    , [ "Projects::"
      , "Success Criteria::"
      , "Completed Date::"
      , "Tags:: #goal #in-progress"
      ])
  , ( "person-template"
    , [ "Contact Info::"
      , "Company::"
      , "Role::"
      , "Location::"
      , "How We Met::"
      , "Interests::"
      , "Tags:: #person"
      ])
  ]

roamInsert :: [String] -> Xio ()
roamInsert [] = return ()
roamInsert ls = forM_ ls $ \l -> do
  xdotoolType (l ++ " ")
  liftIO $ threadDelay (1000 * 100)
  xdotoolType "\n"
  liftIO $ threadDelay (1000 * 100)

xdotoolType :: String -> Xio ()
xdotoolType text = syncSpawn "xdotool" ["type", text]
