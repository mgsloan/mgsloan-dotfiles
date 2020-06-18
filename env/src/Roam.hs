module Roam where

import Imports

roamTemplates :: [(String, XX ())]
roamTemplates = map (second (forkXio . roamInsert))
  [ ( "book-template"
    , [ "Author::"
      , "Reading Status:: #waiting"
      , "Found via::"
      , "Tags:: #book"
      ])
  , ( "paper-template"
    , [ "Author::"
      , "Source::"
      , "Found via::"
      , "Tags:: #paper"
      ])
  , ( "article-template"
    , [ "Author::"
      , "Source::"
      , "Found via::"
      , "Tags:: #article"
      ])
  , ( "project-template"
    , [ "Due Date::"
      , "Status:: #waiting "
      , "Success Criteria::"
      , "Completed Date::"
      , "Related Goals::"
      , "Tags:: #project"
      ])
  , ( "goal-template"
    , [ "Projects::"
      , "Status:: #in-progress"
      , "Success Criteria::"
      , "Completed Date::"
      , "Tags:: #goal"
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
