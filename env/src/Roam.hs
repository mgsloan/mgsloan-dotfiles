module Roam where

import Imports

roamTemplates :: [(String, XX ())]
roamTemplates = map (second (forkXio . roamInsert))
  [ ( "book-template"
    , [ "Author::"
      , "Found via::"
      , "Publication year:: #published-"
      , "Add date:: /today\n"
      , "First read date:: "
      , "Tags:: #book #unread"
      ])
  , ( "paper-template"
    , [ "Author::"
      , "Source::"
      , "Found via::"
      , "Publication year:: #published-"
      , "Add date:: /today\n"
      , "First read date:: "
      , "Tags:: #paper #unread"
      ])
  , ( "article-template"
    , [ "Author::"
      , "Source::"
      , "Found via::"
      , "Publication year:: #published-"
      , "Add date:: /today\n"
      , "First read date:: "
      , "Tags:: #article #unread"
      ])
  , ( "podcast-template"
    , [ "Podcast::"
      , "Interviewee::"
      , "Listen date:: /today\n"
      , "Tags:: #podcast"
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
      , "Add date:: /today\n"
      , "Tags:: #person"
      ])
  -- This is my equivalent of "permanent note"
  , ( "thought-template"
    , [ "Initial source::"
      , "Add date:: /today\n"
      , "Keywords:: #thought"
      , "Related Notes:: "
      ]
    )
  ]

roamInsert :: [String] -> Xio ()
roamInsert [] = return ()
roamInsert ls = forM_ ls $ \l -> do
  if lastMay l == Just '\n'
  then do
    xdotoolType (init l)
    liftIO $ threadDelay (1000 * 100)
    xdotoolKey "Return"
    liftIO $ threadDelay (1000 * 20)
  else do
    xdotoolType (l ++ " ")
  liftIO $ threadDelay (1000 * 100)
  xdotoolKey "Return"
  liftIO $ threadDelay (1000 * 100)

xdotoolType :: String -> Xio ()
xdotoolType text = syncSpawn "xdotool" ["type", "--delay", "20", text]

xdotoolKey :: String -> Xio ()
xdotoolKey key = syncSpawn "xdotool" ["key", key]
