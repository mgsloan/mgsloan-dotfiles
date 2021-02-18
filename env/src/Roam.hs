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
  , ( "talk-template"
    , [ "Author::"
      , "Source::"
      , "Found via::"
      , "Publication year:: #published-"
      , "Add date:: /today\n"
      , "First watch date:: "
      , "Tags:: #talk #unwatched"
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
    {-
  , ( "weekly-review"
    , [ "#weekly-review "
      , "\t## What's going on?"
      , "## What did I learn?"
      , "## How am I doing on #current-priorities ?"
      , "## What's the plan?"
      , "\t**Reminder**: Scan through `soon` label, resolve open loops"
      , "\r## What have I done to make this plan happen?"
      ]
    )
    -}
  ]

roamInsert :: [String] -> Xio ()
roamInsert ls = mapM_ (roamLine . (++ "\n")) ls

roamLine :: String -> Xio ()
roamLine [] = return ()
roamLine ('\t' : xs) = do
  liftIO $ threadDelay (1000 * 100)
  xdotoolKey "Tab"
  liftIO $ threadDelay (1000 * 100)
  roamLine xs
roamLine ('\r' : xs) = do
  -- Hack alert: using \r to represent shift+tab
  liftIO $ threadDelay (1000 * 100)
  xdotoolShiftTab
  liftIO $ threadDelay (1000 * 100)
  roamLine xs
roamLine ('\n' : xs) = do
  liftIO $ threadDelay (1000 * 100)
  xdotoolKey "Return"
  liftIO $ threadDelay (1000 * 100)
  roamLine xs
roamLine ('#' : '#' : ' ' : xs) = do
  liftIO $ threadDelay (1000 * 100)
  xdotoolType "## "
  liftIO $ threadDelay (1000 * 200)
  roamLine xs
roamLine xs = do
  let (contiguous, rest) = break (`elem` ("\t\n" :: String)) xs
  xdotoolType contiguous
  roamLine rest

xdotoolType :: String -> Xio ()
xdotoolType text = syncSpawn "xdotool" ["type", "--delay", "20", text]

xdotoolKey :: String -> Xio ()
xdotoolKey key = syncSpawn "xdotool" ["key", key]

xdotoolShiftTab :: Xio ()
xdotoolShiftTab = do
  syncSpawn "xdotool" ["keydown", "shift", "key", "Tab"]
  liftIO $ threadDelay (1000 * 100)
  syncSpawn "xdotool" ["keyup", "shift"]
