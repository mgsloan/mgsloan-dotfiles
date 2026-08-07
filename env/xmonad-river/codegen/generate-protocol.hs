#!/usr/bin/env stack
-- stack runghc --package xml-conduit --package text --package containers --package directory --package filepath

-- | Generates Haskell bindings from Wayland protocol XML.
--
-- Run from the xmonad-river directory:
--
-- > ./codegen/generate-protocol.hs
--
-- The generated modules are checked in, so this only needs to be re-run when
-- the vendored protocol XML in @protocol/@ is updated from river. Generating
-- at build time was rejected deliberately: it would put a custom Setup.hs and
-- an xml parsing dependency in the path of every rebuild, and the M-q
-- recompile loop is the thing this project most needs to keep fast.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Char (isAlpha, toLower, toUpper)
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (fromMaybe, mapMaybe)
import System.FilePath ((</>), (<.>))
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.XML as X

--------------------------------------------------------------------------------
-- Protocol model

data Interface = Interface
  { ifaceName     :: String
  , ifaceVersion  :: Int
  , ifaceRequests :: [Message]
  , ifaceEvents   :: [Message]
  , ifaceEnums    :: [Enum']
  }

data Message = Message
  { msgName       :: String
  , msgOpcode     :: Int
  , msgArgs       :: [Argument]
  , msgDestructor :: Bool
  , msgSince      :: Maybe Int
  , msgSummary    :: String
  }

data Argument = Argument
  { argName      :: String
  , argType      :: ArgType
  , argNullable  :: Bool
  , argInterface :: Maybe String
  }

data ArgType = TInt | TUInt | TFixed | TString | TObject | TNewId | TArray | TFd
  deriving (Eq)

data Enum' = Enum'
  { enumName    :: String
  , enumEntries :: [(String, Integer)]
  }

--------------------------------------------------------------------------------
-- Parsing

parseProtocol :: FilePath -> IO [Interface]
parseProtocol path = do
  doc <- X.readFile X.def path
  pure $ map parseInterface (childrenNamed "interface" (X.documentRoot doc))

parseInterface :: X.Element -> Interface
parseInterface el = Interface
  { ifaceName     = attr "name" el
  , ifaceVersion  = read (attr "version" el)
  , ifaceRequests = zipWith parseMessage [0..] (childrenNamed "request" el)
  , ifaceEvents   = zipWith parseMessage [0..] (childrenNamed "event" el)
  , ifaceEnums    = map parseEnum (childrenNamed "enum" el)
  }

parseMessage :: Int -> X.Element -> Message
parseMessage opcode el = Message
  { msgName       = attr "name" el
  , msgOpcode     = opcode
  , msgArgs       = map parseArg (childrenNamed "arg" el)
  , msgDestructor = attrMaybe "type" el == Just "destructor"
  , msgSince      = read <$> attrMaybe "since" el
  , msgSummary    = maybe "" (attr "summary")
                      (listToMaybe' (childrenNamed "description" el))
  }

parseArg :: X.Element -> Argument
parseArg el = Argument
  { argName      = attr "name" el
  , argType      = case attr "type" el of
      "int"     -> TInt
      "uint"    -> TUInt
      "fixed"   -> TFixed
      "string"  -> TString
      "object"  -> TObject
      "new_id"  -> TNewId
      "array"   -> TArray
      "fd"      -> TFd
      t         -> error ("unknown arg type: " ++ t)
  , argNullable  = attrMaybe "allow-null" el == Just "true"
  , argInterface = attrMaybe "interface" el
  }

parseEnum :: X.Element -> Enum'
parseEnum el = Enum'
  { enumName    = attr "name" el
  , enumEntries =
      [ (attr "name" e, readValue (attr "value" e))
      | e <- childrenNamed "entry" el
      ]
  }
  where
    readValue s
      | "0x" `isPrefixOf` s = read s
      | otherwise           = read s

--------------------------------------------------------------------------------
-- XML helpers

childrenNamed :: T.Text -> X.Element -> [X.Element]
childrenNamed n el =
  [ e | X.NodeElement e <- X.elementNodes el, X.nameLocalName (X.elementName e) == n ]

attrMaybe :: T.Text -> X.Element -> Maybe String
attrMaybe n el = T.unpack <$> M.lookup (X.Name n Nothing Nothing) (X.elementAttributes el)

attr :: T.Text -> X.Element -> String
attr n el = fromMaybe (error ("missing attribute " ++ T.unpack n)) (attrMaybe n el)

listToMaybe' :: [a] -> Maybe a
listToMaybe' = \case { (x:_) -> Just x; [] -> Nothing }

--------------------------------------------------------------------------------
-- Naming

-- | @river_window_v1@ becomes @RiverWindowV1@.
typeName :: String -> String
typeName = concatMap capitalise . splitOn '_'
  where capitalise = \case { (c:cs) -> toUpper c : cs; [] -> [] }

-- | @river_window_v1@ becomes @riverWindowV1@.
funName :: String -> String
funName s = case typeName s of
  (c:cs) -> toLower c : cs
  []     -> []

-- | Avoids colliding with Haskell keywords and Prelude names.
safeVar :: String -> String
safeVar n
  | n `elem` reserved = n ++ "_"
  | otherwise         = lowerFirst (concatMap capitalise (zip [0 :: Int ..] (splitOn '_' n)))
  where
    reserved = ["type", "class", "data", "where", "id", "min", "max", "then", "else"]
    capitalise (0, w) = w
    capitalise (_, w) = case w of { (c:cs) -> toUpper c : cs; [] -> [] }
    lowerFirst = \case { (c:cs) -> toLower c : cs; [] -> [] }

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (w, [])      -> [w]
  (w, _:rest)  -> w : splitOn c rest

--------------------------------------------------------------------------------
-- Type and codec mapping

haskellType :: Argument -> String
haskellType a = case argType a of
  TInt    -> "Int32"
  TUInt   -> "Word32"
  TFixed  -> "Fixed"
  TString -> if argNullable a then "(Maybe ByteString)" else "ByteString"
  TObject -> "ObjectId"
  TNewId  -> "ObjectId"
  TArray  -> "ByteString"
  TFd     -> error "file descriptor arguments are not supported"

-- | How a request argument is encoded.
argEncoder :: Argument -> String
argEncoder a = case argType a of
  TInt    -> "argInt " ++ v
  TUInt   -> "argUInt " ++ v
  TFixed  -> "argFixed " ++ v
  TString -> if argNullable a then "argString " ++ v else "argString (Just " ++ v ++ ")"
  TObject -> "argObject " ++ v
  TNewId  -> "argObject " ++ v
  TArray  -> "argArray " ++ v
  TFd     -> error "file descriptor arguments are not supported"
  where v = safeVar (argName a)

-- | How an event argument is decoded.
argDecoder :: Argument -> String
argDecoder a = case argType a of
  TInt    -> "getInt"
  TUInt   -> "getWord32"
  TFixed  -> "getFixed"
  TString -> if argNullable a then "getStringMaybe" else "getString"
  TObject -> "getObject"
  TNewId  -> "getObject"
  TArray  -> "getArray"
  TFd     -> error "file descriptor arguments are not supported"

hasFd :: Message -> Bool
hasFd = any ((== TFd) . argType) . msgArgs

--------------------------------------------------------------------------------
-- Rendering

renderModule :: String -> String -> [Interface] -> String
renderModule modName source ifaces = unlines $
  [ "-- | Bindings for @" ++ source ++ "@."
  , "--"
  , "-- Generated by @codegen/generate-protocol.hs@. Do not edit by hand."
  , "{-# LANGUAGE OverloadedStrings #-}"
  , "module " ++ modName
  , "  ("
  ] ++
  prefixFirst "    " "  , " (concatMap ifaceExports ifaces) ++
  [ "  ) where"
  , ""
  ] ++ imports ++
  [ ""
  , "import XMonad.River.Connection"
  , "import XMonad.River.Wire"
  , ""
  ] ++
  concatMap renderInterface ifaces
  where
    allArgs = [ a | i <- ifaces, m <- ifaceRequests i ++ ifaceEvents i, a <- msgArgs m ]
    uses t = any ((== t) . argType) allArgs
    -- ByteString and Word16 are always needed: every interface gets an
    -- Unknown event constructor carrying the raw body and opcode, and every
    -- interface exports its name as a ByteString.
    imports = concat
      [ [ "import Data.ByteString (ByteString)" ]
      , [ "import Data.Int (Int32)" | uses TInt ]
      , [ "import Data.Word (Word16, Word32)" ]
      ]

-- | Renders a list with one prefix for the first element and another for the
-- rest, which is how Haskell export and import lists are laid out.
prefixFirst :: String -> String -> [String] -> [String]
prefixFirst _ _ [] = []
prefixFirst p1 pn (x:xs) = (p1 ++ x) : map (pn ++) xs

ifaceExports :: Interface -> [String]
ifaceExports i =
  [ typeName (ifaceName i) ++ "Event(..)"
  , funName (ifaceName i) ++ "Interface"
  , funName (ifaceName i) ++ "Version"
  , funName (ifaceName i) ++ "Listen"
  ] ++
  [ funName (ifaceName i) ++ typeName (msgName m)
  | m <- ifaceRequests i, not (hasFd m)
  ] ++
  [ enumConstName i e n | e <- ifaceEnums i, (n, _) <- enumEntries e ]

enumConstName :: Interface -> Enum' -> String -> String
enumConstName i e n =
  funName (ifaceName i) ++ typeName (enumName e) ++ typeName n

renderInterface :: Interface -> [String]
renderInterface i =
  [ sectionRule
  , "-- " ++ ifaceName i ++ " (version " ++ show (ifaceVersion i) ++ ")"
  , sectionRule
  , ""
  , "-- | The interface name, as advertised by @wl_registry@."
  , funName (ifaceName i) ++ "Interface :: ByteString"
  , funName (ifaceName i) ++ "Interface = \"" ++ ifaceName i ++ "\""
  , ""
  , "-- | The highest version these bindings were generated against."
  , funName (ifaceName i) ++ "Version :: Word32"
  , funName (ifaceName i) ++ "Version = " ++ show (ifaceVersion i)
  , ""
  ] ++
  concatMap (renderEnum i) (ifaceEnums i) ++
  concatMap (renderRequest i) (ifaceRequests i) ++
  renderEventType i ++
  renderListener i

sectionRule :: String
sectionRule = "--------------------------------------------------------------------------------"

renderEnum :: Interface -> Enum' -> [String]
renderEnum i e = concat
  [ [ "-- | @" ++ ifaceName i ++ "." ++ enumName e ++ "." ++ n ++ "@"
    , nm ++ " :: Word32"
    , nm ++ " = " ++ show v
    , ""
    ]
  | (n, v) <- enumEntries e
  , let nm = enumConstName i e n
  ]

renderRequest :: Interface -> Message -> [String]
renderRequest i m
  | hasFd m =
      [ "-- NOTE: " ++ ifaceName i ++ "." ++ msgName m ++ " is not generated:"
      , "-- it passes a file descriptor, which this pure-Haskell wire"
      , "-- implementation does not support (it would need SCM_RIGHTS)."
      , ""
      ]
  | otherwise =
      [ "-- | @" ++ ifaceName i ++ "." ++ msgName m ++ "@"
      ] ++
      sinceComment ++
      [ name ++ " :: Connection -> ObjectId" ++ concatMap ((" -> " ++) . haskellType) plainArgs
          ++ " -> IO " ++ resultType
      , name ++ " conn self" ++ concatMap ((' ' :) . safeVar . argName) plainArgs ++ " ="
      ] ++
      body ++
      [ "" ]
  where
    name = funName (ifaceName i) ++ typeName (msgName m)
    -- A new_id argument in a request is allocated by us and returned, rather
    -- than being taken as a parameter.
    newIdArgs = [ a | a <- msgArgs m, argType a == TNewId ]
    plainArgs = [ a | a <- msgArgs m, argType a /= TNewId ]
    resultType = case newIdArgs of
      []  -> "()"
      _   -> "ObjectId"
    sinceComment = case msgSince m of
      Nothing -> []
      Just s  -> [ "-- Since version " ++ show s ++ "." ]
    encoded = case msgArgs m of
      [] -> "mempty"
      as -> intercalate " <> " (map argEncoder as)
    -- A destructor request also drops the local listener, so that events the
    -- server had already queued for the object are discarded rather than
    -- delivered to a handler that no longer has any state to update.
    body = case newIdArgs of
      [] ->
        [ "  request conn self " ++ show (msgOpcode m) ++ " (" ++ encoded ++ ")" ]
        ++ [ "    >> freeObject conn self" | msgDestructor m ]
      (nid:_) ->
        [ "  do"
        , "    " ++ safeVar (argName nid) ++ " <- newObject conn"
        , "    request conn self " ++ show (msgOpcode m) ++ " (" ++ encoded ++ ")"
        , "    pure " ++ safeVar (argName nid)
        ]

renderEventType :: Interface -> [String]
renderEventType i =
  [ "-- | Events delivered to a @" ++ ifaceName i ++ "@."
  , "data " ++ tn ++ "Event"
  ] ++
  zipWith renderCon [0 :: Int ..] (ifaceEvents i) ++
  [ sep (length (ifaceEvents i)) ++ tn ++ "Unknown !Word16 !ByteString"
  , "    -- ^ An event this build does not know about, from a server speaking"
  , "    -- a newer version of the protocol. Ignoring these is what keeps a"
  , "    -- client forward compatible."
  , "  deriving (Eq, Show)"
  , ""
  ]
  where
    tn = typeName (ifaceName i)
    sep 0 = "  = "
    sep _ = "  | "
    renderCon n m =
      sep n ++ tn ++ typeName (msgName m)
        ++ concatMap ((" !" ++) . haskellType) (msgArgs m)

renderListener :: Interface -> [String]
renderListener i =
  [ "-- | Attach an event handler to a @" ++ ifaceName i ++ "@ object."
  , name ++ " :: Connection -> ObjectId -> (" ++ tn ++ "Event -> IO ()) -> IO ()"
  , name ++ " conn self handler ="
  , "  setListener conn self $ \\opcode body -> case opcode of"
  ] ++
  concatMap renderCase (ifaceEvents i) ++
  [ "    _ -> handler (" ++ tn ++ "Unknown opcode body)"
  , ""
  ]
  where
    tn = typeName (ifaceName i)
    name = funName (ifaceName i) ++ "Listen"
    renderCase m = case msgArgs m of
      [] ->
        [ "    " ++ show (msgOpcode m) ++ " -> handler " ++ tn ++ typeName (msgName m) ]
      as ->
        [ "    " ++ show (msgOpcode m) ++ " ->"
        , "      handler =<< decode (" ++ applicative as ++ ") body"
        ]
      where
        applicative as' =
          tn ++ typeName (msgName m) ++ " <$> "
            ++ intercalate " <*> " (map argDecoder as')

--------------------------------------------------------------------------------
-- Main

-- | Which protocol files to generate, and what to call the resulting modules.
targets :: [(FilePath, String)]
targets =
  [ ("river-window-management-v1.xml", "XMonad.River.Protocol.WindowManagement")
  , ("river-xkb-bindings-v1.xml",      "XMonad.River.Protocol.XkbBindings")
  , ("river-layer-shell-v1.xml",       "XMonad.River.Protocol.LayerShell")
  ]

main :: IO ()
main = mapM_ generate targets
  where
    generate (xmlFile, modName) = do
      ifaces <- parseProtocol ("protocol" </> xmlFile)
      let out = "src" </> map (\c -> if c == '.' then '/' else c) modName <.> "hs"
      writeFile out (renderModule modName xmlFile ifaces)
      putStrLn ("wrote " ++ out ++ " (" ++ show (length ifaces) ++ " interfaces)")
