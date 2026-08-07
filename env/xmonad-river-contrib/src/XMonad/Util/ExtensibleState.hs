-- | Persistent, typed state for contrib modules and configs.
--
-- Mirrors @XMonad.Util.ExtensibleState@. The 'PersistentExtension' variant is
-- accepted and behaves like the plain one for now: xmonad persists such values
-- across a restart by serialising them into the resume state it hands the new
-- process, and river's restart path (stop, then exec) has no equivalent
-- channel yet.
module XMonad.Util.ExtensibleState
  ( get
  , gets
  , put
  , modify
  , modify'
  , remove
  ) where

import Data.Maybe (fromMaybe)
import Data.Typeable (cast, typeOf)
import qualified Data.Map.Strict as M
import qualified Control.Monad.State as State

import XMonad.Core

-- | Key under which a given extension type is stored.
keyOf :: forall a. ExtensionClass a => a -> String
keyOf = show . typeOf

get :: forall a. ExtensionClass a => X a
get = do
  st <- State.gets extensibleState
  pure $ case M.lookup (keyOf (initialValue :: a)) st of
    Just (Right (StateExtension val))      -> fromMaybe initialValue (cast val)
    Just (Right (PersistentExtension val)) -> fromMaybe initialValue (cast val)
    -- A Left holds a value that was serialised but not yet read back. Nothing
    -- writes one today, since river's restart has no resume channel.
    Just (Left _)                          -> initialValue
    Nothing                                -> initialValue

gets :: ExtensionClass a => (a -> b) -> X b
gets f = f <$> get

put :: ExtensionClass a => a -> X ()
put v = State.modify $ \st -> st
  { extensibleState =
      M.insert (keyOf v) (Right (extensionType v)) (extensibleState st) }

modify :: ExtensionClass a => (a -> a) -> X ()
modify f = put . f =<< get

modify' :: ExtensionClass a => (a -> a) -> X ()
modify' f = (\v -> v `seq` put v) . f =<< get

remove :: forall a. ExtensionClass a => a -> X ()
remove v = State.modify $ \st -> st
  { extensibleState = M.delete (keyOf v) (extensibleState st) }
