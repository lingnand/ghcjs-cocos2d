{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Actions are (mostly) opaque objects containing logic about mutating the behavior of nodes.
-- Mostly this should be avoided in favor of modulating the input via Events, but sometimes (for
-- example loading from an external ccs), it's more convenient to wrap them in actions.

module JavaScript.Cocos2d.Action
  (
    Action
  ) where

import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Marshal.Pure

newtype Action = Action JSVal deriving (PFromJSVal, PToJSVal, FromJSVal, ToJSVal)
