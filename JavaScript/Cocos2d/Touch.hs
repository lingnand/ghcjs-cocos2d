{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module JavaScript.Cocos2d.Touch where

import GHCJS.Types
import GHCJS.Marshal

newtype Touch = Touch JSVal deriving (FromJSVal, ToJSVal)
