{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module JavaScript.Cocos2d.Event where

import GHCJS.Types
import GHCJS.Marshal

newtype Event = Event JSVal deriving (FromJSVal, ToJSVal)
newtype EventFocus = EventFocus JSVal deriving (FromJSVal, ToJSVal)
newtype EventMouse = EventMouse JSVal deriving (FromJSVal, ToJSVal)
newtype EventTouch = EventTouch JSVal deriving (FromJSVal, ToJSVal)
