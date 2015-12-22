module JavaScript.Cocos2d.Event where

import GHCJS.Types
import GHCJS.Marshal

newtype Event = Event JSVal deriving (FromJSVal, ToJSVal)
newtype FocusEvent = FocusEvent JSVal deriving (FromJSVal, ToJSVal)
newtype TouchEvent = TouchEvent JSVal deriving (FromJSVal, ToJSVal)
newtype MouseEvent = MouseEvent JSVal deriving (FromJSVal, ToJSVal)
newtype KeyboardEvent = KeyboardEvent JSVal deriving (FromJSVal, ToJSVal)
newtype AccelerationEvent = AccelerationEvent JSVal deriving (FromJSVal, ToJSVal)
