module JavaScript.Cocos2d.Widget where

import GHCJS.Types
import GHCJS.Marshal

newtype Widget = Widget JSVal deriving (FromJSVal, ToJSVal)
