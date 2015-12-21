module JavaScript.Cocos2d.Event where

import GHCJS.Types

newtype Event = Event JSVal
newtype EventFocus = EventFocus JSVal
newtype EventMouse = EventMouse JSVal
newtype EventTouch = EventTouch JSVal
