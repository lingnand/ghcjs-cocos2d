module JavaScript.Cocos2d.Node where

import Control.Monad
import GHCJS.Types
import GHCJS.Marshal

class ToJSVal a => IsNode a where
    toNode :: a -> IO Node
    toNode = fromJSValUnchecked <=< toJSVal

newtype Node = Node JSVal deriving (FromJSVal, ToJSVal)
instance IsNode Node where
    toNode = pure

foreign import javascript unsafe "new cc.Node()"  createNode :: IO Node
