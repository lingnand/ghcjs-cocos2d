module JavaScript.Cocos2d.Layer where

import Data.Colour
import Control.Monad
import GHCJS.Types
import GHCJS.Marshal
import JavaScript.Cocos2d.Node

class IsNode a => IsLayer a where
    toLayer :: a -> Layer

newtype Layer = Layer JSVal
instance IsNode Layer where
    toNode (Layer v) = Node v
instance IsLayer Layer where
    toLayer = id

foreign import javascript unsafe "new cc.Layer()" createLayer :: IO Layer

newtype LayerColor = LayerColor JSVal
instance IsNode LayerColor where
    toNode (LayerColor v) = Node v
instance IsLayer LayerColor where
    toLayer (LayerColor v) = Layer v

createLayerColor :: Colour Double -> IO LayerColor
createLayerColor = cc_createLayerColor <=< toJSVal

foreign import javascript unsafe "new cc.LayerColor($1)" cc_createLayerColor :: JSVal -> IO LayerColor
