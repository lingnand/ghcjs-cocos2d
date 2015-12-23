module JavaScript.Cocos2d.Layer where

import Control.Monad
import GHCJS.Types
import GHCJS.Marshal
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Types

class IsNode a => IsLayer a where
    toLayer :: a -> IO Layer
    toLayer = fromJSValUnchecked <=< toJSVal

newtype Layer = Layer JSVal deriving (FromJSVal, ToJSVal)
instance IsNode Layer where
instance IsLayer Layer where
    toLayer = pure

foreign import javascript unsafe "new cc.Layer()" createLayer :: IO Layer

newtype LayerColor = LayerColor JSVal deriving (FromJSVal, ToJSVal)
instance IsNode LayerColor where
instance IsLayer LayerColor where

createLayerColor :: Color -> Double -> Double -> IO LayerColor
createLayerColor c width height = toJSVal c >>= \v -> cc_createLayerColor v width height

foreign import javascript unsafe "new cc.LayerColor($1, $2, $3)" cc_createLayerColor :: JSVal -> Double -> Double -> IO LayerColor
