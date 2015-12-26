{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module JavaScript.Cocos2d.Layer where

import Data.Colour
import Control.Monad
import GHCJS.Types
import GHCJS.Marshal
import JavaScript.Cocos2d.Class
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Types

class IsNode a => IsLayer a where
    toLayer :: a -> Layer

newtype Layer = Layer JSVal deriving (FromJSVal, ToJSVal)
instance IsNode Layer where
    toNode (Layer v) = Node v
instance IsLayer Layer where
    toLayer = id

createLayer :: Cocos2d m => m Layer
createLayer = liftIO cc_createLayer

newtype LayerColor = LayerColor JSVal deriving (FromJSVal, ToJSVal)
instance IsNode LayerColor where
    toNode (LayerColor v) = Node v
instance IsLayer LayerColor where
    toLayer (LayerColor v) = Layer v

createLayerColor :: Cocos2d m => AlphaColour Double -> m LayerColor
createLayerColor = liftIO <$> (cc_createLayerColor <=< toJSVal)

foreign import javascript unsafe "new cc.Layer()" cc_createLayer :: IO Layer
foreign import javascript unsafe "new cc.LayerColor($1)" cc_createLayerColor :: JSVal -> IO LayerColor
