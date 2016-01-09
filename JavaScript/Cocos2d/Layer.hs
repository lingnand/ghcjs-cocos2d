module JavaScript.Cocos2d.Layer where

import Data.Colour
import Control.Monad.IO.Class
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

createLayer :: MonadIO m => m Layer
createLayer = liftIO cc_createLayer

newtype LayerColor = LayerColor JSVal
instance IsNode LayerColor where
    toNode (LayerColor v) = Node v
instance IsLayer LayerColor where
    toLayer (LayerColor v) = Layer v

createLayerColor :: MonadIO m => m LayerColor
createLayerColor = liftIO cc_createLayerColor

createLayerColor' :: MonadIO m => Colour Double -> m LayerColor
createLayerColor' c = liftIO $ cc_createLayerColor' =<< toJSVal c

foreign import javascript unsafe "new cc.Layer()" cc_createLayer :: IO Layer
foreign import javascript unsafe "new cc.LayerColor()" cc_createLayerColor :: IO LayerColor
foreign import javascript unsafe "new cc.LayerColor($1)" cc_createLayerColor' :: JSVal -> IO LayerColor
