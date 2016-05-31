{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module JavaScript.Cocos2d.Layer where

import Data.Colour
import Control.Monad.IO.Class
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Utils

class IsNode a => IsLayer a where
    toLayer :: a -> Layer

newtype Layer = Layer JSVal deriving (PFromJSVal, PToJSVal)
instance IsNode Layer where
    toNode (Layer v) = pFromJSVal v
instance IsLayer Layer where
    toLayer = id

instance Eq Layer where
    Layer a == Layer b = js_eq a b

createLayer :: MonadIO m => m Layer
createLayer = liftIO cc_createLayer

newtype LayerColor = LayerColor JSVal deriving (PFromJSVal, PToJSVal)
instance IsNode LayerColor where
    toNode (LayerColor v) = pFromJSVal v
instance IsLayer LayerColor where
    toLayer (LayerColor v) = Layer v

instance Eq LayerColor where
    LayerColor a == LayerColor b = js_eq a b

createLayerColor :: MonadIO m => m LayerColor
createLayerColor = liftIO cc_createLayerColor

createLayerColor' :: MonadIO m => Colour Double -> m LayerColor
createLayerColor' c = liftIO $ cc_createLayerColor' =<< toJSVal c

foreign import javascript unsafe "new cc.Layer()" cc_createLayer :: IO Layer
foreign import javascript unsafe "new cc.LayerColor()" cc_createLayerColor :: IO LayerColor
foreign import javascript unsafe "new cc.LayerColor($1)" cc_createLayerColor' :: JSVal -> IO LayerColor
