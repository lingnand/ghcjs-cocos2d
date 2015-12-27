{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module JavaScript.Cocos2d.Node where

import Data.Word
import Data.Colour
import Data.Colour.Names
import Data.Default
import Linear
import Control.Monad
import Control.Lens
import GHCJS.Types
import GHCJS.Marshal.Internal
import GHCJS.Foreign.Callback
import GHCJS.Foreign.Internal
import JavaScript.Cocos2d.Class
import JavaScript.Cocos2d.Utils
import JavaScript.Cocos2d.Types

class IsNode a where
    toNode :: a -> Node

newtype Node = Node JSVal
instance IsNode Node where
    toNode = id

data NodeConfig
   = NodeConfig { _nodeConfig_pos :: V2 Double
                , _nodeConfig_width :: Double
                , _nodeConfig_height :: Double
                , _nodeConfig_anchor :: V2 Double
                , _nodeConfig_skew :: V2 Double
                , _nodeConfig_zIndex :: Int
                , _nodeConfig_rotation :: V2 Double
                , _nodeConfig_scale :: V2 Double
                , _nodeConfig_visible :: Bool
                , _nodeConfig_color :: Colour Double
                , _nodeConfig_cascadeColor :: Bool
                , _nodeConfig_opacity :: Double -- | 0.0 - 1.0
                , _nodeConfig_cascadeOpacity :: Bool
                } deriving (Show)
makeLenses ''NodeConfig

instance Default NodeConfig where
    def = NodeConfig { _nodeConfig_pos = zero
                     , _nodeConfig_width = 0.0
                     , _nodeConfig_height = 0.0
                     , _nodeConfig_anchor = zero
                     , _nodeConfig_skew = zero
                     , _nodeConfig_zIndex = 0
                     , _nodeConfig_rotation = zero
                     , _nodeConfig_scale = pure 1.0
                     , _nodeConfig_visible = False
                     , _nodeConfig_color = white
                     , _nodeConfig_cascadeColor = False
                     , _nodeConfig_opacity = 1.0 -- | 0.0 - 1.0
                     , _nodeConfig_cascadeOpacity = False
                     }

createNode :: Cocos2d m => m Node
createNode = liftIO cc_createNode

createNodeWithConfig :: Cocos2d m => NodeConfig -> m Node
createNodeWithConfig c = createNode >>= \n -> setConfig n c >> return n

-- we are not using attr() here because it can be quite inefficient
setConfig :: (Cocos2d m, IsNode n) => n -> NodeConfig -> m ()
setConfig n (NodeConfig (V2 px py) width height (V2 ax ay) (V2 skx sky) zIndex
          (V2 rx ry) (V2 slx sly) visible color cascadeColor opacity cascadeOpacity) = liftIO $ do
    let n' = toNode n
    cc_setX n' px >> cc_setY n' py
    cc_setWidth n' width >> cc_setHeight n' height
    cc_setAnchorX n' ax >> cc_setAnchorY n' ay
    cc_setSkewX n' skx >> cc_setSkewY n' sky
    cc_setZIndex n' zIndex
    cc_setRotationX n' rx >> cc_setRotationY n' ry
    cc_setScaleX n' slx >> cc_setScaleY n' sly
    cc_setVisible n' visible
    cc_setColor n' =<< toJSVal color
    cc_setCascadeColor n' cascadeColor
    cc_setOpacity n' (round $ opacity * 255)
    cc_setCascadeOpacity n' cascadeOpacity

getConfig :: (Cocos2d m, IsNode n) => n -> m NodeConfig
getConfig n = liftIO $ NodeConfig <$> (V2 <$> cc_getX n' <*> cc_getY n') <*> cc_getWidth n' <*> cc_getHeight n'
    <*> (V2 <$> cc_getAnchorX n' <*> cc_getAnchorY n') <*> (V2 <$> cc_getSkewX n' <*> cc_getSkewY n')
    <*> cc_getZIndex n' <*> (V2 <$> cc_getRotationX n' <*> cc_getRotationY n') <*> (V2 <$> cc_getScaleX n' <*> cc_getScaleY n')
    <*> cc_getVisible n' <*> (cc_getColor n' >>= fromJSValUnchecked) <*> cc_getCascadeColor n'
    <*> ((/255) . fromIntegral <$> cc_getOpacity n') <*> cc_getCascadeOpacity n'
        where n' = toNode n

modifyConfig :: (Cocos2d m, IsNode n) => n -> (NodeConfig -> NodeConfig) -> m ()
modifyConfig n f = getConfig n' >>= setConfig n' . f
        where n' = toNode n

setOnEnter :: (Cocos2d m, IsNode n) => n -> IO () -> m (IO ())
setOnEnter = convCallback . cc_setOnEnter . toNode

addChild :: (Cocos2d m, IsNode n, IsNode c) => n -> c -> m ()
addChild n c = liftIO $ cc_addChild (toNode n) (toNode c) jsUndefined

addChild' :: (Cocos2d m, IsNode n, IsNode c) => n -> c -> Int -> m ()
addChild' n c localZOrder = liftIO $ cc_addChild (toNode n) (toNode c) (pToJSVal localZOrder)

foreign import javascript unsafe "new cc.Node()"  cc_createNode :: IO Node
foreign import javascript unsafe "$1.onEnter = function() { cc.Node.prototype.onEnter.call(this); $2(); }"  cc_setOnEnter :: Node -> Callback a -> IO ()
foreign import javascript unsafe "$1.addChild($2, $3)" cc_addChild :: Node -> Node -> JSVal -> IO ()
foreign import javascript unsafe "if ($1.x !== $2) {$1.x = $2}" cc_setX :: Node -> Double -> IO ()
foreign import javascript unsafe "if ($1.y !== $2) {$1.y = $2}" cc_setY :: Node -> Double -> IO ()
foreign import javascript unsafe "if ($1.width !== $2) {$1.width = $2}" cc_setWidth :: Node -> Double -> IO ()
foreign import javascript unsafe "if ($1.height !== $2) {$1.height = $2}" cc_setHeight :: Node -> Double -> IO ()
foreign import javascript unsafe "if ($1.anchorX !== $2) {$1.anchorX = $2}" cc_setAnchorX :: Node -> Double -> IO ()
foreign import javascript unsafe "if ($1.anchorY !== $2) {$1.anchorY = $2}" cc_setAnchorY :: Node -> Double -> IO ()
foreign import javascript unsafe "if ($1.skewX !== $2) {$1.skewX = $2}" cc_setSkewX :: Node -> Double -> IO ()
foreign import javascript unsafe "if ($1.skewY !== $2) {$1.skewY = $2}" cc_setSkewY :: Node -> Double -> IO ()
foreign import javascript unsafe "if ($1.zIndex !== $2) {$1.zIndex = $2}" cc_setZIndex :: Node -> Int -> IO ()
foreign import javascript unsafe "if ($1.rotationX !== $2) {$1.rotationX = $2}" cc_setRotationX :: Node -> Double -> IO ()
foreign import javascript unsafe "if ($1.rotationY !== $2) {$1.rotationY = $2}" cc_setRotationY :: Node -> Double -> IO ()
foreign import javascript unsafe "if ($1.scaleX !== $2) {$1.scaleX = $2}" cc_setScaleX :: Node -> Double -> IO ()
foreign import javascript unsafe "if ($1.scaleY !== $2) {$1.scaleY = $2}" cc_setScaleY :: Node -> Double -> IO ()
foreign import javascript unsafe "if ($1.visible !== $2) {$1.visible = $2}" cc_setVisible :: Node -> Bool -> IO ()
foreign import javascript unsafe "if (!cc.colorEqual($1.color, $2)) {$1.color = $2}" cc_setColor :: Node -> JSVal -> IO ()
foreign import javascript unsafe "if ($1.cascadeColor !== $2) {$1.cascadeColor = $2}" cc_setCascadeColor :: Node -> Bool -> IO ()
foreign import javascript unsafe "if ($1.opacity !== $2) {$1.opacity = $2}" cc_setOpacity :: Node -> Word8 -> IO ()
foreign import javascript unsafe "if ($1.cascadeOpacity !== $2) {$1.cascadeOpacity = $2}" cc_setCascadeOpacity :: Node -> Bool -> IO ()
foreign import javascript unsafe "$1.x" cc_getX :: Node -> IO Double
foreign import javascript unsafe "$1.y" cc_getY :: Node -> IO Double
foreign import javascript unsafe "$1.width" cc_getWidth :: Node -> IO Double
foreign import javascript unsafe "$1.height" cc_getHeight :: Node -> IO Double
foreign import javascript unsafe "$1.anchorX" cc_getAnchorX :: Node -> IO Double
foreign import javascript unsafe "$1.anchorY" cc_getAnchorY :: Node -> IO Double
foreign import javascript unsafe "$1.skewX" cc_getSkewX :: Node -> IO Double
foreign import javascript unsafe "$1.skewY" cc_getSkewY :: Node -> IO Double
foreign import javascript unsafe "$1.zIndex" cc_getZIndex :: Node -> IO Int
foreign import javascript unsafe "$1.rotationX" cc_getRotationX :: Node -> IO Double
foreign import javascript unsafe "$1.rotationY" cc_getRotationY :: Node -> IO Double
foreign import javascript unsafe "$1.scaleX" cc_getScaleX :: Node -> IO Double
foreign import javascript unsafe "$1.scaleY" cc_getScaleY :: Node -> IO Double
foreign import javascript unsafe "$1.visible" cc_getVisible :: Node -> IO Bool
foreign import javascript unsafe "$1.color" cc_getColor :: Node -> IO JSVal
foreign import javascript unsafe "$1.cascadeColor" cc_getCascadeColor :: Node -> IO Bool
foreign import javascript unsafe "$1.opacity" cc_getOpacity :: Node -> IO Word8
foreign import javascript unsafe "$1.cascadeOpacity" cc_getCascadeOpacity :: Node -> IO Bool
