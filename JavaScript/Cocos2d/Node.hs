{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module JavaScript.Cocos2d.Node where

import Control.Monad
import Data.Word
import Data.Colour
import GHCJS.Types
import GHCJS.Marshal.Internal
import GHCJS.Foreign.Callback
import JavaScript.Cocos2d.Types()
import JavaScript.Cocos2d.Utils

class IsNode a where
    toNode :: a -> Node

newtype Node = Node JSVal
instance IsNode Node where
    toNode = id

foreign import javascript unsafe "new cc.Node()"  createNode :: IO Node

setOnEnter :: IsNode n => n -> IO () -> IO (IO ())
setOnEnter = convCallback . cc_setOnEnter . toNode

addChild :: (IsNode n, IsNode c) => n -> c -> IO ()
addChild n c = cc_addChild (toNode n) (toNode c)

addChild' :: (IsNode n, IsNode c) => n -> c -> Int -> IO ()
addChild' n c localZOrder = cc_addChild' (toNode n) (toNode c) localZOrder

setX :: IsNode n => n -> Double -> IO ()
setX = cc_setX . toNode

setY :: IsNode n => n -> Double -> IO ()
setY = cc_setY . toNode

setWidth :: IsNode n => n -> Double -> IO ()
setWidth = cc_setWidth . toNode

setHeight :: IsNode n => n -> Double -> IO ()
setHeight = cc_setHeight . toNode

setAnchorX :: IsNode n => n -> Double -> IO ()
setAnchorX = cc_setAnchorX . toNode

setAnchorY :: IsNode n => n -> Double -> IO ()
setAnchorY = cc_setAnchorY . toNode

setSkewX :: IsNode n => n -> Double -> IO ()
setSkewX = cc_setSkewX . toNode

setSkewY :: IsNode n => n -> Double -> IO ()
setSkewY = cc_setSkewY . toNode

setZIndex :: IsNode n => n -> Int -> IO ()
setZIndex = cc_setZIndex . toNode

setRotationX :: IsNode n => n -> Double -> IO ()
setRotationX = cc_setRotationX . toNode

setRotationY :: IsNode n => n -> Double -> IO ()
setRotationY = cc_setRotationY . toNode

setScaleX :: IsNode n => n -> Double -> IO ()
setScaleX = cc_setScaleX . toNode

setScaleY :: IsNode n => n -> Double -> IO ()
setScaleY = cc_setScaleY . toNode

setVisible :: IsNode n => n -> Bool -> IO ()
setVisible = cc_setVisible . toNode

setColor :: IsNode n => n -> Colour Double -> IO ()
setColor n = cc_setColor (toNode n) <=< toJSVal

setCascadeColor :: IsNode n => n -> Bool -> IO ()
setCascadeColor = cc_setCascadeColor . toNode

setOpacity :: IsNode n => n -> Double -> IO ()
setOpacity n o = cc_setOpacity (toNode n) (round $ o * 255)

setCascadeOpacity :: IsNode n => n -> Bool -> IO ()
setCascadeOpacity = cc_setCascadeOpacity . toNode

getX :: IsNode n => n -> IO Double
getX = cc_getX . toNode

getY :: IsNode n => n -> IO Double
getY = cc_getY . toNode

getWidth :: IsNode n => n -> IO Double
getWidth = cc_getWidth . toNode

getHeight :: IsNode n => n -> IO Double
getHeight = cc_getHeight . toNode

getAnchorX :: IsNode n => n -> IO Double
getAnchorX = cc_getAnchorX . toNode

getAnchorY :: IsNode n => n -> IO Double
getAnchorY = cc_getAnchorY . toNode

getSkewX :: IsNode n => n -> IO Double
getSkewX = cc_getSkewX . toNode

getSkewY :: IsNode n => n -> IO Double
getSkewY = cc_getSkewY . toNode

getZIndex :: IsNode n => n -> IO Int
getZIndex = cc_getZIndex . toNode

getRotationX :: IsNode n => n -> IO Double
getRotationX = cc_getRotationX . toNode

getRotationY :: IsNode n => n -> IO Double
getRotationY = cc_getRotationY . toNode

getScaleX :: IsNode n => n -> IO Double
getScaleX = cc_getScaleX . toNode

getScaleY :: IsNode n => n -> IO Double
getScaleY = cc_getScaleY . toNode

getVisible :: IsNode n => n -> IO Bool
getVisible = cc_getVisible . toNode

getColor :: IsNode n => n -> IO (Colour Double)
getColor = fromJSValUnchecked <=< cc_getColor . toNode

getCascadeColor :: IsNode n => n -> IO Bool
getCascadeColor = cc_getCascadeColor . toNode

getOpacity :: IsNode n => n -> IO Double
getOpacity = ((/255) . fromIntegral <$>) . cc_getOpacity . toNode

getCascadeOpacity :: IsNode n => n -> IO Bool
getCascadeOpacity = cc_getCascadeOpacity . toNode

foreign import javascript unsafe "$1.onEnter = function() { cc.Node.prototype.onEnter.call(this); $2(); }"  cc_setOnEnter :: Node -> Callback a -> IO ()
foreign import javascript unsafe "$1.addChild($2)" cc_addChild :: Node -> Node -> IO ()
foreign import javascript unsafe "$1.addChild($2, $3)" cc_addChild' :: Node -> Node -> Int -> IO ()
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
