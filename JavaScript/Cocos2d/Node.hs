{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module JavaScript.Cocos2d.Node
    (
      IsNode(..)
    , Node
    , createNode
    , setOnEnter
    , addChild
    , addChild'
    , removeChild
    , removeAllChildren
    , setX
    , setY
    , setPosition
    , setWidth
    , setHeight
    , setSize
    , setAnchorX
    , setAnchorY
    , setAnchor
    , setSkewX
    , setSkewY
    , setSkew
    , setZIndex
    , setRotation
    , setScaleX
    , setScaleY
    , setScale
    , setVisible
    , setColor
    , setCascadeColor
    , setOpacity
    , setCascadeOpacity
    , getX
    , getY
    , getPosition
    , getWidth
    , getHeight
    , getSize
    , getAnchorX
    , getAnchorY
    , getAnchor
    , getSkewX
    , getSkewY
    , getSkew
    , getZIndex
    , getRotation
    , getScaleX
    , getScaleY
    , getScale
    , getVisible
    , getColor
    , getCascadeColor
    , getOpacity
    , getCascadeOpacity
    , getChildByName
    , convertToNodeSpace
    , convertToWorldSpace
    , runAction
    , castNode
    ) where

import Diagrams (V2(..), P2, unp2, (^&))
import Data.Word
import Data.Colour
import Control.Monad.IO.Class
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Foreign.Callback
import JavaScript.Cocos2d.Types()
import JavaScript.Cocos2d.Utils
import JavaScript.Cocos2d.Action

class IsNode a where
    toNode :: a -> Node

newtype Node = Node JSVal deriving (PToJSVal, PFromJSVal)
instance IsNode Node where
    toNode = id

instance Eq Node where
    Node a == Node b = js_eq a b

createNode :: MonadIO m => m Node
createNode = liftIO cc_createNode

setOnEnter :: (IsNode n, MonadIO m) => n -> IO () -> m (IO ())
setOnEnter n = liftIO . convCallback (cc_setOnEnter (toNode n))

addChild :: (IsNode n, IsNode c, MonadIO m) => n -> c -> m ()
addChild n c = liftIO $ cc_addChild (toNode n) (toNode c)

addChild' :: (IsNode n, IsNode c, MonadIO m) => n -> c -> Int -> m ()
addChild' n c localZOrder = liftIO $ cc_addChild' (toNode n) (toNode c) localZOrder

removeChild :: (IsNode n, IsNode c, MonadIO m) => n -> c -> m ()
removeChild n c = liftIO $ cc_removeChild (toNode n) (toNode c)

removeAllChildren :: (IsNode n, MonadIO m) => n -> m()
removeAllChildren = liftIO . cc_removeAllChildren . toNode

setX :: (IsNode n, MonadIO m) => n -> Double -> m ()
setX n = liftIO . cc_setX (toNode n)

setY :: (IsNode n, MonadIO m) => n -> Double -> m ()
setY n = liftIO . cc_setY (toNode n)

setPosition :: (IsNode n, MonadIO m) => n -> P2 Double -> m ()
setPosition n p = liftIO $ cc_setPosition (toNode n) x y
  where (x, y) = unp2 p

setWidth :: (IsNode n, MonadIO m) => n -> Double -> m ()
setWidth n = liftIO . cc_setWidth (toNode n)

setHeight :: (IsNode n, MonadIO m) => n -> Double -> m ()
setHeight n = liftIO . cc_setHeight (toNode n)

setSize :: (IsNode n, MonadIO m) => n -> V2 Double -> m ()
setSize n (V2 x y) = liftIO $ cc_setContentSize (toNode n) x y

setAnchorX :: (IsNode n, MonadIO m) => n -> Double -> m ()
setAnchorX n = liftIO . cc_setAnchorX (toNode n)

setAnchorY :: (IsNode n, MonadIO m) => n -> Double -> m ()
setAnchorY n = liftIO . cc_setAnchorY (toNode n)

setAnchor :: (IsNode n, MonadIO m) => n -> V2 Double -> m ()
setAnchor n (V2 x y) = liftIO $ cc_setAnchor (toNode n) x y

setSkewX :: (IsNode n, MonadIO m) => n -> Double -> m ()
setSkewX n = liftIO . cc_setSkewX (toNode n)

setSkewY :: (IsNode n, MonadIO m) => n -> Double -> m ()
setSkewY n = liftIO . cc_setSkewY (toNode n)

setSkew :: (IsNode n, MonadIO m) => n -> V2 Double -> m ()
setSkew n (V2 x y) = liftIO $ cc_setSkewX n' x >> cc_setSkewY n' y
  where n' = toNode n

setZIndex :: (IsNode n, MonadIO m) => n -> Int -> m ()
setZIndex n = liftIO . cc_setZIndex (toNode n)

setRotation :: (IsNode n, MonadIO m) => n -> Double -> m ()
setRotation n = liftIO . cc_setRotation (toNode n)

setScaleX :: (IsNode n, MonadIO m) => n -> Double -> m ()
setScaleX n = liftIO . cc_setScaleX (toNode n)

setScaleY :: (IsNode n, MonadIO m) => n -> Double -> m ()
setScaleY n = liftIO . cc_setScaleY (toNode n)

setScale :: (IsNode n, MonadIO m) => n -> V2 Double -> m ()
setScale n (V2 x y) = liftIO $ cc_setScale (toNode n) x y

setVisible :: (IsNode n, MonadIO m) => n -> Bool -> m ()
setVisible n = liftIO . cc_setVisible (toNode n)

setColor :: (IsNode n, MonadIO m) => n -> Colour Double -> m ()
setColor n c = liftIO $ cc_setColor (toNode n) =<< toJSVal c

setCascadeColor :: (IsNode n, MonadIO m) => n -> Bool -> m ()
setCascadeColor n = liftIO . cc_setCascadeColor (toNode n)

setOpacity :: (IsNode n, MonadIO m) => n -> Double -> m ()
setOpacity n o = liftIO $ cc_setOpacity (toNode n) (round $ o * 255)

setCascadeOpacity :: (IsNode n, MonadIO m) => n -> Bool -> m ()
setCascadeOpacity n = liftIO . cc_setCascadeOpacity (toNode n)

getX :: (IsNode n, MonadIO m) => n -> m Double
getX = liftIO . cc_getX . toNode

getY :: (IsNode n, MonadIO m) => n -> m Double
getY = liftIO . cc_getY . toNode

getPosition :: (IsNode n, MonadIO m) => n -> m (P2 Double)
getPosition n =  liftIO $ (^&) <$> cc_getX n' <*> cc_getY n'
  where n' = toNode n

getWidth :: (IsNode n, MonadIO m) => n -> m Double
getWidth = liftIO . cc_getWidth . toNode

getHeight :: (IsNode n, MonadIO m) => n -> m Double
getHeight = liftIO . cc_getHeight . toNode

getSize :: (IsNode n, MonadIO m) => n -> m (V2 Double)
getSize n =  liftIO $ (^&) <$> cc_getWidth n' <*> cc_getHeight n'
  where n' = toNode n

getAnchorX :: (IsNode n, MonadIO m) => n -> m Double
getAnchorX = liftIO . cc_getAnchorX . toNode

getAnchorY :: (IsNode n, MonadIO m) => n -> m Double
getAnchorY = liftIO . cc_getAnchorY . toNode

getAnchor :: (IsNode n, MonadIO m) => n -> m (V2 Double)
getAnchor n = liftIO $ (^&) <$> cc_getAnchorX n' <*> cc_getAnchorY n'
  where n' = toNode n

getSkewX :: (IsNode n, MonadIO m) => n -> m Double
getSkewX = liftIO . cc_getSkewX . toNode

getSkewY :: (IsNode n, MonadIO m) => n -> m Double
getSkewY = liftIO . cc_getSkewY . toNode

getSkew :: (IsNode n, MonadIO m) => n -> m (V2 Double)
getSkew n = liftIO $ (^&) <$> cc_getSkewX n' <*> cc_getSkewY n'
  where n' = toNode n

getZIndex :: (IsNode n, MonadIO m) => n -> m Int
getZIndex = liftIO . cc_getZIndex . toNode

getRotation :: (IsNode n, MonadIO m) => n -> m Double
getRotation = liftIO . cc_getRotation . toNode

getScaleX :: (IsNode n, MonadIO m) => n -> m Double
getScaleX = liftIO . cc_getScaleX . toNode

getScaleY :: (IsNode n, MonadIO m) => n -> m Double
getScaleY = liftIO . cc_getScaleY . toNode

getScale :: (IsNode n, MonadIO m) => n -> m (V2 Double)
getScale n = liftIO $ (^&) <$> cc_getScaleX n' <*> cc_getScaleY n'
  where n' = toNode n

getVisible :: (IsNode n, MonadIO m) => n -> m Bool
getVisible = liftIO . cc_getVisible . toNode

getColor :: (IsNode n, MonadIO m) => n -> m (Colour Double)
getColor n = liftIO $ fromJSValUnchecked =<< cc_getColor (toNode n)

getCascadeColor :: (IsNode n, MonadIO m) => n -> m Bool
getCascadeColor = liftIO . cc_getCascadeColor . toNode

getOpacity :: (IsNode n, MonadIO m) => n -> m Double
getOpacity = liftIO . ((/255) . fromIntegral <$>) . cc_getOpacity . toNode

getCascadeOpacity :: (IsNode n, MonadIO m) => n -> m Bool
getCascadeOpacity = liftIO . cc_getCascadeOpacity . toNode

convertToNodeSpace :: (IsNode n, MonadIO m) => n -> P2 Double -> m (P2 Double)
convertToNodeSpace n p = liftIO $ fromJSValUnchecked =<< cc_convertToNodeSpace (toNode n) =<< toJSVal p

convertToWorldSpace :: (IsNode n, MonadIO m) => n -> P2 Double -> m (P2 Double)
convertToWorldSpace n p = liftIO $ fromJSValUnchecked =<< cc_convertToWorldSpace (toNode n) =<< toJSVal p

-- accessing the children
getChildByName :: (IsNode n, PFromJSVal n', IsNode n', MonadIO m) => n -> String -> m (Maybe n')
getChildByName n name = liftIO $ jsNullOrUndefinedMaybe pFromJSVal <$> cc_getChildByName (toNode n) (pToJSVal name)

runAction :: (IsNode n, MonadIO m) => n -> Action -> m ()
runAction n = liftIO . cc_runAction (toNode n)

-- | workaround to convert between different Node types (required for loading ccs, for example)
castNode :: (IsNode a, PToJSVal a, IsNode b, PFromJSVal b) => a -> b
castNode = pFromJSVal . pToJSVal

foreign import javascript unsafe "new cc.Node()"  cc_createNode :: IO Node
foreign import javascript unsafe "$1.onEnter = function() { cc.Node.prototype.onEnter.call(this); $2(); }"  cc_setOnEnter :: Node -> Callback a -> IO ()
foreign import javascript unsafe "$1.addChild($2)" cc_addChild :: Node -> Node -> IO ()
foreign import javascript unsafe "$1.addChild($2, $3)" cc_addChild' :: Node -> Node -> Int -> IO ()
foreign import javascript unsafe "$1.getChildByName($2)" cc_getChildByName :: Node -> JSVal -> IO JSVal
foreign import javascript unsafe "$1.removeChild($2, false)" cc_removeChild :: Node -> Node -> IO ()
foreign import javascript unsafe "$1.removeAllChildren(false)" cc_removeAllChildren :: Node -> IO ()
foreign import javascript unsafe "if ($1.x !== $2) {$1.x = $2}" cc_setX :: Node -> Double -> IO ()
foreign import javascript unsafe "if ($1.y !== $2) {$1.y = $2}" cc_setY :: Node -> Double -> IO ()
foreign import javascript unsafe "$1.setPosition($2, $3)" cc_setPosition :: Node -> Double -> Double -> IO ()
foreign import javascript unsafe "if ($1.width !== $2) {$1.width = $2}" cc_setWidth :: Node -> Double -> IO ()
foreign import javascript unsafe "if ($1.height !== $2) {$1.height = $2}" cc_setHeight :: Node -> Double -> IO ()
foreign import javascript unsafe "$1.setContentSize($2, $3)" cc_setContentSize :: Node -> Double -> Double -> IO ()
foreign import javascript unsafe "if ($1.anchorX !== $2) {$1.anchorX = $2}" cc_setAnchorX :: Node -> Double -> IO ()
foreign import javascript unsafe "if ($1.anchorY !== $2) {$1.anchorY = $2}" cc_setAnchorY :: Node -> Double -> IO ()
foreign import javascript unsafe "$1.setAnchorPoint($2, $3)" cc_setAnchor :: Node -> Double -> Double -> IO ()
foreign import javascript unsafe "if ($1.skewX !== $2) {$1.skewX = $2}" cc_setSkewX :: Node -> Double -> IO ()
foreign import javascript unsafe "if ($1.skewY !== $2) {$1.skewY = $2}" cc_setSkewY :: Node -> Double -> IO ()
foreign import javascript unsafe "if ($1.zIndex !== $2) {$1.zIndex = $2}" cc_setZIndex :: Node -> Int -> IO ()
foreign import javascript unsafe "if ($1.rotation !== $2) {$1.rotation = $2}" cc_setRotation :: Node -> Double -> IO ()
foreign import javascript unsafe "if ($1.scaleX !== $2) {$1.scaleX = $2}" cc_setScaleX :: Node -> Double -> IO ()
foreign import javascript unsafe "if ($1.scaleY !== $2) {$1.scaleY = $2}" cc_setScaleY :: Node -> Double -> IO ()
foreign import javascript unsafe "$1.setScale($2, $3)" cc_setScale :: Node -> Double -> Double -> IO ()
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
foreign import javascript unsafe "$1.rotation" cc_getRotation :: Node -> IO Double
foreign import javascript unsafe "$1.scaleX" cc_getScaleX :: Node -> IO Double
foreign import javascript unsafe "$1.scaleY" cc_getScaleY :: Node -> IO Double
foreign import javascript unsafe "$1.visible" cc_getVisible :: Node -> IO Bool
foreign import javascript unsafe "$1.color" cc_getColor :: Node -> IO JSVal
foreign import javascript unsafe "$1.cascadeColor" cc_getCascadeColor :: Node -> IO Bool
foreign import javascript unsafe "$1.opacity" cc_getOpacity :: Node -> IO Word8
foreign import javascript unsafe "$1.cascadeOpacity" cc_getCascadeOpacity :: Node -> IO Bool
foreign import javascript unsafe "$1.convertToNodeSpace($2)" cc_convertToNodeSpace :: Node -> JSVal -> IO JSVal
foreign import javascript unsafe "$1.convertToWorldSpace($2)" cc_convertToWorldSpace :: Node -> JSVal -> IO JSVal
foreign import javascript unsafe "$1.runAction($2)" cc_runAction :: Node -> Action -> IO ()
