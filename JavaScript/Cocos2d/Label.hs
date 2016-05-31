{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module JavaScript.Cocos2d.Label
  (
    IsLabel(..)
  , Label
  , createLabel
  , setText
  , getText
  , HAlign
  , VAlign
  , setHorizontalAlign
  , getHorizontalAlign
  , setVerticalAlign
  , getVerticalAlign
  , setFontSize
  , getFontSize
  , setFontName
  , getFontName
  , setBoundingSize
  , getBoundingSize
  , setBoundingWidth
  , getBoundingWidth
  , setBoundingHeight
  , getBoundingHeight
  , setFontFillColor
  , getFontFillColor
  , enableStroke
  , disableStroke
  , enableShadow
  , disableShadow
  )
  where

import Diagrams (V2(..))
import Data.Colour
import Control.Monad.IO.Class
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Utils
import JavaScript.Cocos2d.Types()

-- | Rename LabelTTF to Label for simplicity

class IsNode a => IsLabel a where
    toLabel :: a -> Label

newtype Label = Label JSVal deriving (PFromJSVal, PToJSVal)
instance IsNode Label where
    toNode (Label v) = pFromJSVal v
instance IsLabel Label where
    toLabel = id

createLabel :: MonadIO m => m Label
createLabel = liftIO cc_createLabelTTF

-- | Rename @setString@ to @setText@ for more readability
setText :: (IsLabel l, MonadIO m) => l -> String -> m ()
setText l = liftIO . cc_setString (toLabel l) . pToJSVal

getText :: (IsLabel l, MonadIO m) => l -> m String
getText l = liftIO $ pFromJSVal <$> cc_getString (toLabel l)

-- | NOTE: Mapping from 0 - 2
data HAlign = ALeft | ACenter | ARight deriving (Bounded, Enum, Show, Eq, Ord, Read)

instance ToJSVal HAlign where
    toJSVal = toJSVal . fromEnum

instance FromJSVal HAlign where
    fromJSVal = enumFromJSValByInt

-- | NOTE: Mapping from 0 - 2
data VAlign = ATop | AMiddle | ABottom deriving (Bounded, Enum, Show, Eq, Ord, Read)

instance ToJSVal VAlign where
    toJSVal = toJSVal . fromEnum

instance FromJSVal VAlign where
    fromJSVal = enumFromJSValByInt

setHorizontalAlign :: (IsLabel l, MonadIO m) => l -> HAlign -> m ()
setHorizontalAlign l a = liftIO $ cc_setHorizontalAlign (toLabel l) =<< toJSVal a

getHorizontalAlign :: (IsLabel l, MonadIO m) => l -> m HAlign
getHorizontalAlign l = liftIO $ fromJSValUnchecked =<< cc_getHorizontalAlign (toLabel l)

setVerticalAlign :: (IsLabel l, MonadIO m) => l -> VAlign -> m ()
setVerticalAlign l a = liftIO $ cc_setVerticalAlign (toLabel l) =<< toJSVal a

getVerticalAlign :: (IsLabel l, MonadIO m) => l -> m VAlign
getVerticalAlign l = liftIO $ fromJSValUnchecked =<< cc_getVerticalAlign (toLabel l)

setFontSize :: (IsLabel l, MonadIO m) => l -> Double -> m ()
setFontSize l = liftIO . cc_setFontSize (toLabel l)

getFontSize :: (IsLabel l, MonadIO m) => l -> m Double
getFontSize l = liftIO $ cc_getFontSize (toLabel l)

setFontName :: (IsLabel l, MonadIO m) => l -> String -> m ()
setFontName l name = liftIO $ cc_setFontName (toLabel l) (pToJSVal name)

getFontName :: (IsLabel l, MonadIO m) => l -> m String
getFontName l = liftIO $ pFromJSVal <$> cc_getFontName (toLabel l)

-- | Rename dimension to boundingSize for better consistency
setBoundingSize :: (IsLabel l, MonadIO m) => l -> V2 Double -> m ()
setBoundingSize l (V2 w h) = liftIO $ cc_setDimensions (toLabel l) w h

setBoundingWidth :: (IsLabel l, MonadIO m) => l -> Double -> m ()
setBoundingWidth l = liftIO . cc_setBoundingWidth (toLabel l)

setBoundingHeight :: (IsLabel l, MonadIO m) => l -> Double -> m ()
setBoundingHeight l = liftIO . cc_setBoundingHeight (toLabel l)

getBoundingSize :: (IsLabel l, MonadIO m) => l -> m (V2 Double)
getBoundingSize l = liftIO $ V2 <$> cc_getBoundingWidth l' <*> cc_getBoundingHeight l'
  where l' = toLabel l

getBoundingWidth :: (IsLabel l, MonadIO m) => l -> m Double
getBoundingWidth l = liftIO $ cc_getBoundingWidth (toLabel l)

getBoundingHeight :: (IsLabel l, MonadIO m) => l -> m Double
getBoundingHeight l = liftIO $ cc_getBoundingHeight (toLabel l)

setFontFillColor :: (IsLabel l, MonadIO m) => l -> Colour Double -> m ()
setFontFillColor l c = liftIO $ cc_setFontFillColor (toLabel l) =<< toJSVal c

getFontFillColor :: (IsLabel l, MonadIO m) => l -> m (Colour Double)
getFontFillColor l = liftIO $ cc_getFontFillColor (toLabel l) >>= fromJSValUnchecked

-- setStrokeColor :: (IsLabel l, MonadIO m) => l -> Colour Double -> m ()
-- setStrokeColor l c = liftIO $ cc_setStrokeColor (toLabel l) =<< toJSVal c
--
-- getStrokeColor :: (IsLabel l, MonadIO m) => l -> m (Colour Double)
-- getStrokeColor l = liftIO $ cc_getStrokeColor (toLabel l) >>= fromJSValUnchecked

enableStroke :: (IsLabel l, MonadIO m) => l -> Colour Double -> Double -> m ()
enableStroke l color size = liftIO $ do
    c <- toJSVal color
    cc_enableStroke (toLabel l) c size

disableStroke :: (IsLabel l, MonadIO m) => l -> m ()
disableStroke l = liftIO $ cc_disableStroke (toLabel l)

enableShadow :: (IsLabel l, MonadIO m) => l -> AlphaColour Double -> Double -> Double -> Double -> m ()
enableShadow l color offsetX offsetY blurRadius = liftIO $ do
    c <- toJSVal color
    cc_enableShadow (toLabel l) c offsetX offsetY blurRadius

disableShadow :: (IsLabel l, MonadIO m) => l -> m ()
disableShadow l = liftIO $ cc_disableShadow (toLabel l)

foreign import javascript unsafe "new cc.LabelTTF()" cc_createLabelTTF :: IO Label
foreign import javascript unsafe "$1.setString($2)" cc_setString :: Label -> JSVal -> IO ()
foreign import javascript unsafe "$1.getString()" cc_getString :: Label -> IO JSVal
foreign import javascript unsafe "$1.setHorizontalAlignment($2)" cc_setHorizontalAlign :: Label -> JSVal -> IO ()
foreign import javascript unsafe "$1.setVerticalAlignment($2)" cc_setVerticalAlign :: Label -> JSVal -> IO ()
foreign import javascript unsafe "$1.getHorizontalAlignment()" cc_getHorizontalAlign :: Label -> IO JSVal
foreign import javascript unsafe "$1.getVerticalAlignment()" cc_getVerticalAlign :: Label -> IO JSVal
foreign import javascript unsafe "$1.setFontSize($2)" cc_setFontSize :: Label -> Double -> IO ()
foreign import javascript unsafe "$1.getFontSize()" cc_getFontSize :: Label -> IO Double
foreign import javascript unsafe "$1.setFontName($2)" cc_setFontName :: Label -> JSVal -> IO ()
foreign import javascript unsafe "$1.getFontName()" cc_getFontName :: Label -> IO JSVal
foreign import javascript unsafe "$1.setDimensions($2, $3)" cc_setDimensions :: Label -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.boundingWidth = $2" cc_setBoundingWidth :: Label -> Double -> IO ()
foreign import javascript unsafe "$1.boundingHeight = $2" cc_setBoundingHeight :: Label -> Double -> IO ()
foreign import javascript unsafe "$1.boundingWidth" cc_getBoundingWidth :: Label -> IO Double
foreign import javascript unsafe "$1.boundingHeight" cc_getBoundingHeight :: Label -> IO Double
foreign import javascript unsafe "$1.setFontFillColor($2)" cc_setFontFillColor :: Label -> JSVal -> IO ()
foreign import javascript unsafe "$1.fillStyle" cc_getFontFillColor :: Label -> IO JSVal
foreign import javascript unsafe "$1.strokeStyle = $2" cc_setStrokeColor :: Label -> JSVal -> IO ()
foreign import javascript unsafe "$1.strokeStyle" cc_getStrokeColor :: Label -> IO JSVal
foreign import javascript unsafe "$1.enableStroke($2, $3)" cc_enableStroke :: Label -> JSVal -> Double -> IO ()
foreign import javascript unsafe "$1.disableStroke()" cc_disableStroke :: Label -> IO ()
foreign import javascript unsafe "$1.enableShadow($2, cc.size($3, $4), $5)" cc_enableShadow :: Label -> JSVal -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.disableShadow()" cc_disableShadow :: Label -> IO ()
