module JavaScript.Cocos2d.Label where

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

newtype Label = Label JSVal
instance IsNode Label where
    toNode (Label v) = pFromJSVal v
instance IsLabel Label where
    toLabel = id

createLabel :: MonadIO m => m Label
createLabel = liftIO cc_createLabelTTF

-- | Rename @setString@ to @setText@ for more readability
setText :: (IsLabel l, MonadIO m) => l -> String -> m ()
setText l = liftIO . cc_setString (toLabel l) . pToJSVal

-- | Mapping from 0 - 2
data HAlign = ALeft | ACenter | ARight deriving (Bounded, Enum, Show, Eq, Ord, Read)

instance ToJSVal HAlign where
    toJSVal = toJSVal . fromEnum

instance FromJSVal HAlign where
    fromJSVal = enumFromJSValByInt

-- | Mapping from 0 - 2
data VAlign = ATop | AMiddle | ABottom deriving (Bounded, Enum, Show, Eq, Ord, Read)

instance ToJSVal VAlign where
    toJSVal = toJSVal . fromEnum

instance FromJSVal VAlign where
    fromJSVal = enumFromJSValByInt

setHorizontalAlign :: (IsLabel l, MonadIO m) => l -> HAlign -> m ()
setHorizontalAlign l a = liftIO $ cc_setHorizontalAlign (toLabel l) =<< toJSVal a

setVerticalAlign :: (IsLabel l, MonadIO m) => l -> VAlign -> m ()
setVerticalAlign l a = liftIO $ cc_setVerticalAlign (toLabel l) =<< toJSVal a

setFontSize :: (IsLabel l, MonadIO m) => l -> Double -> m ()
setFontSize l = liftIO . cc_setFontSize (toLabel l)

setFontName :: (IsLabel l, MonadIO m) => l -> String -> m ()
setFontName l name = liftIO $ cc_setFontName (toLabel l) (pToJSVal name)

setDimensions :: (IsLabel l, MonadIO m) => l -> Double -> Double -> m ()
setDimensions l w h = liftIO $ cc_setDimensions (toLabel l) w h

setFontFillColor :: (IsLabel l, MonadIO m) => l -> Colour Double -> m ()
setFontFillColor l c = liftIO $ cc_setFontFillColor (toLabel l) =<< toJSVal c

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
foreign import javascript unsafe "$1.setHorizontalAlignment($2)" cc_setHorizontalAlign :: Label -> JSVal -> IO ()
foreign import javascript unsafe "$1.setVerticalAlignment($2)" cc_setVerticalAlign :: Label -> JSVal -> IO ()
foreign import javascript unsafe "$1.setFontSize($2)" cc_setFontSize :: Label -> Double -> IO ()
foreign import javascript unsafe "$1.setFontName($2)" cc_setFontName :: Label -> JSVal -> IO ()
foreign import javascript unsafe "$1.setDimensions($2, $3)" cc_setDimensions :: Label -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.setFontFillColor($2)" cc_setFontFillColor :: Label -> JSVal -> IO ()
foreign import javascript unsafe "$1.enableStroke($2, $3)" cc_enableStroke :: Label -> JSVal -> Double -> IO ()
foreign import javascript unsafe "$1.disableStroke()" cc_disableStroke :: Label -> IO ()
foreign import javascript unsafe "$1.enableShadow($2, cc.size($3, $4), $5)" cc_enableShadow :: Label -> JSVal -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.disableShadow()" cc_disableShadow :: Label -> IO ()
