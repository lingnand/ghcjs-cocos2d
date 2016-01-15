{-# LANGUAGE JavaScriptFFI #-}
module JavaScript.Cocos2d where

import Linear
import GHCJS.Types
import GHCJS.Marshal
import Control.Monad
import Control.Monad.IO.Class
import JavaScript.Cocos2d.Scene
import JavaScript.Cocos2d.Utils

--- sys
data OS = IOS | Android | Windows | Marmalade | Linux | Bada | Blackberry | OSX | WP8 | WinRT
        deriving (Bounded, Enum, Show, Eq, Ord, Read)

getOS :: IO (Maybe OS)
getOS = cc_os >>= fromJSVal

getWinSize :: IO (V2 Double)
getWinSize = V2 <$> cc_getWinWidth <*> cc_getWinHeight

instance ToJSVal OS where
    toJSVal IOS = cc_OS_IOS
    toJSVal Android = cc_OS_ANDROID
    toJSVal Windows = cc_OS_WINDOWS
    toJSVal Marmalade = cc_OS_MARMALADE
    toJSVal Linux = cc_OS_LINUX
    toJSVal Bada = cc_OS_BADA
    toJSVal Blackberry = cc_OS_BLACKBERRY
    toJSVal OSX = cc_OS_OSX
    toJSVal WP8 = cc_OS_WP8
    toJSVal WinRT = cc_OS_WINRT

instance FromJSVal OS where
    fromJSVal = enumFromJSValByTryAll

foreign import javascript unsafe "cc.sys.os" cc_os :: IO JSVal
foreign import javascript unsafe "cc.winSize.width" cc_getWinWidth :: IO Double
foreign import javascript unsafe "cc.winSize.height" cc_getWinHeight :: IO Double
foreign import javascript unsafe "cc.sys.OS_IOS" cc_OS_IOS :: IO JSVal
foreign import javascript unsafe "cc.sys.OS_ANDROID" cc_OS_ANDROID :: IO JSVal
foreign import javascript unsafe "cc.sys.OS_WINDOWS" cc_OS_WINDOWS :: IO JSVal
foreign import javascript unsafe "cc.sys.OS_MARMALADE" cc_OS_MARMALADE :: IO JSVal
foreign import javascript unsafe "cc.sys.OS_LINUX" cc_OS_LINUX :: IO JSVal
foreign import javascript unsafe "cc.sys.OS_BADA" cc_OS_BADA :: IO JSVal
foreign import javascript unsafe "cc.sys.OS_BLACKBERRY" cc_OS_BLACKBERRY :: IO JSVal
foreign import javascript unsafe "cc.sys.OS_OSX" cc_OS_OSX :: IO JSVal
foreign import javascript unsafe "cc.sys.OS_WP8" cc_OS_WP8 :: IO JSVal
foreign import javascript unsafe "cc.sys.OS_WINRT" cc_OS_WINRT :: IO JSVal

--- director
runScene :: MonadIO m => Scene -> m ()
runScene = liftIO . cc_runScene

--- EGLView
-- | Mapping from 0 - 4
data ResolutionPolicy = ExactFit | NoBorder | ShowAll | FixedHeight | FixedWidth
        deriving (Bounded, Enum, Show, Eq, Ord, Read)

instance ToJSVal ResolutionPolicy where
    toJSVal = toJSVal . fromEnum

instance FromJSVal ResolutionPolicy where
    fromJSVal = enumFromJSValByInt

setEnableRetina :: MonadIO m => Bool -> m ()
setEnableRetina = liftIO . cc_setEnableRetina

setAdjustViewPort :: MonadIO m => Bool -> m ()
setAdjustViewPort = liftIO . cc_setAdjustViewPort

setResizeWithBrowserSize :: MonadIO m => Bool -> m ()
setResizeWithBrowserSize = liftIO . cc_setResizeWithBrowserSize

setDesignResolutionSize :: Int -> Int -> ResolutionPolicy -> IO ()
setDesignResolutionSize width height = cc_setDesignResolutionSize width height <=< toJSVal

foreign import javascript unsafe "cc.director.runScene($1)" cc_runScene :: Scene -> IO ()
foreign import javascript unsafe "cc.view.enableRetina($1)" cc_setEnableRetina :: Bool -> IO ()
foreign import javascript unsafe "cc.view.adjustViewPort($1)" cc_setAdjustViewPort :: Bool -> IO ()
foreign import javascript unsafe "cc.view.resizeWithBrowserSize($1)" cc_setResizeWithBrowserSize :: Bool -> IO ()
foreign import javascript unsafe "cc.view.setDesignResolutionSize($1, $2, $3)" cc_setDesignResolutionSize :: Int -> Int -> JSVal -> IO ()
