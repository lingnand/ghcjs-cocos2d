{-# LANGUAGE JavaScriptFFI #-}
module JavaScript.Cocos2d where

import Linear
import GHCJS.Types
import GHCJS.Marshal.Internal
import Control.Monad
import JavaScript.Cocos2d.Scene
import JavaScript.Cocos2d.Types

--- sys
data OS = IOS | Android | Windows | Marmalade | Linux | Bada | Blackberry | OSX | WP8 | WinRT
        deriving (Bounded, Enum, Show)

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
    fromJSVal = enumFromJSVal

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
foreign import javascript unsafe "cc.director.runScene($1)" runScene :: Scene -> IO ()

--- EGLView
data ResolutionPolicy = ExactFit | NoBorder | ShowAll | FixedHeight | FixedWidth
        deriving (Bounded, Enum, Show)

instance ToJSVal ResolutionPolicy where
    toJSVal ExactFit = cc_ResolutionPolicy_EXACT_FIT
    toJSVal NoBorder = cc_ResolutionPolicy_NO_BORDER
    toJSVal ShowAll = cc_ResolutionPolicy_SHOW_ALL
    toJSVal FixedHeight = cc_ResolutionPolicy_FIXED_HEIGHT
    toJSVal FixedWidth = cc_ResolutionPolicy_FIXED_WIDTH

instance FromJSVal ResolutionPolicy where
    fromJSVal = enumFromJSVal

foreign import javascript unsafe "cc.view.enableRetina($1)" setEnableRetina :: Bool -> IO ()
foreign import javascript unsafe "cc.view.adjustViewPort($1)" setAdjustViewPort :: Bool -> IO ()
foreign import javascript unsafe "cc.view.resizeWithBrowserSize($1)" setResizeWithBrowserSize :: Bool -> IO ()
setDesignResolutionSize :: Int -> Int -> ResolutionPolicy -> IO ()
setDesignResolutionSize width height = cc_setDesignResolutionSize width height <=< toJSVal

foreign import javascript unsafe "cc.view.setDesignResolutionSize($1, $2, $3)" cc_setDesignResolutionSize :: Int -> Int -> JSVal -> IO ()
foreign import javascript unsafe "cc.ResolutionPolicy.EXACT_FIT" cc_ResolutionPolicy_EXACT_FIT :: IO JSVal
foreign import javascript unsafe "cc.ResolutionPolicy.NO_BORDER" cc_ResolutionPolicy_NO_BORDER :: IO JSVal
foreign import javascript unsafe "cc.ResolutionPolicy.SHOW_ALL" cc_ResolutionPolicy_SHOW_ALL :: IO JSVal
foreign import javascript unsafe "cc.ResolutionPolicy.FIXED_HEIGHT" cc_ResolutionPolicy_FIXED_HEIGHT :: IO JSVal
foreign import javascript unsafe "cc.ResolutionPolicy.FIXED_WIDTH" cc_ResolutionPolicy_FIXED_WIDTH :: IO JSVal
