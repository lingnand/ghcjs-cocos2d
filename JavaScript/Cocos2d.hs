{-# LANGUAGE JavaScriptFFI #-}
module JavaScript.Cocos2d where

import Linear
import GHCJS.Types
import GHCJS.Marshal.Internal
import JavaScript.Cocos2d.Scene
import JavaScript.Cocos2d.Types

--- sys
data OS = IOS | Android | Windows | Marmalade | Linux | Bada | Blackberry | OSX | WP8 | WinRT | UnknownOS
        deriving (Bounded, Enum, Show)

getOS :: IO OS
getOS = pFromJSVal <$> cc_os

getWinSize :: IO (V2 Double)
getWinSize = V2 <$> cc_getWinWidth <*> cc_getWinHeight

instance PToJSVal OS where
    pToJSVal IOS = cc_OS_IOS
    pToJSVal Android = cc_OS_ANDROID
    pToJSVal Windows = cc_OS_WINDOWS
    pToJSVal Marmalade = cc_OS_MARMALADE
    pToJSVal Linux = cc_OS_LINUX
    pToJSVal Bada = cc_OS_BADA
    pToJSVal Blackberry = cc_OS_BLACKBERRY
    pToJSVal OSX = cc_OS_OSX
    pToJSVal WP8 = cc_OS_WP8
    pToJSVal WinRT = cc_OS_WINRT
    pToJSVal _ = cc_OS_UNKNOWN

instance PFromJSVal OS where
    pFromJSVal = pEnumFromJSVal

foreign import javascript unsafe "cc.sys.os" cc_os :: IO JSVal
foreign import javascript unsafe "cc.winSize.width" cc_getWinWidth :: IO Double
foreign import javascript unsafe "cc.winSize.height" cc_getWinHeight :: IO Double
foreign import javascript unsafe "cc.sys.OS_IOS" cc_OS_IOS :: JSVal
foreign import javascript unsafe "cc.sys.OS_ANDROID" cc_OS_ANDROID :: JSVal
foreign import javascript unsafe "cc.sys.OS_WINDOWS" cc_OS_WINDOWS :: JSVal
foreign import javascript unsafe "cc.sys.OS_MARMALADE" cc_OS_MARMALADE :: JSVal
foreign import javascript unsafe "cc.sys.OS_LINUX" cc_OS_LINUX :: JSVal
foreign import javascript unsafe "cc.sys.OS_BADA" cc_OS_BADA :: JSVal
foreign import javascript unsafe "cc.sys.OS_BLACKBERRY" cc_OS_BLACKBERRY :: JSVal
foreign import javascript unsafe "cc.sys.OS_OSX" cc_OS_OSX :: JSVal
foreign import javascript unsafe "cc.sys.OS_WP8" cc_OS_WP8 :: JSVal
foreign import javascript unsafe "cc.sys.OS_WINRT" cc_OS_WINRT :: JSVal
foreign import javascript unsafe "cc.sys.OS_UNKNOWN" cc_OS_UNKNOWN :: JSVal

--- director
foreign import javascript unsafe "cc.director.runScene($1)" runScene :: Scene -> IO ()

--- EGLView
data ResolutionPolicy = ExactFit | NoBorder | ShowAll | FixedHeight | FixedWidth | UnknownRP
        deriving (Bounded, Enum, Show)

instance PToJSVal ResolutionPolicy where
    pToJSVal ExactFit = cc_ResolutionPolicy_EXACT_FIT
    pToJSVal NoBorder = cc_ResolutionPolicy_NO_BORDER
    pToJSVal ShowAll = cc_ResolutionPolicy_SHOW_ALL
    pToJSVal FixedHeight = cc_ResolutionPolicy_FIXED_HEIGHT
    pToJSVal FixedWidth = cc_ResolutionPolicy_FIXED_WIDTH
    pToJSVal _ = cc_ResolutionPolicy_UNKNOWN

instance PFromJSVal ResolutionPolicy where
    pFromJSVal = pEnumFromJSVal

foreign import javascript unsafe "cc.view.enableRetina($1)" setEnableRetina :: Bool -> IO ()
foreign import javascript unsafe "cc.view.adjustViewPort($1)" setAdjustViewPort :: Bool -> IO ()
foreign import javascript unsafe "cc.view.resizeWithBrowserSize($1)" setResizeWithBrowserSize :: Bool -> IO ()
setDesignResolutionSize :: Int -> Int -> ResolutionPolicy -> IO ()
setDesignResolutionSize width height = cc_setDesignResolutionSize width height . pToJSVal

foreign import javascript unsafe "cc.view.setDesignResolutionSize($1, $2, $3)" cc_setDesignResolutionSize :: Int -> Int -> JSVal -> IO ()
foreign import javascript unsafe "cc.ResolutionPolicy.EXACT_FIT" cc_ResolutionPolicy_EXACT_FIT :: JSVal
foreign import javascript unsafe "cc.ResolutionPolicy.NO_BORDER" cc_ResolutionPolicy_NO_BORDER :: JSVal
foreign import javascript unsafe "cc.ResolutionPolicy.SHOW_ALL" cc_ResolutionPolicy_SHOW_ALL :: JSVal
foreign import javascript unsafe "cc.ResolutionPolicy.FIXED_HEIGHT" cc_ResolutionPolicy_FIXED_HEIGHT :: JSVal
foreign import javascript unsafe "cc.ResolutionPolicy.FIXED_WIDTH" cc_ResolutionPolicy_FIXED_WIDTH :: JSVal
foreign import javascript unsafe "cc.ResolutionPolicy.UNKNOWN" cc_ResolutionPolicy_UNKNOWN :: JSVal
