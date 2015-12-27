module JavaScript.Cocos2d.Sys where

import Data.Maybe
import Control.Monad
import GHCJS.Types
import GHCJS.Marshal
import JavaScript.Cocos2d.Class
import JavaScript.Cocos2d.Types

newtype Sys = Sys JSVal

data OS = IOS | Android | Windows | Marmalade | Linux | Bada | Blackberry | OSX | WP8 | WinRT | Unknown
        deriving (Bounded, Enum, Show) 

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
    toJSVal Unknown = cc_OS_UNKNOWN

instance FromJSVal OS where
    fromJSVal = enumFromJSVal

getOS :: Cocos2d m => Sys -> m OS
getOS = liftIO . fmap (fromMaybe Unknown) . (fromJSVal <=< cc_getOS)

foreign import javascript unsafe "$1.os" cc_getOS :: Sys -> IO JSVal
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
foreign import javascript unsafe "cc.sys.OS_UNKNOWN" cc_OS_UNKNOWN :: IO JSVal
