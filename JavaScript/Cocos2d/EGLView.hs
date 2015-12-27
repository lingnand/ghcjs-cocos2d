module JavaScript.Cocos2d.EGLView where

import GHCJS.Types
import GHCJS.Marshal
import JavaScript.Cocos2d.Class
import JavaScript.Cocos2d.Types

data ResolutionPolicy = ExactFit | NoBorder | ShowAll | FixedHeight | FixedWidth | Unknown
        deriving (Bounded, Enum, Show)

instance ToJSVal ResolutionPolicy where
        toJSVal ExactFit = cc_ResolutionPolicy_EXACT_FIT
        toJSVal NoBorder = cc_ResolutionPolicy_NO_BORDER
        toJSVal ShowAll = cc_ResolutionPolicy_SHOW_ALL
        toJSVal FixedHeight = cc_ResolutionPolicy_FIXED_HEIGHT
        toJSVal FixedWidth = cc_ResolutionPolicy_FIXED_WIDTH
        toJSVal Unknown = cc_ResolutionPolicy_UNKNOWN

instance FromJSVal ResolutionPolicy where
        fromJSVal = enumFromJSVal

newtype EGLView = EGLView JSVal

setEnableRetina :: Cocos2d m => EGLView -> Bool -> m ()
setEnableRetina v b = liftIO $ cc_enableRetina v b

setAdjustViewPort :: Cocos2d m => EGLView -> Bool -> m ()
setAdjustViewPort v b = liftIO $ cc_adjustViewPort v b

setDesignResolutionSize :: Cocos2d m => EGLView -> Int -> Int -> ResolutionPolicy -> m ()
setDesignResolutionSize v width height resolutionPolicy = liftIO $ cc_setDesignResolutionSize v width height =<< toJSVal resolutionPolicy

setResizeWithBrowserSize :: Cocos2d m => EGLView -> Bool -> m ()
setResizeWithBrowserSize v b = liftIO $ cc_resizeWithBrowserSize v b

foreign import javascript unsafe "$1.enableRetina($2)" cc_enableRetina :: EGLView -> Bool -> IO ()
foreign import javascript unsafe "$1.adjustViewPort($2)" cc_adjustViewPort :: EGLView -> Bool -> IO ()
foreign import javascript unsafe "$1.setDesignResolutionSize($2, $3, $4)" cc_setDesignResolutionSize :: EGLView -> Int -> Int -> JSVal -> IO ()
foreign import javascript unsafe "$1.resizeWithBrowserSize($2)" cc_resizeWithBrowserSize :: EGLView -> Bool -> IO ()
foreign import javascript unsafe "cc.ResolutionPolicy.EXACT_FIT" cc_ResolutionPolicy_EXACT_FIT :: IO JSVal
foreign import javascript unsafe "cc.ResolutionPolicy.NO_BORDER" cc_ResolutionPolicy_NO_BORDER :: IO JSVal
foreign import javascript unsafe "cc.ResolutionPolicy.SHOW_ALL" cc_ResolutionPolicy_SHOW_ALL :: IO JSVal
foreign import javascript unsafe "cc.ResolutionPolicy.FIXED_HEIGHT" cc_ResolutionPolicy_FIXED_HEIGHT :: IO JSVal
foreign import javascript unsafe "cc.ResolutionPolicy.FIXED_WIDTH" cc_ResolutionPolicy_FIXED_WIDTH :: IO JSVal
foreign import javascript unsafe "cc.ResolutionPolicy.UNKNOWN" cc_ResolutionPolicy_UNKNOWN :: IO JSVal
