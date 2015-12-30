{-# LANGUAGE JavaScriptFFI #-}
module JavaScript.Cocos2d (
    getSys,
    getDirector,
    getView
) where

import JavaScript.Cocos2d.Sys
import JavaScript.Cocos2d.Director
import JavaScript.Cocos2d.EGLView

foreign import javascript unsafe "cc.sys" getSys :: IO Sys
foreign import javascript unsafe "cc.director" getDirector :: IO Director
foreign import javascript unsafe "cc.view" getView :: IO EGLView
