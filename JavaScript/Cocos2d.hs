{-# LANGUAGE JavaScriptFFI #-}
module JavaScript.Cocos2d (
    module JavaScript.Cocos2d.Class,
    getSys,
    getDirector,
    getView
) where

import JavaScript.Cocos2d.Class
import JavaScript.Cocos2d.Sys
import JavaScript.Cocos2d.Director
import JavaScript.Cocos2d.EGLView

getSys :: Cocos2d m => m Sys
getSys = liftIO cc_getSys

getDirector :: Cocos2d m => m Director
getDirector = liftIO cc_getDirector

getView :: Cocos2d m => m EGLView
getView = liftIO cc_getView

foreign import javascript unsafe "cc.sys" cc_getSys :: IO Sys
foreign import javascript unsafe "cc.director" cc_getDirector :: IO Director
foreign import javascript unsafe "cc.view" cc_getView :: IO EGLView
