{-# LANGUAGE JavaScriptFFI #-}
module JavaScript.Cocos2d (
    module JavaScript.Cocos2d.Class,
    module JavaScript.Cocos2d.Game,
    getSys,
    getDirector,
    getGame,
    runWithGame,
    getView
) where

import Control.Applicative
import JavaScript.Cocos2d.Class
import JavaScript.Cocos2d.Sys
import JavaScript.Cocos2d.Director
import JavaScript.Cocos2d.Game
import JavaScript.Cocos2d.EGLView

getSys :: Cocos2d m => m Sys 
getSys = liftIO cc_getSys

getDirector :: Cocos2d m => m Director
getDirector = liftIO cc_getDirector

getGame :: Cocos2d m => m Game
getGame = liftIO cc_getGame

runWithGame :: Cocos2d m => (Game -> m a) -> m a
runWithGame h = getGame >>= liftA2 (<*) h run

getView :: Cocos2d m => m EGLView
getView = liftIO cc_getView

foreign import javascript unsafe "cc.sys" cc_getSys :: IO Sys
foreign import javascript unsafe "cc.director" cc_getDirector :: IO Director
foreign import javascript unsafe "cc.game" cc_getGame :: IO Game
foreign import javascript unsafe "cc.view" cc_getView :: IO EGLView
