{-# LANGUAGE JavaScriptFFI #-}
module JavaScript.Cocos2d.Director where

import GHCJS.Types
import JavaScript.Cocos2d.Class
import JavaScript.Cocos2d.Scene

newtype Director = Director JSVal

runScene :: Cocos2d m => Director -> Scene -> m ()
runScene d s = liftIO $ cc_runScene d s

foreign import javascript unsafe "$1.runScene($2)" cc_runScene :: Director -> Scene -> IO ()
