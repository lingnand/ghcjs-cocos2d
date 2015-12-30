{-# LANGUAGE JavaScriptFFI #-}
module JavaScript.Cocos2d.Director where

import GHCJS.Types
import JavaScript.Cocos2d.Scene

newtype Director = Director JSVal

foreign import javascript unsafe "$1.runScene($2)" runScene :: Director -> Scene -> IO ()
