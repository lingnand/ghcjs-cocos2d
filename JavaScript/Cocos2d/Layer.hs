{-# LANGUAGE JavaScriptFFI #-}

module JavaScript.Cocos2d.Layer where

import Reflex
import GHSJS.Types

newtype Layer = Layer JSVal

class IsLayer a where
    toLayer :: a -> Layer

foreign import javascript unsafe "new cc.Layer()"  cc_createLayer :: IO Layer
