{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module JavaScript.Cocos2d.Node where

import Control.Monad
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Marshal.Internal
import GHCJS.Foreign.Callback
import GHCJS.Foreign.Internal
import JavaScript.Cocos2d.Utils
import JavaScript.Cocos2d.Types

class IsNode a where
    toNode :: a -> Node

newtype Node = Node JSVal deriving (FromJSVal, ToJSVal)
instance IsNode Node where
    toNode = id

foreign import javascript unsafe "new cc.Node()"  createNode :: IO Node

setOnEnter :: IsNode n => n -> IO () -> IO (IO ())
setOnEnter n = convCallback cc_setOnEnter (toNode n)

addChild :: (IsNode n, IsNode c) => n -> c -> Int -> IO ()
addChild n c localZOrder = cc_addChild (toNode n) (toNode c) (pToJSVal localZOrder)

addChild_ :: (IsNode n, IsNode c) => n -> c -> IO ()
addChild_ n c = cc_addChild (toNode n) (toNode c) jsUndefined

-- other foreign imports
foreign import javascript unsafe "$1.onEnter = function() { cc.Node.prototype.onEnter.call(this); $2(); }"  cc_setOnEnter :: Node -> Callback a -> IO ()
foreign import javascript unsafe "$1.addChild($2, $3)" cc_addChild :: Node -> Node -> JSVal -> IO ()
