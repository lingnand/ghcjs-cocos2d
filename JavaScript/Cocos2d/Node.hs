{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module JavaScript.Cocos2d.Node where

import Control.Monad
import GHCJS.Types
import GHCJS.Marshal.Internal
import GHCJS.Foreign.Callback
import GHCJS.Foreign.Internal
import JavaScript.Cocos2d.Class
import JavaScript.Cocos2d.Utils
import JavaScript.Cocos2d.Types

class IsNode a where
    toNode :: a -> Node

newtype Node = Node JSVal
instance IsNode Node where
    toNode = id

createNode :: Cocos2d m => m Node
createNode = liftIO cc_createNode

setOnEnter :: (Cocos2d m, IsNode n) => n -> IO () -> m (IO ())
setOnEnter = convCallback . cc_setOnEnter . toNode

addChild :: (Cocos2d m, IsNode n, IsNode c) => n -> c -> m ()
addChild n c = liftIO $ cc_addChild (toNode n) (toNode c) jsUndefined

addChild' :: (Cocos2d m, IsNode n, IsNode c) => n -> c -> Int -> m ()
addChild' n c localZOrder = liftIO $ cc_addChild (toNode n) (toNode c) (pToJSVal localZOrder)

foreign import javascript unsafe "new cc.Node()"  cc_createNode :: IO Node
foreign import javascript unsafe "$1.onEnter = function() { cc.Node.prototype.onEnter.call(this); $2(); }"  cc_setOnEnter :: Node -> Callback a -> IO ()
foreign import javascript unsafe "$1.addChild($2, $3)" cc_addChild :: Node -> Node -> JSVal -> IO ()
