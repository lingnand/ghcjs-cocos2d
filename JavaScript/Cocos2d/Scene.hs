module JavaScript.Cocos2d.Scene where

import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign.Callback
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Utils

class IsNode a => IsScene a where
    toScene :: a -> Scene

newtype Scene = Scene JSVal
instance IsNode Scene where
    toNode (Scene v) = Node v
instance IsScene Scene where
    toScene = id

foreign import javascript unsafe "new cc.Scene()" createScene :: IO Scene

newtype LoaderScene = LoaderScene JSVal
instance IsNode LoaderScene where
    toNode (LoaderScene v) = Node v
instance IsScene LoaderScene where
    toScene (LoaderScene v) = Scene v

foreign import javascript unsafe "new cc.LoaderScene()" createLoaderScene :: IO LoaderScene

preload :: [String] -> IO () -> IO (LoaderScene, IO ())
preload resources cb = flip convCallbackWithReturn cb . cc_preload =<< toJSVal resources

foreign import javascript unsafe "cc.LoaderScene.preload($1, $2, cc.game)" cc_preload :: JSVal -> Callback a -> IO LoaderScene
