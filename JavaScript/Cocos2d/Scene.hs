module JavaScript.Cocos2d.Scene where

import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign.Callback
import JavaScript.Cocos2d.Class
import JavaScript.Cocos2d.Node
import JavaScript.Cocos2d.Utils

class IsNode a => IsScene a where
    toScene :: a -> Scene

newtype Scene = Scene JSVal
instance IsNode Scene where
    toNode (Scene v) = Node v
instance IsScene Scene where
    toScene = id

createScene :: Cocos2d m => m Scene
createScene = liftIO cc_createScene

newtype LoaderScene = LoaderScene JSVal
instance IsNode LoaderScene where
    toNode (LoaderScene v) = Node v
instance IsScene LoaderScene where
    toScene (LoaderScene v) = Scene v

createLoaderScene :: Cocos2d m => m LoaderScene
createLoaderScene = liftIO cc_createLoaderScene

preload :: Cocos2d m => [String] -> IO () -> m (IO ())
preload resources cb = flip convCallback cb . cc_preload =<< liftIO (toJSVal resources)

foreign import javascript unsafe "new cc.Scene()" cc_createScene :: IO Scene
foreign import javascript unsafe "new cc.LoaderScene()" cc_createLoaderScene :: IO LoaderScene
foreign import javascript unsafe "cc.LoaderScene.preload($1, $2, cc.game)" cc_preload :: JSVal -> Callback a -> IO ()
