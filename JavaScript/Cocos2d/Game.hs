module JavaScript.Cocos2d.Game where

import GHCJS.Types
import GHCJS.Foreign.Callback
import JavaScript.Cocos2d.Class
import JavaScript.Cocos2d.Utils

newtype Game = Game JSVal

setOnStart :: Cocos2d m => Game -> IO () -> m (IO ())
setOnStart = convCallback . cc_setOnStart

run :: Cocos2d m => Game -> m ()
run = liftIO . cc_run

foreign import javascript unsafe "$1.onStart = $2" cc_setOnStart :: Game -> Callback a -> IO ()
foreign import javascript unsafe "$1.run()" cc_run :: Game -> IO ()
