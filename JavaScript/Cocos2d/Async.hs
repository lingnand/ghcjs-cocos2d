module JavaScript.Cocos2d.Async where

import Control.Monad.IO.Class
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign.Callback
import JavaScript.Cocos2d.Utils

newtype LoadOption = LoadOption JSVal

createLoadOption :: MonadIO m => m LoadOption
createLoadOption = liftIO cc_createLoadOption

-- | Callback takes [Total count] and [Finished count]
setLoadTrigger :: MonadIO m => LoadOption -> (Int -> Int -> IO ()) -> m (IO ())
setLoadTrigger o = liftIO . convCallback2 (cc_setLoadTrigger o)

setLoadFinish :: MonadIO m => LoadOption -> IO () -> m (IO ())
setLoadFinish o = liftIO . convCallback (cc_setLoadFinish o)

load :: MonadIO m => [String] -> LoadOption -> m ()
load resources o = liftIO $ toJSVal resources >>= flip cc_load o

foreign import javascript unsafe "{}" cc_createLoadOption :: IO LoadOption
foreign import javascript unsafe "$1.trigger = function(r, c, t) { $2(c, t); }" cc_setLoadTrigger :: LoadOption -> Callback a -> IO ()
-- XXX: ignore the error for now
foreign import javascript unsafe "$1.cb = $2" cc_setLoadFinish :: LoadOption -> Callback a -> IO ()
foreign import javascript unsafe "cc.loader.load($1, $2)" cc_load :: JSVal -> LoadOption -> IO ()
