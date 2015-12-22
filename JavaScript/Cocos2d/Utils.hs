module JavaScript.Cocos2d.Utils where

import Control.Monad
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign.Callback

setCallback2 :: (FromJSVal a, FromJSVal b) => (listener -> Callback (JSVal -> JSVal -> IO ()) -> IO ()) -> listener -> (a -> b -> IO ()) -> IO (IO ())
setCallback2 ffi l h = do
        cb <- syncCallback2 ContinueAsync $ \a b -> join $ h <$> fromJSValUnchecked a <*> fromJSValUnchecked b
        ffi l cb
        return $ releaseCallback cb
