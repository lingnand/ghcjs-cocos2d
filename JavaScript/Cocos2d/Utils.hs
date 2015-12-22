module JavaScript.Cocos2d.Utils where

import Control.Monad
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign.Callback

convCallback1 :: (FromJSVal a) => (listener -> Callback (JSVal -> IO ()) -> IO ()) -> listener -> (a -> IO ()) -> IO (IO ())
convCallback1 ffi l h = do
        cb <- syncCallback1 ContinueAsync $ h <=< fromJSValUnchecked
        ffi l cb
        return $ releaseCallback cb

convCallback2 :: (FromJSVal a, FromJSVal b) => (listener -> Callback (JSVal -> JSVal -> IO ()) -> IO ()) -> listener -> (a -> b -> IO ()) -> IO (IO ())
convCallback2 ffi l h = do
        cb <- syncCallback2 ContinueAsync $ \a b -> join $ h <$> fromJSValUnchecked a <*> fromJSValUnchecked b
        ffi l cb
        return $ releaseCallback cb
