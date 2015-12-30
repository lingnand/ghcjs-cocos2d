{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module JavaScript.Cocos2d.Utils where

import Control.Monad
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign.Callback

convCallback :: (Callback (IO ()) -> IO ()) -> IO () -> IO (IO ())
convCallback ffi h = do
        cb <- syncCallback ContinueAsync h
        ffi cb
        return $ releaseCallback cb

convCallback' :: (Callback (IO ()) -> IO a) -> IO () -> IO (a, IO ())
convCallback' ffi h = do
        cb <- syncCallback ContinueAsync h
        a <- ffi cb
        return (a, releaseCallback cb)

convCallback1 :: FromJSVal a => (Callback (JSVal -> IO ()) -> IO ()) -> (a -> IO ()) -> IO (IO ())
convCallback1 ffi h = do
        cb <- syncCallback1 ContinueAsync $ h <=< fromJSValUnchecked
        ffi cb
        return $ releaseCallback cb

convCallback2 :: (FromJSVal a, FromJSVal b) => (Callback (JSVal -> JSVal -> IO ()) -> IO ()) -> (a -> b -> IO ()) -> IO (IO ())
convCallback2 ffi h = do
        cb <- syncCallback2 ContinueAsync $ \a b -> join $ h <$> fromJSValUnchecked a <*> fromJSValUnchecked b
        ffi cb
        return $ releaseCallback cb
